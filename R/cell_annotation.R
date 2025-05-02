#' Cell Type Annotation using LLMs
#' 
#' @param input Input data for cell type annotation
#' @param tissuename Optional tissue name
#' @param model Model to use for annotation
#' @param topgenenumber Number of top genes to consider
#' @param api_key API key for the LLM service
#' @return Cell type annotation results
#' @export
llm_celltype <- function(input, tissuename = NULL, model = "deepseek-reasoner", 
                         topgenenumber = 10, api_key = NULL) {
  
  supported_models <- list(
    openai = c("gpt-4o"),
    
    deepseek = c("deepseek-reasoner"),
    
    claude = c("claude-3-7-sonnet-20250219"),
    
    gemini = c("gemini-2.0-flash"),
    
    grok = c("grok-2-1212"),
    
    kimi = c("moonshot-v1-128k"),
    
    doubao = c("doubao-1-5-pro-256k-250115")
  )
  
  model_type <- NA
  for (type in names(supported_models)) {
    if (model %in% supported_models[[type]] || grepl(paste0("^", type), model)) {
      model_type <- type
      break
    }
  }
  
  if (is.na(model_type)) {
    stop("Not Supported: ", model)
  }
  
  if (is.null(api_key)) {
    env_var_name <- paste0(toupper(model_type), "_API_KEY")
    api_key <- Sys.getenv(env_var_name)
    if (api_key == "") {
      print(paste0("Note: ", model_type, " API key not found: returning the prompt itself."))
      API.flag <- 0
    } else {
      API.flag <- 1
    }
  } else {
    API.flag <- 1
  }
  
  if (class(input) == 'list') {
    input <- sapply(input, paste, collapse = ',')
  } else {
    input <- input[input$avg_log2FC > 0, , drop = FALSE]
    input <- tapply(input$gene, list(input$cluster), 
                    function(i) paste0(i[1:min(length(i), topgenenumber)], collapse = ','))
  }
  
  if (!API.flag) {
    message <- paste0('Identify cell types of ', tissuename, 
                      ' cells using the following markers separately for each\n row. ',
                      'Only provide the cell type name. Do not show numbers before the name.\n ',
                      'IMPORTANT: Return exactly ', length(id), ' lines, one for each row.\n',
                      'Some can be a mixture of multiple cell types. ', "\n", 
                      paste0(names(input), ':', unlist(input), collapse = "\n"))
    return(message)
  } else {
    print(paste0("Note: ", model_type, " API key found: returning the cell type annotations."))
    
    cutnum <- ceiling(length(input) / 30)
    if (cutnum > 1) {
      cid <- as.numeric(cut(1:length(input), cutnum))
    } else {
      cid <- rep(1, length(input))
    }
    
    allres <- sapply(1:cutnum, function(i) {
      id <- which(cid == i)
      flag <- 0
      retry_count <- 0
      max_retries <- 3
      
      while (flag == 0 && retry_count < max_retries) {
        tryCatch({
          prompt <- paste0('Identify cell types of ', tissuename, 
                           ' cells using the following markers separately for each\n row. ',
                           'Only provide the cell type name. Do not show numbers before the name.\n',
                           'IMPORTANT: Return exactly ', length(id), ' lines, one for each row.\n',
                           'Some can be a mixture of multiple cell types.\n', 
                           paste(names(input)[id], ':', input[id], collapse = '\n'))
          
          if (model_type == "openai") {
            response <- openai::create_chat_completion(
              model = model,
              messages = list(list(
                "role" = "user", 
                "content" = prompt
              )),
              temperature = 0.3
            )
            
            res <- strsplit(response$choices[, 'message.content'], '\n')[[1]]
            
          } else if (model_type == "deepseek") {
            req <- httr2::request("https://api.deepseek.com/v1/chat/completions") |>
              httr2::req_headers(
                "Content-Type" = "application/json",
                "Authorization" = paste("Bearer", api_key)
              ) |>
              httr2::req_body_json(list(
                model = model,
                messages = list(
                  list(role = "user", content = prompt)
                ),
                temperature = 0.3
              )) |>
              httr2::req_perform()
            
            resp <- httr2::resp_body_json(req)
            res <- strsplit(resp$choices[[1]]$message$content, '\n')[[1]]
            
          } else if (model_type == "claude") {
            req <- httr2::request("https://api.anthropic.com/v1/messages") |>
              httr2::req_headers(
                "Content-Type" = "application/json",
                "x-api-key" = api_key,
                "anthropic-version" = "2023-06-01"
              ) |>
              httr2::req_body_json(list(
                model = model,
                messages = list(
                  list(role = "user", content = prompt)
                ),
                temperature = 0.3,
                max_tokens = 32000
              )) |>
              httr2::req_perform()
            
            resp <- httr2::resp_body_json(req)
            res <- strsplit(resp$content[[1]]$text, '\n')[[1]]
            
          } else if (model_type == "gemini") {
            base_url <- "https://generativelanguage.googleapis.com/v1/models"
            endpoint <- paste0(base_url, "/", model, ":generateContent?key=", api_key)
            
            req <- httr2::request(endpoint) |>
              httr2::req_headers("Content-Type" = "application/json") |>
              httr2::req_body_json(list(
                contents = list(
                  list(parts = list(
                    list(text = prompt)
                  ))
                ),
                generationConfig = list(
                  temperature = 0.3
                )
              )) |>
              httr2::req_perform()
            
            resp <- httr2::resp_body_json(req)
            res <- strsplit(resp$candidates[[1]]$content$parts[[1]]$text, '\n')[[1]]
            
          } else if (model_type == "grok") {
            req <- httr2::request("https://api.x.ai/v1/chat/completions") |>
              httr2::req_headers(
                "Content-Type" = "application/json",
                "Authorization" = paste("Bearer", api_key)
              ) |>
              httr2::req_body_json(list(
                model = model,
                messages = list(
                  list(role = "user", content = prompt)
                ),
                temperature = 0.3
              )) |>
              httr2::req_perform()
            
            resp <- httr2::resp_body_json(req)
            res <- strsplit(resp$choices[[1]]$message$content, '\n')[[1]]
            
          } else if (model_type == "kimi") {
            req <- httr2::request("https://api.moonshot.cn/v1/chat/completions") |>
              httr2::req_headers(
                "Content-Type" = "application/json",
                "Authorization" = paste("Bearer", api_key)
              ) |>
              httr2::req_body_json(list(
                model = model,
                messages = list(
                  list(role = "user", content = prompt)
                ),
                temperature = 0.3
              )) |>
              httr2::req_perform()
            
            resp <- httr2::resp_body_json(req)
            res <- strsplit(resp$choices[[1]]$message$content, '\n')[[1]]
            
          } else if (model_type == "doubao") {
            req <- httr2::request("https://ark.cn-beijing.volces.com/api/v3/chat/completions") |>
              httr2::req_headers(
                "Content-Type" = "application/json",
                "Authorization" = paste("Bearer", api_key)
              ) |>
              httr2::req_body_json(list(
                model = model,
                messages = list(
                  list(role = "user", content = prompt)
                ),
                temperature = 0.3
              )) |>
              httr2::req_perform()
            
            resp <- httr2::resp_body_json(req)
            res <- strsplit(resp$choices[[1]]$message$content, '\n')[[1]]
          }
          
          res <- res[nzchar(trimws(res))]
          
          if (length(res) == length(id)) {
            flag <- 1
          } else {
            warning("Response mismatch: expected ", length(id), " items but got ", length(res))
            
            valid_results <- min(length(res), length(id))
            partial_results <- res[1:valid_results]
            
            if (valid_results > 0 && retry_count >= max_retries - 1) {
              missing_count <- length(id) - valid_results
              res <- c(partial_results, rep("Failed to annotate", missing_count))
              flag <- 1 
            } else {
              cat("Response length mismatch. Retrying...\n")
              retry_count <- retry_count + 1
            }
          }
        }, error = function(e) {
          cat("API call error:", e$message, ". Retrying...\n")
          retry_count <- retry_count + 1
          Sys.sleep(2)
        })
      }
      
      if (flag == 0) {
        warning("Failed to get proper response after ", max_retries, " retries for batch ", i)
        res <- rep("Failed to annotate", length(id))
      }
      
      names(res) <- names(input)[id]
      return(res)
    }, simplify = FALSE)
    
    print(paste0('Note: It is always recommended to check the results returned by ', 
                 model, ' in case of AI hallucination, before going to down-stream analysis.'))
    
    result <- gsub(',$', '', unlist(allres))
    result <- sub("^[^a-zA-Z]*([a-zA-Z].*)", "\\1", result)
    
    return(result)
  }
}

#' Cell Subtype Annotation using LLMs
#' 
#' @param input Input data for cell subtype annotation
#' @param tissuename Optional tissue name
#' @param celltypename Optional cell type name
#' @param model Model to use for annotation
#' @param topgenenumber Number of top genes to consider
#' @param api_key API key for the LLM service
#' @return Cell subtype annotation results
#' @export
llm_subcelltype <- function(input, tissuename = NULL, celltypename = NULL, model = "deepseek-reasoner", 
                         topgenenumber = 10, api_key = NULL) {
  
  supported_models <- list(
    openai = c("gpt-4o"),
    
    deepseek = c("deepseek-reasoner"),
    
    claude = c("claude-3-7-sonnet-20250219"),
    
    gemini = c("gemini-2.0-flash"),
    
    grok = c("grok-2-1212"),
    
    kimi = c("moonshot-v1-128k"),
    
    doubao = c("doubao-1-5-pro-256k-250115")
  )
  
  model_type <- NA
  for (type in names(supported_models)) {
    if (model %in% supported_models[[type]] || grepl(paste0("^", type), model)) {
      model_type <- type
      break
    }
  }
  
  if (is.na(model_type)) {
    stop("Not Supported: ", model)
  }
  
  if (is.null(api_key)) {
    env_var_name <- paste0(toupper(model_type), "_API_KEY")
    api_key <- Sys.getenv(env_var_name)
    if (api_key == "") {
      print(paste0("Note: ", model_type, " API key not found: returning the prompt itself."))
      API.flag <- 0
    } else {
      API.flag <- 1
    }
  } else {
    API.flag <- 1
  }
  
  if (class(input) == 'list') {
    input <- sapply(input, paste, collapse = ',')
  } else {
    input <- input[input$avg_log2FC > 0, , drop = FALSE]
    input <- tapply(input$gene, list(input$cluster), 
                    function(i) paste0(i[1:min(length(i), topgenenumber)], collapse = ','))
  }
  
  if (!API.flag) {
    message <- paste0('Identify detailed cell subtypes for ', celltypename, ' in ', tissuename,
                      ' cells using the following markers, provided separately for each row. \n',
                      'For each row, use established nomenclature to provide the most precise cell subtype classification possible based on the marker expression profile. \n',
                      'Only output the cell subtype name. Do not include any numbers or extra annotations before the name. \n',
                      'IMPORTANT: Return exactly ', length(id), ' lines, one for each row. \n',
                      'Note: Some rows may represent a mixture of multiple subtypes. \n',
                      paste0(names(input), ':', unlist(input), collapse = "\n"))
    return(message)
  } else {
    print(paste0("Note: ", model_type, " API key found: returning the cell type annotations."))
    
    cutnum <- ceiling(length(input) / 30)
    if (cutnum > 1) {
      cid <- as.numeric(cut(1:length(input), cutnum))
    } else {
      cid <- rep(1, length(input))
    }
    
    allres <- sapply(1:cutnum, function(i) {
      id <- which(cid == i)
      flag <- 0
      retry_count <- 0
      max_retries <- 3
      
      while (flag == 0 && retry_count < max_retries) {
        tryCatch({
          prompt <- paste0('Identify detailed cell subtypes for ', celltypename, ' in ', tissuename,
                           ' cells using the following markers, provided separately for each row. \n',
                           'For each row, use established nomenclature to provide the most precise cell subtype classification possible based on the marker expression profile. \n',
                           'Only output the cell subtype name. Do not include any numbers or extra annotations before the name. \n',
                           'IMPORTANT: Return exactly ', length(id), ' lines, one for each row. \n',
                           'Note: Some rows may represent a mixture of multiple subtypes. \n',
                           paste(names(input)[id], ':', input[id], collapse = '\n'))
          
          if (model_type == "openai") {
            response <- openai::create_chat_completion(
              model = model,
              messages = list(list(
                "role" = "user", 
                "content" = prompt
              )),
              temperature = 0.3
            )
            
            res <- strsplit(response$choices[, 'message.content'], '\n')[[1]]
            
          } else if (model_type == "deepseek") {
            req <- httr2::request("https://api.deepseek.com/v1/chat/completions") |>
              httr2::req_headers(
                "Content-Type" = "application/json",
                "Authorization" = paste("Bearer", api_key)
              ) |>
              httr2::req_body_json(list(
                model = model,
                messages = list(
                  list(role = "user", content = prompt)
                ),
                temperature = 0.3
              )) |>
              httr2::req_perform()
            
            resp <- httr2::resp_body_json(req)
            res <- strsplit(resp$choices[[1]]$message$content, '\n')[[1]]
            
          } else if (model_type == "claude") {
            req <- httr2::request("https://api.anthropic.com/v1/messages") |>
              httr2::req_headers(
                "Content-Type" = "application/json",
                "x-api-key" = api_key,
                "anthropic-version" = "2023-06-01"
              ) |>
              httr2::req_body_json(list(
                model = model,
                messages = list(
                  list(role = "user", content = prompt)
                ),
                temperature = 0.3,
                max_tokens = 32000
              )) |>
              httr2::req_perform()
            
            resp <- httr2::resp_body_json(req)
            res <- strsplit(resp$content[[1]]$text, '\n')[[1]]
            
          } else if (model_type == "gemini") {
            base_url <- "https://generativelanguage.googleapis.com/v1/models"
            endpoint <- paste0(base_url, "/", model, ":generateContent?key=", api_key)
            
            req <- httr2::request(endpoint) |>
              httr2::req_headers("Content-Type" = "application/json") |>
              httr2::req_body_json(list(
                contents = list(
                  list(parts = list(
                    list(text = prompt)
                  ))
                ),
                generationConfig = list(
                  temperature = 0.3
                )
              )) |>
              httr2::req_perform()
            
            resp <- httr2::resp_body_json(req)
            res <- strsplit(resp$candidates[[1]]$content$parts[[1]]$text, '\n')[[1]]
            
          } else if (model_type == "grok") {
            req <- httr2::request("https://api.x.ai/v1/chat/completions") |>
              httr2::req_headers(
                "Content-Type" = "application/json",
                "Authorization" = paste("Bearer", api_key)
              ) |>
              httr2::req_body_json(list(
                model = model,
                messages = list(
                  list(role = "user", content = prompt)
                ),
                temperature = 0.3
              )) |>
              httr2::req_perform()
            
            resp <- httr2::resp_body_json(req)
            res <- strsplit(resp$choices[[1]]$message$content, '\n')[[1]]
            
          } else if (model_type == "kimi") {
            req <- httr2::request("https://api.moonshot.cn/v1/chat/completions") |>
              httr2::req_headers(
                "Content-Type" = "application/json",
                "Authorization" = paste("Bearer", api_key)
              ) |>
              httr2::req_body_json(list(
                model = model,
                messages = list(
                  list(role = "user", content = prompt)
                ),
                temperature = 0.3
              )) |>
              httr2::req_perform()
            
            resp <- httr2::resp_body_json(req)
            res <- strsplit(resp$choices[[1]]$message$content, '\n')[[1]]
            
          } else if (model_type == "doubao") {
            req <- httr2::request("https://ark.cn-beijing.volces.com/api/v3/chat/completions") |>
              httr2::req_headers(
                "Content-Type" = "application/json",
                "Authorization" = paste("Bearer", api_key)
              ) |>
              httr2::req_body_json(list(
                model = model,
                messages = list(
                  list(role = "user", content = prompt)
                ),
                temperature = 0.3
              )) |>
              httr2::req_perform()
            
            resp <- httr2::resp_body_json(req)
            res <- strsplit(resp$choices[[1]]$message$content, '\n')[[1]]
          }
          
          res <- res[nzchar(trimws(res))]
          
          if (length(res) == length(id)) {
            flag <- 1
          } else {
            warning("Response mismatch: expected ", length(id), " items but got ", length(res))
            
            valid_results <- min(length(res), length(id))
            partial_results <- res[1:valid_results]
            
            if (valid_results > 0 && retry_count >= max_retries - 1) {
              missing_count <- length(id) - valid_results
              res <- c(partial_results, rep("Failed to annotate", missing_count))
              flag <- 1 
            } else {
              cat("Response length mismatch. Retrying...\n")
              retry_count <- retry_count + 1
            }
          }
        }, error = function(e) {
          cat("API call error:", e$message, ". Retrying...\n")
          retry_count <- retry_count + 1
          Sys.sleep(2)
        })
      }
      
      if (flag == 0) {
        warning("Failed to get proper response after ", max_retries, " retries for batch ", i)
        res <- rep("Failed to annotate", length(id))
      }
      
      names(res) <- names(input)[id]
      return(res)
    }, simplify = FALSE)
    
    print(paste0('Note: It is always recommended to check the results returned by ', 
                 model, ' in case of AI hallucination, before going to down-stream analysis.'))
    
    result <- gsub(',$', '', unlist(allres))
    result <- sub("^[^a-zA-Z]*([a-zA-Z].*)", "\\1", result)
    
    return(result)
  }
}