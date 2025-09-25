#' Free API Gateway Client
#' 
#' This module provides access to DeepCellSeek free API gateway services
#' based on the official API documentation at https://deepcellseek.kiz.ac.cn
#' 
#' @description
#' This client uses the official DeepCellSeek API gateway that provides access 
#' to multiple LLM models without requiring individual API keys.

# DeepCellSeek API Gateway Configuration
DEEPCELLSEEK_API_CONFIG <- list(
  base_url = "https://deepcellseek.kiz.ac.cn",
  
  endpoints = list(
    models = "/api/chat/llms",
    predict = "/api/chat/send", 
    voting = "/api/chat/voting"
  ),

  auth_required = FALSE,

  supported_models = list(
    "OpenAI" = c("gpt-4o", "gpt-5"),
    "DeepSeek" = c("deepseek-reasoner", "deepseek-chat"), 
    "Claude" = c("claude-3-7-sonnet-20250219", "claude-opus-4-1-20250805"),
    "Gemini" = c("gemini-2.0-flash", "gemini-2.5-flash"),
    "Kimi" = c("kimi-k2-turbo-preview"),
    "DouBao" = c("doubao-seed-1-6-250615")
  ),

  elite_models = c("kimi-k2-turbo-preview", "gpt-5", "claude-opus-4-1-20250805"),

  default_timeout = 300
)

#' Check if DeepCellSeek free API gateway should be used
#' 
#' @return Logical indicating if DeepCellSeek gateway should be used
use_deepcellseek_api <- function() {
  use_free <- Sys.getenv("USE_DEEPCELLSEEK_API", "FALSE")
  return(toupper(use_free) == "TRUE")
}

#' Get available models from DeepCellSeek API
#' 
#' @param timeout_seconds Request timeout
#' @return List of available models by provider
get_deepcellseek_models <- function(timeout_seconds = 30) {
  endpoint <- paste0(DEEPCELLSEEK_API_CONFIG$base_url, DEEPCELLSEEK_API_CONFIG$endpoints$models)
  
  tryCatch({
    if (requireNamespace("httr2", quietly = TRUE)) {
      req <- httr2::request(endpoint) |>
        httr2::req_timeout(timeout_seconds) |>
        httr2::req_perform()
      
      resp <- httr2::resp_body_json(req)
      
    } else {
      response <- httr::GET(endpoint, httr::timeout(timeout_seconds))
      
      if (httr::status_code(response) == 200) {
        resp_content <- httr::content(response, "text", encoding = "UTF-8")
        resp <- jsonlite::fromJSON(resp_content, flatten = FALSE)
      } else {
        stop("HTTP error: ", httr::status_code(response))
      }
    }
    
    if (resp$success && resp$code == 200) {
      return(resp$data)
    } else {
      cat("⚠️ API returned error:", resp$message, "\n")
      return(NULL)
    }
    
  }, error = function(e) {
    cat("❌ Failed to get models from DeepCellSeek API:", e$message, "\n")
    return(NULL)
  })
}

#' Call DeepCellSeek API for cell type prediction
#' 
#' @param clusters Gene marker data (formatted string)
#' @param species Species name (e.g., "Human", "Mouse")
#' @param tissueName Tissue name (e.g., "Brain", "PBMC")
#' @param model_provider Provider name (e.g., "OpenAI", "DeepSeek")
#' @param model_name Model name (e.g., "gpt-4o", "deepseek-reasoner")
#' @param annotation_type 0 for CellType, 1 for CellSubType
#' @param parent_cell_type Parent cell type (required for subtype annotation)
#' @param timeout_seconds Request timeout
#' @return API response or NULL if failed
call_deepcellseek_predict <- function(clusters, species, tissueName, model_provider, model_name, 
                                     annotation_type = 0, parent_cell_type = NULL, timeout_seconds = 300) {
  
  endpoint <- paste0(DEEPCELLSEEK_API_CONFIG$base_url, DEEPCELLSEEK_API_CONFIG$endpoints$predict)

  request_body <- list(
    clusters = clusters,
    annotationType = annotation_type,
    species = species,
    tissueName = tissueName,
    model = list(
      provider = model_provider,
      modelName = model_name
    )
  )
  
  if (annotation_type == 1 && !is.null(parent_cell_type)) {
    request_body$parentCellType <- parent_cell_type
  }
  
  cat("🆓 Calling DeepCellSeek API with", model_provider, model_name, "\n")
  
  tryCatch({
    
    if (requireNamespace("httr2", quietly = TRUE)) {
      req <- httr2::request(endpoint) |>
        httr2::req_headers("Content-Type" = "application/json") |>
        httr2::req_body_json(request_body) |>
        httr2::req_timeout(timeout_seconds) |>
        httr2::req_perform()
      
      resp <- httr2::resp_body_json(req)
      
    } else {
      response <- httr::POST(
        endpoint,
        httr::add_headers("Content-Type" = "application/json"),
        body = jsonlite::toJSON(request_body, auto_unbox = TRUE),
        httr::timeout(timeout_seconds),
        encode = "raw"
      )
      
      if (httr::status_code(response) == 200) {
        resp_content <- httr::content(response, "text", encoding = "UTF-8")
        resp <- jsonlite::fromJSON(resp_content, flatten = FALSE)
      } else {
        stop("HTTP error: ", httr::status_code(response))
      }
    }

    if (resp$success && resp$code == 200) {
      cat("✅ DeepCellSeek API call successful\n")
      return(resp$data$result)
    } else {
      cat("❌ DeepCellSeek API returned error:", resp$message, "\n")
      return(NULL)
    }
    
  }, error = function(e) {
    cat("❌ DeepCellSeek API call failed:", e$message, "\n")
    return(NULL)
  })
}

#' Call DeepCellSeek API for ensemble voting
#' 
#' @param clusters Gene marker data (formatted string)
#' @param species Species name  
#' @param tissueName Tissue name
#' @param annotation_type 0 for CellType, 1 for CellSubType
#' @param parent_cell_type Parent cell type (for subtype annotation)
#' @param chat_outputs List of ChatOutput objects from elite models
#' @param timeout_seconds Request timeout
#' @return Final voting result or NULL if failed
call_deepcellseek_voting <- function(clusters, species, tissueName, annotation_type = 0, 
                                   parent_cell_type = NULL, chat_outputs = NULL, timeout_seconds = 300) {
  
  endpoint <- paste0(DEEPCELLSEEK_API_CONFIG$base_url, DEEPCELLSEEK_API_CONFIG$endpoints$voting)
  
  if (is.null(chat_outputs)) {
    cat("⚖️ No predictions provided, will auto-fill with elite models\n")
    chat_outputs <- list()
  }

  request_body <- list(
    clusters = clusters,
    annotationType = annotation_type,
    species = species,
    tissueName = tissueName,
    chatOutputs = chat_outputs
  )

  if (annotation_type == 1 && !is.null(parent_cell_type)) {
    request_body$parentCellType <- parent_cell_type
  }
  
  tryCatch({
    
    if (requireNamespace("httr2", quietly = TRUE)) {
      req <- httr2::request(endpoint) |>
        httr2::req_headers("Content-Type" = "application/json") |>
        httr2::req_body_json(request_body) |>
        httr2::req_timeout(timeout_seconds) |>
        httr2::req_perform()
      
      resp <- httr2::resp_body_json(req)
      
    } else {
      response <- httr::POST(
        endpoint,
        httr::add_headers("Content-Type" = "application/json"),
        body = jsonlite::toJSON(request_body, auto_unbox = TRUE),
        httr::timeout(timeout_seconds),
        encode = "raw"
      )
      
      if (httr::status_code(response) == 200) {
        resp_content <- httr::content(response, "text", encoding = "UTF-8")
        resp <- jsonlite::fromJSON(resp_content, flatten = FALSE)
      } else {
        stop("HTTP error: ", httr::status_code(response))
      }
    }

    if (resp$success && resp$code == 200) {
      cat("✅ DeepCellSeek ensemble voting successful\n")
      return(resp$data$result)
    } else {
      cat("❌ DeepCellSeek voting API returned error:", resp$message, "\n")
      return(NULL)
    }
    
  }, error = function(e) {
    cat("❌ DeepCellSeek voting API call failed:", e$message, "\n")
    return(NULL)
  })
}

#' Convert processed input to DeepCellSeek clusters format
#' 
#' @param processed_input Named vector of cluster genes (from process_input_data)
#' @return Formatted clusters string for DeepCellSeek API
format_clusters_for_deepcellseek <- function(processed_input) {
  cluster_lines <- paste0(names(processed_input), ":", processed_input)
  return(paste(cluster_lines, collapse = "\n"))
}

#' Map model name to DeepCellSeek provider and model
#' 
#' @param model Model name from our system
#' @return List with provider and modelName
map_model_to_deepcellseek <- function(model) {
  model_mapping <- list(
    "gpt-4o" = list(provider = "OpenAI", modelName = "gpt-4o"),
    "gpt-5" = list(provider = "OpenAI", modelName = "gpt-5"),
    "deepseek-reasoner" = list(provider = "DeepSeek", modelName = "deepseek-reasoner"),
    "deepseek-chat" = list(provider = "DeepSeek", modelName = "deepseek-chat"),
    "claude-3-7-sonnet-20250219" = list(provider = "Claude", modelName = "claude-3-7-sonnet-20250219"),
    "claude-opus-4-1-20250805" = list(provider = "Claude", modelName = "claude-opus-4-1-20250805"),
    "gemini-2.0-flash" = list(provider = "Gemini", modelName = "gemini-2.0-flash"),
    "gemini-2.5-flash" = list(provider = "Gemini", modelName = "gemini-2.5-flash"),
    "kimi-k2-turbo-preview" = list(provider = "Kimi", modelName = "kimi-k2-turbo-preview"),
    "doubao-seed-1-6-250615" = list(provider = "DouBao", modelName = "doubao-seed-1-6-250615")
  )
  
  if (model %in% names(model_mapping)) {
    return(model_mapping[[model]])
  } else {
    cat("⚠️ Model", model, "not found in DeepCellSeek mapping\n")
    return(NULL)
  }
}

#' One-step Cell Type Annotation using DeepCellSeek API
#' 
#' @param input Input data (Seurat FindAllMarkers output or processed list)
#' @param tissuename Tissue name (e.g., "PBMC", "Brain")
#' @param species Species name (default: "Human")
#' @param model Model name
#' @param topgenenumber Number of top genes per cluster (default: 10)
#' @param timeout_seconds Request timeout (default: 300)
#' @return Named vector of cell type annotations
#' @export
deepcellseek_celltype <- function(input, tissuename, species = "Human", 
                                 model = "gemini-2.0-flash", topgenenumber = 10, 
                                 timeout_seconds = 300) {
  
  if (!use_deepcellseek_api()) {
    stop("❌ DeepCellSeek API is not enabled. Please set: Sys.setenv(USE_DEEPCELLSEEK_API = 'TRUE')")
  }

  processed_input <- process_input_data(input, topgenenumber)
  formatted_clusters <- format_clusters_for_deepcellseek(processed_input)
  
  model_mapping <- map_model_to_deepcellseek(model)
  if (is.null(model_mapping)) {
    stop("❌ Model not supported: ", model)
  }

  result <- call_deepcellseek_predict(
    clusters = formatted_clusters,
    species = species,
    tissueName = tissuename,
    model_provider = model_mapping$provider,
    model_name = model_mapping$modelName,
    annotation_type = 0,
    timeout_seconds = timeout_seconds
  )
  
  if (is.null(result)) {
    stop("❌ DeepCellSeek API call failed")
  }

  cat("✅ Cell type annotation completed\n")
  cat("📝 Note: It is recommended to check results for potential AI hallucinations before downstream analysis\n")
  
  result <- gsub(',$', '', unlist(result))
  result <- sub("^[^a-zA-Z]*([a-zA-Z].*)", "\\1", result)

  if (length(result) == length(processed_input)) {
    names(result) <- names(processed_input)
  }

  return(result)
}

#' One-step Cell Subtype Annotation using DeepCellSeek API
#' 
#' @param input Input data (Seurat FindAllMarkers output or processed list)
#' @param tissuename Tissue name (e.g., "PBMC", "Brain")
#' @param celltypename Parent cell type name (e.g., "T cell", "Neuron")
#' @param species Species name (default: "Human")
#' @param model Model name
#' @param topgenenumber Number of top genes per cluster (default: 10)
#' @param timeout_seconds Request timeout (default: 300)
#' @return Named vector of cell subtype annotations
#' @export
deepcellseek_subcelltype <- function(input, tissuename, celltypename, species = "Human", 
                                    model = "gemini-2.0-flash", topgenenumber = 10, 
                                    timeout_seconds = 300) {
  
  if (!use_deepcellseek_api()) {
    stop("❌ DeepCellSeek API is not enabled. Please set: Sys.setenv(USE_DEEPCELLSEEK_API = 'TRUE')")
  }

  processed_input <- process_input_data(input, topgenenumber)
  formatted_clusters <- format_clusters_for_deepcellseek(processed_input)
  
  model_mapping <- map_model_to_deepcellseek(model)
  if (is.null(model_mapping)) {
    stop("❌ Model not supported: ", model)
  }

  result <- call_deepcellseek_predict(
    clusters = formatted_clusters,
    species = species,
    tissueName = tissuename,
    model_provider = model_mapping$provider,
    model_name = model_mapping$modelName,
    annotation_type = 1,
    parent_cell_type = celltypename,
    timeout_seconds = timeout_seconds
  )
  
  if (is.null(result)) {
    stop("❌ DeepCellSeek API call failed")
  }
  
  cat("✅ Cell subtype annotation completed\n")
  cat("📝 Note: It is recommended to check results for potential AI hallucinations before downstream analysis\n")
  
  result <- gsub(',$', '', unlist(result))
  result <- sub("^[^a-zA-Z]*([a-zA-Z].*)", "\\1", result)

  if (length(result) == length(processed_input)) {
    names(result) <- names(processed_input)
  }

  return(result)
}

#' One-step Ensemble Voting for Cell Type Annotation
#' 
#' @param input Input data (Seurat FindAllMarkers output or processed list)
#' @param tissuename Tissue name (e.g., "PBMC", "Brain")
#' @param species Species name (default: "Human")
#' @param topgenenumber Number of top genes per cluster (default: 10)
#' @param timeout_seconds Request timeout (default: 300)
#' @return Named vector of cell type annotations from ensemble voting
#' @export
deepcellseek_ensemble <- function(input, tissuename, species = "Human", 
                                 topgenenumber = 10, timeout_seconds = 300) {
  
  if (!use_deepcellseek_api()) {
    stop("❌ DeepCellSeek API is not enabled. Please set: Sys.setenv(USE_DEEPCELLSEEK_API = 'TRUE')")
  }

  processed_input <- process_input_data(input, topgenenumber)
  formatted_clusters <- format_clusters_for_deepcellseek(processed_input)

  result <- call_deepcellseek_voting(
    clusters = formatted_clusters,
    species = species,
    tissueName = tissuename,
    annotation_type = 0,
    timeout_seconds = timeout_seconds
  )
  
  if (is.null(result)) {
    stop("❌ DeepCellSeek ensemble voting API call failed")
  }
  
  cat("✅ Ensemble voting annotation completed\n")
  cat("📝 Note: It is recommended to check results for potential AI hallucinations before downstream analysis\n")
  
  result <- gsub(',$', '', unlist(result))
  result <- sub("^[^a-zA-Z]*([a-zA-Z].*)", "\\1", result)

  if (length(result) == length(processed_input)) {
    names(result) <- names(processed_input)
  }
  
  return(result)
}

#' One-step Ensemble Voting for Cell Subtype Annotation using DeepCellSeek API
#'
#' @param input Input data (Seurat FindAllMarkers output or processed list)
#' @param tissuename Tissue name (e.g., "PBMC", "Brain")
#' @param celltypename Parent cell type name (e.g., "T cell", "Neuron")
#' @param species Species name (default: "Human")
#' @param topgenenumber Number of top genes per cluster (default: 10)
#' @param timeout_seconds Request timeout (default: 300)
#' @return Named vector of cell subtype annotations from ensemble voting
#' @export
deepcellseek_subcelltype_ensemble <- function(input, tissuename, celltypename, species = "Human",
                                               topgenenumber = 10, timeout_seconds = 300) {

  if (!use_deepcellseek_api()) {
    stop("❌ DeepCellSeek API is not enabled. Please set: Sys.setenv(USE_DEEPCELLSEEK_API = 'TRUE')")
  }

  processed_input <- process_input_data(input, topgenenumber)
  formatted_clusters <- format_clusters_for_deepcellseek(processed_input)

  result <- call_deepcellseek_voting(
    clusters = formatted_clusters,
    species = species,
    tissueName = tissuename,
    annotation_type = 1, # This is the key change for subtype
    parent_cell_type = celltypename, # Pass the parent cell type
    timeout_seconds = timeout_seconds
  )

  if (is.null(result)) {
    stop("❌ DeepCellSeek ensemble subtype voting API call failed")
  }

  cat("✅ Ensemble subtype voting annotation completed\n")
  cat("📝 Note: It is recommended to check results for potential AI hallucinations before downstream analysis\n")

  result <- gsub(',$', '', unlist(result))
  result <- sub("^[^a-zA-Z]*([a-zA-Z].*)", "\\1", result)

  # Ensure the output is a named vector, consistent with other functions
  if (length(result) == length(processed_input)) {
    names(result) <- names(processed_input)
  }

  return(result)
}