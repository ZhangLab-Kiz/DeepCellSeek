#' Cell Type Annotation using LLMs
#' 
#' @param input Input data for cell type annotation (data frame or named list)
#' @param tissuename Tissue name (e.g., "Brain", "Lung")
#' @param species Species name (e.g., "Human", "Mouse") 
#' @param model Model to use for annotation
#' @param topgenenumber Number of top genes to consider
#' @param api_key API key for the LLM service
#' @return Cell type annotation results
#' @export
llm_celltype <- function(input, tissuename = NULL, species = "Human", model = "deepseek-reasoner", 
                         topgenenumber = 10, api_key = NULL) {

  if (!requireNamespace("glue", quietly = TRUE)) {
    stop("Package 'glue' is required. Please install it with: install.packages('glue')")
  }

  if (!is_model_supported(model)) {
    stop("❌ Model not supported: ", model, "\nSupported models: ", 
         paste(unlist(get_supported_models()), collapse = ", "))
  }

  config <- get_model_config(model)
  if (is.null(api_key)) {
    api_key <- Sys.getenv(config$env_var)
    if (api_key == "") {
      cat("📝 Note:", config$provider, "API key not found: returning the prompt itself.\n")
      API.flag <- 0
    } else {
      cat("🔑 Note:", config$provider, "API key found: proceeding with annotation.\n")
      API.flag <- 1
    }
  } else {
    API.flag <- 1
  }

  processed_input <- process_input_data(input, topgenenumber)

  if (!API.flag) {
    marker_data <- paste0(names(processed_input), ':', unlist(processed_input), collapse = "\n")
    message <- glue::glue("Identify cell types of {species} {tissuename} cells using the following markers separately for each row.
You MUST use standardized cell type names from the Cell Ontology (CL). If no exact CL term exists, use the most specific CL term available and add descriptive modifiers.
Only provide the cell type name. Do not include any numbers or extra annotations before the name. Do not include any explanatory text, introductory phrases, or descriptions.
IMPORTANT: Return exactly {num_clusters} lines, one for each row.
Some can be a mixture of multiple cell types.

{marker_data}",
                         species = species,
                         tissuename = tissuename,
                         num_clusters = length(processed_input),
                         marker_data = marker_data)
    return(message)
  }

  cutnum <- ceiling(length(processed_input) / 30)
  if (cutnum > 1) {
    cid <- as.numeric(cut(1:length(processed_input), cutnum))
  } else {
    cid <- rep(1, length(processed_input))
  }
  
  allres <- sapply(1:cutnum, function(i) {
    id <- which(cid == i)
    retry_count <- 0
    max_retries <- 3
    
    while (retry_count < max_retries) {
      marker_data <- paste(names(processed_input)[id], ':', processed_input[id], collapse = '\n')
      
      prompt <- glue::glue("Identify cell types of {species} {tissuename} cells using the following markers separately for each row.
You MUST use standardized cell type names from the Cell Ontology (CL). If no exact CL term exists, use the most specific CL term available and add descriptive modifiers.
Only provide the cell type name. Do not include any numbers or extra annotations before the name. Do not include any explanatory text, introductory phrases, or descriptions.
IMPORTANT: Return exactly {num_clusters} lines, one for each row.
Some can be a mixture of multiple cell types.

{marker_data}",
                          species = species,
                          tissuename = tissuename,
                          num_clusters = length(id),
                          marker_data = marker_data)

      result <- call_llm_api(model, prompt, temperature = NULL, timeout_seconds = 300, api_key = api_key)

      if (!is.null(result)) {
        res <- trimws(unlist(strsplit(result, '\n')))
        res <- res[nzchar(res)]

        if (length(res) == length(id)) {
          names(res) <- names(processed_input)[id]
          return(res)
        } else {
          cat("⚠️ Response length mismatch for batch", i, ". Expected:", length(id), "Got:", length(res), "\n")
          
          if (retry_count >= max_retries - 1) {
            valid_results <- min(length(res), length(id))
            final_res <- rep("Failed to annotate", length(id))
            if (valid_results > 0) {
              final_res[1:valid_results] <- res[1:valid_results]
            }
            names(final_res) <- names(processed_input)[id]
            return(final_res)
          }
        }
      }
      
      retry_count <- retry_count + 1
      cat("🔄 Retrying batch", i, "(attempt", retry_count + 1, ")\n")
      Sys.sleep(2)
    }
    
    cat("❌ Failed to get proper response for batch", i, "after", max_retries, "retries\n")
    res <- rep("Failed to annotate", length(id))
    names(res) <- names(processed_input)[id]
    return(res)
  }, simplify = FALSE)
  
  cat("✅ Cell type annotation completed\n")
  cat("📝 Note: It is recommended to check results for potential AI hallucinations before downstream analysis\n")
  
  result <- gsub(',$', '', unlist(allres))
  result <- sub("^[^a-zA-Z]*([a-zA-Z].*)", "\\1", result)
  
  return(result)
}

#' Cell Subtype Annotation using LLMs
#' 
#' @param input Input data for cell subtype annotation (data frame or named list)
#' @param tissuename Tissue name (e.g., "Brain", "Lung")
#' @param species Species name (e.g., "Human", "Mouse")
#' @param celltypename Parent cell type name
#' @param model Model to use for annotation
#' @param topgenenumber Number of top genes to consider
#' @param api_key API key for the LLM service
#' @return Cell subtype annotation results
#' @export
llm_subcelltype <- function(input, tissuename = NULL, species = "Human", celltypename = NULL, 
                           model = "deepseek-reasoner", topgenenumber = 10, api_key = NULL) {

  if (!requireNamespace("glue", quietly = TRUE)) {
    stop("Package 'glue' is required. Please install it with: install.packages('glue')")
  }

  if (!is_model_supported(model)) {
    stop("❌ Model not supported: ", model, "\nSupported models: ", 
         paste(unlist(get_supported_models()), collapse = ", "))
  }

  config <- get_model_config(model)
  if (is.null(api_key)) {
    api_key <- Sys.getenv(config$env_var)
    if (api_key == "") {
      cat("📝 Note:", config$provider, "API key not found: returning the prompt itself.\n")
      API.flag <- 0
    } else {
      cat("🔑 Note:", config$provider, "API key found: proceeding with annotation.\n")
      API.flag <- 1
    }
  } else {
    API.flag <- 1
  }

  processed_input <- process_input_data(input, topgenenumber)
  
  if (!API.flag) {
    marker_data <- paste0(names(processed_input), ':', unlist(processed_input), collapse = "\n")
    message <- glue::glue("Identify detailed cell subtypes for {celltypename} in {species} {tissuename} cells using the following markers, provided separately for each row.
You MUST use standardized cell subtype names from the Cell Ontology (CL). If no exact CL term exists, use the most specific CL term available and add descriptive modifiers.
Only output the cell subtype name. Do not include any numbers or extra annotations before the name. Do not include any explanatory text, introductory phrases, or descriptions.
IMPORTANT: Return exactly {num_clusters} lines, one for each row.
Note: Some rows may represent a mixture of multiple subtypes.

{marker_data}",
                         celltypename = celltypename,
                         species = species,
                         tissuename = tissuename,
                         num_clusters = length(processed_input),
                         marker_data = marker_data)
    return(message)
  }

  cutnum <- ceiling(length(processed_input) / 30)
  if (cutnum > 1) {
    cid <- as.numeric(cut(1:length(processed_input), cutnum))
  } else {
    cid <- rep(1, length(processed_input))
  }
  
  allres <- sapply(1:cutnum, function(i) {
    id <- which(cid == i)
    retry_count <- 0
    max_retries <- 3
    
    while (retry_count < max_retries) {
      marker_data <- paste(names(processed_input)[id], ':', processed_input[id], collapse = '\n')
      
      prompt <- glue::glue("Identify detailed cell subtypes for {celltypename} in {species} {tissuename} cells using the following markers, provided separately for each row.
You MUST use standardized cell subtype names from the Cell Ontology (CL). If no exact CL term exists, use the most specific CL term available and add descriptive modifiers.
Only output the cell subtype name. Do not include any numbers or extra annotations before the name. Do not include any explanatory text, introductory phrases, or descriptions.
IMPORTANT: Return exactly {num_clusters} lines, one for each row.
Note: Some rows may represent a mixture of multiple subtypes.

{marker_data}",
                          celltypename = celltypename,
                          species = species,
                          tissuename = tissuename,
                          num_clusters = length(id),
                          marker_data = marker_data)
      
      result <- call_llm_api(model, prompt, temperature = NULL, timeout_seconds = 300, api_key = api_key)
      
      if (!is.null(result)) {
        res <- trimws(unlist(strsplit(result, '\n')))
        res <- res[nzchar(res)]
        
        if (length(res) == length(id)) {
          names(res) <- names(processed_input)[id]
          return(res)
        } else {
          cat("⚠️ Response length mismatch for batch", i, ". Expected:", length(id), "Got:", length(res), "\n")
          
          if (retry_count >= max_retries - 1) {
            valid_results <- min(length(res), length(id))
            final_res <- rep("Failed to annotate", length(id))
            if (valid_results > 0) {
              final_res[1:valid_results] <- res[1:valid_results]
            }
            names(final_res) <- names(processed_input)[id]
            return(final_res)
          }
        }
      }
      
      retry_count <- retry_count + 1
      cat("🔄 Retrying batch", i, "(attempt", retry_count + 1, ")\n")
      Sys.sleep(2)
    }
    
    cat("❌ Failed to get proper response for batch", i, "after", max_retries, "retries\n")
    res <- rep("Failed to annotate", length(id))
    names(res) <- names(processed_input)[id]
    return(res)
  }, simplify = FALSE)
  
  cat("✅ Cell subtype annotation completed\n")
  cat("📝 Note: It is recommended to check results for potential AI hallucinations before downstream analysis\n")
  
  result <- gsub(',$', '', unlist(allres))
  result <- sub("^[^a-zA-Z]*([a-zA-Z].*)", "\\1", result)
  
  return(result)
}