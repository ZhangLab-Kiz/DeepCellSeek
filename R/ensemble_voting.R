#' Ensemble Voting for Cell Type Annotation
#' 
#' This module implements ensemble voting functionality using multiple LLM models
#' with intelligent arbitration for improved accuracy.

#' Cell Type Ensemble Annotation using Multiple LLMs
#' 
#' @param input Input data for cell type annotation (data frame or named list)
#' @param tissuename Tissue name (e.g., "Brain", "Lung")
#' @param species Species name (e.g., "Human", "Mouse") 
#' @param elite_models Vector of model names to use for initial predictions
#' @param arbitrator_model Model name to use for final arbitration
#' @param topgenenumber Number of top genes to consider per cluster
#' @param parallel Whether to call models in parallel (default: TRUE)
#' @param timeout_seconds Timeout for each API call (default: 300)
#' @return Named vector of cell type annotations
#' @export
llm_celltype_ensemble <- function(input, 
                                 tissuename, 
                                 species = "Human",
                                 elite_models = c("kimi-k2-turbo-preview", "gpt-5", "claude-opus-4-1-20250805", "grok-4-0709"),
                                 arbitrator_model = "kimi-k2-turbo-preview",
                                 topgenenumber = 10,
                                 parallel = TRUE,
                                 timeout_seconds = 300) {
  
  cat("🗳️ Starting ensemble voting for", species, tissuename, "cell type annotation\n")
  cat("🤖 Elite models:", paste(elite_models, collapse = ", "), "\n")
  cat("⚖️ Arbitrator model:", arbitrator_model, "\n")

  processed_input <- process_input_data(input, topgenenumber)
  num_clusters <- length(processed_input)
  
  cat("📊 Processing", num_clusters, "cell clusters\n")

  elite_predictions <- get_elite_predictions(
    processed_input, 
    tissuename, 
    species,
    elite_models, 
    parallel, 
    timeout_seconds
  )
  
  if (length(elite_predictions) == 0) {
    stop("❌ Failed to get predictions from any elite model")
  }

  arbitration_data <- prepare_arbitration_data(elite_predictions, processed_input)

  final_result <- get_arbitrator_decision(
    arbitration_data,
    tissuename,
    species, 
    arbitrator_model,
    num_clusters,
    timeout_seconds,
    names(processed_input)
  )
  
  if (is.null(final_result)) {
    stop("❌ Arbitrator failed to provide final decision")
  }
  
  cat("✅ Ensemble voting completed successfully\n")
  return(final_result)
}

#' Cell Subtype Ensemble Annotation using Multiple LLMs
#' 
#' @param input Input data for cell subtype annotation
#' @param tissuename Tissue name
#' @param species Species name
#' @param celltypename Parent cell type name
#' @param elite_models Vector of model names for initial predictions
#' @param arbitrator_model Model for final arbitration
#' @param topgenenumber Number of top genes to consider
#' @param parallel Whether to use parallel processing
#' @param timeout_seconds API timeout
#' @return Named vector of cell subtype annotations
#' @export
llm_subcelltype_ensemble <- function(input,
                                    tissuename,
                                    species = "Human", 
                                    celltypename,
                                    elite_models = c("kimi-k2-turbo-preview", "gpt-5", "claude-opus-4-1-20250805", "grok-4-0709"),
                                    arbitrator_model = "kimi-k2-turbo-preview",
                                    topgenenumber = 10,
                                    parallel = TRUE,
                                    timeout_seconds = 300) {
  
  cat("🗳️ Starting ensemble voting for", species, tissuename, celltypename, "subtype annotation\n")

  processed_input <- process_input_data(input, topgenenumber)
  num_clusters <- length(processed_input)

  elite_predictions <- get_elite_subtype_predictions(
    processed_input,
    tissuename,
    species,
    celltypename,
    elite_models,
    parallel,
    timeout_seconds
  )
  
  if (length(elite_predictions) == 0) {
    stop("❌ Failed to get predictions from any elite model")
  }

  arbitration_data <- prepare_arbitration_data(elite_predictions, processed_input)

  final_result <- get_arbitrator_subtype_decision(
    arbitration_data,
    tissuename,
    species,
    celltypename,
    arbitrator_model,
    num_clusters,
    timeout_seconds,
    names(processed_input)
  )
  
  if (is.null(final_result)) {
    stop("❌ Arbitrator failed to provide final decision")
  }
  
  cat("✅ Ensemble subtype voting completed successfully\n")
  return(final_result)
}

get_elite_subtype_predictions <- function(processed_input, tissuename, species, celltypename, elite_models, parallel, timeout_seconds) {
  
  marker_data <- paste(names(processed_input), ':', processed_input, collapse = '\n')
  
  prompt <- glue::glue("Identify cell types of {species} {tissuename} cells using the following markers separately for each row.
You MUST use standardized cell type names from the Cell Ontology (CL). If no exact CL term exists, use the most specific CL term available and add descriptive modifiers.
Only provide the cell type name. Do not include any numbers or extra annotations before the name. Do not include any explanatory text, introductory phrases, or descriptions.
IMPORTANT: Return exactly {num_clusters} lines, one for each row.
Some can be a mixture of multiple cell types.

{marker_data}",
                      species = species,
                      tissuename = tissuename, 
                      num_clusters = length(processed_input),
                      marker_data = marker_data)

  processed_predictions <- list()
  
  for (model in elite_models) {
    cat("🤖 Calling elite model:", model, "\n")
    
    result <- call_llm_api(model, prompt, temperature = NULL, timeout_seconds = timeout_seconds)
    
    if (!is.null(result) && result != "") {
      lines <- trimws(unlist(strsplit(result, "\n")))
      lines <- lines[lines != ""]
      
      if (length(lines) == length(processed_input)) {
        processed_predictions[[model]] <- lines
        cat("✅", model, "provided", length(lines), "predictions\n")
      } else {
        cat("⚠️", model, "returned", length(lines), "predictions, expected", length(processed_input), "\n")
      }
    } else {
      cat("❌", model, "failed to provide predictions\n")
    }
  }
  
  return(processed_predictions)
}

get_elite_subtype_predictions <- function(processed_input, tissuename, species, celltypename, elite_models, parallel, timeout_seconds) {
  
  marker_data <- paste(names(processed_input), ':', processed_input, collapse = '\n')
  
  prompt <- glue::glue("Identify detailed cell subtypes for {celltypename} in {species} {tissuename} cells using the following markers, provided separately for each row.
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

  processed_predictions <- list()
  
  for (model in elite_models) {
    cat("🤖 Calling elite model for subtypes:", model, "\n")
    
    result <- call_llm_api(model, prompt, temperature = NULL, timeout_seconds = timeout_seconds)
    
    if (!is.null(result) && result != "") {
      lines <- trimws(unlist(strsplit(result, "\n")))
      lines <- lines[lines != ""]
      
      if (length(lines) == length(processed_input)) {
        processed_predictions[[model]] <- lines
        cat("✅", model, "provided", length(lines), "subtype predictions\n")
      } else {
        cat("⚠️", model, "returned", length(lines), "predictions, expected", length(processed_input), "\n")
      }
    } else {
      cat("❌", model, "failed to provide subtype predictions\n")
    }
  }
  
  return(processed_predictions)
}

prepare_arbitration_data <- function(elite_predictions, processed_input) {
  
  cluster_names <- names(processed_input)
  prediction_lines <- c()
  
  for (i in 1:length(cluster_names)) {
    cluster_name <- cluster_names[i]

    model_predictions <- list()
    for (j in 1:length(elite_predictions)) {
      model_name <- paste0("LLM", j)
      model_results <- elite_predictions[[j]]
      
      if (i <= length(model_results)) {
        model_predictions[[model_name]] <- model_results[i]
      } else {
        model_predictions[[model_name]] <- "unknown"
      }
    }

    pred_parts <- paste0(names(model_predictions), ":", model_predictions)
    cluster_line <- paste0(i, ": ", paste(pred_parts, collapse = ", "))
    prediction_lines <- c(prediction_lines, cluster_line)
  }
  
  return(paste(prediction_lines, collapse = "\n"))
}

get_arbitrator_decision <- function(arbitration_data, tissuename, species, arbitrator_model, num_clusters, timeout_seconds, cluster_names) {
  
  arbitration_prompt <- glue::glue("
Integrate multiple AI model predictions to determine the final cell type for {species} {tissuename} cells. Predictions are provided separately for each cell type.
Do not consolidate predictions that have a hierarchical relationship (e.g., parent-child classes in Cell Ontology) into a single entity.
Before voting, filter out any prediction whose Cell Ontology ID is not a descendant of the declared lineage root for {species} {tissuename}.
Determine the final annotation by majority vote. If this vote results in a tie, select the prediction from the first model listed.
You MUST return a standardized cell type name from the Cell Ontology (CL). Consolidate synonymous predictions into a single entity before voting.
Only output the final cell type name. Do not include any numbers or extra annotations before the name. Do not include any explanatory text, introductory phrases, or descriptions.
IMPORTANT: Return exactly {num_clusters} lines, one for each row.

{arbitration_data}",
                                 species = species,
                                 tissuename = tissuename,
                                 num_clusters = num_clusters,
                                 arbitration_data = arbitration_data)
  
  cat("⚖️ Calling arbitrator model for final decision...\n")
  
  result <- call_llm_api(arbitrator_model, arbitration_prompt, temperature = NULL, timeout_seconds = timeout_seconds)
  
  if (is.null(result) || result == "") {
    return(NULL)
  }

  result_lines <- trimws(unlist(strsplit(result, "\n")))
  result_lines <- result_lines[result_lines != ""]
  
  if (length(result_lines) != num_clusters) {
    cat("⚠️ Arbitrator returned", length(result_lines), "results, expected", num_clusters, "\n")
    return(NULL)
  }

  final_results <- sub("^[^a-zA-Z]*([a-zA-Z].*)", "\\1", result_lines)
  names(final_results) <- cluster_names
  
  return(final_results)
}

get_arbitrator_subtype_decision <- function(arbitration_data, tissuename, species, celltypename, arbitrator_model, num_clusters, timeout_seconds, cluster_names) {
  
  arbitration_prompt <- glue::glue("
Integrate multiple AI model predictions to determine the final cell subtype for {celltypename} in {species} {tissuename} cells. Predictions are provided separately for each cell subtype.
Do not consolidate predictions that have a hierarchical relationship (e.g., parent-child classes in Cell Ontology) into a single entity.
Before voting, filter out any prediction whose Cell Ontology ID is not a descendant of the declared lineage root for {celltypename} in {species} {tissuename}.
Determine the final annotation by majority vote. If this vote results in a tie, select the prediction from the first model listed.
You MUST return a standardized cell subtype name from the Cell Ontology (CL). Consolidate synonymous predictions into a single entity before voting.
Only output the final cell subtype name. Do not include any numbers or extra annotations before the name. Do not include any explanatory text, introductory phrases, or descriptions.
IMPORTANT: Return exactly {num_clusters} lines, one for each row.

{arbitration_data}",
                                 celltypename = celltypename,
                                 species = species,
                                 tissuename = tissuename,
                                 num_clusters = num_clusters,
                                 arbitration_data = arbitration_data)
  
  cat("⚖️ Calling arbitrator model for final subtype decision...\n")
  
  result <- call_llm_api(arbitrator_model, arbitration_prompt, temperature = NULL, timeout_seconds = timeout_seconds)
  
  if (is.null(result) || result == "") {
    return(NULL)
  }

  result_lines <- trimws(unlist(strsplit(result, "\n")))
  result_lines <- result_lines[result_lines != ""]
  
  if (length(result_lines) != num_clusters) {
    cat("⚠️ Arbitrator returned", length(result_lines), "results, expected", num_clusters, "\n")
    return(NULL)
  }

  final_results <- sub("^[^a-zA-Z]*([a-zA-Z].*)", "\\1", result_lines)
  names(final_results) <- cluster_names
  
  return(final_results)
}