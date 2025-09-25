#' Unified API Client for LLM Services
#' 
#' This module provides a unified interface for calling different LLM APIs
#' with standardized error handling and response processing.

#' Get effective temperature for a specific model
#' 
#' @param model Model name
#' @param requested_temperature The temperature requested by user
#' @return Effective temperature to use for this model
get_effective_temperature <- function(model, requested_temperature) {
  if (grepl("gpt-5", model, ignore.case = TRUE)) {
    return(1.0)
  }

  return(0.0)
}

#' Get model provider from model name
#' 
#' @param model Model name
#' @return Provider name
get_model_provider <- function(model) {
  config <- get_model_config(model)
  if (is.null(config)) {
    return(NULL)
  }
  return(config$provider)
}

#' Make API call to LLM service
#' 
#' @param model Model name to use
#' @param prompt Text prompt to send
#' @param temperature Sampling temperature (default: 0.0)
#' @param timeout_seconds Request timeout in seconds (default: 300)
#' @param api_key Optional API key (if not provided, will use environment variable)
#' @return API response content or NULL if failed
call_llm_api <- function(model, prompt, temperature = NULL, timeout_seconds = 300, api_key = NULL) {

  config <- get_model_config(model)
  if (is.null(config)) {
    stop("❌ Model not supported: ", model)
  }
  
  if (is.null(temperature)) {
    temperature <- 0.0
  }

  effective_temperature <- get_effective_temperature(model, temperature)

  if (effective_temperature != temperature) {
    cat("🌡️ Model", model, "using temperature", effective_temperature, "instead of requested", temperature, "\n")
  }
  
  model_type <- get_model_provider(model)

  if (is.null(api_key)) {
    api_key <- Sys.getenv(config$env_var)
    if (api_key == "") {
      cat("❌ API key not found for", config$provider, "\n")
      return(NULL)
    }
  }
  
  cat("🔌 Calling", config$provider, "API with model:", model, "\n")
  
  tryCatch({
    
    if (config$provider == "openai") {
      result <- call_openai_api(config, model, prompt, effective_temperature, timeout_seconds, api_key)
      
    } else if (config$provider == "deepseek") {
      result <- call_deepseek_api(config, model, prompt, effective_temperature, timeout_seconds, api_key)
      
    } else if (config$provider == "claude") {
      result <- call_claude_api(config, model, prompt, effective_temperature, timeout_seconds, api_key)
      
    } else if (config$provider == "gemini") {
      result <- call_gemini_api(config, model, prompt, effective_temperature, timeout_seconds, api_key)
      
    } else if (config$provider == "grok") {
      result <- call_grok_api(config, model, prompt, effective_temperature, timeout_seconds, api_key)
      
    } else if (config$provider == "kimi") {
      result <- call_kimi_api(config, model, prompt, effective_temperature, timeout_seconds, api_key)
      
    } else if (config$provider == "doubao") {
      result <- call_doubao_api(config, model, prompt, effective_temperature, timeout_seconds, api_key)
      
    } else {
      stop("❌ Provider not implemented: ", config$provider)
    }
    
    cat("✅ API call successful\n")
    return(result)
    
  }, error = function(e) {
    cat("❌ API call failed for", config$provider, ":", e$message, "\n")
    return(NULL)
  })
}

call_openai_api <- function(config, model, prompt, temperature, timeout_seconds, api_key) {
  timeout_seconds <- if (!is.null(config$api$timeout)) config$api$timeout else 360
  
  response <- httr::POST(
    "https://api.openai.com/v1/chat/completions",
    httr::add_headers(
      "Content-Type" = "application/json",
      "Authorization" = paste("Bearer", api_key)
    ),
    body = jsonlite::toJSON(list(
      model = model,
      messages = list(
        list(role = "user", content = prompt)
      ),
      temperature = temperature
    ), auto_unbox = TRUE),
    httr::timeout(timeout_seconds),
    httr::config(connecttimeout = 360),
    encode = "raw"
  )
  
  if (httr::status_code(response) == 200) {
    resp_content <- httr::content(response, "text", encoding = "UTF-8")
    resp_json <- jsonlite::fromJSON(resp_content, flatten = TRUE)
    return(resp_json$choices$message.content[1])
  } else {
    stop("HTTP error: ", httr::status_code(response))
  }
}

call_deepseek_api <- function(config, model, prompt, temperature, timeout_seconds, api_key) {
  req <- httr2::request(config$endpoint) |>
    httr2::req_headers(
      "Content-Type" = "application/json",
      !!config$auth_header := paste(config$auth_prefix, api_key)
    ) |>
    httr2::req_body_json(list(
      model = model,
      messages = list(list(role = "user", content = prompt)),
      temperature = temperature
    )) |>
    httr2::req_timeout(timeout_seconds) |>
    httr2::req_perform()
  
  resp <- httr2::resp_body_json(req)
  return(resp$choices[[1]]$message$content)
}

call_claude_api <- function(config, model, prompt, temperature, timeout_seconds, api_key) {
  req <- httr2::request(config$endpoint) |>
    httr2::req_headers(
      "Content-Type" = "application/json",
      !!config$auth_header := api_key,
      "anthropic-version" = "2023-06-01"
    ) |>
    httr2::req_body_json(list(
      model = model,
      messages = list(list(role = "user", content = prompt)),
      temperature = temperature,
      max_tokens = config$max_tokens %||% 4096
    )) |>
    httr2::req_timeout(timeout_seconds) |>
    httr2::req_perform()
  
  resp <- httr2::resp_body_json(req)
  return(resp$content[[1]]$text)
}

call_gemini_api <- function(config, model, prompt, temperature, timeout_seconds, api_key) {
  endpoint <- paste0(config$endpoint, "/", model, ":generateContent?key=", api_key)
  
  req <- httr2::request(endpoint) |>
    httr2::req_headers("Content-Type" = "application/json") |>
    httr2::req_body_json(list(
      contents = list(list(parts = list(list(text = prompt)))),
      generationConfig = list(temperature = temperature)
    )) |>
    httr2::req_timeout(timeout_seconds) |>
    httr2::req_perform()
  
  resp <- httr2::resp_body_json(req)
  return(resp$candidates[[1]]$content$parts[[1]]$text)
}

call_grok_api <- function(config, model, prompt, temperature, timeout_seconds, api_key) {
  req <- httr2::request(config$endpoint) |>
    httr2::req_headers(
      "Content-Type" = "application/json",
      !!config$auth_header := paste(config$auth_prefix, api_key)
    ) |>
    httr2::req_body_json(list(
      model = model,
      messages = list(list(role = "user", content = prompt)),
      temperature = temperature
    )) |>
    httr2::req_timeout(timeout_seconds) |>
    httr2::req_perform()
  
  resp <- httr2::resp_body_json(req)
  return(resp$choices[[1]]$message$content)
}

call_kimi_api <- function(config, model, prompt, temperature, timeout_seconds, api_key) {
  req <- httr2::request(config$endpoint) |>
    httr2::req_headers(
      "Content-Type" = "application/json",
      !!config$auth_header := paste(config$auth_prefix, api_key)
    ) |>
    httr2::req_body_json(list(
      model = model,
      messages = list(list(role = "user", content = prompt)),
      temperature = temperature
    )) |>
    httr2::req_timeout(timeout_seconds) |>
    httr2::req_perform()
  
  resp <- httr2::resp_body_json(req)
  return(resp$choices[[1]]$message$content)
}

call_doubao_api <- function(config, model, prompt, temperature, timeout_seconds, api_key) {
  req <- httr2::request(config$endpoint) |>
    httr2::req_headers(
      "Content-Type" = "application/json",
      !!config$auth_header := paste(config$auth_prefix, api_key)
    ) |>
    httr2::req_body_json(list(
      model = model,
      messages = list(list(role = "user", content = prompt)),
      temperature = temperature
    )) |>
    httr2::req_timeout(timeout_seconds) |>
    httr2::req_perform()
  
  resp <- httr2::resp_body_json(req)
  return(resp$choices[[1]]$message$content)
}

#' Call multiple models in parallel
#' 
#' @param models Vector of model names
#' @param prompt Text prompt to send to all models
#' @param temperature Sampling temperature (will be adjusted per model)
#' @param timeout_seconds Request timeout
#' @return Named list of results (model_name -> response)
call_models_parallel <- function(models, prompt, temperature = NULL, timeout_seconds = 300) {
  
  if (!requireNamespace("future", quietly = TRUE)) {
    stop("Package 'future' is required for parallel processing. Please install it.")
  }
  
  if (is.null(temperature)) {
    temperature <- 0.0
  }

  future::plan(future::multisession, workers = min(length(models), 4))

  futures <- list()
  for (model in models) {
    futures[[model]] <- future::future({
      call_llm_api(model, prompt, temperature, timeout_seconds)
    }, seed = TRUE)
  }

  results <- list()
  for (model in names(futures)) {
    tryCatch({
      results[[model]] <- future::value(futures[[model]])
    }, error = function(e) {
      cat("❌ Failed to get result from", model, ":", e$message, "\n")
      results[[model]] <- NULL
    })
  }

  future::plan(future::sequential)

  successful_results <- results[!sapply(results, is.null)]
  
  return(successful_results)
}