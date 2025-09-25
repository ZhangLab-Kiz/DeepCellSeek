#' Model Configuration Management
#' 
#' This module manages the configuration of different LLM providers and models
#' to ensure easy maintenance and extensibility.

MODEL_CONFIGS <- list(
  openai = list(
    endpoint = "https://api.openai.com/v1/chat/completions",
    auth_header = "Authorization",
    auth_prefix = "Bearer",
    env_var = "OPENAI_API_KEY",
    models = c("gpt-4o", "gpt-5", "gpt-4.1"),
    default_model = "gpt-4o"
  ),
  
  deepseek = list(
    endpoint = "https://api.deepseek.com/v1/chat/completions",
    auth_header = "Authorization", 
    auth_prefix = "Bearer",
    env_var = "DEEPSEEK_API_KEY",
    models = c("deepseek-reasoner", "deepseek-chat"),
    default_model = "deepseek-reasoner"
  ),
  
  claude = list(
    endpoint = "https://api.anthropic.com/v1/messages",
    auth_header = "x-api-key",
    auth_prefix = "",
    env_var = "CLAUDE_API_KEY",
    models = c("claude-3-7-sonnet-20250219", "claude-sonnet-4-20250514", "claude-opus-4-20250514", "claude-opus-4-1-20250805"),
    default_model = "claude-3-7-sonnet-20250219",
    extra_headers = list("anthropic-version" = "2023-06-01"),
    max_tokens = 32000
  ),
  
  gemini = list(
    endpoint = "https://generativelanguage.googleapis.com/v1/models",
    auth_method = "query_param",
    env_var = "GEMINI_API_KEY",
    models = c("gemini-2.0-flash", "gemini-2.5-flash"),
    default_model = "gemini-2.0-flash"
  ),
  
  grok = list(
    endpoint = "https://api.x.ai/v1/chat/completions",
    auth_header = "Authorization",
    auth_prefix = "Bearer", 
    env_var = "GROK_API_KEY",
    models = c("grok-2-1212", "grok-3", "grok-4-0709"),
    default_model = "grok-2-1212"
  ),
  
  kimi = list(
    endpoint = "https://api.moonshot.cn/v1/chat/completions",
    auth_header = "Authorization",
    auth_prefix = "Bearer",
    env_var = "KIMI_API_KEY", 
    models = c("moonshot-v1-128k", "kimi-k2-turbo-preview"),
    default_model = "kimi-k2-turbo-preview"
  ),
  
  doubao = list(
    endpoint = "https://ark.cn-beijing.volces.com/api/v3/chat/completions",
    auth_header = "Authorization",
    auth_prefix = "Bearer",
    env_var = "DOUBAO_API_KEY",
    models = c("doubao-1-5-pro-256k-250115", "doubao-seed-1-6-250615"),
    default_model = "doubao-seed-1-6-250615"
  )
)

#' Get model configuration by model name
#' 
#' @param model Model name to lookup
#' @return List containing model configuration or NULL if not found
get_model_config <- function(model) {
  for (provider in names(MODEL_CONFIGS)) {
    config <- MODEL_CONFIGS[[provider]]
    if (model %in% config$models || grepl(paste0("^", provider), model)) {
      config$provider <- provider
      config$model <- model
      return(config)
    }
  }
  return(NULL)
}

#' Get all supported models
#' 
#' @return Named list of all supported models grouped by provider
get_supported_models <- function() {
  result <- list()
  for (provider in names(MODEL_CONFIGS)) {
    result[[provider]] <- MODEL_CONFIGS[[provider]]$models
  }
  return(result)
}

#' Validate model exists
#' 
#' @param model Model name to validate
#' @return TRUE if model is supported, FALSE otherwise
is_model_supported <- function(model) {
  return(!is.null(get_model_config(model)))
}