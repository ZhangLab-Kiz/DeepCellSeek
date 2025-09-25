# 🧬 DeepCellSeek: Leveraging Large Language Models for Single-Cell RNA-Seq Annotation 🚀

**DeepCellSeek** is a comprehensive R package that leverages Large Language Models (LLMs) to provide accurate cell type and subtype annotations for single-cell RNA-sequencing (scRNA-seq) data. The package implements a novel dual-architecture system offering both free gateway access and direct API integration for flexible deployment scenarios.

## 🔍 Overview

Accurate cell type identification is crucial for interpreting scRNA-seq data but often relies on time-consuming manual annotation or traditional computational methods with inherent limitations. Large Language Models, with their extensive biological knowledge and reasoning capabilities, offer a promising computational alternative for automated cell annotation.

DeepCellSeek emerged from our comprehensive benchmarking study comparing multiple leading LLMs against traditional annotation tools across diverse single-cell datasets. Our findings demonstrated significant advantages of top-performing LLMs in accuracy, consistency, and biological interpretability.

## ✨ Features

* **🏗️ Dual Architecture**: Flexible deployment with both free gateway and local direct API access
* **🤖 Multi-Model Integration**: Integration with industry-leading LLM models (OpenAI GPT, Claude, DeepSeek, Gemini, Grok, Kimi, Doubao)
* **🗳️ Ensemble Voting Mechanism**: Advanced consensus algorithms significantly improve annotation accuracy
* **🔄 Flexible Input**: Support for Seurat FindAllMarkers output, gene lists, and custom formats
* **🧬 Comprehensive Coverage**: Support for both broad cell type and fine-grained subtype annotation
* **📦 R Package Integration**: Seamless integration for R-based single-cell analysis pipelines
* **🌐 Web Tool**: Clean and user-friendly graphical interface for rapid annotation needs

## 🌐 Web Interface

For users preferring a graphical interface, visit our web application:
**https://deepcellseek.kiz.ac.cn**

## 🛠️ Installation

Install the development version from GitHub:

```r
# Install devtools if not already installed
if (!require("devtools")) install.packages("devtools")

# Install DeepCellSeek
devtools::install_github("ZhangLab-Kiz/DeepCellSeek")
```

## 🚀 Quick Start: Workflow with Seurat

The most common use case for DeepCellSeek is integration with Seurat workflows. Here's a complete example from raw data to annotated visualization:

```r
# 1. Load necessary libraries
library(DeepCellSeek)
library(Seurat)

# 2. Prepare Input Data
# Assume you have already run the Seurat pipeline https://satijalab.org/seurat/;
# "seu" is the Seurat object
markers_df <- FindAllMarkers(object = seu)

# 3. Use DeepCellSeek for annotation
# --- Activate Free Gateway Mode (No API key needed) ---
Sys.setenv(USE_DEEPCELLSEEK_API = "TRUE")

# --- Or, to use Local API Mode, set your key instead ---
# Sys.setenv(OPENAI_API_KEY = "your_real_api_key_here")

annotations <- deepcellseek_celltype(
  input = markers_df,
  tissuename = "PBMC",  # Providing tissue context is important
  species = "Human",
  model = "gpt-5"  # Choose any supported model
)

# 4. Seamlessly integrate annotations back into Seurat object
seu@meta.data$LLM_Annotation <- as.factor(results[as.character(Idents(seu))])

# 5. Visualize the LLM-annotated cell types!
DimPlot(seu, group.by = "LLM_Annotation", label = TRUE, repel = TRUE)
```

## 📚 Complete Function Reference

DeepCellSeek provides comprehensive annotation functions for both architectural modes:

### 🆓 Free Gateway Mode Functions

Set `Sys.setenv(USE_DEEPCELLSEEK_API = "TRUE")` to activate free gateway mode:

```r
# Cell type annotation
celltype_results <- deepcellseek_celltype(
  input = markers_df, 
  tissuename = "PBMC", 
  species = "Human",
  model = "gemini-2.0-flash",
  topgenenumber = "10"
)

# Cell subtype annotation  
subtype_results <- deepcellseek_subcelltype(
  input = markers_df,
  tissuename = "PBMC",
  celltypename = "T cell",
  species = "Human",
  model = "gpt-5",
  topgenenumber = "10"
)

# Ensemble voting for cell types
ensemble_celltype <- deepcellseek_ensemble(
  input = markers_df,
  tissuename = "PBMC", 
  species = "Human",
  topgenenumber = "10"
)

# Ensemble voting for subtypes
ensemble_subtype <- deepcellseek_subcelltype_ensemble(
  input = markers_df,
  tissuename = "PBMC",
  celltypename = "T cell",
  species = "Human",
  topgenenumber = "10"
)
```

### 🔧 Local API Mode Functions

Configure your API keys and use direct model calls:

```r
# Set API keys as environment variables
Sys.setenv(DEEPSEEK_API_KEY = "your_deepseek_api_key")
Sys.setenv(OPENAI_API_KEY = "your_openai_api_key")

# Cell type annotation with direct API
celltype_results <- llm_celltype(
  input = markers_df,
  tissuename = "Brain",
  species = "Human", 
  model = "deepseek-reasoner",
  topgenenumber = 10
)

# Cell subtype annotation
subtype_results <- llm_subcelltype(
  input = markers_df,
  tissuename = "Brain",
  species = "Human",
  celltypename = "GABAergic neuron",
  model = "gpt-5",
  topgenenumber = 10
)

# Ensemble cell type annotation with multiple models
# elite_models: List of "elite models" participating in initial predictions
# arbitrator_model: "Arbitrator model" responsible for final decision-making
ensemble_results <- llm_celltype_ensemble(
  input = markers_df,
  tissuename = "Brain",
  species = "Human",
  elite_models = c("kimi-k2-turbo-preview", "gpt-5", "claude-opus-4-1-20250805", "grok-4-0709"), 
  arbitrator_model = "kimi-k2-turbo-preview"
)

# Ensemble subtype annotation with multiple models
ensemble_results <- llm_subcelltype_ensemble(
  input = markers_df,
  tissuename = "Brain",
  species = "Human",
  celltypename = "GABAergic neuron",
  elite_models = c("kimi-k2-turbo-preview", "gpt-5", "claude-opus-4-1-20250805", "grok-4-0709"), 
  arbitrator_model = "kimi-k2-turbo-preview"
)
```

## 🏗️ Architecture

DeepCellSeek implements a **dual-architecture system** providing two distinct annotation pathways:

### 🆓 Mode A: Free API Gateway

This is the simplest and most recommended approach for getting started. With a simple R environment setup, you can delegate all computational tasks to our deployed central server without needing to register for any API keys - completely free.

**Core Advantages:**
- **Zero Cost**: Free access to multiple industry-leading LLM models without paying any API fees
- **Zero Configuration**: No need to manage and protect cumbersome API keys, ready to use out of the box
- **Simple & Fast**: Perfect for quick validation, educational demonstrations, or users who prefer not to handle API keys

### 🔧 Mode B: Local Direct API Calls

This is the default mode, designed for advanced users who need maximum flexibility and control. In this mode, DeepCellSeek uses your locally configured personal API keys to directly call the official interfaces of major LLM providers.

**Core Advantages:**
- **Ultimate Flexibility**: Use any of our predefined models without being limited by gateway support lists. When performing ensemble predictions, freely choose and combine "elite models" and "arbitrator models"
- **Future-Ready Design**: The modular architecture allows for easy integration of new models as they become available from supported providers
- **Complete Control**: Full control over API calls, data privacy, and computational resources

#### 🔑 API Key Configuration

DeepCellSeek supports the following LLM providers. Please set the corresponding environment variables:

- **DeepSeek**: `DEEPSEEK_API_KEY`
- **Claude (Anthropic)**: `CLAUDE_API_KEY`  
- **GPT (OpenAI)**: `OPENAI_API_KEY`
- **Gemini (Google)**: `GEMINI_API_KEY`
- **Grok (xAI)**: `GROK_API_KEY`
- **Kimi (Moonshot AI)**: `KIMI_API_KEY`
- **Doubao (ByteDance/VolcEngine)**: `DOUBAO_API_KEY`

```r
# Example: Setting API keys
Sys.setenv(DEEPSEEK_API_KEY = "your_deepseek_api_key")
Sys.setenv(OPENAI_API_KEY = "your_openai_api_key")
```

## 🤖 Supported Models

### 🆓 Free Gateway Mode
- **OpenAI**: `gpt-4o`, `gpt-5`
- **DeepSeek**: `deepseek-reasoner`, `deepseek-chat`
- **Claude**: `claude-3-7-sonnet-20250219`, `claude-opus-4-1-20250805`
- **Gemini**: `gemini-2.0-flash`, `gemini-2.5-flash`
- **Kimi**: `kimi-k2-turbo-preview`
- **Doubao**: `doubao-seed-1-6-250615`

### 🔧 Local API Mode
- **OpenAI**: `gpt-4o`, `gpt-5`, `gpt-4.1`
- **DeepSeek**: `deepseek-reasoner`, `deepseek-chat`
- **Claude**: `claude-3-7-sonnet-20250219`, `claude-sonnet-4-20250514`, `claude-opus-4-20250514`, `claude-opus-4-1-20250805`
- **Gemini**: `gemini-2.0-flash`, `gemini-2.5-flash`
- **Grok**: `grok-2-1212`, `grok-3`, `grok-4-0709`
- **Kimi**: `moonshot-v1-128k`, `kimi-k2-turbo-preview`
- **Doubao**: `doubao-1-5-pro-256k-250115`, `doubao-seed-1-6-250615`

🔮 We plan to incorporate support for additional models in future updates. Stay tuned! 👀

## 📚 Citation

If you use DeepCellSeek in your research, please cite our work:
```
[Paper information to be added]
```

## 📝 License

This project is licensed under the MIT License - see the [LICENSE](https://github.com/ZhangLab-Kiz/DeepCellSeek/blob/main/LICENSE) file for details. 

## 📬 Support and Contact

- **Primary Contact**: Dr. Chao Zhang (zhangchao@mail.kiz.ac.cn)
- **Developer Contact**: Tianxiang Xiao (xiaotianxiang251@mails.ucas.ac.cn)
- **Issues**: Please report bugs and feature requests via [GitHub Issues](https://github.com/YourUsername/DeepCellSeek/issues)

