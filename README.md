# 🧬 DeepCellSeek: Leveraging Large Language Models for Single-Cell RNA-Seq Annotation 🚀

**DeepCellSeek** utilizes the power of Large Language Models (LLMs) to provide accurate cell type and subtype annotations for single-cell RNA-sequencing (scRNA-seq) data based on marker gene lists.

## 🔍 Overview

Accurate cell type identification is crucial for interpreting scRNA-seq data but often relies on time-consuming manual annotation or traditional computational methods with limitations. Large Language Models, with their extensive knowledge integration and reasoning capabilities, offer a promising alternative.

DeepCellSeek emerged from our comprehensive benchmarking study comparing seven leading LLMs against traditional tools for cell type and subtype annotation across diverse datasets. Our findings demonstrated the significant advantages of top-performing LLMs.

This repository contains the **R package** implementation of DeepCellSeek, allowing users to leverage multiple LLM APIs directly within their R workflow. We also provide a user-friendly **online web tool** for easy access. 🌐


## ✨ Features

* **LLM-Powered Annotation:** 🚀 Utilizes state-of-the-art LLMs for cell type and subtype annotation
* **Multi-Model Support:** 🤖 Access predictions from various leading LLMs (DeepSeek, Claude, GPT, Gemini, Grok, Kimi, Doubao)
* **Flexible Input:** 🔄 Accepts marker gene lists as named lists/vectors or data frames
* **Broad Celltype & Cell Subtype Annotation:** 🧬 Supports both broad cell type identification and fine-grained subtype discrimination
* **R Package:** 📦 Provides functions for seamless integration into R-based single-cell analysis pipelines
* **Web Tool:** 🌐 Offers an easy-to-use graphical interface for quick annotation tasks


## 🌐 Online Web Tool

For a convenient graphical interface without needing installation, please visit our web tool:

**[DeepCellSeek Web Tool](https://deepcellseek.kiz.ac.cn/)**

<!-- Can add a screenshot of your web tool here 📸 -->


## 🛠️ R Package Installation

You can install the development version of DeepCellSeek from GitHub using `devtools`:

```R
# install.packages("devtools") 
devtools::install_github("ZhangLab-Kiz/DeepCellSeek")
```

## 🧑‍💻 R Package Usage

### 1. 🔑 Setup API Keys

Accessing LLM APIs requires API keys. Obtain keys for the models you wish to use from their respective providers (DeepSeek, Anthropic, OpenAI, Google AI, xAI, Moonshot AI, ByteDance/VolcEngine).

Store keys securely as environment variables. The functions `llm_celltype` and `llm_subcelltype` will automatically look for them using the following naming convention:

- **DeepSeek:** `DEEPSEEK_API_KEY`
- **Claude (Anthropic):** `CLAUDE_API_KEY`
- **GPT (OpenAI):** `OPENAI_API_KEY`
- **Gemini (Google):** `GEMINI_API_KEY`
- **Grok (xAI):** `GROK_API_KEY`
- **Kimi (Moonshot AI):** `KIMI_API_KEY`
- **Doubao (ByteDance/VolcEngine):** `DOUBAO_API_KEY`

You can set environment variables using `Sys.setenv()` for the current session:

```R
# Sys.setenv(DEEPSEEK_API_KEY = "your_deepseek_api_key")
# Sys.setenv(OPENAI_API_KEY = "your_openai_api_key")
```

If an API key is not found as an environment variable and not provided directly via the api_key argument, the function will return the generated prompt instead of making the API call.

### 2. 📊 Prepare Input Data

The input argument for `llm_celltype` and `llm_subcelltype` can be:

**A data frame:** Typically the output from `Seurat::FindAllMarkers` or similar, containing at least columns named `cluster` and `gene`. The function will automatically select the top `topgenenumber` genes per cluster based on the input data frame's ordering.

```R
# Assume you have already run the Seurat pipeline https://satijalab.org/seurat/; "seu" is the Seurat object
markers_df <- FindAllMarkers(object = seu)
```

### 3. 🏷️ Run Annotation Functions

#### 🧬 Cell Type Annotation

```R
library(DeepCellSeek)

results <- llm_celltype(
  input = markers_df, # marker_vec,
  tissuename = "Colon",
  model = "claude-3-7-sonnet-20250219", 
  topgenenumber = 10 # Top 10 genes recommended (this is also the default)
)

print(results)
```

#### 🧬 Cell Subtype Annotation

```R
library(DeepCellSeek)

results <- llm_subcelltype(
  input = markers_df, # marker_vec,
  tissuename = "Brain",
  celltypename = "GABAergic", # Specify the parent type
  model = "gemini-2.0-flash",
  topgenenumber = 10 # Top 10 genes recommended (this is also the default)
)

print(results)
```

Note: The functions print status messages regarding API key detection and include a final note recommending users to check results for potential AI hallucinations before downstream analysis.

### 4. 🔄 Integrate Annotations for Downstream Analysis

```R
# Add the annotations as a new metadata column
seu@meta.data$LLM_Annotation <- as.factor(results[as.character(Idents(seu))])

DimPlot(seu, group.by = "LLM_Annotation", label = TRUE, repel = TRUE)
```

You can now proceed with further downstream analyses using the 'LLM_Annotation' metadata column

## 🧠 Supported LLMs

DeepCellSeek R package or web tool currently supports API calls to the following models (*Confirm exact supported model name*):

* **DeepSeek:** `deepseek-reasoner`
* **Claude (Anthropic):** `claude-3-7-sonnet-20250219`
* **GPT (OpenAI):** `gpt-4o`
* **Gemini (Google):** `gemini-2.0-flash`
* **Grok (xAI):** `grok-2-1212`
* **Kimi (Moonshot AI):** `moonshot-v1-128k`
* **Doubao (ByteDance/VolcEngine):** `doubao-1-5-pro-256k-250115`

🔮 We plan to incorporate support for additional models in future updates. Stay tuned! 👀

## 📚 Citation

If you use DeepCellSeek in your research, please cite our work:
```
[Paper information to be added]
```

## 📝 License

This project is licensed under the MIT License - see the [LICENSE](https://github.com/ZhangLab-Kiz/DeepCellSeek/blob/main/LICENSE) file for details. 

## 📬 Contact

Authors: Tianxiang Xiao (x1058513236@gmail.com), Chao Zhang (zhangchao@mail.kiz.ac.cn).

For questions or issues, please open an [issue](https://github.com/ZhangLab-Kiz/DeepCellSeek/issues) on this GitHub repository or contact Dr. Chao Zhang (zhangchao@mail.kiz.ac.cn). 📧

