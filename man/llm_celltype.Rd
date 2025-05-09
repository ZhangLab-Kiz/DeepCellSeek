% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/cell_annotation.R
\name{llm_celltype}
\alias{llm_celltype}
\title{Cell Type Annotation using LLMs}
\usage{
llm_celltype(
  input,
  tissuename = NULL,
  model = "deepseek-reasoner",
  topgenenumber = 10,
  api_key = NULL
)
}
\arguments{
\item{input}{Input data for cell type annotation}

\item{tissuename}{Optional tissue name}

\item{model}{Model to use for annotation}

\item{topgenenumber}{Number of top genes to consider}

\item{api_key}{API key for the LLM service}
}
\value{
Cell type annotation results
}
\description{
Cell Type Annotation using LLMs
}
