#' Data Processing Utilities for DeepCellSeek
#' 
#' This module provides common utility functions used across different
#' components of the DeepCellSeek package.

#' Process input data for cell annotation
#' 
#' This function standardizes different input formats (data frames, lists, vectors)
#' into a consistent format for downstream processing by both local API calls 
#' and DeepCellSeek gateway API calls.
#' 
#' @param input Input data in various formats:
#'   - data.frame: Seurat FindAllMarkers output with columns: gene, cluster, avg_log2FC, p_val_adj
#'   - list: Named list where each element contains gene names for a cluster
#'   - character: Already processed gene list
#' @param topgenenumber Number of top genes to select per cluster (default: 10)
#' @return Named vector where names are cluster identifiers and values are 
#'   comma-separated gene lists
#' @export
process_input_data <- function(input, topgenenumber = 10) {
  # Handle character input or list input that's already processed
  if (is.character(input) || (is.list(input) && !is.data.frame(input))) {
    if (is.character(input)) {
      processed <- input
    } else {
      # Convert list to comma-separated strings
      processed <- sapply(input, paste, collapse = ',')
    }
  } 
  # Handle data.frame input (typical Seurat FindAllMarkers output)
  else if (is.data.frame(input)) {
    # Check if required columns exist
    required_cols <- c("gene", "cluster", "avg_log2FC", "p_val_adj")
    missing_cols <- setdiff(required_cols, names(input))
    if (length(missing_cols) > 0) {
      stop("❌ Input data.frame is missing required columns: ", 
           paste(missing_cols, collapse = ", "))
    }
    
    # Process with or without dplyr
    if (!requireNamespace("dplyr", quietly = TRUE)) {
      # Base R processing
      input <- input[input$avg_log2FC > 0, , drop = FALSE]
      processed <- tapply(input$gene, list(input$cluster), 
                         function(i) paste0(i[1:min(length(i), topgenenumber)], collapse = ','))
    } else {
      # dplyr processing for more sophisticated filtering
      top_genes <- input %>%
        dplyr::filter(p_val_adj < 2.2e-16, avg_log2FC > 0) %>%
        dplyr::group_by(cluster) %>%
        dplyr::arrange(desc(avg_log2FC), .by_group = TRUE) %>%
        dplyr::slice_head(n = topgenenumber) %>%
        dplyr::ungroup()
      
      processed <- tapply(top_genes$gene, list(top_genes$cluster), 
                         function(i) paste0(i, collapse = ','))
    }
  } 
  else {
    stop("❌ Input must be either a data frame with gene, cluster, avg_log2FC, p_val_adj columns or a processed gene list")
  }
  
  return(processed)
}