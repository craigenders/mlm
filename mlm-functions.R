boxplots_by_cluster <- function(data, var2plot, lev2id, numboxes) {
  
  # require ggplot2
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("Package 'ggplot2' is required but is not installed. Please install it using install.packages('ggplot2').")
  } else {
    library(ggplot2)
  }
  
  # subset the data to the first 'numboxes' unique level-2 IDs
  ids2plot <- unique(data[[lev2id]])[1:numboxes]
  data2plot <- subset(data, data[[lev2id]] %in% ids2plot)
  
  # create the plot
  p <- ggplot(data2plot, aes(x = factor(.data[[lev2id]]), y = .data[[var2plot]])) +
    geom_boxplot(width = 0.3, outlier.shape = NA, fill = NA, color = "grey") +
    geom_jitter(width = 0.1, height = 0, size = 2, alpha = 0.6) +
    stat_summary(fun = mean, geom = "point", color = "#D95C14", size = 4) +
    labs(x = lev2id, y = var2plot, title = paste(var2plot, "Scores by Cluster")) +
    theme_minimal()
  
  return(p)
}

