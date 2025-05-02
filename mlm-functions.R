################################################################################
# function to graph box plots by cluster
################################################################################

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

################################################################################
# function to compute wald-based chibar test of random slopes
################################################################################

chibar_test <- function(model, raneff = NULL, print = TRUE) {
  
  # error handling: if raneff is "Intercept", exit with an appropriate message
  if (!is.null(raneff) && any(tolower(raneff) == "intercept")) {
    return("This function currently supports tests of random slopes only.")
  }
  
  # get all column names from the model's iterations slot.
  col_names <- names(model@iterations)
  
  # count number of level-2 random effect parameters
  raneff_pattern <- "(?i)level-2 intercept covariance with|level-2 covariance between|level-2 slope variance|level-2 intercept variance"
  raneff_matches <- grep(raneff_pattern, col_names, perl = TRUE, value = TRUE)
  total_raneff <- length(raneff_matches)
  
  # count total number of random slopes
  slp_pattern <- "(?i)level-2 slope variance"
  slp_matches <- grep(slp_pattern, col_names, perl = TRUE, value = TRUE)
  total_slp <- length(slp_matches)
  
  # check whether input list has a random slope
  missing_vars <- raneff[!sapply(raneff, function(v) {
    any(grepl(v, col_names, ignore.case = TRUE) & 
          grepl("level-2 slope variance", col_names, ignore.case = TRUE))
  })]
  
  # identify column names to test
  if (!is.null(raneff)) {
    target_terms <- c("level-2 slope variance", "level-2 covariance between", "level-2 intercept covariance with")
    target_terms_pattern <- paste(target_terms, collapse = "|")
    raneff_pattern <- paste(raneff, collapse = "|")
    combined_pattern <- paste0("(?i)(", raneff_pattern, ").*(", target_terms_pattern, ")|(", 
                               target_terms_pattern, ").*(", raneff_pattern, ")")
    combined_matches <- grep(combined_pattern, col_names, perl = TRUE, value = TRUE)
  } else {
    combined_matches <- NULL
  }
  
  # subset the iterations data
  if (!is.null(combined_matches) && length(combined_matches) > 0) {
    est2test <- model@iterations[, combined_matches, drop = FALSE]
  } else {
    est2test <- model@iterations[, raneff_matches, drop = FALSE]
  }
  
  # compute wald
  wald <- colMeans(est2test) %*% solve(cov(est2test)) %*% colMeans(est2test)
  df <- length(combined_matches)
  pvalue_central <- pchisq(wald, df = df, lower.tail = FALSE) 
  
  # pvalue for a model with a single random slope
  if(total_raneff == 3){
    pvalue_binom <- if (wald == 0) {0.25 * 1 + 0.50 * 1 + 0.25 * 1 # If LRT == 0, the 0-df component gives probability 1.
    } else {0.25 * 0 + 0.50 * (1 - pchisq(wald, df = 1)) + 0.25 * (1 - pchisq(wald, df = 2))}
    pvalue_mixture <- 0.5 * (1 - pchisq(wald, df = 1)) + 0.5 * (1 - pchisq(wald, df = 2))
  }
  
  # pvalues for models with two random slopes
  if(total_raneff == 6){
    # test one slope
    if(total_slp - length(raneff) == 1){
      if (wald == 0) {pvalue_binom <- 1
      } else {
        pvalue_binom <- (3/8) * pchisq(wald, df = 1, lower.tail = FALSE) + (3/8) * pchisq(wald, df = 2, lower.tail = FALSE) + (1/8) * pchisq(wald, df = 3, lower.tail = FALSE)
      }
      pvalue_mixture <- 0.5 * (1 - pchisq(wald, df = 2)) + 0.5 * (1 - pchisq(wald, df = 3))
    } 
    # test both slopes
    if(total_slp - length(raneff) == 0){
      if (wald == 0) {
        pvalue_binom <- 1
      } else {
        pvalue_binom <- (5/32) * pchisq(wald, df = 1, lower.tail = FALSE) +
          (10/32) * pchisq(wald, df = 2, lower.tail = FALSE) +
          (10/32) * pchisq(wald, df = 3, lower.tail = FALSE) +
          (5/32) * pchisq(wald, df = 4, lower.tail = FALSE) +
          (1/32) * pchisq(wald, df = 5, lower.tail = FALSE)
      }
      pvalue_mixture <- NA
    } 
  }
  
  # round printed values
  wald_r <- round(wald, 3)
  pvalue_mixture_r <- round(pvalue_mixture, 3)
  pvalue_binom_r <- round(pvalue_binom, 3)
  pvalue_central_r <- round(pvalue_central, 3)
  
  # create formatted lines: left-align the description in a 40-character field,
  # then right-align the numeric value in a 10-character field.
  line1 <- sprintf("%-40s %10.3f", "Wald Statistic", wald_r)
  line2 <- sprintf("%-40s %10d", "Number of Parameters Tested (df)", df)
  line3 <- sprintf("%-40s %10.3f", "Probability (Chi-Bar Mixture Method)", pvalue_mixture_r)
  line4 <- sprintf("%-40s %10.3f", "Probability (Chi-Bar Binomial Method)", pvalue_binom_r)
  line5 <- sprintf("%-40s %10.3f", "Probability (Central Chi-Square)", pvalue_central_r)
  
  # combine the lines into a single multi-line string
  results2print <- paste(line1, line2, line3, line4, line5, sep = "\n")
  
  # print the formatted table to the console
  if(length(missing_vars) == 0 & print == TRUE){
    cat(results2print, "\n")
  }
  
  # return table or error message
  if(total_raneff > 6) {
    return("This function currently supports models with only two random slopes.")
  } else if(length(missing_vars) >= 1) {
    return("Error: One or more variables on the test list do not have a random slope in the fitted model.")
  } else {
    return(invisible(list(wald = wald,pmixture = pvalue_mixture,pbinom = pvalue_binom,pcentral = pvalue_central)))
  }
  
}

################################################################################
# function to graph ols slopes by cluster
################################################################################

slopes_by_cluster <- function(data, y, x, lev2id, numlines = NULL) {
  # Drop rows with missing x or y
  data <- data[!is.na(data[[x]]) & !is.na(data[[y]]), ]
  
  # Optionally sample a subset of level-2 groups
  if (!is.null(numlines)) {
    sampled_groups <- sample(unique(data[[lev2id]]), numlines)
    data <- data[data[[lev2id]] %in% sampled_groups, ]
  }
  
  # Group-mean center the x variable
  group_means <- ave(data[[x]], data[[lev2id]], FUN = mean)
  data$x_centered <- data[[x]] - group_means
  
  # Determine y-axis limits with 10% padding
  y_vals <- data[[y]]
  y_min <- min(y_vals, na.rm = TRUE)
  y_max <- max(y_vals, na.rm = TRUE)
  y_lower <- y_min - 0.10 * abs(y_min)
  y_upper <- y_max + 0.10 * abs(y_max)
  
  # Plot using centered x
  p <- ggplot(data, aes(x = x_centered, y = .data[[y]])) +
    geom_smooth(aes(group = .data[[lev2id]]), method = "lm", se = FALSE, color = "#3B4B5A", size = 0.5) +
    geom_smooth(method = "lm", se = FALSE, color = "#D95C14", size = 1.2) +
    scale_y_continuous(limits = c(y_lower, y_upper)) +
    theme_minimal() +
    labs(
      x = paste0("Group Mean Centered ", x),
      y = y,
      title = paste0(lev2id, "-Specific and Overall Regressions of ", y, " on Centered ", x)
    )
  
  return(p)
}