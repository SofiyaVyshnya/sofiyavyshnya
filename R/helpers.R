library(dplyr)
library(ggplot2)
library(patchwork) # for plot layout
library(stringr)

# Helper: remove outliers
remove_outliers <- function(x) {
  if (!is.numeric(x)) return(x)  # skip non-numeric
  q1 <- quantile(x, 0.25, na.rm = TRUE)
  q3 <- quantile(x, 0.75, na.rm = TRUE)
  iqr <- q3 - q1
  lower <- q1 - 1.5 * iqr
  upper <- q3 + 1.5 * iqr
  x[x < lower | x > upper] <- NA
  return(x)
}

# Main data visualization helper: function takes in the original data and drops 
# outliers for numeric variables. Optionally, it also log transforms the data 
# after outlier removal.
visualize_data <- function(data, var, var_label = var, log_transform = TRUE) {
  
  # Pick out variable of interest
  vals <- data[[var]]
  
  # Remove outliers
  clean_vals <- remove_outliers(vals) # sets outliers to NA
  clean_vals <- clean_vals[!is.na(clean_vals)] # drops NAs
  
  # Conditionally log transform
  log_vals <- if (log_transform) {
    if (!is.numeric(clean_vals)) stop("Cannot log-transform non-numeric data")
    log(clean_vals + 1) # log1p style to handle zeros
  } else {
    NULL
  }
  
  # Base histogram (all data)
  p_all <- ggplot(data.frame(x = vals), aes(x = x)) +
    geom_histogram(binwidth = diff(range(vals, na.rm=TRUE))/30,
                   fill = "gray70", color = "white") +
    labs(title = str_wrap(paste(var_label, "(All Data)"), width = 25),
         x = var_label, y = "Count") +
    theme_minimal() +
    theme(plot.title = element_text(size = 10))
  
  # Cleaned histogram (outliers removed)
  p_clean <- ggplot(data.frame(x = clean_vals), aes(x = x)) +
    geom_histogram(binwidth = diff(range(clean_vals, na.rm=TRUE))/30,
                   fill = "skyblue", color = "white") +
    labs(title = str_wrap(paste(var_label, "(Outliers Removed)"), width = 25),
         x = var_label, y = "Count") +
    theme_minimal() +
    theme(plot.title = element_text(size = 10))
  
  # Log-transformed histogram (optional)
  if (!is.null(log_vals)) {
    p_log <- ggplot(data.frame(x = log_vals), aes(x = x)) +
      geom_histogram(binwidth = diff(range(log_vals, na.rm=TRUE))/30,
                     fill = "tomato", color = "white") +
      labs(title = str_wrap(paste("log(", var_label, ") (Outliers Removed)"), width = 25),
           x = paste0("log(", var_label, ")"), y = "Count") +
      theme_minimal() +
      theme(plot.title = element_text(size = 10))
    
    return(p_all + p_clean + p_log) # 3-column layout
  } else {
    return(p_all + p_clean) # only 2 plots
  }
}
