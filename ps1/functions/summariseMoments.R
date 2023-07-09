# - SummariseMoments -----------------------

#############################################
#                                           #
# This function creates a set of summary    #
# statistics to describe the moments of     #
# all histograms.                           #
#                                           #
# by Juergen Amann                          #
#                                           #
#############################################


summariseMoments <- function(.data, var, var.name = NULL, groupby = "") {

  # Package for higher moments
  library(moments)


  # Define variable and label names
  .var <- sym(var)

  if (is.null(var.name)) var.name <- quo_name(.var)



  ### - 0.0: Evaluate grouping variable ---------------------------
  ### If empty, pass arbitrary variable that doesn't impose any further conditioning
  groupby <- groupby[groupby != ""]
  if (length(groupby) == 0) {
    .data %>%
      mutate(groupby = 1) -> .data
  }


  # Summary stats for Spark session
  if (sum(attr(.data, "class") %in% "tbl_spark") > 0) {
    .data %>%
      na.omit() %>%
      group_by_(.dots = groupby) %>%
      summarize(
        Mean = mean(!!.var),
        SD = sd(!!.var),
        Skewness = skewness(!!.var),
        Kurtosis = kurtosis(!!.var),
        Minimum = min(!!.var),
        First.Decile = percentile_approx(!!.var, .1),
        First.Quartile = percentile_approx(!!.var, .25),
        Median = percentile_approx(!!.var, .5),
        Third.Quartile = percentile_approx(!!.var, .75),
        Ninth.Decile = percentile_approx(!!.var, .9),
        Maximum = max(!!.var),
        Skewness = skewness(!!.var),   
        Kurtosis = kurtosis(!!.var),
        Name = var.name
      ) %>%
      collect() %>%
      select(Name, everything()) -> df_out

    # Summary stats for local session
  } else if (sum(attr(.data, "class") %in% "tbl_df") > 0) {
    .data %>%
      na.omit() %>%  
      group_by_(.dots = groupby) %>% 
      summarize(
        Mean = mean(!!.var, na.rm = TRUE),
        SD = sd(!!.var),
        Skewness = skewness(!!.var),
        Kurtosis = kurtosis(!!.var),
        Minimum = min(!!.var),
        First.Decile = quantile(!!.var, .1),
        First.Quartile = quantile(!!.var, .25),
        Median = quantile(!!.var, .5),
        Third.Quartile = quantile(!!.var, .75),
        Ninth.Decile = quantile(!!.var, .9),
        Maximum = max(!!.var),
        Skewness = skewness(!!.var),   
        Kurtosis = kurtosis(!!.var),
        Name = var.name
      ) %>%
      select(Name, everything()) -> df_out
  } else {
    stop("Data format not supported. Function only supports Spark tables or local data frames")
  }

  return(df_out)
}