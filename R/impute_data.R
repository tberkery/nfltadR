#' Utility function: impute missing values, replacing with 0 for numeric variables and "" for character variables
#' @param df the dataframe in which to perform the imputation
#' @return a dataframe with all numeric and character columns fully imputed w/o NAs
#' @examples
#' impute_blank_zero(df)
impute_blank_zero = function(df) {
  df_new = df %>%
    dplyr::mutate(dplyr::across(where(is.numeric), ~tidyr::replace_na(., 0))) %>%
    dplyr::mutate(dplyr::across(where(is.character), ~tidyr::replace_na(., "MISSING")))
  return(df_new)
}

#' Impute blank for character variables and column median for numeric variables
#' @param df the dataframe on which to perform the imputaiton
#' @return a dataframe with the values imputed
#' @examples
#' impute_blank_median(df)
impute_blank_median = function(df) {
  df_new = df %>%
    dplyr::group_by(season) %>%
    dplyr::mutate(dplyr::across(where(is.numeric), ~tidyr::replace_na(., round(median(., na.rm = TRUE), digits = 0)))) %>%
    dplyr::mutate(dplyr::across(where(is.character), ~tidyr::replace_na(., "MISSING")))
  return(df_new)
}

impute_blank_fifth_percentile = function(df) {
  df_new = df %>%
    dplyr::group_by(season) %>%
    dplyr::mutate(dplyr::across(where(is.numeric), ~tidyr::replace_na(., round(quantile(., 0.05, na.rm = TRUE), digits = 0)))) %>%
    dplyr::mutate(dplyr::across(where(is.character), ~tidyr::replace_na(., "MISSING")))
  return(df_new)
}