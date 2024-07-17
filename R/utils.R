keep_x_eliminate_xy = function(df) {
  df_new = df %>%
    dplyr::select(-ends_with(".y")) %>%
    dplyr::rename_at(vars(ends_with(".x")), ~ gsub("\\.x$", "", .))
  return(df_new)
}

eliminate_passing_stats = function(df) {
  df_new = df %>%
    dplyr::select(-ends_with("_receptions"), -ends_with("_targets"),
                  -contains("passing"))
  return(df_new)
}

eliminate_receiving_stats = function(df) {
  df_new = df %>%
    dplyr::select(-ends_with("_completions"), -ends_with("_attempts"),
                  -contains("receiving"), -contains("sack"))
  return(df_new)
}

eliminate_irrelevant_stats = function(df) {
  df_new = df %>%
    dplyr::select(-present_age, -pos)
  return(df_new)
}

create_factors = function(df) {
  char_cols = colnames(
    df %>%
      dplyr::ungroup() %>%
      dplyr::select(where(is.character), player_id, season, contains("_age"))
  )
  for (col in char_cols) {
    df = df %>%
      dplyr::mutate(!!sym(col) := as.factor(!!sym(col)))
  }
  return(df)
}
