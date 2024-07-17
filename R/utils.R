keep_x_eliminate_xy = function(df) {
  df_new = df %>%
    dplyr::select(-ends_with(".y")) %>%
    dplyr::rename_at(dplyr::vars(ends_with(".x")), ~ gsub("\\.x$", "", .))
  return(df_new)
}

eliminate_passing_stats = function(df) {
  df_new = df %>%
    dplyr::select(-dplyr::ends_with("_receptions"), -dplyr::ends_with("_targets"),
                  -dplyr::contains("passing"))
  return(df_new)
}

eliminate_receiving_stats = function(df) {
  df_new = df %>%
    dplyr::select(-dplyr::ends_with("_completions"), -dplyr::ends_with("_attempts"),
                  -dplyr::contains("receiving"), -dplyr::contains("sack"))
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
      dplyr::select(where(is.character), player_id, season, dplyr::contains("_age"))
  )
  for (col in char_cols) {
    df = df %>%
      dplyr::mutate(!!rlang::sym(col) := as.factor(!!rlang::sym(col)))
  }
  return(df)
}

Mode = function(x) {
  ux = unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}
