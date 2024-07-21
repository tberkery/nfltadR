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

add_NA_for_missing_cols = function(df1, df2) {
  df1_cols = colnames(df1)
  df2_cols = colnames(df2)
  missing_cols = setdiff(df1_cols, df2_cols)
  for (col in missing_cols) {
    df2[[col]] = NA
  }
  return(df2)
}

create_model_directory = function(model_name, position, response_variable, category_num) {
  # Construct the directory path
  dir_path = file.path(".", "models", paste0(model_name, "_", position, "_", response_variable, "_", category_num))

  # Check if the directory exists
  if (!dir.exists(dir_path)) {
    # Create the directory and any necessary parent directories
    dir.create(dir_path, recursive = TRUE)
    message("Directory created: ", dir_path)
  } else {
    message("Directory already exists: ", dir_path)
  }
}
