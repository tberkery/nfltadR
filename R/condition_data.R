#' Append next-year fantasy points (PPR and regular) to existing table containing present-year info
#' @param joined a dataframe. It can be the output of join_weekly_and_seasonal but just needs to include fantasy point and games data
#' @return a dataframe with present- and next-year fantasy point and games stats
#' @examples
#' compute_next_year_fantasy_pts(joined)
compute_next_year_fantasy_pts = function(joined_dfs) {
  max_i = length(joined_dfs)
  for (i in 1:max_i) {
    fpts_sub = joined_dfs[[i]] %>%
      dplyr::ungroup() %>%
      dplyr::select(player_id, season, games, ppg, ppg_ppr)
    joined_next = joined_dfs[[i]] %>%
      dplyr::mutate(next_season = season + 1) %>%
      dplyr::inner_join(fpts_sub,
                 by = c('player_id', 'next_season' = 'season'),
                 suffix = c('', '_next_year'))
    joined_dfs[[i]] = joined_next
  }
  return(joined_dfs)
}

#' Utility function: adjust age to reflect relevant seasons
#' @param df the dataframe, which must contain an age and season column, in which to apply the adjustments
#' @return the dataframe with age adjusted
#' @examples
#' adjust_age(df)
adjust_age = function(df) {
  current_season = as.numeric(format(Sys.Date(), "%Y"))
  df_new = df %>%
    dplyr::rename(present_age = age) %>%
    dplyr::mutate(nfl_age = season - draft_year, # nfl_age is years in league since being drafted
           real_age = present_age - (current_season - season)) # real_age adjusts present age for season row describes
  return(df_new)
}

#' Utility function: via INNER JOIN, lookup gsis_id given some other valid id type.
#' @param df the dataframe in which to populate gsis_id via inner join with lookup
#' @param id_type a string indicating the name of the id_type (besides gsis_id) to use in the lookup.
#' @return the seasonal summary dataframe for snap counts
#' @examples
#' find_id(df, "nfl_id")
find_id = function(df, id_type = "pfr_id") {
  roster = nflreadr::load_rosters()
  roster_sub = roster %>%
    dplyr::filter(depth_chart_position %in% c("QB", "RB", "WR", "TE")) %>%
    dplyr::mutate(draft_club = tidyr::replace_na(draft_club, "UNDRAFTED"),
           draft_number = tidyr::replace_na(draft_number, max(draft_number, na.rm = TRUE) + 1))
  id_lookup = roster_sub %>%
    dplyr::select(gsis_id, all_of(id_type)) %>%
    dplyr::distinct(gsis_id, !!rlang::sym(id_type), .keep_all = TRUE) %>%
    tidyr::drop_na(gsis_id, !!rlang::sym(id_type))
  df_new = df %>%
    dplyr::inner_join(id_lookup, by = c(id_type))
  return(df_new)
}

create_seasonal_summary = function(df, numeric_stat_cols) {
  # requires player_id, season, week, numeric_stat_cols
  if ("player_id" %notin% colnames(df)) {
    df = df %>%
      dplyr::rename(player_id = gsis_id)
  }
  df_new = df %>%
    dplyr::mutate(logarithmic_week = log(week)) %>%
    dplyr::group_by(player_id, season) %>%
    dplyr::summarize(
      dplyr::across(all_of(numeric_stat_cols), ~ mean(.x, na.rm = TRUE), .names = "mean_{.col}"),
      dplyr::across(all_of(numeric_stat_cols), ~ sum(.x, na.rm = TRUE), .names = "sum_{.col}"),
      dplyr::across(all_of(numeric_stat_cols), ~ weighted.mean(.x, w = logarithmic_week, na.rm = TRUE), .names = "wtd_mean_{.col}"),
      dplyr::across(all_of(numeric_stat_cols), ~ sd(.x, na.rm = TRUE), .names = "sd_{.col}"),
      dplyr::across(all_of(numeric_stat_cols), ~ median(.x, na.rm = TRUE), .names = "median_{.col}"),
      dplyr::across(all_of(numeric_stat_cols), ~ quantile(.x, 0.2, na.rm = TRUE), .names = "p20_{.col}"),
      dplyr::across(all_of(numeric_stat_cols), ~ quantile(.x, 0.8, na.rm = TRUE), .names = "p80_{.col}"),
      .groups = "keep"
    )
  return(df_new)
}

standardize_by_season = function(df, ignore_next_year) {
  if (ignore_next_year == TRUE) {
    stat_cols = colnames(df %>%
                           dplyr::ungroup() %>%
                           dplyr::select(where(is.numeric)) %>%
                           dplyr::select(-contains("next_year")))
  } else {
    stat_cols = colnames(df %>%
                           dplyr::ungroup() %>%
                           dplyr::select(where(is.numeric)))
  }
  df_new = df %>%
    dplyr::group_by(season) %>%
    dplyr::mutate(across(all_of(stat_cols), ~scale(.)))
  return(df_new)
}

#' Pull conditioned data for specified seasons
#' @param min_year an integer year correpsonding to the earliest valid season of interest
#' @param max_year an integer year correpsonding to the latest valid season of interest
#' @return a conditioned dataframe of stats at the player-season level that can be used for training
#' @examples
#' load_weekly()
get_data = function(min_year, max_year) {
  weekly = load_weekly(min_year, max_year)
  seasonal = load_seasonal(min_year, max_year)
  joined = join_weekly_and_seasonal(weekly, seasonal)

  stats_latest_season = joined

  for (i in 1:length(stats_latest_season)) {
    stats_latest_season[[i]] = stats_latest_season[[i]] %>%
      dplyr::filter(season == max_year)
  }

  stats = compute_next_year_fantasy_pts(joined)

  stats_full = stats %>%
    join_player_bios()
  stats_latest_season = stats_latest_season %>%
    join_player_bios()

  snap_counts = load_snap_counts()
  combine = load_combine()
  next_gen = load_next_gen_stats()
  espn_qbr = load_espn_qbr(min_year, max_year)

  stats_full = stats_full %>%
    left_join_positional_list_by_player_id_and_season(combine)
  stats_full = stats_full %>%
    left_join_positional_list_by_player_id_and_season(next_gen)

  stats_latest_season = stats_latest_season %>%
    left_join_positional_list_by_player_id_and_season(combine)
  stats_latest_season = stats_latest_season %>%
    left_join_positional_list_by_player_id_and_season(next_gen)

  stats_full[[1]] = stats_full[[1]] %>%
    left_join_by_player_id_and_season(espn_qbr)

  stats_latest_season[[1]] = stats_latest_season[[1]] %>%
    left_join_by_player_id_and_season(espn_qbr)

  for (i in 1:length(stats_full)) {
    stats_full[[i]] = stats_full[[i]] %>%
      left_join_by_player_id_and_season(snap_counts) %>%
      dplyr::select_if(~ !all(is.na(.))) %>%
      impute_blank_median() %>%
      keep_x_eliminate_xy() %>%
      eliminate_irrelevant_stats()
  }

  for (i in 1:length(stats_latest_season)) {
    stats_latest_season[[i]] = stats_latest_season[[i]] %>%
      left_join_by_player_id_and_season(snap_counts) %>%
      dplyr::select_if(~ !all(is.na(.))) %>%
      impute_blank_median() %>%
      keep_x_eliminate_xy() %>%
      eliminate_irrelevant_stats()
  }

  data_qb_latest = stats_latest_season[[1]] %>%
    eliminate_receiving_stats()
  data_rb_latest = stats_latest_season[[2]] %>%
    eliminate_passing_stats()
  data_wr_latest = stats_latest_season[[3]] %>%
    eliminate_passing_stats()
  data_te_latest = stats_latest_season[[4]] %>%
    eliminate_passing_stats()

  data_qb = stats_full[[1]] %>%
    eliminate_receiving_stats()
  data_rb = stats_full[[2]] %>%
    eliminate_passing_stats()
  data_wr = stats_full[[3]] %>%
    eliminate_passing_stats()
  data_te = stats_full[[4]] %>%
    eliminate_passing_stats()

  data_qb = data_qb %>%
    rbind(add_NA_for_missing_cols(data_qb, data_qb_latest))
  data_rb = data_rb %>%
    rbind(add_NA_for_missing_cols(data_rb, data_rb_latest))
  data_wr = data_wr %>%
    rbind(add_NA_for_missing_cols(data_wr, data_wr_latest))
  data_te = data_te %>%
    rbind(add_NA_for_missing_cols(data_te, data_te_latest))

  data_qb %>% readr::write_csv("data_qb.csv")
  data_rb %>% readr::write_csv("data_rb.csv")
  data_wr %>% readr::write_csv("data_wr.csv")
  data_te %>% readr::write_csv("data_te.csv")

  con = connect_write_db()

  write_data(data_qb, "fantasy_football", "QB", con)
  write_data(data_rb, "fantasy_football", "RB", con)
  write_data(data_wr, "fantasy_football", "WR", con)
  write_data(data_te, "fantasy_football", "TE", con)
}
