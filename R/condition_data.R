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
  df = df 
  seasons = setdiff(unique(df$season), seq(min(df$season), min(df$season) + 2, 1))
  df_new = NULL
  for (seas in seasons) {
    futile.logger::flog.info(glue::glue("Working on summarizing metrics for {seas} season."))
    df_sub = df %>%
      dplyr::mutate(earliest_considered_season = seas - 2, # in effect 3 seasons back
                    sqrt_week = (week + 17 * (season - earliest_considered_season)) ** 0.67) %>%
      dplyr::mutate(consider = dplyr::if_else(season >= earliest_considered_season & season <= seas, 1, 0)) %>%
      dplyr::filter(consider == 1) %>%
      dplyr::ungroup() %>%
      dplyr::select(-consider, -earliest_considered_season) %>%
      dplyr::group_by(player_id, season) %>%
      dplyr::summarize(
        dplyr::across(dplyr::all_of(numeric_stat_cols), ~ mean(.x, na.rm = TRUE), .names = "mean_{.col}_l3"),
        dplyr::across(dplyr::all_of(numeric_stat_cols), ~ weighted.mean(.x, w = sqrt_week, na.rm = TRUE) - mean(.x, na.rm = TRUE), .names = "wtd_mean_{.col}_l3"),
        dplyr::across(dplyr::all_of(numeric_stat_cols), ~ sd(.x, na.rm = TRUE), .names = "sd_{.col}_l3"),
        .groups = "keep"
      )
    df_sub_prior_season = df %>%
      dplyr::filter(season == seas) %>%
      dplyr::mutate(sqrt_week = week ** 0.5) %>%
      dplyr::group_by(player_id, season) %>%
      dplyr::summarize(
        dplyr::across(dplyr::all_of(numeric_stat_cols), ~ mean(.x, na.rm = TRUE), .names = "mean_{.col}_l1"),
        dplyr::across(dplyr::all_of(numeric_stat_cols), ~ weighted.mean(.x, w = sqrt_week, na.rm = TRUE) - mean(.x, na.rm = TRUE), .names = "wtd_mean_{.col}_l1"),
        dplyr::across(dplyr::all_of(numeric_stat_cols), ~ sd(.x, na.rm = TRUE), .names = "sd_{.col}_l1"),
        .groups = "keep"
      )
    df_sub = df_sub %>%
      dplyr::left_join(df_sub_prior_season,
                        by = c('player_id', 'season'))
    df_new = rbind(df_new, df_sub)
  }
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

  snap_counts = load_snap_counts(min_year, max_year)
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
      keep_x_eliminate_xy() %>%
      eliminate_irrelevant_stats()
  }

  for (i in 1:length(stats_latest_season)) {
    stats_latest_season[[i]] = stats_latest_season[[i]] %>%
      left_join_by_player_id_and_season(snap_counts) %>%
      keep_x_eliminate_xy() %>%
      eliminate_irrelevant_stats()
  }

  data_qb_latest = stats_latest_season[[1]] %>%
    eliminate_receiving_stats() %>%
    join_ff_opportunity_stats(min_year + 2, max_year)
  data_rb_latest = stats_latest_season[[2]] %>%
    eliminate_passing_stats() %>%
    join_ff_opportunity_stats(min_year + 2, max_year)
  data_wr_latest = stats_latest_season[[3]] %>%
    eliminate_passing_stats() %>%
    join_ff_opportunity_stats(min_year + 2, max_year)
  data_te_latest = stats_latest_season[[4]] %>%
    eliminate_passing_stats() %>%
    join_ff_opportunity_stats(min_year + 2, max_year)

  data_qb = stats_full[[1]] %>%
    eliminate_receiving_stats() %>%
    join_ff_opportunity_stats(min_year + 2, max_year)
  data_rb = stats_full[[2]] %>%
    eliminate_passing_stats() %>%
    join_ff_opportunity_stats(min_year + 2, max_year)
  data_wr = stats_full[[3]] %>%
    eliminate_passing_stats() %>%
    join_ff_opportunity_stats(min_year + 2, max_year)
  data_te = stats_full[[4]] %>%
    eliminate_passing_stats() %>%
    join_ff_opportunity_stats(min_year + 2, max_year)

  data_qb = data_qb %>%
    rbind(add_NA_for_missing_cols(data_qb, data_qb_latest)) %>%
    dplyr::select_if(~ !all(is.na(.)))
  data_rb = data_rb %>%
    rbind(add_NA_for_missing_cols(data_rb, data_rb_latest)) %>%
    dplyr::select_if(~ !all(is.na(.)))
  data_wr = data_wr %>%
    rbind(add_NA_for_missing_cols(data_wr, data_wr_latest)) %>%
    dplyr::select_if(~ !all(is.na(.)))
  data_te = data_te %>%
    rbind(add_NA_for_missing_cols(data_te, data_te_latest)) %>%
    dplyr::select_if(~ !all(is.na(.)))

  con = connect_write_db()

  write_data(data_qb, "fantasy_football", "QB", con)
  write_data(data_rb, "fantasy_football", "RB", con)
  write_data(data_wr, "fantasy_football", "WR", con)
  write_data(data_te, "fantasy_football", "TE", con)
}
