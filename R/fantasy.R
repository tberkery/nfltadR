`%notin%` = Negate(`%in%`)
`%>%` = magrittr::`%>%`

#' Pull weekly performance data using nflfastR
#' @return a dataframe of weekly stats
#' @examples
#' load_weekly()
load_weekly = function(min_year, max_year) {
  stats = nflfastR::load_player_stats(seasons = min_year:max_year)
  stats_by_position = vector(mode = "list", length = length(pos_groups))
  i = 1
  for (pg in pos_groups) {
    stats_sub = stats %>%
      dplyr::filter(season_type == "REG") %>%
      dplyr::filter(position_group == pg) %>%
      dplyr::filter(week >= 1, week <= 16) %>%
      dplyr::select_if(~ !all(is.na(.)))
    numeric_stat_cols = setdiff(colnames(stats_sub %>% dplyr::select(where(is.numeric))),
                                    c("player_id", "season", "week"))
    stats_sub = stats_sub %>%
      tidyr::drop_na(player_id, week) %>%
      dplyr::mutate(across(where(is.character), ~ ifelse(is.na(.), "", .))) %>%
      dplyr::mutate(across(where(is.numeric), ~ ifelse(is.na(.), Mode(.[!is.na(.)]), .)))
    stats_sub_summary = stats_sub %>%
      dplyr::filter(week >= 1, week <= 16) %>%
      dplyr::group_by(player_id, season) %>%
      tidyr::pivot_wider(id_cols = c(player_id, season), names_from = c(week), values_from = numeric_stat_cols) %>%
      dplyr::select(-ends_with("_17"), -ends_with("_18")) # omit week 18 for dimensionality reasons + last week weirdness, 17 for late week weirdness

    summary_stats = stats_sub %>%
      create_seasonal_summary(numeric_stat_cols)

    stats_by_position[[i]] = summary_stats
    i = i + 1
  }
  return(stats_by_position)
}

#' Pull seasonal stats using nflfastR
#' @param min_year a number indicating the earliest season (inclusive)
#' @param max_year a number indicating the latest season (also inclusive)
#' @return a dataframe of seasonal stats
#' @examples
#' load_seasonal(1999, 2022)
#' load_seasonal(2017, 2017)
load_seasonal = function(min_year, max_year) {
  season_stats = NULL
  for (year in min_year:max_year) {
    season_stats_year = nflfastR::load_pbp(year) %>%
      dplyr::filter(week <= 16) %>%
      nflfastR::calculate_player_stats() %>%
      dplyr::mutate(
        ppg_ppr = fantasy_points_ppr / games,
        ppg = fantasy_points / games,
        season = year
      )
    season_stats = rbind(season_stats, season_stats_year)
  }

  roster = nflfastR::fast_scraper_roster(min_year:max_year) %>%
    dplyr::filter(position == "WR") %>%
    dplyr::select(player_id = gsis_id) %>%
    distinct(player_id)
  seasonal_stats_by_position = vector(mode = "list", length = length(pos_groups))
  i = 1
  for (pg in pos_groups) {
    stats_sub = season_stats %>%
      dplyr::filter(position_group == pg) %>%
      dplyr::select_if(~ !all(is.na(.))) %>%
      impute_blank_zero()
    seasonal_stats_by_position[[i]] = stats_sub
    i = i + 1
  }
  return(seasonal_stats_by_position)
}

#' Join weekly and seasonal data together into single table
#' @param weekly the dataframe of weekly data from load_weekly()
#' @param seasonal the dataframe of seasonal data from load_seasonal(x, y)
#' @return a dataframe with joined weekly and seasonal data
#' @examples
#' join_weekly_and_seasonal(weekly, seasonal)
join_weekly_and_seasonal = function(weekly, seasonal) {
  max_i = length(weekly)
  joined_dfs = vector(mode = "list", length = max_i)
  for (i in 1:max_i) {
    joined_dfs[[i]] = weekly[[i]] %>%
      dplyr::inner_join(seasonal[[i]], by = c('player_id', 'season')) %>%
      dplyr::select_if(~ !all(is.na(.))) %>%
      impute_blank_zero()
  }
  return(joined_dfs)
}

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

#' Utility function: impute missing values, replacing with 0 for numeric variables and "" for character variables
#' @param df the dataframe in which to perform the imputation
#' @return a dataframe with all numeric and character columns fully imputed w/o NAs
#' @examples
#' impute_blank_zero(df)
impute_blank_zero = function(df) {
  df_new = df %>%
    dplyr::mutate(dplyr::across(where(is.numeric), ~replace_na(., 0))) %>%
    dplyr::mutate(dplyr::across(where(is.character), ~replace_na(., "MISSING")))
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


#' Get player bios from ff_playerids dictionary of nflreadr
#' @return player bios table w/ gsis_id as only player ID
#' @examples
#' get_player_bios()
get_player_bios = function() {
  player_bios = nflreadr::load_ff_playerids()
  player_bios_sub = player_bios %>%
    drop_na(gsis_id) %>% # we need this ID to join to existing data... seems to mean we lose all rookies
    select(-contains("_id"), gsis_id, -any_of("twitter_username"))
  return(player_bios_sub)
}

#' Utility function: joins datframe w/ gsis-type player_id and season to player bios data
#' @param dfs the dataframe, w/ gsis-style player_id and season as columns, to inner join with player bio info
#' @return dataframe with player bio info
#' @examples
#' join_player_bios(df)
join_player_bios = function(dfs) {
  player_bios = get_player_bios()
  max_i = length(dfs)
  for (i in 1:max_i) {
    dfs[[i]] = dfs[[i]] %>%
      dplyr::inner_join(player_bios, by = c('player_id' = 'gsis_id'), suffix = c('', 'IGNORE')) %>%
      dplyr::select(-ends_with("IGNORE")) %>%
      adjust_age()
  }
  return(dfs)
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

#' Utility function: performs inner join between two dataframes on player_id and season
#' @param dfA the first dataframe (must contain player_id and season)
#' @param dfB the second dataframe (must contain player_id and season)
#' @param suffixA the suffix to apply to the left side (defaults to "")
#' @param suffixB the suffix to apply to the right side (defaults to "")
#' @return the joined dataframe
#' @examples
#' inner_join_by_player_id_and_season(df1, df2)
#' inner_join_by_player_id_and_season(df1, df2, "_left", "right")
inner_join_by_player_id_and_season = function(dfA, dfB, suffixA = "", suffixB = "") {
  dfC = dfA %>%
    dplyr::inner_join(dfB, by = c('player_id', 'season'), suffix = c(suffixA, suffixB))
  return(dfC)
}

#' Utility function: performs left join between two dataframes on player_id and season
#' @param dfA the first dataframe (must contain player_id and season)
#' @param dfB the second dataframe (must contain player_id and season)
#' @param suffixA the suffix to apply to the left side (defaults to "")
#' @param suffixB the suffix to apply to the right side (defaults to "")
#' @return the joined dataframe
#' @examples
#' left_join_by_player_id_and_season(df1, df2)
#' left_join_by_player_id_and_season(df1, df2, "_left", "right")
left_join_by_player_id_and_season = function(dfA, dfB, suffixA = "", suffixB = "") {
  dfC = dfA %>%
    dplyr::left_join(dfB, by = c('player_id', 'season'), suffix = c(suffixA, suffixB))
  return(dfC)
}

#' Utility function: performs left join between across to lists of dataframe vectors on player_id and season
#' @param dfAs the vector of first dataframe (must contain player_id and season)
#' @param dfBs the vector of second dataframe (must contain player_id and season)
#' @param suffixA the suffix to apply to the left side (defaults to "")
#' @param suffixB the suffix to apply to the right side (defaults to "")
#' @return the vector of joined dataframes
#' @examples
#' left_join_positinoal_list_by_player_id_and_season(df1s, df2s)
#' left_join_positinoal_list_by_player_id_and_season(df1s, df2s, "_left", "right")
left_join_positional_list_by_player_id_and_season = function(dfAs, dfBs, suffixA = "", suffixB = "") {
  max_i = length(dfAs)
  dfCs = vector(mode = "list", length = max_i)
  for (i in 1:max_i) {
    dfCs[[i]] = dfAs[[i]] %>%
      dplyr::ungroup() %>%
      dplyr::left_join(dfBs[[i]], by = c('player_id', 'season'))
  }
  return(dfCs)
}

#' Load snap count data from nflreadr
#' @return the seasonal summary dataframe for snap counts
#' @examples
#' load_snap_counts()
load_snap_counts = function() {
  # Snap counts handled identically for all QB/RB/WR/TE positions
  snap_counts = nflreadr::load_snap_counts() %>%
    dplyr::rename(pfr_id = pfr_player_id) %>%
    find_id("pfr_id") %>%
    dplyr::filter(week <= 16) # ignore weeks 17, 18, and postseason
  stat_cols = colnames(snap_counts %>%
                         dplyr::select(where(is.numeric) &
                                         !contains("_id") &
                                         !contains("defense"),
                                       -season, -week))
  seasonal_summary = create_seasonal_summary(snap_counts, stat_cols)
  return(seasonal_summary)
}

#' Load snap count data from nflreadr
#' @return the summary dataframe for combine data
#' @examples
#' load_combine()
load_combine = function() {
  combine = nflreadr::load_combine() %>%
    find_id("pfr_id")
  combine_by_position = vector(mode = "list", length = length(pos_groups))
  i = 1
  for (pg in pos_groups) {
    combine_sub = combine %>%
      dplyr::filter(pos == pg) %>%
      dplyr::select_if(~ !all(is.na(.)))
    numeric_stat_cols = setdiff(colnames(combine_sub %>%
                                           select(where(is.numeric))),
                                c("gsis_id", "season"))
    combine_sub = combine_sub %>%
      tidyr::drop_na(gsis_id, season) %>%
      dplyr::mutate(across(where(is.character), ~ ifelse(is.na(.), "", .))) %>%
      dplyr::mutate(across(where(is.numeric), ~ ifelse(is.na(.), Mode(.[!is.na(.)]), .))) %>%
      dplyr::rename(player_id = gsis_id)
    combine_by_position[[i]] = combine_sub
    i = i + 1
  }
  return(combine_by_position)
}

load_next_gen_stats = function() {
  # next gen stats only goes back to 2016
  next_gen_by_position = vector(mode = "list", length(pos_groups))
  i = 1
  for (pg in pos_groups) {
    ngs_type = NULL
    if (pg == "QB") {
      ngs_type = "passing"
    } else if (pg == "RB") {
      ngs_type = "rushing"
    } else if (pg %in% c("WR", "TE")) {
      ngs_type = "receiving"
    }
    next_gen = nflreadr::load_nextgen_stats(stat = ngs_type) %>%
      dplyr::filter(season_type == "REG") %>%
      dplyr::filter(week <= 16, week >= 1) %>%
      dplyr::rename(gsis_id = player_gsis_id,
             pos = player_position) %>%
      dplyr::select(-c(team_abbr, player_first_name, player_display_name,
                player_last_name, player_jersey_number, player_short_name))
    next_gen_sub = next_gen %>%
      dplyr::filter(pos == pg) %>%
      dplyr::select_if(~ !all(is.na(.)))
    stat_cols = colnames(next_gen_sub %>%
                           dplyr::select(where(is.numeric) & !contains("_id"),
                                         -contains('season'), -contains('week')))

    next_gen_seasonal_summary = next_gen_sub %>%
      create_seasonal_summary(stat_cols)

    next_gen_by_position[[i]] = next_gen_seasonal_summary
    i = i + 1
  }
  return(next_gen_by_position)
}

#' Load weekly pfr advanced data from nflreadr. This function is weekly, not seasonal. Only applicable to QBs.
#' @return the weekly pivoted dataframe for snap counts
#' @examples
#' load_pfr_advanced()
load_pfr_advanced = function(min_year, max_year) {
  # only applicable to quarterbacks
  # likely won't want to use this... only goes back to 2018
  pfr_adv = nflreadr::load_pfr_advstats(min_year:max_year) %>%
    dplyr::rename(pfr_id = pfr_player_id) %>%
    find_id("pfr_id") %>%
    dplyr::filter(game_type == "REG") %>%
    dplyr::select_if(~ !all(is.na(.))) %>%
    dplyr::group_by(gsis_id, season)
  stat_cols = colnames(pfr_adv %>%
                         dplyr::ungroup() %>%
                         dplyr::select(where(is.numeric),
                                       -season, -week, -contains("_id")))
  pfr_adv_pivoted = pfr_adv %>%
    dplyr::filter(week >= 1, week <= 16) %>%
    tidyr::pivot_wider(id_cols = c(gsis_id, season),
                names_from = c(week),
                values_from = stat_cols) %>%
    impute_blank_zero() %>%
    dplyr::rename(player_id = gsis_id)
}

#' Load ESPN Quarterback Rating data from nflreadr
#' @return the seasonal summary dataframe for QBR
#' @examples
#' load_espn_qbr()
load_espn_qbr = function(min_year, max_year) {
  # only applies to QBs
  espn_qbr = nflreadr::load_espn_qbr(min_year:max_year, league = "nfl", summary_type = "weekly") %>%
    dplyr::filter(season_type == "Regular") %>%
    dplyr::select_if(~ !all(is.na(.))) %>%
    dplyr::rename(espn_id = player_id) %>%
    find_id("espn_id") %>%
    dplyr::select(-contains("_id"), gsis_id) %>%
    dplyr::select(-c(season_type, team_abb, name_short, name_first, name_last, name_display, headshot_href, team, rank)) %>%
    dplyr::mutate(epa_per_play = epa_total / qb_plays,
           qbr_per_play = qbr_total / qb_plays,
           pts_added_per_play = pts_added / qb_plays) %>%
    dplyr::rename(week = game_week) %>%
    dplyr::filter(week >= 1, week <= 16)
  stat_cols = colnames(espn_qbr %>% ungroup() %>% select(where(is.numeric), -gsis_id, -season, -week))
  espn_qbr_season_summary = create_seasonal_summary(espn_qbr %>%
                                                      filter(!(week == "Season Total")) %>%
                                                      mutate(week = as.numeric(week)), stat_cols)
  return(espn_qbr_season_summary)
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
    dplyr::mutate(draft_club = replace_na(draft_club, "UNDRAFTED")) #,
           #draft_number = replace_na(draft_number, max(draft_number, na.rm = TRUE) + 1))
  id_lookup = roster_sub %>%
    dplyr::select(gsis_id, all_of(id_type)) %>%
    dplyr::distinct(gsis_id, !!sym(id_type), .keep_all = TRUE) %>%
    tidyr::drop_na(gsis_id, !!sym(id_type))
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

  data_qb_standardized_latest = data_qb_latest %>%
    create_factors() %>%
    standardize_by_season(FALSE)
  data_rb_standardized_latest = data_rb_latest %>%
    create_factors() %>%
    standardize_by_season(FALSE)
  data_wr_standardized_latest = data_wr_latest %>%
    create_factors() %>%
    standardize_by_season(FALSE)
  data_te_standardized_latest = data_te_latest %>%
    create_factors() %>%
    standardize_by_season(FALSE)

  data_qb = stats_full[[1]] %>%
    eliminate_receiving_stats()
  data_rb = stats_full[[2]] %>%
    eliminate_passing_stats()
  data_wr = stats_full[[3]] %>%
    eliminate_passing_stats()
  data_te = stats_full[[4]] %>%
    eliminate_passing_stats()

  data_qb %>% write_csv("data_qb.csv")
  data_rb %>% write_csv("data_rb.csv")
  data_wr %>% write_csv("data_wr.csv")
  data_te %>% write_csv("data_te.csv")

  data_qb %>% saveRDS("data_qb.rds")
  data_rb %>% saveRDS("data_rb.rds")
  data_wr %>% saveRDS("data_wr.rds")
  data_te %>% saveRDS("data_te.rds")

  data_qb_standardized = data_qb %>%
    create_factors() %>%
    standardize_by_season(TRUE)
  data_rb_standardized = data_rb %>%
    create_factors() %>%
    standardize_by_season(TRUE)
  data_wr_standardized = data_wr %>%
    create_factors() %>%
    standardize_by_season(TRUE)
  data_te_standardized = data_te %>%
    create_factors() %>%
    standardize_by_season(TRUE)
}
