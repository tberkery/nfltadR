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
