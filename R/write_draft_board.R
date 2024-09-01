run_projections = function(num_iterations = 25, db_name = "fantasy_football") {
  features_by_position <- list(
    "QB" = c(
      "mean_passing_tds_l1", "mean_passing_tds_l3", "wtd_mean_passing_tds_l1", "wtd_mean_passing_tds_l3", 
      "mean_interceptions_l1", "mean_interceptions_l3", "wtd_mean_interceptions_l1", "wtd_mean_interceptions_l3", 
      "mean_passing_air_yards_l1", "mean_passing_air_yards_l3", "wtd_mean_passing_air_yards_l1", "wtd_mean_passing_air_yards_l3", 
      "mean_passing_yards_after_catch_l1", "mean_passing_yards_after_catch_l3", "wtd_mean_passing_yards_after_catch_l1", "wtd_mean_passing_yards_after_catch_l3", 
      "mean_passing_first_downs_l1", "mean_passing_first_downs_l3", "wtd_mean_passing_first_downs_l1", "wtd_mean_passing_first_downs_l3", 
      "mean_dakota_l1", "mean_dakota_l3", "wtd_mean_dakota_l1", "wtd_mean_dakota_l3", 
      "mean_carries_l1", "mean_carries_l3", "wtd_mean_carries_l1", "wtd_mean_carries_l3", 
      "mean_rushing_yards_l1", "mean_rushing_yards_l3", "wtd_mean_rushing_yards_l1", "wtd_mean_rushing_yards_l3", 
      "mean_rushing_tds_l1", "mean_rushing_tds_l3", "wtd_mean_rushing_tds_l1", "wtd_mean_rushing_tds_l3", 
      "mean_rushing_fumbles_l1", "mean_rushing_fumbles_l3", "wtd_mean_rushing_fumbles_l1", "wtd_mean_rushing_fumbles_l3", 
      "mean_rushing_first_downs_l1", "mean_rushing_first_downs_l3", "wtd_mean_rushing_first_downs_l1", "wtd_mean_rushing_first_downs_l3", 
      "mean_rushing_epa_l1", "mean_rushing_epa_l3", "wtd_mean_rushing_epa_l1", "wtd_mean_rushing_epa_l3", 
      "mean_avg_time_to_throw_l1", "mean_avg_time_to_throw_l3", "wtd_mean_avg_time_to_throw_l1", "wtd_mean_avg_time_to_throw_l3", 
      "mean_aggressiveness_l1", "mean_aggressiveness_l3", "wtd_mean_aggressiveness_l1", "wtd_mean_aggressiveness_l3", 
      "mean_max_completed_air_distance_l1", "mean_max_completed_air_distance_l3", "wtd_mean_max_completed_air_distance_l1", "wtd_mean_max_completed_air_distance_l3", 
      "mean_avg_air_yards_to_sticks_l1", "mean_avg_air_yards_to_sticks_l3", "wtd_mean_avg_air_yards_to_sticks_l1", "wtd_mean_avg_air_yards_to_sticks_l3", 
      "mean_passer_rating_l1", "mean_passer_rating_l3", "wtd_mean_passer_rating_l1", "wtd_mean_passer_rating_l3", 
      "mean_expected_completion_percentage_l1", "mean_expected_completion_percentage_l3", "wtd_mean_expected_completion_percentage_l1", "wtd_mean_expected_completion_percentage_l3", 
      "mean_completion_percentage_above_expectation_l1", "mean_completion_percentage_above_expectation_l3", "wtd_mean_completion_percentage_above_expectation_l1", "wtd_mean_completion_percentage_above_expectation_l3", 
      "mean_avg_air_distance_l1", "mean_avg_air_distance_l3", "wtd_mean_avg_air_distance_l1", "wtd_mean_avg_air_distance_l3", 
      "mean_max_air_distance_l1", "mean_max_air_distance_l3", "wtd_mean_max_air_distance_l1", "wtd_mean_max_air_distance_l3", 
      "mean_epa_per_play_l1", "mean_epa_per_play_l3", "wtd_mean_epa_per_play_l1", "wtd_mean_epa_per_play_l3", 
      "mean_offense_pct_l1", "mean_offense_pct_l3", "wtd_mean_offense_pct_l1", "wtd_mean_offense_pct_l3", 
      "mean_offense_snaps_l1", "mean_offense_snaps_l3", "wtd_mean_offense_snaps_l1", "wtd_mean_offense_snaps_l3", 
      "mean_pass_completions_exp", "mean_pass_yards_gained_exp", 
      "mean_rush_yards_gained_exp", "mean_pass_touchdown_exp", 
      "mean_rush_touchdown_exp", "mean_pass_first_down_exp", 
      "mean_rush_first_down_exp", "mean_pass_interception_exp", 
      "mean_pass_fantasy_points_exp", "mean_rush_fantasy_points_exp", 
      "mean_total_yards_gained_exp", "mean_total_touchdown_exp", 
      "mean_total_first_down_exp", "mean_total_fantasy_points_exp", 
      "wtd_mean_pass_completions_exp", "wtd_mean_pass_yards_gained_exp", 
      "wtd_mean_rush_yards_gained_exp", "wtd_mean_pass_touchdown_exp", 
      "wtd_mean_rush_touchdown_exp", "wtd_mean_pass_two_point_conv_exp", 
      "wtd_mean_rush_two_point_conv_exp", "wtd_mean_pass_first_down_exp", 
      "wtd_mean_rush_first_down_exp", "wtd_mean_pass_interception_exp", 
      "wtd_mean_pass_fantasy_points_exp", "wtd_mean_rush_fantasy_points_exp", 
      "wtd_mean_total_yards_gained_exp", "wtd_mean_total_touchdown_exp", 
      "wtd_mean_total_first_down_exp", "wtd_mean_total_fantasy_points_exp", 
      "mean_pass_completions_exp_team_pct", "mean_pass_yards_gained_exp_team_pct", 
      "mean_rush_yards_gained_exp_team_pct", "mean_pass_touchdown_exp_team_pct", 
      "mean_rush_touchdown_exp_team_pct", "mean_pass_two_point_conv_exp_team_pct", 
      "mean_rush_two_point_conv_exp_team_pct", "mean_pass_first_down_exp_team_pct", 
      "mean_rush_first_down_exp_team_pct", "mean_pass_interception_exp_team_pct", 
      "mean_pass_fantasy_points_exp_team_pct", "mean_rush_fantasy_points_exp_team_pct", 
      "mean_total_yards_gained_exp_team_pct", "mean_total_touchdown_exp_team_pct", 
      "mean_total_first_down_exp_team_pct", "mean_total_fantasy_points_exp_team_pct", 
      "wtd_mean_pass_completions_exp_team_pct", "wtd_mean_pass_yards_gained_exp_team_pct", 
      "wtd_mean_rush_yards_gained_exp_team_pct", "wtd_mean_pass_touchdown_exp_team_pct", 
      "wtd_mean_rush_touchdown_exp_team_pct", "wtd_mean_pass_first_down_exp_team_pct", 
      "wtd_mean_rush_first_down_exp_team_pct", "wtd_mean_pass_interception_exp_team_pct", 
      "wtd_mean_pass_fantasy_points_exp_team_pct", "wtd_mean_rush_fantasy_points_exp_team_pct", 
      "wtd_mean_total_yards_gained_exp_team_pct", "wtd_mean_total_touchdown_exp_team_pct", 
      "wtd_mean_total_first_down_exp_team_pct", "wtd_mean_total_fantasy_points_exp_team_pct"
    ),
    "RB" = c(
      "mean_carries_l1", "mean_carries_l3", "wtd_mean_carries_l1", "wtd_mean_carries_l3", 
      "mean_rushing_yards_l1", "mean_rushing_yards_l3", "wtd_mean_rushing_yards_l1", "wtd_mean_rushing_yards_l3", 
      "mean_rushing_tds_l1", "mean_rushing_tds_l3", "wtd_mean_rushing_tds_l1", "wtd_mean_rushing_tds_l3", 
      "mean_rushing_fumbles_l1", "mean_rushing_fumbles_l3", "wtd_mean_rushing_fumbles_l1", "wtd_mean_rushing_fumbles_l3", 
      "mean_rushing_first_downs_l1", "mean_rushing_first_downs_l3", "wtd_mean_rushing_first_downs_l1", "wtd_mean_rushing_first_downs_l3", 
      "mean_rushing_epa_l1", "mean_rushing_epa_l3", "wtd_mean_rushing_epa_l1", "wtd_mean_rushing_epa_l3", 
      "mean_racr_l1", "mean_racr_l3", "wtd_mean_racr_l1", "wtd_mean_racr_l3", 
      "mean_target_share_l1", "mean_target_share_l3", "wtd_mean_target_share_l1", "wtd_mean_target_share_l3", 
      "mean_wopr_l1", "mean_wopr_l3", "wtd_mean_wopr_l1", "wtd_mean_wopr_l3", 
      "mean_receiving_yards_l1", "mean_receiving_yards_l3", "wtd_mean_receiving_yards_l1", "wtd_mean_receiving_yards_l3", 
      "mean_receiving_tds_l1", "mean_receiving_tds_l3", "wtd_mean_receiving_tds_l1", "wtd_mean_receiving_tds_l3", 
      "mean_rushing_2pt_conversions_l1", "mean_rushing_2pt_conversions_l3", "wtd_mean_rushing_2pt_conversions_l1", "wtd_mean_rushing_2pt_conversions_l3", 
      "mean_rush_yards_over_expected_per_att_l1", "mean_rush_yards_over_expected_per_att_l3", "wtd_mean_rush_yards_over_expected_per_att_l1", "wtd_mean_rush_yards_over_expected_per_att_l3", 
      "mean_rush_pct_over_expected_l1", "mean_rush_pct_over_expected_l3", "wtd_mean_rush_pct_over_expected_l1", "wtd_mean_rush_pct_over_expected_l3", 
      "mean_efficiency_l1", "mean_efficiency_l3", "wtd_mean_efficiency_l1", "wtd_mean_efficiency_l3", 
      "mean_percent_attempts_gte_eight_defenders_l1", "mean_percent_attempts_gte_eight_defenders_l3", "wtd_mean_percent_attempts_gte_eight_defenders_l1", "wtd_mean_percent_attempts_gte_eight_defenders_l3", 
      "mean_avg_time_to_los_l1", "mean_avg_time_to_los_l3", "wtd_mean_avg_time_to_los_l1", "wtd_mean_avg_time_to_los_l3", 
      "mean_offense_pct_l1", "mean_offense_pct_l3", "wtd_mean_offense_pct_l1", "wtd_mean_offense_pct_l3", 
      "mean_offense_snaps_l1", "mean_offense_snaps_l3", "wtd_mean_offense_snaps_l1", "wtd_mean_offense_snaps_l3", 
      "mean_receptions_exp", "mean_rec_yards_gained_exp", 
      "mean_rush_yards_gained_exp", "mean_rec_touchdown_exp", 
      "mean_rush_touchdown_exp", "mean_rec_first_down_exp", 
      "mean_rush_first_down_exp", "mean_rec_fantasy_points_exp", 
      "mean_rush_fantasy_points_exp", "mean_total_yards_gained_exp", 
      "mean_total_touchdown_exp", "mean_total_first_down_exp", 
      "mean_total_fantasy_points_exp", "wtd_mean_pass_completions_exp", 
      "wtd_mean_receptions_exp", "wtd_mean_rec_yards_gained_exp", 
      "wtd_mean_rush_yards_gained_exp", "wtd_mean_rec_touchdown_exp", 
      "wtd_mean_rush_touchdown_exp", "wtd_mean_rec_first_down_exp", 
      "wtd_mean_rush_first_down_exp", "wtd_mean_rec_fantasy_points_exp", 
      "wtd_mean_rush_fantasy_points_exp", "wtd_mean_total_yards_gained_exp", 
      "wtd_mean_total_touchdown_exp", "wtd_mean_total_first_down_exp", 
      "wtd_mean_total_fantasy_points_exp", "mean_receptions_exp_team_pct", 
      "mean_rec_yards_gained_exp_team_pct", "mean_rush_yards_gained_exp_team_pct", 
      "mean_rec_touchdown_exp_team_pct", "mean_rush_touchdown_exp_team_pct", 
      "mean_rec_first_down_exp_team_pct", "mean_rush_first_down_exp_team_pct", 
      "mean_rec_interception_exp_team_pct", "mean_rec_fantasy_points_exp_team_pct", 
      "mean_rush_fantasy_points_exp_team_pct", "mean_total_yards_gained_exp_team_pct", 
      "mean_total_touchdown_exp_team_pct", "mean_total_first_down_exp_team_pct", 
      "mean_total_fantasy_points_exp_team_pct", "wtd_mean_pass_completions_exp_team_pct", 
      "wtd_mean_receptions_exp_team_pct", "wtd_mean_rec_yards_gained_exp_team_pct", 
      "wtd_mean_rush_yards_gained_exp_team_pct", "wtd_mean_rec_touchdown_exp_team_pct", 
      "wtd_mean_rush_touchdown_exp_team_pct", "wtd_mean_rec_first_down_exp_team_pct", 
      "wtd_mean_rush_first_down_exp_team_pct", "wtd_mean_rec_fantasy_points_exp_team_pct", 
      "wtd_mean_rush_fantasy_points_exp_team_pct", "wtd_mean_total_yards_gained_exp_team_pct", 
      "wtd_mean_total_touchdown_exp_team_pct", "wtd_mean_total_first_down_exp_team_pct", 
      "wtd_mean_total_fantasy_points_exp_team_pct"
    ),
    "WR-TE" = c(
      "mean_rushing_yards_l1", "mean_rushing_yards_l3", "wtd_mean_rushing_yards_l1", "wtd_mean_rushing_yards_l3", 
      "mean_rushing_tds_l1", "mean_rushing_tds_l3", "wtd_mean_rushing_tds_l1", "wtd_mean_rushing_tds_l3", 
      "mean_rushing_fumbles_l1", "mean_rushing_fumbles_l3", "wtd_mean_rushing_fumbles_l1", "wtd_mean_rushing_fumbles_l3", 
      "mean_rushing_first_downs_l1", "mean_rushing_first_downs_l3", "wtd_mean_rushing_first_downs_l1", "wtd_mean_rushing_first_downs_l3", 
      "mean_receiving_yards_l1", "mean_receiving_yards_l3", "wtd_mean_receiving_yards_l1", "wtd_mean_receiving_yards_l3", 
      "mean_receiving_tds_l1", "mean_receiving_tds_l3", "wtd_mean_receiving_tds_l1", "wtd_mean_receiving_tds_l3", 
      "mean_receiving_fumbles_l1", "mean_receiving_fumbles_l3", "wtd_mean_receiving_fumbles_l1", "wtd_mean_receiving_fumbles_l3", 
      "mean_receiving_air_yards_l1", "mean_receiving_air_yards_l3", "wtd_mean_receiving_air_yards_l1", "wtd_mean_receiving_air_yards_l3", 
      "mean_receiving_yards_after_catch_l1", "mean_receiving_yards_after_catch_l3", "wtd_mean_receiving_yards_after_catch_l1", "wtd_mean_receiving_yards_after_catch_l3", 
      "mean_receiving_first_downs_l1", "mean_receiving_first_downs_l3", "wtd_mean_receiving_first_downs_l1", "wtd_mean_receiving_first_downs_l3", 
      "mean_receiving_epa_l1", "mean_receiving_epa_l3", "wtd_mean_receiving_epa_l1", "wtd_mean_receiving_epa_l3", 
      "mean_target_share_l1", "mean_target_share_l3", "wtd_mean_target_share_l1", "wtd_mean_target_share_l3", 
      "mean_racr_l1", "mean_racr_l3", "wtd_mean_racr_l1", "wtd_mean_racr_l3", 
      "mean_wopr_l1", "mean_wopr_l3", "wtd_mean_wopr_l1", "wtd_mean_wopr_l3", 
      "mean_air_yards_share_l1", "mean_air_yards_share_l3", "wtd_mean_air_yards_share_l1", "wtd_mean_air_yards_share_l3", 
      "mean_avg_separation_l1", "mean_avg_separation_l3", "wtd_mean_avg_separation_l1", "wtd_mean_avg_separation_l3", 
      "mean_avg_cushion_l1", "mean_avg_cushion_l3", "wtd_mean_avg_cushion_l1", "wtd_mean_avg_cushion_l3", 
      "mean_avg_intended_air_yards_l1", "mean_avg_intended_air_yards_l3", "wtd_mean_avg_intended_air_yards_l1", "wtd_mean_avg_intended_air_yards_l3", 
      "mean_percent_share_of_intended_air_yards_l1", "mean_percent_share_of_intended_air_yards_l3", "wtd_mean_percent_share_of_intended_air_yards_l1", "wtd_mean_percent_share_of_intended_air_yards_l3", 
      "mean_catch_percentage_l1", "mean_catch_percentage_l3", "wtd_mean_catch_percentage_l1", "wtd_mean_catch_percentage_l3", 
      "mean_avg_yac_above_expectation_l1", "mean_avg_yac_above_expectation_l3", "wtd_mean_avg_yac_above_expectation_l1", "wtd_mean_avg_yac_above_expectation_l3", 
      "mean_offense_pct_l1", "mean_offense_pct_l3", "wtd_mean_offense_pct_l1", "wtd_mean_offense_pct_l3", 
      "mean_offense_snaps_l1", "mean_offense_snaps_l3", "wtd_mean_offense_snaps_l1", "wtd_mean_offense_snaps_l3", 
      "mean_receptions_exp", "mean_rec_yards_gained_exp", 
      "mean_rush_yards_gained_exp", "mean_rec_touchdown_exp", 
      "mean_rush_touchdown_exp", "mean_rec_first_down_exp", 
      "mean_rush_first_down_exp", "mean_rec_fantasy_points_exp", 
      "mean_rush_fantasy_points_exp", "mean_total_yards_gained_exp", 
      "mean_total_touchdown_exp", "mean_total_first_down_exp", 
      "mean_total_fantasy_points_exp", "wtd_mean_pass_completions_exp", 
      "wtd_mean_receptions_exp", "wtd_mean_rec_yards_gained_exp", 
      "wtd_mean_rush_yards_gained_exp", "wtd_mean_rec_touchdown_exp", 
      "wtd_mean_rush_touchdown_exp", "wtd_mean_rec_first_down_exp", 
      "wtd_mean_rush_first_down_exp", "wtd_mean_rec_fantasy_points_exp", 
      "wtd_mean_rush_fantasy_points_exp", "wtd_mean_total_yards_gained_exp", 
      "wtd_mean_total_touchdown_exp", "wtd_mean_total_first_down_exp", 
      "wtd_mean_total_fantasy_points_exp", "mean_receptions_exp_team_pct", 
      "mean_rec_yards_gained_exp_team_pct", "mean_rush_yards_gained_exp_team_pct", 
      "mean_rec_touchdown_exp_team_pct", "mean_rush_touchdown_exp_team_pct", 
      "mean_rec_first_down_exp_team_pct", "mean_rush_first_down_exp_team_pct", 
      "mean_rec_interception_exp_team_pct", "mean_rec_fantasy_points_exp_team_pct", 
      "mean_rush_fantasy_points_exp_team_pct", "mean_total_yards_gained_exp_team_pct", 
      "mean_total_touchdown_exp_team_pct", "mean_total_first_down_exp_team_pct", 
      "mean_total_fantasy_points_exp_team_pct", "wtd_mean_pass_completions_exp_team_pct", 
      "wtd_mean_receptions_exp_team_pct", "wtd_mean_rec_yards_gained_exp_team_pct", 
      "wtd_mean_rush_yards_gained_exp_team_pct", "wtd_mean_rec_touchdown_exp_team_pct", 
      "wtd_mean_rush_touchdown_exp_team_pct", "wtd_mean_rec_first_down_exp_team_pct", 
      "wtd_mean_rush_first_down_exp_team_pct", "wtd_mean_rec_fantasy_points_exp_team_pct", 
      "wtd_mean_rush_fantasy_points_exp_team_pct", "wtd_mean_total_yards_gained_exp_team_pct", 
      "wtd_mean_total_touchdown_exp_team_pct", "wtd_mean_total_first_down_exp_team_pct", 
      "wtd_mean_total_fantasy_points_exp_team_pct"
    )
  )
  projections = NULL
  for (model_run in 1:num_iterations) {
    for (scoring_system in c("ppg_next_year", "ppg_ppr_next_year")) {
      for (pos in c("QB", "RB", "WR-TE")) {
        futile.logger::flog.info(glue::glue("Working on model run #{model_run} using {scoring_system} for {pos}."))
        features = features_by_position[[pos]]
        if (pos %in% c("QB", "RB", "WR", "TE")) {
          data = get_data_by_position_and_year(db_name, pos)
        } else if (pos == "WR-TE") {
          data_wr = get_data_by_position_and_year(db_name, "WR")
          data_te = get_data_by_position_and_year(db_name, "TE")
          common_cols = intersect(colnames(data_wr), colnames(data_te))
          data_wr = data_wr %>%
            dplyr::select(dplyr::all_of(common_cols))
          data_te = data_te %>%
            dplyr::select(dplyr::all_of(common_cols))
          data = rbind(data_wr, data_te)
        }
        num_cv_folds = NULL
        if (pos == "QB") {
          num_cv_folds = 5
        } else if (pos == "RB") {
          num_cv_folds = 5
        } else {
          num_cv_folds = 5
        }
        projections_sub = xgboost("standard", model_run, pos, data, scoring_system, features, num_cv_folds, 15)
        projections_sub = projections_sub %>%
          dplyr::group_by(position, season) %>%
          dplyr::mutate(rank_pos = dplyr::min_rank(desc(proj))) %>%
          dplyr::group_by(position, season, nfl_age) %>%
          dplyr::mutate(rank_nfl_age = dplyr::min_rank(desc(proj))) %>%
          dplyr::group_by(position, season, team) %>%
          dplyr::mutate(rank_tm = dplyr::min_rank(desc(proj))) %>%
          dplyr::ungroup() %>%
          dplyr::mutate(model_run = model_run)
        projections = rbind(projections, projections_sub)
      }
    }
  }
  
  player_seasons_with_id = projections %>%
    dplyr::ungroup() %>%
    dplyr::distinct(player_id, season, scoring_system, name) %>%
    dplyr::distinct(player_id, season, scoring_system, .keep_all = TRUE)
  
  con_write = connect_write_db()
  write_data(player_seasons_with_id, "fantasy_football", "adjustments", con_write)
  
  con_read = connect_read_db()
  existing_adjustments = DBI::dbGetQuery(con_read, "SELECT * FROM fantasy_football.adjustments") %>%
    tibble::as_tibble() %>%
    dplyr::ungroup() %>%
    dplyr::select(-name)
  disconnect_read_db(con_read)
  
  if ("adjustment" %notin% colnames(existing_adjustments)) {
    existing_adjustments$adjustment = 0
  }
  
  df_2024_board = projections %>%
    dplyr::ungroup() %>%
    dplyr::mutate(position = dplyr::case_when(
      position == "FB" ~ "RB",
      position %in% c("QB", "RB", "WR", "TE") ~ position,
      TRUE ~ NA_character_
    )) %>%
    tidyr::drop_na(position) %>%
    dplyr::left_join(existing_adjustments, by = c("player_id", "season", "scoring_system")) %>%
    dplyr::mutate(proj = proj * (1 + adjustment)) %>%
    dplyr::group_by(player_id, name, season, position, team, headshot_url, age, nfl_age, scoring_system) %>%
    dplyr::summarize(
      count = dplyr::n(),
      adjustment = mean(adjustment, na.rm = TRUE),
      exp_proj = mean(proj, na.rm = TRUE),
      sd_proj = sd(proj, na.rm = TRUE),
      min_proj = min(proj, na.rm = TRUE),
      max_proj = max(proj, na.rm = TRUE),
      p20_proj = average_20th_percentile(proj, na.rm = TRUE),
      p80_proj = average_80th_percentile(proj, na.rm = TRUE),
      exp_rank_pos = mean(rank_pos, na.rm = TRUE),
      sd_rank_pos = sd(rank_pos, na.rm = TRUE),
      min_rank_pos = min(rank_pos, na.rm = TRUE),
      max_rank_pos = max(rank_pos, na.rm = TRUE),
      p20_rank_pos = average_20th_percentile(rank_pos, na.rm = TRUE),
      p80_rank_pos = average_80th_percentile(rank_pos, na.rm = TRUE),
      top_5_prob = sum(rank_pos <= 5, na.rm = TRUE) / dplyr::n(),
      top_10_prob = sum(rank_pos <= 10, na.rm = TRUE) / dplyr::n(),
      top_20_prob = sum(rank_pos <= 20, na.rm = TRUE) / dplyr::n(),
      top_30_prob = sum(rank_pos <= 30, na.rm = TRUE) / dplyr::n(),
      top_40_prob = sum(rank_pos <= 40, na.rm = TRUE) / dplyr::n(),
      exp_rank_nfl_age = mean(rank_nfl_age, na.rm = TRUE),
      min_rank_nfl_age = min(rank_nfl_age, na.rm = TRUE),
      max_rank_nfl_age = max(rank_nfl_age, na.rm = TRUE),
      p20_rank_nfl_age = average_20th_percentile(rank_nfl_age, na.rm = TRUE),
      p80_rank_nfl_age = average_80th_percentile(rank_nfl_age, na.rm = TRUE), 
      exp_rank_tm = mean(rank_tm, na.rm = TRUE),
      .groups = 'keep'
    )
  df_2024_board %>% 
    readr::write_csv("board.csv")
  
  write_data(df_2024_board, "fantasy_football", "draft_board", con_write)
  DBI::dbDisconnect(con_write)
  brd_1 = compute_par(df_2024_board, 
                      ss = "ppg_next_year", 
                      num_teams = 10, 
                      num_qbs_per_team = 2,
                      num_rbs_per_team = 4.5,
                      num_wrs_per_team = 5,
                      num_tes_per_team = 1.5)
  brd_1 %>%
    readr::write_csv("board_1.csv")
  brd_2 = compute_par(df_2024_board, 
                      ss = "ppg_next_year", 
                      num_teams = 10, 
                      num_qbs_per_team = 1.5,
                      num_rbs_per_team = 4,
                      num_wrs_per_team = 4.25,
                      num_tes_per_team = 1.25)
  brd_2 %>%
    readr::write_csv("board_2.csv")
  brd_3 = compute_par(df_2024_board, 
                      ss = "ppg_next_year", 
                      num_teams = 10, 
                      num_qbs_per_team = 3,
                      num_rbs_per_team = 3.25,
                      num_wrs_per_team = 4.5,
                      num_tes_per_team = 1.25)
  brd_3 %>%
    readr::write_csv("board_3.csv")
  return(df_2024_board)
}

compute_par = function(df, ss, num_teams, num_qbs_per_team, num_rbs_per_team, num_wrs_per_team, num_tes_per_team) {
  df = df %>%
    dplyr::filter(scoring_system == ss) %>%
    dplyr::group_by(season, position) %>%
    dplyr::mutate(pos_threshold = dplyr::case_when(
      position == "QB" ~ nth_highest(exp_proj, round(num_teams * num_qbs_per_team, digits = 0)),
      position == "RB" ~ nth_highest(exp_proj, round(num_teams * num_rbs_per_team, digits = 0)),
      position == "WR" ~ nth_highest(exp_proj, round(num_teams * num_wrs_per_team, digits = 0)),
      position == "TE" ~ nth_highest(exp_proj, round(num_teams * num_tes_per_team, digits = 0))
    )) %>%
    dplyr::mutate(dplyr::across(dplyr::contains("proj") & !dplyr::contains("sd_"), ~. - pos_threshold)) %>%
    dplyr::ungroup() %>%
    dplyr::arrange(desc(exp_proj)) %>%
    dplyr::mutate(dplyr::across(where(is.numeric), ~round(., digits = 2)))
  table_name = glue::glue("draft_board_", scoring_system, "SS_", num_teams, "TM_", num_qbs_per_team, "QB_", num_rbs_per_team, "RB_", num_wrs_per_team, "WR_", num_tes_per_team, "TE")
  con_write = connect_write_db()
  write_data(df, "fantasy_football", table_name, con_write)
  DBI::dbDisconnect(con_write)
  return(df)
}
