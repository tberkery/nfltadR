run_projections = function(db_name = "fantasy_football") {
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
  
  for (scoring_system in c("ppg_next_year", "ppg_ppr_next_year")) {
    for (pos in c("QB", "RB", "WR-TE")) {
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
      xgboost("standard", 1, pos, data, scoring_system, features, num_cv_folds, 15)
    }
  }
}
