run_projections = function(db_name = "fantasy_football") {
  for (scoring_system in c("ppg_next_year", "ppg_ppr_next_year")) {
    for (pos in c("QB", "RB", "WR", "TE")) {
      data = get_data_by_position_and_year(db_name, pos)
      features = data %>%
        dplyr::ungroup() %>%
        dplyr::select(starts_with("mean_"))
      xgboost("standard", 1, pos, data, scoring_system, features, 5, 15)
    }
  }
}
