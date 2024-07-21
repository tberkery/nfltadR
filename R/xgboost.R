get_data_by_position_and_year = function(db_name, pos) {
  con = connect_read_db()
  data = DBI::dbGetQuery(con, glue::glue_sql("SELECT * FROM {`db_name`}.{`pos`}", .con = con))
  DBI::dbDisconnect(con)
  return(data)
}

# Inspiration from which this notebook is modeled: https://juliasilge.com/blog/xgboost-tune-volleyball/
xgboost = function(model_name, category_num, position, data, response_variable,
                   features, num_cv_folds = 5, grid_size = 15) {

  futile.logger::flog.info(glue::glue("Working on XGBoost {model_name} model for {position} and {response_variable} (process #{category_num})"))

  create_model_directory(model_name, position, response_variable, category_num)

  data = data %>%
    tidyr::drop_na(!!rlang::sym(response_variable)) %>% # do not remove response variable (which assuming it involves a next-year stat will be NA for some database entries)
    dplyr::mutate(dplyr::across(where(is.double), ~as.numeric(.))) # convert database double objects to numeric

  futile.logger::flog.info(glue::glue("The model will be trained on {nrow(data)} rows of data."))
  features = data %>%
    dplyr::ungroup() %>%
    dplyr::select(starts_with("mean_")) %>%
    colnames()
  df_split = rsample::group_initial_split(data, player_id)
  df_train = rsample::training(df_split)
  df_test = rsample::testing(df_split)

  df_split %>%
    saveRDS(glue::glue("./models/{model_name}_{position}_{response_variable}_{category_num}/split.rds"))

  xgb_spec = parsnip::boost_tree(
    trees = tune(),
    tree_depth = tune(),
    min_n = tune(),
    loss_reduction = tune(),
    sample_size = tune(),
    mtry = tune(),
    learn_rate = tune()
  ) %>%
    parsnip::set_engine("xgboost") %>%
    parsnip::set_mode("regression")

  xgb_spec

  xgb_grid = dials::grid_latin_hypercube(
    dials::trees(),
    dials::tree_depth(),
    dials::min_n(),
    dials::loss_reduction(),
    sample_size = dials::sample_prop(),
    dials::finalize(dials::mtry(), df_train),
    dials::learn_rate(),
    size = grid_size
  )

  xgb_grid

  formula = as.formula(paste(response_variable, "~", paste(features, collapse = " + ")))

  recipe = recipes::recipe(formula, data = df_train) %>%
    recipes::step_dummy(recipes::all_nominal(), -recipes::all_outcomes()) %>%
    recipes::step_zv(recipes::all_predictors()) %>%
    recipes::step_nzv(recipes::all_predictors()) %>%
    recipes::step_corr(recipes::all_predictors()) %>%
    recipes::step_novel(recipes::all_nominal_predictors()) %>%
    recipes::step_other(recipes::all_nominal_predictors(), -recipes::all_outcomes(), threshold = 0.99)

  xgb_wf = workflows::workflow() %>%
    workflows::add_recipe(recipe) %>%
    workflows::add_model(xgb_spec)

  xgb_wf

  set.seed(123)
  df_train_folds = rsample::group_vfold_cv(df_train, response_variable, v = num_cv_folds)

  df_train_folds

  doParallel::registerDoParallel(cores = parallel::detectCores() - 1)

  set.seed(234)
  xgb_res = tune::tune_grid(
    xgb_wf,
    resamples = df_train_folds,
    grid = xgb_grid,
    control = tune::control_grid(save_pred = TRUE)
  )

  xgb_res

  tune::collect_metrics(xgb_res)

  xgb_res %>%
    tune::collect_metrics() %>%
    dplyr::filter(.metric == "rmse") %>%
    dplyr::select(mean, mtry:sample_size) %>%
    tidyr::pivot_longer(mtry:sample_size,
                 values_to = "value",
                 names_to = "parameter"
    ) %>%
    ggplot2::ggplot(ggplot2::aes(value, mean, color = parameter)) +
    ggplot2::geom_point(alpha = 0.8, show.legend = FALSE) +
    ggplot2::facet_wrap(~parameter, scales = "free_x") +
    ggplot2::labs(x = NULL, y = "RMSE", title = "Hyperparameter Tuning")

  tune::show_best(xgb_res, metric = "rmse")

  best_rmse = tune::select_best(xgb_res, metric = "rmse")
  best_rmse

  final_xgb_wf = tune::finalize_workflow(
    xgb_wf,
    best_rmse
  )

  final_xgb_wf

  final_xgb_fit = final_xgb_wf %>%
    parsnip::fit(data = df_train)

  bundle::bundle(final_xgb_fit) %>%
    saveRDS(glue::glue("./models/{model_name}_{position}_{response_variable}_{category_num}/model.rds"))

  var_imps = final_xgb_fit %>%
    workflows::extract_fit_parsnip() %>%
    vip::vip(geom = "point")

  ggplot2::ggsave(glue::glue("./models/{model_name}_{position}_{response_variable}_{category_num}/var_imps.png"))

  test_preds = predict(final_xgb_fit, new_data = df_test)
  df_test$proj = test_preds$.pred

  futile.logger::flog.info(glue::glue("Test-set correlation: {cor(df_test$proj, as.vector(df_test[[response_variable]]))}"))

  con = connect_read_db()
  df_2024 = DBI::dbGetQuery(con, glue::glue_sql("SELECT * FROM fantasy_football.{`position`} WHERE season = 2023", .con = con))
  DBI::dbDisconnect(con)
  preds_2024 = predict(final_xgb_fit, new_data = df_2024)
  df_2024$proj = preds_2024$.pred
  player_bios = get_player_bios()
  df_2024 = df_2024 %>%
    dplyr::inner_join(player_bios, by = c('player_id' = 'gsis_id'),
                      suffix = c("", "_DUPLICATIVE")) %>%
    dplyr::select(-ends_with("DUPLICATIVE"))
  df_2024_board = df_2024 %>%
    dplyr::select(player_id, name, position, season, team, headshot_url, age, nfl_age, proj) %>%
    dplyr::mutate(season = season + 1,
                  scoring_system = response_variable) %>%
    dplyr::arrange(desc(proj))

  con_write = connect_write_db()
  write_data(df_2024_board, "fantasy_football", "draft_board", con_write)
  DBI::dbDisconnect(con_write)

}
