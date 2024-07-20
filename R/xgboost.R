# Inspiration from which this notebook is modeled: https://juliasilge.com/blog/xgboost-tune-volleyball/

xgboost = function(model_name, category_num, position, data, response_variable, num_cv_folds = 5, grid_size = 15) {
  vb_split = rsample::group_initial_split(data, player_id, strata = response_variable)
  df_train = rsample::training(vb_split)
  df_test = rsample::testing(vb_split)

  xgb_spec = parsnip::boost_tree(
    trees = tune(),
    tree_depth = tune(),
    min_n = tune(),
    loss_reduction = tune(),
    sample_size = tune(),
    mtry = tune(),
    learn_rate = tune(),
    scale_pos_weight = tune(),
    penalty_L1 = tune(),
    penalty_L2 = tune()
  ) %>%
    parsnip::set_engine("xgboost") %>%
    parsnip::set_mode("classification")

  xgb_spec

  xgb_grid = dials::grid_latin_hypercube(
    dials::trees(),
    dials::tree_depth(),
    dials::min_n(),
    dials::loss_reduction(),
    sample_size = dials::sample_prop(),
    dials::finalize(dials::mtry(), df_train),
    dials::learn_rate(),
    dials::scale_pos_weight(),
    dials::penalty_L1(),
    dials::penalty_L2(),
    size = grid_size
  )

  xgb_grid

  recipe = recipes::recipe(response_variable ~ ., data = df_train) %>%
    recipes::step_rm(player_id) %>%
    recipes::step_dummy(recipes::all_nominal(), -recipes::all_outcomes()) %>%
    recipes::step_zv(recipes::all_predictors()) %>%
    recipes::step_nzv(recipes::all_predictors()) %>%
    recipes::step_corr(recipes::all_predictors()) %>%
    recipes::step_novel(recipes::all_predictors()) %>%
    recipes::step_other(recipes::all_predictors(), -recipes::all_outcomes(), threshold = 0.99)

  xgb_wf = workflows::workflow() %>%
    workflows::add_recipe(recipe) %>%
    workflows::add_model(xgb_spec)

  xgb_wf

  set.seed(123)
  df_train_folds = rsample::group_vfold_cv(df_train, response_variable, v = num_cv_folds, strata = response_variable)

  df_train_folds

  doParallel::registerDoParallel(cores = detectCores() - 1)

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
    dplyr::filter(.metric == "roc_auc") %>%
    dplyr::select(mean, mtry:sample_size) %>%
    tidyr::pivot_longer(mtry:sample_size,
                 values_to = "value",
                 names_to = "parameter"
    ) %>%
    ggplot2::ggplot(ggplot2::aes(value, mean, color = parameter)) +
    ggplot2::geom_point(alpha = 0.8, show.legend = FALSE) +
    ggplot2::facet_wrap(~parameter, scales = "free_x") +
    ggplot2::labs(x = NULL, y = "AUC")

  tune::show_best(xgb_res, "rmse")

  best_rmse = tune::select_best(xgb_res, "rmse")
  best_rmse

  final_xgb_wf = workflows::finalize_workflow(
    xgb_wf,
    best_rmse
  )

  final_xgb_wf

  final_xgb_fit = final_xgb_wf %>%
    parsnip::fit(data = df_train)

  var_imps = final_xgb_fit %>%
    workflows::pull_workflow_fit() %>%
    vip::vip(geom = "point")
}
