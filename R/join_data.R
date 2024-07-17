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
      dplyr::select(-dplyr::ends_with("IGNORE")) %>%
      adjust_age()
  }
  return(dfs)
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
