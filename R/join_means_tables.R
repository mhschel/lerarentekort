#' join_means_tables
#'
#' Sort table of summarized schools data.
#' @param means_data_earlier Dataframe from summarize_schools_by_loc
#' @param means_data_later Dataframe from summarize_schools_by_loc
#' @import dplyr
#' @export
#' @examples
#' join_means_tables(df_a, df_b)

join_means_tables = function(means_data_earlier, means_data_later) {
  left_join(
    means_data_earlier, means_data_later, by = 'statcode', na_matches = "never"
  ) %>%
    mutate(
      change_avg_school_size = mean_school_size.y - mean_school_size.x,
      change_avg_school_size_cut = cut(change_avg_school_size, breaks = quantile(change_avg_school_size, na.rm = T)),
      change_ll_lr_ratio = mean_ll_lr_ratio.y - mean_ll_lr_ratio.x,
      change_ll_lr_ratio_cut = cut(change_ll_lr_ratio, breaks = quantile(change_ll_lr_ratio, na.rm = T)),

    ) %>% select(
      statcode,
      change_ll_lr_ratio,
      change_ll_lr_ratio_cut,
      change_avg_school_size,
      ll_lr_ratio_2019 = mean_ll_lr_ratio.y,
      ll_lr_ratio_2009 = mean_ll_lr_ratio.x)
}


