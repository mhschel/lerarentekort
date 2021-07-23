#' sort_means_table
#'
#' Sort table of summarized schools data.
#' @param .data Dataframe from summarize_schools function with a statname.
#' @param sort_var Variable to sort by.
#' @param desc Descending
#' @import dplyr
#' @export
#' @examples
#' means_df %>% sort_means_table(sort_var = desc(mean_ll_lr_ratio), name_var = statcode)

sort_means_table = function(.data, sort_var) {

  .data %>% as_tibble() %>%
    arrange({{sort_var}}) %>%
    select(statnaam, mean_ll_lr_ratio, mean_gewogen_llr, perc_high_ll_lr_ratio, perc_high_gewogen_llr)

}

sort_changes_table = function(.data, sort_var) {

  .data %>% as_tibble() %>%
    arrange({{sort_var}}) %>%
    select(statnaam, change_ll_lr_ratio, change_weighted_ll_lr_ratio ,change_ll_lr_ratio_cut, change_weighted_ll_lr_ratio_cut, ll_lr_ratio_2009, ll_lr_ratio_2019)

}
