#' summarize_schools_by_loc
#'
#' Summarize school data by a particular column.
#' @param .data Dataframe with DUO school data, including `leerlingen`, `onderwijzers`, `gewicht_ll`, `ll_lr_ratio`, etc.
#' @param loc Column to summarize by.
#' @import dplyr
#' @export
#' @examples
#' schools_df %>% summarize_schools_by_loc(loc = statcode)

summarize_schools_by_loc = function(.data, loc) {
  landelijk_gemiddelde = mean(.data$ll_lr_ratio, na.rm = T)
  landelijk_gewogen_gemiddelde = mean(.data$gewogen_ll_lr_ratio, na.rm = T)

  .data %>%
    group_by({{loc}}) %>%
    summarise(
      tot_onderwijzers = sum(onderwijzers),
      tot_leerlingen = sum(leerlingen),
      tot_gewicht = sum(gewicht_ll),
      mean_school_size = mean(leerlingen),
      ll_manager_ratio = sum(leerlingen) / sum(managers),
      ll_support_ratio = sum(leerlingen) / sum(ondersteunend_personeel),
      mean_ll_lr_ratio = mean(ll_lr_ratio, na.rm = T),
      mean_weighted_ll_lr_ratio = mean(gewogen_ll_lr_ratio),
      perc_high_ll_lr_ratio = sum(ll_lr_ratio > landelijk_gemiddelde, na.rm = T) / n(),
      perc_high_weighted_ll_lr_ratio = sum(gewogen_ll_lr_ratio > landelijk_gewogen_gemiddelde, na.rm = T) / n()
    ) %>%
    ungroup() %>%
    mutate(
      ll_lr_ratio_cut = cut(mean_ll_lr_ratio, breaks = quantile(mean_ll_lr_ratio)),
      weighted_ll_lr_ratio_cut = cut(mean_weighted_ll_lr_ratio, breaks = quantile(mean_weighted_ll_lr_ratio)),
      ll_manager_ratio_cut = cut(ll_manager_ratio, breaks = quantile(ll_manager_ratio)),
      ll_support_ratio_cut = cut(ll_support_ratio, breaks = quantile(ll_support_ratio))
    )
}
