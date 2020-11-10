mean_baos_2010 = baos %>% filter(jaar == 2010) %>% group_by(statcode) %>% summarise_all(mean) %>% ungroup() %>%
  mutate(ll_lr_ratio_cut = cut(ll_lr_ratio, breaks = quantile(ll_lr_ratio)))

mean_baos_2018 = baos %>% filter(jaar == 2018) %>% group_by(statcode) %>% summarise(
  mean_ll_lr_ratio = mean(ll_lr_ratio, na.rm = T),
  perc_high_ll_lr_ratio = sum(ll_lr_ratio > 20, na.rm = T) / n()
  ) %>% ungroup() %>%
  mutate(ll_lr_ratio_cut = cut(mean_ll_lr_ratio, breaks = quantile(mean_ll_lr_ratio)))

data <-
  gemeentegrenzen %>%
  left_join(mean_baos_2018)

ggplot(data) +
  geom_sf(aes(fill = ll_lr_ratio_cut)) +
  labs(title = "ll_lr_ratio_cut", fill = "") +
  scale_fill_brewer(palette = 'Reds') +
  theme_void()

ggplot(data) +
  geom_sf(aes(fill = mean_ll_lr_ratio)) +
  labs(title = "mean_ll_lr_ratio", fill = "") +
  scale_fill_continuous(type = "viridis") +
  theme_void()

ggplot(data) +
  geom_sf(aes(fill = perc_high_ll_lr_ratio)) +
  labs(title = "perc_high_ll_lr_ratio", fill = "") +
  scale_fill_continuous(type = "viridis") +
  theme_void()




mean_baos_joined = left_join(
  mean_baos_2010, mean_baos_2018, by = 'statcode', na_matches = "never"
  ) %>%
  mutate(change_ll_lr_ratio = mean_ll_lr_ratio - ll_lr_ratio) %>% select(
    statcode,
    change_ll_lr_ratio,
    ll_lr_ratio_2018 = mean_ll_lr_ratio,
    ll_lr_ratio_2010 = ll_lr_ratio)

data <-
  gemeentegrenzen %>%
  left_join(mean_baos_joined)

ggplot(data) +
  geom_sf(aes(fill = change_ll_lr_ratio)) +
  labs(title = "change_ll_lr_ratio", fill = "") +
  scale_fill_continuous(type = "viridis") +
  theme_void()
