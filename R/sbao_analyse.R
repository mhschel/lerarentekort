functiemix_school %>% filter(schooltype %in% c('sbao', 'bao', 'wec')) %>%
  group_by(jaar, schooltype) %>%
  summarise(leerlingen = sum(leerlingen)) %>%
  ggplot(aes(x = jaar, y = leerlingen, group = schooltype, fill = schooltype)) + geom_area()

functiemix_school %>% filter(schooltype %in% c('sbao', 'bao', 'wec')) %>%
  group_by(jaar, schooltype) %>%
  summarise(leerlingen = sum(leerlingen)) %>%
  group_by(jaar) %>% summarise(perc_sbao = leerlingen[2] / sum(leerlingen), perc_wec = leerlingen[3] / sum(leerlingen))
