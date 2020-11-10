library(tidyverse)
library(sf)

statcode_from_gm_nummer = function(x) paste0('GM', paste(rep('0', 4-nchar(x)), collapse = ''), x)

adres_bao = read_csv('data/adres_bao.csv') %>% select(
  brin = `BRIN NUMMER`,
  pc6 = POSTCODE,
  statcode = GEMEENTENUMMER
)
adres_bao$statcode = sapply(adres_bao$statcode, statcode_from_gm_nummer)

functiemix_school = read_csv('data/functiemix-instellingen.csv') %>% select(
  jaar = JAAR,
  brin = BRIN_NUMMER,
  naam_school = INSTELLINGSNAAM,
  gemeente = GEMEENTENAAM,
  bevoegd_gezag = BEVOEGD_GEZAGNUMMER,
  ll_lr_ratio = LEERLING_LERAAR_RATIO,
  schooltype = SCHOOLTYPE,
  medewerkers = OMVANG_FORMATIE_TOTAAL,
  onderwijzers = OMVANG_FORMATIE_OP,
  medewerkers_op_salaris = OMVANG_FORMATIE_OP_BRUTSAL,
  loonsom = LOONSOM_OP_BRUTSAL,
  gem_loonsom = GEMIDDELDE_LOONSOM_OP_BRUTSAL,
  leerlingen = AANTAL_LEERLINGEN,
  loc_randstad = RANDSTAD,
  gewicht_ll = AANTAL_GEWICHTLEERLINGEN
) %>% mutate(
  perc_gewicht = gewicht_ll / leerlingen
)

functiemix_school = left_join(functiemix_school, adres_bao)

baos = functiemix_school %>% filter(schooltype == 'bao' & ll_lr_ratio > 5 & ll_lr_ratio < 50)
baos_2018 = baos %>% filter(jaar == 2018)
ggplot(baos_2018, aes(x = ll_lr_ratio)) + geom_histogram() + theme_minimal()

models = baos %>% group_by(naam_school, gemeente, statcode) %>% filter(n() > 3) %>%
  do(ratio_model = lm(ll_lr_ratio ~ jaar, data = .),
     loonsom_model = lm(gem_loonsom ~ jaar, data = .),
     groei_model = lm(leerlingen ~ jaar, data = .),
     gewicht_model = lm(perc_gewicht ~ jaar, data = .)
     )

coef = models %>% rowwise() %>% mutate(
  loonsom_score_floored = max(c(loonsom_model$coefficients[2], 0)),
  ratio_score_floored = max(c(ratio_model$coefficients[2], 0)),
  loonsom_score = loonsom_model$coefficients[2],
  ratio_score = ratio_model$coefficients[2],
  groei_score = groei_model$coefficients[2],
  gewicht_score = gewicht_model$coefficients[2],
  total_score = loonsom_score_floored * ratio_score_floored,
  ratio_cat = cut(ratio_score, c(-Inf, 0.25, Inf), labels = c('bottom80', 'top20'))
) %>% select(-loonsom_model, -ratio_model, -groei_model)

baos_gewicht = baos %>% group_by(naam_school) %>% summarise(mean_gewicht = mean(perc_gewicht, na.rm = T))
coef = left_join(coef, baos_gewicht)

amsterdam_coef = coef %>% filter(gemeente == 'AMSTERDAM')
den_haag_coef = coef %>% filter(gemeente == "'S-GRAVENHAGE")
eindhoven_coef = coef %>% filter(gemeente == "EINDHOVEN")
rotterdam_coef = coef %>% filter(gemeente == "ROTTERDAM")
utrecht_coef = coef %>% filter(gemeente == "UTRECHT")
haarlemmermeer_coef = coef %>% filter(gemeente == "HAARLEMMERMEER")
almere_coef = coef %>% filter(gemeente == "ALMERE")
zoetermeer_coef = coef %>% filter(gemeente == "ZOETERMEER")

gemeente_coef <- coef %>%
  ungroup() %>%
  group_by(statcode) %>%
  summarise(
    gemeente = gemeente[1],
    total_score = mean(total_score, na.rm = T),
    loonsom_score = mean(loonsom_score, na.rm = T),
    ratio_score = mean(ratio_score, na.rm = T),
    groei_score = mean(groei_score, na.rm = T),
    perc_top_ratio_score = sum(ratio_cat == 'top20', na.rm = T) / n(),
    n = n()
  ) %>% rowwise() %>% mutate(
    total_score = min(total_score, 25),
    perc_top_ratio_score_capped = min(perc_top_ratio_score, 0.5)
  ) %>% filter(
    n > 5
  )

hist(gemeente_coef$perc_top_ratio_score)

gemeentegrenzen <- st_read("https://geodata.nationaalgeoregister.nl/cbsgebiedsindelingen/wfs?request=GetFeature&service=WFS&version=2.0.0&typeName=cbs_gemeente_2017_gegeneraliseerd&outputFormat=json")

data <-
  gemeentegrenzen %>%
  left_join(gemeente_coef)

data %>%
  ggplot() +
  geom_sf(aes(fill = total_score)) +
  scale_fill_viridis_c() +
  labs(title = "total_score", fill = "") +
  theme_void()

data %>%
  ggplot() +
  geom_sf(aes(fill = loonsom_score)) +
  scale_alpha() +
  labs(title = "loonsom_score", fill = "") +
  theme_void()

data %>%
  ggplot() +
  geom_sf(aes(fill = ratio_score)) +
  scale_fill_viridis_c() +
  labs(title = "ratio_score", fill = "") +
  theme_void()

data %>%
  ggplot() +
  geom_sf(aes(fill = perc_top_ratio_score_capped)) +
  scale_fill_viridis_c() +
  labs(title = "perc_top_ratio_score_capped", fill = "") +
  theme_void()





ses_coef = coef %>% group_by(statcode) %>% filter(n() > 20) %>%
  do(ses_flight_model = lm(groei_score ~ mean_gewicht, data = .)) %>%
  rowwise() %>% mutate(ses_model_coef = ses_flight_model$coefficients[2]) %>% select(-ses_flight_model)

data <-
  gemeentegrenzen %>%
  left_join(ses_coef)

data %>%
  ggplot() +
  geom_sf(aes(fill = ses_model_coef)) +
  scale_fill_viridis_c() +
  labs(title = "ses_model_coef", fill = "") +
  theme_void()
