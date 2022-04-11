# setup -------------------------------------------------------------------

# library(Hmisc) # describe
# library(skimr) # skim
# library(gmodels) # CrossTable
library(gtsummary)
library(gt)
# library(effectsize)
# library(finalfit) # missing_compare

# setup gtsummary theme
theme_ff_gtsummary()
theme_gtsummary_compact()
theme_gtsummary_language(language = "pt") # traduzir
theme_gtsummary_eda()

# exploratory -------------------------------------------------------------

# overall description
# analytical %>% skimr::skim()
# perfil %>% skimr::skim()

# pop idosos MRJ
mrj <- perfil %>% pull(total) %>% sum

pct_ap <- perfil %>%
  transmute(
    ap_resid,
    pct_h = Homens / total,
    pct60 = f1/total,
    pct70 = f2/total,
    pct80 = f3/total,
  )

cv <- analytical %>%
  summarise(vac = sd(vacinacao)/mean(vacinacao), int = sd(internacoes)/mean(internacoes))

# análise descritiva ------------------------------------------------------

tab_perfil <- perfil %>%
  left_join(pct_ap, by = "ap_resid") %>%
  transmute(
    ap_resid,
    Homens = paste0(Homens, " (", format.pct(pct_h), ")"),
    Mulheres = paste0(Mulheres, " (", format.pct(1-pct_h), ")"),
    f1 = paste0(f1, " (", format.pct(pct60), ")"),
    f2 = paste0(f2, " (", format.pct(pct70), ")"),
    f3 = paste0(f3, " (", format.pct(pct80), ")"),
    total,
  )

# Tabela 2
tab_vars <- analytical %>%
  tbl_summary(include = c(vacinacao, internacoes)) %>%
  bold_labels()

# Dose 1 ------------------------------------------------------------------

tab_d1_f1 <- analytical %>%
  filter(dose == "d1", fe == f1) %>%
  group_by(ap_resid) %>%
  summarise(vacinacao, internacoes, correlacao = cor(internacoes, vacinacao), .groups = "keep") %>%
  summarise(vacinacao = sum(vacinacao), internacoes = sum(internacoes), correlacao, .groups = "drop") %>%
  distinct() %>%
  mutate(correlacao = format.float(correlacao))

tab_d1_f2 <- analytical %>%
  filter(dose == "d1", fe == f2) %>%
  group_by(ap_resid) %>%
  summarise(vacinacao, internacoes, correlacao = cor(internacoes, vacinacao), .groups = "keep") %>%
  summarise(vacinacao = sum(vacinacao), internacoes = sum(internacoes), correlacao, .groups = "drop") %>%
  distinct() %>%
  mutate(correlacao = format.float(correlacao))

tab_d1_f3 <- analytical %>%
  filter(dose == "d1", fe == f3) %>%
  group_by(ap_resid) %>%
  summarise(vacinacao, internacoes, correlacao = cor(internacoes, vacinacao), .groups = "keep") %>%
  summarise(vacinacao = sum(vacinacao), internacoes = sum(internacoes), correlacao, .groups = "drop") %>%
  distinct() %>%
  mutate(correlacao = format.float(correlacao))

# Dose 2 ------------------------------------------------------------------

tab_d2_f1 <- analytical %>%
  filter(dose == "d2", fe == f1) %>%
  group_by(ap_resid) %>%
  summarise(vacinacao, internacoes, correlacao = cor(internacoes, vacinacao), .groups = "keep") %>%
  summarise(vacinacao = sum(vacinacao), internacoes = sum(internacoes), correlacao, .groups = "drop") %>%
  distinct() %>%
  mutate(correlacao = format.float(correlacao))

tab_d2_f2 <- analytical %>%
  filter(dose == "d2", fe == f2) %>%
  group_by(ap_resid) %>%
  summarise(vacinacao, internacoes, correlacao = cor(internacoes, vacinacao), .groups = "keep") %>%
  summarise(vacinacao = sum(vacinacao), internacoes = sum(internacoes), correlacao, .groups = "drop") %>%
  distinct() %>%
  mutate(correlacao = format.float(correlacao))

tab_d2_f3 <- analytical %>%
  filter(dose == "d2", fe == f3) %>%
  group_by(ap_resid) %>%
  summarise(vacinacao, internacoes, correlacao = cor(internacoes, vacinacao), .groups = "keep") %>%
  summarise(vacinacao = sum(vacinacao), internacoes = sum(internacoes), correlacao, .groups = "drop") %>%
  distinct() %>%
  mutate(correlacao = format.float(correlacao))

# Dose R ------------------------------------------------------------------

tab_dr_f1 <- analytical %>%
  filter(dose == "dr", fe == f1) %>%
  group_by(ap_resid) %>%
  summarise(vacinacao, internacoes, correlacao = cor(internacoes, vacinacao), .groups = "keep") %>%
  summarise(vacinacao = sum(vacinacao), internacoes = sum(internacoes), correlacao, .groups = "drop") %>%
  distinct() %>%
  mutate(correlacao = format.float(correlacao))

tab_dr_f1 <- analytical %>%
  filter(dose == "dr", fe == f1) %>%
  group_by(ap_resid) %>%
  summarise(vacinacao, internacoes, correlacao = cor(internacoes, vacinacao), .groups = "keep") %>%
  summarise(vacinacao = sum(vacinacao), internacoes = sum(internacoes), correlacao, .groups = "drop") %>%
  distinct() %>%
  mutate(correlacao = format.float(correlacao))

tab_dr_f2 <- analytical %>%
  filter(dose == "dr", fe == f2) %>%
  group_by(ap_resid) %>%
  summarise(vacinacao, internacoes, correlacao = cor(internacoes, vacinacao), .groups = "keep") %>%
  summarise(vacinacao = sum(vacinacao), internacoes = sum(internacoes), correlacao, .groups = "drop") %>%
  distinct() %>%
  mutate(correlacao = format.float(correlacao))

tab_dr_f3 <- analytical %>%
  filter(dose == "dr", fe == f3) %>%
  group_by(ap_resid) %>%
  summarise(vacinacao, internacoes, correlacao = cor(internacoes, vacinacao), .groups = "keep") %>%
  summarise(vacinacao = sum(vacinacao), internacoes = sum(internacoes), correlacao, .groups = "drop") %>%
  distinct() %>%
  mutate(correlacao = format.float(correlacao))

# Avaliação Global --------------------------------------------------------

tab_f0_dose <- analytical %>%
  group_by(ap_resid, dose) %>%
  summarise(correlacao = cor(internacoes, vacinacao), .groups = "drop") %>%
  pivot_wider(names_from = dose, values_from = correlacao)

tab_f0_todas <- analytical %>%
  filter(dose != "d1") %>%
  group_by(ap_resid) %>%
  summarise(Todas = cor(internacoes, vacinacao), .groups = "drop")

tab_f0 <- tab_f0_dose %>%
  right_join(tab_f0_todas, by = "ap_resid") %>%
  mutate(across(c(d1, d2, dr, Todas), format.float))

# cobertura ---------------------------------------------------------------

# perfil %>%
#   select(ap_resid, total) %>%
#   left_join(analytical, by = "ap_resid") %>%
#   group_by(dose, fe) %>%
#   summarise(cobertura = sum(vacinacao/total), .groups = "drop") %>%
#   # summarise(cobertura = ifelse(sum(vacinacao/total)<=1, sum(vacinacao/total), 1), .groups = "drop") %>%
#   distinct() %>%
#   ggplot(aes(fe, cobertura, fill = dose)) +
#   geom_col(position = "dodge") +
#   # coord_flip() +
#   geom_hline(yintercept = 1, col = "red", lty = 2) +
#   scale_fill_brewer(palette = "Blues") +
#   scale_y_continuous(labels = scales::label_percent()) +
#   theme_ff()
# 
# int_tx <- perfil %>%
#   select(ap_resid, total) %>%
#   left_join(analytical, by = "ap_resid") %>%
#   mutate(internacoes = internacoes / total) %>%
#   group_by(mes, fe) %>%
#   summarise(internacoes = sum(internacoes), .groups = "drop")
# 
# int_tx
# 
# cob_comp <- analytical %>%
#   select(-internacoes) %>%
#   filter(dose == "d2") %>%
#   left_join(perfil %>% select(ap_resid, total), by = "ap_resid") %>%
#   mutate(cobertura = vacinacao/total) %>%
#   select(ap_resid, mes, fe, dose, cobertura) %>%
#   group_by(mes, fe, dose) %>%
#   summarise(cobertura = sum(cobertura), .groups = "drop")
# 
# cob_comp
# 
# int_tx %>%
#   # mutate(cobertura = ifelse(internacoes<=1, internacoes, 1)) %>%
#   ggplot() +
#   geom_col(aes(mes, internacoes, fill = fe), position = "dodge") +
#   scale_fill_brewer(palette = "Reds") +
#   scale_y_continuous(labels = scales::label_percent()) +
#   theme_ff()
# 
# cob_comp %>%
#   # mutate(cobertura = ifelse(cobertura<=1, cobertura, 1)) %>%
#   ggplot() +
#   scale_fill_brewer(palette = "Blues") +
#   geom_col(aes(mes, cobertura, fill = fe), position = "dodge") +
#   scale_y_continuous(labels = scales::label_percent()) +
#   theme_ff()
# 
# analytical %>%
#   pivot_wider(names_from = dose, values_from = vacinacao) %>%
#   replace_na(list(dr = 0)) %>%
#   summarise(c1 = sum(d1)/mrj, c2 = sum(d2)/mrj, cr = sum(dr)/mrj) 
# 
# analytical %>%
#   pivot_wider(names_from = dose, values_from = vacinacao) %>%
#   replace_na(list(dr = 0)) %>%
#   select(mes, internacoes, d1:dr) %>%
#   mutate(d1 = cumsum(d1/mrj), d2 = cumsum(d2/mrj), dr = cumsum(dr/mrj)) %>%
#   # mutate(across(d1:dr, ~ ifelse(.x <= 1, .x, 1))) %>% # censurar em 100%
#   pivot_longer(d1:dr, values_to = "cobertura", names_to = "dose") %>%
#   ggplot(aes(mes, cobertura, col = dose)) +
#   geom_line(lwd = 1) +
#   scale_color_brewer(palette = "Greens") +
#   scale_y_continuous(labels = scales::label_percent(),
#                      limits = c(0, 1.5),
#                      ) +
#   geom_hline(yintercept = 1, lty = 2) +
#   theme_ff()
# 
# tx <- 100
# 
# analytical %>%
#   pivot_wider(names_from = dose, values_from = vacinacao) %>%
#   select(mes, d2, internacoes) %>%
#   mutate(d2 = cumsum(d2/mrj), internacoes = internacoes/mrj*tx) %>%
#   group_by(mes) %>%
#   summarise(d2, internacoes = sum(internacoes), .groups = "drop") %>%
#   ggplot(aes(mes)) +
#   scale_y_continuous(
#     labels = scales::label_percent(),
#     # limits = c(0, 1.5),
#     sec.axis = sec_axis(~./tx, name = paste0("Internações (por ", tx, ")"),
#                         # labels = scales::label_percent(),
#                         )
#   ) +
#   geom_line(aes(y=d2), lwd = 1, col = "steelblue") +
#   geom_line(aes(y=internacoes), lwd = 1, col = "firebrick4") +
#   scale_color_brewer(palette = "Greens") +
#   geom_hline(yintercept = 1, lty = 2) +
#   labs(subtitle = "MRJ 60+ tudão") +
#   theme_ff()
