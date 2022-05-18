# setup -------------------------------------------------------------------
# library(moderndive)
library(broom)
# library(lmerTest)
# library(broom.mixed)
# library(simputation)
# library(mice)

analytical <- analytical %>%
  mutate(
    # criar variável tempo, com mesmos atributos de "mes"
    tempo = month(mes),
    # vacinacao = vacinacao*100, # quanto 1% de cobertura altera a taxa de internação?
    # internacoes = internacoes*100, # a escala é a mesma, que sorte!
  ) %>%
  # rotular nova variável
  set_variable_labels(tempo = "Tempo")

# raw estimate ------------------------------------------------------------

m0 <- lm(internacoes ~ vacinacao * dose * tempo,
         analytical)

# adjusted ----------------------------------------------------------------

m1 <- lm(internacoes ~ vacinacao * dose * tempo + ap_resid,
         analytical)
m2 <- lm(internacoes ~ vacinacao * dose * tempo + fe,
         analytical)
m3 <- lm(internacoes ~ (vacinacao + dose)*tempo,
         analytical)
m4 <- lm(internacoes ~ vacinacao * dose * tempo + fe + ap_resid,
         analytical)

# report table ------------------------------------------------------------

m0_lab <- "Vacinação"
m1_lab <- "Controlada por AP"
m2_lab <- "Controlada por faixa etária"
m3_lab <- "Controlada por etapa do esquema vacinal"
m4_lab <- "Modelo final (completo)"

# tab_mod <- tbl_stack(list(
#   m0 %>% tbl_regression(include = vacinacao, label = list(vacinacao = m0_lab)),
#   m1 %>% tbl_regression(include = vacinacao, label = list(vacinacao = m1_lab)),
#   m2 %>% tbl_regression(include = vacinacao, label = list(vacinacao = m2_lab)),
#   # m3 %>% tbl_regression(include = vacinacao, label = list(vacinacao = m3_lab)),
#   m4 %>% tbl_regression(include = vacinacao, label = list(vacinacao = m4_lab))
#   ))
# 
# write_rds(tab_mod, "dataset/tab_mod.rds")

# appendix table ----------------------------------------------------------

# tab_app <- tbl_merge(list(
#   m0 %>% tbl_regression(),
#   m1 %>% tbl_regression(),
#   m2 %>% tbl_regression(),
#   # m3 %>% tbl_regression(),
#   m4 %>% tbl_regression()
#   ), c(
#     m0_lab,
#     m1_lab,
#     m2_lab,
#     # m3_lab,
#     m4_lab
#     )
#   )
# 
# write_rds(tab_app, "dataset/tab_app.rds")

# use tables --------------------------------------------------------------

tab_mod <- read_rds("dataset/tab_mod.rds")
tab_app <- read_rds("dataset/tab_app.rds")
