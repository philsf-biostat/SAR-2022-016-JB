# setup -------------------------------------------------------------------
library(philsfmisc)
# library(data.table)
library(tidyverse)
library(readxl)
library(janitor)
library(lubridate)
library(naniar)

# data loading ------------------------------------------------------------
set.seed(42)

# internacoes
SIVEP_Base_Internação_SRAG <- read_excel("dataset/SIVEP_Base_Internação_SRAG.xlsx") %>% clean_names()

# vacinacao

vac <- bind_rows(
  d1_f1 = read_csv2("dataset/60-69 D1 por AP e mês.csv", skip = 6, n_max = 12) %>% clean_names(),
  d1_f2 = read_csv2("dataset/70-79 D1 por AP e mês.csv", skip = 6, n_max = 12) %>% clean_names(),
  d1_f3 = read_csv2("dataset/80+ D1 por AP e mês.csv", skip = 6, n_max = 12) %>% clean_names(),
  d2_f1 = read_csv2("dataset/60-69 d2 e du por ap e mes.csv", skip = 6, n_max = 12) %>% clean_names(),
  d2_f2 = read_csv2("dataset/70-79 d2 e du por ap e mes.csv", skip = 6, n_max = 12) %>% clean_names(),
  d2_f3 = read_csv2("dataset/80+ d2 e du por ap e mes.csv", skip = 6, n_max = 12) %>% clean_names(),
  dr_f1 = read_csv2("dataset/60-69 REF por ap e mes.csv", skip = 6, n_max = 12) %>% clean_names(),
  dr_f2 = read_csv2("dataset/70-79 ref por ap e mes.csv", skip = 6, n_max = 12) %>% clean_names(),
  dr_f3 = read_csv2("dataset/80+ ref por ap e mes.csv", skip = 6, n_max = 12) %>% clean_names(),
  .id = "id"
  ) %>%
  rename(mes = mes_da_vacinacao) %>%
  select(-total) %>%
  filter(mes != "Total") %>%
  separate(id, into = c("dose", "fe"))

# perfil

# sexo
p1 <- read_excel("dataset/População por AP e sexo _maior que 60 anos.xlsx") %>%
  clean_names()

# pop
p2 <- read_excel("dataset/População por AP_maior que 60.xlsx") %>%
  clean_names()

# data cleaning -----------------------------------------------------------

SIVEP_Base_Internação_SRAG <- SIVEP_Base_Internação_SRAG %>% 
  select(ap_resid, dt_nasc, dt_interna) %>%
  replace_with_na(list(dt_interna = "0-0-0")) %>%
  mutate(
    # datas de internação
    dt_interna = parse_number(dt_interna),
    dt_interna = excel_numeric_to_date(dt_interna),
    dt_nasc = as.Date(dt_nasc),
  ) %>%
  filter(dt_interna>= as.Date("2021-01-01"), dt_interna <= as.Date("2021-12-31"))

SIVEP_Base_Internação_SRAG <- SIVEP_Base_Internação_SRAG %>%
  mutate(
    idade = floor((dt_nasc %--% dt_interna)/dyears()),
  ) %>%
  filter(idade >= 60)

SIVEP_Base_Internação_SRAG <- SIVEP_Base_Internação_SRAG %>%
  filter(!(ap_resid %in% c("FORA MRJ", "FORA DO MRJ", "SEM INFORMAÇÃO")))

## corrigir AP corrompido (codificado no excel como data)
SIVEP_Base_Internação_SRAG <- SIVEP_Base_Internação_SRAG %>%
  mutate(ap_resid = case_when(
    ap_resid == "44563.0" ~ "2.1",
    ap_resid == "44564.0" ~ "3.1",
    ap_resid == "44566.0" ~ "5.1",
    ap_resid == "44594.0" ~ "2.2",
    ap_resid == "44595.0" ~ "3.2",
    ap_resid == "44597.0" ~ "5.2",
    ap_resid == "44623.0" ~ "3.3",
    ap_resid == "44625.0" ~ "5.3",
    # ap_resid == ".0" ~ "",
    TRUE ~ ap_resid,
  ))

interna <- SIVEP_Base_Internação_SRAG %>%
  mutate(
    mes = month(dt_interna),
    # mes = case_when(
    #   mes == 1 ~ "Jan/2021",
    #   mes == 2 ~ "Fev/2021",
    #   mes == 3 ~ "Mar/2021",
    #   mes == 4 ~ "Abr/2021",
    #   mes == 5 ~ "Mai/2021",
    #   mes == 6 ~ "Jun/2021",
    #   mes == 7 ~ "Jul/2021",
    #   mes == 8 ~ "Ago/2021",
    #   mes == 9 ~ "Set/2021",
    #   mes == 10 ~ "Out/2021",
    #   mes == 11 ~ "Nov/2021",
    #   mes == 12 ~ "Dez/2021",
    # ),
    fe = case_when(
      # between(idade, 60, 69) ~ "60-69",
      # between(idade, 70, 79) ~ "70-79",
      # TRUE ~ "80+"
      between(idade, 60, 69) ~ "f1",
      between(idade, 70, 79) ~ "f2",
      idade >= 80 ~ "f3"
    ),
  ) %>%
  group_by(mes, ap_resid) %>%
  count(fe, name = "internacoes") %>%
  ungroup()

# padronizar mês como número, conforme base de internacoes
vac <- vac %>%
  mutate(
    mes = case_when(
      mes == "Jan/2021" ~ 1,
      mes == "Fev/2021" ~ 2,
      mes == "Mar/2021" ~ 3,
      mes == "Abr/2021" ~ 4,
      mes == "Mai/2021" ~ 5,
      mes == "Jun/2021" ~ 6,
      mes == "Jul/2021" ~ 7,
      mes == "Ago/2021" ~ 8,
      mes == "Set/2021" ~ 9,
      mes == "Out/2021" ~ 10,
      mes == "Nov/2021" ~ 11,
      mes == "Dez/2021" ~ 12,
    ),
  )

vac <- vac %>%
  pivot_longer(-c(dose, fe, mes), names_to = "ap_resid", values_to = "vacinacao") %>%
  mutate(
    ap_resid = str_remove(ap_resid, "cap"),
    ap_resid = format.float(as.numeric(ap_resid)/10, digits = 1),
  )

p1 <- p1 %>%
  rename(sexo = x1) %>%
  filter(sexo %in% c("Homens", "Mulheres")) %>%
  select(-total)

p1 <- p1 %>%
  pivot_longer(-sexo, names_to = "ap_resid") %>%
  mutate(
    ap_resid = str_remove(ap_resid, "area_de_planejamento_"),
    ap_resid = format.float(as.numeric(ap_resid)/10, digits = 1),
    # # fix AP 1 e AP4
    ap_resid = case_when(
      ap_resid == "0.1" ~ "1.0",
      ap_resid == "0.4" ~ "4.0",
      TRUE ~ ap_resid
    )
  ) %>%
  pivot_wider(names_from = sexo)

p2 <- p2 %>%
  rename(ap_resid = x1) %>%
  filter(ap_resid != "Total") %>%
  select(-total)

p2 <- p2 %>%
  mutate(
    ap_resid = str_remove(ap_resid, "Área de Planejamento "),
  ) %>%
  transmute(
    ap_resid,
    f1 = x60_a_64_anos + x65_a_69_anos,
    f2 = x70_a_74_anos + x75_a_79_anos,
    f3 = x80_anos_ou_mais,
    total = f1 + f2 + f3,
  )


# join --------------------------------------------------------------------

data.raw <- interna %>%
  full_join(vac, by = c("mes", "ap_resid", "fe"))

perfil <- p1 %>%
  full_join(p2, by = "ap_resid")


# save data ---------------------------------------------------------------

write_csv(data.raw, "dataset/srag_vac.csv")
write_csv(perfil, "dataset/perfil.csv")
