# setup -------------------------------------------------------------------
library(philsfmisc)
# library(data.table)
library(tidyverse)
library(readxl)
# library(haven)
# library(foreign)
# library(lubridate)
# library(naniar)
library(labelled)

# data loading ------------------------------------------------------------
set.seed(42)
data.raw <- tibble(id=gl(2, 10), group = gl(2, 10), outcome = rnorm(20))
data.raw <- read_csv("dataset/srag_vac.csv")
perfil <- read_csv("dataset/perfil.csv")

Nvar_orig <- data.raw %>% ncol
Nobs_orig <- data.raw %>% nrow

# data cleaning -----------------------------------------------------------

data.raw <- data.raw %>%
  # select() %>%
  mutate() %>%
  filter()

# data wrangling ----------------------------------------------------------

data.raw <- data.raw %>%
  mutate(
    ap_resid = format.float(ap_resid, digits = 1), # or as.character
    mes = case_when(
      mes ==  1 ~ as.Date("2021-01-01"),
      mes ==  2 ~ as.Date("2021-02-01"),
      mes ==  3 ~ as.Date("2021-03-01"),
      mes ==  4 ~ as.Date("2021-04-01"),
      mes ==  5 ~ as.Date("2021-05-01"),
      mes ==  6 ~ as.Date("2021-06-01"),
      mes ==  7 ~ as.Date("2021-07-01"),
      mes ==  8 ~ as.Date("2021-08-01"),
      mes ==  9 ~ as.Date("2021-09-01"),
      mes == 10 ~ as.Date("2021-10-01"),
      mes == 11 ~ as.Date("2021-11-01"),
      mes == 12 ~ as.Date("2021-12-01"),
    )
  )

perfil <- perfil %>%
  mutate(
    ap_resid = format.float(ap_resid, digits = 1), # or as.character
  )

# taxas
data.raw <- perfil %>%
  select(ap_resid, total) %>%
  left_join(data.raw, by = "ap_resid") %>%
  mutate(
    # internações (por 1k hab)
    internacoes = internacoes / total * 1000,
    # internacoes = internacoes / total,
    # cobertura (% do mês)
    vacinacao = vacinacao/total,
  )

# labels ------------------------------------------------------------------

f1 <- "60-69 anos"
f2 <- "70-79 anos"
f3 <- "80 anos ou mais"
d1 <- "Dose 1"
d2 <- "Dose 2 ou dose única"
dr <- "Dose de reforço"

data.raw <- data.raw %>%
  mutate(
    fe = factor(fe, labels = c(f1, f2, f3)),
  )

data.raw <- data.raw %>%
  set_variable_labels(
    # group = "Study group",
    # outcome = "Study outcome",
    ap_resid = "AP",
    mes = "Mês",
    internacoes = "Internações por SRAG",
    vacinacao = "Vacinação contra COVID-19",
    dose = "Dose",
    fe = "Faixa etária",
  )

# analytical dataset ------------------------------------------------------

analytical <- data.raw %>%
  # select analytic variables
  select(
    ap_resid,
    # group,
    # outcome,
    everything(),
  )

Nvar_final <- analytical %>% ncol
Nobs_final <- analytical %>% nrow

# mockup of analytical dataset for SAP and public SAR
# analytical_mockup <- tibble( ap_resid = c( "1", "2", "3", "...", "N") ) %>%
analytical_mockup <- tibble( ap_resid = c( "1", "2", "3", "...", as.character(Nobs_final) ) ) %>%
  left_join(analytical %>% head(0), by = "ap_resid") %>%
  mutate_all(as.character) %>%
  replace(is.na(.), "")
