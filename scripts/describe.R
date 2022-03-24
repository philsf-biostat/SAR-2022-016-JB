# setup -------------------------------------------------------------------

# library(Hmisc) # describe
# library(skimr) # skim
# library(gmodels) # CrossTable
library(gtsummary)
library(gt)
# library(effectsize)
# library(finalfit) # missing_compare

# setup gtsummary theme
lst_theme <- list(`pkgwide-str:theme_name` = "FF gtsummary theme",
                  `pkgwide-fn:pvalue_fun` = function(x) style_pvalue(x,  digits = 3),
                  `pkgwide-fn:prependpvalue_fun` = function(x) style_pvalue(x, digits = 3, prepend_p = TRUE),
                  `tbl_summary-str:continuous_stat` = "{mean} ({sd})",
                  `add_p.tbl_summary-attr:test.continuous_by2` = "t.test",
                  `add_p.tbl_summary-attr:test.continuous` = "aov",
                  `add_p.tbl_svysummary-attr:test.continuous` = "svy.t.test",
                  `add_p.tbl_svysummary-attr:test.categorical` = "svy.adj.chisq.test",
                  `style_number-arg:decimal.mark` = ".",
                  `style_number-arg:big.mark` = ",",
                  `tbl_summary-fn:addnl-fn-to-run` = function(x) add_stat_label(x),
                  # `tbl_summary-str:categorical_stat` = "{n} ({p}%)",
                  `tbl_svysummary-fn:addnl-fn-to-run` = function(x) add_stat_label(x),
                  `pkgwide-str:ci.sep` = " to ")

set_gtsummary_theme(lst_theme)
theme_gtsummary_compact()
theme_gtsummary_language(language = "pt") # traduzir
theme_gtsummary_eda()

# exploratory -------------------------------------------------------------

# overall description
# analytical %>% skimr::skim()
# perfil %>% skimr::skim()

pct_ap <- perfil %>%
  transmute(
    ap_resid,
    pct_h = Homens / total,
    pct60 = f1/total,
    pct70 = f2/total,
    pct80 = f3/total,
  )

# correlacoes -------------------------------------------------------------

tab_d1 <- analytical %>%
  filter(dose == "d1") %>%
  group_by(ap_resid, fe) %>%
  summarise(correlacao = cor(internacoes, vacinacao)) %>%
  ungroup() %>%
  pivot_wider(names_from = fe, values_from = correlacao) %>%
  mutate(across(c(f1, f2, f3), format.float))

tab_d2 <- analytical %>%
  filter(dose == "d2") %>%
  group_by(ap_resid, fe) %>%
  summarise(correlacao = cor(internacoes, vacinacao)) %>%
  ungroup() %>%
  pivot_wider(names_from = fe, values_from = correlacao) %>%
  mutate(across(c(f1, f2, f3), format.float))

tab_dr <- analytical %>%
  filter(dose == "dr") %>%
  group_by(ap_resid, fe) %>%
  summarise(correlacao = cor(internacoes, vacinacao)) %>%
  ungroup() %>%
  pivot_wider(names_from = fe, values_from = correlacao) %>%
  mutate(across(c(f1, f2, f3), format.float))

tab_f0 <- analytical %>%
  group_by(ap_resid, dose) %>%
  summarise(correlacao = cor(internacoes, vacinacao)) %>%
  ungroup() %>%
  pivot_wider(names_from = dose, values_from = correlacao) %>%
  mutate(across(c(d1, d2, dr), format.float))

# tables ------------------------------------------------------------------

tab_vars <- analytical %>%
  tbl_summary(include = c(vacinacao, internacoes)) %>%
  bold_labels()
