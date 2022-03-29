# setup -------------------------------------------------------------------
# library(survminer)

ff.col <- "steelblue" # good for single groups scale fill/color brewer
ff.pal <- "Blues"     # paleta sequencial
facetcol <- 2
lwd <- 1
alpha <- NA

scale_color_discrete <- function(...) scale_color_brewer(palette = ff.pal, ...)
scale_fill_discrete <- function(...) scale_fill_brewer(palette = ff.pal, ...)

gg <- analytical %>%
  ggplot() +
  facet_wrap(~ ap_resid, ncol = facetcol) +
  scale_color_brewer() +
  scale_fill_brewer() +
  theme_ff()

gg_d1 <- analytical %>%
  group_by(ap_resid, mes, fe) %>%
  filter(dose == "d1") %>%
  summarise(vacinacao = sum(vacinacao), internacoes = sum(internacoes), .groups = "drop") %>%
  ggplot() +
  scale_color_brewer() +
  facet_wrap(~ ap_resid, ncol = facetcol) +
  labs(subtitle = d1, color = NULL, x = "") +
  theme_ff()

gg_d2 <- analytical %>%
  group_by(ap_resid, mes, fe) %>%
  filter(dose == "d2") %>%
  summarise(vacinacao = sum(vacinacao), internacoes = sum(internacoes), .groups = "drop") %>%
  ggplot() +
  scale_color_brewer() +
  facet_wrap(~ ap_resid, ncol = facetcol) +
  labs(subtitle = d2, color = NULL, x = "") +
  theme_ff()

gg_dr <- analytical %>%
  group_by(ap_resid, mes, fe) %>%
  filter(dose == "dr") %>%
  summarise(vacinacao = sum(vacinacao), internacoes = sum(internacoes), .groups = "drop") %>%
  ggplot() +
  scale_color_brewer() +
  facet_wrap(~ ap_resid, ncol = facetcol) +
  labs(subtitle = dr, color = NULL, x = "") +
  xlim(range(analytical$mes)) +
  theme_ff()

# gg_f0 <- analytical %>%
#   filter(dose != "d1") %>%
#   group_by(ap_resid, mes) %>%
#   summarise(vacinacao = sum(vacinacao), internacoes = sum(internacoes), .groups = "drop") %>%
#   pivot_longer(c(vacinacao, internacoes)) %>%
#   ggplot() +
#   scale_color_brewer(palette = "Set1") +
#   facet_wrap(~ ap_resid, ncol = facetcol) +
#   labs(color = NULL, x = "") +
#   theme_ff()

gg_f0_2x <- analytical %>%
  filter(dose != "d1") %>%
  group_by(ap_resid, mes) %>%
  summarise(vacinacao = sum(vacinacao), internacoes = sum(internacoes), .groups = "drop") %>%
  # pivot_longer(c(vacinacao, internacoes)) %>%
  ggplot(aes(x = mes)) +
  scale_color_brewer(palette = "Set1") +
  facet_wrap(~ ap_resid, ncol = facetcol) +
  labs(
    color = NULL,
    x = "",
    subtitle = "Esquema vacinal completo (DU, D2 ou DR)") +
  theme_ff()

# plots -------------------------------------------------------------------

gg.outcome <- gg +
  geom_histogram(aes(internacoes), bins = 6, fill = ff.col) +
  # scale_x_log10(labels = scales::label_number_auto()) +
  labs(
    subtitle = attr(analytical$internacoes, "label"),
  ) +
  xlab("") +
  ylab("")

gg.iv <- gg +
  geom_histogram(aes(vacinacao+1), fill = ff.col, bins = 7) +
  # scale_x_log10(labels = scales::label_number_auto()) +
  labs(
    subtitle = attr(analytical$vacinacao, "label"),
  ) +
  xlab("") +
  ylab("")

gg_d1_int <- gg_d1 +
  # scale_y_log10(labels = scales::label_number_auto()) +
  geom_line(aes(mes, internacoes, color = fe), lwd = lwd, alpha = alpha) +
  ylab(attr(analytical$internacoes, "label"))

gg_d1_vac <- gg_d1 +
  # scale_y_log10(labels = scales::label_number_auto()) +
  geom_line(aes(mes, vacinacao, color = fe), lwd = lwd, alpha = alpha) +
  ylab(attr(analytical$vacinacao, "label"))

gg_d2_int <- gg_d2 +
  # scale_y_log10(labels = scales::label_number_auto()) +
  geom_line(aes(mes, internacoes, color = fe), lwd = lwd, alpha = alpha) +
  ylab(attr(analytical$internacoes, "label"))

gg_d2_vac <- gg_d2 +
  # scale_y_log10(labels = scales::label_number_auto()) +
  geom_line(aes(mes, vacinacao+1, color = fe), lwd = lwd, alpha = alpha) +
  ylab(attr(analytical$vacinacao, "label"))

gg_dr_int <- gg_dr +
  # scale_y_log10(labels = scales::label_number_auto()) +
  geom_line(aes(mes, internacoes, color = fe), lwd = lwd, alpha = alpha) +
  ylab(attr(analytical$internacoes, "label"))

gg_dr_vac <- gg_dr +
  # scale_y_log10(labels = scales::label_number_auto()) +
  geom_line(aes(mes, vacinacao+1, color = fe), lwd = lwd, alpha = alpha) +
  ylab(attr(analytical$vacinacao, "label"))

# gg_f0_int <- gg_f0 +
#   # scale_y_log10(labels = scales::label_number_auto()) +
#   geom_line(aes(mes, internacoes), col = ff.col, lwd = lwd, alpha = alpha) +
#   ylab(attr(analytical$internacoes, "label"))
# 
# gg_f0_vac <- gg_f0 +
#   scale_y_log10(labels = scales::label_number_auto()) +
#   geom_line(aes(mes, vacinacao), col = ff.col, lwd = lwd, alpha = alpha) +
#   ylab(attr(analytical$vacinacao, "label"))
# 
# gg_f0_int_vac <- gg_f0 +
#   scale_y_log10(labels = scales::label_number_auto()) +
#   geom_line(aes(mes, value+1, col = name)) +
#   ylab("")

gg_f0_int_vac <- gg_f0_2x +
  scale_y_continuous(
    # primeiro eixo
    name = attr(analytical$vacinacao, "label"),
    # segundo eixo
    sec.axis = sec_axis(~./100, name = attr(analytical$internacoes, "label"),)
  ) +
  geom_line(aes(y = vacinacao), color = ff.col) +
  geom_line(aes(y = internacoes*100), color = "red") +
  labs()

# cool facet trick from https://stackoverflow.com/questions/3695497 by JWilliman
# gg +
#   geom_histogram(bins = 10, aes(internacoes, y = ..count../tapply(..count.., ..PANEL.., sum)[..PANEL..]), fill = ff.col) +
#   scale_y_continuous(labels = scales::label_percent(accuracy = 1)) +
#   xlab(attr(analytical$internacoes, "label")) +
#   ylab("") +
#   facet_wrap(~ ap_resid, ncol = facetcol)
