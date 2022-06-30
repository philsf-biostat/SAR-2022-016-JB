# setup -------------------------------------------------------------------
# library(survminer)

ff.col <- "steelblue" # good for single groups scale fill/color brewer
# ff.pal <- "Blues"     # paleta sequencial
facetcol <- 2
lwd <- 1
alpha <- NA

# scale_color_discrete <- function(...) scale_color_brewer(palette = ff.pal, ...)
# scale_fill_discrete <- function(...) scale_fill_brewer(palette = ff.pal, ...)

gg <- analytical %>%
  ggplot() +
  facet_wrap(~ ap_resid, ncol = facetcol) +
  scale_color_brewer() +
  scale_fill_brewer() +
  theme_ff()

# plots -------------------------------------------------------------------

gg.outcome <- gg +
  geom_histogram(aes(internacoes, ..count../3), bins = 6, fill = "firebrick4") +
  # scale_x_log10(labels = scales::label_number_auto()) +
  labs(
    subtitle = attr(analytical$internacoes, "label"),
  ) +
  xlab("") +
  ylab("")

gg.iv <- gg +
  geom_histogram(aes(vacinacao + 1e-6), fill = ff.col, bins = 7) +
  # scale_x_log10(labels = scales::label_number_auto()) +
  labs(
    subtitle = attr(analytical$vacinacao, "label"),
  ) +
  xlab("") +
  ylab("")

gg.f1 <- analytical %>%
  filter(fe == f1) %>%
  group_by(ap_resid, mes, dose) %>%
  summarise(vacinacao = sum(vacinacao), internacoes = sum(internacoes), .groups = "drop") %>%
  ggplot(aes(x = mes)) +
  facet_wrap(~ ap_resid, ncol = facetcol) +
  scale_color_brewer(palette = "Blues") +
  labs(subtitle = f1, color = NULL, x = "", linetype = "") +
  xlim(range(analytical$mes)) +
  scale_y_continuous(
    # primeiro eixo
    name = attr(analytical$vacinacao, "label"),
    labels = scales::label_number_auto(),
    # segundo eixo
    sec.axis = sec_axis(~.*1, name = attr(analytical$internacoes, "label"),)
  ) +
  geom_line(aes(y = internacoes/1, linetype = "Internações"), color = "firebrick4", lwd = lwd) +
  geom_line(aes(y = vacinacao, color = dose), lwd = lwd, alpha = .95) +
  theme_ff()

gg.f2 <- analytical %>%
  filter(fe == f2) %>%
  group_by(ap_resid, mes, dose) %>%
  summarise(vacinacao = sum(vacinacao), internacoes = sum(internacoes), .groups = "drop") %>%
  ggplot(aes(x = mes)) +
  facet_wrap(~ ap_resid, ncol = facetcol) +
  scale_color_brewer(palette = "Blues") +
  labs(subtitle = f2, color = NULL, x = "", linetype = "") +
  xlim(range(analytical$mes)) +
  scale_y_continuous(
    # primeiro eixo
    name = attr(analytical$vacinacao, "label"),
    labels = scales::label_number_auto(),
    # segundo eixo
    sec.axis = sec_axis(~.*1, name = attr(analytical$internacoes, "label"),)
  ) +
  geom_line(aes(y = internacoes/1, linetype = "Internações"), color = "firebrick4", lwd = lwd) +
  geom_line(aes(y = vacinacao, color = dose), lwd = lwd, alpha = .95) +
  theme_ff()

gg.f3 <- analytical %>%
  filter(fe == f3) %>%
  group_by(ap_resid, mes, dose) %>%
  summarise(vacinacao = sum(vacinacao), internacoes = sum(internacoes), .groups = "drop") %>%
  ggplot(aes(x = mes)) +
  facet_wrap(~ ap_resid, ncol = facetcol) +
  scale_color_brewer(palette = "Blues") +
  labs(subtitle = f3, color = NULL, x = "", linetype = "") +
  xlim(range(analytical$mes)) +
  scale_y_continuous(
    # primeiro eixo
    name = attr(analytical$vacinacao, "label"),
    labels = scales::label_number_auto(),
    # segundo eixo
    sec.axis = sec_axis(~.*1, name = attr(analytical$internacoes, "label"),)
  ) +
  geom_line(aes(y = internacoes/1, linetype = "Internações"), color = "firebrick4", lwd = lwd) +
  geom_line(aes(y = vacinacao, color = dose), lwd = lwd, alpha = .95) +
  theme_ff()

gg_interna <- analytical %>%
  select(-dose, -vacinacao) %>%
  distinct() %>%
  ggplot() +
  facet_wrap(~ ap_resid, ncol = facetcol) +
  labs(color = NULL, x = "") +
  # scale_y_log10(labels = scales::label_number_auto()) +
  geom_line(aes(mes, internacoes, color = fe), lwd = lwd, alpha = alpha) +
  scale_color_brewer(palette = "Reds") +
  ylab(attr(analytical$internacoes, "label")) +
  theme_ff()

# obsolete ----------------------------------------------------------------
# 
# gg_d1 <- analytical %>%
#   group_by(ap_resid, mes, fe) %>%
#   filter(dose == "d1") %>%
#   summarise(vacinacao = sum(vacinacao), internacoes = sum(internacoes), .groups = "drop") %>%
#   ggplot() +
#   facet_wrap(~ ap_resid, ncol = facetcol) +
#   labs(subtitle = d1, color = NULL, x = "") +
#   theme_ff()
# 
# gg_d2 <- analytical %>%
#   group_by(ap_resid, mes, fe) %>%
#   filter(dose == "d2") %>%
#   summarise(vacinacao = sum(vacinacao), .groups = "drop") %>%
#   ggplot() +
#   facet_wrap(~ ap_resid, ncol = facetcol) +
#   labs(subtitle = d2, color = NULL, x = "") +
#   theme_ff()
# 
# gg_dr <- analytical %>%
#   group_by(ap_resid, mes, fe) %>%
#   filter(dose == "dr") %>%
#   summarise(vacinacao = sum(vacinacao), .groups = "drop") %>%
#   ggplot() +
#   facet_wrap(~ ap_resid, ncol = facetcol) +
#   labs(subtitle = dr, color = NULL, x = "") +
#   xlim(range(analytical$mes)) +
#   theme_ff()
# 
# # gg_f0 <- analytical %>%
# #   filter(dose != "d1") %>%
# #   group_by(ap_resid, mes) %>%
# #   summarise(vacinacao = sum(vacinacao), internacoes = sum(internacoes), .groups = "drop") %>%
# #   pivot_longer(c(vacinacao, internacoes)) %>%
# #   ggplot() +
# #   facet_wrap(~ ap_resid, ncol = facetcol) +
# #   labs(color = NULL, x = "") +
# #   theme_ff()
# gg_f0_2x <- analytical %>%
#   filter(dose == "d2") %>%
#   group_by(ap_resid, mes) %>%
#   summarise(vacinacao = sum(vacinacao), internacoes = sum(internacoes), .groups = "drop") %>%
#   # pivot_longer(c(vacinacao, internacoes)) %>%
#   ggplot(aes(x = mes)) +
#   facet_wrap(~ ap_resid, ncol = facetcol) +
#   labs(
#     color = NULL,
#     x = "",
#     subtitle = "Esquema vacinal completo (D2 ou DU)") +
#   theme_ff()
# 
# gg_d1_vac <- gg_d1 +
#   geom_line(aes(mes, vacinacao, color = fe), lwd = lwd, alpha = alpha) +
#   scale_color_brewer(palette = "Blues") +
#   ylab(attr(analytical$vacinacao, "label"))
# 
# gg_d2_vac <- gg_d2 +
#   geom_line(aes(mes, vacinacao, color = fe), lwd = lwd, alpha = alpha) +
#   scale_color_brewer(palette = "Blues") +
#   ylab(attr(analytical$vacinacao, "label"))
# 
# gg_dr_vac <- gg_dr +
#   geom_line(aes(mes, vacinacao, color = fe), lwd = lwd, alpha = alpha) +
#   scale_color_brewer(palette = "Blues") +
#   ylab(attr(analytical$vacinacao, "label"))
# 
# # gg_f0_int <- gg_f0 +
# #   # scale_y_log10(labels = scales::label_number_auto()) +
# #   geom_line(aes(mes, internacoes), col = ff.col, lwd = lwd, alpha = alpha) +
# #   ylab(attr(analytical$internacoes, "label"))
# # 
# # gg_f0_vac <- gg_f0 +
# #   scale_y_log10(labels = scales::label_number_auto()) +
# #   geom_line(aes(mes, vacinacao), col = ff.col, lwd = lwd, alpha = alpha) +
# #   ylab(attr(analytical$vacinacao, "label"))
# # 
# # gg_f0_int_vac <- gg_f0 +
# #   scale_y_log10(labels = scales::label_number_auto()) +
# #   geom_line(aes(mes, value+1, col = name)) +
# #   ylab("")
# 
# gg_f0_int_vac <- gg_f0_2x +
#   scale_y_continuous(
#     # primeiro eixo
#     name = attr(analytical$vacinacao, "label"),
#     labels = scales::label_number_auto(),
#     # segundo eixo
#     sec.axis = sec_axis(~.*1, name = attr(analytical$internacoes, "label"),)
#   ) +
#   geom_line(aes(y = vacinacao), color = ff.col) +
#   geom_line(aes(y = internacoes/1), color = "firebrick4") +
#   labs()

# cool facet trick from https://stackoverflow.com/questions/3695497 by JWilliman
# gg +
#   geom_histogram(bins = 10, aes(internacoes, y = ..count../tapply(..count.., ..PANEL.., sum)[..PANEL..]), fill = ff.col) +
#   scale_y_continuous(labels = scales::label_percent(accuracy = 1)) +
#   xlab(attr(analytical$internacoes, "label")) +
#   ylab("") +
#   facet_wrap(~ ap_resid, ncol = facetcol)
