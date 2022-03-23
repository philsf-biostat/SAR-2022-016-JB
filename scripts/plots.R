# setup -------------------------------------------------------------------
# library(survminer)

ff.col <- "steelblue" # good for single groups scale fill/color brewer
ff.pal <- "Paired"    # good for binary groups scale fill/color brewer

scale_color_discrete <- function(...) scale_color_brewer(palette = ff.pal, ...)
scale_fill_discrete <- function(...) scale_fill_brewer(palette = ff.pal, ...)

gg <- analytical %>%
  ggplot() +
  theme_ff()

# plots -------------------------------------------------------------------

# gg.outcome <- 
gg +
  geom_density(aes(internacoes, fill = dose), alpha = .8) +
  xlab(attr(analytical$internacoes, "label")) +
  ylab("")
gg +
  geom_line(aes(mes, internacoes)) +
  xlab(attr(analytical$internacoes, "label")) +
  ylab("")

gg_d1 <- analytical %>%
  group_by(ap_resid, mes, fe) %>%
  filter(dose == "d1") %>%
  summarise(vacinacao = sum(vacinacao), internacoes = sum(internacoes)) %>%
  ggplot() +
  scale_color_brewer(palette = "Paired") +
  facet_wrap(~ ap_resid, ncol = 2) +
  labs(subtitle = "Dose 1") +
  theme_ff()

gg_d1 +
  geom_line(aes(mes, internacoes, color = fe))
ggsave("figures/d1_int.png", h = 16, w = 8, units = "cm")

gg_d1 +
  geom_line(aes(mes, vacinacao, color = fe))
ggsave("figures/d1_vac.png", h = 16, w = 8, units = "cm")

# cool facet trick from https://stackoverflow.com/questions/3695497 by JWilliman
# gg +
#   geom_histogram(bins = 5, aes(outcome, y = ..count../tapply(..count.., ..PANEL.., sum)[..PANEL..]), fill = ff.col) +
#   scale_y_continuous(labels = scales::label_percent(accuracy = 1)) +
#   xlab(attr(analytical$outcome, "label")) +
#   ylab("") +
#   facet_wrap(~ group, ncol = 2)
