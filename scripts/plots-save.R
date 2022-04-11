# setup -------------------------------------------------------------------
height <- 16
width <- 12
units <- "cm"

# publication ready tables ------------------------------------------------

# Don't need to version these files on git
# tab_inf %>%
#   as_gt() %>%
#   as_rtf() %>%
#   writeLines(con = "report/SAR-2022-016-JB-v01-T2.rtf")

# save plots --------------------------------------------------------------

ggsave(filename = "figures/hist_int.png", plot = gg.outcome, height = height, width = width, units = units)
ggsave(filename = "figures/hist_vac.png", plot = gg.iv, height = height, width = width, units = units)
ggsave(filename = "figures/d1_vac.png", plot = gg_d1_vac, height = height, width = width, units = units)
ggsave(filename = "figures/d2_vac.png", plot = gg_d2_vac, height = height, width = width, units = units)
ggsave(filename = "figures/dr_vac.png", plot = gg_dr_vac, height = height, width = width, units = units)
ggsave(filename = "figures/int.png", plot = gg_interna, height = height, width = width, units = units)
ggsave(filename = "figures/f0_int_vac.png", plot = gg_f0_int_vac, height = height, width = width, units = units)
