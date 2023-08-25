# Loading packages and data ---------------------------------------------------
pacman::p_load(dplyr, tidyr, readr, ggplot2)
theme_set(theme_bw())

np.bslns <- read_csv("data/npara-baselines.csv") 
p.bslns <- read_csv("data/para-baselines.csv") |> 
  pivot_longer(HSE0306:HSE14, names_to="Source", values_to="Index")

# Fig. 1: Non-Parametric Baselines --------------------------------------------
np.bslns |>
  ggplot(aes(x = Ages, y = HSUV, fill = Source)) +
  geom_col(position = "dodge") +
  geom_errorbar(aes(ymin = Min, ymax = Max),
                position = position_dodge(0.9), width = 0.2) +
  facet_grid(rows = vars(Gender)) +
  
  coord_cartesian(ylim = c(0.5, 1)) +
  #scale_fill_manual(values = pal, labels = c("Kind et al. (1999)", "Szende et al. (2014)")) +
  scale_fill_viridis_d(
    begin = 0.2, end = 0.8, option = "E",
    labels = c("Kind et al. (1999)", "Szende et al. (2014)")
    ) +
  theme_bw() +
  theme(panel.grid = element_blank(),
        legend.position = "bottom")
ggsave("output/literature/np-bslns.png", height = 5, width = 7)


# Fig. 2: Parametric Baselines ------------------------------------------------
p.bslns |> 
  ggplot(aes(x=Age, y=Index, colour=Source)) + 
  geom_line() +
  facet_grid(cols=vars(Sex)) +
  
  theme_bw() +
  scale_colour_viridis_d(
    begin = 0.2, end = 0.8, option = "E",
    labels = c("Ara & Brazier (2010)", "Hernandez Alava et al. (2022)")
  ) +
  theme(
    legend.position = "bottom",
    panel.grid.minor = element_blank()
  ) +
  coord_cartesian(ylim = c(0.5, 1))
  
ggsave("output/literature/p-bslns.png", height=5, width=7)
