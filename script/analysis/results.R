# Loading packages and data ---------------------------------------------------
pacman::p_load(dplyr, tidyr, readr, ggplot2)
theme_set(theme_bw())

exclude1 <- c("Ones", "Poly-11", "Poly-12", "RCS-11", "RCS-12")
exclude2 <- c(exclude1, "Mean", "Poly-6", "Poly-7", 
              "Poly-8", "Poly-9", "Poly-10")
capt <- "Source: HSE 2003-2014, 10-fold cross-validation"
lw <- 0.25
al <- 0.5
mod.ord <- c(
  "Ones",  "Mean", "Linear", "ALDVMM", "Quadratic", "Poly-3", "Poly-4", 
  "Poly-5", "Poly-6", "Poly-7", "Poly-8",  "Poly-9", "Poly-10", "Poly-11", 
  "Poly-12", "RCS-3", "RCS-4", "RCS-5", "RCS-6", "RCS-7", "RCS-8", "RCS-9", 
  "RCS-10", "RCS-11", "RCS-12"
)

errs <- read_csv("output/results/kfold-errs-10.csv", show_col_types = FALSE) |> 
  mutate(
    Model = factor(Model, levels = mod.ord),
    Type = factor(case_when(
      substr(Model, 1, 4) %in% c("Poly", "Quad") ~ "Polynomial",
      substr(Model, 1, 4) == "RCS-" ~ "RCS",
      Model == "ALDVMM" ~ "ALDVMM",
      Model == "Linear" ~ "Linear",
      .default = "Other"
    ), levels = c("Linear", "ALDVMM", "Polynomial", "RCS", "Other"))
  ) |> 
  filter(! if_any(everything(), is.na))

# Appendix B1: Overall Error Statistics ---------------------------------------
etab.ovr <- errs |> 
  group_by(Type, Model, Fold) |> 
  summarise(
    ME = mean(Error),
    MSE = mean(Error^2),
    MAE = mean(abs(Error)),
    .groups = "drop_last"
  ) |> 
  summarise(
    ME = mean(ME),
    RMSE = sqrt(mean(MSE)),
    MAE = mean(MAE),
    .groups = "drop"
  )
write_csv(etab.ovr, "output/results/etab-ovr.csv")

# Appendix B2: Age-stratified Error Statistics --------------------------------
etab.age <- errs |> 
  group_by(Agedec = cut(
    Age, quantile(Age, seq(0, 1, 0.1)), 
    include.lowest = TRUE
  ), 
  Type, Model, Fold
  ) |> 
  summarise(
    ME = mean(Error),
    MSE = mean(Error^2),
    MAE = mean(abs(Error)),
    .groups = "drop_last"
  ) |> 
  summarise(
    ME = mean(ME),
    RMSE = sqrt(mean(MSE)),
    MAE = mean(MAE),
    .groups = "drop"
  )
write_csv(etab.age, "output/results/etab-age.csv")

# Fig. 7: Visualisations for one fold -----------------------------------------
errs |> 
  filter(Fold == "Female-2", ! Model %in% exclude1) |> 
  group_by(Model, Age) |>
  summarise(n = n(), Actual = mean(Actual), Predicted = mean(Predicted),
            .groups = "drop") |> 
  mutate(predlab = "Predicted", actlab = "Actual Mean") |> 
  
  ggplot(aes(x = Age)) +
  facet_wrap(vars(Model)) +
  
  geom_point(aes(y = Actual, alpha = n, shape = actlab),  colour = "#cbba69") +
  geom_line(aes(y = Predicted, linetype = predlab)) +
  
  guides(colour = "none") +
  labs(
    alpha = "Sample Size",
    linetype = "", shape = "",
    y = "HSUV",
    caption = "Source: Pooled HSE 2003-2014, 10-fold cross validation"
  ) +
  theme(
    panel.grid.minor = element_blank(),
    legend.position = "bottom"
  ) +
  coord_cartesian(ylim = c(0.4, 1))
ggsave("output/results/kfold-vis.png", width = 10, height = 10)

# Fig. 8: ALDVMM and quadratic differences ------------------------------------
errs |> 
  filter(Model %in% c("ALDVMM", "Quadratic")) |> 
  group_by(Model, Age, Sex, Fold) |> 
  summarise(Predicted = mean(Predicted)) |> 
  pivot_wider(names_from = Model, values_from = Predicted) |> 
  mutate(diff = ALDVMM - Quadratic) |> 
  
  ggplot(aes(x = Age, y = diff)) + 
  facet_grid(cols = vars(Sex)) +
  geom_hline(yintercept = 0, linetype = 2) +
  
  geom_line(aes(colour = Sex, group = Fold), 
            alpha = 0.8, show.legend = FALSE) +
  
  labs(
    caption = "Source: Pooled HSE 2003-2014, 10-fold cross-validation",
    y = "ALDVMM Prediction - Quadratic Prediction"
  )
ggsave("output/results/aldvmm-vis.png", height = 4, width = 8)

# Fig. 9: Overall RMSE plot ---------------------------------------------------
etab.ovr |> 
  mutate(
      ME.diff = ME - filter(etab.ovr, Model == "Linear")$ME,
      RMSE.loss = RMSE - filter(etab.ovr, Model == "Linear")$RMSE,
      MAE.loss = MAE - filter(etab.ovr, Model == "Linear")$MAE
    ) |> 
  filter(! Model %in% exclude2) |> 
  ggplot(aes(x = Model, y = RMSE.loss)) +
  facet_grid(cols = vars(Type), scale = "free_x", space = "free") +
  geom_hline(yintercept = 0, linetype = 2) +
  
  geom_line(aes(group = Type), 
            colour = "#31446b", linewidth = lw, alpha = al) +
  geom_point(colour = "#31446b") +
  
  labs(
    caption = capt,
    y = "Difference in RMSE"
  ) +
  theme(
    axis.text.x = element_text(angle = 270, vjust = 0.5, hjust = 0.1),
    panel.grid.minor = element_blank(),
    strip.text = element_blank()
  )

ggsave("output/results/rmse-loss-ovr.png", height = 4, width = 7)

# Fig. 10: RMSE plot by age ---------------------------------------------------
etab.age |> 
  filter(Model == "Linear") |> 
    right_join(etab.age, by = "Agedec") |> 
    group_by(Agedec, Type = Type.y, Model = Model.y) |> 
    summarise(
      ME.diff = ME.y - ME.x,
      RMSE.loss = RMSE.y - RMSE.x,
      MAE.loss = MAE.y - MAE.x,
      .groups = "drop"
    ) |> 
  filter(! Model %in% exclude2) |> 
  
  ggplot(aes(x = Model, y = RMSE.loss)) +
  facet_grid(cols = vars(Type), scale = "free_x", space = "free") +
  geom_hline(yintercept = 0, linetype = 2) +
  
  geom_line(aes(group = Agedec, colour = Agedec), linewidth = lw, alpha = al) +
  geom_point(aes(colour = Agedec), alpha = 0.8) +
  
  labs(
    y = "Difference in RMSE",
    colour = "Age Decile",
    caption = capt
  ) +
  scale_colour_viridis_d(option = "E", begin = 0.2, end = 0.8) +
  theme(
    axis.text.x = element_text(angle = 270, vjust = 0.5, hjust = 0.1),
    panel.grid.minor = element_blank(),
    legend.position = "bottom",
    strip.text = element_blank()
  )

ggsave("output/results/rmse-loss-age.png", height = 5, width = 7)

# Fig. 11: Ones vs linear error distributions (relevant for MAEs) -------------
errs |> 
  filter(Model %in% c("Ones", "Linear")) |> 
  group_by(Model) |> 
  ggplot() +
  geom_vline(xintercept = 0, linetype = 2) +
  
  geom_density(aes(x = Error, fill = Model), alpha = 0.5, linewidth = lw) +
  
  labs(
    y = "Density",
    caption = "Source: Pooled HSE 2004-2014, 10-fold cross-validation"
  ) +
  coord_cartesian(xlim = c(-1.239, 0.3)) +
  scale_fill_viridis_d(option = "E", begin = 0.2, end = 0.8)

ggsave("output/results/ones-v-lin-dens.png", width = 7, height = 4)

# Fig. 12: Overall ME plot ----------------------------------------------------
etab.ovr |> 
  filter(! Model %in% c(exclude2, "ALDVMM")) |> 
  mutate(Type = case_match(Type, "Linear" ~ "Polynomial", .default = Type)) |> 
  ggplot(aes(x = Model, y = ME)) +
  facet_grid(cols = vars(Type), scale = "free_x", space = "free") +
  geom_hline(yintercept = 0, linetype = 2) +
  
  geom_line(aes(group = Type), 
            colour = "#31446b", linewidth = lw, alpha = al) +
  geom_point(colour = "#31446b") +
  
  labs(
    caption = capt
  ) +   
  theme(
    axis.text.x = element_text(angle = 270, vjust = 0.5, hjust = 0.1),
    panel.grid.minor = element_blank(),
    strip.text = element_blank()
  )
ggsave("output/results/me-ovr.png", height = 4, width = 7)

# Fig. 13: ME plot by age -----------------------------------------------------
etab.age |> 
  filter(! Model %in% c(exclude2, "ALDVMM")) |> 
  mutate(Type = case_match(Type, "Linear" ~ "Polynomial", .default = Type)) |> 
  ggplot(aes(x = Model, y = ME)) +
  facet_grid(cols = vars(Type), scale = "free_x", space = "free") +
  geom_hline(yintercept = 0, linetype = 2) +
  
  geom_line(aes(group = Agedec, colour = Agedec), linewidth = lw, alpha = al) +
  geom_point(aes(colour = Agedec)) +
  
  
  labs(
    caption = "Source: Pooled HSE 2003-2014, 10-fold cross-validation",
    colour = "Age Decile"
  ) +
  theme(
    axis.text.x = element_text(angle = 270, vjust = 0.5, hjust = 0.1),
    panel.grid.minor = element_blank(),
    strip.text = element_blank(),
    legend.position = "bottom"
  ) +
  scale_colour_viridis_d(option = "E", begin = 0.2, end = 0.8)
ggsave("output/results/me-age.png", height = 5.5, width = 7)
