# Loading packages and data ---------------------------------------------------
pacman::p_load(dplyr, readr, tidyr, rms, ggplot2)
theme_set(theme_bw())

hse <- read_csv("data/hse.csv")
lt <- read_csv("data/lifetables-1820.csv")

# Half-cycle correction
lt$s_hcc <- NA
for (i in 1:84) {
  lt$s_hcc[i] <- 0.5 * lt$Survival[i] + 0.5 * lt$Survival[i + 1]
}
for (i in 86:169) {
  lt$s_hcc[i] <- 0.5 * lt$Survival[i] + 0.5 * lt$Survival[i + 1]
}

# Appendix C1-2: RCS-7 Predictions --------------------------------------------
rcs7 <- lm(Index ~ rcs(Age, 7)*Sex, data = hse)
quad <- lm(Index ~ poly(Age, 2, raw = TRUE)*Sex, data = hse)

target <- as_tibble(expand.grid(Age = c(16:100), Sex = c("Male", "Female")))
preds <- target |> 
  mutate(
    rcs7 = predict(rcs7, target),
    quad = predict(quad, target)
  )
write_csv(select(preds, -quad), "output/discussion/rcs7-preds.csv")

preds |> 
  pivot_longer(rcs7:quad, names_to = "Model", values_to = "Baseline") |>  
  ggplot(aes(x = Age, y = Baseline, colour = forcats::fct_rev(Sex), linetype = Model, alpha = Model)) +
  geom_line(aes()) +

  scale_linetype_manual(values = c(2, 1), labels = c("Quadratic", "RCS-7")) +
  scale_alpha_manual(values = c(0.5, 1)) +
  guides(alpha = "none") +
  labs(
    caption = "Source: Pooled HSE 2003-2014",
    colour = "Sex"
  ) 
ggsave("output/discussion/rcs7-vis.png", height = 4, width = 7)        

# QALE Estimates --------------------------------------------------------------

# Discount rate
r <- 0.035

qale <- preds |> 
  full_join(lt, by = c("Age", "Sex")) |> 
  filter(Age != 100) |> 
  mutate(
    rcs7.q = mapply(\(x, y, z)
                    sum(
                      rcs7[Age >= x & Sex == y] * 
                        (s_hcc[Age >= x & Sex == y]/z) *
                        1/((1+r)^(0:(99 - x)))
                    ),
                    Age, Sex, s_hcc
    ),
    quad.q = mapply(\(x, y, z)
                    sum(
                      quad[Age >= x & Sex == y] * 
                        (s_hcc[Age >= x & Sex == y]/z) *
                        1/((1+r)^(0:(99 - x)))
                    ),
                    Age, Sex, s_hcc
    ),
  )

# Appendix D1: Difference in absolute shortfall -------------------------------
qale |> 
  mutate(dAS = rcs7.q - quad.q) |> 
  ggplot(aes(x = Age)) + 
  geom_hline(yintercept = 0, linetype = 2) +
  
  geom_line(aes(y = dAS, colour = Sex)) +
  labs(
    y = "Difference in Absolute Shortfall",
    caption = "Source: Models trained on pooled HSE 2003-2014, 
                life tables from ONS 2018-2020"
  )
ggsave("output/discussion/AS-diff.png", height = 4, width = 8)

# Appendix D2: Range of differences in proportional shortfall -----------------
qale |> 
  mutate(
    dPS = case_when(
      rcs7.q > quad.q ~ (rcs7.q - quad.q)/rcs7.q,
      rcs7.q < quad.q ~ (rcs7.q - quad.q)/quad.q
    )
  ) |> 
  ggplot(aes(x = Age)) + 
  facet_grid(cols = vars(Sex)) +
  geom_hline(yintercept = 0, linetype = 2) +
  
  geom_ribbon(aes(ymin = pmin(0, dPS), ymax = pmax(0, dPS), fill = Sex), 
              alpha = 0.5, show.legend = FALSE) +
  
  labs(
    y = "% Point Difference in Proportional Shortfall",
    caption = "Source: Models trained on pooled HSE 2003-2014, 
                life tables from ONS 2018-2020"
  ) +
  scale_y_continuous(labels = scales::percent)
ggsave("output/discussion/PS-diff.png", height = 4, width = 8)