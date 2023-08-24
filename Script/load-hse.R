# Loading packages
require(tidyverse)
theme_set(theme_bw())

# Loading and tidying the data
hse <- tibble()
yrs <- c("03", "04", "05", "06", "08", "10", "11", "12", "14")
for (i in yrs) {
  # Reading the .tab file
  df <- read_tsv(paste("Data/hse", i, "ai.tab", sep = ""))
  # Making all names lower-case for consistency
  names(df) <- tolower(names(df))
  # Adjusting for 2014 censoring at 90+
  if (i == "14")  {df <- df |> mutate(age = case_match(age90, 90 ~ 92.5, .default = age90))}
  
  df <- df |> 
    # Renaming variables for ease of analysis
    rename(Sex = sex, Age = age, MO = mobility, SC = selfcare, 
           UA = usualact, PD = pain, AD = anxiety) |> 
    # Selecting the required variables
    select(Sex, Age, MO, SC, UA, PD, AD)
  df <- df |> 
    # Adding a 'year' variable to keep track of the data's source
    mutate(Year = as.numeric(paste("20", i, sep="")),
           # Changing sex to factor variable
           Sex = as.factor(case_match(Sex, 2 ~ "Female", 1 ~ "Male")),
           # Changing scores to factor variables
           across(c(MO, SC, UA, PD, AD), as.factor)
    ) |> 
    # Filtering out missing values
    filter(! if_any(everything(), ~. %in% c(-9, -8, -2, -1)))
  # Pooling data together
  hse <- hse |> bind_rows(df)
}
# Removing the temporary variables
rm(df, i, yrs)

# Adding HSUVs using the standard UK value set
hse$Index <- eq5d::eq5d(
  scores = hse[,3:7],
  version = "3L",
  country = "UK",
  type = "TTO"
)

# Density plot of ages in different years
hse |> ggplot(aes(x=Age, colour=as.factor(Year))) +
  geom_density() +
  
  labs(title = "Density Plot of Ages for Different HSE Rounds") +
  scale_colour_viridis_d(name = "Year")
ggsave("Output/age_dist.png", height = 4, width = 7)

# Density plot of indices in different years
hse |> ggplot(aes(x=Index, colour=as.factor(Year))) +
  geom_density(linewidth=0.2) +
  
  labs(title = "Density plot of HSUVs for Different HSE Rounds") +
  coord_cartesian(ylim=c(0, 2.5)) +
  scale_colour_viridis_d(name = "Year")

ggsave("Output/HSUV_dist.png", height = 4, width = 7)

# Plot of mean HSUVs by age and sx
hse |> 
  group_by(cut(Year, 4), Age, Sex) |> 
  filter(! `cut(Year, 4)` %in% c("(2006,2008]", "(2008,2011]")) |> 
  summarise(n = n(), Mean = mean(Index), .groups = "drop") |> 
  
  ggplot(aes(x = Age, y = Mean)) +
  geom_line(aes(group = `cut(Year, 4)`, colour = `cut(Year, 4)`)) +
  geom_density(aes(x = Age, fill = `cut(Year, 4)`), alpha = 0.2, inherit.aes = FALSE) +
  facet_grid(rows = vars(Sex)) +
  
  labs(title = "Comparing Mean HSUVs for Early and Late HSE Rounds") +
  scale_y_continuous(sec.axis = sec_axis(~. * 1000, name = "Sample Size"))

ggsave("Output/HSUV_compare.png", height = 4, width = 7)