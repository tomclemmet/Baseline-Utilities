# This script loads the .tab files for each desired HSE round, selects the
# relevant variables, joins the datasets together, and adds the TTO scores.

# Loading packages
pacman::p_load(readr, dplyr, eq5d)
hse <- tibble()
yrs <- c("03", "04", "05", "06", "08", "10", "11", "12", "14")

# For loop over each year of the HSE
for (i in yrs) {
  # Reading the .tab file
  df <- read_tsv(paste("Data/hse", i, "ai.tab", sep = ""))
  # Making all names lower-case for consistency
  names(df) <- tolower(names(df))
  # Adjusting for 2014 censoring at 90+. 92.5 is the mean age for those over 90 in the 2013 HSE.
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
  # Joining data together
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