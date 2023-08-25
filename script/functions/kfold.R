# K FOLD CROSS VALIDATION

# This file implements k-fold cross validation for the selected models. The
# output is a table with predictions errors for every observation for every
# model, giving maximum flexibility for analysing the results. The output file
# is saved as `kfold-errs.csv` in the `Output` folder so that results can be 
# loaded without re-running the analysis.
# Note that this file may take some time to run due to the complexity of 
# fitting the ALDVMM model.

################################################################################

# Loading packages
if (! "pacman" %in% installed.packages()) {install.packages("pacman")}
pacman::p_load(
  dplyr, tidyr, ggplot2, rsample, rms, aldvmm
)

# Setup - defining useful variables and setting a random seed so that results
# are fully reproducible.
theme_set(theme_bw())
set.seed(84848484)
aldvmm.coefs <- c(
  1, -0.01, -0.001, 1, -0.01, -0.001, 1, -0.01, -0.001,
  0, 0, 0, 0, -2, -2, -2
)

# Loading the cleaned HSE data from a csv
hse <- readr::read_csv("Data/hse.csv", show_col_types = FALSE) |>
  mutate(across(Sex:AD, as.factor))

# Defining the list of models to test
models <- list(
  Ones = \(x) NULL,
  Mean = \(x) lm(Index ~ 1, data = x),
  Linear = \(x) lm(Index ~ Age, data = x),
  Quadratic = \(x) lm(Index ~ poly(Age, 2, raw = TRUE), data = x),
  `Poly-3` = \(x) lm(Index ~ poly(Age, 3, raw = TRUE), data = x),
  `Poly-4` = \(x) lm(Index ~ poly(Age, 4, raw = TRUE), data = x),
  `Poly-5` = \(x) lm(Index ~ poly(Age, 5, raw = TRUE), data = x),
  `Poly-6` = \(x) lm(Index ~ poly(Age, 6, raw = TRUE), data = x),
  `Poly-7` = \(x) lm(Index ~ poly(Age, 7, raw = TRUE), data = x),
  `Poly-8` = \(x) lm(Index ~ poly(Age, 8, raw = TRUE), data = x),
  `Poly-9` = \(x) lm(Index ~ poly(Age, 9, raw = TRUE), data = x),
  `Poly-10` = \(x) lm(Index ~ poly(Age, 10, raw = TRUE), data = x),
  ALDVMM = \(x) aldvmm(Index ~ I(Age/10) + I((Age/10)^2) | I(Age/10), data = x,
                       psi = c(0.883, -0.594), ncmp = 3,
                       init.est = aldvmm.coefs, optim.method = "nlminb"),
  `RCS-3` = \(x) lm(Index ~ rcs(Age, 3), data = x),
  `RCS-4` = \(x) lm(Index ~ rcs(Age, 4), data = x),
  `RCS-5` = \(x) lm(Index ~ rcs(Age, 5), data = x),
  `RCS-6` = \(x) lm(Index ~ rcs(Age, 6), data = x),
  `RCS-7` = \(x) lm(Index ~ rcs(Age, 7), data = x),
  `RCS-8` = \(x) lm(Index ~ rcs(Age, 8), data = x),
  `RCS-9` = \(x) lm(Index ~ rcs(Age, 9), data = x),
  `RCS-10` = \(x) lm(Index ~ rcs(Age, 10), data = x)
)

# Initialising a blank results object
results <- list(
  errors = tibble(),
  folds = list(),
  models = list()
  )

# The number of folds and repeats to be used.
k <- 10
rep <- 1

# K FOLD CROSS VALIDATION
# Loop to repeat the process for men and women
for (s in c("Male", "Female")) {
  
  # Creating the splits in the data using the rsample package's vfold_cv(). The
  # data is stratified by age to ensure balanced splits
  results$folds[[s]] <- vfold_cv(filter(hse, Sex == s), v = k, repeats = rep,
                                 strata = Age, breaks = 10)
  
  # Loop to repeat the process for each model
  for (label in names(models)) {
    # lapply() function to loop through each test/training pair
    r <- lapply(
      results$folds[[s]]$splits,
      function (x) {
        # Defining the test and training datasets
        train <- analysis(x)
        test <- assessment(x)
        
        # Running the model on the training set
        model <- models[[label]](train)
        
        # Fitting predicted values to the training set. This differs slightly
        # for ALDVMM and for the model where the predictions are all 1.
        if (label == "ALDVMM") {
          message(paste("\nALDVMM fitted, AIC =", model$gof$aic))
          train$Predicted <- model$pred$yhat
        } else if (label == "Ones") {
          train$Predicted <-  1
        }
        else {
          train$Predicted <- predict(model, train)
        }
        
        # Using the predictions for the training set to predict data in the
        # test set.
        test <- train |> 
          group_by(Age) |> 
          summarise(Predicted = mean(Predicted)) |> 
          right_join(test, by = "Age")
        
        # Creating an output object
        list(
          act = test$Index,
          pred = test$Predicted,
          errs = test$Index - test$Predicted,
          age = test$Age,
          mod = model
        )
      }
    )
    # Adding the results from this run to the results table
    for (i in 1:(k*rep)) {
      results$errors <- results$errors |> 
        bind_rows(
          tibble(
            Model = as.factor(label),
            Fold = as.factor(paste(s, rep, i, sep = "-")),
            Sex = as.factor(s),
            Age = r[[i]]$age,
            Actual = r[[i]]$act,
            Predicted = r[[i]]$pred,
            Error = r[[i]]$errs
          )
        )
      results$models[[paste(label, s, i, sep = "-")]] <- r[[i]]$mod
    }
    
    # Progress message
    message(".", appendLF = FALSE)
  }
}

# Writing the results to a file
readr::write_csv(results$errors, "Output/kfold-errs-10k.csv")

