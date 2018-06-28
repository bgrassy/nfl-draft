# Use multiple imputation to impute missing data in all of our data.
require("mice")

impute <- function(all) {
  all[8:17][all[8:17] == 0] <- NA
  
  # Approximately 10% of the data is missing, so I used 10 imputations.
  modelFit1 <- mice(all[c(3, 10:17)], m=10, maxit = 1, method = 'pmm', seed = 500)
  # Impute seasons played
  games <- mice(all[8:9], m=10, maxit = 1, method = 'pmm', seed = 500)
  
  all[c(3, 10:17)] <- complete(modelFit1, 1)
  all[8:9] <- complete(games, 1)
  return(all)
}
