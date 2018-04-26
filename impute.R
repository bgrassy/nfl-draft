# Use multiple imputation to impute missing data in all of our data.
require("mice")
dfs <- list()
for (year in 2008:2018) {
  dfs = c(dfs, paste("data/", year, ".csv", sep=""))
}
dfs <- lapply(dfs, read.csv)
all <- do.call(rbind, dfs)

all[9:18][all[9:18] == 0] <- NA

# Approximately 10% of the data is missing, so I used 10 imputations.
modelFit1 <- mice(all[c(3, 11:18)], m=10, maxit = 30, method = 'pmm', seed = 500)
games <- mice(all[9:10], m=10, maxit = 30, method = 'pmm', seed = 500)

all[c(3, 11:18)] <- complete(modelFit1, 10)
all[9:10] <- complete(games, 10)

write.csv(all, "data/full.csv", row.names = FALSE)
