# This takes the full dataset and fixes all of the conferences and schools, condensing them
# and delineating small schools.
all <- read.csv("data/full.csv")

levels(all$Conference) <- c(levels(all$Conference), "Unknown")
levels(all$College) <- c(levels(all$College), "Small")
for (row in row.names(all[which((all$Conference == "0") | is.na(all$Conference)),])) {
  print(all[row, "College"])
  confs <- unique(all[which(all$College == all[row, "College"]),]$Conference)
  confs <- na.omit(confs[confs != 0])
  if (length(confs) > 0) {
    all[row, "Conference"] <- confs[length(confs)]
  } else {
    all[row, "Conference"] <- "Unknown"
  }
}

small_schools <- table(all$College)
small_schools <- names(small_schools[small_schools <= 15])
all[(all$College %in% small_schools),]$College <- "Small"

write.csv(all, "data/full_fixed.csv", row.names = FALSE)
