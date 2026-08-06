library(dplyr)

all_rcs <- lapply(Sys.glob("Data/spec_yr_files/*.rds"), readRDS)
New_compiled_yrs <- do.call(bind_rows,all_rcs)

write.csv(New_compiled_yrs, "Data/New_compiled_yrs.csv", row.names = FALSE)

