# Test speed of business.csv import using four different methods, measured with microbenchmark

library(tidyverse)
library(vroom)
library(data.table)
library(microbenchmark)


# Microbenchmark:
results_vroom <- microbenchmark(
  'base::read.csv' = read.csv("CSVFiles/business.csv"),
  'readr::read_csv' = read_csv("CSVFiles/business.csv"),
  'vroom::vroom' = vroom("CSVFiles/business.csv"),
  'data.table::fread' = fread("CSVFiles/business.csv"),
  times = 25L
)


### Plot and save results
# Start pdf capture
pdf("business_microbenchmark.pdf")

# plot
autoplot(object = results_vroom) +
  scale_y_log10() +
  labs(y = "Time [milliseconds], logged",
       title = "Business.csv Benchmark",
       caption = "Package \"Microbenchmark\"")

# sign dev off
dev.off()

# save results for later
fwrite(results_vroom, file = "results_microbenchmark.csv")
