library(dplyr)
library(tibble)

# ---- 1. Example Data with Features ----
# nodes <- tibble(
#   id = 1:10,
#   label = c("Alice", "Bob", "Charlie", "Diana", "Eve", "Acme Corp", "Initech", "AI Project", "R Language", "London"),
#   type = c("Person", "Person", "Person", "Person", "Person", "Company", "Company", "Project", "Technology", "Location"),
#   role = c("Data Scientist", "Engineer", "Manager", "Analyst", "Intern", NA, NA, NA, NA, NA),
#   department = c("AI", "IT", "Management", "AI", "HR", NA, NA, NA, NA, NA),
#   join_date = as.Date(c("2020-01-10", "2019-05-15", "2017-09-01", "2021-03-20", "2022-06-01", NA, NA, NA, NA, NA)),
#   performance_score = c(92, 85, 88, 90, 70, NA, NA, NA, NA, NA)
# )
# 
# edges <- tibble(
#   from = c(1, 2, 3, 4, 1, 2, 3, 5, 6, 5, 7, 8, 7, 3, 1, 9, 2),
#   to   = c(6, 6, 7, 7, 8, 8, 8, 8, 8, 9, 9, 7, 1, 9, 9, 10, 10),
#   relation = c("works_at", "works_at", "works_at", "works_at", "contributes_to", "contributes_to", "contributes_to",
#                "sponsors", "sponsors", "develops", "uses", "implements", "led_by", "uses", "uses", "based_in", "based_in"),
#   strength = c(4,3,2,1,2,3,4,5,3,2,5,3,3,1,3,1,2),
#   timestamp = as.Date(c("2020-01-15", "2019-06-01", "2017-10-01", "2021-04-01", "2021-07-01", "2020-08-01", "2021-12-01",
#                         "2022-01-01", "2020-09-01", "2020-07-01", "2019-12-15", "2022-01-10", "2022-03-01", "2021-10-15",
#                         "2022-05-01", "2022-07-01", "2021-11-01"))
# )

qs::qload("graph_data.qs", nthreads = 3)