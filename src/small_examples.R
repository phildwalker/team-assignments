library(tidyverse)

library(ompr)
library(ompr.roi)
library(ROI.plugin.glpk)
#----knapsack example
max_capacity <- 5
n <- 10
set.seed(1234)
weights <- runif(n, max = max_capacity)

df <- weights %>% as.data.frame() %>% 
  mutate(id = row_number())

MIPModel() %>% 
  add_variable(x[i], i = 1:n, type = "binary") %>% 
  set_objective(sum_over(weights[i] * x[i], i = 1:n), "max") %>% 
  add_constraint(sum_over(weights[i] * x[i], i = 1:n) <= max_capacity) %>% 
  solve_model(with_ROI(solver = "glpk")) %>% 
  get_solution(x[i]) %>% 
  filter(value > 0)

df %>% 
  filter(id %in% c(1,6,7,8)) %>% 
  summarise(Tot = sum(.))
  


#-----------

max_bins <- 10
bin_size <- 3
n <- 10

weights <- runif(n, max = bin_size)

wt_tbl <- weights %>% as.data.frame() %>% 
  mutate(id = row_number()) %>% 
  rename(wt = 1)

output <- MIPModel() %>%
  add_variable(y[i], i = 1:max_bins, type = "binary") %>%
  add_variable(x[i, j], i = 1:max_bins, j = 1:n, type = "binary") %>%
  set_objective(sum_over(y[i], i = 1:max_bins), "min") %>%
  add_constraint(sum_over(weights[j] * x[i, j], j = 1:n) <= y[i] * bin_size, i = 1:max_bins) %>%
  add_constraint(sum_over(x[i, j], i = 1:max_bins) == 1, j = 1:n) %>%
  solve_model(with_ROI(solver = "glpk", verbose = TRUE)) %>%
  get_solution(x[i, j]) %>%
  filter(value > 0) %>%
  arrange(i)

# comp <-
  output %>% 
  left_join(., wt_tbl,
            by= c("j" = "id")) %>% 
    group_by(i) %>% 
    summarise(totWt = sum(wt))




