#Example

n <- 40
m <- 4
capacity <- rep.int(11, m) # all have equal capacities


set.seed(1234)
preference_data <- lapply(seq_len(n), function(x) sample(seq_len(m), 3))
preferences <- function(student) preference_data[[student]]

# the weight of a student choosing a course
# if the course is not among the preferences, the weight is -100000
weight <- function(student, course) {
  p <- which(as.numeric(course) == preferences(as.numeric(student)))
  as.integer(if (length(p) == 0) {
    -100000
  } else {
    p
  })
}

weight(1, 3)
weight(1, 23) # this was not a choice by student 1, so we give it a big penalty

#----------------
library(ggplot2)
library(purrr)
library(dplyr)
plot_data <- expand.grid(
  course = seq_len(m),
  weight = 1:3
) %>% rowwise() %>% 
  mutate(count = sum(map_int(seq_len(n), ~weight(.x, course) == weight))) %>% 
  mutate(course = factor(course), weight = factor(weight))
ggplot(plot_data, aes(x = course, y = count, fill = weight)) + 
  geom_bar(stat = "identity") + 
  viridis::scale_fill_viridis(discrete = TRUE) + 
  geom_hline(yintercept = 11)


#--------------

# install.packages("ompr")
library(ompr)
model <- MIPModel() %>%
  
  # 1 iff student i is assigned to course m
  add_variable(x[i, j], i = 1:n, j = 1:m, type = "binary") %>%
  
  # maximize the preferences
  set_objective(sum_over(weight(i, j) * x[i, j], i = 1:n, j = 1:m)) %>%
  
  # we cannot exceed the capacity of a course
  add_constraint(sum_over(x[i, j], i = 1:n) <= capacity[j], j = 1:m) %>% 
  
  # each student needs to be assigned to one course
  add_constraint(sum_over(x[i, j], j = 1:m) == 1, i = 1:n)
model


# install.packages(c("ompr.roi", "ROI.plugin.glpk"))
library(ompr.roi)
library(ROI.plugin.glpk)
result <- solve_model(model, with_ROI(solver = "glpk", verbose = TRUE))


objective_value(result)


matching <- result %>% 
  get_solution(x[i,j]) %>%
  filter(value > .9) %>%  
  select(i, j) %>% 
  rowwise() %>% 
  mutate(weight = weight(as.numeric(i), as.numeric(j)), 
         preferences = paste0(preferences(as.numeric(i)), collapse = ",")) %>% ungroup


matching %>% 
  group_by(weight) %>% 
  summarise(count = n())


plot_data <- matching %>% 
  mutate(course = factor(j), weight = factor(weight, levels = c(1, 2, 3))) %>% 
  group_by(course, weight) %>% 
  summarise(count = n()) %>% 
  tidyr::complete(weight, fill = list(count = 0))

ggplot(plot_data, aes(x = course, y = count, fill = weight)) + 
  geom_bar(stat = "identity") + 
  viridis::scale_fill_viridis(discrete = TRUE) + 
  geom_hline(yintercept = 11)
