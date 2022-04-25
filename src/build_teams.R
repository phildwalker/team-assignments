# Looking to build off this example: https://dirkschumacher.github.io/ompr/articles/problem-course-assignment.html
# Thu Apr 21 14:26:43 2022 ------------------------------

library(tidyverse)

columns <- c("Name", "Gender", "WeeksMiss", "Excitement", "SelfSkill", "CorrectedSkill", "DaysMiss")

clean <- 
  readxl::read_excel(here::here("data", "signups.xlsx"))  %>% 
  rename_with(~ columns, all_of(colnames(.))) %>% 
  mutate(Score = CorrectedSkill + (10-DaysMiss)*.5,
         ID = row_number())

NameScore <-
  clean %>% 
  select(ID, Name, Gender, Score) %>% 
  mutate(Score= floor(Score))
  

Men <- 
  NameScore %>% 
  filter(Gender == "Male") %>% 
  mutate(OptID = row_number())

Women <-
  NameScore %>% 
  filter(Gender == "Female") %>% 
  mutate(OptID = row_number())




#---------
# Visualize score distribution----
NameScore %>% 
  ggplot(aes(Score, fill = Gender))+
  geom_bar(stat = "count", position = "dodge")


mean(Men$Score)
mean(Women$Score)

#----------------

NumTeams <- 5
NumPlayers <- count(Women) %>% pull() %>% as.numeric()

MinTeamSize <- floor(NumPlayers/NumTeams)
MaxTeamSize <- floor(NumPlayers/NumTeams)+1




#---------------------

library(ompr)
library(ompr.roi)
library(ROI.plugin.glpk)




max_bins <- 10
bin_size <- 3
n <- 10

weights <- runif(n, max = bin_size)

wt_tbl <- weights %>% as.data.frame() %>% 
  mutate(id = row_number()) %>% 
  rename(wt = 1)

model <- MIPModel() %>%
  add_variable(y[i], i = 1:max_bins, type = "binary") %>%
  add_variable(x[i, j], i = 1:max_bins, j = 1:n, type = "binary") %>%
  set_objective(sum_over(y[i], i = 1:max_bins), "min") %>%
  add_constraint(sum_over(weights[j] * x[i, j], j = 1:n) <= y[i] * bin_size, i = 1:max_bins) %>%
  add_constraint(sum_over(x[i, j], i = 1:max_bins) == 1, j = 1:n) 


result <-
  model %>%
  solve_model(with_ROI(solver = "glpk", verbose = TRUE)) 

outpt <- 
  result %>%
  get_solution(x[i, j]) %>%
  filter(value > 0) %>%
  arrange(i)

# comp <-
outpt %>% 
  left_join(., wt_tbl,
            by= c("j" = "id")) %>% 
  group_by(i) %>% 
  summarise(totWt = sum(wt),
            count = n())














