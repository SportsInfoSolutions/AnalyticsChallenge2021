library(tidyverse)
library(rsample)
library(ranger)
source("Scripts/wrangle.R")


# TODO: predict ThrowDepth for completed passes
  
skimr::skim(plays_data)


##data split
train_prop <- 0.7

#by week
week_cut <- ceiling(16*train_prop)
d_wk <- list(
  train = plays_data %>% filter(Week > week_cut),
  test = plays_data %>% filter(Week <= week_cut)
)

#stratified sampling
split_strata <- function(...) {
  require(rsample)
  df_split <- initial_split(...)
  list(
    train = training(df_split),
    test = testing(df_split)
  )
}

d_success <- split_strata(data = plays_data, prop = train_prop, strata = "off_success")
d_success_epa <- split_strata(data = plays_data, prop = train_prop, strata = "off_success_epa")

##target distribution

plot_dist <- function(df, target) ggplot(df, aes(x = !!as.name(target))) + geom_bar() + ggtitle(target)

plot_dist(d_wk$train, "off_success")
plot_dist(d_wk$train, "off_success_epa")
plot_dist(d_success$train, "off_success")
plot_dist(d_success_epa$train, "off_success_epa")


##grid search
n_predictors <- ncol(d_wk$train)-3

hyperparams <- expand.grid(
  mtry = floor(n_predictors * c(.05, .15, .25, .333, .4)),
  min.node.size = c(1, 3, 5, 10), 
  replace = c(TRUE, FALSE),                               
  sample.fraction = c(.5, .63, .8)
)

##model
m_wk_success <- ranger(off_success ~ .-GameID-EventID-off_success_epa, data = d_wk$train, classification = TRUE)
m_wk_success_epa <- ranger(off_success_epa ~ .-GameID-EventID-off_success, data = d_wk$train, classification = TRUE)

models <- vector("list", nrow(hyperparams))
ntrees <- 1000
rseed <- 2021

for(i in 1:nrow(hyperparams)) {
  m_success <- ranger(
    off_success ~ .-GameID-EventID-off_success_epa, 
    data = d_wk$train, 
    classification = TRUE,
    num.trees = ntrees,
    mtry = hyperparams$mtry[i],
    min.node.size = hyperparams$min.node.size[i],
    replace = hyperparams$replace[i],
    sample.fraction = hyperparams$sample.fraction[i],
    seed = rseed
  )
  
  m_success_epa <- ranger(
    off_success_epa ~ .-GameID-EventID-off_success, 
    data = d_wk$train, 
    classification = TRUE,
    num.trees = ntrees,
    mtry = hyperparams$mtry[i],
    min.node.size = hyperparams$min.node.size[i],
    replace = hyperparams$replace[i],
    sample.fraction = hyperparams$sample.fraction[i],
    seed = rseed
  )
  
  models[[i]][["success"]] <- m_success
  models[[i]][["success_epa"]] <- m_success_epa
  print(i)
}

hyperparams$oob_error_success <- models %>% map("success") %>% map_dbl("prediction.error")
hyperparams$oob_error_success_epa <- models %>% map("success_epa") %>% map_dbl("prediction.error")

hyperparams %>% arrange(oob_error_success_epa)

min_error_ind <- which(hyperparams$oob_error_success_epa == min(hyperparams$oob_error_success_epa))
success_model <- models %>% map("success_epa") %>% chuck(min_error_ind)

test_preds <- predict(success_model, d_wk$test)
test_confmat <- table(d_wk$test$off_success, test_preds$predictions)
test_accuracy <- (test_confmat[1,1] + test_confmat[2,2]) / sum(test_confmat)

