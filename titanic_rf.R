library(dplyr)
library(magrittr)
library(randomForest)


set.seed(1)

train.loc <- "~/Documents/Nerd Stuff/train.csv"
test.loc <- "~/Documents/Nerd Stuff/test.csv"

train <- read.csv(train.loc, stringsAsFactors = T)
test <- read.csv(test.loc, stringsAsFactors = T)

rf.train <- train %>% 
  group_by(Survived) %>% 
  select(-Name, -Ticket, -Cabin) %>% 
  sample_frac(.7) %>% 
  ungroup() %>% 
  rfImpute(Survived ~ ., .)

rf.test <- train[!(train$PassengerId %in% rf.train$PassengerId),] %>% 
  rfImpute(Survived ~ . , .)

fit <- randomForest(factor(Survived) ~ Age + Sex + Pclass + Fare + SibSp, 
                    cutoff = c(.75, .25),  
                    data = rf.train)

validate <- rf.test %>% 
  mutate(survive.validate = predict(fit, rf.test))

table(validate$Survived, validate$survive.validate)