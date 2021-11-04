# Lets work through the regression example:


library(magrittr)
library(readr)
library(tidyverse)
library(caret)
library(mlbench)
library(skimr)
library(ggcorrplot)
library(psych)
library(tidyverse)
library(factoextra)

 df_raw <- read_csv("Downloads/geohackathon_bootcamps-main/regression_example/example_WT2021.csv")


 skim(df_raw)

# Drop the rows with NA
 df_raw %<>% drop_na()


 # lets plot the flowrate and pumpspeed

 ggplot(df_raw, aes(x=`Time [min]`)) +
   geom_point(aes(y = `Flow rate [m3/d]`),color="red",alpha=1/10) +
   geom_point(aes(y = `Pump speed [rpm]`),color="blue",alpha=1/10) +
   geom_point(aes(y = `WHP [bar]`*1000),color="green",alpha=1/10) +
   # Custom the Y scales:
   scale_y_continuous(
     # Features of the first axis
     name = "Axis",
     # Add a second axis and specify its features
     sec.axis = sec_axis( ~./1000, name=" WHP Bar",)) +
   theme_minimal()



 pairs.panels(df_raw, jiggle = T,
              method = "pearson", # correlation method
              hist.col = "#00AFBB",
              density = T,  # show density plots
              ellipses = TRUE # show correlation ellipses
 )



 corr <- round(cor(df_raw), 1)



 ggcorrplot(corr, method = "circle")

 ggcorrplot(corr)


 # features = [ 'Pump speed [rpm]', 'WHP [bar]','WHT [C]' ] , predict "Flow rate [m3/d]"

df_model <- df_raw %>% select(`Time [min]`,`Pump speed [rpm]`, `WHP [bar]`,`WHT [C]`,`Flow rate [m3/d]` )

inTraining <- createDataPartition(df_model$`Flow rate [m3/d]`, p = .75, list = FALSE)


?createDataPartition
# data(Sonar)
# str(Sonar[, 1:10])
# skim(Sonar$Class)



 training <- df_model[inTraining, ]
 test <- df_model[-inTraining, ]


 fitControl <- trainControl(## 10-fold CV
   method = "repeatedcv",
   number = 10,
   ## repeated ten times
   repeats = 10)


 gbmFit1 <- train(`Flow rate [m3/d]` ~ `Pump speed [rpm]`+`WHP [bar]`+`WHT [C]` , data = training,
                  method = "gbm",
                  trControl = fitControl,
                  ## This last option is actually one
                  ## for gbm() that passes through
                  verbose = FALSE)
 gbmFit1

 trellis.par.set(caretTheme())

 plot(gbmFit1)


 gbmGrid <-  expand.grid(interaction.depth = c(1, 5, 9),
                         n.trees = (1:30)*50,
                         shrinkage = 0.1,
                         n.minobsinnode = 20)

 nrow(gbmGrid)

 set.seed(825)
 gbmFit2 <- train(`Flow rate [m3/d]` ~ `Pump speed [rpm]`+`WHP [bar]`+`WHT [C]`, data = training,
                  method = "gbm",
                  trControl = fitControl,
                  verbose = FALSE,
                  ## Now specify the exact models
                  ## to evaluate:
                  tuneGrid = gbmGrid)
 gbmFit2

 # lm

 trellis.par.set(caretTheme())
 plot(gbmFit2)


 predict(gbmFit2, newdata = head(test))

 densityplot(gbmFit2, pch = "|")
 xyplot(resamps, what = "BlandAltman")



 ###################
 set.seed(7279)



 lm_fit <- train(`Flow rate [m3/d]` ~ `Pump speed [rpm]`+`WHP [bar]`+`WHT [C]` , data = training, method = "brnn")

 test$pred <- bh_pred <- predict(lm_fit, test)

 training$pred <- be_pred <- predict(lm_fit, training)

 lm_fit

lm_train <- train(`Flow rate [m3/d]` ~ `Pump speed [rpm]`+`WHP [bar]`+`WHT [C]`, data = training, method = "lm")

postResample(pred = bh_pred, obs = test$`Flow rate [m3/d]`)





g1 <-ggplot(data = df_raw, aes(x=`Time [min]` ,y=`Flow rate [m3/d]`))+
  geom_point( aes(y = `Flow rate [m3/d]`), colour = "grey")

g2 <- ggplot(data = training, aes(x=`Time [min]` ))+
  geom_point( aes(y = `Flow rate [m3/d]`), colour = "blue")+
  geom_point( aes(y = pred), colour = "yellow")

g3 <- ggplot(data = test, aes(x=`Time [min]` ))+
  geom_point( aes(y = `Flow rate [m3/d]`), colour = "blue")+
  geom_point( aes(y = pred), colour = "yellow")

g3

