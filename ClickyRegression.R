library(assertr)
library(dplyr)
library(ROCR)
library(ggplot2)


data <- read.csv("~/Dropbox/uwe/training_sample.csv")

# Initial EDA, includes assertions
data %>%  
 verify(has_all_names("ordered", "device_mobile", "device_computer", "device_tablet", "returning_user")) %>%
 verify(sum(ordered, nr.rm=TRUE) > 1000) %>%
 group_by(ordered, loc_uk) %>%
 tally() %>%
 spread(loc_uk, n)

# fit model
Model <- data %>%
    verify(has_all_names("ordered", "device_mobile", "device_computer", "device_tablet", "returning_user")) %>%
   verify(sum(ordered, nr.rm=TRUE) > 1000) %>%
    glm(ordered ~ loc_uk + returning_user + device_mobile, family = binomial(), data = .)


# quick hack method for creating confusion matrix
training_predictions <- predict(Model, type="response")
CutOff <- runif(length(data$ordered))
confusionMatrix <- xtabs(~as.numeric(CutOff < training_predictions) + data$ordered)

# better method for classifier, using ROCR package
pred <- prediction(training_predictions, data$ordered)
perf <- performance(pred, measure="tpr", x.measure="fpr")
plot(perf, main="ROC curve")

# find optimal cutoff
## first visual check
sens <- data.frame(x=unlist(performance(pred, "sens")@x.values), 
                   y=unlist(performance(pred, "sens")@y.values))
spec <- data.frame(x=unlist(performance(pred, "spec")@x.values), 
                   y=unlist(performance(pred, "spec")@y.values))
sens %>% ggplot(aes(x,y)) + 
  geom_line() + 
  geom_line(data=spec, aes(x,y,col="red")) +
  scale_y_continuous(sec.axis = sec_axis(~., name = "Specificity")) +
  labs(x='Cutoff', y="Sensitivity") +
    theme(axis.title.y.right = element_text(colour = "red"), legend.position="none")

#sens = cbind(unlist(performance(pred, "sens")@x.values), unlist(performance(pred, "sens")@y.values))
#spec = cbind(unlist(performance(pred, "spec")@x.values), unlist(performance(pred, "spec")@y.values))

optimalCutoff <- sens[which.min(apply(sens, 1, function(x) min(colSums(abs(t(spec) - x))))), 1]

# now make predictions
purchase_predictions <- as.numeric(training_predictions < optimalCutoff)
# final confusion matrix
xtabs(~purchase_predictions + data$ordered)
