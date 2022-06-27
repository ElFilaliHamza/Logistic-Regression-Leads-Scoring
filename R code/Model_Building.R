library(Hmisc)# for data statistics
library(dplyr)# to replace with na call na_if()
library(ggplot2)
library(scales) # to use squish which help us in removing outliers
library(caret) # used for one hot encoding
library(InformationValue)

leads_data = read.csv("D:/H/study/wisd/S2/Simulation/regression logistic mini projet/leads_cleaned.csv")
leads_data = na.omit(leads_data)

leads_data$Do.Not.Call <- ifelse(leads_data$Do.Not.Call=="Yes", 1, 0)
leads_data$Do.Not.Email <- ifelse(leads_data$Do.Not.Email=="Yes", 1, 0)

dummy <- dummyVars(~ Lead.Origin+ Lead.Source + Last.Activity + Specialization+
                What.is.your.current.occupation+ Tags+ Lead.Quality+City+
                Last.Notable.Activity, data=leads_data)

final_df <- data.frame(predict(dummy, newdata=leads_data))
final_df = subset(final_df, select = -c(1))
leads_data <- cbind(leads_data, final_df)
leads_data = subset(leads_data, select = -c(Lead.Origin, Lead.Source, Last.Activity, Specialization,
                                                   What.is.your.current.occupation, Tags,Lead.Quality,City,
                                                   Last.Notable.Activity))
X = subset(leads_data, select = -c(Prospect.ID))
X = X[!duplicated(X),]
head(X)
X$TotalVisits = c(scale(X$TotalVisits, center = TRUE, scale = TRUE))
X$Total.Time.Spent.on.Website = c(scale(X$Total.Time.Spent.on.Website, center = TRUE, scale = TRUE))
X$Page.Views.Per.Visit = c(scale(X$Page.Views.Per.Visit, center = TRUE, scale = TRUE))


sample <- createDataPartition(X$Converted, p = .7, list = F)
leads_data_train = X[sample, ]
leads_data_test = X[-sample, ]





converted = (sum(leads_data$Converted/ length(leads_data$Converted)))*100

#model building
logistic_reg <- glm(Converted~., family = "binomial", data = leads_data_train)

summary(logistic_reg)


predict <- predict(logistic_reg, leads_data_test, type = 'response')

table_mat <- table(leads_data_test$Converted, predict > 0.5)

accuracy_Test1 <- sum(diag(table_mat))/ sum(table_mat)

anova(logistic_reg)
#there is no such R2 value for logistic regression. Instead, we can compute a metric known as McFadden's R2, which ranges from 0 to just under 1. Values close to 0 indicate that the model has no predictive power. In practice, values over 0.40 indicate that a model fits the data very well.
#We can compute McFadden's R2 for our model using the pR2 
#function from the pscl package:
pscl::pR2(logistic_reg)["McFadden"]

# leads_data_train[1:4, c(3,1)]
x_data_train = leads_data_train[, -3]
y_data_train = leads_data_train[, c("Converted") ]

sum_na = sapply(x_data_train, function(x) sum(is.na(x)))
round(100*(sum_na/nrow(x_data_train)), 2)
#####
# This part of code take a lot of time to be executed so thats why its result is showing down as comment  
#####
# control_rfe = rfeControl(functions = rfFuncs, # random forest
#                          method = "repeatedcv", # repeated cv
#                          repeats = 5, # number of repeats
#                          number = 10) # number of folds
# 
# Run RFE just while exploiting data
# result_rfe1 <- rfe(x = x_data_train,
#                    y = y_data_train,
#                    sizes = c(1:15),
#                    rfeControl = control_rfe)
# 
# Print the selected features
# predictors(result_rfe1)

set.seed(47)

logistic_reg2 <- glm(Converted~Do.Not.Email+Lead.OriginLead.Add.Form+
                       Lead.SourceWelingak.Website+
                       What.is.your.current.occupationWorking.Professional+TagsBusy+
                       TagsClosed.by.Horizzon+TagsLost.to.EINS+TagsRinging+
                       TagsWill.revert.after.reading.the.email+Tagsswitched.off+
                       Lead.QualityNot.Sure+Lead.QualityWorst+Last.Notable.ActivitySMS.Sent,
                     family = "binomial", data = leads_data_train)

summary(logistic_reg2)

pscl::pR2(logistic_reg2)["McFadden"]

caret::varImp(logistic_reg2)

car::vif(logistic_reg2)

#We can use the following code to calculate the probability of default for every individual in our test dataset:
#calculate probability of default for each individual in test dataset
predicted2 <- predict(logistic_reg2, leads_data_test, type="response")

#find optimal cutoff probability to use to maximize accuracy
optimal <- optimalCutoff(leads_data_test$Converted, predicted2)[1]

table_mat2 <- table(leads_data_test$Converted, predict > 0.5)

accuracy_Test2 <- sum(diag(table_mat2))/ sum(table_mat)




myPrediction <- predict(logistic_reg2, leads_data_test[1,], type="response")*100

myLead = leads_data_test[1,]
print("The score of the ")


score_lead = function(my_lead, my_model, israw=F){
  if( !israw ){
    my_lead$TotalVisits = c(scale(my_lead$TotalVisits, center = TRUE, scale = TRUE))
    my_lead$Total.Time.Spent.on.Website = c(scale(my_lead$Total.Time.Spent.on.Website, center = TRUE, scale = TRUE))
    my_lead$Page.Views.Per.Visit = c(scale(my_lead$Page.Views.Per.Visit, center = TRUE, scale = TRUE))
  }
    myPrediction <- predict(my_model, my_lead, type="response")*100
    myPrediction
}

i = 2
lead_score = score_lead(leads_data_test[i,], logistic_reg2, TRUE)
lead_id = leads_data[i,]$Prospect.ID
conv = leads_data_test[i,]$Converted
sprintf('The lead with id %s and with convertion equal to %s  is scored by the model as %.2f percent', lead_id, conv,lead_score[1])

sprintf('the score of the lead with convertion equal to %s  is equal to %s', conv, lead_score[1])


