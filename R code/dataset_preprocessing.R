library(Hmisc)# for data statistics
library(dplyr)# to replace with na call na_if()
library(ggplot2)

leads_data = read.csv("D:/H/study/wisd/S2/Simulation/regression logistic mini projet/leads.csv")

head(leads_data)
sum(duplicated(leads_data)) == 0 # True => No duplicate values
dim(leads_data)
summary(leads_data)
describe(leads_data)

leads_data[leads_data == "Select"] = NA
leads_data[leads_data == ""] = NA

leads_data$Lead.Quality = ifelse( is.na( leads_data$Lead.Quality ),
                                   "Not Sure",
                                   leads_data$Lead.Quality)
leads_data$Asymmetrique.Activity.Index = ifelse( is.na(leads_data$Asymmetrique.Activity.Index),
                                  "Not Sure",
                                  leads_data$Asymmetrique.Activity.Index)
leads_data$Asymmetrique.Profile.Index = ifelse( is.na(leads_data$Asymmetrique.Profile.Index),
                                  "Not Sure",
                                  leads_data$Asymmetrique.Profile.Index)

sum_na = sapply(leads_data, function(x) sum(is.na(x)))
round(100*(sum_na/nrow(leads_data)), 2)

for( i in 0:ncol(leads_data)){
  sum_col_na = sum(is.na(leads_data[i]))
  na_percentage = round(100*(sum_col_na/nrow(leads_data)), 2)
  print(na_percentage)
  if( na_percentage > 70){
    leads_data = subset(leads_data, select = -c(i)) #select(leads_data, i)
  }
}

ggplot(leads_data, aes(x = Specialization)) +
  geom_bar() + 
  theme(axis.text.x = element_text(angle = 90))
ggplot(leads_data, aes(x=reorder(Specialization, Specialization, function(x)-length(x)))) +
  geom_bar(fill='red')  + 
  theme(axis.text.x = element_text(angle = 90))
ggplot(leads_data, aes(x=reorder(Lead.Quality, Lead.Quality, function(x)-length(x)))) +
  geom_bar(fill='red') + 
  theme(axis.text.x = element_text(angle = 90))


describe(leads_data$Lead.Quality)

ggplot(leads_data, aes(x=reorder(as_act_indx, as_act_indx, function(x)-length(x)))) +
  geom_bar(fill='red') +  labs(x='Asymmetrique.Activity.Index')

as_act_score = leads_data$Asymmetrique.Activity.Score 
ggplot(leads_data, aes(x=reorder(as_act_score, as_act_score, function(x)-length(x)))) +
  geom_bar(fill='red') +  labs(x='Asymmetrique.Activity.Score')
ggplot(leads_data, aes(x=as_act_score, y=as_act_score ) )+
  geom_boxplot(fill='steelblue') +  labs(x='Asymmetrique.Activity.Score') + coord_flip()

as_prf_indx = leads_data$Asymmetrique.Profile.Index 
ggplot(leads_data, aes(x=reorder(as_prf_indx, as_prf_indx, function(x)-length(x)))) +
  geom_bar(fill='red') +  labs(x='Asymmetrique.Activity.Score')

as_prf_score = leads_data$Asymmetrique.Profile.Score
ggplot(leads_data, aes(x=as_prf_score, y=as_prf_score ) )+
  geom_boxplot(fill='steelblue') +  labs(x='Asymmetrique.Activity.Score') + coord_flip()

leads_data = subset(leads_data, select = -c(Asymmetrique.Activity.Index, Asymmetrique.Profile.Index, Asymmetrique.Activity.Score, Asymmetrique.Profile.Score))


# city
describe(leads_data$City)
ggplot(leads_data, aes(x=reorder(City, City, function(x)-length(x)))) +
  geom_bar(fill='red') +  labs(x='City', y="") + 
  theme(axis.text.x = element_text(angle = 90))
leads_data$City = ifelse(is.na(leads_data$City),
                                   "Mumbai",
                                   leads_data$City)
leads_data$City = ifelse(leads_data$City == "Select",
                                   "Mumbai",
                                   leads_data$City)
#Specailization
describe(leads_data$Specialization)
ggplot(leads_data, aes(x=reorder(Specialization, Specialization, function(x)-length(x)))) +
  geom_bar(fill='red') +  labs(x='Specialization', y="") + 
  theme(axis.text.x = element_text(angle = 90))
leads_data$Specialization = ifelse(is.na(leads_data$Specialization),
                                   "Others",
                                   leads_data$Specialization)
# Tags
describe(leads_data$Tags)
ggplot(leads_data, aes(x=reorder(Tags, Tags, function(x)-length(x)))) +
  geom_bar(fill='red') +  labs(x='', y="") + 
  theme(axis.text.x = element_text(angle = 90))
leads_data$Tags = ifelse(is.na(leads_data$Tags),
                                   "Will back after reading the email",
                                   leads_data$Tags)
# What matters most to you in choosing a course
describe(leads_data$What.matters.most.to.you.in.choosing.a.course)
ggplot(leads_data, aes(x=reorder(What.matters.most.to.you.in.choosing.a.course, What.matters.most.to.you.in.choosing.a.course, function(x)-length(x)))) +
  geom_bar(fill='red') +  labs(x='', y="") + 
  theme(axis.text.x = element_text(angle = 90))
leads_data$What.matters.most.to.you.in.choosing.a.course = ifelse(is.na(leads_data$What.matters.most.to.you.in.choosing.a.course),
                                    "Better Career Prospects",
                                    leads_data$What.matters.most.to.you.in.choosing.a.course)
# Occupation
describe(leads_data$What.is.your.current.occupation)
ggplot(leads_data, aes(x=reorder(What.is.your.current.occupation, What.is.your.current.occupation, function(x)-length(x)))) +
  geom_bar(fill='red') +  labs(x='', y="") + 
  theme(axis.text.x = element_text(angle = 90))
leads_data$What.is.your.current.occupation = ifelse(is.na(leads_data$What.is.your.current.occupation),
                                   "Unemployed",
                                   leads_data$What.is.your.current.occupation)
# Country
leads_data$Country = ifelse(is.na(leads_data$Country),
                                   "India",
                                   leads_data$Country)

sum_na = sapply(leads_data, function(x) sum(is.na(x)))
round(100*(sum_na/nrow(leads_data)), 2)

# data preproccessing finished
write.csv( leads_data,"D:/H/study/wisd/S2/Simulation/regression logistic mini projet/leads_cleaned.csv", row.names = FALSE)


















unique(leads_data$Lead.Quality)
unique(leads_data$Specialization)
unique(leads_data$Tags)
unique(leads_data$Asymmetrique.Activity.Index)
unique(leads_data$Asymmetrique.Profile.Index)

# install.packages("Hmisc")
# install.packages("dplyr")
# install.packages('ggplot')

#ggplot(leads_data, aes(x=Specialization)) +
 # geom_bar()