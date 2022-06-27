library(Hmisc)# for data statistics
library(dplyr)# to replace with na call na_if()
library(ggplot2)
library(scales)

leads_data = read.csv("D:/H/study/wisd/S2/Simulation/regression logistic mini projet/leads_cleaned.csv")
dim(leads_data)

#Converted
convertedFreq = (sum(leads_data$Converted)/length(leads_data$Converted))*100

#Lead Origin
ggplot(leads_data) +
  geom_bar(aes(x = Lead.Origin, fill = as.factor(Converted)), position = "dodge") + 
  theme(axis.text.x = element_text(angle = 90))

ggplot(leads_data) +
  geom_bar(aes(x = Lead.Source, fill = as.factor(Converted)), position = "dodge") + 
  theme(axis.text.x = element_text(angle = 90))

leads_data$Lead.Source = ifelse( leads_data$Lead.Source == "google",
                                                "Google",
                                                leads_data$Lead.Source)
leads_data$Lead.Source = ifelse( is.na(leads_data$Lead.Source == "google"),
                                                "Others",
                                                leads_data$Lead.Source)
leads_data$Lead.Source = ifelse( leads_data$Lead.Source %in% c('Click2call', 'Live Chat', 'NC_EDM', 'Pay per Click Ads', 'Press_Release',
                                                            'Social Media', 'WeLearn', 'bing', 'blog', 'testone', 'welearnblog_Home', 'youtubechannel'),
                                                "Others",
                                                leads_data$Lead.Source)
ggplot(leads_data) +
  geom_bar(aes(x = Lead.Source, fill = as.factor(Converted)), position = "dodge") + 
  theme(axis.text.x = element_text(angle = 90))

#Do Not Email & Do Not Call
ggplot(leads_data) +
  geom_bar(aes(x = Do.Not.Call, fill = as.factor(Converted)), position = "dodge") + 
  theme(axis.text.x = element_text(angle = 90))
ggplot(leads_data) +
  geom_bar(aes(x = Do.Not.Email, fill = as.factor(Converted)), position = "dodge") + 
  theme(axis.text.x = element_text(angle = 90))

#Total Visits
describe(leads_data$TotalVisits)
ggplot(leads_data, aes(x=TotalVisits, y=TotalVisits ) )+
  geom_boxplot(fill='steelblue') +  labs(x='TotalVisits') + coord_flip()
ggplot(leads_data, aes(x=Converted, y=TotalVisits ) )+
  geom_boxplot(fill='steelblue') +  labs(x='TotalVisits') + coord_flip()

fun <- function(x){
  quantiles <- quantile( x, c(.05, .95 ), na.rm = T )
  x[ x <= quantiles[0] ] <- quantiles[0]
  x[ x >= quantiles[1] ] <- quantiles[1]
  x
}

leads_data$TotalVisits <- squish(leads_data$TotalVisits, quantile(leads_data$TotalVisits, c(.05, .95), na.rm = T))
ggplot(leads_data, aes(x=Converted, y=TotalVisits ) )+
  geom_boxplot(fill='steelblue') +  labs(x='TotalVisits') + coord_flip()


ggplot(leads_data, aes(x = Converted, y = TotalVisits, fill=factor(Converted))) +
  geom_boxplot() 

#Total time spent on website
describe(leads_data$Total.Time.Spent.on.Website)

ggplot(leads_data, aes(x = Total.Time.Spent.on.Website)) +
  geom_boxplot(fill='steelblue') 

ggplot(leads_data, aes(x = Converted, y = Total.Time.Spent.on.Website, fill=factor(Converted))) +
  geom_boxplot() 

#Page views per visit
describe(leads_data$Page.Views.Per.Visit)

ggplot(leads_data, aes(x = Page.Views.Per.Visit)) +
  geom_boxplot(fill='steelblue') 

ggplot(leads_data, aes(x = Converted, y = Page.Views.Per.Visit, fill=factor(Converted))) +
  geom_boxplot() 
leads_data$Page.Views.Per.Visit <- squish(leads_data$Page.Views.Per.Visit, quantile(leads_data$TotalVisits, c(.05, .95), na.rm = T))

ggplot(leads_data, aes(x = Converted, y = Page.Views.Per.Visit, fill=factor(Converted))) +
  geom_boxplot() 

#Last Activity
describer(leads_data$Last.Activity)

ggplot(leads_data) +
  geom_bar(aes(x = Last.Activity, fill = as.factor(Converted)), position = "dodge") + 
  theme(axis.text.x = element_text(angle = 90))
# Let's keep considerable last activities as such and club all others to "Other_Activity"
leads_data$Last.Activity = ifelse( leads_data$Last.Activity %in% c(NA,'Had a Phone Conversation', 'View in browser link Clicked', 
                                                               'Visited Booth in Tradeshow', 'Approached upfront',
                                                               'Resubscribed to emails','Email Received', 'Email Marked Spam'),
                                 "Other_Activity",
                                 leads_data$Last.Activity)
ggplot(leads_data) +
  geom_bar(aes(x = Last.Activity, fill = as.factor(Converted)), position = "dodge") + 
  theme(axis.text.x = element_text(angle = 90))
#Country
describe(leads_data$Country)
#Specialization
describe(leads_data$Specialization)
leads_data$Specialization = ifelse( leads_data$Specialization == "Others","Other_Specialization",
                                 leads_data$Specialization)
ggplot(leads_data) +
  geom_bar(aes(x = Specialization, fill = as.factor(Converted)), position = "dodge") + 
  theme(axis.text.x = element_text(angle = 90))

#Occupation
describe(leads_data$What.is.your.current.occupation)
leads_data$What.is.your.current.occupation = ifelse( leads_data$What.is.your.current.occupation == "Other","Other_Occupation",
                                 leads_data$What.is.your.current.occupation)
ggplot(leads_data) +
  geom_bar(aes(x = What.is.your.current.occupation, fill = as.factor(Converted)), position = "dodge") + 
  theme(axis.text.x = element_text(angle = 90))

#What matters most to you in choosing a course¶
describe(leads_data$What.matters.most.to.you.in.choosing.a.course)

######
describe(leads_data$Newspaper.Article)
describe(leads_data$Digital.Advertisement)
describe(leads_data$Through.Recommendations)
describe(leads_data$Receive.More.Updates.About.Our.Courses)

#Tags
describe(leads_data$Tags)
ggplot(leads_data) +
  geom_bar(aes(x = Tags, fill = as.factor(Converted)), position = "dodge") + 
  theme(axis.text.x = element_text(angle = 90))
leads_data$Tags = ifelse( leads_data$Tags %in% c(NA,'In confusion whether part time or DLP', 'in touch with EINS','Diploma holder (Not Eligible)',
                                                 'Approached upfront','Graduation in progress','number not provided', 'opp hangup','Still Thinking',
                                                 'Lost to Others','Shall take in the next coming month','Lateral student','Interested in Next batch',
                                                 'Recognition issue (DEC approval)','Want to take admission but has financial problems',
                                                 'University not recognized'),
                                                  "Other_Tags",
                                                  leads_data$Tags)
ggplot(leads_data) +
  geom_bar(aes(x = Tags, fill = as.factor(Converted)), position = "dodge") + 
  theme(axis.text.x = element_text(angle = 90))

#Lead Quality
describe(leads_data$Lead.Quality)
ggplot(leads_data) +
  geom_bar(aes(x = Lead.Quality, fill = as.factor(Converted)), position = "dodge") + 
  theme(axis.text.x = element_text(angle = 90))

#######
describe(leads_data$Update.me.on.Supply.Chain.Content)
describe(leads_data$Get.updates.on.DM.Content)
describe(leads_data$I.agree.to.pay.the.amount.through.cheque)
describe(leads_data$A.free.copy.of.Mastering.The.Interview)

#city
describe(leads_data$City)
ggplot(leads_data) +
  geom_bar(aes(x = City, fill = as.factor(Converted)), position = "dodge") + 
  theme(axis.text.x = element_text(angle = 90))

#Last Notable Activity
describe(leads_data$Last.Notable.Activity)
ggplot(leads_data) +
  geom_bar(aes(x = Last.Notable.Activity, fill = as.factor(Converted)), position = "dodge") + 
  theme(axis.text.x = element_text(angle = 90))

leads_data = subset(leads_data, select = -c(Lead.Number, What.matters.most.to.you.in.choosing.a.course,
                                            Search, Magazine, Newspaper.Article, WISD.Forums,Newspaper,
                                            Digital.Advertisement, Through.Recommendations, 
                                            Receive.More.Updates.About.Our.Courses, 
                                            Update.me.on.Supply.Chain.Content,
                                            Get.updates.on.DM.Content, I.agree.to.pay.the.amount.through.cheque,
                                            A.free.copy.of.Mastering.The.Interview, Country))


write.csv( leads_data,"D:/H/study/wisd/S2/Simulation/regression logistic mini projet/leads_cleaned.csv", row.names = FALSE)



unique(leads_data$TotalVisits)

