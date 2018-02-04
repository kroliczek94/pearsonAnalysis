library(ggplot2)
library(dplyr)
library(tidyr)
library(ggthemes)

setwd("D:/pearson")
#tabAll <- read.csv("data2018.csv", sep = ";", classes = c(integer, factor, factor, factor, numeric, numeric, numeric))
initial <- read.csv("data2018.csv",sep=";", nrows = 1000)
classes <- sapply(initial, class)
classes["inv_rate"] = "numeric"
tabAll <- read.csv("data2018.csv",sep=";",
                   colClasses = classes)

colnames(tabAll)[1] <- "learner_id"

#missing values
sapply(tabAll, function(x) sum(is.na(x)))
tabAll[rowSums(is.na(tabAll)) > 0,]

tabCleaned <- tabAll %>% drop_na() %>% filter(unit != "") %>% filter(country != "") %>% filter(avg_score <= 1)
  #learner to country
countrySummary <- tabCleaned %>% group_by(country, in_course) %>% summarise(avg_completion = mean(completion),avg_score = mean(avg_score), inv_rate= mean(inv_rate), nlearners = length(unique(learner_id))) %>% arrange(-nlearners) %>% filter(nlearners > 30)
p <- ggplot(data = countrySummary, aes(x= country, y = nlearners))
p + geom_bar(stat="identity")

unitSummary <- tabCleaned %>% group_by(unit, in_course) %>% summarise(avg_scores = mean(avg_score), n = n(), avg_compl = mean(completion))
p <- ggplot(data = unitSummary, aes(x= unit, y = n, fill = avg_compl))
p + geom_bar(stat= "identity")  + geom_label(label =  "2")

bottom1percentSummary <- tabCleaned %>% group_by(learner_id, country, in_course) %>% summarise(avg_scores = mean(avg_score), n = n(), inv_rate = mean(inv_rate)) %>% arrange(avg_scores)
p <- ggplot(data = bottom1percentSummary, aes(x = learner_id, y = avg_scores, color = in_course))
p + geom_point()

ggplot(unitSummary, aes(x = unit, y = avg_compl, color = in_course, size= completion)) +   # Fill column
  geom_point(stat = "identity", width = .6) +   # draw the bars
 
  coord_flip() +  # Flip axes
  labs(title="Email Campaign Funnel") +
  theme_tufte() +  # Tufte theme from ggfortify
  theme(plot.title = element_text(hjust = .5), 
        axis.ticks = element_blank()) +   # Centre plot title
  scale_fill_brewer(palette = "Dark2")  # Color palette
