library(tidyverse)
setwd("/Users/tommie/Desktop/DataFest/For Students-Updated2022-03-28/data files")
logs = read_csv("logs.csv", guess_max = 2106600)

action = logs[logs$event_category== "Challenge Stack",]

player1 = action[action$player_id== 6486029,]

player2 = logs[logs$player_id== 6607011,]

player3 =  action[action$player_id== 6427031,]

plot(player1$row_id, player1$new_skill_point, NA = T)


s5 = read.csv('S5_scores_cleaned.csv')

happy = logs[logs$event_id== 707,]

player1 = happy[happy$player_id== 6486029,]

{
  library(ggplot2)
  
  schools = logs
  
  schools = schools[!is.na(schools$stars_for_this_level_new),]
  mean(schools$stars_for_this_level_new)
  
  school1 = subset(schools, school == "0647")
  #select = -c(SecondDoseDaily, SecondDoseCumulative))
  school2 = subset(schools, school == "1531")
  #select = -c(SecondDoseDaily, SecondDoseCumulative))
  school3 = subset(schools, school == "1827")
  # select = -c(SecondDoseDaily, SecondDoseCumulative))
  school4 = subset(schools, school == "2238")
  #select = -c(SecondDoseDaily, SecondDoseCumulative))
  school5 = subset(schools, school == "2570")
  #select = -c(SecondDoseDaily, SecondDoseCumulative))
  school6 = subset(schools, school == "3205")
  # select = -c(SecondDoseDaily, SecondDoseCumulative))
  school7 = subset(schools, school == "3561")
  #select = -c(SecondDoseDaily, SecondDoseCumulative))
  school8 = subset(schools, school == "4829")
  #select = -c(SecondDoseDaily, SecondDoseCumulative))
  school9 = subset(schools, school == "5340")
  # select = -c(SecondDoseDaily, SecondDoseCumulative))
  school10 = subset(schools, school == "6266")
  #select = -c(SecondDoseDaily, SecondDoseCumulative))
  school11= subset(schools, school == "7167")
  #select = -c(SecondDoseDaily, SecondDoseCumulative))
  school12 = subset(schools, school == "9691")
  # select = -c(SecondDoseDaily, SecondDoseCumulative))
  
  #school1 = na.omit(school1$stars_for_this_level_new)
  #school2 = na.omit(school2$stars_for_this_level_new)
  #school3 = na.omit(school3$stars_for_this_level_new)
  #school4 = na.omit(school4$stars_for_this_level_new)
  #school5 = na.omit(school5$stars_for_this_level_new)
  #school6 = na.omit(school6$stars_for_this_level_new)
  #school7 = na.omit(school7$stars_for_this_level_new)
  #school8 = na.omit(school8$stars_for_this_level_new)
  #school9 = na.omit(school9$stars_for_this_level_new)
  #school10 = na.omit(school10$stars_for_this_level_new)
  #school11 = na.omit(school11$stars_for_this_level_new)
  #school12 = na.omit(school12$stars_for_this_level_new)
  
  avgstars1 <- mean(school1$stars_for_this_level_new)
  avgstars2 <- mean(school2$stars_for_this_level_new)
  avgstars3 <- mean(school3$stars_for_this_level_new)
  avgstars4 <- mean(school4$stars_for_this_level_new)
  avgstars5 <- mean(school5$stars_for_this_level_new)
  avgstars6 <- mean(school6$stars_for_this_level_new)
  avgstars7 <- mean(school7$stars_for_this_level_new)
  avgstars8 <- mean(school8$stars_for_this_level_new)
  avgstars9 <- mean(school9$stars_for_this_level_new)
  avgstars10 <- mean(school10$stars_for_this_level_new)
  avgstars11 <- mean(school11$stars_for_this_level_new)
  avgstars12 <- mean(school12$stars_for_this_level_new)
  
  avgstarsall = rbind(avgstars1, avgstars2, avgstars3, avgstars4, avgstars5, avgstars6, avgstars7 ,avgstars8, avgstars9, avgstars10, avgstars11, avgstars12)
  
  #schoolsandstar = data.frame(logs$school, )
  
  
  ggplot(data = logs,  mapping = aes(x = school, y = avgstars, + 
                                       geom_point(color = "purple", alpha = .05, size = 3) + 
                                       geom_bar(stat="identity") +
                                       labs(y= "Stars Earned", x = "School",Title="Completion of Elm City Stories Based on School") ))
}

minigame = logs[!is.na(logs$minigame_id),]

completed =  logs[logs$proportion_complete == 1,]
completed1 = completed[!is.na(completed$proportion_complete),]

playersfactor = as.factor(completed1$player_id)
table(playersfactor)
plot(completed1$player_id)

player1 = player1[!is.na(player1$proportion_complete),]

player2 = player2[!is.na(player2$proportion_complete),]

timeforplayer2 = data.frame(player2$session, player2$date,player2$event_time_dbl, player2$proportion_complete)

timeforplayer1 = data.frame(player1$session, player1$stack_id, player1$date,player1$event_time_dbl, player1$proportion_complete)

mean(action$event_time_dbl)

players = logs
players = players[players$player_id> 6000000,]
players$player_id = as.factor(players$player_id)
table(players$player_id)

is.factor(players$player_id)

hist(table(players$player_id), freq= T)



newlogs = logs
newlogs = newlogs[!is.na(newlogs$proportion_complete),]

numperc = as.numeric(newlogs$proportion_complete)
numtime = as.numeric(newlogs$event_time_dbl)

plot(numtime, numperc)

player1= newlogs[newlogs$player_id== 6486029,]

plot((player1$event_time_dbl), player1$proportion_complete,type='o',col='green',pch='o', xlab='Seconds Spent in Game',ylim=(0:1),xlim = c(0,200000),ylab='Percentage of Game Complete',main='Completion Percentage vs Amount of Time Played in Seconds')
points((player2$event_time_dbl), player2$proportion_complete,col='red',pch='*')
lines((player2$event_time_dbl), player2$proportion_complete, col='red',type='b')
points((player3$event_time_dbl), player3$proportion_complete,col='blue',pch='+')
lines((player3$event_time_dbl), player3$proportion_complete,col='blue',type='b')
legend(180000,1,legend=c('Player 1','Player 2','Player 3'), col=c('green','red','blue'),pch=c('o','*','+'))

player2 = newlogs[newlogs$player_id== 6607011,]

player3 =  newlogs[newlogs$player_id== 6427031,]

playerdf = data.frame(rbind(player1,player2, player3))


timeforall = data.frame(playerdf$player_id, playerdf$stack_id, playerdf$date,playerdf$event_time_dbl, playerdf$proportion_complete)

ggplot(data=timeforall, aes(x=timeforall$event_time_dbl, y=timeforall$proportion_complete)) + geom_line() + geom_point(size=4)
library(ggplot2)

plot(player1$proportion_complete,player1$event_time_dbl)

plot(timeforall$playerdf.proportion_complete,timeforall$playerdf.event_time_dbl)

plot(timeforall$playerdf.proportion_complete,timeforall$playerdf.event_time_dbl)


completedsome = logs[!is.na(logs$proportion_complete),]
cor(completedsome$event_time_dbl, completedsome$proportion_complete)



library(tidyverse)
logs = read_csv("logs.csv", guess_max = 2106600)

View(logs)


action = logs[logs$event_category== "Challenge Stack",]

player648 = read.csv("player-6486029.csv",stringsAsFactors = TRUE)
player660 = read.csv("player-6607011.csv",stringsAsFactors = TRUE)
player642 = read_csv("player-6427031.csv",stringsAsFactors = TRUE)

View(player648)

str(player648$wave)

player1 = action[action$player_id== 6486029,]
player2 = action[action$player_id== 6607011,]
player3 = action[action$player_id== 6427031,]



# subset to only look at the first
logs1 = subset(logs, stack_id == 1)


str(logs1$event_time_dbl)
str(logs1$school)


logs1 = as.data.frame(unclass(logs1),stringsAsFactors=TRUE)
str(logs1$school)



completed = logs[logs$proportion_complete == 1,]
not_completed = logs[-logs$proportion_complete == 1,]

completed1 = completed[!is.na(completed$proportion_complete),]
not_completed1 = not_completed[!is.na(not_completed$proportion_complete),]







### cannon
logs$priority_type = as.factor(logs$priority_type)
priority = logs[-which(is.na(logs$priority_type)),]
str(priority$priority_type)

str(priority$priority_type)


# GENDER
gender <- logs %>%
  group_by(player_id, school, avatar_gender) %>%
  summarise(count = n())
# %>% mutate(percent = Freq/(sum(Freq)))


gender = gender[-which(is.na(gender$avatar_gender)),]
# gender$avatar_gender = ifelse(gender$avatar_gender=="Male",1,0)


# GENDER DISTR PLOT
data_perc <- t(prop.table(table(gender$avatar_gender))) * 100    # Convert data to probability table
barplot(data_perc, ylab = "Percent",  xlab = "Gender Group",
        #ylab = "Distribution, (%)",
        ylim=c(0,60),
        beside=TRUE,
        main = "Gender Distribution",
        col = c("mistyrose", "lightblue"))               # Draw barchart with Base R





# GENDER
age <- logs %>%
  group_by(player_id, school, avatar_age) %>%
  summarise(count = n())
# %>% mutate(percent = Freq/(sum(Freq)))


age = age[-which(is.na(age$avatar_age)),]

# AGE DISTR PLOT
data_percage <- t(prop.table(table(age$avatar_age))) * 100    # Convert data to probability table
barplot(data_percage, ylab = "Percent",
        xlab = "Age Group",
        #ylab = "Distribution, (%)",
        ylim=c(0,30),
        beside=TRUE,
        main = "Age Distribution",
        col = c("gray","lightblue", "mistyrose", "lightcyan"))

#names.arg=c("Health","Money","School","Friends","Happiness","Family"))



str(logs$priority_type)

priority = logs[-which(is.na(logs$priority_type)),]
priority$priority_type = as.factor(priority$priority_type)


# PRIORITY FOR EVERYONE
age <- priority %>%
  group_by( player_id, school, priority_type) %>%
  summarise(count = n())

data_percpri <- t(prop.table(table(priority$priority_type))) * 100    # Convert data to probability table
barplot(data_percpri, ylab = "Percent",
        xlab="Priority Type",
        ylim=c(0,35),
        beside=TRUE,
        main = "Priority Types Distribution",
        col = c("lightblue", "mistyrose", "lightcyan","gray","light yellow"),
        names.arg=c("Health","Money","School","Friends","Happiness","Family"))


# PRIORITY FOR PEOPLE WHO COMPLETED IT  
priority_complete = completed[-which(is.na(completed$priority_type)),]
age <- priority_complete %>%
  group_by(player_id, school, priority_type) %>%
  summarise(count = n())

data_perc <- t(prop.table(table(priority_complete$priority_type))) * 100    # Convert data to probability table
barplot(data_perc, ylab = "Percent")    




completion = logs[-which(is.na(logs$proportion_complete)),]

# completion rate
completion <- completion %>%
  group_by(player_id, school, proportion_complete) %>%
  summarise(count = n())

completion <- sort(completion$proportion_complete)

data_perc <- t(prop.table(table(completion$proportion_complete))) * 100    # Convert data to probability table
barplot(data_perc, ylab = "Percent",
        xlab = "Percent Completed",
        main = "Completion Distribution")
#col = c("gray","lightblue", "mistyrose", "lightcyan"))