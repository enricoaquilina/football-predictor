rm(list=ls())
library(plyr)

raw<-read.csv('/home/psycheyyy/Desktop/MSc/Statistics for Data Scientists/Week 8/data/soccer.csv', header=TRUE, sep=',')

# Start of 2 BASIC tables...

# This is the start of the HOME table
home_matches_played = data.frame(table(raw$HomeTeam))
home_goals_scored <- data.frame(ddply(raw, 'HomeTeam', numcolwise(sum))[2])

home_table <- data.frame(home_matches_played$Var1, home_matches_played$Freq, home_goals_scored)
mean_data <- data.frame(home_table[3]/home_table[2])
#colnames(mean_data)<-c('Mean Home Goals For')
home_table <- cbind(home_table, mean_data)

rm(home_matches_played)
rm(home_goals_scored)
rm(mean_data)

home_goals_against <- data.frame(ddply(raw, 'HomeTeam', numcolwise(sum))[3])
#colnames(home_goals_against)<-'Home Goals Against'
home_table <- cbind(home_table, home_goals_against)

rm(home_goals_against)

mean_home_goals_against <- data.frame(home_table[5]/home_table[2])
#colnames(mean_home_goals_against)<-'Mean Home Goals Against'
home_table <- cbind(home_table, mean_home_goals_against)
rm(mean_home_goals_against)

# This is the start of the AWAY table
away_table <- data.frame(home_table[1], home_table[2])
away_goals_for <- data.frame(ddply(raw, 'AwayTeam', numcolwise(sum))[3])
#colnames(away_goals_for) <- 'Away Goals For'
away_table <- cbind(away_table, away_goals_for)
rm(away_goals_for)


mean_away_goals_for <- data.frame(away_table[3]/away_table[2])
#colnames(mean_away_goals_for) <- 'Mean Away Goals For'
away_table <- cbind(away_table, mean_away_goals_for)
rm(mean_away_goals_for)

away_goals_against <- data.frame(ddply(raw, 'AwayTeam', numcolwise(sum)))[2]
#colnames(away_goals_against) <- 'Away Goals Against'
away_table <- cbind(away_table, away_goals_against)
rm(away_goals_against)
rm(raw)

away_home_goals_against <- data.frame(away_table[5]/away_table[2])
#colnames(away_home_goals_against) <- 'Away Home Goals Against'
away_table <- cbind(away_table, away_home_goals_against)
rm(away_home_goals_against)

#Doing the mean stuff for HOME Basic table
colnames(home_table) = c('','Home Goals Played', 'Home Goals For', 'Mean Home Goals For', 'Home Goals Against', 'Mean Home Goals Against')
calculation_frame = data.frame('Total', sum(home_table$`Home Goals Played`), sum(home_table$`Home Goals For`), sum(home_table$`Mean Home Goals For`), sum(home_table$`Home Goals Against`), sum(home_table$`Mean Home Goals Against`))
colnames(calculation_frame) = c('','Home Goals Played', 'Home Goals For', 'Mean Home Goals For', 'Home Goals Against', 'Mean Home Goals Against')

no_of_teams <- nrow(home_table)
mean_frame = data.frame('Mean', calculation_frame$`Home Goals Played`/no_of_teams, calculation_frame$`Home Goals For`/no_of_teams, calculation_frame$`Mean Home Goals For`/no_of_teams, calculation_frame$`Home Goals Against`/no_of_teams, calculation_frame$`Mean Home Goals Against`/no_of_teams)
colnames(mean_frame) <- c('','Home Goals Played', 'Home Goals For', 'Mean Home Goals For', 'Home Goals Against', 'Mean Home Goals Against')
calculation_frame <- rbind(calculation_frame, mean_frame)
home_table<-rbind(home_table, calculation_frame)
rm(mean_frame)
rm(calculation_frame)

#Doing the mean stuff for AWAY Basic table
colnames(away_table) = c('','Away Goals Played', 'Away Goals For', 'Mean Away Goals For', 'Away Goals Against', 'Mean Away Goals Against')
calculation_frame = data.frame('Total', sum(away_table$`Away Goals Played`), sum(away_table$`Away Goals For`), sum(away_table$`Mean Away Goals For`), sum(away_table$`Away Goals Against`), sum(away_table$`Mean Away Goals Against`))
colnames(calculation_frame) = c('','Away Goals Played', 'Away Goals For', 'Mean Away Goals For', 'Away Goals Against', 'Mean Away Goals Against')

mean_frame = data.frame('Mean', calculation_frame$`Away Goals Played`/no_of_teams, calculation_frame$`Away Goals For`/no_of_teams, calculation_frame$`Mean Away Goals For`/no_of_teams, calculation_frame$`Away Goals Against`/no_of_teams, calculation_frame$`Mean Away Goals Against`/no_of_teams)
colnames(mean_frame) <- c('','Away Goals Played', 'Away Goals For', 'Mean Away Goals For', 'Away Goals Against', 'Mean Away Goals Against')
calculation_frame <- rbind(calculation_frame, mean_frame)
away_table <- rbind(away_table, calculation_frame)
rm(mean_frame)
rm(calculation_frame)

# End of 2 BASIC tables

#Start of POWER table..
home_attack_divisor <- home_table$`Mean Home Goals For`[no_of_teams+2]
home_defence_divisor <- home_table$`Mean Home Goals Against`[no_of_teams+2]

away_attack_divisor <- away_table$`Mean Away Goals For`[no_of_teams+2]
away_defence_divisor <- away_table$`Mean Away Goals Against`[no_of_teams+2]

home_attack_list <- home_table$`Mean Home Goals For`[1:no_of_teams]
home_defence_list <- home_table$`Mean Home Goals Against`[1:no_of_teams]

away_attack_list <- away_table$`Mean Away Goals For`[1:no_of_teams]
away_defence_list <- away_table$`Mean Away Goals Against`[1:no_of_teams]

home_attack <- lapply(home_attack_list, function(x) x/home_attack_divisor)
home_defence <- lapply(home_defence_list, function(x) x/home_defence_divisor)
away_attack <- lapply(away_attack_list, function(x) x/away_attack_divisor)
away_defence <- lapply(away_defence_list, function(x) x/away_defence_divisor)

power_table <- data.frame(cbind(home_attack,home_defence,away_attack,away_defence))

rm(no_of_teams)
rm(home_attack_divisor)
rm(home_defence_divisor)
rm(away_attack_divisor)
rm(away_defence_divisor)
rm(home_attack_list)
rm(home_defence_list)
rm(away_attack_list)
rm(away_defence_list)
rm(home_attack)
rm(home_defence)
rm(away_attack)
rm(away_defence)




