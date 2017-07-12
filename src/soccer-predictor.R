rm(list=ls())
library(plyr)

ComputeBasicTables<-function(){
  raw<-read.csv('/home/enrico/Desktop/football-predictor/data/soccer.csv', header=TRUE, sep=',')
  
  home_matches_played<<-data.frame(table(raw$HomeTeam))
  no_of_teams<<-nrow(home_matches_played)
  home_scored<-data.frame(GoalsScored=ddply(raw, 'HomeTeam', numcolwise(sum))[2])
  home_conceded<-data.frame(GoalsConceded=ddply(raw, 'HomeTeam', numcolwise(sum))[3])
  
  home_table<<-data.frame(HomeTeam=home_matches_played$Var1, 
                          TotalMatches=home_matches_played$Freq, 
                          GoalsScored=home_scored,
                          GoalsConceded=home_conceded)
  
  home_table<<-cbind(home_table, 
                     AvgScored=home_table[3]/home_table[2], 
                     AvgConceded=home_table[4]/home_table[2])
  
  colnames(home_table)<<-c('','TotalMatches', 'Scored', 'Conceded', 'MeanScored', 'MeanConceded')
  
  #rm(home_matches_played)
  rm(home_scored)
  rm(home_conceded)
  #-------------------------------------------------------------------------------------------------
  #-------------------------------------------------------------------------------------------------
  # AWAY table
  away_matches_played=data.frame(table(raw$AwayTeam))
  away_scored<-data.frame(GoalsScored=ddply(raw, 'AwayTeam', numcolwise(sum))[3])
  away_conceded<-data.frame(GoalsConceded=ddply(raw, 'AwayTeam', numcolwise(sum))[2])
  
  away_table<<-data.frame(AwayTeam=away_matches_played$Var1, 
                          TotalMatches=away_matches_played$Freq, 
                          GoalsScored=away_scored,
                          GoalsConceded=away_conceded)
  
  away_table<<-cbind(away_table, 
                     AvgScored=away_table[3]/away_table[2], 
                     AvgConceded=away_table[4]/away_table[2])
  
  colnames(away_table)<<-c('','TotalMatches', 'Scored', 'Conceded', 'MeanScored', 'MeanConceded')
  
  rm(away_matches_played)
  rm(away_scored)
  rm(away_conceded)
  
  MeanComputation()
}
PowerTables<-function(){
  #Start of POWER table..
  home_attack_divisor <- home_table$MeanScored[no_of_teams+2]
  home_defence_divisor <- home_table$MeanConceded[no_of_teams+2]
  
  away_attack_divisor <- away_table$MeanScored[no_of_teams+2]
  away_defence_divisor <- away_table$MeanConceded[no_of_teams+2]
  
  home_attack_list <- home_table$MeanScored[1:no_of_teams]
  home_defence_list <- home_table$MeanConceded[1:no_of_teams]
  
  away_attack_list <- away_table$MeanScored[1:no_of_teams]
  away_defence_list <- away_table$MeanConceded[1:no_of_teams]
  
  home_attack <- lapply(home_attack_list, function(x) x/home_attack_divisor)
  home_defence <- lapply(home_defence_list, function(x) x/home_defence_divisor)
  away_attack <- lapply(away_attack_list, function(x) x/away_attack_divisor)
  away_defence <- lapply(away_defence_list, function(x) x/away_defence_divisor)
  
  power_table<<-data.frame(home_matches_played$Var1, cbind(home_attack,home_defence,away_attack,away_defence))
  colnames(power_table)<<-c('Team', 'HomeAttack', 'HomeDefence', 'AwayAttack', 'AwayDefence')
  
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
  home_matches_played<<-NULL
  #rm(raw)
}
MeanComputation<-function(){
  #Doing the mean stuff for HOME table
  total_frame<-data.frame('Total', 
                          TotalMatches=sum(home_table$TotalMatches), 
                          Scored=sum(home_table$Scored), 
                          Conceded=sum(home_table$Conceded), 
                          MeanScored=sum(home_table$MeanScored), 
                          MeanConceded=sum(home_table$MeanConceded))
  
  colnames(total_frame)<-c('','TotalMatches','Scored','Conceded','MeanScored','MeanConceded')
  home_table<<-rbind(home_table,total_frame)
  
  mean_frame<-data.frame(Mean='Mean', 
                         total_frame$TotalMatches/no_of_teams, 
                         total_frame$Scored/no_of_teams, 
                         total_frame$Conceded/no_of_teams, 
                         total_frame$MeanScored/no_of_teams, 
                         total_frame$MeanConceded/no_of_teams)
  
  colnames(mean_frame)<-c('','TotalMatches','Scored','Conceded','MeanScored','MeanConceded')
  home_table<<-rbind(home_table, mean_frame)
  
  rm(mean_frame)
  rm(total_frame)
  
  #Doing the mean stuff for AWAY Basic table
  total_frame=data.frame('Total',
                         sum(away_table$TotalMatches), 
                         sum(away_table$Scored), 
                         sum(away_table$Conceded), 
                         sum(away_table$MeanScored), 
                         sum(away_table$MeanConceded))
  colnames(total_frame)<-c('','TotalMatches', 'Scored', 'Conceded', 'MeanScored', 'MeanConceded')
  away_table<<-rbind(away_table,total_frame)
  
  mean_frame=data.frame('Mean', 
                        total_frame$TotalMatches/no_of_teams, 
                        total_frame$Scored/no_of_teams, 
                        total_frame$Conceded/no_of_teams, 
                        total_frame$MeanScored/no_of_teams,
                        total_frame$MeanConceded/no_of_teams)
  colnames(mean_frame)<-c('','TotalMatches', 'Scored', 'Conceded', 'MeanScored', 'MeanConceded')
  away_table<<-rbind(away_table,mean_frame)
  
  rm(mean_frame)
  rm(total_frame)
}
ExpectancyCalculation<-function(home, away){
  #Do the Poisson probability matrix
  home_team <- home
  away_team <- away
  
  home_idx <- which(power_table$Team == home_team)
  away_idx <- which(power_table$Team == away_team)
  
  home_team_data <- power_table[home_idx,]
  colnames(home_team_data) <- c('Team', 'HomeAttack', 'HomeDefence', 'AwayAttack', 'AwayDefence')
  home_team_data <- data.frame(home_team_data$Team, home_team_data$HomeAttack, home_team_data$HomeDefence, home_team_data$AwayAttack, home_team_data$AwayDefence)
  colnames(home_team_data) <- c('Team', 'HomeAttack', 'HomeDefence', 'AwayAttack', 'AwayDefence')
  
  away_team_data <- power_table[away_idx,]
  away_team_data <- data.frame(away_team_data$Team, away_team_data$HomeAttack, away_team_data$HomeDefence, away_team_data$AwayAttack, away_team_data$AwayDefence)
  colnames(away_team_data) <- c('Team', 'HomeAttack', 'HomeDefence', 'AwayAttack', 'AwayDefence')
  
  home_goal_expectancy <<- home_team_data[2] * away_team_data[5] * home_table$MeanScored[nrow(home_table)]
  names(home_goal_expectancy) <<- 'Home Goal Expectancy'
  away_goal_expectancy<<- away_team_data[4] * home_team_data[3] * away_table$MeanScored[nrow(away_table)]
  names(away_goal_expectancy) <<- 'Away Goal Expectancy'
  
  rm(home_team)
  rm(away_team)
  rm(home_idx)
  rm(away_idx)
  rm(home_team_data)
  rm(away_team_data)
}

poisson <- function(home_goal_expectancy, away_goal_expectancy, home_goals, away_goals) {
  home_probability <- exp(1) ^ (-home_goal_expectancy) * home_goal_expectancy ^ (home_goals) / factorial(home_goals)
  away_probability <- exp(1) ^ (-away_goal_expectancy) * away_goal_expectancy ^ (away_goals) / factorial(away_goals)
  return(home_probability * away_probability)
}
build_poisson_graph <- function() {
  graph <<- matrix(, nrow = 5, ncol = 5)
  dimnames(graph)[[1]]<<-list('0','1','2','3','4')
  dimnames(graph)[[2]]<<-list('0','1','2','3','4')
  
  for(row in 1:nrow(graph)) {
    for(column in 1:ncol(graph)) {
      graph[row, column]<<-poisson(home_goal_expectancy, away_goal_expectancy, row-1, column-1)
    }
  }
}
get_prediction <- function(graph) {
  away_win_prob <- 0
  draw_prob <- 0
  home_win_prob <- 0
  over_2_and_half_goals <- 0
  under_2_and_half<-graph[[1]]+graph[[2]]+graph[[3]]+
                    graph[[1,2]]+graph[[2,2]]+graph[[1,3]]
  
  for(row in 1:nrow(graph)) {
    for(column in 1:ncol(graph)) {
      if(column > row) {
        away_win_prob <- away_win_prob + graph[row, column]
      }else if(column == row){
        draw_prob <- draw_prob + graph[row, column]
      }else{
        home_win_prob <- home_win_prob + graph[row, column]
      }
    }
  }

  for(row in 1:nrow(graph)) {
    for(column in 1:ncol(graph)) {
      if((column +row >= 5)) {
        over_2_and_half_goals <- over_2_and_half_goals + graph[row, column]
      }
    }
  }
  print(paste('Home win: ',home_win_prob*100, sep=''))
  print(paste('Away win: ',away_win_prob*100, sep=''))
  print(paste('Match draw: ',draw_prob*100, sep=''))
  print(paste('Under 2.5 goals: ',under_2_and_half*100, sep=''))
  print(paste('Over 2.5 goals: ',over_2_and_half_goals*100, sep=''))
  
}

CrystalBall<-function(home, away){
  ComputeBasicTables()
  PowerTables()
  ExpectancyCalculation(home,away)
  
  build_poisson_graph()
  get_prediction(graph)
}



CrystalBall('Torino','Crotone')
