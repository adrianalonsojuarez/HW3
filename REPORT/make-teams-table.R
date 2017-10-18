##title: short title

#description: a short description of what the script is about
#input(s): what are the inputs required by the script?
#output(s): what are the outputs created when running the script?

``` {r}
read.csv(nba2017_roster)
read.csv(nbs2017_stats)

library("dplyr")

stats = nba2017_stats

#using mutate()

stats <- mutate(stats, missed_fg = field_goals_atts - field_goals_made)

stats <- mutate(stats, missed_ft = points1_atts - points1_made)

stats <- mutate(stats, points = points1_made + points2_made * 2 + points3_made * 3)

stats <- mutate(stats, rebounds = def_rebounds + off_rebounds)

stats <- mutate(stats, efficiency = (points + rebounds + assists + steals +blocks - missed_fg - missed_ft - turnovers) / games_played)

#other way of doing the same

stats$missed_fg = stats$field_goals_atts - stats$field_goals_made

stats$missed_ft <- stats$points1_atts - stats$points1_made

stats$points <- stats$points1_made + stats$points2_made * 2 + stats$points3_made * 3

stats$rebounds <- stats$def_rebounds + stats$off_rebounds

stats$efficiency <- (stats$points + stats$rebounds + stats$assists + stats$assists + stats$steals + stats$blocks - stats$missed_fg - stats$missed_ft - stats$turnovers) / stats$games_played

summary(stats$efficiency)

sink("efficiency-summary.txt ") 
sink()

summary(stats$efficiency)

#USING join()

library(dplyr)
bothdatasets <- left_join(stats, roster, by ="player")

#Other way

roster <- nba2017_roster

merge(stats, roster)

mergeddata <- merge(stats, roster)

#Creating nba2017-teams.csv. With your merged data table you will do some data aggregation—or grouped by operations to create a data frame teams, computing total values, for each team, of the following required variables:
#• team: 3-letter team abbreviation
#• experience: sum of years of experience (up to 2 decimal digits)
#• salary: total salary (in millions, up to 2 decimal digits)
#• points3: total 3-Point Field Goals
#• points2: total 2-Point Field Goals
#• free_throws: total free throws
#• points: total Points
#• off_rebounds: total Offensive Rebounds
#• def_rebounds: total Defensive Rebounds
#• assists: total Assists
#• steals: total Steals
#• blocks: total Blocks
#• turnovers: total Turnovers
#• fouls: total fouls
#• efficiency: total efficiency

mergeddata$salaryinmillions <- mergeddata$salary / 1000000


nba2017teams <- data.frame(
  team = mergeddata$team,
  experience = mergeddata$experience,
  salary = mergeddata$salaryinmillions,
  points3 = mergeddata$points3_made,
  points2 = mergeddata$points2_made,
  free_throws = mergeddata$free_throws,
  points = mergeddata$points,
  off_rebounds = mergeddata$off_rebounds,
  def_rebounds = mergeddata$def_rebounds,
  assists = mergeddata$assists,
  steals = mergeddata$steals,
  blocks = mergeddata$blocks,
  turnovers = mergeddata$turnovers,
  fouls = mergeddata$fouls,
  efficiency = mergeddata$efficiency
)

library(dplyr)

nbateamsss <- nba2017teams %>% group_by(team) %>% 
  summarize(experiecea = sum(experience, na.rm = T), salaryy = sum(salary), points3a = sum(points3), points2a = sum(points2), free_throwsa = sum(free_throws), pointsa = sum(points), off_reboundsa = sum(off_rebounds), def_reboundsa = sum(def_rebounds), assistsa = sum(assists), stealsa = sum(steals), blocksa = sum(blocks), turnoversa = sum(turnovers), foulsa = sum(fouls), efficiencya = sum(efficiency)) 
summary(nbateamsss)

sink("teams-summary.txt")

write.csv(nbateamsss, file = "nba2017-teams.csv")

teams <- nba2017_teams
stars(teams[ ,-1], labels = teams$team)

library(ggplot2)

ggplot(data = teams, aes(experiecea, salaryy, label = team)) + geom_point() + geom_text()

 
```
