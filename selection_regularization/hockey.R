library(gamlr)
library(tidyverse)

# read in data: all goals in NHL hockey from 2002-2014
goal = read.csv("./hockey_data/goal.csv", row.names=1)

head(goal, 50)


# data on situation, teams, and players on the ice
config = readMM("./hockey_data/config.mtx.txt")
team = readMM("./hockey_data/team.mtx.txt")
player = readMM("./hockey_data/player.mtx.txt")

# read in the column names
colnames(config) = scan('./hockey_data/config_names.txt', what='char', sep="\n")
colnames(team) = scan('./hockey_data/team_names.txt', what='char', sep="\n")
colnames(player) = scan('./hockey_data/player_names.txt', what='char', sep="\n")

# +1 for home team, -1 for visiting team
head(config,50)
team[1:30, 1:50]
player[1:30, 1:50]

# set up x and y: we'll fit a model with situation, team, and player effects
# but we'll put a lasso penalty on the player coefficients
x = cbind(config,team,player)
y = goal$homegoal

## fit the plus-minus regression model
## note: non-player effects are unpenalized
## use the `free` flag to encode this
fit = gamlr(x, y, free=1:(ncol(config)+ncol(team)), standardize=FALSE, family="binomial")

plot(fit)
beta_hat = coef(fit)
head(beta_hat)

player_pm_logit = coef(fit)[colnames(player),] %>% sort(., decreasing=TRUE)
head(player_pm_logit, 25)
tail(player_pm_logit, 25)

beta_hat[colnames(config),]
beta_hat[colnames(team),]
