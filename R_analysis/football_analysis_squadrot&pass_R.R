# Needed Libraries/Packages
install.packages('rjson')
install.packages('data.table')

library(rjson)
library(data.table)


# Reading the File from JSON into a List
comps <- fromJSON(file = '/Users/alijazibrizvi/Documents/Data Analytics/SoccermaticsInPython/R_analysis/competitions.json')

# Converting the List into a DataFrame
comps <- data.frame(do.call(rbind, comps), stringsAsFactors = FALSE)
## View(comps)

# Obtain Matches
matches.files <- list.files(path = '/Users/alijazibrizvi/Documents/Data Analytics/SoccermaticsInPython/open-data-master/data/matches',
                      full.names = TRUE, recursive = TRUE)

matches.list <- list()
for(i in 1:length(matches.files)){
  match.temp <- fromJSON(file = matches.files[i]) ## Loops Through each File and Obtains the Necessary Match Info
  matches <- lapply(match.temp, function(x) data.frame(t(unlist(x)), stringsAsFactors = FALSE))
  matches.df <- rbindlist(matches, fill = TRUE)
  matches.list[[i]] <- matches.df
}

all.matches.df <- data.frame(rbindlist(matches.list, fill = TRUE)) ## Combines all Matches from all Competitions into 1 DataFrame
## View(all.matches.df)

# Removing a lot of Unneeded Columns
cols.tokeep <- names(which(unlist(lapply(all.matches.df, function(x) length(which(is.na(x))))) == 0))

all.matches.clean <- all.matches.df[, cols.tokeep]
all.matches.clean$match_week <- as.numeric(all.matches.clean$gameweek, na = TRUE) ## Converting some Variables to Numeric
all.matches.clean$home_score <- as.numeric(all.matches.clean$roundId, na = TRUE)
all.matches.clean$away_score <- as.numeric(all.matches.clean$duration, na = TRUE)
## View(all.matches.clean$match_week)


# Obtaining all Events' Data
event.files <- list.files(path = '/Users/alijazibrizvi/Documents/Data Analytics/SoccermaticsInPython/open-data-master/data/events',
                          full.names = TRUE, recursive = TRUE)

event.list <- list()
for(i in 1:length(event.files)){
  event.temp <- fromJSON(file = event.files[i])
  
  # unique(unlist(lapply(event.temp, function(x) x$type$name))) | Let's us see the Unique Events that happen in a Game
  
  team_ids <- c() # Getting the Unique Team IDs participating in a Match
  
  # Obtaining the Index where we find the Event that talks about the Starting XI
  starting.x11.index <- which(unlist(lapply(event.temp,function(x) x$type$name))=="Starting XI")
  starting.x11.list <- list()
  for(s in 1:2){
    starting.x11.team1 <- data.frame(matrix(t(unlist(event.temp[[s]]$tactics$lineup)),ncol = 5, byrow = TRUE), stringsAsFactors = FALSE)
    colnames(starting.x11.team1) <- names(unlist(event.temp[[s]]$tactics$lineup))[1:5]
    starting.x11.team1$formation <- event.temp[[s]]$tactics$formation
    starting.x11.team1$team_id <- event.temp[[s]]$team$id
    
    teamids <- c(teamids, event.temp[[s]]$team$id)
    
    starting.x11.team1$team_name <- event.temp[[s]]$team$name
    starting.x11.list[[s]] <- starting.x11.team1
  }
  
  pass.index <- which(unlist(lapply(event.temp, function(x) x$type$name)) == 'Pass')
  
  # Obtaining the Passes just for team1 (the first element in "teamids")
  pass.team1 <- pass.index[wchich(unlist(lapply(pass.index, function(x) event.temp[[x]]$team$id)) == teamids[1])]
  
  pass.team1.df <- data.frame(matrix(NA,nrow=1,ncol=11))
  colnames(pass.team1.df) <- c("Possession","Passer","X.Pass","Y.Pass",
                               "Pass.Type","Receiver","X.Receive","Y.Receive",
                               "Pass.Length","Pass.Angle","Body.Part")
  
  for(p in 1:length(pass.team1)){
    pass.temp <- event.temp[[pass.team1[p]]]
    possession <- pass.temp$possession
    passer <- pass.temp$player$id
    pass.location <- pass.temp$location
    pass.type <- pass.temp$pass$height$name
    receiver <- pass.temp$pass$recipient$id
    receive.location <- pass.temp$pass$end_location
    pass.length <- pass.temp$pass$length
    pass.angle <- pass.temp$pass$angle
    body.part <- pass.temp$pass$body_part$name
    
    row.toadd <- c(possession, passer, pass.location, pass.type, receiver, receive.location, pass.length, pass.angle, body.part)
    pass.team1.df <- rbind(pass.team1.df, row.toadd)
  }
  pass.team1.df <- pass.team1.df[-1,]
  pass.team1.df[, c(1:4, 6:10)] <- lapply(pass.team1.df[, c(1:4, 6:10)], as.numeric)
  
  pass.team1.df <- pass.team1.df |>
    group_by(Possession) |>
      mutate(seq = row_number())
  pass.team1.df$team_id <- teamids[1]

  pass.team2 <- pass.index[which(unlist(lapply(pass.index, function(x) event.temp[[x]]$team$id)) == teamids[2])]
  pass.team2.df <- data.frame(matrix(NA, nrow = 1, ncol = 11))
  colnames(pass.team2.df) <- c('Possession', 'Passer', 'X.Pass', 'Y.Pass',
                               'Pass.Type', 'Receiver', 'X.Receive', 'Y.Receive',
                               'Pass.Length', 'Pass.Angle', 'Body.Part')


  for(p in 1:length(pass.team2)){
    pass.temp <- event.temp[[pass.team2[p]]]
    possession <- pass.temp$possession
    passer <- pass.temp$player$id
    pass.location <- pass.temp$location
    pass.type <- pass.temp$pass$height$name
    receiver <- pass.temp$pass$recipient$id
    receive.location <- pass.temp$pass$end_location
    pass.length <- pass.temp$pass$length
    pass.angle <- pass.temp$pass$angle
    body.part <- pass.temp$pass$body_part$name
    
    row.toadd <- c(possession, passer, pass.location, pass.type, receiver, receive.location, pass.length, pass.angle, body.part)
    pass.team2.df <- rbind(pass.team2.df, row.toadd)
  }
  pass.team2.df <- pass.team2.df[-1, ]
  pass.team2.df[, c(1:4, 6:10)] <- lapply(pass.team2.df[, c(1:4, 6:10)], as.numeric)
  pass.team2.df <- pass.team2.df |> group_by(Possession) |> mutate(seq = row_number())
  pass.team2.df$team_id <- teamids[2]
  
  
  pass.list <- list(pass.team1.df, pass.team2.df)
  
  match.id <- strsplit(basename(event.files[i]), "[.]")[[1]][1]
  
  event.list[[match.id]] <- list(starting.x11.list, pass.list)
  
}


# Analysis 1: Let's Look at the Squad Rotation per Match

# We will use the FA Women's Super League, which is competition_id == 37 and season_id == 4

## This gets us the Number of Matches per Competition and Season to check which Season we have the most Data for
matches.count <- all.matches.clean |>
  group_by(competition.competition_id,season.season_id) |>
  summarise(count = n())

matches.wsl.1819 <- all.matches.clean[which(all.matches.clean$competition.competition_id==37 & all.matches.clean$season.season_id==4),]
matches.wsl.1819 <- matches.wsl.1819[order(matches.wsl.1819$match_week),]

wsl.teams <- unique(matches.wsl.1819$home_team.home_team_name) # Getting the Unique list of Teams so we can Loop through each Team

squad.rotation.list <- list() # This list is for keeping track of the number of Squad rotations per Matchweek
team.starting.x11 <- list() # This list is for keeping track of the Starting 11 for each Matchweek
for(w in 1:length(wsl.teams)){
  squad.rotation.list[[wsl.teams[w]]] <- list()
  team.starting.x11[[wsl.teams[w]]] <- list()
  team.matches <- matches.wsl.1819[which(matches.wsl.1819$home_team.home_team_name==wsl.teams[w] | 
                                           matches.wsl.1819$away_team.away_team_name==wsl.teams[w]),]
  team.matches$GD <- team.matches$home_score-team.matches$away_score
  
  team.events.index <- which(names(event.list) %in% team.matches$match_id)
  team.events <- event.list[team.events.index]
  team.id <- unique(matches.wsl.1819[which(matches.wsl.1819$home_team.home_team_name==wsl.teams[w]),]$home_team.home_team_id)
  team.matches$Team.GD <- ifelse(team.matches$home_team.home_team_id==team.id,team.matches$GD,team.matches$GD*-1)
  team.matches$Result <- ifelse(team.matches$Team.GD>0,"W",
                                ifelse(team.matches$Team.GD==0,"D","L"))
  
  
  for(i in 1:length(team.events)){ # for each game of that particular Team, get the Starting 11 for them
    starting.x11 <- team.events[[i]][[1]]
    starting.x11.index <- which(lapply(starting.x11, function(x) unique(x$team_id))==team.id)
    
    team.11 <- starting.x11[[starting.x11.index]]
    team.starting.x11[[wsl.teams[w]]][[i]] <- team.11$player.name
  }
  
  num.matches <- length(team.events)
  # for all the Matches after the first match, Calculating the difference in Players from matchweek X and matchweek X+1
  squad.rotation <- c(0,sapply(seq(1:(num.matches-1)),function(x) length(setdiff(team.starting.x11[[w]][[x]],team.starting.x11[[w]][[x+1]]))))
  team.matches$Rotated <- squad.rotation 
  squad.rotation.list[[w]] <- team.matches[,c("match_week","Result","Rotated")]
}

result.colors <- c("W"="forestgreen","L"="red","D" = "yellow") # Defining a set of colors to use in our plot

# "ggplot" is where you bind the data. The "aes" stands for aesthetic and defines what data is bound to what part of the graph
ggplot(data=squad.rotation.list[[1]], aes(x=match_week,y=Rotated,fill=Result)) + geom_bar(stat="identity",width=0.5)+
  scale_fill_manual(values=result.colors)

all.squad.rotations <- plyr::ldply(squad.rotation.list,.id="Team") # Binds all the Rows of the list elements together and adds the list element name as an additional column

ggplot(data=all.squad.rotations, aes(x=match_week,y=Rotated,fill=Result)) + geom_bar(stat = "identity", width=0.5)+
  scale_fill_manual(values=result.colors) + facet_grid(rows=vars(Team)) # Adding a Plot for each Team


# Analysis 2: Clustering Passes
## We want to Cluster Passes per Team to understand the Passing tendencies of each Team

## We will use the same Competition and Season

pass.events.index <- which(names(event.list) %in% matches.wsl.1819$match_id)

passes.list <- list()
for(i in 1:length(pass.events.index)){
  match.temp <- event.list[[pass.events.index[i]]][[2]]
  
  all.passes <- do.call(rbind,match.temp)
  
  all.passes.locations <- all.passes[,c("team_id","X.Pass","Y.Pass","X.Receive","Y.Receive")]
  
  passes.list[[i]] <- all.passes.locations
  
}

full.pass.df <- do.call(rbind,passes.list)
full.pass.df <- full.pass.df[which(full.pass.df$Y.Receive <= 80),] # Cleaning the data
full.pass.df$Y.Pass <- 80 - full.pass.df$Y.Pass # Changing the axis so that the Origin starts at the lower left corner
full.pass.df$Y.Receive <- 80 - full.pass.df$Y.Receive


library(parallel)
library(ggplot2)

# Performing K-Means on the Dataset (Removing the 1st column because we just need to use the last 4 columns in our Analysis)
mc = mclapply(c(25, 50, 75), function(x, centers) kmeans(x, centers, iter.max = 1000), x = full.pass.df[, -1])

full.pass.df$Cluster.25 <- mc[[1]]$cluster # Created Clusters using 25 Clusters
full.pass.df$Cluster.50 <- mc[[2]]$cluster # Created Clusters using 50 Clusters
full.pass.df$Cluster.75 <- mc[[3]]$cluster # Created Clusters using 75 Clusters

cluster.50.summary <- full.pass.df %>% group_by(Cluster.50) |>
  summarise(X.Pass = mean(X.Pass),Y.Pass = mean(Y.Pass),
    X.Receive = mean(X.Receive), Y.Receive = mean(Y.Receive), count = n()) # Obtaining for each Cluster ID, the Average location of the Pass

cluster.50.team.summary <- full.pass.df |> 
  group_by(Cluster.50,team_id) |>
    summarise(count = n()) # Getting a Count per Team
arsenal.clusters <- cluster.50.team.summary |> 
  group_by(Cluster.50) |>
    mutate(z.score = (count - mean(count))/sd(count)) |>
      filter(team_id == 968 & z.score >= 1.5) # Identifying which Clusters that Arsenal does more than 1.5 sd than the league average

source('/Users/alijazibrizvi/Documents/Data Analytics/SoccermaticsInPython/Draw_Pitch.R') # Loading in "hori5", which contains the Football field

hori5 + geom_segment(data=cluster.50.summary, aes(x = X.Pass, xend = X.Receive,
                                                  y = Y.Pass, yend = Y.Receive, color=count), size=1.5, 
                     arrow = arrow(length = unit(0.03, "npc"))) +
                     geom_text(data = cluster.50.summary, aes(x = X.Pass, y = Y.Pass, label = Cluster.50))

hori5 + geom_segment(data = cluster.50.summary, aes(x = X.Pass, xend = X.Receive,
                                                  y = Y.Pass, yend = Y.Receive), size = 1.5, 
                     arrow = arrow(length = unit(0.03, "npc"))) +
                     geom_segment(data = cluster.50.summary[which(cluster.50.summary$Cluster.50 %in% arsenal.clusters$Cluster.50), ], aes(x = X.Pass, xend = X.Receive,
                     y = Y.Pass, yend = Y.Receive), size = 1.5, color = "red", arrow = arrow(length = unit(0.03, "npc")))

hori5 + geom_segment(data = full.pass.df[which(full.pass.df$Cluster.50 == 12 & full.pass.df$team_id == 968),], 
                    aes(x = X.Pass, xend = X.Receive, y = Y.Pass,yend = Y.Receive), 
                    size = 1.5, arrow = arrow(length = unit(0.03, "npc")))

