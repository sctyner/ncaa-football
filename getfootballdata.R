library(tidyverse)
library(rvest)
library(purrr)
library(stringr)
library(lubridate)
library(geomnet)
library(plotly)
"http://www.sports-reference.com/cfb/years/2016-schedule.html"
"http://www.sports-reference.com/cfb/years/2016-standings.html"
years <- 2000:2016
sched_urls <- paste0("http://www.sports-reference.com/cfb/years/", years, "-schedule.html")
team_urls <- paste0("http://www.sports-reference.com/cfb/years/", years, "-standings.html")

game_data <- data.frame(year = years, url = sched_urls, stringsAsFactors = F)
team_data <- data.frame(year = years, url = team_urls, stringsAsFactors = F)
season_end <- read_csv("reg_season_ends.csv")

get_schedule <- function(url){
  tab <- (url %>% read_html() %>% html_nodes("table") %>% html_table())[[1]]
  tab <- tab[-which(tab$Rk == "Rk"),]
  names(tab)[grep("Pts", names(tab))] <- c("WinPts", "LosePts")
  return(tab)
}

game_data <- game_data %>% mutate(games = map(url, get_schedule))
game_data <- game_data[,-2] %>% unnest %>% separate(Date, c("Month", "Date", "Year"))
names(game_data)[10] <- "WinTeamHome"
game_data$WinTeamHome<- ifelse(game_data$WinTeamHome == "@", FALSE, TRUE)
game_data$`Winner/Tie` <- str_replace(game_data$`Winner/Tie`,"\\(\\d+\\)\\s", "")
game_data$`Loser/Tie` <- str_replace(game_data$`Loser/Tie`,"\\(\\d+\\)\\s", "")

game_data <- game_data %>% mutate(Month2 = match(Month, month.abb))
game_data$GameDate <- as_date(as.Date(paste(game_data$Year, game_data$Month2, game_data$Date, sep = "-"), format = "%Y-%m-%d"))
season_end$Date <- as_date(as.Date(paste(season_end$Year, 12, season_end$EndDay, sep = "-"), format = "%Y-%m-%d"))

write_csv(game_data, "footballgamedata.csv")

game_data_list <- split(game_data, as.factor(game_data$year))

for (i in 1:length(years)){
  yr <- years[i]
  endDate <- season_end$Date[season_end$Year == yr]
  gdat <- game_data_list[[i]]
  gdat <- gdat %>% filter(GameDate <= endDate)
  game_data_list[[i]] <- gdat
}

game_data_regs <- do.call("rbind", game_data_list)
rownames(game_data_regs) <- 1:nrow(game_data_regs)  

get_teams <- function(url){
  tab <- (url %>% read_html %>% html_nodes("table") %>% html_table())[[1]]
  names(tab) <- str_trim(paste(names(tab), tab[1,]))
  tab <- tab[-which(tab$Rk == "Rk"),]
  tab <- tab[-which(tab$`Overall L` == "Overall"),]
  names(tab)[grep(pattern = "Pts",names(tab))] <- c("PPG", "OPPG")
  return(tab)
}

team_data <- team_data %>% mutate(games = map(url, get_teams))
team_data <- team_data[,-2] %>% unnest

names(game_data)[grep("Pts", names(game_data))]

team_data %>% group_by(School) %>% count %>% data.frame

write_csv(team_data, "footballteamdata.csv")

# remove schools with missing data for now. 

misswinidx <- which(!(game_data$`Winner/Tie` %in% unique(team_data$School)))
missloseidx <- which(!(game_data$`Loser/Tie` %in% unique(team_data$School)))
rmvidx <- union(misswinidx, missloseidx)

game_data2 <- game_data[-rmvidx,]


library(geomnet)

game_edata <- as.edgedf(game_data2[,c(8, 11, 1, 3:7, 9:10, 12:17)])
names(game_edata)[7] <- "TrueYear"
allgames <- list()
for (i in 1:length(years)){
  yr <- years[i]
  net <- merge(game_edata %>% filter(year == yr), team_data[,c(3, 1:2, 4:19)] %>% filter(year == yr), by.x = c("from_id", "year"), by.y = c("School", "year"), all = T)
  allgames[[i]] <- net
}

allgames_df <- do.call("rbind", allgames)




unique(game_data$`Winner/Tie`)[is.na(match(unique(game_data$`Winner/Tie`), unique(team_data$School)))]


# still not working right. must be some schools in the overall data set that are missing in their year. 

#  
allgames <- list()
for (i in 1:length(years)){
  yr <- years[i]
  edata <- game_edata %>% filter(year == yr) %>% select(-Notes)
  vdata <- team_data[,c(3, 1:2, 4:19)] %>% filter(year == yr) %>% select(-Notes)
  misswinidx <- which(!(edata$from_id %in% unique(vdata$School)))
  missloseidx <- which(!(edata$to_id %in% unique(vdata$School)))
  rmvidx <- union(misswinidx, missloseidx)
  net <- merge(edata[-rmvidx,], vdata, by.x = c("from_id", "year"), by.y = c("School", "year"), all = T)
  allgames[[i]] <- net
}

allgames_df <- do.call("rbind", allgames)

allgames_df %>% separate(Conf, into = c("Conf", "Divsn"), sep = " \\(") %>% 
  mutate(Conf = str_trim(Conf), Divsn = str_trim(str_replace(Divsn, "\\)", ""))) -> allgames_df2

table(allgames_df2$Conf)
table(allgames_df2$Divsn)

team_data %>% ggplot(aes(x = year, y = School, color =Conf)) + geom_line() + geom_point(size = 1) + theme(legend.position = 'none')

allgames_df <- read_csv("footballData2000-16.csv")
ggplot(data = allgames_df %>% filter(year <=2005), aes(from_id = from_id, to_id = to_id, color = Conf, group = Conf)) + 
  geom_net(fiteach = T, size = 1.5, linewidth = .5, singletons = F) + 
  theme_net() + theme(legend.position = "none") + 
  facet_wrap(~year)

dim(allgames_df)

library(crosstalk)

fb_sd <- SharedData$new(allgames_df %>% filter(year <= 2005), ~from_id)

p <- ggplot(data = fb_sd, aes(from_id = from_id, to_id = to_id, color = Conf, group = Conf)) + 
  geom_net(fiteach = T, size = 1.5, linewidth = .5, singletons = F) + 
  theme_net() + theme(legend.position = "none") + 
  facet_wrap(~year)
ggplotly(p, tooltip = c("from_id", "color")) %>%
  highlight(on = "plotly_click", color = "red")

write_csv(allgames_df, "footballData2000-16.csv")

