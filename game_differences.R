# visualizing differences between football years. 
library(tidyverse)
library(geomnet)

all_fb <- read_csv("footballData2000-16.csv")
names(all_fb) <- str_replace_all(names(all_fb), " ", "_")
create_gamesumm <- function(dat){
  dat2 <- data.frame(do.call("rbind", dat %>% select(from_id, to_id) %>% apply(1, sort)), stringsAsFactors = F)
  dat3 <- dat2 %>% group_by(from_id, to_id) %>% 
    summarize(ngames = n()) 
  return(dat3)
}
create_diffnet <- function(old, new){
  N1 <- nrow(old)
  N2 <- nrow(new)
  diffs <- data_frame(from_id = character(0), to_id = character(0), 
                      ngames = integer(0), type = character(0))
  for(i in 1:N2){
    game <- new[i,]
    check <- filter(old, from_id == game$from_id & 
                      to_id == game$to_id)
    if (nrow(check) == 0){
      diffs <- add_row(diffs, from_id = game$from_id, to_id = game$to_id,
              ngames = game$ngames, type = "newgame")
    } else if (nrow(check) > 0 & check$ngames < game$ngames){
      diffs <- add_row(diffs, from_id = game$from_id, to_id = game$to_id,
              ngames = game$ngames - check$ngames, type = "newgame")
    } 
  }
  
  for(i in 1:N1){
    game <- old[i,]
    check <- filter(new, from_id == game$from_id & 
                      to_id == game$to_id)
    if (nrow(check) == 0){
      diffs <- add_row(diffs, from_id = game$from_id, to_id = game$to_id,
              ngames = game$ngames, type = "removedgame")
    } else if (nrow(check) > 0 & check$ngames < game$ngames){
      diffs <- add_row(diffs, from_id = game$from_id, to_id = game$to_id,
              ngames = game$ngames - check$ngames, type = "removedgame")
    } 
  }
  return(diffs)
  # newInold <- dplyr:::match_data_frame(new[,-3], old[,-3])
  # newgames <- new[is.na(newInold),-3]
  # newgames$type <- "newgame"
  # oldInnew <- dplyr:::match_data_frame(old[,-3], new[,-3])
  # oldgames <- old[is.na(oldInnew),-3]
  # oldgames$type <- "oldgame"
  # diffs <- rbind(newgames, oldgames)
  # return(diffs)
}
all_fb %>% nest(-year) %>% 
  mutate(games = map(data, create_gamesumm)) -> nest_fb_all

nest_fb_all$diffnet <- list(NA)
for(i in 2:nrow(nest_fb_all)){
  nest_fb_all$diffnet[[i]] <- create_diffnet(old = nest_fb_all$games[[i-1]],
                                           new = nest_fb_all$games[[i]])
  
}

diffs_unnest <- unnest(nest_fb_all[-1,c("year","diffnet")])

team_data <- read_csv("footballteamdata.csv")
teamconfs <- team_data[,c("year", "School", "Conf")]

diffs_unnest2 <- left_join(diffs_unnest, teamconfs, by = c("from_id"="School", "year"="year"))

names(diffs_unnest2)[6] <- paste0(names(diffs_unnest2)[6], ".from")

diffs_unnest3 <- left_join(diffs_unnest2, teamconfs, by = c("to_id"="School", "year"="year"))
names(diffs_unnest3)[7] <- paste0(names(diffs_unnest3)[7], ".to")


head(diffs_unnest3)

ggplot(data = diffs_unnest3, aes(from_id = from_id, to_id = to_id, color = Conf.from)) + 
  geom_net(aes(linetype = type), fiteach = T) +
  theme(legend.position = "none") + 
  facet_wrap(~year)
