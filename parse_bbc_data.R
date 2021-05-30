library(readr)
library(stringr)
library(dplyr)
library(tidyverse)

polls <- read_csv("polls.csv")
ranking <- read_csv("ranking2.csv")

ranking %>%
  separate('artist', sep="ft.", into=c("Main", "Featured")) %>%
  mutate(Main = str_squish(Main)) %>%
  mutate(Featured = str_squish(Featured))-> ranking2


# ranking %>%
#   separate('artist', sep="ft.", into=c("Main", "Featured")) %>%
#   mutate(Main = str_squish(Main)) %>%
#   mutate(Featured = str_squish(Featured))-> ranking2
# 
# sort(unique(ranking$Main))
# 
# write_csv(ranking2, path="ranking2.csv", col_names = T)
# 


#Methods: Total Score, High Score, Average Score, Top 3 Score, Total List Appearances

ranking %>%
  group_by (Main) %>%
  summarise("total_score" = sum(points), "high_score" = max(points), "drop_one" = (total_score - high_score), "total_songs" = n(), "avg_score"=mean(points),
            "top_three" = sum(tail(sort(points), 3)), "top_five"= sum(tail(sort(points), 5)),"main_appearances" = sum(n)) %>%
  ungroup() -> top_rappers

  cor(top_rappers[,2:10]) 
  rowSums(cor(top_rappers[,2:10])  )

  s
  polls %>% group_by(critic_name) %>% summarize(artists = unique(artist)) -> test
  
  polls %>% spread(critic_name, artist)-> test
  
  polls %>% spread(artist, title)
  
  polls %>% pivot_wider(names_from = rank, values_from = artist, names_prefix = "position_")

  
  polls %>%
    group_by(critic_name) %>%
    summarize("all_list" = unique(artist))
  
  polls %>%
    group_by(artist) %>%
    summarize("all_list" = unique(critic_name)) %>% ungroup() -> test
critics =   unique(polls$critic_name)

which(critics == "Lawrence Burney")

polls %>%
  mutate('critic_id' = which(critics==critic_name)) -> polls2

polls$critic_id = -1


for(i in 1:nrow(polls)){
  c = polls[i, 'critic_name'][[1]]
  id = which(critics == c)
  cat(i, c, id, '\n', sep=" : ")
  polls[i, 'critic_id'] = id
}


artists = sort(unique(ranking$Main))
ranks = vector(mode = "integer", length=length(artists))

for(i in 1:length(artists)) {
  a = artists[i]
  result <- polls[grep(a, polls$artist), 'critic_id']
  ranks[i] = nrow(unique(result))
}

list_appearances = data.frame(artists, ranks)

top_rappers$total_appearances = list_appearances[list_appearances$artists==top_rappers$Main, 'ranks']

top_rappers[grep('Too \\$hort', top_rappers$Main), 'total_appearances'] <- 1
write_csv(ranking2, path="ranking2.csv", col_names = T)

write_csv(top_rappers, path ="top_rappers.csv", col_names =T)

mutate_all(top_rappers, funs(dense_rank(desc(.))))
