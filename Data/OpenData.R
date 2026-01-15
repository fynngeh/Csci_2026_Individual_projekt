library(tidyverse)
install.packages("jsonlite")
library(jsonlite)

competitions <- fromJSON(
  "https://raw.githubusercontent.com/statsbomb/open-data/master/data/competitions.json"
)

competitions |>
  select(competition_id, season_id, competition_name, season_name) |>
  head(10)

comp_seasons <- competitions |>
  select(competition_id, season_id) |>
  distinct()

comp_seasons

get_matches_one <- function(competition_id, season_id) {

  url <- paste0(
    "https://raw.githubusercontent.com/statsbomb/open-data/master/data/matches/",
    competition_id, "/", season_id, ".json"
  )
  fromJSON(url) |> as_tibble()
}

m <- get_matches_one(9, 281)
match_ids <- m$match_id


get_events_one <- function(match_id) {

  url <- paste0(
    "https://raw.githubusercontent.com/statsbomb/open-data/master/data/events/",
    match_id, ".json"
  )

  fromJSON(url) |> as_tibble()
}

ev <- get_events_one(match_ids[1])

nrow(ev)

shots <- ev |> filter(type$name == "Shot")
nrow(shots)


