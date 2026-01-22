library(tidyverse)
install.packages("jsonlite")
library(jsonlite)
library(purrr)



# Competitions laden
competitions <- fromJSON(
  "https://raw.githubusercontent.com/statsbomb/open-data/master/data/competitions.json",
  flatten = TRUE
) |> as_tibble()

competitions |> 
  select(competition_id, season_id, competition_name, season_name) |> 
  head(15)


# Funktion: Matches einer Competition + Season
get_matches_one <- function(competition_id, season_id) {
  url <- paste0(
    "https://raw.githubusercontent.com/statsbomb/open-data/master/data/matches/",
    competition_id, "/", season_id, ".json"
  )
  fromJSON(url, flatten = TRUE) |> as_tibble()
}


# Bundesliga: letzte (bis zu) 10 Saisons wählen
bundes <- competitions |>
  filter(competition_name == "1. Bundesliga") |>
  mutate(
    start_year = readr::parse_number(season_name)
  ) |>
  arrange(desc(start_year)) |>
  slice_head(n = 10)

bundes


# Alle Matches dieser Bundesliga-Saisons holen
matches_all <- map2_dfr(
  bundes$competition_id,
  bundes$season_id,
  get_matches_one
)

match_ids <- matches_all$match_id
length(match_ids)

# Funktion: Events eines Matches
get_events_one <- function(match_id) {
  url <- paste0(
    "https://raw.githubusercontent.com/statsbomb/open-data/master/data/events/",
    match_id, ".json"
  )
  fromJSON(url, flatten = TRUE) |> as_tibble()
}


# Alle Events aus allen Matches holen
events_all <- map_dfr(match_ids, get_events_one)
nrow(events_all)


# Alle Schüsse filtern
shots_all <- events_all |>
  filter(type.name == "Shot")

nrow(shots_all)

# Shot-Koordinaten + Zielvariable
shots_all <- shots_all |>
  mutate(
    x = map_dbl(location, 1),
    y = map_dbl(location, 2),
    goal = (shot.outcome.name == "Goal")
  )

mean(shots_all$goal)


# Tor-Mitte bei (120, 40) im StatsBomb-Koordinatensystem
# Winkel zwischen den beiden Pfosten (y = 36 und y = 44)
shots_all <- shots_all |>
  mutate(
    dx = 120 - x,
    dy = 40 - y,
    distance = sqrt(dx^2 + dy^2),
    angle = abs(
      atan2(36 - y, 120 - x) -
      atan2(44 - y, 120 - x)
    )
  )

summary(shots_all$distance)
summary(shots_all$angle)
