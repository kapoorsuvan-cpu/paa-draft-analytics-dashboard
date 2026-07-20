suppressPackageStartupMessages({
  library(cfbfastR)
  library(nflreadr)
  library(tidyverse)
})
source("R/config.R")
source("R/utils.R")

if (Sys.getenv("CFBD_API_KEY") == "") {
  stop("CFBD_API_KEY is not set. Export it before running the pipeline.")
}

pull_years <- sort(unique(c(
  HISTORICAL_JUNIOR_YEARS - 1L,
  HISTORICAL_JUNIOR_YEARS,
  CURRENT_STAT_YEAR - 1L,
  CURRENT_STAT_YEAR
)))

stats <- map_dfr(pull_years, function(yr) {
  message("Player stats: ", yr)
  tryCatch(
    cfbd_stats_season_player(year = yr) %>% mutate(season = yr),
    error = function(e) {
      warning("Stats year ", yr, " skipped: ", conditionMessage(e))
      tibble()
    }
  )
}) %>% ensure_wide_player_stats()

roster_years <- sort(unique(c(HISTORICAL_JUNIOR_YEARS, CURRENT_STAT_YEAR)))
message("Loading full roster seasons: ", paste(roster_years, collapse = ", "))

rosters_raw <- map_dfr(roster_years, function(yr) {
  message("Roster athletes: ", yr)
  out <- tryCatch(
    load_cfb_rosters(seasons = yr),
    error = function(e) {
      warning("Roster year ", yr, " skipped: ", conditionMessage(e))
      tibble()
    }
  )
  if (!nrow(out)) return(tibble())
  out %>% mutate(season = as.integer(yr), .before = 1)
})

first_name <- first_existing(rosters_raw, c("first_name", "firstName"))
last_name <- first_existing(rosters_raw, c("last_name", "lastName"))
parts_name <- str_squish(paste(first_name, last_name))
existing_name <- first_existing(
  rosters_raw,
  c("player", "name", "full_name", "athlete_name")
)
player_name <- if_else(
  !is.na(existing_name) & existing_name != "",
  existing_name, parts_name
)

rosters <- tibble(
  season = suppressWarnings(as.integer(first_existing(rosters_raw, "season"))),
  player = player_name,
  team = first_existing(rosters_raw, c("team", "school")),
  raw_position = first_existing(rosters_raw, "position"),
  raw_class = first_existing(rosters_raw, c("year", "class", "classification")),
  athlete_id = first_existing(rosters_raw, c("athlete_id", "id")),
  height_raw = first_existing(rosters_raw, c("height", "height_inches")),
  weight_raw = first_existing(rosters_raw, c("weight", "weight_lbs")),
  headshot_url = first_existing(rosters_raw, c("headshot_url", "headshot"))
) %>%
  mutate(
    player_clean = clean_name(player),
    school_clean = clean_school(team),
    position = standardize_position(raw_position),
    class = normalize_class(raw_class),
    height_inches = parse_height_inches(height_raw),
    weight_lbs = suppressWarnings(as.numeric(weight_raw)),
    bmi_proxy = safe_divide(weight_lbs * 703, height_inches ^ 2)
  ) %>%
  filter(
    !is.na(season),
    !is.na(player_clean), player_clean != "",
    !is.na(school_clean), school_clean != ""
  ) %>%
  distinct(season, athlete_id, player_clean, school_clean, .keep_all = TRUE)

if (nrow(rosters) < 5000L) {
  stop(
    "Roster pull returned only ", nrow(rosters),
    " player rows. Columns: ", paste(names(rosters_raw), collapse = ", ")
  )
}
if (sum(rosters$class == "JR", na.rm = TRUE) == 0L) {
  stop("Roster pull has no recognized juniors.")
}

message("Roster rows retained: ", nrow(rosters))
message("Junior roster rows: ", sum(rosters$class == "JR", na.rm = TRUE))

recruit_years <- (min(HISTORICAL_JUNIOR_YEARS) - 5L):CURRENT_STAT_YEAR
recruiting <- map_dfr(recruit_years, function(yr) {
  message("Recruiting: ", yr)
  tryCatch(
    cfbd_recruiting_player(
      year = yr,
      recruit_type = "HighSchool"
    ) %>% mutate(recruit_year = yr),
    error = function(e) {
      warning("Recruiting year ", yr, " skipped: ", conditionMessage(e))
      tibble()
    }
  )
}) %>%
  transmute(
    player_clean = clean_name(name),
    committed_school_clean = clean_school(committed_to),
    recruit_year = as.integer(recruit_year),
    recruit_position = standardize_position(position),
    stars = as.numeric(stars),
    recruit_rank = as.numeric(ranking),
    recruit_rating = as.numeric(rating),
    blue_chip = as.integer(stars >= 4)
  ) %>%
  arrange(player_clean, committed_school_clean, desc(recruit_rating)) %>%
  distinct(player_clean, committed_school_clean, .keep_all = TRUE)

draft_picks <- load_draft_picks(
  seasons = (min(HISTORICAL_JUNIOR_YEARS) + 1L):
    (max(HISTORICAL_JUNIOR_YEARS) + DRAFT_LOOKAHEAD_YEARS)
) %>%
  filter(!is.na(pick)) %>%
  transmute(
    player_clean = clean_name(pfr_player_name),
    draft_year = as.integer(season),
    draft_pick = as.integer(pick),
    draft_round = as.integer(round),
    nfl_position = standardize_position(position),
    college_clean = clean_school(college)
  )

manual_path <- file.path(PROJECT_ROOT, "manual_draft_matches.csv")
if (!file.exists(manual_path)) {
  write_csv(
    tibble(
      player_clean = character(),
      junior_year = integer(),
      draft_year = integer(),
      draft_pick = integer(),
      draft_round = integer()
    ),
    manual_path
  )
}

write_rds(stats, file.path(RAW_DIR, "player_stats.rds"))
write_rds(rosters, file.path(RAW_DIR, "rosters.rds"))
write_rds(recruiting, file.path(RAW_DIR, "recruiting.rds"))
write_rds(draft_picks, file.path(RAW_DIR, "draft_picks.rds"))
