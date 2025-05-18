library(readxl)
library(dplyr)
library(stringr)
library(vtable)
library(ggplot2)
full_season <- read_excel("C:/Users/barry/full_season_stats_wsl2020_21.xlsx")

attach(full_season)
str(full_season)
colSums(is.na(full_season)) # 1 missing value in referee column
full_season %>% filter(is.na(referee)) # Match 115 - Birmingham City WFC vs. Reading WFC (Referee = Lucy Oliver)
full_season$referee[116] <- "Lucy Oliver"

full_season %>% select(where(is.numeric)) %>% ncol()
full_season %>% select(where(is.character)) %>% ncol()

# List unique teams in the league
unique(full_season$home_team)

# Season Aggregate Stats

# Most Common Result
common_results <- full_season %>% group_by(`winning team`) %>% summarise(n = n()) %>% arrange(desc(n))
par(mar = c(7,4,3,4), mgp = c(5,1,0)) # Change margins and axis line height to fit the data
# Generate the barplot with customised/shortenes team names
barplot(common_results$n, names.arg = c("Draw", "Chelsea", "Man City", "Arsenal", "Man Utd", "Everton", "Brighton", "Reading", "Tottenham", "Aston Villa", "Birmingham", "West Ham", "Bristol City"), las = 2, ylim = c(0,30), cex.names = 0.9, main = "Most Common Results: 2020/21 WSL Season", ylab = "No. of Wins", xlab = "Winning Team")
par(mar = c(5.1, 4.1, 4.1, 2.1), mgp = c(3,1,0)) # reset par to default values

# Subset the dataframe to filter for totals columns
season_totals <- full_season %>% mutate(total_goals = home_score + away_score) %>% 
  select(total_goals, total_passes, total_touches, total_clearances, total_corners, total_fouls, total_tackles, total_yellow_cards, total_red_cards)

# Create a summary table for all statistics
sumtable(season_totals, labels = c("Goals", "Passes Complete", "Touches", "Clearances", "Corners", "Fouls", "Tackles", "Yellow Cards", "Red Cards"), summ = c('mean(x)','sd(x)','min(x)','max(x)', 'sum(x)'), summ.names = c('Mean','SD','Min','Max', 'Total'), title = "Season Statistics")


# Goal Distribution per 10 minute intervals
# Strip the non-numeric characters
goal_times <- str_replace_all(all_goal_times , "\\[|\\]", "")  # https://stackoverflow.com/a/40595850

# Filter out any empty rows
goal_times <- goal_times[goal_times != ""]

# Combine all the number lists into one list of numeric values
goal_times <- as.data.frame(goal_times)
goal_times <- as.numeric(unlist(strsplit(goal_times$goal_times, ",\\s*")))    # https://stackoverflow.com/a/39279181


# Plot distribution of times that a yellow card was issued

# Strip the non-numeric characters
yellow_times <- str_replace_all(all_yellow_times, "\\[|\\]", "")  # https://stackoverflow.com/a/40595850

# Filter out any empty rows
yellow_times <- yellow_times[yellow_times != ""]

# Combine all the number lists into one list of numeric values
yellow_times <- as.data.frame(yellow_times)
yellow_times <- as.numeric(unlist(strsplit(yellow_times$yellow_times, ",\\s*")))    # https://stackoverflow.com/a/39279181
par(mfrow = c(2,1))
hist(goal_times, main = "Distribution of Goals - Entire Season")
hist(yellow_times, main = "Distribution of Yellow Cards - Entire Season")

# Define a function to extract and clean goal times
goal_times_extract <- function(df){
  times <- str_replace_all(df$goal_times, "\\[|\\]", "")
  times <- times[times != ""]
  times <- as.data.frame(times)
  times <- as.numeric(unlist(strsplit(times$times, ",\\s*")))
  return(times)
}
# Define a function to extract and clean yellow card times
yellow_times_extract <- function(df){
  times <- str_replace_all(df$yellow_times, "\\[|\\]", "") # Replace square brackets with empty string
  times <- times[times != ""]
  times <- as.data.frame(times)
  times <- as.numeric(unlist(strsplit(times$times, ",\\s*"))) # Split at the space
  return(times)
}

################################################################################
# Bivariate Descriptive Statistics

# Grouped by matchweek

stats_per_week <- full_season %>% mutate(total_goals = home_score + away_score) %>%
  group_by(match_week) %>%
  summarise(total_goals = sum(total_goals), total_passes = sum(total_passes), total_touches = sum(total_touches), total_clearances = sum(total_clearances), total_corners = sum(total_corners), total_fouls = sum(total_fouls), total_tackles = sum(total_tackles), total_yellow_cards = sum(total_yellow_cards), total_red_cards = sum(total_red_cards))

print.data.frame(stats_per_week)

par(mfrow = c(4,2), mar = c(2,2,2,2))
# Goal tallies per week
plot(stats_per_week$match_week, stats_per_week$total_goals, type = 'l', main = "Goals per Matchweek") 
# Pass totals per week
plot(stats_per_week$match_week, stats_per_week$total_passes, type = 'l', main = "Passes per Matchweek") 
# Total touches per week
plot(stats_per_week$match_week, stats_per_week$total_touches, type = 'l', main = "Touches per Matchweek") 
# Total Clearances per week
plot(stats_per_week$match_week, stats_per_week$total_clearances, type = 'l', main = "Clearances per Matchweek") 
# Total corners per week
plot(stats_per_week$match_week, stats_per_week$total_corners, type = 'l', main = "Corners per Matchweek") 
# Total fouls per week
plot(stats_per_week$match_week, stats_per_week$total_fouls, type = 'l', main = "Fouls per Matchweek") 
# Total tackles per week
plot(stats_per_week$match_week, stats_per_week$total_tackles, type = 'l', main = "Tackles per Matchweek") 
# Yellow cards per week
plot(stats_per_week$match_week, stats_per_week$total_yellow_cards, type = 'l', main = "Yellow Cards per Matchweek") 

# Grouped by team
# Show a list of all teams 
unique(full_season$home_team)
################################################################################
# Extract away games and home games for each team and combine into one dataframe

# Aston Villa

aston_villa_away <- full_season %>% filter(away_team == "Aston Villa") %>% 
  select("match_id", "match_date", "kick_off", "stadium", "match_week", "away_score", "referee", "away_managers", "winning team", "match_result", "half_time_score_away", "away_goal_times", "away_clearances", "away_shots", "away_shot_on_target", "away_possession", "away_corners", "away_fouls_committed", "away_offside", "away_passes", "away_passes_into_box", "away_passes_short", "away_passes_medium", "away_passes_long", "away_average_pass_length", "away_tackles", "away_yellow_cards", "away_red_cards", "away_yellow_times", "away_red_times") %>% 
  rename(score = away_score, manager = away_managers, half_time_score = half_time_score_away, goal_times = away_goal_times, clearances = away_clearances, shots = away_shots, shot_on_target = away_shot_on_target, possession = away_possession, corners = away_corners, fouls_committed = away_fouls_committed, offside = away_offside, passes = away_passes, passes_into_box = away_passes_into_box, short_passes = away_passes_short, medium_passes = away_passes_medium, long_passes = away_passes_long, average_pass_length = away_average_pass_length, tackles = away_tackles, yellow_cards = away_yellow_cards, red_cards = away_red_cards, yellow_times = away_yellow_times, red_times = away_red_times)

aston_villa_home <- full_season %>% filter(home_team == "Aston Villa") %>% 
  select("match_id", "match_date", "kick_off", "stadium", "match_week", "home_score", "referee", "home_managers", "winning team", "match_result", "half_time_score_home", "home_goal_times", "home_clearances", "home_shots", "home_shot_on_target", "home_possession", "home_corners", "home_fouls_committed", "home_offsides", "home_passes", "home_passes_into_box", "home_passes_short", "home_passes_medium", "home_passes_long", "home_average_pass_length", "home_tackles", "home_yellow_cards", "home_red_cards", "home_yellow_times", "home_red_times") %>% 
  rename(score = home_score, manager = home_managers, half_time_score = half_time_score_home, goal_times = home_goal_times, clearances = home_clearances, shots = home_shots, shot_on_target = home_shot_on_target, possession = home_possession, corners = home_corners, fouls_committed = home_fouls_committed, offside = home_offsides, passes = home_passes, passes_into_box = home_passes_into_box, short_passes = home_passes_short, medium_passes = home_passes_medium, long_passes = home_passes_long, average_pass_length = home_average_pass_length, tackles = home_tackles, yellow_cards = home_yellow_cards, red_cards = home_red_cards, yellow_times = home_yellow_times, red_times = home_red_times)


aston_villa <- rbind(aston_villa_home, aston_villa_away) %>% arrange(match_week)

# Extract aggregate stats
aston_villa_stats <- aston_villa %>% summarise(total_goals = sum(score), avg_goals = round(mean(score), 2), avg_clearance = round(mean(clearances),2), avg_shots = round(mean(shots),2), avg_sot = round(mean(shot_on_target),2), avg_possession = round(mean(possession),2), avg_fouls = round(mean(fouls_committed),2), avg_passes_complete = round(mean(passes),2), avg_pass_length = round(mean(average_pass_length),2), avg_tackles = round(mean(tackles),2), avg_yellow_cards = round(mean(yellow_cards),2), total_red_cards = sum(red_cards))
###############################################################################

#"Brighton & Hove Albion WFC"

bha_away <- full_season %>% filter(away_team == "Brighton & Hove Albion WFC") %>% 
  select("match_id", "match_date", "kick_off", "stadium", "match_week", "away_score", "referee", "away_managers", "winning team", "match_result", "half_time_score_away", "away_goal_times", "away_clearances", "away_shots", "away_shot_on_target", "away_possession", "away_corners", "away_fouls_committed", "away_offside", "away_passes", "away_passes_into_box", "away_passes_short", "away_passes_medium", "away_passes_long", "away_average_pass_length", "away_tackles", "away_yellow_cards", "away_red_cards", "away_yellow_times", "away_red_times") %>% 
  rename(score = away_score, manager = away_managers, half_time_score = half_time_score_away, goal_times = away_goal_times, clearances = away_clearances, shots = away_shots, shot_on_target = away_shot_on_target, possession = away_possession, corners = away_corners, fouls_committed = away_fouls_committed, offside = away_offside, passes = away_passes, passes_into_box = away_passes_into_box, short_passes = away_passes_short, medium_passes = away_passes_medium, long_passes = away_passes_long, average_pass_length = away_average_pass_length, tackles = away_tackles, yellow_cards = away_yellow_cards, red_cards = away_red_cards, yellow_times = away_yellow_times, red_times = away_red_times)

bha_home <- full_season %>% filter(home_team == "Brighton & Hove Albion WFC") %>% 
  select("match_id", "match_date", "kick_off", "stadium", "match_week", "home_score", "referee", "home_managers", "winning team", "match_result", "half_time_score_home", "home_goal_times", "home_clearances", "home_shots", "home_shot_on_target", "home_possession", "home_corners", "home_fouls_committed", "home_offsides", "home_passes", "home_passes_into_box", "home_passes_short", "home_passes_medium", "home_passes_long", "home_average_pass_length", "home_tackles", "home_yellow_cards", "home_red_cards", "home_yellow_times", "home_red_times") %>% 
  rename(score = home_score, manager = home_managers, half_time_score = half_time_score_home, goal_times = home_goal_times, clearances = home_clearances, shots = home_shots, shot_on_target = home_shot_on_target, possession = home_possession, corners = home_corners, fouls_committed = home_fouls_committed, offside = home_offsides, passes = home_passes, passes_into_box = home_passes_into_box, short_passes = home_passes_short, medium_passes = home_passes_medium, long_passes = home_passes_long, average_pass_length = home_average_pass_length, tackles = home_tackles, yellow_cards = home_yellow_cards, red_cards = home_red_cards, yellow_times = home_yellow_times, red_times = home_red_times)

bha <- rbind(bha_home, bha_away) %>% arrange(match_week)

# Extract aggregate stats
bha_stats <- bha %>% summarise(total_goals = sum(score), avg_goals = round(mean(score), 2), avg_clearance = round(mean(clearances),2), avg_shots = round(mean(shots),2), avg_sot = round(mean(shot_on_target),2), avg_possession = round(mean(possession),2), avg_fouls = round(mean(fouls_committed),2), avg_passes_complete = round(mean(passes),2), avg_pass_length = round(mean(average_pass_length),2), avg_tackles = round(mean(tackles),2), avg_yellow_cards = round(mean(yellow_cards),2), total_red_cards = sum(red_cards))
###############################################################################

# "Tottenham Hotspur Women"
tottenham_away <- full_season %>% filter(away_team == "Tottenham Hotspur Women") %>% 
  select("match_id", "match_date", "kick_off", "stadium", "match_week", "away_score", "referee", "away_managers", "winning team", "match_result", "half_time_score_away", "away_goal_times", "away_clearances", "away_shots", "away_shot_on_target", "away_possession", "away_corners", "away_fouls_committed", "away_offside", "away_passes", "away_passes_into_box", "away_passes_short", "away_passes_medium", "away_passes_long", "away_average_pass_length", "away_tackles", "away_yellow_cards", "away_red_cards", "away_yellow_times", "away_red_times") %>% 
  rename(score = away_score, manager = away_managers, half_time_score = half_time_score_away, goal_times = away_goal_times, clearances = away_clearances, shots = away_shots, shot_on_target = away_shot_on_target, possession = away_possession, corners = away_corners, fouls_committed = away_fouls_committed, offside = away_offside, passes = away_passes, passes_into_box = away_passes_into_box, short_passes = away_passes_short, medium_passes = away_passes_medium, long_passes = away_passes_long, average_pass_length = away_average_pass_length, tackles = away_tackles, yellow_cards = away_yellow_cards, red_cards = away_red_cards, yellow_times = away_yellow_times, red_times = away_red_times)

tottenham_home <- full_season %>% filter(home_team == "Tottenham Hotspur Women") %>% 
  select("match_id", "match_date", "kick_off", "stadium", "match_week", "home_score", "referee", "home_managers", "winning team", "match_result", "half_time_score_home", "home_goal_times", "home_clearances", "home_shots", "home_shot_on_target", "home_possession", "home_corners", "home_fouls_committed", "home_offsides", "home_passes", "home_passes_into_box", "home_passes_short", "home_passes_medium", "home_passes_long", "home_average_pass_length", "home_tackles", "home_yellow_cards", "home_red_cards", "home_yellow_times", "home_red_times") %>% 
  rename(score = home_score, manager = home_managers, half_time_score = half_time_score_home, goal_times = home_goal_times, clearances = home_clearances, shots = home_shots, shot_on_target = home_shot_on_target, possession = home_possession, corners = home_corners, fouls_committed = home_fouls_committed, offside = home_offsides, passes = home_passes, passes_into_box = home_passes_into_box, short_passes = home_passes_short, medium_passes = home_passes_medium, long_passes = home_passes_long, average_pass_length = home_average_pass_length, tackles = home_tackles, yellow_cards = home_yellow_cards, red_cards = home_red_cards, yellow_times = home_yellow_times, red_times = home_red_times)

tottenham <- rbind(tottenham_home, tottenham_away) %>% arrange(match_week)

# Extract aggregate stats
tottenham_stats <- tottenham %>% summarise(total_goals = sum(score), avg_goals = round(mean(score), 2), avg_clearance = round(mean(clearances),2), avg_shots = round(mean(shots),2), avg_sot = round(mean(shot_on_target),2), avg_possession = round(mean(possession),2), avg_fouls = round(mean(fouls_committed),2), avg_passes_complete = round(mean(passes),2), avg_pass_length = round(mean(average_pass_length),2), avg_tackles = round(mean(tackles),2), avg_yellow_cards = round(mean(yellow_cards),2), total_red_cards = sum(red_cards))
###############################################################################

# "Manchester United"
manutd_away <- full_season %>% filter(away_team == "Manchester United") %>% 
  select("match_id", "match_date", "kick_off", "stadium", "match_week", "away_score", "referee", "away_managers", "winning team", "match_result", "half_time_score_away", "away_goal_times", "away_clearances", "away_shots", "away_shot_on_target", "away_possession", "away_corners", "away_fouls_committed", "away_offside", "away_passes", "away_passes_into_box", "away_passes_short", "away_passes_medium", "away_passes_long", "away_average_pass_length", "away_tackles", "away_yellow_cards", "away_red_cards", "away_yellow_times", "away_red_times") %>% 
  rename(score = away_score, manager = away_managers, half_time_score = half_time_score_away, goal_times = away_goal_times, clearances = away_clearances, shots = away_shots, shot_on_target = away_shot_on_target, possession = away_possession, corners = away_corners, fouls_committed = away_fouls_committed, offside = away_offside, passes = away_passes, passes_into_box = away_passes_into_box, short_passes = away_passes_short, medium_passes = away_passes_medium, long_passes = away_passes_long, average_pass_length = away_average_pass_length, tackles = away_tackles, yellow_cards = away_yellow_cards, red_cards = away_red_cards, yellow_times = away_yellow_times, red_times = away_red_times)

manutd_home <- full_season %>% filter(home_team == "Manchester United") %>% 
  select("match_id", "match_date", "kick_off", "stadium", "match_week", "home_score", "referee", "home_managers", "winning team", "match_result", "half_time_score_home", "home_goal_times", "home_clearances", "home_shots", "home_shot_on_target", "home_possession", "home_corners", "home_fouls_committed", "home_offsides", "home_passes", "home_passes_into_box", "home_passes_short", "home_passes_medium", "home_passes_long", "home_average_pass_length", "home_tackles", "home_yellow_cards", "home_red_cards", "home_yellow_times", "home_red_times") %>% 
  rename(score = home_score, manager = home_managers, half_time_score = half_time_score_home, goal_times = home_goal_times, clearances = home_clearances, shots = home_shots, shot_on_target = home_shot_on_target, possession = home_possession, corners = home_corners, fouls_committed = home_fouls_committed, offside = home_offsides, passes = home_passes, passes_into_box = home_passes_into_box, short_passes = home_passes_short, medium_passes = home_passes_medium, long_passes = home_passes_long, average_pass_length = home_average_pass_length, tackles = home_tackles, yellow_cards = home_yellow_cards, red_cards = home_red_cards, yellow_times = home_yellow_times, red_times = home_red_times)

manutd <- rbind(manutd_home, manutd_away) %>% arrange(match_week)

# Extract aggregate stats
manutd_stats <- manutd %>% summarise(total_goals = sum(score), avg_goals = round(mean(score), 2), avg_clearance = round(mean(clearances),2), avg_shots = round(mean(shots),2), avg_sot = round(mean(shot_on_target),2), avg_possession = round(mean(possession),2), avg_fouls = round(mean(fouls_committed),2), avg_passes_complete = round(mean(passes),2), avg_pass_length = round(mean(average_pass_length),2), avg_tackles = round(mean(tackles),2), avg_yellow_cards = round(mean(yellow_cards),2), total_red_cards = sum(red_cards))
#################################################################################

# "Arsenal WFC"
arsenal_away <- full_season %>% filter(away_team == "Arsenal WFC") %>% 
  select("match_id", "match_date", "kick_off", "stadium", "match_week", "away_score", "referee", "away_managers", "winning team", "match_result", "half_time_score_away", "away_goal_times", "away_clearances", "away_shots", "away_shot_on_target", "away_possession", "away_corners", "away_fouls_committed", "away_offside", "away_passes", "away_passes_into_box", "away_passes_short", "away_passes_medium", "away_passes_long", "away_average_pass_length", "away_tackles", "away_yellow_cards", "away_red_cards", "away_yellow_times", "away_red_times") %>% 
  rename(score = away_score, manager = away_managers, half_time_score = half_time_score_away, goal_times = away_goal_times, clearances = away_clearances, shots = away_shots, shot_on_target = away_shot_on_target, possession = away_possession, corners = away_corners, fouls_committed = away_fouls_committed, offside = away_offside, passes = away_passes, passes_into_box = away_passes_into_box, short_passes = away_passes_short, medium_passes = away_passes_medium, long_passes = away_passes_long, average_pass_length = away_average_pass_length, tackles = away_tackles, yellow_cards = away_yellow_cards, red_cards = away_red_cards, yellow_times = away_yellow_times, red_times = away_red_times)

arsenal_home <- full_season %>% filter(home_team == "Arsenal WFC") %>% 
  select("match_id", "match_date", "kick_off", "stadium", "match_week", "home_score", "referee", "home_managers", "winning team", "match_result", "half_time_score_home", "home_goal_times", "home_clearances", "home_shots", "home_shot_on_target", "home_possession", "home_corners", "home_fouls_committed", "home_offsides", "home_passes", "home_passes_into_box", "home_passes_short", "home_passes_medium", "home_passes_long", "home_average_pass_length", "home_tackles", "home_yellow_cards", "home_red_cards", "home_yellow_times", "home_red_times") %>% 
  rename(score = home_score, manager = home_managers, half_time_score = half_time_score_home, goal_times = home_goal_times, clearances = home_clearances, shots = home_shots, shot_on_target = home_shot_on_target, possession = home_possession, corners = home_corners, fouls_committed = home_fouls_committed, offside = home_offsides, passes = home_passes, passes_into_box = home_passes_into_box, short_passes = home_passes_short, medium_passes = home_passes_medium, long_passes = home_passes_long, average_pass_length = home_average_pass_length, tackles = home_tackles, yellow_cards = home_yellow_cards, red_cards = home_red_cards, yellow_times = home_yellow_times, red_times = home_red_times)

arsenal <- rbind(arsenal_home, arsenal_away) %>% arrange(match_week)

# Extract aggregate stats
arsenal_stats <-arsenal %>% summarise(total_goals = sum(score), avg_goals = round(mean(score), 2), avg_clearance = round(mean(clearances),2), avg_shots = round(mean(shots),2), avg_sot = round(mean(shot_on_target),2), avg_possession = round(mean(possession),2), avg_fouls = round(mean(fouls_committed),2), avg_passes_complete = round(mean(passes),2), avg_pass_length = round(mean(average_pass_length),2), avg_tackles = round(mean(tackles),2), avg_yellow_cards = round(mean(yellow_cards),2), total_red_cards = sum(red_cards))
#################################################################################

# "Bristol City WFC"
bristol_away <- full_season %>% filter(away_team == "Bristol City WFC") %>% 
  select("match_id", "match_date", "kick_off", "stadium", "match_week", "away_score", "referee", "away_managers", "winning team", "match_result", "half_time_score_away", "away_goal_times", "away_clearances", "away_shots", "away_shot_on_target", "away_possession", "away_corners", "away_fouls_committed", "away_offside", "away_passes", "away_passes_into_box", "away_passes_short", "away_passes_medium", "away_passes_long", "away_average_pass_length", "away_tackles", "away_yellow_cards", "away_red_cards", "away_yellow_times", "away_red_times") %>% 
  rename(score = away_score, manager = away_managers, half_time_score = half_time_score_away, goal_times = away_goal_times, clearances = away_clearances, shots = away_shots, shot_on_target = away_shot_on_target, possession = away_possession, corners = away_corners, fouls_committed = away_fouls_committed, offside = away_offside, passes = away_passes, passes_into_box = away_passes_into_box, short_passes = away_passes_short, medium_passes = away_passes_medium, long_passes = away_passes_long, average_pass_length = away_average_pass_length, tackles = away_tackles, yellow_cards = away_yellow_cards, red_cards = away_red_cards, yellow_times = away_yellow_times, red_times = away_red_times)

bristol_home <- full_season %>% filter(home_team == "Bristol City WFC") %>% 
  select("match_id", "match_date", "kick_off", "stadium", "match_week", "home_score", "referee", "home_managers", "winning team", "match_result", "half_time_score_home", "home_goal_times", "home_clearances", "home_shots", "home_shot_on_target", "home_possession", "home_corners", "home_fouls_committed", "home_offsides", "home_passes", "home_passes_into_box", "home_passes_short", "home_passes_medium", "home_passes_long", "home_average_pass_length", "home_tackles", "home_yellow_cards", "home_red_cards", "home_yellow_times", "home_red_times") %>% 
  rename(score = home_score, manager = home_managers, half_time_score = half_time_score_home, goal_times = home_goal_times, clearances = home_clearances, shots = home_shots, shot_on_target = home_shot_on_target, possession = home_possession, corners = home_corners, fouls_committed = home_fouls_committed, offside = home_offsides, passes = home_passes, passes_into_box = home_passes_into_box, short_passes = home_passes_short, medium_passes = home_passes_medium, long_passes = home_passes_long, average_pass_length = home_average_pass_length, tackles = home_tackles, yellow_cards = home_yellow_cards, red_cards = home_red_cards, yellow_times = home_yellow_times, red_times = home_red_times)

bristol <- rbind(bristol_home, bristol_away) %>% arrange(match_week)

# Extract aggregate stats
bristol_stats <- bristol %>% summarise(total_goals = sum(score), avg_goals = round(mean(score), 2), avg_clearance = round(mean(clearances),2), avg_shots = round(mean(shots),2), avg_sot = round(mean(shot_on_target),2), avg_possession = round(mean(possession),2), avg_fouls = round(mean(fouls_committed),2), avg_passes_complete = round(mean(passes),2), avg_pass_length = round(mean(average_pass_length),2), avg_tackles = round(mean(tackles),2), avg_yellow_cards = round(mean(yellow_cards),2), total_red_cards = sum(red_cards))
#################################################################################

# "West Ham United LFC"     
westham_away <- full_season %>% filter(away_team == "West Ham United LFC") %>% 
  select("match_id", "match_date", "kick_off", "stadium", "match_week", "away_score", "referee", "away_managers", "winning team", "match_result", "half_time_score_away", "away_goal_times", "away_clearances", "away_shots", "away_shot_on_target", "away_possession", "away_corners", "away_fouls_committed", "away_offside", "away_passes", "away_passes_into_box", "away_passes_short", "away_passes_medium", "away_passes_long", "away_average_pass_length", "away_tackles", "away_yellow_cards", "away_red_cards", "away_yellow_times", "away_red_times") %>% 
  rename(score = away_score, manager = away_managers, half_time_score = half_time_score_away, goal_times = away_goal_times, clearances = away_clearances, shots = away_shots, shot_on_target = away_shot_on_target, possession = away_possession, corners = away_corners, fouls_committed = away_fouls_committed, offside = away_offside, passes = away_passes, passes_into_box = away_passes_into_box, short_passes = away_passes_short, medium_passes = away_passes_medium, long_passes = away_passes_long, average_pass_length = away_average_pass_length, tackles = away_tackles, yellow_cards = away_yellow_cards, red_cards = away_red_cards, yellow_times = away_yellow_times, red_times = away_red_times)

westham_home <- full_season %>% filter(home_team == "West Ham United LFC") %>% 
  select("match_id", "match_date", "kick_off", "stadium", "match_week", "home_score", "referee", "home_managers", "winning team", "match_result", "half_time_score_home", "home_goal_times", "home_clearances", "home_shots", "home_shot_on_target", "home_possession", "home_corners", "home_fouls_committed", "home_offsides", "home_passes", "home_passes_into_box", "home_passes_short", "home_passes_medium", "home_passes_long", "home_average_pass_length", "home_tackles", "home_yellow_cards", "home_red_cards", "home_yellow_times", "home_red_times") %>% 
  rename(score = home_score, manager = home_managers, half_time_score = half_time_score_home, goal_times = home_goal_times, clearances = home_clearances, shots = home_shots, shot_on_target = home_shot_on_target, possession = home_possession, corners = home_corners, fouls_committed = home_fouls_committed, offside = home_offsides, passes = home_passes, passes_into_box = home_passes_into_box, short_passes = home_passes_short, medium_passes = home_passes_medium, long_passes = home_passes_long, average_pass_length = home_average_pass_length, tackles = home_tackles, yellow_cards = home_yellow_cards, red_cards = home_red_cards, yellow_times = home_yellow_times, red_times = home_red_times)

westham <- rbind(westham_home, westham_away) %>% arrange(match_week)

# Extract aggregate stats
westham_stats <- westham %>% summarise(total_goals = sum(score), avg_goals = round(mean(score), 2), avg_clearance = round(mean(clearances),2), avg_shots = round(mean(shots),2), avg_sot = round(mean(shot_on_target),2), avg_possession = round(mean(possession),2), avg_fouls = round(mean(fouls_committed),2), avg_passes_complete = round(mean(passes),2), avg_pass_length = round(mean(average_pass_length),2), avg_tackles = round(mean(tackles),2), avg_yellow_cards = round(mean(yellow_cards),2), total_red_cards = sum(red_cards))
################################################################################

# "Reading WFC"               
reading_away <- full_season %>% filter(away_team == "Reading WFC") %>% 
  select("match_id", "match_date", "kick_off", "stadium", "match_week", "away_score", "referee", "away_managers", "winning team", "match_result", "half_time_score_away", "away_goal_times", "away_clearances", "away_shots", "away_shot_on_target", "away_possession", "away_corners", "away_fouls_committed", "away_offside", "away_passes", "away_passes_into_box", "away_passes_short", "away_passes_medium", "away_passes_long", "away_average_pass_length", "away_tackles", "away_yellow_cards", "away_red_cards", "away_yellow_times", "away_red_times") %>% 
  rename(score = away_score, manager = away_managers, half_time_score = half_time_score_away, goal_times = away_goal_times, clearances = away_clearances, shots = away_shots, shot_on_target = away_shot_on_target, possession = away_possession, corners = away_corners, fouls_committed = away_fouls_committed, offside = away_offside, passes = away_passes, passes_into_box = away_passes_into_box, short_passes = away_passes_short, medium_passes = away_passes_medium, long_passes = away_passes_long, average_pass_length = away_average_pass_length, tackles = away_tackles, yellow_cards = away_yellow_cards, red_cards = away_red_cards, yellow_times = away_yellow_times, red_times = away_red_times)

reading_home <- full_season %>% filter(home_team == "Reading WFC") %>% 
  select("match_id", "match_date", "kick_off", "stadium", "match_week", "home_score", "referee", "home_managers", "winning team", "match_result", "half_time_score_home", "home_goal_times", "home_clearances", "home_shots", "home_shot_on_target", "home_possession", "home_corners", "home_fouls_committed", "home_offsides", "home_passes", "home_passes_into_box", "home_passes_short", "home_passes_medium", "home_passes_long", "home_average_pass_length", "home_tackles", "home_yellow_cards", "home_red_cards", "home_yellow_times", "home_red_times") %>% 
  rename(score = home_score, manager = home_managers, half_time_score = half_time_score_home, goal_times = home_goal_times, clearances = home_clearances, shots = home_shots, shot_on_target = home_shot_on_target, possession = home_possession, corners = home_corners, fouls_committed = home_fouls_committed, offside = home_offsides, passes = home_passes, passes_into_box = home_passes_into_box, short_passes = home_passes_short, medium_passes = home_passes_medium, long_passes = home_passes_long, average_pass_length = home_average_pass_length, tackles = home_tackles, yellow_cards = home_yellow_cards, red_cards = home_red_cards, yellow_times = home_yellow_times, red_times = home_red_times)

reading <- rbind(reading_home, reading_away) %>% arrange(match_week)

# Extract aggregate stats
reading_stats <- reading %>% summarise(total_goals = sum(score), avg_goals = round(mean(score), 2), avg_clearance = round(mean(clearances),2), avg_shots = round(mean(shots),2), avg_sot = round(mean(shot_on_target),2), avg_possession = round(mean(possession),2), avg_fouls = round(mean(fouls_committed),2), avg_passes_complete = round(mean(passes),2), avg_pass_length = round(mean(average_pass_length),2), avg_tackles = round(mean(tackles),2), avg_yellow_cards = round(mean(yellow_cards),2), total_red_cards = sum(red_cards))
###############################################################################

# "Everton LFC"                
everton_away <- full_season %>% filter(away_team == "Everton LFC") %>% 
  select("match_id", "match_date", "kick_off", "stadium", "match_week", "away_score", "referee", "away_managers", "winning team", "match_result", "half_time_score_away", "away_goal_times", "away_clearances", "away_shots", "away_shot_on_target", "away_possession", "away_corners", "away_fouls_committed", "away_offside", "away_passes", "away_passes_into_box", "away_passes_short", "away_passes_medium", "away_passes_long", "away_average_pass_length", "away_tackles", "away_yellow_cards", "away_red_cards", "away_yellow_times", "away_red_times") %>% 
  rename(score = away_score, manager = away_managers, half_time_score = half_time_score_away, goal_times = away_goal_times, clearances = away_clearances, shots = away_shots, shot_on_target = away_shot_on_target, possession = away_possession, corners = away_corners, fouls_committed = away_fouls_committed, offside = away_offside, passes = away_passes, passes_into_box = away_passes_into_box, short_passes = away_passes_short, medium_passes = away_passes_medium, long_passes = away_passes_long, average_pass_length = away_average_pass_length, tackles = away_tackles, yellow_cards = away_yellow_cards, red_cards = away_red_cards, yellow_times = away_yellow_times, red_times = away_red_times)

everton_home <- full_season %>% filter(home_team == "Everton LFC") %>% 
  select("match_id", "match_date", "kick_off", "stadium", "match_week", "home_score", "referee", "home_managers", "winning team", "match_result", "half_time_score_home", "home_goal_times", "home_clearances", "home_shots", "home_shot_on_target", "home_possession", "home_corners", "home_fouls_committed", "home_offsides", "home_passes", "home_passes_into_box", "home_passes_short", "home_passes_medium", "home_passes_long", "home_average_pass_length", "home_tackles", "home_yellow_cards", "home_red_cards", "home_yellow_times", "home_red_times") %>% 
  rename(score = home_score, manager = home_managers, half_time_score = half_time_score_home, goal_times = home_goal_times, clearances = home_clearances, shots = home_shots, shot_on_target = home_shot_on_target, possession = home_possession, corners = home_corners, fouls_committed = home_fouls_committed, offside = home_offsides, passes = home_passes, passes_into_box = home_passes_into_box, short_passes = home_passes_short, medium_passes = home_passes_medium, long_passes = home_passes_long, average_pass_length = home_average_pass_length, tackles = home_tackles, yellow_cards = home_yellow_cards, red_cards = home_red_cards, yellow_times = home_yellow_times, red_times = home_red_times)

everton <- rbind(everton_home, everton_away) %>% arrange(match_week)

# Extract aggregate stats
everton_stats <- everton %>% summarise(total_goals = sum(score), avg_goals = round(mean(score), 2), avg_clearance = round(mean(clearances),2), avg_shots = round(mean(shots),2), avg_sot = round(mean(shot_on_target),2), avg_possession = round(mean(possession),2), avg_fouls = round(mean(fouls_committed),2), avg_passes_complete = round(mean(passes),2), avg_pass_length = round(mean(average_pass_length),2), avg_tackles = round(mean(tackles),2), avg_yellow_cards = round(mean(yellow_cards),2), total_red_cards = sum(red_cards))
###############################################################################

# "Chelsea FCW"              
chelsea_away <- full_season %>% filter(away_team == "Chelsea FCW") %>% 
  select("match_id", "match_date", "kick_off", "stadium", "match_week", "away_score", "referee", "away_managers", "winning team", "match_result", "half_time_score_away", "away_goal_times", "away_clearances", "away_shots", "away_shot_on_target", "away_possession", "away_corners", "away_fouls_committed", "away_offside", "away_passes", "away_passes_into_box", "away_passes_short", "away_passes_medium", "away_passes_long", "away_average_pass_length", "away_tackles", "away_yellow_cards", "away_red_cards", "away_yellow_times", "away_red_times") %>% 
  rename(score = away_score, manager = away_managers, half_time_score = half_time_score_away, goal_times = away_goal_times, clearances = away_clearances, shots = away_shots, shot_on_target = away_shot_on_target, possession = away_possession, corners = away_corners, fouls_committed = away_fouls_committed, offside = away_offside, passes = away_passes, passes_into_box = away_passes_into_box, short_passes = away_passes_short, medium_passes = away_passes_medium, long_passes = away_passes_long, average_pass_length = away_average_pass_length, tackles = away_tackles, yellow_cards = away_yellow_cards, red_cards = away_red_cards, yellow_times = away_yellow_times, red_times = away_red_times)

chelsea_home <- full_season %>% filter(home_team == "Chelsea FCW") %>% 
  select("match_id", "match_date", "kick_off", "stadium", "match_week", "home_score", "referee", "home_managers", "winning team", "match_result", "half_time_score_home", "home_goal_times", "home_clearances", "home_shots", "home_shot_on_target", "home_possession", "home_corners", "home_fouls_committed", "home_offsides", "home_passes", "home_passes_into_box", "home_passes_short", "home_passes_medium", "home_passes_long", "home_average_pass_length", "home_tackles", "home_yellow_cards", "home_red_cards", "home_yellow_times", "home_red_times") %>% 
  rename(score = home_score, manager = home_managers, half_time_score = half_time_score_home, goal_times = home_goal_times, clearances = home_clearances, shots = home_shots, shot_on_target = home_shot_on_target, possession = home_possession, corners = home_corners, fouls_committed = home_fouls_committed, offside = home_offsides, passes = home_passes, passes_into_box = home_passes_into_box, short_passes = home_passes_short, medium_passes = home_passes_medium, long_passes = home_passes_long, average_pass_length = home_average_pass_length, tackles = home_tackles, yellow_cards = home_yellow_cards, red_cards = home_red_cards, yellow_times = home_yellow_times, red_times = home_red_times)

chelsea <- rbind(chelsea_home, chelsea_away) %>% arrange(match_week)

# Extract aggregate stats
chelsea_stats <- chelsea %>% summarise(total_goals = sum(score), avg_goals = round(mean(score), 2), avg_clearance = round(mean(clearances),2), avg_shots = round(mean(shots),2), avg_sot = round(mean(shot_on_target),2), avg_possession = round(mean(possession),2), avg_fouls = round(mean(fouls_committed),2), avg_passes_complete = round(mean(passes),2), avg_pass_length = round(mean(average_pass_length),2), avg_tackles = round(mean(tackles),2), avg_yellow_cards = round(mean(yellow_cards),2), total_red_cards = sum(red_cards))
###############################################################################

# "Manchester City WFC"        
mancity_away <- full_season %>% filter(away_team == "Manchester City WFC") %>% 
  select("match_id", "match_date", "kick_off", "stadium", "match_week", "away_score", "referee", "away_managers", "winning team", "match_result", "half_time_score_away", "away_goal_times", "away_clearances", "away_shots", "away_shot_on_target", "away_possession", "away_corners", "away_fouls_committed", "away_offside", "away_passes", "away_passes_into_box", "away_passes_short", "away_passes_medium", "away_passes_long", "away_average_pass_length", "away_tackles", "away_yellow_cards", "away_red_cards", "away_yellow_times", "away_red_times") %>% 
  rename(score = away_score, manager = away_managers, half_time_score = half_time_score_away, goal_times = away_goal_times, clearances = away_clearances, shots = away_shots, shot_on_target = away_shot_on_target, possession = away_possession, corners = away_corners, fouls_committed = away_fouls_committed, offside = away_offside, passes = away_passes, passes_into_box = away_passes_into_box, short_passes = away_passes_short, medium_passes = away_passes_medium, long_passes = away_passes_long, average_pass_length = away_average_pass_length, tackles = away_tackles, yellow_cards = away_yellow_cards, red_cards = away_red_cards, yellow_times = away_yellow_times, red_times = away_red_times)

mancity_home <- full_season %>% filter(home_team == "Manchester City WFC") %>% 
  select("match_id", "match_date", "kick_off", "stadium", "match_week", "home_score", "referee", "home_managers", "winning team", "match_result", "half_time_score_home", "home_goal_times", "home_clearances", "home_shots", "home_shot_on_target", "home_possession", "home_corners", "home_fouls_committed", "home_offsides", "home_passes", "home_passes_into_box", "home_passes_short", "home_passes_medium", "home_passes_long", "home_average_pass_length", "home_tackles", "home_yellow_cards", "home_red_cards", "home_yellow_times", "home_red_times") %>% 
  rename(score = home_score, manager = home_managers, half_time_score = half_time_score_home, goal_times = home_goal_times, clearances = home_clearances, shots = home_shots, shot_on_target = home_shot_on_target, possession = home_possession, corners = home_corners, fouls_committed = home_fouls_committed, offside = home_offsides, passes = home_passes, passes_into_box = home_passes_into_box, short_passes = home_passes_short, medium_passes = home_passes_medium, long_passes = home_passes_long, average_pass_length = home_average_pass_length, tackles = home_tackles, yellow_cards = home_yellow_cards, red_cards = home_red_cards, yellow_times = home_yellow_times, red_times = home_red_times)

mancity <- rbind(mancity_home, mancity_away) %>% arrange(match_week)

# Extract aggregate stats
mancity_stats <- mancity %>% summarise(total_goals = sum(score), avg_goals = round(mean(score), 2), avg_clearance = round(mean(clearances),2), avg_shots = round(mean(shots),2), avg_sot = round(mean(shot_on_target),2), avg_possession = round(mean(possession),2), avg_fouls = round(mean(fouls_committed),2), avg_passes_complete = round(mean(passes),2), avg_pass_length = round(mean(average_pass_length),2), avg_tackles = round(mean(tackles),2), avg_yellow_cards = round(mean(yellow_cards),2), total_red_cards = sum(red_cards))
###############################################################################
# "Birmingham City WFC"
birmingham_away <- full_season %>% filter(away_team == "Birmingham City WFC") %>% 
  select("match_id", "match_date", "kick_off", "stadium", "match_week", "away_score", "referee", "away_managers", "winning team", "match_result", "half_time_score_away", "away_goal_times", "away_clearances", "away_shots", "away_shot_on_target", "away_possession", "away_corners", "away_fouls_committed", "away_offside", "away_passes", "away_passes_into_box", "away_passes_short", "away_passes_medium", "away_passes_long", "away_average_pass_length", "away_tackles", "away_yellow_cards", "away_red_cards", "away_yellow_times", "away_red_times") %>% 
  rename(score = away_score, manager = away_managers, half_time_score = half_time_score_away, goal_times = away_goal_times, clearances = away_clearances, shots = away_shots, shot_on_target = away_shot_on_target, possession = away_possession, corners = away_corners, fouls_committed = away_fouls_committed, offside = away_offside, passes = away_passes, passes_into_box = away_passes_into_box, short_passes = away_passes_short, medium_passes = away_passes_medium, long_passes = away_passes_long, average_pass_length = away_average_pass_length, tackles = away_tackles, yellow_cards = away_yellow_cards, red_cards = away_red_cards, yellow_times = away_yellow_times, red_times = away_red_times)

birmingham_home <- full_season %>% filter(home_team == "Birmingham City WFC") %>% 
  select("match_id", "match_date", "kick_off", "stadium", "match_week", "home_score", "referee", "home_managers", "winning team", "match_result", "half_time_score_home", "home_goal_times", "home_clearances", "home_shots", "home_shot_on_target", "home_possession", "home_corners", "home_fouls_committed", "home_offsides", "home_passes", "home_passes_into_box", "home_passes_short", "home_passes_medium", "home_passes_long", "home_average_pass_length", "home_tackles", "home_yellow_cards", "home_red_cards", "home_yellow_times", "home_red_times") %>% 
  rename(score = home_score, manager = home_managers, half_time_score = half_time_score_home, goal_times = home_goal_times, clearances = home_clearances, shots = home_shots, shot_on_target = home_shot_on_target, possession = home_possession, corners = home_corners, fouls_committed = home_fouls_committed, offside = home_offsides, passes = home_passes, passes_into_box = home_passes_into_box, short_passes = home_passes_short, medium_passes = home_passes_medium, long_passes = home_passes_long, average_pass_length = home_average_pass_length, tackles = home_tackles, yellow_cards = home_yellow_cards, red_cards = home_red_cards, yellow_times = home_yellow_times, red_times = home_red_times)

birmingham <- rbind(birmingham_home, birmingham_away) %>% arrange(match_week)

# Extract aggregate stats
birmingham_stats <- birmingham %>% summarise(total_goals = sum(score), avg_goals = round(mean(score), 2), avg_clearance = round(mean(clearances),2), avg_shots = round(mean(shots),2), avg_sot = round(mean(shot_on_target),2), avg_possession = round(mean(possession),2), avg_fouls = round(mean(fouls_committed),2), avg_passes_complete = round(mean(passes),2), avg_pass_length = round(mean(average_pass_length),2), avg_tackles = round(mean(tackles),2), avg_yellow_cards = round(mean(yellow_cards),2), total_red_cards = sum(red_cards))
###############################################################################
# Combine aggregate stats rows for each team into a table
team_aggregate_stats <- rbind(aston_villa_stats, bha_stats, tottenham_stats, arsenal_stats, mancity_stats,manutd_stats, chelsea_stats, bristol_stats, everton_stats, reading_stats, birmingham_stats, westham_stats)

rownames(team_aggregate_stats) <- c("Aston Villa", "Brighton", "Tottenham", "Arsenal", "Man City", "Man United", "Chelsea", "Bristol", "Everton", "Reading", "Birmingham", "West Ham")

team_aggregate_stats

###############################################################################

# Extract goal and yellow card distributions for each team

# Aston Villa Goals
aston_villa_goals <- aston_villa$goal_times
aston_villa_goal_times <- goal_times_extract(aston_villa)

# Aston Villa Yellows
aston_villa_yellows <- aston_villa$yellow_times 
aston_villa_yellow_times <- yellow_times_extract(aston_villa)

# BHA Goals and Yellows
bha_goals <- bha$goal_times
bha_yellows <- bha$yellow_times

bha_yellow_times <- yellow_times_extract(bha) 
bha_goal_times <- goal_times_extract(bha)

# "Tottenham Hotspur Women Goals and Yellows"    
tottenham_goals <- tottenham$goal_times
tottenham_yellows <- tottenham$yellow_times

tottenham_yellow_times <- yellow_times_extract(tottenham) 
tottenham_goal_times <- goal_times_extract(tottenham)

# "Manchester United Goals and Yellows"
manutd_goals <- manutd$goal_times
manutd_yellows <- manutd$yellow_times

manutd_yellow_times <- yellow_times_extract(manutd) 
manutd_goal_times <- goal_times_extract(manutd)
# "Arsenal WFC Goals and Yellows"
arsenal_goals <- arsenal$goal_times
arsenal_yellows <- arsenal$yellow_times

arsenal_yellow_times <- yellow_times_extract(arsenal) 
arsenal_goal_times <- goal_times_extract(arsenal)
# "Bristol City WFC Goals and Yellows"  
bristol_goals <- bristol$goal_times
bristol_yellows <- bristol$yellow_times

bristol_yellow_times <- yellow_times_extract(bristol) 
bristol_goal_times <- goal_times_extract(bristol)
# "West Ham United LFC Goals and Yellows"
westham_goals <- westham$goal_times
westham_yellows <- westham$yellow_times

westham_yellow_times <- yellow_times_extract(westham) 
westham_goal_times <- goal_times_extract(westham)
# "Reading WFC Goals and Yellows"
reading_goals <- reading$goal_times
reading_yellows <- reading$yellow_times

reading_yellow_times <- yellow_times_extract(reading) 
reading_goal_times <- goal_times_extract(reading)
# "Everton LFC Goals and Yellows"
everton_goals <- everton$goal_times
everton_yellows <- everton$yellow_times

everton_yellow_times <- yellow_times_extract(everton) 
everton_goal_times <- goal_times_extract(everton)
# "Chelsea FCW Goals and Yellows"
chelsea_goals <- chelsea$goal_times
chelsea_yellows <- chelsea$yellow_times

chelsea_yellow_times <- yellow_times_extract(chelsea) 
chelsea_goal_times <- goal_times_extract(chelsea)
# "Manchester City WFC Goals and Yellows"
mancity_goals <- mancity$goal_times
mancity_yellows <- mancity$yellow_times

mancity_yellow_times <- yellow_times_extract(mancity) 
mancity_goal_times <- goal_times_extract(mancity)
# "Birmingham City WFC Goals and Yellows"
birmingham_goals <- birmingham$goal_times
birmingham_yellows <- birmingham$yellow_times

birmingham_yellow_times <- yellow_times_extract(birmingham) 
birmingham_goal_times <- goal_times_extract(birmingham)

# Histograms of Goal Distributions
par(mfrow = c(6,2), mar=c(2,2,2,2))
hist(aston_villa_goal_times, breaks = c(0,10,20,30,40,50,60,70,80,90,100), main = "Distribution of Goals - Aston Villa")
hist(bha_goal_times, breaks = c(0,10,20,30,40,50,60,70,80,90,100), main = "Distribution of Goals - Brighton")
hist(tottenham_goal_times, breaks = c(0,10,20,30,40,50,60,70,80,90,100), main = "Distribution of Goals - Tottenham")
hist(manutd_goal_times, breaks = c(0,10,20,30,40,50,60,70,80,90,100), main = "Distribution of Goals - Man. Utd")
hist(arsenal_goal_times, breaks = c(0,10,20,30,40,50,60,70,80,90,100), main = "Distribution of Goals - Arsenal ")
hist(bristol_goal_times, breaks = c(0,10,20,30,40,50,60,70,80,90,100), main = "Distribution of Goals - Bristol")
hist(westham_goal_times, breaks = c(0,10,20,30,40,50,60,70,80,90,100), main = "Distribution of Goals - West Ham")
hist(reading_goal_times, breaks = c(0,10,20,30,40,50,60,70,80,90,100), main = "Distribution of Goals - Reading")
hist(everton_goal_times, breaks = c(0,10,20,30,40,50,60,70,80,90,100), main = "Distribution of Goals - Everton")
hist(chelsea_goal_times, breaks = c(0,10,20,30,40,50,60,70,80,90,100), main = "Distribution of Goals - Chelsea")
hist(mancity_goal_times, breaks = c(0,10,20,30,40,50,60,70,80,90,100), main = "Distribution of Goals - Man. City")
hist(birmingham_goal_times, breaks = c(0,10,20,30,40,50,60,70,80,90,100), main = "Distribution of Goals - Birmingham")

# Histograms of Yellow Card Distributions
par(mfrow = c(6,2), mar=c(2,2,2,2))
hist(aston_villa_yellow_times, breaks = c(0,10,20,30,40,50,60,70,80,90,100), main = "Distribution of Yellow Cards - Aston Villa")
hist(bha_yellow_times, breaks = c(0,10,20,30,40,50,60,70,80,90,100), main = "Distribution of Yellow Cards - Brighton")
hist(tottenham_yellow_times, breaks = c(0,10,20,30,40,50,60,70,80,90,100), main = "Distribution of Yellow Cards - Tottenham")
hist(manutd_yellow_times, breaks = c(0,10,20,30,40,50,60,70,80,90,100), main = "Distribution of Yellow Cards - Man. Utd")
hist(arsenal_yellow_times, breaks = c(0,10,20,30,40,50,60,70,80,90,100), main = "Distribution of Yellow Cards - Arsenal ")
hist(bristol_yellow_times, breaks = c(0,10,20,30,40,50,60,70,80,90,100), main = "Distribution of Yellow Cards - Bristol")
hist(westham_yellow_times, breaks = c(0,10,20,30,40,50,60,70,80,90,100), main = "Distribution of Yellow Cards - West Ham")
hist(reading_yellow_times, breaks = c(0,10,20,30,40,50,60,70,80,90,100), main = "Distribution of Yellow Cards - Reading")
hist(everton_yellow_times, breaks = c(0,10,20,30,40,50,60,70,80,90,100), main = "Distribution of Yellow Cards - Everton")
hist(chelsea_yellow_times, breaks = c(0,10,20,30,40,50,60,70,80,90,100), main = "Distribution of Yellow Cards - Chelsea")
hist(mancity_yellow_times, breaks = c(0,10,20,30,40,50,60,70,80,90,100), main = "Distribution of Yellow Cards - Man. City")
hist(birmingham_yellow_times, breaks = c(0,10,20,30,40,50,60,70,80,90,100), main = "Distribution of Yellow Cards - Birmingham")

