library(readxl)
library(plm)
library(stargazer)
library(ggplot2)
library(dplyr)
library(ggcorrplot)
library(xtable)

#loading dataset
data = read_excel("C:/Users/Bryan/SynologyDrive/Universität/Master/24 FS/Applied Sports Research/Data/engineered/galacticos_dataset.xlsx")
plot_path = "C:/Users/Bryan/SynologyDrive/Universität/Master/24 FS/Applied Sports Research/Plots"

# defining datatypes
data$season = as.factor(data$season)
data$team = as.factor(data$team)
data$elo_season_rank = as.factor(data$elo_season_rank)
data$league = as.factor(data$league)
data$cup_won = as.factor(data$cup_won)
data$overall = as.numeric(data$overall)

# handle outliers
data = data[data$net_spend < 500, ]

data_2 = data[c("PPG", "elo", "LOR", "league_normalized_net_spend", "league_normalized_team_value", 'high_value_transfers', "out", "promoted", "rank")]
data_2 = data_2 %>%
  rename(
    LNNS = league_normalized_net_spend,
    LNTV = league_normalized_team_value,
    galacticos = high_value_transfers)

# coloring
league_colors = c("Premier League" = "deepskyblue3",
                  "LaLiga" = "gold2",
                  "Bundesliga" = "orangered2",
                  "Serie A" = "slateblue3",
                  "Ligue 1" = "chartreuse3")

base_theme = theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(size = 16, face = "bold"),
    axis.title.x = element_text(size = 16, face = "bold"),
    axis.title.y = element_text(size = 16, face = "bold"),
    axis.text.x = element_text(size = 14.5),
    axis.text.y = element_text(size = 14.5),
    legend.title = element_text(size = 16, face = "bold"),
    legend.text = element_text(size = 14.5))

# some EDA
basic_stats_table = data.frame(
  Variable = character(),
  Mean = double(),
  Median = double(),
  SD = double(),
  Variance = double(),
  Min = double(),
  Max = double(),
  stringsAsFactors = FALSE)

relevant_cols = c("PPG", "elo", "LOR", "LNNS", "LNTV", 'galacticos', "out", "promoted")

data = data %>% rename(galacticos = high_value_transfers)

corr = round(cor(data[c("PPG", "elo", "LOR", "net_spend", "team_value", 'galacticos', "out", "promoted")], use = "complete.obs"), 1)

cor_plot = ggcorrplot(corr, hc.order = TRUE, type = "lower",
                     outline.col = "white",
                     ggtheme = base_theme,
                     colors = c("#6D9EC1", "white", "#E46726"),
                     lab = T) + theme(legend.position = "None")

print(cor_plot)

plot_eda_numerical <- function(data, columns) {
  library(dplyr)
  library(ggplot2)
  
  if (!exists("basic_stats_table", .GlobalEnv)) {
    basic_stats_table <<- data.frame()}
  
  plot_path <- "C:/Users/Bryan/SynologyDrive/Universität/Master/24 FS/Applied Sports Research/Plots"
  
  for (column_name in columns) {
    basic_stats <- data %>%
      summarise(
        Mean = round(mean(!!sym(column_name), na.rm = TRUE), digits = 3),
        Median = round(median(!!sym(column_name), na.rm = TRUE), digits = 3),
        SD = round(sd(!!sym(column_name), na.rm = TRUE), digits = 3),
        Variance = round(var(!!sym(column_name), na.rm = TRUE), digits = 3),
        Min = round(min(!!sym(column_name), na.rm = TRUE), digits = 3),
        Max = round(max(!!sym(column_name), na.rm = TRUE), digits = 3))
    
    basic_stats$Variable <- column_name
    basic_stats_table <<- rbind(basic_stats_table, basic_stats)
    
    p_hist <- ggplot(data, aes(x = .data[[column_name]])) +
      geom_histogram(aes(y = ..density..), bins = 10, fill = "blue", alpha = 0.7) +
      geom_density(alpha = 0.2, fill = "#FF6666") +
      scale_y_continuous(breaks = seq(-3.25, 3.25, by = 0.25), name = "Density") +
      labs(x = column_name, y = "Density", title = NULL) + 
      theme(panel.background = element_rect(fill = "white", colour = "white")) +
      base_theme
    
    file_name_hist <- paste(plot_path, "/", column_name, "_histogram.pdf", sep = "")
    ggsave(file_name_hist, plot = p_hist, width = 10, height = 6, dpi = 600)
    
    p_boxplot <- ggplot(data, aes(x = "", y = .data[[column_name]])) +
      geom_boxplot(fill = "blue", alpha = 0.7) +
      geom_hline(aes(yintercept = basic_stats$Mean, color = "Mean"), linetype = "dashed", size = 1) +
      geom_hline(aes(yintercept = basic_stats$Median, color = "Median"), linetype = "dashed", size = 1) +
      theme(axis.text.x = element_blank(), axis.title.x = element_blank(),
            panel.background = element_rect(fill = "white", colour = "white")) +
      scale_color_manual(name = element_blank(), values = c("Mean" = "red", "Median" = "green")) + 
      base_theme +
      ggtitle("")
    
    file_name_box <- paste(plot_path, "/", column_name, "_boxplot.pdf", sep = "")
    ggsave(file_name_box, plot = p_boxplot, width = 10, height = 6, dpi = 600)
    
    print(p_hist)
    print(p_boxplot)}}

for (column_name in c("PPG", "elo", "LOR", "net_spend", "team_value", 'galacticos', "out", "promoted")) {
  if (is.numeric(data[[column_name]])) {
    plot_eda_numerical(data, c(column_name))}}

basic_stats_table = basic_stats_table %>% relocate(Variable) # for better looking latex table

print(xtable(basic_stats_table), type = "latex", include.rownames = F, align = "lcccccc", table.placement = "H")

# scatterplots per season per team in each league
combinations = distinct(data, season, league)

for (i in 1:nrow(combinations)) {
  current_season = combinations$season[i]
  current_league = combinations$league[i]
  
  filtered_data = filter(data, season == current_season & league == current_league)
  
  variable_x = "net_spend"
  variable_y = "Pts" 
  filtered_data = filtered_data %>%
    filter(!is.na(!!sym(variable_x)) & !is.na(!!sym(variable_y)))
  
  title_x_axis = "Net Spend"
  title_y_axis = "Points"
  
  if (nrow(filtered_data) == 0) next
  
  # scatter plot for the current season and league
  p = ggplot(filtered_data, aes(x = !!sym(variable_x), y = !!sym(variable_y), label = team)) +
    geom_point(aes(color = team), alpha = 0.7) +
    geom_text(aes(label = team), vjust = 1.5, hjust = 0.5, size = 3, check_overlap = TRUE, color = "black") +
    base_theme +
    labs(title = paste(current_league, current_season, "Season"),
         x = title_x_axis, 
         y = title_y_axis, 
         color = "Team") +
    theme(plot.title = element_text(hjust = 0.5), legend.position = "none")
  
  print(p)}

### scatterplot: average net_spend and average points over all seasons for each team in each league ###

for (current_league in unique_leagues) {
  
  filtered_data = data %>%
    filter(league == current_league) %>%  
    group_by(team) %>%  
    filter(n() >= 3) %>%  
    summarise(
      avg_net_spend = mean(net_spend, na.rm = TRUE), 
      avg_Points = mean(Pts, na.rm = TRUE),  
      .groups = 'drop'  )
  
  p = ggplot(filtered_data, aes(x = avg_net_spend, y = avg_Points, label = team, color = team)) +
    geom_point(alpha = 0.9, size = 4) +
    geom_text(aes(label = team), check_overlap = TRUE, vjust = 1.5, hjust = 0.5, size = 4.5, color = "black") + 
    base_theme +
    labs(
      x = "Average Net Spend (€m)",
      y = "Average Points",
      color = "Team") + 
    theme(
          legend.position = "none",
          text = element_text(size = 16),
          axis.text.x = element_text(size = 16),
          axis.text.y = element_text(size = 16))
  print(p)
  
  file_name <- paste0(plot_path, "/", current_league, "Avg_Net_Spend_vs_Points.pdf")
  ggsave(file_name, plot = p, width = 10, height = 10, dpi = 600)}

### PPG leaders table and graph ###

data_sorted_PPG = data %>%
  arrange(desc(PPG)) %>%
  mutate(PPG_rank = min_rank(desc(PPG)))

top_15_ppg_teams = data_sorted_PPG %>%
  select(team, league, season, PPG, PPG_rank) %>%
  mutate(Team_Season = paste(team, season, sep = " ("), Team_Season = paste(Team_Season, ")", sep = "")) %>%
  arrange(desc(PPG)) %>%
  head(10)

top_ppg_plot = ggplot(data = top_15_ppg_teams, aes(x = reorder(Team_Season, PPG), y = PPG, fill = league)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  base_theme +
  labs(title = "", x = "", y = "PPG") +
  geom_text(aes(label = sprintf("%0.2f", PPG)), hjust = 0.95, position = position_identity()) + 
  scale_y_continuous(breaks = seq(0, 3.5, by = 0.5)) +
  theme(
    legend.title = element_blank(),
    plot.title.position = "plot",
    axis.text.x = element_text(size = 10),  
    axis.text.y = element_text(size = 10),
    legend.text = element_text(size = 10),
    legend.position = "none") +
  scale_fill_manual(values = league_colors)

print(top_ppg_plot)
file_name <- paste0(plot_path, "/Top_Teams_by_PPG.pdf")
ggsave(file_name, plot = top_ppg_plot, width = 10, height = 6, dpi = 600)

### average net spend leaders ###
avg_spend = data %>%
  group_by(team, league) %>%
  summarize(Count = n(), AvgSpend = mean(net_spend, na.rm = TRUE), .groups = 'drop') %>%
  filter(Count > 5)

top_net_spend = avg_spend %>%
  arrange(desc(AvgSpend)) %>%
  mutate(SpendRank = min_rank(desc(AvgSpend))) %>%
  slice_head(n = 15) %>%
  mutate(TeamSeason = team)

net_spend_p = ggplot(top_net_spend, aes(x = reorder(TeamSeason, AvgSpend), y = AvgSpend, fill = league)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  labs(title = "", x = "", y = "Average Net Spend (in m€)") +
  geom_text(aes(label = sprintf("%0.f", AvgSpend), hjust = 1.25)) +
  scale_y_continuous(breaks = seq(0, 150, 25)) +
  base_theme +
  theme(
    plot.title = element_text(hjust = 0),
    axis.text.x = element_text(size = 10),  
    axis.text.y = element_text(size = 10),
    legend.title = element_blank(),
    legend.text = element_text(size = 10),
    plot.title.position = "plot",
    legend.position = "none",
  ) +
  scale_fill_manual(values = league_colors)

print(net_spend_p)
file_name <- paste0(plot_path, "/Highest_Average_Net_Spend.pdf")
ggsave(file_name, plot = net_spend_p, width = 10, height = 6, dpi = 600)

### highest valued team per season ###
highest_valued = data %>%
  group_by(season) %>%
  slice_max(order_by = team_value, n = 1, with_ties = FALSE) %>%
  ungroup() %>%
  arrange(desc(season)) %>%
  mutate(Team_Season = paste(team, season, sep = " - "))

highest_net_spend = data %>%
  group_by(season) %>%
  slice_max(order_by = net_spend, n = 1, with_ties = FALSE) %>%
  ungroup() %>%
  arrange(desc(season)) %>%
  mutate(Team_Season = paste(team, season, sep = " - "))

top_team_value_p = ggplot(highest_net_spend, aes(x = reorder(Team_Season, team_value), y = team_value, fill = league)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  labs(title = "", y = "Team Value (€m)", x = "") +
  geom_text(aes(label = sprintf("%0.f", team_value), hjust = 1.25)) +
  scale_y_continuous(breaks = seq(0, 1100, by = 100)) +
  base_theme +
  theme(
    plot.title = element_text(hjust = 0),  
    axis.text.x = element_text(size = 10, angle = 45, hjust = 1),  
    axis.text.y = element_text(size = 10),  
    legend.title = element_blank(),
    legend.text = element_text(size = 10),
    legend.position = "none",  
    plot.title.position = "plot") +
  scale_fill_manual(values = league_colors)

print(top_team_value_p)
file_name <- paste0(plot_path, "/Highest_Valued_Teams_per_Season.pdf")
ggsave(file_name, plot = top_team_value_p, width = 10, height = 6, dpi = 600)

### highest net spend per season ###
highest_net_spend_p = ggplot(highest_net_spend, aes(x = reorder(Team_Season, net_spend), y = net_spend, fill = league)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  labs(title = "", y = "Net Spend (€m)", x = "") +
  geom_text(aes(label = sprintf("%0.f", net_spend), hjust = 1.25)) +
  scale_y_continuous(breaks = seq(0, 400, by = 50)) +
  base_theme +
  theme(
    plot.title = element_text(hjust = 0),  
    axis.text.x = element_text(size = 10),
    axis.text.y = element_text(size = 10),  
    legend.title = element_blank(),
    legend.position = "none",
    legend.text = element_text(size = 10),  
    plot.title.position = "plot") +
  scale_fill_manual(values = league_colors)

print(highest_net_spend_p)
file_name <- paste0(plot_path, "/Highest_Net_Spend_per_Season.pdf")
ggsave(file_name, plot = highest_net_spend_p, width = 10, height = 6, dpi = 600)

### average net spend over time ###
seasonal_net_spend_avg = data %>%
  group_by(season) %>%
  summarise(avg_total_net_spend = mean(total_net_spend_league, na.rm = TRUE))

total_average_net_spend = ggplot(seasonal_net_spend_avg, aes(x = season, y = avg_total_net_spend, group = 1)) +
  geom_line(color = "blue", size = 1) + 
  geom_point(color = "blue", size = 3) +
  labs(title = "",
       x = "",
       y = "Avg Total Net Spend (€m)") +
  base_theme +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

plot(total_average_net_spend)
file_name_box = paste(plot_path, "/total_average_net_spend.pdf", sep = "")
ggsave(file_name_box, plot = total_average_net_spend, width = 10, height = 6, dpi = 600)

# 10 top valued teams
top_valued_teams = data %>%
  slice_max(order_by = team_value, n = 10, with_ties = FALSE) %>%
  arrange(desc(team_value)) %>%
  mutate(Team_Season = paste(team, season, sep = " - "))

top_team_value_p = ggplot(top_valued_teams, aes(x = reorder(Team_Season, team_value), y = team_value, fill = league)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  labs(title = "Top 10 Highest Valued Teams", y = "Team Value (€m)", x = "") +
  geom_text(aes(label = sprintf("%0.f", team_value), hjust = 1.25)) +  # Adjusted text positioning to improve readability
  scale_y_continuous(breaks = seq(0, max(top_valued_teams$team_value, na.rm = TRUE) + 100, by = 100)) +
  base_theme +
  theme(
    plot.title = element_text(hjust = 0.5),  
    axis.text.x = element_text(size = 10, angle = 45, hjust = 1),  
    axis.text.y = element_text(size = 10),  
    legend.title = element_blank(),
    legend.text = element_text(size = 10),
    legend.position = "none",  
    plot.title.position = "plot") +
  scale_fill_manual(values = league_colors)

print(top_team_value_p)

data_sorted_elo = data %>%
  arrange(desc(elo)) %>%
  mutate(elo_rank = min_rank(desc(elo)))

top_15_elo_teams = data_sorted_elo %>%
  select(team, league, season, elo, elo_rank) %>%
  mutate(Team_Season = paste(team, season, sep = " ("), Team_Season = paste(Team_Season, ")", sep = "")) %>%
  arrange(desc(elo)) %>%
  head(10)

top_elo_plot = ggplot(data = top_15_elo_teams, aes(x = reorder(Team_Season, elo), y = elo, fill = league)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  base_theme +
  labs(title = "", x = "", y = "elo") +
  geom_text(aes(label = sprintf("%0.2f", elo)), hjust = 0.95, position = position_identity()) + 
  scale_y_continuous(breaks = seq(0, 3.5, by = 0.5)) +
  theme(
    legend.title = element_blank(),
    plot.title.position = "plot",
    axis.text.x = element_text(size = 10),  
    axis.text.y = element_text(size = 10),
    legend.text = element_text(size = 10),
    legend.position = "none") +
  scale_fill_manual(values = league_colors)

print(top_elo_plot)
file_name <- paste0(plot_path, "/Top_Teams_by_elo.pdf")
ggsave(file_name, plot = top_elo_plot, width = 10, height = 6, dpi = 600)

### highest valued team per season ###
highest_valued = data %>%
  group_by(season) %>%
  slice_max(order_by = league_normalized_team_value, n = 1, with_ties = FALSE) %>%
  ungroup() %>%
  arrange(desc(season)) %>%
  mutate(Team_Season = paste(team, season, sep = " - "))

highest_net_spend = data %>%
  group_by(season) %>%
  slice_max(order_by = net_spend, n = 1, with_ties = FALSE) %>%
  ungroup() %>%
  arrange(desc(season)) %>%
  mutate(Team_Season = paste(team, season, sep = " - "))

top_league_normalized_team_value_p = ggplot(highest_net_spend, aes(x = reorder(Team_Season, league_normalized_team_value), y = league_normalized_team_value, fill = league)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  labs(title = "", y = "Team Value (€m)", x = "") +
  geom_text(aes(label = sprintf("%0.f", league_normalized_team_value), hjust = 1.25)) +
  scale_y_continuous(breaks = seq(0, 1100, by = 100)) +
  base_theme +
  theme(
    plot.title = element_text(hjust = 0),  
    axis.text.x = element_text(size = 10, angle = 45, hjust = 1),  
    axis.text.y = element_text(size = 10),  
    legend.title = element_blank(),
    legend.text = element_text(size = 10),
    legend.position = "none",  
    plot.title.position = "plot") +
  scale_fill_manual(values = league_colors)

print(top_league_normalized_team_value_p)
file_name <- paste0(plot_path, "/Highest_Valued_Teams_per_Season.pdf")
ggsave(file_name, plot = top_league_normalized_team_value_p, width = 10, height = 6, dpi = 600)

