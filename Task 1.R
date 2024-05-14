
# Author: Luca Moreno Louzada

# October 18th 2023

# This code: 
# > Prepares a dataset with candidate's newspaper coverage by transforming data 
# > Calculates the number of mentions (counts) relative to a candidate's visit to a county.
# > Visualizes mention count trends around candidate visits.
# > Computes regression models to explore the relationship between visits and mentions.

###########################################################################

# Step 0:  Cleaning workplace and loading libraries

rm(list = ls())

setwd("C:/Users/Luca Moreno Louzada/Desktop/Predoc/Tasks/Gentzkow/task1")

library(dplyr) # data wrangling
library(tidyr) # tidy data
library(readr) # opening tidy data 
library(lubridate) # dealing with dates
library(tidylog) # tracking changes
library(ggplot2) # plot data
library(lfe) # fixed effects regression
library(stargazer) # regression tables
library(cowplot) # multiple plots
library(zoo) # moving average function

###########################################################################

# Step 1: Read and prepare data

df = read_csv("visits.csv")

# Transform dataset to tidy format, to capture candidate-specific details
df = df %>%   
  pivot_longer(cols = -c(countycode, county, state, date), 
               names_to = c("type", "candidate"), 
               names_pattern = "(.+)_(.+)") %>% 
  pivot_wider(names_from = type, values_from = value) %>% 
  filter(!(candidate %in% c("buchanan", "bradley"))) 

# Adjust the date format, and add an indicator and the date value of the first visit
df = df %>% 
  mutate(date = as.Date(date, format = "%m/%d/%Y")) %>% 
  group_by(countycode, candidate) %>% 
  mutate(any_visit = max(visit),
         first_visit = min(date[visit == 1], na.rm = TRUE),
         dummy_first_visit = (date == first_visit)) %>% 
  ungroup()

# Calculate relative date distance
df = df %>% 
     mutate(days_dif = (date - first_visit))

# Calculate total count by candidate for comparisons
df = df %>% 
  group_by(countycode, date) %>% 
  mutate(
    counts_bush = sum(counts[candidate == "bush"], na.rm = TRUE),
    counts_cheney = sum(counts[candidate == "cheney"], na.rm = TRUE),
    counts_gore = sum(counts[candidate == "gore"], na.rm = TRUE),
    counts_lieberman = sum(counts[candidate == "lieberman"], na.rm = TRUE),
    all_counts = sum(counts)) 

###########################################################################

# Step 2: Visualization of data

# Check number of visits by candidate
df %>% 
  group_by(candidate, countycode) %>% 
  summarise(any_visit = max(any_visit)) %>% 
  group_by(candidate) %>%
  summarise(counties_visited = sum(any_visit))

# Plot with Gore's visits
p1 = 
  df %>%
  ungroup() %>% 
  filter(candidate == "gore") %>% 
  filter(any_visit == 1) %>% 
  filter(abs(days_dif) < 120) %>% 
  arrange(days_dif) %>% 
  group_by(days_dif) %>% 
  summarise(counts = mean(counts)) %>% 
  ggplot(., aes(x = days_dif)) + 
  geom_line(aes(y = counts)) +
  theme_classic() +
  labs(title = "Mention count by date relative to visit --- Gore ") +
  ylab("Average count across counties") +
  xlab("Days to/from visit") +
  ylim(0, 8) 
  
# Plot with Bush's visits
p2 =
  df %>%
  ungroup() %>% 
  filter(candidate == "bush") %>% 
  filter(any_visit == 1) %>% 
  filter(abs(days_dif) < 120) %>% 
  arrange(days_dif) %>% 
  group_by(days_dif) %>% 
  summarise(counts = mean(counts)) %>% 
  ggplot(., aes(x = days_dif)) + 
  geom_line(aes(y = counts)) +
  theme_classic() +
  labs(title = "Mention count by date relative to visit --- Bush ") +
  ylab("Average count across counties") +
  xlab("Days to/from visit") +
  ylim(0, 8) 

# Save plots
grid1 = plot_grid(p1, p2)
ggsave("plot_gore_bush.png",  height = 4, width = 10, grid1)

# Plot with Cheney's visits
p3 = 
  df %>%
  ungroup() %>% 
  filter(candidate == "cheney") %>% 
  filter(any_visit == 1) %>% 
  filter(abs(days_dif) < 120) %>% 
  arrange(days_dif) %>% 
  group_by(days_dif) %>% 
  summarise(counts = mean(counts)) %>% 
  ggplot(., aes(x = days_dif)) + 
  geom_line(aes(y = counts)) +
  theme_classic() +
  labs(title = "Mention count by date relative to visit --- Cheney ") +
  ylab("Average count across counties") +
  xlab("Days to/from visit") +
  ylim(0, 8) 

# Plot with Lieberman's visits
p4 =
  df %>%
  ungroup() %>% 
  filter(candidate == "lieberman") %>% 
  filter(any_visit == 1) %>% 
  filter(abs(days_dif) < 120) %>% 
  arrange(days_dif) %>% 
  group_by(days_dif) %>% 
  summarise(counts = mean(counts)) %>% 
  ggplot(., aes(x = days_dif)) + 
  geom_line(aes(y = counts)) +
  theme_classic() +
  labs(title = "Mention count by date relative to visit --- Lieberman ") +
  ylab("Average count across counties") +
  xlab("Days to/from visit") +
  ylim(0, 8) 

# Save plots
grid2 = plot_grid(p3, p4)
ggsave("plot_lieberman_cheney.png", height = 4, width = 10, grid2)

# Plot with Bush and Gore's visits
p5 = df %>% 
     ungroup() %>%
     filter(candidate == "bush") %>% 
     filter(any_visit == 1) %>% 
     filter(abs(days_dif) < 120) %>% 
     arrange(days_dif) %>% 
     group_by(days_dif) %>% 
     summarise(counts = mean(counts),
               counts_gore = mean(counts_gore)) %>%  
     mutate(counts = rollmean(counts, k = 7, fill = NA, align = "center")) %>%  # 7-day moving average 
     mutate(counts_gore = rollmean(counts_gore, k = 7, fill = NA, align = "center")) %>%  # 7-day moving average
     ggplot(., aes(x = days_dif)) + 
     geom_line(aes(y = counts, color = "Bush")) +
     geom_line(aes(y = counts_gore, color = "Gore")) +
     theme_classic() +
     labs(title = "Mention count by date relative to Bush's visit (counties visited by Bush)") +
     ylab("Average count across counties (7-day moving average)") +
     xlab("Days to/from visit") +
     scale_color_manual(name = "Candidate", values = c("Bush" = "red",
                                   "Gore" = "blue")) +
     ylim(0, 8) 

# Save plot
ggsave("plot_bush.png", height = 4, width = 10, p5)

###########################################################################

# Step 3: Regression analysis

# First we'll add a dummy for any previous visit by each candidate and others
# First for presidential candidates
df_pres = df %>% 
     filter(candidate %in% c("bush", "gore")) %>% 
     group_by(countycode, candidate) %>% 
     mutate(previous_visit = cummax(visit))  %>% 
     group_by(county) %>%
     mutate(all_previous_visits = cumsum(dummy_first_visit)) %>%
     ungroup() %>%
     mutate(previous_visit_other = (all_previous_visits - previous_visit))

#  Now for all
df_all = df %>% 
  group_by(countycode, candidate) %>% 
  mutate(previous_visit = cummax(visit))  %>% 
  group_by(county) %>%
  mutate(all_previous_visits = cumsum(dummy_first_visit)) %>%
  ungroup() %>%
  mutate(previous_visit_other = (all_previous_visits - previous_visit))


# Regression with sample selection (Bush and Gore)
reg_df = df_pres
m1 = felm(counts ~ previous_visit | countycode + date + candidate, data = reg_df) 
m2 = felm(counts ~ previous_visit + previous_visit_other| countycode + date + candidate, data = reg_df) 
m3 = felm(counts ~ previous_visit + previous_visit_other|  countycode + candidate + factor(state):date, data = reg_df) 

# Same with headline mentions
m4 = felm(hcounts ~ previous_visit | countycode + date + candidate, data = reg_df) 
m5 = felm(hcounts ~ previous_visit + previous_visit_other| countycode + date + candidate, data = reg_df) 
m6 = felm(hcounts ~ previous_visit + previous_visit_other|  countycode + candidate + factor(state):date, data = reg_df) 

# Export regression
stargazer(m1,m2, m3, m4, m5, m6, type = "latex")

# All candidates
reg_df = df_all
m1 = felm(counts ~ previous_visit | countycode + date + candidate, data = reg_df) 
m2 = felm(counts ~ previous_visit + previous_visit_other| countycode + date + candidate, data = reg_df) 
m3 = felm(counts ~ previous_visit + previous_visit_other|  countycode + candidate + factor(state):date, data = reg_df) 

m4 = felm(hcounts ~ previous_visit | countycode + date + candidate, data = reg_df) 
m5 = felm(hcounts ~ previous_visit + previous_visit_other| countycode + date + candidate, data = reg_df) 
m6 = felm(hcounts ~ previous_visit + previous_visit_other|  countycode + candidate + factor(state):date, data = reg_df) 

stargazer(m1,m2, m3, m4, m5, m6, type = "latex")

###########################################################################

