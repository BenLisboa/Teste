
# Author: Luca Moreno Louzada

# October 22nd 2023


###########################################################################

# Step 0:  Cleaning workplace and loading libraries

rm(list = ls())

setwd("C:/Users/Luca Moreno Louzada/Desktop/Predoc/Tasks/Stigler/data")

library(dplyr) # data wrangling
library(tidyr) # tidy data
library(readr) # opening tidy data 
library(tidylog) # tracking changes
library(ggplot2) # plot data
library(lfe) # fixed effects regression
library(stargazer) # regression tables
library(cowplot) # multiple plots

options(scipen = 999) # turn of scientific notation

###########################################################################

# Step 1: Read and clean data

# Opening state characteristics dataset
state_df = xml2::read_html("state_chars_text.html") %>% 
              rvest::html_table() 

# The function returns two dataframes, we need the first one
state_df = state_df[[1]] 

# We need to remove the first column which is empty
state_df = state_df %>% 
           select(-1)

# Firm donations
law_firms = xml2::read_html("law_firms_donations.html") %>% 
            rvest::html_table() 

law_firms = law_firms[[1]] 

nonlaw_firms = xml2::read_html("nonlaw_firms_donations.html") %>% 
  rvest::html_table() 

nonlaw_firms = nonlaw_firms[[1]] 

# We can combine the dataframes as they have the same columns
# But we need to do some name cleaning first for them to match
names(nonlaw_firms) = gsub(" ", "_", names(nonlaw_firms)) 

# Then we add an indicator for each of them
law_firms = law_firms %>% 
            select(-1) %>%  # remove empty column 
            mutate(type = "law")

nonlaw_firms = nonlaw_firms %>% 
               select(-1) %>%  # remove empty column 
               mutate(type = "nonlaw") 

firms = bind_rows(law_firms, nonlaw_firms) 
        

# Individual donations
lawyers = read_csv("lawyers_donations.csv") %>%
          mutate(type = "lawyer")

# Let's drop the sector information as we'll not use them
nonlawyers = read_csv("nonlaw_indivs_donations.csv") %>% 
             select(-contains("broad")) %>% 
             mutate(type = "nonlawyer")

names(nonlawyers) = gsub(" ", "_", names(nonlawyers)) 


# Combine the dataframes
individuals = bind_rows(lawyers, nonlawyers)

###########################################################################

# Step 2: Preparing datasets

# Adding requested dummies
state_df = state_df %>%
           mutate(binding_commission = 1 * grepl(" binding", interim_appointment_method)) %>% 
           mutate(partisan_election = 1 * grepl("are elected by partisan election", first_term_method)) %>% 
           mutate(retention = 1 * grepl("retention", additional_term_method))

# Note that West Virginia and North Carolina had changes and thus appear twice on the dataset
state_df$partisan_election[state_df$slug == "NC_96_03"] = 1
state_df$partisan_election[state_df$slug == "WV_96_14"] = 1


# Adding retirement dummy and dropping unused variables
state_df = state_df %>% 
           mutate(retirement =  1 - 1 * grepl("do not face", mandatory_retirement)) %>% 
           select(slug, state_name, bench_size, term_length,
                  binding_commission, partisan_election,
                  retention, retirement) %>% 
           mutate(state_name = toupper(state_name)) # uppercase to match donation dataset


# Retrieve list of states with 6 year term-length
states_term6 = state_df %>% 
               filter(term_length == 6) %>% 
               pull(state_name) %>% 
               toupper()

# Retrieve list of states with retention
states_retention = state_df %>% 
  filter(retention == 1) %>% 
  pull(state_name) %>% 
  toupper() 

###########################################################################

# Step 3: Tasks (2) to (4)

# Some statistics
individuals %>% 
      mutate(state_name = toupper(state_name))  %>% 
  filter(state_name %in% states_term6) %>% 
  group_by(state_name) %>% 
  summarise(min = min(amount, na.rm = T),
            max = max(amount, na.rm = T),
            median = median(amount, na.rm = T),
            iqr = IQR(amount, na.rm = T),
            mean = mean(amount, na.rm = T)) %>% 
  mutate(across(where(is.numeric), round, 2)) %>% 
  stargazer(type = "latex", summary = F)


individuals %>% 
  mutate(state_name = toupper(state_name))  %>% 
  filter(state_name %in% states_term6) %>% 
  filter(type == "lawyer") %>% 
  group_by(state_name) %>% 
  summarise(min = min(amount, na.rm = T),
            max = max(amount, na.rm = T),
            median = median(amount, na.rm = T),
            iqr = IQR(amount, na.rm = T),
            mean = mean(amount, na.rm = T)) %>% 
  mutate(across(where(is.numeric), round, 2)) %>% 
  stargazer(type = "latex", summary = F)

firms %>% 
  filter(type == "law") %>% 
  mutate(state_name = toupper(state_name))  %>% 
  filter(state_name == "OHIO" & contributor != "NaN") %>% 
  group_by(contributor) %>% 
  summarise(amount = sum(amount, na.rm = T)) %>% 
  ungroup() %>% 
  arrange(desc(amount)) %>% 
  top_n(10, amount)  %>% 
  stargazer(type = "latex", summary = F)


individuals %>% 
  filter(type == "nonlawyer") %>% 
  mutate(state_name = toupper(state_name))  %>% 
  mutate(donation_retention = state_name %in% states_retention) %>%
  group_by(contributor) %>% 
  mutate(all_donation_retention = sum(donation_retention, na.rm = T)) %>% 
  ungroup() %>% 
  filter(all_donation_retention > 0) %>% 
  group_by(contributor) %>% 
  summarise(amount = sum(amount, na.rm = T),
            states_donated = paste(unique(state_name), collapse = ", ")
            ) %>% 
  ungroup() %>% 
  arrange(desc(amount)) %>% 
  top_n(10, amount) %>% 
  stargazer(type = "latex", summary = F)

###########################################################################

# Step 4: Analysis

# Note that West Virginia and North Carolina had changes and thus appear twice on the dataset
# Let's keep just one of them and fix manually
state_df = state_df %>%
            filter(!(slug %in% c("NC_96_03", "WV_96_14")))


# Add state characteristics to donation dataset
df = individuals %>% 
     mutate(state_name = toupper(state_name))  %>% 
          left_join(state_df, by = "state_name")

# Fix WV and NC
df = df %>% 
  mutate(partisan_election = case_when(state_name == "WEST VIRGINIA" &
                                  election_year < 2015 ~ 1,
                                state_name == "WEST VIRGINIA" &
                                  election_year >= 2015 ~ 0,
                                state_name == "NORTH CAROLINA" &
                                  election_year < 2004 ~ 1,
                                state_name == "NORTH CAROLINA" &
                                  election_year >= 2004 ~ 0,
                                TRUE ~ partisan_election))


# Same for firms
df_firms = firms %>% 
  mutate(state_name = toupper(state_name))  %>% 
  left_join(state_df, by = "state_name")

df_firms = df_firms %>% 
  mutate(partisan_election = case_when(state_name == "WEST VIRGINIA" &
                                         election_year < 2015 ~ 1,
                                       state_name == "WEST VIRGINIA" &
                                         election_year >= 2015 ~ 0,
                                       state_name == "NORTH CAROLINA" &
                                         election_year < 2004 ~ 1,
                                       state_name == "NORTH CAROLINA" &
                                         election_year >= 2004 ~ 0,
                                       TRUE ~ partisan_election))

# Plot evolution of donations
p1 = df_firms %>%
group_by(election_year, type) %>% 
summarise(amount = sum(amount)) %>% 
ggplot(., aes(x = election_year, y = amount)) + 
  geom_line(aes(color = factor(type)))+
  theme_classic() +
  xlab("Election year") +
  ylab("Total amount") +
  labs(title = "Total amount donated by firms by year (law-firms and non-law firms)") +
  scale_color_discrete(name = "Group", labels = c("law" = "Law firms", "nonlaw" = "Others"))

p1 %>% 
  ggsave(filename = "plot_firms.png")



# Plot evolution of donations
p2 = df %>%
  group_by(election_year, type) %>% 
  summarise(amount = sum(amount)) %>% 
  ggplot(., aes(x = election_year, y = amount)) + 
  geom_line(aes(color = factor(type)))+
  theme_classic() +
  xlab("Election year") +
  ylab("Total amount") +
  labs(title = "Total amount donated by individuals by year (lawyers and non-lawyers)") +
  scale_color_discrete(name = "Group", labels = c("lawyer" = "Lawyers", "nonlawyer" = "Non-lawyers"))

p2 %>% 
  ggsave(filename = "plot_individuals.png")

# Excluding negative values and calculating logs
df = df %>% 
     filter(amount > 0) %>% 
     mutate(lamount = log(amount)) 


df_firms = df_firms %>% 
  filter(amount > 0) %>% 
  mutate(lamount = log(amount)) 

# Institutional characteristics regressions
m1  = felm(lamount ~ partisan_election  | election_year, data = df ) 
m2  = felm(lamount ~ partisan_election | election_year + type, data = df ) 
m3  = felm(lamount ~ partisan_election   |election_year + type + general_party, data = df ) 
m4  = felm(lamount ~ partisan_election| election_year + type + general_party + incumbency_status, data = df ) 

stargazer(m1, m2, m3, m4, type = "latex", keep.stat = "n",
          add.lines = list(c("Year FE", "Y", "Y", "Y", "Y"),
                           c("Contributor group", "", "Y", "Y", "Y"),
                           c("Party", "", "", "Y", "Y"),
                           c("Incumbency status", "", "", "", "Y")))

m1  = felm(lamount ~ partisan_election  | election_year, data = df_firms ) 
m2  = felm(lamount ~ partisan_election | election_year + type, data = df_firms ) 
m3  = felm(lamount ~ partisan_election   |election_year + type + general_party, data = df_firms ) 
m4  = felm(lamount ~ partisan_election| election_year + type + general_party + incumbency_status, data = df_firms ) 

stargazer(m1, m2, m3, m4, type = "latex", keep.stat = "n",
          add.lines = list(c("Year FE", "Y", "Y", "Y", "Y"),
                           c("Contributor group", "", "Y", "Y", "Y"),
                           c("Party", "", "", "Y", "Y"),
                           c("Incumbency status", "", "", "", "Y")))


m1  = felm(lamount ~ retention  | election_year, data = df ) 
m2  = felm(lamount ~ retention | election_year + type, data = df ) 
m3  = felm(lamount ~ retention   |election_year + type + general_party, data = df ) 
m4  = felm(lamount ~ retention| election_year + type + general_party + incumbency_status, data = df ) 

stargazer(m1, m2, m3, m4, type = "latex", keep.stat = "n",
          add.lines = list(c("Year FE", "Y", "Y", "Y", "Y"),
                           c("Contributor group", "", "Y", "Y", "Y"),
                           c("Party", "", "", "Y", "Y"),
                           c("Incumbency status", "", "", "", "Y")))

m1  = felm(lamount ~ retention  | election_year, data = df_firms ) 
m2  = felm(lamount ~ retention | election_year + type, data = df_firms ) 
m3  = felm(lamount ~ retention   |election_year + type + general_party, data = df_firms ) 
m4  = felm(lamount ~ retention| election_year + type + general_party + incumbency_status, data = df_firms ) 

stargazer(m1, m2, m3, m4, type = "latex", keep.stat = "n",
          add.lines = list(c("Year FE", "Y", "Y", "Y", "Y"),
                           c("Contributor group", "", "Y", "Y", "Y"),
                           c("Party", "", "", "Y", "Y"),
                           c("Incumbency status", "", "", "", "Y")))



m1  = felm(lamount ~ binding_commission  | election_year, data = df ) 
m2  = felm(lamount ~ binding_commission | election_year + type, data = df ) 
m3  = felm(lamount ~ binding_commission   |election_year + type + general_party, data = df ) 
m4  = felm(lamount ~ binding_commission| election_year + type + general_party + incumbency_status, data = df ) 

stargazer(m1, m2, m3, m4, type = "latex", keep.stat = "n",
          add.lines = list(c("Year FE", "Y", "Y", "Y", "Y"),
                           c("Contributor group", "", "Y", "Y", "Y"),
                           c("Party", "", "", "Y", "Y"),
                           c("Incumbency status", "", "", "", "Y")))

m1  = felm(lamount ~ binding_commission  | election_year, data = df_firms ) 
m2  = felm(lamount ~ binding_commission | election_year + type, data = df_firms ) 
m3  = felm(lamount ~ binding_commission   |election_year + type + general_party, data = df_firms ) 
m4  = felm(lamount ~ binding_commission| election_year + type + general_party + incumbency_status, data = df_firms ) 

stargazer(m1, m2, m3, m4, type = "latex", keep.stat = "n",
          add.lines = list(c("Year FE", "Y", "Y", "Y", "Y"),
                           c("Contributor group", "", "Y", "Y", "Y"),
                           c("Party", "", "", "Y", "Y"),
                           c("Incumbency status", "", "", "", "Y")))


m1  = felm(lamount ~ retirement  | election_year, data = df ) 
m2  = felm(lamount ~ retirement | election_year + type, data = df ) 
m3  = felm(lamount ~ retirement   |election_year + type + general_party, data = df ) 
m4  = felm(lamount ~ retirement| election_year + type + general_party + incumbency_status, data = df ) 

stargazer(m1, m2, m3, m4, type = "latex", keep.stat = "n",
          add.lines = list(c("Year FE", "Y", "Y", "Y", "Y"),
                           c("Contributor group", "", "Y", "Y", "Y"),
                           c("Party", "", "", "Y", "Y"),
                           c("Incumbency status", "", "", "", "Y")))

m1  = felm(lamount ~ retirement  | election_year, data = df_firms ) 
m2  = felm(lamount ~ retirement | election_year + type, data = df_firms ) 
m3  = felm(lamount ~ retirement   |election_year + type + general_party, data = df_firms ) 
m4  = felm(lamount ~ retirement| election_year + type + general_party + incumbency_status, data = df_firms ) 

stargazer(m1, m2, m3, m4, type = "latex", keep.stat = "n",
          add.lines = list(c("Year FE", "Y", "Y", "Y", "Y"),
                           c("Contributor group", "", "Y", "Y", "Y"),
                           c("Party", "", "", "Y", "Y"),
                           c("Incumbency status", "", "", "", "Y")))




m1  = felm(lamount ~ term_length  | election_year, data = df ) 
m2  = felm(lamount ~ term_length | election_year + type, data = df ) 
m3  = felm(lamount ~ term_length   |election_year + type + general_party, data = df ) 
m4  = felm(lamount ~ term_length| election_year + type + general_party + incumbency_status, data = df ) 

stargazer(m1, m2, m3, m4, type = "latex", keep.stat = "n",
          add.lines = list(c("Year FE", "Y", "Y", "Y", "Y"),
                           c("Contributor group", "", "Y", "Y", "Y"),
                           c("Party", "", "", "Y", "Y"),
                           c("Incumbency status", "", "", "", "Y")))

m1  = felm(lamount ~ term_length  | election_year, data = df_firms ) 
m2  = felm(lamount ~ term_length | election_year + type, data = df_firms ) 
m3  = felm(lamount ~ term_length   |election_year + type + general_party, data = df_firms ) 
m4  = felm(lamount ~ term_length| election_year + type + general_party + incumbency_status, data = df_firms ) 

stargazer(m1, m2, m3, m4, type = "latex", keep.stat = "n",
          add.lines = list(c("Year FE", "Y", "Y", "Y", "Y"),
                           c("Contributor group", "", "Y", "Y", "Y"),
                           c("Party", "", "", "Y", "Y"),
                           c("Incumbency status", "", "", "", "Y")))

m1  = felm(lamount ~ bench_size  | election_year, data = df ) 
m2  = felm(lamount ~ bench_size | election_year + type, data = df ) 
m3  = felm(lamount ~ bench_size   |election_year + type + general_party, data = df ) 
m4  = felm(lamount ~ bench_size| election_year + type + general_party + incumbency_status, data = df ) 

stargazer(m1, m2, m3, m4, type = "latex", keep.stat = "n",
          add.lines = list(c("Year FE", "Y", "Y", "Y", "Y"),
                           c("Contributor group", "", "Y", "Y", "Y"),
                           c("Party", "", "", "Y", "Y"),
                           c("Incumbency status", "", "", "", "Y")))

m1  = felm(lamount ~ bench_size  | election_year, data = df_firms ) 
m2  = felm(lamount ~ bench_size | election_year + type, data = df_firms ) 
m3  = felm(lamount ~ bench_size   |election_year + type + general_party, data = df_firms ) 
m4  = felm(lamount ~ bench_size| election_year + type + general_party + incumbency_status, data = df_firms ) 

stargazer(m1, m2, m3, m4, type = "latex", keep.stat = "n",
          add.lines = list(c("Year FE", "Y", "Y", "Y", "Y"),
                           c("Contributor group", "", "Y", "Y", "Y"),
                           c("Party", "", "", "Y", "Y"),
                           c("Incumbency status", "", "", "", "Y")))

# Adding election outcomes dummy
df = df %>% 
     filter(candidate_status %in% c("Won", "Lost")) %>% 
     mutate(candidate_won = 1 * (candidate_status == "Won"))

df_firms = df_firms %>% 
  filter(candidate_status %in% c("Won", "Lost")) %>% 
  mutate(candidate_won = 1 * (candidate_status == "Won"))

# Regressions for election outcomes
m1  = felm(candidate_won ~ lamount  | election_year, data = df ) 
m2  = felm(candidate_won ~ lamount | election_year + type, data = df ) 
m3  = felm(candidate_won ~ lamount   |election_year + type + general_party, data = df ) 
m4  = felm(candidate_won ~ lamount| election_year + type + general_party + incumbency_status, data = df ) 

stargazer(m1, m2, m3, m4, type = "latex", keep.stat = "n",
          add.lines = list(c("Year FE", "Y", "Y", "Y", "Y"),
                           c("Contributor group", "", "Y", "Y", "Y"),
                           c("Party", "", "", "Y", "Y"),
                           c("Incumbency status", "", "", "", "Y")))


m1  = felm(candidate_won ~ lamount  | election_year, data = df_firms ) 
m2  = felm(candidate_won ~ lamount | election_year + type, data = df_firms ) 
m3  = felm(candidate_won ~ lamount   |election_year + type + general_party, data = df_firms ) 
m4  = felm(candidate_won ~ lamount| election_year + type + general_party + incumbency_status, data = df_firms ) 

stargazer(m1, m2, m3, m4, type = "latex", keep.stat = "n",
          add.lines = list(c("Year FE", "Y", "Y", "Y", "Y"),
                           c("Contributor group", "", "Y", "Y", "Y"),
                           c("Party", "", "", "Y", "Y"),
                           c("Incumbency status", "", "", "", "Y")))


###########################################################################

# Step 5: Exporting cleaned dataset

write.csv(df, "individual_donations_cleaned.csv")
write.csv(df_firms, "firm_donations_cleaned.csv")
