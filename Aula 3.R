
rm(list = ls())

library(tidyverse)
library(nycflights13)
library(sf)
library(spData)
library(scales)
library(ggthemes)

### mutate() agrupado

# sem agrupar

flights %>% 
  mutate(avg_delay = mean(dep_delay)) %>% # não funciona pois existem valores ausentes em dep_delay
  select(avg_delay, everything()) 

flights %>% 
  filter(!is.na(dep_delay)) %>% # tirar valores ausentes para poder tirar a média
  mutate(avg_delay = mean(dep_delay)) %>% 
  select(avg_delay, everything())

flights %>% 
  mutate(avg_delay = mean(dep_delay, na.rm = TRUE)) %>% # uma alternativa para não ter que excluir os dados ausentes
  select(avg_delay, everything())

# agrupando

flights %>% 
  filter(!is.na(dep_delay)) %>%
  group_by(carrier) %>% 
  mutate(avg_delay = mean(dep_delay)) %>% 
  select(avg_delay, everything())

# Padronização geral

flights %>% 
  filter(!is.na(arr_delay)) %>% 
  mutate(std_delay = (arr_delay - mean(arr_delay)) / sd(arr_delay)) %>% 
  select(carrier, std_delay)

# Padronização por carrier

flights %>% 
  filter(!is.na(arr_delay)) %>% 
  group_by(carrier) %>% 
  mutate(std_delay = (arr_delay - mean(arr_delay)) / sd(arr_delay)) %>% 
  select(carrier, std_delay)


################### Tidy Data


# Tidy data = Dados organizados
# Contrapartida: messy data
# Baseado em 3 regras básicas:

# 1) Cada variável em sua coluna
# 2) Cada observação na sua linha
# 3) Cada valor na sua célula

# tabelas do tidyverse sobre casos de tuberculose entre países

table1 # está no formato tidy

# essas outras não estão tidy

table2
table3

table4a
table4b

# quando uma variável está espalhada pelas colunas: pivot_longer()

tidy4a <- table4a %>% 
  pivot_longer(-country, names_to = "year", values_to = "cases")

tidy4b <- table4b %>% 
  pivot_longer(-country, names_to = "year", values_to = "population")

table4 <- tidy4a %>% 
  left_join(tidy4b, by = c("country", "year"))

table4 # está no formato tidy

# quando uma observação está espalhada por várias linhas: pivot_wider()

tidy2 <- table2 %>% 
  pivot_wider(names_from = type, values_from = count)

tidy2 # está no formato tidy

# quando uma coluna contém mais de uma variável: separate

tidy3 <- table3 %>% 
  separate(rate, 
           into = c("cases", "population"),
           sep = "/",
           convert = TRUE)

tidy3 # está no formato tidy

tab <- table3 %>% 
  separate(year, into = c("century", "year"), sep = 2) # separar a cada dois dígitos

tab %>% 
  unite(new, century, year, sep = "") %>% 
  rename(year = new)

# Estudo de caso 1: Billboard

billboard
?billboard

tidy_billboard <- billboard %>% 
  pivot_longer(-c(artist:date.entered), names_to = "week", 
               values_to = "position", values_drop_na = TRUE) %>% 
  mutate(week = as.integer(str_remove(week, "wk")))

tidy_billboard %>% 
  count(track, sort = TRUE) %>% 
  filter(min_rank(desc(n)) <= 19) %>% 
  mutate(track = fct_reorder(track, n)) %>% 
  ggplot(aes(n, track, fill = track)) +
  geom_col(show.legend = FALSE) +
  labs(x = "Número de dias na billboard",
       y = "",
       title = "Tracks que permaneceram maior tempo na billboard")

# Estudo de caso 2: WHO (world health organization)

who
?who

tidy_who <- who %>%
  pivot_longer(-c(country:year), names_to = "key", values_to = "cases") %>% 
  mutate(key = str_replace(key, "newrel", "new_rel")) %>% 
  separate(key, into = c("new", "type", "sex_age"), sep = "_") %>% 
  separate(sex_age, into = c("sex", "age"), sep = 1) %>% 
  mutate(sex = str_to_upper(sex)) %>% 
  separate(age, into = c("min_age", "max_age"), sep = -2) %>% 
  unite("age", min_age:max_age, sep = "-") %>% 
  mutate(age = ifelse(age == "-65", "65+", age)) %>% 
  select(-c("iso3", "new"))

tidy_who %>% 
  count(country)

tidy_who %>% 
  count(year) %>% view

tidy_who %>% 
  group_by(country, year) %>% 
  filter(!is.na(cases), 
         country %in% c("Brazil", "United States of America",
                        "Russian Federation", "China",
                        "India")) %>% 
  summarise(total_cases = sum(cases)) %>% 
  ggplot(aes(year, total_cases, color = country)) +
  geom_line(size = 2) +
  scale_y_continuous(labels = number_format(scale = 1/1000, suffix = "k")) +
  labs(x = "Ano",
       y = "# Total de casos",
       title = "Avanço da tuberculose",
       subtitle = "entre 1995 e 2015",
       caption = "Fonte: World Health Organization")

# mapa de 2013    

tidy_who %>% 
  filter(year == 2013) %>% 
  group_by(iso2) %>% 
  summarise(total_cases = sum(cases, na.rm = TRUE)) %>% 
  left_join(world, by = c("iso2" = "iso_a2")) %>%
  ggplot() +
  geom_sf(data = world, aes(geometry = geom), fill = "lightgrey") +
  geom_sf(aes(geometry = geom, fill = total_cases)) +
  scale_fill_gradient(low = "#fff7bc", high = "red",
                      labels = number_format(scale = 1 / 1000,
                                             suffix = "k"),
                      guide = "legend") +
  theme_map() +
  theme(panel.background = element_rect(fill = "lightblue"),
        legend.position = "right") +
  labs(fill = "# de Casos",
       title = "Casos de tuberculose",
       subtitle = "em 2013",
       caption = "Fonte: World Health Organization")

# todos os anos juntos

tidy_who %>% 
  group_by(country, iso2, year) %>% 
  summarise(total_cases = sum(cases, na.rm = TRUE)) %>% 
  left_join(world, by = c("iso2" = "iso_a2")) %>%
  ggplot() +
  geom_sf(data = world, aes(geometry = geom), fill = "#fff7bc") +
  geom_sf(aes(geometry = geom, fill = total_cases)) +
  scale_fill_gradient(low = "#fff7bc", high = "red",
                      labels = number_format(scale = 1 / 1000,
                                             suffix = "k"),
                      guide = "legend") +
  facet_wrap(vars(year)) +
  theme_map() +
  theme(panel.background = element_rect(fill = "lightblue"),
        legend.position = "right") +
  labs(fill = "# de Casos",
       title = "Casos de tuberculose",
       caption = "Fonte: World Health Organization")

# Antes de 2000

tidy_who %>% 
  filter(year <= 2000) %>% 
  group_by(country, iso2, year) %>% 
  summarise(total_cases = sum(cases, na.rm = TRUE)) %>% 
  left_join(world, by = c("iso2" = "iso_a2")) %>%
  ggplot() +
  geom_sf(data = world, aes(geometry = geom), fill = "#fff7bc") +
  geom_sf(aes(geometry = geom, fill = total_cases)) +
  scale_fill_gradient(low = "#fff7bc", high = "red",
                      labels = number_format(scale = 1 / 1000,
                                             suffix = "k"),
                      guide = "legend") +
  facet_wrap(vars(year)) +
  theme_map() +
  theme(panel.background = element_rect(fill = "lightblue"),
        legend.position = "right") +
  labs(fill = "# de Casos",
       title = "Casos de tuberculose",
       caption = "Fonte: World Health Organization")

# Depois de 2000

tidy_who %>% 
  filter(year > 2000) %>% 
  group_by(country, iso2, year) %>% 
  summarise(total_cases = sum(cases, na.rm = TRUE)) %>% 
  left_join(world, by = c("iso2" = "iso_a2")) %>%
  ggplot() +
  geom_sf(data = world, aes(geometry = geom), fill = "#fff7bc") +
  geom_sf(aes(geometry = geom, fill = total_cases)) +
  scale_fill_gradient(low = "#fff7bc", high = "red",
                      labels = number_format(scale = 1 / 1000,
                                             suffix = "k"),
                      guide = "legend") +
  facet_wrap(vars(year)) +
  theme_map() +
  theme(panel.background = element_rect(fill = "lightblue"),
        legend.position = "right") +
  labs(fill = "# de Casos",
       title = "Casos de tuberculose",
       caption = "Fonte: World Health Organization")

# um gráfico para cada ano de uma vez

tidy_who %>% 
  group_by(country, iso2, year) %>% 
  summarise(total_cases = sum(cases, na.rm = TRUE)) %>% 
  left_join(world, by = c("iso2" = "iso_a2")) %>%
  ungroup() %>% 
  group_split(year) %>% 
  map(~ ggplot(data = .x) +
        geom_sf(data = world, aes(geometry = geom), fill = "#fff7bc") +
        geom_sf(aes(geometry = geom, fill = total_cases)) +
        scale_fill_gradient(low = "#fff7bc", high = "red",
                            labels = number_format(scale = 1 / 1000,
                                                   suffix = "k"),
                            guide = "legend") +
        facet_wrap(vars(year)) +
        theme_map() +
        theme(panel.background = element_rect(fill = "lightblue"),
              legend.position = "right") +
        labs(fill = "# de Casos",
             title = "Casos de tuberculose",
             caption = "Fonte: World Health Organization"))
