install.packages('tidyverse')
install.packages('nycflights13')
install.packages('skimr')

rm(list = ls())

library(tidyverse)
library(nycflights13)
library(skimr)

###### O pipe 

# %>% --> esse é o pipe. Aperte Ctrl + Shift + M para usá-lo

# O pipe funciona como uma função composta, dessa forma:

# x %>% f %>% g %>% h é o mesmo que h(g(f(x)))

# Mas ele é realmente útil para fazer uma sequênca de operações, por exemplo:

menos <- function(x, y) x - y

100 %>% menos(30) %>% menos(30) %>% menos(70, .) 

# O '.' simboliza onde da função os dados anteriores entrarão
# Por padrão, o '.' está no primeiro argumento, mas é possível alterar isso
# explicitando o local do .

# Modo que eu uso para deixar organizado:

100 %>% 
  menos(30) %>% 
  menos(30) %>% 
  menos(70, .) 

###### Introdução ao DPLYR

## Utilizaremos alguns operadores básicos:
# filter(): filtra observações de uma tabela com base em um operador lógico
# arrange(): ordena as observações de acordo com critérios inseridos
# select(): seleciona variáveis que serão utilizadas na análise
# mutate() e transmute(): cria novas variáveis que são função das variáveis existentes
# summarise(): colapsa as observações em uma ou mais medidas resumo
# group_by(): muda o escopo de análise agrupando de acordo com uma variável discreta

### Mas antes: introdução a operadores lógicos

# O básico: >, <, ==, !=, >=, <=, retornam valores TRUE ou FALSE 
# TRUE e FALSE podem ser intepretados numericamente como 1 e 0

2 > 3
3 == 3
3 != 2
x <- 1:10 == c(1:5, 11:15)
sum(x)
mean(x)

# '!' funciona como uma negação do operador

2 > 3
!(2 > 3)

# Operadores '&' ('e') e '|' ('ou') 

2 > 3 & 3 == 3
2 > 3 | 3 == 3
3 == 3 | 4 == 4

### Voltando ao DPLYR

?flights
flights
skim(flights)

# filter()

flights %>% 
  filter(month == 1) # Vôos de janeiro

flights %>% 
  filter(month == 1 & day == 1) # vôos de primeiro de janeiro

flights %>% 
  filter(month == 1,
         day == 1) # uma alternativa

flights %>% 
  filter(month == 11 | month == 12) # vôos de novembro ou dezembro

flights %>% 
  filter(month %in% c(11, 12)) # uma alternativa

is.na(flights$dep_delay) # função que retorna um vetor lógico indicando se o dado é ausente ou não

flights %>% 
  filter(is.na(dep_delay)) # seleciona os valores nulos de dep_delay

flights %>% 
  filter(!is.na(dep_delay)) # seleciona os valores não nulos de dep_delay

unique(flights$carrier) # Companhias aéreas únicas
table(flights$carrier) # uma alternativa

flights %>% 
  filter(carrier %in% c("UA", "AA", "B6")) # vôos feitos por essas carriers

# arrange()

flights %>% arrange(dep_delay)

flights %>% arrange(desc(dep_delay)) # descendente

flights %>% 
  filter(month == 12) %>% 
  arrange(desc(month), day)

# select()

flights %>% 
  select(year, month, day)

flights %>% 
  select(starts_with("dep"))

flights %>% 
  select(contains("time"))

flights %>% 
  select(year:arr_delay)

flights %>% 
  select(-year)

flights %>% 
  select(-c(year:dep_time))

flights %>% 
  select(-contains("time"))

flights %>% 
  select(day, month, year, everything())

?select_helpers

# Isso é o tidyverse

flights %>%
  filter(origin == "JFK", dest == "LAX") %>%
  select(carrier, dep_delay) %>%
  arrange(desc(dep_delay))

# mutate() --> cria uma nova coluna

flights %>%
  select(year:day, ends_with("delay"), distance, air_time) %>%
  mutate(gain = dep_delay - arr_delay,
         speed = distance / (air_time/ 60))

flights %>%
  select(year:day, ends_with("delay"), distance, air_time) %>%
  mutate(gain = dep_delay - arr_delay,
         hours = air_time / 60,
         speed = distance / hours) # é possível se referir a variáveis que acabou de criar

# transmute() --> cria nova coluna e elimina todas as outras

flights %>%
  select(year:day, ends_with("delay"), distance, air_time) %>%
  transmute(gain = dep_delay - arr_delay,
            hours = air_time / 60,
            speed = distance / hours)

flights %>%
  filter(!is.na(dep_time)) %>%
  mutate(std_dep_time = (dep_time - mean(dep_time)) / sd(dep_time)) %>%
  select(std_dep_time, everything())

###

?ranking

x <- c(25, 2, 7, 3, 3, 3)

min_rank(x)

row_number(x)
dense_rank(x)
percent_rank(x)
cume_dist(x)
ntile(x, 3)

flights %>%
  filter(month == 1,day == 1, !is.na(dep_delay)) %>%
  select(carrier, dep_delay) %>%
  mutate(position = min_rank(dep_delay))

# summarise() --> criar medidas resumo

flights %>%
  filter(!is.na(dep_delay)) %>%
  summarise(mean_delay = mean(dep_delay), number_of_flights = n())

# group_by() --> cria novas unidades de análise

flights %>%
  filter(!is.na(dep_delay)) %>% # nesse caso, a unidade de análise são os diferentes grupos de carriers
  group_by(carrier) %>%
  summarise(mean_delay = mean(dep_delay), number_of_flights = n()) %>%
  arrange(desc(mean_delay))

airlines %>% 
  filter(carrier %in% c("F9", "EV"))

flights %>%
  filter(!is.na(dep_delay)) %>%
  group_by(carrier) %>%
  summarise(mean_delay = mean(dep_delay), number_of_flights = n()) %>%
  mutate(position = min_rank(mean_delay)) %>%
  arrange(desc(mean_delay))

flights %>%
  filter(!is.na(dep_delay)) %>%
  group_by(carrier) %>%
  summarise(mean_delay = mean(dep_delay), number_of_flights = n()) %>%
  mutate(position = min_rank(mean_delay)) %>%
  left_join(airlines)

flights %>%
  filter(!is.na(dep_delay)) %>%
  group_by(month) %>%
  summarise(mean_delay = mean(dep_delay), number_of_flights = n()) %>%
  arrange(desc(mean_delay))

flights %>%
  filter(!is.na(dep_delay)) %>%
  group_by(year, month, day) %>%
  summarise(mean_delay = mean(dep_delay), number_of_flights = n()) %>%
  arrange(desc(mean_delay))

flights %>%
  filter(!is.na(dep_delay), !is.na(arr_delay)) %>%
  group_by(dest) %>%
  summarise(count = n(), distance = mean(distance), delay = mean(arr_delay))  %>%
  filter(count >= 500) %>%
  ggplot(mapping = aes(x = distance, y = delay)) +
  geom_point(mapping = aes(size = count), alpha = 1/3) +
  geom_smooth(se = FALSE) +
  labs(x = "Mean distance", y = "Mean delay", title = "Destinations") +
  theme_bw()