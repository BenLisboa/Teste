rm(list = ls())

library(tidyverse)
library(nycflights13)

flights
airlines
airports
planes
weather

# Chave primária: identifica em cada tabela as suas linhas de maneria única
# Chave estrangeira: indica como a tabela relaciona-se com outra tabela
# Simples: definida por uma única variável
# Composta: definida por várias variáveis
# Ordem do relacionamento: 1 para N ou N para N

planes %>% 
  count(tailnum) %>% 
  filter(n > 1) # ou seja, tailnum é uma chave primária da tabela planes (dizer o tailnum deixa claro a qual observação se faz referência)

# algumas vezes a tabela não tem uma chave primária

weather %>% 
  count(year, month, day, hour, origin) %>% 
  filter(n > 1) # essa tabela tem um problema: 3 dos dados se repetem (portanto, não tem chave primária)

flights %>% 
  count(year, month, day, flight) %>% 
  filter(n > 1) # o número do vôo muitas vezes é reaproveitado

flights %>% 
  count(year, month, day, tailnum) %>% 
  filter(n > 1) # um avião pode voar mais de uma vez por dia

# chave substituta (surrogate key)

flights %>% 
  mutate(id = row_number()) %>% 
  select(id, everything())

# Mutating joins (nome devido ao fato que eles esticam a tabela, criam novas colunas)

alunos <- tibble(nome = c("João", "Maria", "Pedro", "José", "Francisco"),
                 cod = c("FGV", "INS", "FEA", "PUC", "INS"))

escolas <- tibble(cod = c("FGV", "INS", "FEA", "UAM"),
                  inst = c("Getúlio Vargas", "Insper", "Economia USP", "Anhembi"))

# Mantém dados da tabela da esquerda (primeira a ser mencionada) e relaciona com valores que tem na direita
alunos %>% 
  left_join(escolas, by = "cod") # Nesse caso, o by está implícito e não é necessário. O R tenta adivinhar a chave observando os nomes em comum entre as tabelas

# Mantém dados da tabela da direita e relaciona com valores que tem na esquerda
alunos %>% 
  right_join(escolas) # Natural join (ausência do by). o by continua implícito

# O mais exclusivo dos joins, é uma mistura dos dois anteriores. Imprime os dados comuns nas duas tabelas
alunos %>% 
  inner_join(escolas, by = "cod")

# O mais inclusivo dos joins, é o contrário do inner_join. Inclui todos os dados das duas tabelas
alunos %>% 
  full_join(escolas, by = "cod")

# O join mais comum é o left_join, e o segundo mais comum é o inner_join


escolas2 <- tibble(sigla = c("FGV", "INS", "FEA", "UAM"),
                   inst = c("Getúlio Vargas", "Insper", "Economia USP", "Anhembi"))

alunos %>% 
  left_join(escolas2) # Não funciona, uma vez que não tem colunas com o mesmo nome

alunos %>% 
  left_join(escolas2, by = c("cod" = "sigla")) # vetores nomeados são utilizados para indicar as colunas a serem relacionadas

# x <- c("A", "B", "C")
# x <- c("Primeira" = "A", "Segunda" = "B", "Terceira" = "C")
# x["Primeira"]
# unname(x)

alunos %>% 
  inner_join(escolas2, by = c("cod" = "sigla")) # vetores nomeados são utilizados para indicar as colunas a serem relacionadas

####

flights2 <- flights %>%
  select(year:day, hour, origin, dest, tailnum, carrier)

flights2 %>% 
  left_join(airlines, by = "carrier") %>%
  select(-origin, -dest)

flights2 %>% 
  left_join(weather) # natural join. Cuidado! Às vezes colunas com o mesmo nome podem significar coisas diferentes

flights2 # year: ano do vôo
planes # year: ano de fabricação do avião

# portanto, é errado fazer:
flights2 %>% 
  left_join(planes) # o natural join relaciona as colunas erradas

# o correto seria:
flights2 %>% 
  left_join(planes, by = "tailnum")

flights2 %>% 
  left_join(airports = c("dest" = "faa"))

# filtering joins: semi_join() e anti_join()

# semi_join() filtra a tabela da esquerda por observações que possuem valores equivalentes na tabela da direita
# porém, não adiciona nenhuma coluna, apenas filtra. O resultado final tem o mesmo número de colunas que tinha no início

# anti_join() tem o efeito contrário. Descarta todas as linhas da tabela da esquerda que possuem valores equivalentes na tabela da direita
# muito utilizado para debugar

top_dest <- flights %>% 
  count(dest, sort = TRUE) %>% 
  head(10)

flights %>% 
  filter(dest %in% top_dest$dest)

flights %>% 
  semi_join(top_dest)

airports %>% 
  semi_join(flights, c("faa" = "dest")) %>% 
  ggplot(aes(lon, lat)) +
  borders("state") +
  geom_point() +
  coord_quickmap() +
  theme_void()

?borders
?maps::map

airports %>% 
  semi_join(flights, c("faa" = "dest")) %>% 
  count(lon, name)

###

flights2 %>% 
  filter(!is.na(tailnum))
anti_join(planes, by = "tailnum") %>% 
  count(tailnum, sort = TRUE) # aviões cujos números de cauda não aparecem na base de dados do governo

###

library(ggthemes)
library(sf)
library(spData)
library(scales)

db <- read.csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv", stringsAsFactors = FALSE)

# View(db)

today <- db %>% 
  select(longitude = Long, latitude = Lat, total_cases = X5.10.20) %>% 
  filter(total_cases >= 1000)

spData::world

ggplot(world) +
  geom_sf(aes(fill = continent), alpha = 0.75) +
  geom_point(data = today, aes(x = longitude, y = latitude, size = total_cases), alpha = 0.65) +
  ggtitle("COVID-19") +
  theme_wsj() +
  scale_size(labels = number_format(scale = 1/1000, suffix = "k")) +
  theme(legend.title = element_blank(),
        panel.background = element_rect(fill = "lightblue"))
