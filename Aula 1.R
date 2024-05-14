### Lógica inicial do R

## Operadores Aritméticos

# Soma
2 + 2
#Subtração
2 - 2
# Multiplicação
2 * 2
# Divisão 
10 / 3
# Divisão Modular
10 %% 3
# Expoentes
2 ^ 3
4 ^ (1/2)

## Operadores Lógicos

# Igual
5 == 8
5 == 5
# Diferente
5 != 8
5 != 5
# Maior e maior ou igual
5 > 5
5 >= 5
# Menor e menor ou igual
5 < 5
5 <= 5


## Salvando valores em variáveis e deletando elas
a = 5
a <- 5
5 -> a

# Apagar valor da memória
rm(a)
# Apagar todos valores da memória
rm(list=ls())

## Vetores
x = 1:10
y = c(2, 4, 6, 8, 10, 12, 14, 16, 18, 20)
z = c("queijo", "cenoura", "pão", "abacate")

# Acessando valores de um vetor
z[1]
z[3]
z[1:2]

## Operações com vetores
x * 2
x / y
x + y

#Média
mean(x)
#Soma
sum(x)
# Variância
var(x)
# Desvio Padrão
sd(x)

## Dataframes
df = data.frame("produtos" = c("batata", "pão", "banana", "leite"), "preços" = c(2, 3, 1, 5))

# Acessando colunas
df$produtos

# Filtrando colunas
df[df$produtos=="batata",]

# Operações com dataframes
mean(df$preços)

## Instalando tidyverse
install.packages("tidyverse")
install.packages("ggthemes")
install.packages("sf")
install.packages("spData")
install.packages("gapminder")

## Lendo pacotes
library(tidyverse)
library(ggthemes)
library(gapminder)

##################
rm(list=ls())
##################

options(scipen=999) # Remove notação científica

### gapminder
gap = filter(gapminder, year == 1997)

#1 - dispersão básica
ggplot(data = gap) +
  geom_point(mapping = aes(x = gdpPercap, y = lifeExp))

#2 - Separar continente por cor
ggplot(data = gap) +
  geom_point(mapping = aes(x = gdpPercap, y = lifeExp, color = continent))

#3 - Adicionar tamanho da população e escala logarítmica
ggplot(data = gap) +
  geom_point(mapping = aes(x = gdpPercap, y = lifeExp, color = continent, size = pop)) +
  scale_x_log10()

#4 - Geom text com label de nome dos países
ggplot(data = gap) +
  geom_text(mapping = aes(x = gdpPercap, y = lifeExp, color = continent, size = pop, label = country)) +
  scale_x_log10()

#5 - Geom Smooth básico
ggplot(data = gap, mapping = aes(x = gdpPercap, y = lifeExp)) +
  geom_point(mapping = aes(color = continent, size = pop)) +
  scale_x_log10() +
  geom_smooth(se = FALSE, method = lm)

#6 - Geom Smooth com grupos de cores
ggplot(data = gap, mapping = aes(x = gdpPercap, y = lifeExp)) +
  geom_point(mapping = aes(color = continent, size = pop)) +
  scale_x_log10() +
  geom_smooth(se = FALSE, method = lm, mapping = aes(color=continent))

#7 - Geom smooth com grupos e cor preta
ggplot(data = gap, mapping = aes(x = gdpPercap, y = lifeExp)) +
  geom_point(mapping = aes(color = continent, size = pop)) +
  scale_x_log10() +
  geom_smooth(se = FALSE, method = lm, mapping = aes(group = continent), color = "black")

#8 - Facet wrap de continentes
ggplot(data = gap, mapping = aes(x = gdpPercap, y = lifeExp)) +
  geom_point(mapping = aes(color = continent, size = pop)) +
  scale_x_log10() +
  geom_smooth(se = FALSE, method = lm, mapping = aes(color = continent)) +
  facet_wrap(~continent, scales = "free")

#9 - Regressão com theme bw
ggplot(data = gap, mapping = aes(x = gdpPercap, y = lifeExp)) +
  geom_point(mapping = aes(color = continent, size = pop)) +
  scale_x_log10() +
  geom_smooth(se = FALSE, method = lm, mapping = aes(group = continent), color = "black") +
  theme_bw()

#10 - Theme do stata no facet wrap
ggplot(data = gap, mapping = aes(x = gdpPercap, y = lifeExp)) +
  geom_point(mapping = aes(color = continent, size = pop)) +
  scale_x_log10() +
  geom_smooth(se = FALSE, method = lm, mapping = aes(color = continent)) +
  facet_wrap(vars(continent), scales = "free") +
  theme_stata()

############################
### Mapas

## Lendo pacotes específicos para mapas
library(sf)
library(spData)

## Lendo dataframe de explosões nucleares
nuclear_explosions = read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-08-20/nuclear_explosions.csv")

mundo = world # df world vem do pacote spData
brasil = world[world$name_long=="Brazil",] # Exemplo de selecionar um país

# Mapa em branco
ggplot(data=brasil)+
  geom_sf()

# Vendo explosões nucleares no mundo
ggplot() +
  geom_sf(data=world, mapping=aes(geometry=geom)) +
  geom_point(data=nuclear_explosions, aes(x=longitude, y=latitude, size=10*yield_lower, color=country), alpha=0.5)+
  theme_map() +
  scale_size_continuous(range = c(2, 10))+
  theme(panel.background=element_rect(fill="gray"), legend.position = "bottom")+
  labs(size="Explosion Magnitude in ktons of TNT", title="Nuclear Explosions Around the World")
