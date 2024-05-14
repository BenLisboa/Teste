---
  output:
  html_document:
  df_print: paged
toc: yes
toc_float:
  collapsed: yes
smooth_scroll: yes
number_section: yes
theme: readable
highlight: haddock
---
  
  </br>
  
  ```{r, out.width = "350px",echo=FALSE}
knitr::include_graphics("C:\\Users\\USER\\Documents\\Insper Data\\Logo\\INSPER DATA LOGO\\Logo Horizontal Preferencial\\Logo Colorido\\insperdata_logo_preferencial.png")
```

</br>
  
  # Econometria: Regressão Linear no R
  
  </br>
  
  ## Pacotes utilizados para a regressão linear
  
  </br>
  
  ```{r message=FALSE}
library(skimr)
```

Ferramenta para análise exploratória dos dados.

```{r message=FALSE}
library(tidyverse)
```

Ferramenta completa para análise de dados.

```{r message=FALSE}
library(tidymodels)
```

Além de ser uma forte ferramenta de modelagem (utilizando o parsnip, que não veremos por agora), inclui o pacote broom, que transforma o output de um modelo de regressão linear em uma base de dados tidy.

```{r message=FALSE}
library(olsrr)
```

Possui diversos testes que validam as suposições do modelo de regressão linear.

```{r message=FALSE}
library(tseries)
```

Possui o teste jarque-bera, que testa a normalidade de um conjunto de dados. Em geral o Shapiro-Wilk test performa melhor, mas o teste Jarque-Bera é o ensinado no curso de econometria.

```{r message=FALSE}
library(estimatr)
```

Possui uma função de regressão linear com erros padrões robustos, que nos ajuda a contornar a heterocedasticidade.

```{r message=FALSE}
library(AER)
```

Possui uma função de regressão com variáveis instrumentais, realizando a regressão linear por meio do método 2SLS (Two-Stage Least Squares).

***
  
  O único elemento diferente do curso tradicional de econometria (além da aplicação no R) é o uso do teste Breusch-Pagan de heterocedasticidade ao invés do teste de White. Apesar de ser mais fácil de pronunciar, o teste de White não é feito de forma satisfatória no R. Contudo, ele é apenas um caso especial do teste Breusch-Pagan. Além disso, o teste de White é tão específico que pode indicar outros problemas no modelo que não a heterocedasticidade, não indicando qual o problema encontrado. Dessa forma, o Breusch-Pagan é uma forma menos confusa e mais objetiva de realizar esse teste.  

```{r echo=FALSE}
theme_set(theme_bw())
```

</br>
  
  ## Base utilizada
  
  </br>
  
  A título de exemplo, utilizaremos a clássica base mtcars, já presente no R. Como ela vem como um data.frame, transformarei ela em um tibble para mais fácil manipulação. Aproveitarei para realizar uma análise exploratória com o skimr.

```{r}
tibble(mtcars)

skim(tibble(mtcars))
```

Apesar de todas as variáveis serem numéricas, é possível observar que algumas delas, como a 'cyl', são variáveis discretas. Por isso, é desejável que elas sejam tratadas como fatores, não como variáveis contínuas. O código a seguir faz exatamente isso, transformando em fatores apenas as variáveis com menos de 6 valores distintos.

```{r}
df <- 
  tibble(mtcars) %>% 
  mutate_if(~ length(unique(.x)) < 6, as.factor)
df
```

</br>
  
  ## Execução da regressão linear
  
  </br>
  
  O comando a seguir (lm) utiliza o formato de fórmulas do R. O argumento "mpg ~ ." (que é a fórmula) pode ser lido como "mpg explicado por todas as outras variáveis do modelo". O resultado dessa abordagem é idêntico ao resultado caso a fórmula fosse "mpg ~ cyl + disp + hp + drat + wt + qsec + vs + am + gear + carb", ou seja, "mpg explicado por todas essas variáveis enunciadas". Contudo, escrever assim é claramente mais trabalhoso. Se você quiser usar a maioria das variáveis, mas não todas, é possível escrever no formato "mpg ~ . - cyl", que pode ser lido como "mpg explicado por todas as variáveis enunciadas com exceção de 'cyl'".

</br>
  
  ### Resultado {.tabset .tabset-fade}
  
  #### Sem fct_relevel()
  
  ```{r}
reg <- lm(mpg ~ ., data = df)
reg
```

Com isso, é estimado nosso modelo de regressão linear. Ao imprimirmos "reg", o console nos mostra as estimativas dos parâmetros de cada um dos fatores da regressão, mas não nos mostra as estatísticas da regressão.

É interessante notar que, automaticamente, o R reconhece que variáveis em formato de fatores são variáveis dummy e adiciona essas variáveis em quantidade certa, deixando sempre uma dummy como valor base. É possível alterar a dummy base manipulando a variável por meio da função fct_relevel().

#### Com fct_relevel()

```{r}
df1 <- 
  df %>% 
  mutate(am = fct_relevel(am, "1"))

reg1 <- lm(mpg ~ ., data = df1)
reg1
```

É possível perceber que, enquanto na regressão anterior o valor "0" da variável "am" tinha ficado oculto, usando o fct_relevel() foi possível transformar o valor oculto em "1", sendo esse o novo valor base.

</br>
  
  ### Adaptação ao tidyverse
  
  A função summary pertence ao R base e nos mostra a mesma saída que a função 'reg' do Stata. 

```{r}
summary(reg)
```

Essencialmente, o summary possui as principais estatísticas para a análise do modelo. Contudo, apesar de muito positivo nesse aspecto, peca por não sair em formato tidy. Para isso, usamos o pacote "broom", que possui 3 funções muito úteis na análise de regressões. A função tidy() foca nos parâmetros, mostrando seus valores e estatísticas em formato tidy. A função augment() foca nos resultados do modelo, sendo útil para extrair os resíduos e os resíduos padronizados do modelo. Por último, a função glance() foca na qualidade do modelo, analisando medidas como o $R^2$ e o teste F-parcial da regressão.

É bom relembrar que, uma vez com as tabelas tidy, essas poderão ser manipuladas assim como qualquer outro tibble pelo tidyverse.

```{r}
tidy(reg)
augment(reg)
glance(reg)
```

</br>
  
  ### Outras formas funcionais
  
  </br>
  
  Por enquanto tratamos apenas de formas funcionais em que não há manipulação matemática das variáveis. Contudo, existem diversas outras maneiras de modelar uma relação entre variáveis: modelos logarítmicos, quadráticos, etc. Mostraremos agora como fazer essas manipulações agora

Em um modelo log-nível, log-log ou nível-log, basta aplicar a operação log() na variável desejada. Essa operação funciona dessa maneira simples porque log() entra como uma função, e dessa forma já é reconhecida pelo R como a nova forma funcional da variável.

```{r}
lm(mpg ~ log(hp) + cyl, data = df)
```

No caso de um modelo quadrático, no entanto, como o operador "^" não é reconhecido como uma função, simplesmente colocarmos hp^2 não é o suficiente para o modelo reconhecer a variável como uma quadrática. 

```{r}
lm(mpg ~ hp^2 + cyl, data = df)
```
Como podemos observar, o modelo não reconhece o hp como uma variável que deve ser elevada ao quadrado. Isso ocorre por reconhecer apenas a variável, não o operador.

Para consertar esse problema podemos usar a função I(), que permite que o R interprete a variável como uma quadrática.

```{r}
lm(mpg ~ I(hp^2) + cyl, data = df)
```

</br>
  
  ### Interação
  
  </br>
  
  uando o efeito de uma variável explicativa na variável resposta depende do valor de uma outra variável resposta, podemos dizer que há uma interação entre ambas. Esse fenômeno pode ser traduzido como uma multiplicação entre as duas no modelo e é exatamente assim que o R reconhece uma interação.

Supondo uma interação entre duas variáveis contínuas x1 e x2, podemos interpretar que o efeito da variação de x1 na variável resposta do modelo será diferente dependendo do valor em que a variável x2 estiver. Se a interação ocorrer entre uma variável contínua x1 e uma dummy x2, podemos interpretar que o coeficiente dessa interação representará o efeito adicional de x1 na variável resposta caso a x2 seja igual a um.

Existem duas formas de realizar essa operação no R:
  
  Podemos usar o operador "*", que vai multiplicar uma variável pela outra e inserir no modelo a interação entre ambas, além de incluir as próprias variáveis de forma isolada.

```{r}
lm(mpg ~ hp * cyl, data = df)
```

Dessa forma, é possível observar que o modelo possui, além da interação em si, a variável hp e as variáveis dummys de cyl apresentadas isoladamente.

Às vezes isso não é desejável, e para isso usamos o operador ":", que nos permite inserir uma interação sem inserir as variáveis isoladamente.

```{r}
lm(mpg ~ hp : cyl, data = df)
```

</br>
  
  ## Validando as suposições
  
  </br>
  
  O modelo de regressão linear depende de 6 suposições:
  
  1. O modelo de regressão é linear nos parâmetros

2. A amostragem é aleatória
+ independente
+ identicamente distribuída

3. Existe variação amostral no regressor
+ os regressores apresentam variação amostral
+ não há relação linear perfeita entre os regressores

4. A média condicional do erro é zero $\Rightarrow E(\varepsilon|x_i) = 0$
  + o valor do erro não pode depender do valor de nenhuma variável
+ existe intercepto
+ $x_i$ é exógeno
+ ausência de correlação entre o termo de erro aleatório e as variáveis independentes

5. Homocedasticidade $\Rightarrow Var(\varepsilon|x) = \sigma^2$
  + o termo de erro aleatório tem a mesma variância dado quaisquer conjunto de valores para os regressores

6. O termo de erro aleatório não observável, $\varepsilon$, é:
  + independente dos regressores $x_i$
  + normalmente distribuído, com média zero e variância $\sigma^2_\varepsilon$ $\Rightarrow ~ \sim N(0;\sigma^2_\varepsilon)$
  
  </br>
  
  Algumas suposições são validadas apenas olhando para o modelo escolhido ou para a amostra, como as suposições 1 a 3. Outras, como a 4, são validadas por meio da argumentação (a 4 exige uma validação da exogeneidade das variáveis independentes do modelo). Outras, como é o caso das suposições 5 e 6, são validadas por meio de testes com o modelo. Estudaremos agora como realizar esses testes no R de maneira simples.

</br>
  
  ### Homocedasticidade
  
  </br>
  
  Durante a matéria de econometria, nos acostumamos a usar o teste de White para testar a homodasticidade do modelo. Contudo, um teste mais utilizado e de mais fácil acesso no R é o teste de Breusch-Pagan. No final o resultado é o mesmo: o teste consiste em um teste de hipótese em que Ho é a homocedasticidade e Ha é a heterocedasticidade. Caso o p-valor do teste seja menor do que o nível de significância escolhido, a hipótese nula é rejeitada e o teste passa a indicar a heterocedasticidade do modelo.

A função abaixo vem do pacote "olsrr", que possui diversos testes para o modelo de regressão linear por MQO.

```{r}
ols_test_breusch_pagan(reg)
```

No caso desse exemplo o p-valor é 0.4093, maior do que qualquer nível de significância racional. Dessa forma, é possível concluir que há evidências de que a quinta suposição do modelo de regressão linear é válida.

</br>
  
  ### Normalidade dos resíduos
  
  </br>
  
  Assim como no teste de homocedasticidade, utilizaremos um teste de normalidade diferente do que vemos na sala de aula. Segundo [um paper do Journal of Statistical Computation and Simulation](https://www.tandfonline.com/doi/pdf/10.1080/00949655.2010.520163), que analisou a qualidade de cada teste de normalidade empiricamente, o teste jarque-bera, utilizado em sala de aula, tem performance consistentemente pior ao que usaremos no R, o teste Shapiro-Wilk. Além de menos consistente, a interface do teste JB no R é menos consistente com o tidyverse do que a interface do teste SW. Enquanto o SW aceita uma especificação de modelo (reg), o JB aceita apenas um vetor contendo os resíduos do modelo, sendo menos prático.

Contudo, mostrarei a forma de realizar o teste JB no R também, apesar de não indicar.

O teste SW também é proveniente do pacote "olsrr", enquanto o teste JB vem do pacote "tseries".

Assim como no teste de heterocedasticidade, o teste de normalidade é composto por um teste de hipóteses, onde Ho é a normalidade da distribuição e Ha é a não normalidade da distribuição. Um p-valor abaixo do nível de significância indica a não normalidade da distribuição, enquanto um p-valor alto indica a normalidade da distribuição.

```{r}
ols_test_normality(reg) # Shapiro-Wilk test
jarque.bera.test(reg$residuals) # Jarque-Bera test
```


Dessa forma, quanto no teste JB quanto no SW é possível observar que essa distribuição pode ser considerada normal. 

Interessante notar que o comando ols_test_normality() retorna o resultado de mais de um teste de normalidade. O melhor continua sendo o SW, mas mostra a solidez desse pacote na análise de dados.

</br>
  
  ## Correção de erros nas suposições
  
  </br>
  
  Mas e quando as suposições não são validadas? Quando o modelo, não importa o que você faça, continua heterocedástico? Quando você sabe que uma das variáveis é endógena?
  
  Existem algumas técnicas que são utilizadas para corrigir esses problemas. Abordarei aqui duas delas: a regressão robusta e as variáveis instrumentais.

</br>
  
  ### Lidando com variáveis endógenas
  
  </br>
  
  Quando um regressor “X1” é identificado com endógeno ao modelo, a estimativa dos parâmetros se torna viesada, anulando a validade da regressão. Portanto, a necessidade de corrigir a endogeneidade torna-se evidente. Uma opção é a utilização de variáveis de instrumento: variáveis capazes de explicar a variável X1, mas que não afetam diretamente a variável resposta Y do modelo. Utilizando variáveis instrumentais conseguimos isolar a porção enxógena de variáveis endógenas, tornando nossas estimativas estatisticamente válidas.

Essa metodologia também resulta em uma regressão de dois estágios (2SLS), pois está sendo realizada uma regressão para a variável resposta Y do modelo e outra para a variável X1. 

Para a introdução de instrumentos em ume regressão no R, é recomendável a utilização da função ivreg() do pacote ‘AER’. Um exemplo genérico seria:
  
  ivreg(Y ~ X + W | W + Z, ... )

Sendo que Y é a variável resposta do modelo, X é uma variável endógena, W é uma variável exógena de controle e Z é um instrumento para X.

Trazendo para o nosso exemplo do mtcars (sem ser criterioso em relação a que variáveis são endógenas ou exógenas, só focando na metodologia em si), a regressão pode seguir esse formato:
  
  ```{r}
iv <- ivreg(mpg ~ hp + gear + drat + qsec + disp| . - gear - hp + cyl + carb, data = df)
iv
```

Ao lado esquerdo fica a regressão linear que será utilizada. Ao lado direito, separadas pelo operador "|", ficam todas as variáveis exógenas que serão utilizadas, regressores ou variáveis instrumentais. O "." utilizado "puxa" todas as variáveis do modelo como exógenas, e o sinal de menos em "gear" e "hp" indica que essas variáveis do modelo na verdade são endógenas. "Cyl" e "carb", como não estão presentes no modelo, são consideradas variáveis intrumentais.

Esse formato é útil quando o modelo conta com muitos regressores, mas as duas formas de escrever (ditando variável a variável ou usando o "." e excluindo as variáveis endógenas) são válidas.

O formato do resultado desse modelo é bem parecido com o formato da lm(), funcionando com tidy(), glance() e augment(). A única diferença é que a função augment() não retorna os resíduos padronizados do modelo. Contudo, inseri abaixo um código que conserta essa diferença.

```{r}
tidy(iv)
glance(iv)
augment(iv) %>% 
  mutate(.std.resid = scale(.resid))
```

</br>
  
  ### Lidando com a heterocedasticidade
  
  </br>
  
  Quando violada a suposição de homocedasticidade (termo de erro aleatório e de variância constante), há presença de heterocedasticidade em uma regressão. Diante disso, é interessante buscar alguma metodologia que forneça resultados válidos. Uma possibilidade é a realização de uma regressão na forma robusta.

Para estimar uma regressão robusta no R, basta utilizar o comando lm_robust() do pacote 'estimatr'.

```{r}
rob <- lm_robust(mpg ~ ., data = df, se_type = "stata")
rob
```

Os resultados desse modelo contam com a heterocedasticidade no cálculo dos erros padrões das estimativas dos parâmetros, tornando os testes de hipótese válidos. Importante ressaltar que o modelo robusto não corrige a heterocedasticidade, apenas conta com ela no cálculo

Um problema é que o formato dessa regressão é diferente do formato do comando lm(), mas passarei aqui pelas análises possíveis na regressão robusta.

Primeiramente, quando rodamos o summary() da regressão o resultado é diferente, mas ainda possui as informações importantes: estimativas, erros padrões, p-valor, intervalo de confiança e o $R^2$.

```{r}
summary(rob)
```

Para conseguirmos os dados do comando tidy() e do comando glance() no modelo robusto precisamos de apenas um comando a mais: o tibble(). Sem ele o modelo retorna um data frame, não sendo muito condizente com o tidyverse. Com esses comandos o resultado torna-se idêntico ao resultado com regressões lineares não robustas.

```{r}
tidy(rob) %>% tibble
glance(rob) %>% tibble
```

Contudo, o comando augment() não é aplicável a esse formato. Isso dificultaria a extração dos valores previstos, resíduos e dos resíduos padronizados, mas montei esse comando para extrair essas informações da regressão, como se utilizássemos o augment().

```{r}
rob$fitted.values %>% 
  tibble %>% 
  bind_cols(mpg = df$mpg) %>% 
  rename(.fitted = ".") %>% 
  mutate(.resid = mpg - .fitted,
         .std.resid = scale(.resid))
```

Com esses comandos deve ser possível seguir sua análise do modelo linear sem se preocupar com a heterocedasticidade do mesmo.


</br>
  
  ## Visualização do modelo
  
  </br>
  
  Algumas formas de visualização são muito convenientes para a análise do modelo de regressão linear. Claro que não existem limites, existem diversas visualizações extremamente úteis, mas aqui passarei pelas principais visualizações a serem utilizadas no processo de análise de dados.

</br>
  
  ### Regressão linear simples
  
  </br>
  
  Modelos de regressão linear simples são bons para visualizarmos a intuição do modelo de regressão linear. Essa análise é dificultada em modelos mais complexos pela existência de mais de duas dimensões no modelo. 

Supondo que iremos realizar uma regressão que explica mpg por hp, podemos visualizar esse modelo diretamente no ggplot utilizando geom_smooth():
  
  ```{r}
ggplot(df, aes(hp, mpg)) +
  geom_point() +
  geom_smooth(method = lm, se = FALSE)
```

O método indica ao R que o modelo a ser usado será o de regressão linear (lm) e se = FALSE tira o intervalo de confiança da visualização.

</br> 
  
  ### Distribuição das amostras
  
  </br>
  
  Para testarmos se as amostras são de fato aleatórias é conveniente criarmos um identificador de cada amostra. Além disso, é conveniente realizar essa análise com todas as variáveis ao mesmo tempo, contando com os resíduos e as variáveis explicativas. Dessa forma, terminamos com um diagrama que indica se houve qualquer parcialidade na retirada da amostra.

Portanto, buscamos gráficos aleatórios, sem tendência alguma. Qualquer tendência pode ser problemática.

No exemplo, extraímos as informações que queríamos do modelo com augment(), que além das observações inclui o valores previsto e o resíduo de cada observação. Retiramos os últimos dados porque não são tão relevantes nessa análise, inserimos um índice de identificação de cada observação e, por último, transformamos os dados factor em numéricos (usando uma string como formato intermediário) para facilitar as futuras transformações.

```{r}
observations <- 
  reg %>% 
  augment() %>%  
  select(-c(.hat:.cooksd)) %>% 
  mutate(id = seq_along(mpg)) %>% 
  mutate_if(is.factor, as.character) %>% 
  mutate_if(is.character, as.double)
```

Para a visualização é bom utilizar um pequeno hack. Seria muito conveniente fazer essa análise por meio de facetas, sem ter que analisar variável por variável. Porém, facetas funcionam dentro de uma mesma variável, como resolver?
  
  A solução, apesar de simples, é contraintuitiva. Podemos deixar a tabela não tidy utilizando pivot longer (ao redor da variável ID), resultando em uma coluna com todos os valores e outra indicando a qual variável esse valor pertece, além da coluna que indica o ID de cada observação. Com isso, podemos plotar no eixo x o ID de cada observação e no eixo y o valor das variáveis, utilizando o facet wrap para separar as variáveis. 

```{r}
observations %>% 
  mutate_all(as.double) %>% 
  select(-c(.resid, .fitted)) %>% 
  pivot_longer(-id) %>% 
  ggplot(aes(id, value)) +
  facet_wrap(vars(name), scales = "free") +
  geom_point() +
  labs(x = "ID",
       y = "",
       title = "Checagem de amostragem aleatória",
       subtitle = "subdividida por variável")
```

</br>
  
  ### Dispersão dos resíduos por variável
  
  </br>
  
  Assim como analisar a distribuição das variáveis ao longo da amostra é útil para verificar a aleatoriedade da amostra, analisar a distribuição dos resíduos de acordo com as variáveis é útil para verificar a ausência de padrões (que são prejudiciais à validade do modelo). Devemos verificar, nessa análise, se existem relações entre as variáveis, se existem outliers e se existe uma discrepância visível na variância de acordo com o valor de uma variável.

A técnica utilizada será quase idêntica à anterior, com exceção de que pivotaremos ao redor dos resíduos padronizados do modelo, não do ID. Devemos fazer isso pois o que nos interessa é a relação dos resíduos com as outras variáveis, não da ordem das observações.

```{r}
observations %>% 
  mutate_all(as.double) %>% 
  select(-c(.fitted, .resid)) %>% 
  pivot_longer(-.std.resid) %>% 
  ggplot(aes(value, .std.resid)) +
  facet_wrap(vars(name), scales = "free") +
  geom_point() +
  labs(x = "",
       y = "Resíduos padronizados",
       title = "Análise dos resíduos",
       subtitle = "subdividida por variável")
```

</br>
  
  ### Distribuição dos resíduos
  
  </br>
  
  Além do teste Shapiro-Wilk de normalidade, podemos criar um histograma ou gráfico de densidade para mostrar, visualmente, a distribuição dos resíduos. É possível argumentar, dada a distribuição se essa se assemelha a uma normal ou não.

```{r}
observations %>% 
  ggplot(aes(.resid)) +
  stat_function(fun = dnorm, args = list(mean = 0,
                                         sd = sd(observations$.resid)),
                geom = "line", color = "black") +
  geom_density(fill = "blue", alpha = .4) +
  labs(x = "Resíduos",
       y = "Densidade",
       title = "Distribuição dos resíduos do modelo",
       subtitle = "sobreposta por uma curva normal")
```

Ademais, podemos usar um qqplot, ou seja, um gráfico que cruza os resíduos do modelo com estimativas normais. Quanto mais próximo de uma reta com inclinação 1, mais próximo de uma normal a distribuição é.

```{r}
ggplot(observations, aes(sample = .resid)) +
  geom_qq_line() +
  geom_qq() +
  labs(x = "Valores previstos",
       y = "Valores reais",
       title = "QQPlot dos resíduos")
```

</br>
  
  ### Influência/Confiança dos estimadores
  
  </br>
  
  Por último, vou mostrar uma maneira simples e intuitiva de mostrar os resultados de seu modelo. Uma maneira gráfica de mostrar a significância e o grau de impacto de cada estimador.

Para isso, iremos extrair do nosso modelo o valor das estimativas que fizemos, além do intervalo de confiança de cada uma delas, por meio da função tidy(), adicionando o parâmetro "conf.int = TRUE" para incluir os intervalos de confiança na análise. Em seguida, reordenamos os termos de acordo com o grau das estimativas para facilitar a visualização do gráfico.

Por último, fazemos o gráfico com geom points para indicar o valor das estimativas e geom errorbars para indicar o intervalo de confiança. Como um detalhe a mais, podemos colorir as barras de acordo com a significância, com as barras significantes sendo pretas e as não significantes sendo vermelhas.

```{r}
estimations <- 
  reg %>% 
  tidy(conf.int = TRUE) %>% 
  mutate(term = fct_reorder(term, estimate))

ggplot(estimations, aes(estimate, term, 
                        color = conf.low < 0 & conf.high > 0)) +
  geom_point() +
  geom_errorbar(aes(xmin = conf.low, xmax = conf.high)) +
  scale_color_manual(values = c("red", "black"),
                     labels = c("Sem significância", "Com significância")) +
  labs(x = "Estimativa",
       y = "",
       color = "",
       title = "Análise da significância de cada estimador")
```

</br>
  
  **Obrigado pela atenção! Espero que tenha te ajudado a analisar uma regressão no R.**