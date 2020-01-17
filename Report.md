---
title: "Lumini"
author: "Lira"
date: "15/01/2020"
output: html_document
---

Pacotes necessários

```{r PACKAGES, include=FALSE}

library(tidyverse)
library(skimr)
library(rpart)
library(rpart.plot)
library(leaflet)

```

Baixar as bases de dados

```{r DATASET}

enem <- read.csv("Microdados_Enem_2016/Microdados_Enem_2016.csv")

cidades <- read.csv2("CADMUN.csv", encoding = "latin-1")

```

Ajustar as bases de dados. Selecione aquilo que será relevante para a análise.

```{r FIX DATASET}

cidades2 <- cidades %>% 
  mutate(UFCOD = as.character(UFCOD), 
         MUNCODDV = as.character(MUNCODDV),
         lon = as.character(LONGITUDE),
         lat = as.character(LATITUDE)) %>% 
  select(UFCOD, MUNCODDV,lat, lon)

enem2 <- enem %>% 
  select(NU_INSCRICAO, IN_TREINEIRO, NU_IDADE, TP_SEXO, TP_RENDA = Q006, TP_COR_RACA, TP_ESCOLA, TP_ENSINO, IN_LACTANTE, IN_GESTANTE, TP_TRABALHO = Q026, TP_BOLSAS = Q037, NU_ANO, TP_ANO_CONCLUIU, CO_UF_RESIDENCIA, SG_UF_RESIDENCIA, CO_MUNICIPIO_RESIDENCIA, NO_MUNICIPIO_RESIDENCIA, CO_UF_PROVA, SG_UF_PROVA, CO_MUNICIPIO_PROVA, NO_MUNICIPIO_PROVA, TP_PRESENCA_CH, TP_PRESENCA_CN, TP_PRESENCA_LC, TP_PRESENCA_MT, NU_NOTA_CH, NU_NOTA_CN, NU_NOTA_LC, NU_NOTA_MT, TP_STATUS_REDACAO, NU_NOTA_REDACAO)

str(enem2)
  
```
![alt text](https://github.com/JimmyFlorido/lumini-hire-test/blob/FranciscoLira/Images/1.png "Descriptive")

---

NOTAS: 

Sobre a classificação de renda do ENEM. 

A Nenhuma renda.
B Até R$ 880,00.
C De R$ 880,01 até R$ 1.320,00.
D De R$ 1.320,01 até R$ 1.760,00.
E De R$ 1.760,01 até R$ 2.200,00.
F De R$ 2.200,01 até R$ 2.640,00.
G De R$ 2.640,01 até R$ 3.520,00.
H De R$ 3.520,01 até R$ 4.400,00.
I De R$ 4.400,01 até R$ 5.280,00.
J De R$ 5.280,01 até R$ 6.160,00.
K De R$ 6.160,01 até R$ 7.040,00.
L De R$ 7.040,01 até R$ 7.920,00.
M De R$ 7.920,01 até R$ 8.800,00.
N De R$ 8.800,01 até R$ 10.560,00.
O De R$ 10.560,01 até R$ 13.200,00.
P De R$ 13.200,01 até R$ 17.600,00.
Q Mais de R$ 17.600,00.

Valor do salário mínimo vigente em 2016: R$ 880,00. 
Até 3 salários mínimos: do A até o F

---

"Como é possível segmentar os inscritos de forma clara e objetiva com o intuito de justificar investimentos em educação destinados a certas parcelas de alunos?" 

Na prática, será criada uma regra de classificação para confirmar se determinadas regras referentes a programas, como o Prouni, fazem sentido. 

O Programa Universidade para Todos - Prouni tem como finalidade a concessão de bolsas de estudo integrais e parciais em cursos de graduação e sequenciais de formação específica, em instituições de ensino superior privadas. Criado pelo Governo Federal em 2004 e institucionalizado pela Lei nº 11.096, em 13 de janeiro de 2005 oferece, em contrapartida, isenção de tributos àquelas instituições que aderem ao Programa. Masi informações aqui: http://prouniportal.mec.gov.br/o-programa 

Mas adianta-se que os requisitos para ser apto ao Prouni é simples: ter cursado o ensino médio no ensino público, e ter renda familiar menor do que 3 salários mínimos (R$ 2,640.0 em 2016). Mas além disso, é preciso obter, no mínimo, 450 pontos na nota final do ENEM, e não zerar a prova de redação desse exame. Nota-se que já há uma boa série de critérios para criar uma regra preliminar de classificação por meio de decision tree (árvore de decisão). 

Mas antes disso, é necessário explorar aguns aspectos da base de dados. Primeiro: adicione ou mude algumas variáveis que te ajudarão a entender essa população de candidatos.

```{r FIX DATASET2}

enem2 <- enem2 %>% 
  mutate(TP_PRESENCA = ifelse(TP_PRESENCA_LC == 1 & TP_PRESENCA_CN == 1, 1, 0), #Variável que atesta a presença em todos os testes do Enem
         FINAL_NOTA = (NU_NOTA_CH + NU_NOTA_CN + NU_NOTA_LC + NU_NOTA_MT + NU_NOTA_REDACAO)/5, # A nota final dos alunos ao fim do ENEM. Basicamente uma média aritmética entre as 5 provas. 
         RENDA_PROUNI = ifelse(TP_RENDA %in% c("A", "B", "C", "D", "E", "F"), 1, 0), # Classificação de renda que se enquadra em candidatos aptos ao PROUNI
         CAND_PROUNI = ifelse(TP_ESCOLA == 2 & RENDA_PROUNI == 1, 1, 0), #Dummy que sinaliza se o candidato pode participar do PROUNI
         SEM_PROUNI = ifelse(FINAL_NOTA < 450 | NU_NOTA_REDACAO == 0, 1, 0) # Dummy que mostra se o candidato, apto ao PROUNI, não conseguiu a pontuação necessária para concorrer uma bolsa
         
         )

```

Faça uma análise descritiva dessa população

```{r DESCRIPTIVE}

skim(enem2)

```
![alt text](https://github.com/JimmyFlorido/lumini-hire-test/blob/FranciscoLira/Images/2.png "Descriptive2")

Compreender a distribuição média das notas no ENEM de 2016. 

```{r SCORE DISTRIBUTION, warning=FALSE}

theme_set(theme_minimal())

enem2 %>% 
  filter(IN_TREINEIRO == 0, #Deixar somente aqueles que estão tentando de verdade o exame. 
         TP_PRESENCA == 1 # Filtrar aqueles que tiveram presença confirmada em todas as etapas do exame. 
         ) %>%
  ggplot() +
  aes(x = FINAL_NOTA) +
  geom_histogram(fill = "lawngreen") + 
  labs(x = "Nota final", 
       y = "Frequência", 
       title = "Distribuição das notas do ENEM")+
  scale_x_continuous(limits = c(300,800),
                     breaks = c(seq(300, 800, by = 50)))

```
![alt text](https://github.com/JimmyFlorido/lumini-hire-test/blob/FranciscoLira/Images/3.png "Score")

```{r SCORE AVERAGE}
mean(enem2$FINAL_NOTA, na.rm = TRUE)
```
A média da nota final no ENEM é 506.6, e nota-se que essa é uma distribuição que segue o padrão gaussiano (normal).
É importante destacar que essa média está acima do mínimo exigido para participar do Prouni, ou seja, o custo de entrada para o Prouni para determinada faixa dos candidatos, não é necessariamente alta. 

Antes de fazer a decision tree, é necessário testar algumas relações, a começar pela renda: ela impacta na nota final do ENEM? 
É importante fazer isso para entender se o crítério de renda, e se a pontuação mínima de 450 faz sentido. 

```{r EARNING VS SCORE, warning=FALSE}

renda_tabla <- data.frame(Cod = c("A", "B", "C", "D", "E", "F", "G", "H", "I", "J", "K", "L", "M", "N", "O", "P", "Q"),
                          TP_RENDA2 = c(0,880,1320,1760,2200,2640,3520,4400,5280,6160,7040,7920,8800,10560,13200,17600, 20000))

theme_set(theme_minimal())

enem2 %>% 
  filter(IN_TREINEIRO == 0,
         TP_PRESENCA == 1
         ) %>%
  left_join(renda_tabla, by = c("TP_RENDA" = "Cod")) %>% 
  mutate(TP_RENDA2 = as.numeric(TP_RENDA2)) %>% 
  group_by(Renda = TP_RENDA2) %>% 
  summarise(`Quantidade de candidatos` = n(),
            Nota = mean(FINAL_NOTA)) %>% 
  as.data.frame() %>% 
  ggplot() +
  aes(x = Renda/1000, y = Nota) +
  geom_point(aes(size = `Quantidade de candidatos`), colour = "#ed0647") +
  labs(x = "Faixa de renda mensal (R$ mil)",
       y = "Nota final",
       title = "Relação entre nota no ENEM e renda") +
  scale_x_continuous(breaks = seq(0, 20.0, by = 1.320)) +
  scale_y_continuous(breaks = seq(450, 650, by = 25)) +
  theme(legend.position = "top")

```
![alt text](https://github.com/JimmyFlorido/lumini-hire-test/blob/FranciscoLira/Images/4.png "income1")

Infelizmente, a renda é um fator muito preponderante para decidir a pontuação no ENEM, conforme demosntra o gráfico. E é justificável as regras do PROUNI pautarem a renda e uma pontuação abaixo da média para delimitar o acesso ao programa. 

O fato do candidato ter feito ensino médio no colégio público afeta o desempenho dele no ENEM?

```{r SCHOOL TYPE, warning=FALSE}

escola_tabla <- data.frame(Cod = c(1:4),
                           TP_ESCOLA2 = c(NA, #Não responderam
                                          "Pública", "Privada", "Exterior"))

theme_set(theme_minimal())

enem2 %>% 
  filter(IN_TREINEIRO == 0,
         TP_PRESENCA == 1
  ) %>%
  left_join(escola_tabla, by = c("TP_ESCOLA" = "Cod")) %>% 
  left_join(renda_tabla, by = c("TP_RENDA" = "Cod")) %>% 
  mutate(TP_RENDA2 = as.numeric(TP_RENDA2),
         `Tipo de escola` = as.factor(TP_ESCOLA2)) %>% 
  filter(!is.na(`Tipo de escola`),
         `Tipo de escola` != "Exterior") %>% # Muitos não responderam esse critério no ENEM, por isso, é importante fitrá-lo
  ggplot() +
  aes(x = TP_RENDA2/1000, y = FINAL_NOTA) +
  geom_point(aes(colour = `Tipo de escola`)) +
  scale_color_manual(values = c("tomato", "purple")) +
  labs(x = "Faixa de renda mensal (R$ mil)",
       y = "Nota final",
       title = "Relação entre nota no ENEM e renda",
       subtitle = "Segmentado por tipo de escola") +
  scale_x_continuous(breaks = seq(0, 20.0, by = 1.320)) +
  scale_y_continuous(breaks = seq(225, 800, by = 25)) +
  theme(legend.position = "top")

```
![alt text](https://github.com/JimmyFlorido/lumini-hire-test/blob/FranciscoLira/Images/5.png "income2")

Afeta. Grande parte das notas mais baixas concentram-se não somente naqueles que têm renda mais baixa, mas naqueles que foram ensinados em colégios públicos. 

Há a possibilidade de explorar a questão de cor de pele (infelizmente, há uma correlação forte entre renda baixa e cor de pele negra) ou região, mas de certa forma, são fatores que estarão relacionados à renda, por isso, não há a necessidade de explorar tanto. 

No entanto, vale a pena verificar se a questão de sexo tem algum aspecto visual relevante para o desempenho no ENEM. 

```{r SEX TYPE, warning=FALSE}

theme_set(theme_minimal())

enem2 %>% 
  filter(IN_TREINEIRO == 0,
         TP_PRESENCA == 1
  ) %>%
  left_join(renda_tabla, by = c("TP_RENDA" = "Cod")) %>% 
  mutate(TP_RENDA2 = as.numeric(TP_RENDA2),
         Sexo = ifelse(TP_SEXO == "M", "Masculino", "Feminino")) %>% 
  ggplot() +
  aes(x = TP_RENDA2/1000, y = FINAL_NOTA) +
  geom_point(aes(colour = Sexo)) +
  scale_color_manual(values = c("coral", "purple")) +
  labs(x = "Faixa de renda mensal (R$ mil)",
       y = "Nota final",
       title = "Relação entre nota no ENEM e renda",
       subtitle = "Segmentado por sexo") +
  scale_x_continuous(breaks = seq(0, 20, by = 1.320)) +
  scale_y_continuous(breaks = seq(0, 900, by = 50)) +
  theme(legend.position = "top")

```
![alt text](https://github.com/JimmyFlorido/lumini-hire-test/blob/FranciscoLira/Images/6.png "income3")

Não há um padrão visível. Podemos descartar o sexo como algo importante para favorecer algum grupo. 

Fazer a árvore de decisão, a fim de verificar se as regras para o Prouni, de fato, fazem sentido ao considerar renda e instituição de ensino.

```{r TREE}

enemtree <- enem2 %>% 
  filter(IN_TREINEIRO == 0,
         TP_PRESENCA == 1
  ) %>%
  left_join(escola_tabla, by = c("TP_ESCOLA" = "Cod")) %>% 
  left_join(renda_tabla, by = c("TP_RENDA" = "Cod")) %>% 
  mutate(TP_RENDA2 = as.numeric(TP_RENDA2),
         Sexo = as.factor(ifelse(TP_SEXO == "M", "Masculino", "Feminino")),
         `Tipo de escola` = as.factor(TP_ESCOLA2),
         ABOVE_AVG = ifelse(FINAL_NOTA > mean(FINAL_NOTA, na.rm = TRUE), 1, 0)) %>% 
  filter(!is.na(`Tipo de escola`),
         !is.na(Sexo),
         `Tipo de escola` != "Exterior")

arvore1 <- rpart(formula = ABOVE_AVG ~ TP_RENDA2 + `Tipo de escola` + Sexo, 
                data = enemtree, 
                method = 'class')

summary(arvore1)

rpart.plot(arvore1)

```
![alt text](https://github.com/JimmyFlorido/lumini-hire-test/blob/FranciscoLira/Images/7.png "tree")

Através da árvore de decisão, podemos segmentar os candidatos para justificar determinados investimentos, como o Prouni, a determinadas parcelas de estudantes. 
