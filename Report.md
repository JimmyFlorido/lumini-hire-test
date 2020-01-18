---
title: "Lumini"
author: "Lira"
date: "15/01/2020"
output: html_document
---

Pacotes necessários. Se não tiver, instale eles por meio do install.packages("NOME_DO_PACOTE")

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

# Como é possível segmentar os inscritos de forma clara e objetiva com o intuito de justificar investimentos em educação destinados a certas parcelas de alunos?

Será criada uma regra de classificação para confirmar se determinadas regras referentes a programas, como o Prouni, fazem sentido. Na prática, é verificar se as regras para concessão de bolsa estão atendendo a critérios estatísticos ou a mais pura conveniência por propósitos políticos. 

O Programa Universidade para Todos - Prouni tem como finalidade a concessão de bolsas de estudo integrais e parciais em cursos de graduação e sequenciais de formação específica, em instituições de ensino superior privadas. Criado pelo Governo Federal em 2004 e institucionalizado pela Lei nº 11.096, em 13 de janeiro de 2005 oferece, em contrapartida, isenção de tributos àquelas instituições que aderem ao Programa. Mais informações aqui: http://prouniportal.mec.gov.br/o-programa 

Mas adianta-se que os requisitos para ser apto ao Prouni são simples: ter cursado o ensino médio em instituição pública, e ter renda familiar mensal menor do que 3 salários mínimos (R$ 2,640.0 em 2016). Mas além disso, é preciso obter, no mínimo, 450 pontos na nota final do ENEM, e não zerar a prova de redação desse exame. Nota-se que já há uma boa série de critérios para criar uma regra preliminar de classificação por meio de decision tree (árvore de decisão). 

Mas antes de montar uma árvore de decisão, é necessário explorar alguns aspectos da base de dados. Primeiro: adicione ou mude algumas variáveis que te ajudarão a entender essa população de candidatos. 

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

Sobre o aspecto da renda familiar: ela impacta na nota final do ENEM? 
É importante fazer isso para entender se, além da pontuação mínima de 450, o crítério de renda faz sentido. 

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

Infelizmente, a renda é um fator muito preponderante para decidir a pontuação no ENEM, conforme demonstra o gráfico. E é justificável as regras do PROUNI pautarem a renda e uma pontuação abaixo da média para delimitar o acesso ao programa. 

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

Há a possibilidade de explorar a questão de cor de pele ou região, mas de certa forma, são fatores que estarão relacionados à renda (infelizmente, há uma correlação forte entre renda baixa e cor de pele negra), por isso, não há a necessidade de avançar sobre esses tóṕicos. 

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

Não há um padrão visível. Podemos descartar o sexo como um critério que afeta o desempenho no ENEM. 

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

printcp(arvore1)

rpart.plot(arvore1)

```
![alt text](https://github.com/JimmyFlorido/lumini-hire-test/blob/FranciscoLira/Images/7.png "tree")

# Através da árvore de decisão, podemos segmentar os candidatos para justificar determinados investimentos, como o Prouni, a determinadas parcelas de estudantes. As regras de participação do Prouni tem validade estatística.

---
Antes de qualquer coisa, é preciso dizer que essa árvore de decisão  não tem alto poder de predição: o root node error é de aproximadamente 43%, isto é, a primeira raiz dessa árvore explica somente 43% da amostra ter ou não uma pontuação acima da média. No entanto, o propósito aqui não é ter feito um modelo que obtenha a maior acurácia (tanto que não separou uma amostra para teste do modelo), mas verificar se os critérios do Prouni fazem sentido com os dados mostrados. 

O fato do candidato ter estudado em escola privada já eleva subtancialmente as chances dele conseguir uma pontuação acima da média (83% de probabilidade), a ponto de ter cerca de 4 vezes mais chance do que um estudante de escola pública com renda menor do que R$ 1,100.0. 

O critério de renda na árvore de decisão parece estar adequado à regra do Prouni (até 3 salários mínimos - 2,640.0 reais -  de renda mensal para ser apto), pois a partir de 3,000 reais de renda mensal familiar, o candidato possui maior probabilidade (58%) de ter uma nota acima da média. 

Ou seja, as regras de participação do Prouni tem validade estatística, não é algo feito de forma discricionária (conforme a conveniência). 

---

Além de confirmar a segmentação por meio de classificação, é importante entender quem são os alunos que estão tendo baixo rendimento: nota zero em redação e/ou pontuação menor do que 450 na prova do Enem. Estes não possuem nota mínima para participar do Prouni, e assim, não tem acesso a bolsas de estudo integrais ou parciais.

Por que pensar nessa amostra específica de candidatos ao ENEM? Porque o Prouni, na prática, é um investimento público decorrente da isenção de impostos a instituições privadas de ensino, cujo isenção é proporcional ao número de bolsas usadas. E infelizmente muitas das bolsas de estudo promovidas pelo Prouni não são preenchidas, e isso tem várias repercussões, desde a redução de bolsas integrais até o aumento do EAD.
https://www1.folha.uol.com.br/educacao/2019/07/prouni-tem-menor-oferta-de-bolsas-integrais-e-para-cursos-presenciais.shtml 

Qual é a porcentagem desses candidatos de baixo rendimento na amostra?

```{r WHO}

enem2 %>% 
  filter(IN_TREINEIRO == 0,
         TP_PRESENCA == 1,
         CAND_PROUNI == 1
         ) %>%
  group_by(SEM_PROUNI) %>% 
  summarise(Candidatos = n()) %>% 
  as.data.frame()

```

```{r WHO2}
754/(2007+754)
```
Aproximadamente 27.3% de alunos aptos a participar do Prouni, não conseguem ser incluídos no processo de seleção por insuficiência de nota, de desempenho.

# Neste ponto, é importante entender quem são esses candidatos aptos ao Prouni, que não alcançaram a pontuação mínima, para entender se há algum fator externo que seja determinante para o sucesso deles na prova

Verificar as proporções de estudantes que realmente estão fazendo o ENEM para conquistar uma bolsa de estudos. Compare essas proporções com os grupos de candidatos de baixo rendimento (que não vão conseguir entrar no Prouni) com aqueles que tiveram um melhor rendimento na prova. A ideia é ver se o baixo rendimento decorre da baixa motivação dos candidatos. 

```{r MOTIVATION}

enem2 %>% 
  filter(IN_TREINEIRO == 0,
         TP_PRESENCA == 1,
         CAND_PROUNI == 1
         ) %>%
  mutate(Motivados = ifelse(TP_BOLSAS == 5, 1, 0)) %>% # Na pesquisa, quanto mais próximo de 5, maior é o interesse do candidato em conseguir uma bolsa de estudos. Ou seja, se ele marcou 5: ele quer muito uma bolsa - é um motivado. 
  group_by(SEM_PROUNI) %>% 
  summarise(Motivados = sum(Motivados),
            Candidatos = n()) %>% 
  as.data.frame() %>% 
  mutate(Porcentagem = round((Motivados/Candidatos)*100, 2))

```

Sem Prouni? | Motivados | CandidatosProuni | Motivados (%)
------------ | ------------- | ------------- | -------------
Não |	1,584 |	2,007 |	78.92
Sim |	627 |	754 |	83.16

Não se pode afirmar que não são motivados os estudantes que não seguiram boa nota: eles têm maior quantidade de alunos dispostos a obter uma bolsa de estudos do que aqueles que tiveram melhor desempenho no ENEM. 

Qual é a UF de residência desses candidatos de baixo rendimento? 

```{r STATES}

cidades3 <- cidades %>% 
  filter(CAPITAL == "S") %>%
  mutate(lon = as.character(LONGITUDE),
         lat = as.character(LATITUDE)) %>% 
  mutate_at(vars(lon, lat), list(~as.numeric(as.character(.)))) %>% 
  select(UFCOD, lon, lat)

states <- enem2 %>%
  filter(IN_TREINEIRO == 0,
         TP_PRESENCA == 1,
         CAND_PROUNI == 1,
         SEM_PROUNI == 1
  ) %>%
  group_by(CO_UF_RESIDENCIA, UF = SG_UF_RESIDENCIA) %>%
  summarise(Candidatos = n(),
            NãoConseguiram = sum(SEM_PROUNI)) %>%
  as.data.frame() %>%
  left_join(cidades3, by = c("CO_UF_RESIDENCIA" = "UFCOD"))
  
pal <- colorNumeric(palette = "RdYlBu", 
                    domain = states$Candidatos)

leaflet(states) %>% 
  addTiles() %>%
  addCircleMarkers(~lon, ~lat, 
                   color = ~pal(Candidatos),
                   fillOpacity = 1.0,
                   radius = 15.0,
                   label = ~UF,
                   labelOptions = labelOptions(noHide = TRUE, 
                                               textsize = 10.0)) %>% 
  addLegend(pal = pal, 
            values = ~Candidatos, 
            opacity = 1,
            title = "Nº de candidatos aptos ao Prouni")

```
### Distribuição dos candidatos de baixo rendimento entre os estados do Brasil
![alt text](https://github.com/JimmyFlorido/lumini-hire-test/blob/FranciscoLira/Images/8.png "map")

Nota-se que a maioria está concentrada em São Paulo, e principalmente, no Ceará. Ou seja, é necessário avaliar tambéḿ (com outra base de dados) as condições do ensino médio cearense para entender porque esse alunos não foram bem no ENEM. 

Onde a performance desses alunos de baixo rendimento peca? Mais em matemática, redação ou em outro aspecto da prova?

```{r LOW SCORE}

score <- enem2 %>% 
  filter(IN_TREINEIRO == 0,
         TP_PRESENCA == 1,
         SEM_PROUNI == 1,
         CAND_PROUNI == 1
         ) %>%
  summarise(Humanas = sd(NU_NOTA_CH),
            Naturais = sd(NU_NOTA_CN),
            Língua = sd(NU_NOTA_LC),
            Matemática = sd(NU_NOTA_MT),
            Redação = sd(NU_NOTA_REDACAO)) %>% 
  as.data.frame() %>% 
  gather(key = "Matéria", value = "NotaDesvioPadrão")

enem2 %>% 
  filter(IN_TREINEIRO == 0,
         TP_PRESENCA == 1,
         SEM_PROUNI == 1,
         CAND_PROUNI == 1
         ) %>%
  summarise(Humanas = mean(NU_NOTA_CH),
            Naturais = mean(NU_NOTA_CN),
            Língua = mean(NU_NOTA_LC),
            Matemática = mean(NU_NOTA_MT),
            Redação = mean(NU_NOTA_REDACAO)) %>% 
  as.data.frame() %>% 
  gather(key = "Matéria", value = "NotaMédia") %>% 
  left_join(score, by = "Matéria")

```
Matéria | NotaMédia | NotaDesvioPadrão
------------ | ------------- | -------------
Humanas |	446.51 |	50.53 
Naturais |	421.45 |	45.18 
Língua |	445.48 |	51.43 
Matemática |	410.35 |	54.93 
Redação |	358.48 |	142.62 

A prova de redação é o principal "calcanhar de aquiles" desses candidatos, o que é atestado pela nota média baixa relativa, além do maior desvio padrão. 

Quantos desses alunos já fizeram o EJA (Educação de Jovens e Adultos)?

```{r EJA}

theme_set(theme_minimal())

enem2 %>% 
  filter(IN_TREINEIRO == 0,
         TP_PRESENCA == 1,
         CAND_PROUNI == 1,
         SEM_PROUNI == 1
         ) %>%
  mutate(EJA = ifelse(TP_ENSINO == 3, "Fez", "Não fez")) %>% 
  group_by(EJA) %>% 
  summarise(Candidatos = n()) %>% 
  as.data.frame() %>% 
  ggplot() +
  aes(x = EJA, y = Candidatos) +
  geom_bar(stat = "identity", fill = "orange", width = 0.4) +
  geom_text(aes(label = Candidatos), vjust=-0.3, size=4.5) +
  labs(y = element_blank(),
       title = "Nº de candidatos de baixo rendimento",
       subtitle = "Presença no EJA")+
  theme(axis.text.y = element_blank(),
        axis.text.x = element_text(size = 13.5))
  

```
![alt text](https://github.com/JimmyFlorido/lumini-hire-test/blob/FranciscoLira/Images/9.png "eja")

Não é significativa a quantidade de estudantes que fizeram o EJA. 

Qual é a cor de pele desses estudantes de baixo rendimento? E gênero e idade?

```{r DEMO STATS, warning=FALSE}

theme_set(theme_minimal())

enem2 %>% 
  filter(IN_TREINEIRO == 0,
         TP_PRESENCA == 1,
         CAND_PROUNI == 1,
         SEM_PROUNI == 1
         ) %>%
  ggplot() +
  aes(x = NU_IDADE) +
  geom_histogram(fill = "red") +
  labs(x = "Idade", 
       y = "Frequência",
       title = "Distribuição dos candidatos de baixo rendimento",
       subtitle = "Por idade")+
  scale_x_continuous(breaks = c(seq(15, 65, by = 2)))

enem2 %>% 
  filter(IN_TREINEIRO == 0,
         TP_PRESENCA == 1,
         CAND_PROUNI == 1,
         SEM_PROUNI == 1
         ) %>%
  group_by(Sexo = TP_SEXO) %>% 
  summarise(Candidatos = n()) %>% 
  as.data.frame() %>% 
  ggplot() +
  aes(x = Sexo, y = Candidatos) +
  geom_bar(stat = "identity", fill = "hotpink", width = 0.4) +
  geom_text(aes(label = Candidatos), vjust=-0.3, size=3.5) +
  labs(y = element_blank(),
       title = "Nº de candidatos de baixo rendimento",
       subtitle = "Por sexo")+
  theme(axis.text.y = element_blank(),
        axis.text.x = element_text(size = 13.5))

cor_tabla <- data.frame(Cod = c(0:6), TP_COR_RACA2 = c(NA, "Branca", "Preta", "Parda", "Amarela", "Indígena", "Não sabe"))

enem2 %>% 
  filter(IN_TREINEIRO == 0,
         TP_PRESENCA == 1,
         CAND_PROUNI == 1,
         SEM_PROUNI == 1
         ) %>%
  left_join(cor_tabla, by = c("TP_COR_RACA" = "Cod")) %>% 
  group_by(`Cor da Pele` = TP_COR_RACA2) %>% 
  summarise(Candidatos = n()) %>% 
  as.data.frame() %>% 
  ggplot() +
  aes(x = `Cor da Pele`, y = Candidatos) +
  geom_bar(stat = "identity", fill = "purple") +
  geom_text(aes(label = Candidatos), vjust=-0.3, size=4.5) +
  labs(y = element_blank(), 
       x = element_blank(),
       title = "Nº de candidatos de baixo rendimento",
       subtitle = "Por cor de pele")+
  theme(axis.text.y = element_blank(),
        axis.text.x = element_text(size = 10.5))

```
![alt text](https://github.com/JimmyFlorido/lumini-hire-test/blob/FranciscoLira/Images/10.png "idade")
![alt text](https://github.com/JimmyFlorido/lumini-hire-test/blob/FranciscoLira/Images/11.png "sexo")
![alt text](https://github.com/JimmyFlorido/lumini-hire-test/blob/FranciscoLira/Images/12.png "cor")

Grande parte dessa amostra são jovens mulheres negras. Acrescenta-se que essa amostra não difere-se muito da população geral de alunos aptos a participar do Prouni. 

Desse grupo de candidatos, há muitas mães jovens ou adolescentes?

```{r YOUNG MOTHER}

enem2 %>% 
  filter(IN_TREINEIRO == 0,
         TP_PRESENCA == 1,
         CAND_PROUNI == 1,
         SEM_PROUNI == 1
         ) %>%
  group_by(IN_LACTANTE) %>% 
  summarise(Candidatos =n()) %>% 
  as.data.frame()

```
Quase nenhuma delas é mãe: só 1 teve necessidade de amamentação durante a prova. Não teve ninguém dessa amostra com necessidades de gestante durante o ENEM de 2016. 

Essa amostra trabalha? Algo que poderia supostamente impedir um melhor proveito dos estudos. 

```{r JOB}

trabalho_tabla <- data.frame(Cod = c("A", "B", "C"), 
                             TP_TRABALHO2 = c("Nunca", "Sim, mas não agora", "Continuo"))

enem2 %>% 
  filter(IN_TREINEIRO == 0,
         TP_PRESENCA == 1,
         CAND_PROUNI == 1
         ) %>%
  mutate(SEM_PROUNI = ifelse(SEM_PROUNI == 1, "Não conseguiu", "Conseguiu")) %>% 
  left_join(trabalho_tabla, by = c("TP_TRABALHO" = "Cod")) %>% 
  group_by(`Trabalha?` = TP_TRABALHO2, SEM_PROUNI) %>% 
  summarise(Candidatos = n()) %>% 
  as.data.frame() %>% 
  spread(SEM_PROUNI, Candidatos) %>% 
  mutate(Proporção = `Não conseguiu`/(Conseguiu + `Não conseguiu`))

```
Trabalha? | ConseguiuProuni | NãoConseguiuProuni | Proporção(%)
------------ | ------------- | ------------- | -------------
Continuo |	242 |	80 |	24.84 
Nunca |	1480 |	572 |	27.87 
Sim, mas não agora |	285 |	102 |	26.35 

Os alunos aptos ao Prouni que não foram bem na prova, em grande parte, nunca trabalhou na vida, e isso não se difere muito dos que foram bem-sucedidos na prova. Por enquanto, não se pode dizer que o trabalho na adolescência é um obstáculo importante. 

Quantos dessa amostra tiveram que viajar para fazer a prova do Enem? Compare o município de residência com o município da prova.

```{r DISTANCE}

enem2 %>% 
    filter(IN_TREINEIRO == 0,
         TP_PRESENCA == 1,
         CAND_PROUNI == 1
         ) %>%
  mutate(Cidade = ifelse(as.character(NO_MUNICIPIO_RESIDENCIA) == as.character(NO_MUNICIPIO_PROVA), "Mesma Cidade", "Cidade Diferente")) %>% 
  group_by(Cidade) %>% 
  summarise(Candidatos = n(),
            NãoConseguiram = sum(SEM_PROUNI)) %>% 
  as.data.frame() %>% 
  mutate(Percentual = round((NãoConseguiram/Candidatos)*100, 2))

```
ViajouNoENEM? | Candidatos | NãoConseguiuProuni | Proporção(%)
------------ | ------------- | ------------- | -------------
Sim |	702 |	220 |	31.34 
Não |	2059 |	534 |	25.93 

Parece promissora essa diferença - fazer a prova numa cidade diferente, afeta no desempenho (mais candidatos de baixo rendimento) - mas para ter uma evidência mais robusta, vamos executar um teste de hipóteses sobre as notas dos estudante que fizeram e não fizeram a prova na cidade de residência. 

```{r DISTANCE2}

for_test1 <- enem2 %>% 
  mutate(Cidade = ifelse(as.character(NO_MUNICIPIO_RESIDENCIA) == as.character(NO_MUNICIPIO_PROVA), "Mesma Cidade", "Cidade Diferente")) %>%
  filter(IN_TREINEIRO == 0,
         TP_PRESENCA == 1,
         CAND_PROUNI == 1,
         Cidade == "Mesma Cidade"
  ) %>%
  select(FINAL_NOTA)

for_test2 <- enem2 %>% 
  mutate(Cidade = ifelse(as.character(NO_MUNICIPIO_RESIDENCIA) == as.character(NO_MUNICIPIO_PROVA), "Mesma Cidade", "Cidade Diferente")) %>%
  filter(IN_TREINEIRO == 0,
         TP_PRESENCA == 1,
         CAND_PROUNI == 1,
         Cidade == "Cidade Diferente"
  ) %>%
  select(FINAL_NOTA)

t.test(for_test1, for_test2, 
       var.equal = TRUE)

```
![alt text](https://github.com/JimmyFlorido/lumini-hire-test/blob/FranciscoLira/Images/13.png "t-test")

Com o teste de hipótese, pode-se afirmar que há evidências (embora não conclusivas) de que fazer ou não a prova na cidade de residência, impacta no resultado do ENEM. 

Por fim, descubra qual é o estado que tem mais alunos que precisam viajar para fazer o ENEM.

```{r DISTANCE3}

enem2 %>% 
  mutate(Cidade = ifelse(as.character(NO_MUNICIPIO_RESIDENCIA) == as.character(NO_MUNICIPIO_PROVA), 1, 0)) %>%
    filter(IN_TREINEIRO == 0,
         TP_PRESENCA == 1,
         CAND_PROUNI == 1,
         SEM_PROUNI == 1
         ) %>%
  group_by(UF = SG_UF_RESIDENCIA) %>% 
  summarise(Candidatos = n(),
            Viajou = sum(Cidade)) %>% 
  as.data.frame() %>% 
  arrange(desc(Viajou))

```
Assim como o Ceará lidera na quantidade de candidatos de baixo rendimento, também lidera na quantidade desses candidatos que precisam viajar para fazer a prova, o que traz maior robustez à evidência. É um insight a ser relevado o fato ou não de viajar para fazer o ENEM, pois entende-se que pode afetar em alguma medida o desempenho na prova.  

