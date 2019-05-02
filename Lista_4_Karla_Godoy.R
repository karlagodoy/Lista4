# Análise de Dados

# Karla Godoy 

# Lista 4 

# Exercicio 1

# https://github.com/karlagodoy/Lista_4_UFPE

# Exercício 2 ----

# Definindo Diretorio e carregando base de dados 

require(tidyverse)

getwd()

setwd("C:\\Users\\karla\\Desktop\\Mestrado\\Análise de Dados")

install.packages("readxl")

require(readxl)

if(require(tidyverse) == F) install.packages('tidyverse'); require(tidyverse)
if(require(readxl) == F) install.packages('readxl'); require(readxl)

Dados_PNUD <- read_xlsx("atlas2013_dadosbrutos_pt.xlsx")

load("docentes_pe_censo_escolar_2016.Rdata")

load("matricula_pe_censo_escolar_2016.Rdata")


# Limpando os dados ----


# Dados PNUD----

Dados_PNUD <- read_excel("atlas2013_dadosbrutos_pt.xlsx", sheet = 1)

head(Dados_PNUD)

# selecionando dados de 2010 e do Estado de Pernambuco

pnud_pe_2010 <- Dados_PNUD %>% filter(ANO == 2010 & UF == 26)

IDHM_MUN <- pnud_pe_2010 %>% group_by(Município) %>%
  summarise(IDHM_R = n())


# Dados Docentes---- 

Docentes_pe_mun_idade <- docentes_pe%>% filter(NU_IDADE <=70, 18<= NU_IDADE) 

Docentes_muni_PE <- Docentes_pe_mun_idade %>% group_by(CO_MUNICIPIO) %>%
  summarise(n_docentes = n())

# verificacao

dim(Docentes_muni_PE)

summary(Docentes_muni_PE)

# Dados Matriculas----

matriculas_pe_idade <- matricula_pe %>% filter(NU_IDADE <= 25, 1<=NU_IDADE)

summary(matriculas_pe_idade)

matriculas_PE_mun_idade <- matriculas_pe_idade %>% group_by(CO_MUNICIPIO) %>%
  summarise(n_matriculas = n())

# verificacao

dim(matriculas_PE_mun_idade)



# Unindo as bases de Dados ----


# matriculas e PNUD ----

pnud_pe_matriculas_docentes <- pnud_pe_2010 %>% full_join(matriculas_PE_mun_idade, 
                                                by = c("Codmun7" = "CO_MUNICIPIO")
)
dim(pnud_pe_2010)

dim(matriculas_PE_mun_idade)

dim(pnud_pe_matriculas_docentes)

names(pnud_pe_matriculas_docentes)


# Docentes e Matriculas e PNUD ----

pnud_pe_matriculas_docentes_final <- pnud_pe_matriculas_docentes %>% full_join(Docentes_muni_PE, 
                                                     by = c("Codmun7" = "CO_MUNICIPIO")
)

dim(pnud_pe_matriculas_docentes)

dim(Docentes_muni_PE)

dim (pnud_pe_matriculas_docentes_final)

names(pnud_pe_matriculas_docentes_final)



#Base de dados para Correlação---- 

Matriculas_docentes_IDHM <- pnud_pe_matriculas_docentes_final%>% select (Município,n_docentes,n_matriculas, IDHM)

# Nova Variavel- Matriculas por Docentes 

Matriculas_docentes_IDHM <-Matriculas_docentes_IDHM  %>% mutate(MAT_DOC = n_matriculas /n_docentes)%>% arrange(desc(MAT_DOC))                                                              

dim(Matriculas_docentes_IDHM)

median(Matriculas_docentes_IDHM$n_matriculas)

head(Matriculas_docentes_IDHM)
str(Matriculas_docentes_IDHM)



# Apresente estatísticas descritivas do número de alunos por docente nos municípios do Estado ----

summary(Matriculas_docentes_IDHM)

#    MAT_DOC     
# Min.   :4.431  
# 1st Qu.:5.464  
# Median :5.945  
# Mean   :6.043  
# 3rd Qu.:6.584  
# Max.   :9.557 


# Apresente o município com maior número de alunos por docente e seu IDHM----

summary(Matriculas_docentes_IDHM)

head(Matriculas_docentes_IDHM)

# R: TUPANATINGA


# Faça o teste do coeficiente de correlação linear de pearson e apresente sua resposta----

Dados<- Matriculas_docentes_IDHM

cor(Dados$IDHM, Dados$MAT_DOC, method = c("pearson"))

#R: -0.5057435


# Salvando base de dados em RData e CSV----

getwd()
save(Matriculas_docentes_IDHM, file = "CENSO_PNUD_2016_MATRICULAS_DOCENTES.RData")

write.csv2(Matriculas_docentes_IDHM, file = "CENSO_PNUD_2016_MATRICULAS_DOCENTES.csv",
           row.names = F)



# Gráfico----

#Usando o pacote ggplot2, apresente o gráfico de dispersão entre as duas variáveis (número de alunos
#por docente e IDHM)

require(ggplot2)

ggplot(data = Matriculas_docentes_IDHM, aes(x =MAT_DOC, y = IDHM) ) + 
  geom_point(color = "red", size = 2) +
  labs(x = "Número de Matrículas", y = "IDHM")





