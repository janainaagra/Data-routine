#############ROTINA LIMNOLOGIA#################
#Script base de análise de dados FÍSICO QUÍMICO em R
#Elaborado por Janaina Agra


#Pacotes----
library(tidyverse)
library(readxl) #para importar arquivo em excel
library(writexl) #para exportar arquivo em excel

#Definir diretório e conferir diretório
dir()

#Importando dados----
#Para importar arquivos de outros formatos
# load("nome do arquivo.RData") #para formato RData
# read.csv("nome do arquivo.csv", sep = "", dec = "", stringsAsFactors = T)

dados <- read.csv2("DADOS_FQ_CEMIG_20230602_csv.csv", h = T, sep = ";", dec = ",", stringsAsFactors = T)

#em excel
#dados <- read_excel("DADOS_FQ_CEMIG_20230602.xlsx")

#Conversão de separdor decimal-----
#"," por "."
dados$Resultado <- as.numeric(gsub(",", ".", dados$Resultado))
dados$Latitude <- as.numeric(gsub(",", ".", dados$Latitude))
dados$Longitude <- as.numeric(gsub(",", ".", dados$Longitude))


#Conferência dos dados----
View(dados)
dim(dados)
glimpse(dados)
names(dados)

unique(dados$Empreendimento)
unique(dados$Bacia.Hidrográfica)
unique(dados$Classificação.CONAMA)
unique(dados$Região.de.Coleta)
unique(dados$Local.de.Coleta)
levels(dados$Local.de.Coleta)  
#alterar "Montante do Reservatório ", "Reservatório "
dados$Local.de.Coleta <- gsub("Montante do Reservatório ","Montante do Reservatório", dados$Local.de.Coleta)
dados$Local.de.Coleta <- gsub("Reservatório ","Reservatório", dados$Local.de.Coleta)

unique(dados$Sazonalidade) #alterar seca -> Seca
dados$Sazonalidade <- gsub("seca", "Seca", dados$Sazonalidade)

unique(dados$Tipo.de.ambiente.Condição.física) #conferir se há diferença entre biobox caixa, torneira, e sem nada
unique(dados$Sítio.Amostral) #muitos erros no sitio amostral
unique(dados$Zona)
levels(dados$Zona) #alterar "PI " -> "PI"
dados$Zona <- gsub("PI ", "PI",dados$Zona)
unique(dados$Profundidade.Total.do.Sítio.Amostral) #trocar N.A. por NA
unique(dados$Ano)
unique(dados$Latitude)
unique(dados$Parâmetros)
levels(dados$Parâmetros)

#Dados faltantes -------
#substituir dados faltantes por NA




#Conversão de nomes----
#os parâmetros tem muitos nomes errados, preciso padronizar tudo!

#?????????????????????? 

#Salvar os arquivos após manipulação----
# Salvando em .RData
#save(nome do dataframe, file = "(3) dados_salvos_1.RData")

# Salvando em .csv
#write.csv(nome do dataframe, file = "(3) dados_salvos_3.csv", row.names = F)

# Salvando em Excel:
#write_xlsx(nome do dataframe, "(3) dados_salvos_2.xlsx")


#------EDICAO DE PLANILHA------

#Renomear variavel chave e trocar valores em N.A.
names(dados)
dados_1 <- dados %>%  
  rename(site = "Sítio.Amostral")
  

#Juntar dados atualizados de coordenada geográfica e localizacao e caracteristicas de cada sitio amostral

lat_long <- read.csv2("DADOS_LOCALIZACAO_csv.csv", h = T, sep = ";", dec = ",", stringsAsFactors = T)
lat_long <- lat_long[, 1:17]

#Renomear variável chave
lat_long <- lat_long %>% 
  rename(site = "Codigo.do.sitio.amostral")
 

#Juntar planilhas com base na variavel chave "site", excluir variaveis obsoletas e renomear

dados_1 <- dados_1 %>% 
  left_join(lat_long, by = "site")

#Excluir variaveis antigas

dados_1 <- dados_1[ , -c(2:8, 10, 12, 17, 24:26, 47, 48)]

dados_1 <- dados_1 %>% 
  rename(empresa = "Empresa", 
         tipo_empreendimento = "Tipo.do.empreendimento",
         empreendimento = "Empreendimento.y",
         cidade = "Municipio",
         estado = "Estado.y",
         bacia = "Bacia",
         curso = "CursoDagua",
         classe = "Classificação.CONAMA",
         status = "Status.do.Sítio.amostral",
         ambiente = "Tipo.de.ambiente",
         posicao = "Posicao",
         perfil = "Perfil.Coletado",
         site = "site",
         lat = "Coordenadas.LAT",
         long = "Coordenadas.LONG",
         zona = "Zona",
         prof_total = "Profundidade.Total.do.Sítio.Amostral",
         ano = "Ano",
         mes = "Mês",
         dia = "Dia",
         hora = "Hora",
         rede = "Rede.de.coleta",
         periodici = "Periodicidade",
         sazonal = "Sazonalidade",
         parameter = "Parâmetros",
         unidade = "Unidade.de.medida",
         sinal = "Sinal",
         result = "Resultado",
         analise_tipo = "Tipo.de.análise",
         atividade = "Atividade",
         obs1 = "Observações",
         obs2 = "Observação",
         regiao = "Região.de.Coleta",
         cod_ana = "Codigo.ANA",
         grupo = "Grupo.amostragem") %>% 
  mutate(prof_total = replace(prof_total, prof_total == "N.A.", "NA"))

#quer reorganizar a planilha usando o select, mas nao funciona

#select(empresa, tipo_empreendimento, empreendimento, cidade, estado, bacia, curso, cod_ana, status, ambiente, regiao, lat, long, posicao, site, perfil, zona, prof_total, ano, mes, dia, rede, periodici, sazonal, parameter, unidade, result, obs1, obs2)


#Análises descritivas por empreendimento



str(dados_1)
#Filtrar empreendimento por meio da funcao str_detect

dados_2 <- dados_1 %>% 
  filter(grepl("TM", site)) %>% 
  filter(parameter == "Oxigênio dissolvido")


#Cálculo de média, desvio padrão e n amostral por ponto de coleta  sazonalidade
dados_3 <- dados_2 %>% 
  group_by(site, sazonal) %>% 
  summarise(média = mean(result), desv_pad = sd(result),
            mediana = median(result),
            mínimo = min(result),
            máximo = max(result),
            quartil_1 = quantile(result, probs = 0.25),
            quartil_3 = quantile(result, probs = 0.75), 
            observações = n())

#Plotando gráfico

ggplot(dados_2, aes(x=factor(site, c())), y = result, fill = sazonal) +
  geom_boxplot() +
  scale_fill_manual(values = c("Seca" = "darkgoldenrod1", "Chuva" = "skyblue"))



#Contabilizando o numero de violacoes por ponto de coleta e sazonalidade

dados_4 <- dados_2 %>% 
  mutate(violacao = case_when(result >= 5 ~ "Sem violação", result < 5 & result >= 2 ~ "Violação", result < 2 ~ "Violação crítica")) %>% 
  group_by(site, sazonal) %>% 
  count(violacao)







