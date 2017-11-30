library(dplyr)
library(ggplot2)
library(stringr)

servidores <- download.file("https://github.com/leobarone/meetup_patio_digital_microdados/raw/master/PERFIL_SERVIDOR_2016_FINAL.csv", "PERFIL_SERVIDOR_2016_FINAL.csv")

servidores <- read_delim(file = "PERFIL_SERVIDOR_2016_FINAL.csv", delim = ";", escape_double = FALSE, trim_ws = TRUE)

str(servidores)

print(servidores$DC_CARGO_BASE)

levels(servidores$DC_CARGO_BASE)

cargo <- factor(servidores$DC_CARGO_BASE)
levels(cargo)

servidores_v2 <- dplyr::filter(servidores, grepl('PROF.ENS.FUND.II E MED.-', DC_CARGO_BASE))
str(servidores_v2)

servidores_v3 <- dplyr::filter(servidores_v2, !grepl('PSICOLOGIA|PROT.DENTARIA|HIST.FILOSOFIA.EDUC.|ESTR.FUNC.ENS.1 GR.|DIR.E LEGISL.|CONT.CUSTOS|HIST.FILOS.EDUC', DC_CARGO_BASE))
str(servidores_v3)

servidores_v3$CD_SEXO <- str_replace_all(servidores_v3$CD_SEXO, "f", "F")

table(servidores_v3$CD_SEXO)

servidores_v3$CD_SEXO <- str_replace_all(servidores_v3$CD_SEXO, "m", "M")

table(servidores_v3$CD_SEXO)

glimpse(servidores$CD_CARGO_ATUAL)

servidores_v3$DC_CARGO_BASE <- str_replace_all(servidores_v3$DC_CARGO_BASE, "PROF.ENS.FUND.II E MED.-", "")

servidores_v3$disciplinas <- factor(servidores_v3$DC_CARGO_BASE, order = T, levels <- c("FISICA", "QUIMICA", "BIOL.PROG.SAUDE", "MATEMATICA", "CIENCIAS", "ED.FISICA", "GEOGRAFIA", "SOCIOLOGIA" ,"HISTORIA", "PORTUGUES", "INGLES", "ESPANHOL", "FILOSOFIA","ED.ARTISTICA"))

servidores_v3$disciplinas <- str_replace_all(servidores_v3$disciplinas, "FISICA", "Física")
servidores_v3$disciplinas <- str_replace_all(servidores_v3$disciplinas, "QUIMICA", "Química")
servidores_v3$disciplinas <- str_replace_all(servidores_v3$disciplinas, "BIOL.PROG.SAUDE", "Biologia")
servidores_v3$disciplinas <- str_replace_all(servidores_v3$disciplinas, "MATEMATICA", "Matemática")
servidores_v3$disciplinas <- str_replace_all(servidores_v3$disciplinas, "CIENCIAS", "Ciências")
servidores_v3$disciplinas <- str_replace_all(servidores_v3$disciplinas, "ED.FISICA", "Ed. Física")
servidores_v3$disciplinas <- str_replace_all(servidores_v3$disciplinas, "GEOGRAFIA", "Geografia")
servidores_v3$disciplinas <- str_replace_all(servidores_v3$disciplinas, "SOCIOLOGIA", "Sociologia")
servidores_v3$disciplinas <- str_replace_all(servidores_v3$disciplinas, "HISTORIA", "História")
servidores_v3$disciplinas <- str_replace_all(servidores_v3$disciplinas, "PORTUGUES", "Português")
servidores_v3$disciplinas <- str_replace_all(servidores_v3$disciplinas, "INGLES", "Inglês")
servidores_v3$disciplinas <- str_replace_all(servidores_v3$disciplinas, "ESPANHOL", "Espanhol")
servidores_v3$disciplinas <- str_replace_all(servidores_v3$disciplinas, "FILOSOFIA", "Filosofia")
servidores_v3$disciplinas <- str_replace_all(servidores_v3$disciplinas, "ED.ARTISTICA", "Ed. Artística")

# servidores_v3$disciplinas <- factor(servidores_v3$disciplinas, order = T, levels <- c("Física", "Química", "Biologia", "Matemática", "Ciências", "Ed. Física", "Geografia", "Sociologia", "História", "Português", "Inglês", "Espanhol", "Filosofia", "Ed. Artística"))


servidores_v3$disciplinas <- servidores_v3$disciplinas %>%
  factor(levels = c("Física", "Química", "Biologia", "Matemática", "Ciências", "ED.Física", "Geografia", "Sociologia", "História", "Português", "Inglês", "Espanhol", "Filosofia", "Ed.Artística"))

ggplot(servidores_v3) + 
  geom_bar(mapping = aes(x = disciplinas, fill = CD_SEXO), position = "fill", alpha = 3/4) +
  coord_flip() +
  labs(x = "Disciplina", y = "Proporção", title = "Docentes da rede municipal de Ensino Fundamental II e Médio, por disciplina oferecida e gênero", caption = "Fonte: Secretaria Municipal de Educação de São Paulo. Autora: Camila Gonçalves", fill = "Gênero:") +
  scale_fill_discrete(labels=c("Feminino", "Masculino")) +
  theme(legend.position = "bottom")

?geom_bar

glimpse(servidores_v3$disciplinas)

glimpse(servidores$DC_CARGO_ATUAL)

servidores_v4 <- dplyr::filter(servidores, grepl('DIRETOR DE ESCOLA', DC_CARGO_BASE))

View(servidores_v4)

ggplot(servidores_v4) +
  geom_bar(mapping = aes(x = DC_CARGO_BASE, fill = CD_SEXO), position = "fill", alpha = 3/4)

############################################################################################################

library(dplyr)
library(readr)
library(ggmap)
library(stringr)

dir_esc <- read.csv(file = "dir_esc.csv", sep = ",",stringsAsFactors = F)
View(dir_esc)

dir_esc <- rename(dir_esc, lat = latitude, lon = longitude)

dir_esc <- mutate(dir_esc, lat = lat / 1000000, lon = lon / 1000000)

dir_esc$cd_sexo <- str_replace_all(dir_esc$cd_sexo, "f", "F")

dir_esc$cd_sexo <- str_replace_all(dir_esc$cd_sexo, "m", "M")

se <- c(lon = -46.6362714, lat = -23.5500806)

map_sp <- get_map(se, zoom = 12)

plot(map_sp)

ggmap(map_sp) + 
  geom_point(aes(lon, lat, color = cd_sexo, shape = cd_sexo), size = 4, alpha = 3/4, data = dir_esc) + 
  labs(x = "", y = "", title = "Diretores de escolas da rede municipal de São Paulo, por gênero", caption = "Fonte: Secretaria Municipal de Educação de São Paulo. Autora: Camila Gonçalves") +
  scale_color_discrete(name = "Gênero:", labels = c("Feminino", "Masculino")) +
  scale_shape_discrete(name = "Gênero:", labels = c("Feminino", "Masculino")) +
  theme(legend.position = "bottom")

####################################################################################################
matriculas <- read_delim(file = "Microdados_EOL_Matriculas_2016.csv", delim = ";", escape_double = FALSE, trim_ws = TRUE)

ggplot(matriculas) +
  geom_bar(mapping = aes(x = factor(SIT_AL_ABAND), y = ..prop.., fill = CD_SEXO), stat = "count")

####################################################################################
library(dplyr)
library(readr)
library(ggmap)
library(stringr)

table(servidores$CD_SEXO)

servidores$CD_SEXO <- str_replace_all(servidores$CD_SEXO, "f", "F")

servidores$CD_SEXO <- str_replace_all(servidores$CD_SEXO, "m", "M")

servidores$NIVEL_FORM_v2 <- servidores$NIVEL_FORM %>%
  factor(levels = c("1 GRAU OU INFERIOR", "MAGISTERIO", "LICENCIATURA CURTA", "LICENCIATURA PLENA", "BACHARELADO", "POS GRADUACAO LATO SENSU", "MESTRADO", "DOUTORADO"))

servidores$CD_SEXO <- str_replace_all(servidores$CD_SEXO, "F", "Feminino")

servidores$CD_SEXO <- str_replace_all(servidores$CD_SEXO, "M", "Masculino")

ggplot(servidores) +
  geom_bar(mapping = aes(x = CD_SEXO, fill = as.factor(NIVEL_FORM_v2)), position = "fill", alpha = 3/4) + 
  labs(x = "", y = "Gênero", title = "Funcionários da rede municipal de São Paulo, por gênero e escolaridade", caption = "Fonte: Secretaria Municipal de Educação de São Paulo. Autora: Camila Gonçalves") +
  scale_fill_brewer(type = "seq", palette = "YlOrRd", name = "Escolaridade:", labels = c("1º Grau ou Inferior", "Magistério", "Licenciatura Curta", "Licenciatura Plena", "Bacharelado", "Pós-Grad Lato Sensu", "Mestrado", "Doutorado"))