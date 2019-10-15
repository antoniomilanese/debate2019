#Cargo todas las librerías
library(tidytext)
library(tidyverse)
library(quanteda)
library(ggplot2)
library(zoo)
library(tm)

#Leo la transcripción

debate_raw <- read_lines("transcripcion")

View(debate_raw)

#Listado con los nombres de los candidatos
candidatos_names <- paste(c("mauricio macri","alberto fernández","nicolás del caño",
                      "josé luis espert","juan josé gómez centurión","roberto lavagna"), collapse = "|")
candidatos_names
#Organizo el texto para poder procesarlo por candidato y otras operaciones
debate_clean <- debate_raw %>%
  as.data.frame(fix.empty.names = TRUE) %>% #Convierto el documento importado en una tabla (dataframe)
  rename(original = '.') %>% #Cambio el nombre de la columna por uno más sencillo de trabajar
  mutate(original = as.character(original)) %>% #Convierto la columna con el texto original en texto (está como factor)
  mutate(original = str_replace_all(original,"—"," — ")) %>%
  mutate(original = str_squish(original)) %>%
  mutate(original = str_to_lower(original)) %>%
  filter(!original=="") %>% #Elimino las filas en blanco
  mutate(work = if_else(str_detect(original,"—")==TRUE,original,"")) %>%
  separate(work, into= c("candidato", "text"), sep = "—") %>%
  mutate(text = if_else(str_detect(original,"—")==TRUE, str_extract(original,"—\\s+(.*)$"),original)) %>% #extraigo todo el texto que queda detras del simbolo
  mutate(text = str_remove_all(text,"— ")) %>%
  mutate(len = str_count(text)) %>%
  #Remuevo todos los candidatos uno por uno porque no logro que funcione el separador "|" y el regex juntos
  mutate(text = str_remove_all(text, "^mauricio macri$")) %>%
  mutate(text = str_remove_all(text, "^alberto fernández$")) %>%
  mutate(text = str_remove_all(text, "^nicolás del caño$")) %>%
  mutate(text = str_remove_all(text, "^josé luis espert$")) %>%
  mutate(text = str_remove_all(text, "^juan josé gómez centurión$")) %>%
  mutate(text = str_remove_all(text, "^roberto lavagna$")) %>%
  mutate(text = str_remove_all(text, "^rodolfo barili y maría laura santillán$")) %>%
  #limpio la columna de nombres de candidatos
  mutate(candidato=str_remove_all(candidato, "\\(|\\)|mauricio macri|nicolás del caño|roberto lavagna|alberto fernández|josé luis espert|juan josé gómez centurión")) %>%
  mutate(candidato = str_squish(candidato)) %>%
  mutate(candidato = str_to_upper(candidato))
  
debate_clean[debate_clean==""]<-NA

#Relleno hacia abajo
debate <- debate_clean %>%
  fill(candidato) %>%
  mutate(candidato = str_replace(candidato, "JLGC", "JJGC")) %>%
  mutate(candidato = str_replace(candidato, "NCD", "NDC")) %>%
  mutate(candidato = str_replace(candidato, "JLS", "JLE")) %>%
  filter(candidato %in% c("AF", "MM", "NDC", "JLE", "JJGC", "RL"))%>%
  distinct(candidato,text) %>%
  select(candidato, text) %>%
  rownames_to_column()
View(debate)

#Analisis
corpus_debate <- corpus(debate)
dfm_debate <- dfm(corpus_debate, remove = c(stopwords("es"),"decimos", "muchas", "hizo", "miren", "aquel",
                                            "declaró", "hizo", "aunque", "creen", "darse", "cuáles",
                                            "embargo"),  remove_punct = TRUE, groups = "candidato")


#tf_idf
tf_idf <- dfm_tfidf(dfm_debate, scheme_tf = "count", scheme_df = "inverse", base = 10)
tf_idf_df <- as.data.frame(tf_idf) %>%
  gather(key = "palabra", value = "tf_idf", -document) %>%
  rename(candidato = document)


#Visualización de tf-idf
grafico1 <- tf_idf_df %>%
  group_by(candidato, palabra) %>%
  tally(tf_idf) %>%
  top_n(3) %>%
  ggplot(aes(palabra,n)) +
  geom_bar(stat = "identity", fill="#FF4040") +
  coord_flip() +
  facet_wrap(~candidato, scales="free", ncol = 2) +
  theme_minimal() +
  labs(title = "Top palabras más representativas del discurso de los candidatos",
       subtitle = "tf-idf del debate presidencial 2019",
       caption = "Data source: https://www.infobae.com/politica/2019/10/14/el-primer-debate-presidencial-2019-completo/")
  
grafico1

#Sentiment
afinn <- read.csv("lexicon/lexico_afinn.en.es.csv", stringsAsFactors = F, fileEncoding = "latin1") %>% 
  tbl_df()

debate_token <- debate %>%
  unnest_tokens(input = "text", output = "Palabra") %>%
  inner_join(afinn, ., by = "Palabra") %>%
  mutate(Tipo = ifelse(Puntuacion > 0, "Positiva", "Negativa")) %>% 
  rename("Author" = candidato)
debate_token
debate_sentiment <- debate_token %>%
  group_by(rowname) %>%
  summarise(puntuacion = mean(Puntuacion)) %>%
  #junto las dos tablas, la de puntaje y la de los chats originales por el id común.
  left_join(debate, by = "rowname")
View(debate_sentiment)

#sentiment chart evoluCion
grafico2 <- debate_sentiment %>%
  ggplot(aes(as.numeric(rowname), puntuacion,  color="#FF4040")) +
  geom_jitter(show.legend = FALSE) +
  geom_smooth(se = FALSE, show.legend = FALSE) +
  facet_wrap(~candidato, scales="free", ncol = 2) +
  theme_minimal() +
  theme(legend.position="top") +
  xlab("orden en el debate") +
  labs(title = "Sentimientos en el discurso de cada candidato a lo largo de la exposición",
       subtitle = "sentimientos con vocabulario AFINN",
       caption = "Data source: https://www.infobae.com/politica/2019/10/14/el-primer-debate-presidencial-2019-completo/")


grafico2

#sentiment chart por candidato
grafico3 <- debate_sentiment %>%
  group_by(candidato) %>%
  summarise(sentimiento = mean(puntuacion)) %>%
  ggplot(aes(candidato, sentimiento, fill="#FF4040")) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  theme_minimal()

grafico3
