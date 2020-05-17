
library(rwhatsapp)
library(lubridate)
library(tidyverse)
library(tidytext)
library(kableExtra)
library(RColorBrewer)
library(knitr)

## LIBS PARA EXPORTAR COMO HTML 
library(plotly);
library(htmlwidgets);

# LIBRERÍA PARA FETCH DE IMAGEN PNG DE EMOJI DESDE https://abs.twimg.com
library(ggimage)

# LEEMOS EL CHAT A TRAVÉS DEL TXT EXPORTADO DESDE LA APP
miChat <- rwa_read("miChat_1.txt")

# CHECAMOS UN PREVIEW DEL FORMATO DE DATOS DE LA TABLA
miChat %>% 
  head(10) %>%
  kable() %>% 
  kable_styling(font_size = 10)


# PREPARACIÓN DE DATOS PARA ANÁLIIS POR DATE/TIME
miChat <- miChat %>% 
  mutate(day = date(time)) %>% 
  mutate(
    # SEGMENTACIÓN POR MES
    estacion = case_when(
      day >= dmy(18082018) & day <= dmy(22092018) ~ "Verano 2018",
      day >= dmy(23092018) & day <= dmy(20122018) ~ "Otoño 2018",
      day >= dmy(21122018) & day <= dmy(20032019) ~ "Invierno 2018",
      day >= dmy(21032019) & day <= dmy(21062019) ~ "Primavera 2019",
      day >= dmy(22062019) & day <= dmy(23092019) ~ "Verano 2019",
      day >= dmy(23092019) & day <= dmy(20122019) ~ "Otoño 2019",
      day >= dmy(21122019) & day <= dmy(20032020) ~ "Invierno 2020",
      day >= dmy(21032020) ~ "Primavera 2020",
      T ~ "Fuera de rango"
    )
  ) %>% 
  mutate( estacion = factor(estacion) ) %>% 
  filter(!is.na(author))

# PALETA DE COLORES
paleta.estaciones <- brewer.pal(8,"Set1")[c(7,5,1,3,4,2,6,8)]

# VERIFICANDO CUÁNTOS MENSAJES SE ENVIARON DURANTE EL PERIODO DE TIEMPO
miChat %>% 
  group_by(estacion) %>% 
  count(day) %>%
  ggplot(aes(x = day, y = n, fill=estacion)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values=paleta.estaciones) +
  ylab("Número de mensajes") + xlab("Fecha") +
  ggtitle("Mensajes por día", "Frecuencia por estación del año") +
  theme_minimal() +
  theme( legend.title = element_blank(), 
         legend.position = "bottom")

ggplotly()


# MENSAJES POR DÍA DE LA SEMANA
miChat %>% 
  mutate( wday.num = wday(day),
          wday.name = weekdays(day)) %>% 
  group_by(estacion, wday.num, wday.name) %>% 
  count() %>% 
  ggplot(aes(x = reorder(wday.name, -wday.num), y = n, fill=estacion)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values=paleta.estaciones) +
  ylab("") + xlab("") +
  coord_flip() +
  ggtitle("Número de mensajes por día de la semana", "Frecuencia por estación del año") +
  theme_minimal() +
  theme( legend.title = element_blank(), 
         legend.position = "bottom")

ggplotly()



# MANTENER EL ORDEN DE DÍAS DE LA SEMANA Y NOMBRARLOS
diasemana <- c("domingo","lunes","martes","miércoles","jueves","viernes","sábado","domingo")
names(diasemana) <- 1:7


# MENSAJES POR HORA DEL DÍA
miChat %>% 
  mutate( hour = hour(time), 
          wday.num = wday(day),
          wday.name = weekdays(day)) %>% 
  count(estacion, wday.num, wday.name, hour) %>% 
  ggplot(aes(x = hour, y = n, fill=estacion)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values=paleta.estaciones) +
  ylab("Número de mensajes") + xlab("Horario") +
  ggtitle("Número de mensajes por hora del día", "Frecuencia según estación del año") +
  facet_wrap(~wday.num, ncol=7, labeller = labeller(wday.num=diasemana))+
  theme_minimal() +
  theme( legend.title = element_blank(), 
         legend.position = "bottom",
         panel.spacing.x=unit(0.0, "lines"))

ggplotly()


# CAMBIEMOS EL NOMBRE DE LOS USUARIOS POR CONFIDENCIALIDAD

levels(miChat$author)[2] <- "Ella"
levels(miChat$author)[1] <- "Él"


# MENSAJES POR USUARIO
miChat %>%
  mutate(day = date(time)) %>%
  group_by(estacion) %>% 
  count(author) %>% 
  ggplot(aes(x = reorder(author, n), y = n, fill=estacion)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values=paleta.estaciones) +
  ylab("Número total de mensajes") + xlab("Usuario") +
  coord_flip() +
  ggtitle("Número total de mensajes por usuario.", "¿Quién es más comunicativo? Frecuencia por estación del año") +
  theme_minimal() +
  theme( legend.title = element_blank(), 
         legend.position = "bottom")

ggplotly()



# VEAMOS LA ESTRUCTURA ANIDADA DE EMOJIS
miChat %>% 
  select(time, author, emoji, emoji_name) %>% 
  unnest(emoji, emoji_name) %>% 
  slice(1:10) %>% 
  kable()

# EMOJI RANKING
plotEmojis <- miChat %>% 
  unnest(emoji, emoji_name) %>% 
  mutate( emoji = str_sub(emoji, end = 1)) %>% # REMOVER LIGADURAS
  mutate( emoji_name = str_remove(emoji_name, ":.*")) %>% # REMOVER NOMBRES DE LIGADURA
  count(emoji, emoji_name) %>% 
  # PLOT TOP 20 EMOJIS
  top_n(30, n) %>% 
  arrange(desc(n)) %>% 
  # CREA UNA URL DE IMAGEN CON EL UNICODE DE EMOJI
  mutate( emoji_url = map_chr(emoji, 
                              ~paste0( "https://abs.twimg.com/emoji/v2/72x72/", as.hexmode(utf8ToInt(.x)),".png")) 
  )

# PLOT DEL RANKING DE EMOJIS MÁS USADOS
plotEmojis %>% 
  ggplot(aes(x=reorder(emoji_name, n), y=n)) +
  geom_col(aes(fill=n), show.legend = FALSE, width = .2) +
  geom_point(aes(color=n), show.legend = FALSE, size = 3) +
  geom_image(aes(image=emoji_url), size=.045) +
  scale_fill_gradient(low="#2b83ba",high="#d7191c") +
  scale_color_gradient(low="#2b83ba",high="#d7191c") +
  ylab("Número de veces que el emoji fue usado") +
  xlab("Emoji y significado") +
  ggtitle("Emojis más utilizados de manera general", "Emojis más usados por todos") +
  coord_flip() +
  theme_minimal() +
  theme()

ggplotly()




# EMOJI RANK POR USUARIO
plotEmojis <- miChat %>%
  unnest(emoji, emoji_name) %>%
  mutate( emoji = str_sub(emoji, end = 1)) %>% # 
  count(author, emoji, emoji_name, sort = TRUE) %>%
  # PLOT DEL TOP 8 EMOJIS POR USUARIO
  group_by(author) %>%
  top_n(n = 8, n) %>%
  slice(1:8) %>% 
  # CREA UNA URL DE IMAGEN CON EL UNICODE DE EMOJI
  mutate( emoji_url = map_chr(emoji, 
                              ~paste0("https://abs.twimg.com/emoji/v2/72x72/",as.hexmode(utf8ToInt(.x)),".png")) )

# PLOT DE LA DATA
plotEmojis %>% 
  ggplot(aes(x = reorder(emoji, -n), y = n)) +
  geom_col(aes(fill = author, group=author), show.legend = FALSE, width = .20) +
  # USAR PARA HACER FETCH DE UNA IMAGENPNG DE EMOJI https://abs.twimg.com
  geom_image(aes(image=emoji_url), size=.13) +
  ylab("Número de veces que se usó el emoji") +
  xlab("Emoji") +
  facet_wrap(~author, ncol = 5, scales = "free")  +
  ggtitle("Emojis más usados en la conversación, por usuario") +
  theme_minimal() +
  theme(axis.text.x = element_blank())

ggplotly()



library(tidytext)
library(stopwords)

# REMOVEMOS PALABRAS SIN SIGNIFICADO RELEVANTE, COMO ARTÍCULOS, PRONOMBRES, ETC.
remover_palabras <- c(stopwords(language = "pt"),
                      "multimedia", "y", "la", "el","en", "es", "si", "lo", "ya", "pero", "esa",
                      "los","yo","mi", "un", "con", "las", "omitido", "más","eso", "al", "una",
                      "del", "qué", "todo", "así", "le", "su", "va", "porque", "todos", "hay", "les",
                      "pue", "ese", "son", "está", "pues", "ahí", "sí","ver", "estás", "algo", "vas",
                      "ir","voy", "creo","fue","solo", "ni","sólo","nada", "aqui", "q", "tú", "fez")

# CONTEO DE PALABRAS
miChat %>%
  unnest_tokens(input = text, output = word) %>%
  filter(!word %in% remover_palabras) %>% 
  count(word) %>% 
  # PLOT DEL TOP 20 DE PALABRAS MÁS USADAS EN CONVERSACIÓN
  top_n(30,n) %>% 
  arrange(desc(n)) %>% 
  ggplot(aes(x=reorder(word,n), y=n, fill=n, color=n)) +
  geom_col(show.legend = FALSE, width = .1) +
  geom_point(show.legend = FALSE, size = 3) +
  scale_fill_gradient(low="#2b83ba",high="#d7191c") +
  scale_color_gradient(low="#2b83ba",high="#d7191c") +
  ggtitle("Palabras más usadas en la conversación de manera general") +
  xlab("Palabras") +
  ylab("Número de veces que se usó la palabra") +
  coord_flip() +
  theme_minimal()

ggplotly()



# CONTEO DE PALABRAS POR USUARIO
miChat %>%
  unnest_tokens(input = text,
                output = word) %>%
  filter(!word %in% remover_palabras) %>%
  count(author, word, sort = TRUE) %>%
  # TOP 15 PALABRAS MÁS USADAS POR USUARIO
  group_by(author) %>%
  top_n(n = 20, n) %>%
  slice(1:20) %>%
  ungroup() %>% 
  arrange(author, desc(n)) %>% 
  mutate(order=row_number()) %>% 
  ggplot(aes(x = reorder(word, n), y = n, fill = author, color = author)) +
  geom_col(show.legend = FALSE, width = .1) +
  geom_point(show.legend = FALSE, size = 3) +
  xlab("Palabras") +
  ylab("Número de veces que se usó la palabra") +
  coord_flip() +
  facet_wrap(~author, ncol = 3, scales = "free") +
  ggtitle("Palabras más usadas por usuario en la conversación") +
  theme_minimal()

ggplotly()


