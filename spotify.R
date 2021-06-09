library("readxl")
library(readr)
library(stringr)
library(dplyr)

#Leo los datos
df <- read.csv("tracks.csv",header = TRUE,sep = ",", encoding = "UTF-8")
df_art <- read.csv("artists.csv",encoding = "UTF-8")

#Selecciono como artista de la canción al primero de los artistas
#Nombre
df$artists <- gsub('""', '"', df$artists)
df$artists <- gsub("\\['", '\\["', df$artists)
df$artists <- gsub("']", '"]', df$artists)
df$artists <- gsub("', '",'", "', df$artists)
df$artists <- sapply(strsplit(df$artist,'"'), '[', 2)
df$artists <- gsub("',", "", df$artists)

#Id
df$id_artists <- sapply(strsplit(df$id_artists,"'"), '[', 2)
df$release_date <- as.numeric(substr(df$release_date, 1, 4))

#Cambio los puntos por comas para que tableau pueda leer estos campos como numéricos
df$danceability <- gsub("\\.", "\\,", as.character(df$danceability))
df$acousticness <- gsub("\\.", "\\,", as.character(df$acousticness))
df$energy <- gsub("\\.", "\\,", as.character(df$energy))
df$loudness <- gsub("\\.", "\\,", as.character(df$loudness))
df$speechiness <- gsub("\\.", "\\,", as.character(df$speechiness))
df$instrumentalness <- gsub("\\.", "\\,", as.character(df$instrumentalness))
df$liveness <- gsub("\\.", "\\,", as.character(df$liveness))
df$valence <- gsub("\\.", "\\,", as.character(df$valence))
df$tempo <- round(df$tempo)

#Pongo el género como el primero que aparece.
df_art$genres <- gsub('""', '"', df_art$genres)
df_art$genres <- gsub("\\['", '\\["', df_art$genres)
df_art$genres <- gsub("']", '"]', df_art$genres)
df_art$genres <- gsub("', '",'", "', df_art$genres)
df_art$genres <- sapply(strsplit(df_art$genres,'"'), '[', 2)
df_art$id_artists <- df_art$id
df_art$popularity_artist <- df_art$popularity

#Realizo el join
df_join <- merge(x=df,y=df_art[,c("id_artists","genres","popularity_artist")],by="id_artists",all.x=TRUE)
df_join <- df_join[complete.cases(df_join), ]

#Creo clases de géneros
df_join$generic_genres <- case_when(grepl("pop",df_join$genres, fixed = TRUE) ~ "Pop"
          ,grepl("rock",df_join$genres, fixed = TRUE) ~ "Rock"
          ,grepl("blues",df_join$genres, fixed = TRUE) ~ "Blues/Jazz"
          ,grepl("jazz",df_join$genres, fixed = TRUE) ~ "Blues/Jazz"
          ,grepl("r&b",df_join$genres, fixed = TRUE) ~ "R&B"
          ,grepl("latin",df_join$genres, fixed = TRUE) ~ "Latin"
          ,grepl("country",df_join$genres, fixed = TRUE) ~ "Country/Folk"
          ,grepl("folk",df_join$genres, fixed = TRUE) ~ "Country/Folk"
          ,grepl("indie",df_join$genres, fixed = TRUE) ~ "Indie"
          ,grepl("hip hop",df_join$genres, fixed = TRUE) ~ "Hip hop/Rap"
          ,grepl("rap",df_join$genres, fixed = TRUE) ~ "Hip hop/Rap"
          ,grepl("classical",df_join$genres, fixed = TRUE) ~ "Classical"
          ,grepl("reggae",df_join$genres, fixed = TRUE) ~ "Reggae"
          ,TRUE ~ 'Other'
)

#Creo el campo de década
df_join$decade <- paste(as.character(df_join$release_date - df_join$release_date %% 10),"'s", sep="")

#Filtro los datos para que no haya tantos eliminando las canciones con menos popularidad.
df_join <- filter(df_join, popularity > 35)

#Las canciones están repetidas ya que pueden venir de distintos álbumes
#Cojo la que salío antes como mayor popularidad.
df_join <- df_join  %>% 
  group_by(name,id_artists) %>% 
  slice(which.min(release_date)) %>% 
  slice(which.max(popularity))

#Creo el csv para realizar el pivot de la tabla para el radar chart
df_pivot <- df_join[,c("id","id_artists","genres","generic_genres","popularity",
                       "danceability","energy","speechiness","acousticness",
                       "instrumentalness","liveness","valence")]


#Escribo los csv
write_excel_csv(df_join, "spotify.csv",delim = ";")
write_excel_csv(df_pivot, "pivot.csv",delim = ";")
     
     