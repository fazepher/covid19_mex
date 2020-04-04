library(pdftools)
library(tidyverse)
library(magrittr)

tabla_descargada <- pdf_text("Tabla_casos_positivos_COVID-19_resultado_InDRE_2020_04_02.pdf") %>% 
  map_dfr(~ str_split(.x,"\n") %>% 
            unlist %>% 
            str_remove_all("\r") %>% 
            str_squish %>% 
            str_to_upper %>% 
            data_frame(TEXTO = .)) 

# Algunos regex de utilidad
regex_ent_sexo <- "^([:alpha:]|[:space:])*(M|F){1}[:space:]{1}"
regex_ent_edad <- paste0(regex_ent_sexo,"[:digit:]{1,3}[:space:]{1}")
regex_formato_fecha <- "[:digit:]{2}/[:digit:]{2}/[:digit:]{4}"
regex_fecha_llegada <- paste0("((NA)|",regex_formato_fecha,")")

# Asumimos que: 
# 1) Los casos siempre tienen número de registro
# 2) Ninguno de los renglones de encabezados ni el pie de tabla al final comienzan por número
tabla_registros <- tabla_descargada %>% 
  filter(str_detect(TEXTO,"^[:digit:]")) %>% 
  # Extraemos num_caso
  mutate(TEXTO_AUX = str_remove(TEXTO,"^[:digit:]*[:space:]"),
         num_caso = str_extract(TEXTO,"^[:digit:]*")) %>% 
  # Extraemos entidad, sexo y edad
  mutate(ent = str_extract(TEXTO_AUX,regex_ent_sexo) %>% 
           str_remove("[:space:]{1}(M|F){1}[:space:]{1}$"),
         sexo_edad = str_extract(TEXTO_AUX,regex_ent_edad) %>% 
           map2_chr(ent,~str_remove(.x,.y) %>% str_trim)) %>% 
  separate(sexo_edad,c("sexo","edad"),sep="[:space:]") %>% 
  mutate(TEXTO_AUX = str_remove(TEXTO_AUX,regex_ent_edad)) %>% 
  # Extraemos fecha inicio
  mutate(fecha_inicio = str_extract(TEXTO_AUX,paste0("^",regex_formato_fecha)),
         TEXTO_AUX = str_remove(TEXTO_AUX,paste0("^",regex_formato_fecha,"[:space:]{1}"))) %>% 
  # Extraemos identificador confirmado
  mutate(identificador = str_extract(TEXTO_AUX,"^CONFIRMADO"),
         TEXTO_AUX = str_remove(TEXTO_AUX,"^CONFIRMADO[:space:]{1}")) %>% 
  # Extraemos procedencia y llegada
  mutate(fecha_llegada = str_extract(TEXTO_AUX,paste0("^([:alpha:]|[:space:])*",regex_fecha_llegada)) %>% 
           str_extract(regex_fecha_llegada),
         procedencia = str_extract(TEXTO_AUX,paste0("^([:alpha:]|[:space:])*",regex_fecha_llegada)) %>% 
           str_remove(regex_fecha_llegada) %>% str_trim) %>% 
  select(num_caso,ent,sexo,fecha_inicio,identificador,procedencia,fecha_llegada)
  
