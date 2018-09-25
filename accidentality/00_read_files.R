rm(list=ls());gc()
if (!require('pacman')) install.packages('pacman'); library('pacman') 
p_load(readxl, tidyverse, lubridate, data.table)
# tomados de https://datos.madrid.es/portal/site/egob/menuitem.c05c1f754a33a9fbe4b2e4b284f1a5a0/?vgnextoid=7c2843010d9c3610VgnVCM2000001f4a900aRCRD&vgnextchannel=374512b9ace9f310VgnVCM100000171f5a0aRCRD&vgnextfmt=default

files <- list.files("./data/", pattern = ".xlsx")
files <- paste0("./data/", files)

df <- data.frame()
for (file in files){
  df0 <- read_excel(file)
  df <- rbind(df, df0)
}

# Formatear columnas
colnames(df) = tolower(colnames(df))
colnames(df) = str_replace_all(colnames(df), "[^[:alnum:]]", " ")
colnames(df) = str_replace_all(colnames(df), " ", "_")
colnames(df) = str_replace_all(colnames(df), "__", "")
colnames(df) = str_replace_all(colnames(df), "º", "")
colnames(df) = str_replace_all(colnames(df), "â", "")


df$fecha = substr(df$fecha, 1, 10)
# Formatear horas
df$hora <- df$rango_horario %>%
  substr(., 3, 5) %>% 
  str_replace_all(., ":", "") %>% 
  as.character() %>% as.numeric
# mes, año y fecha
df$mes <- month(ymd(df$fecha))
df$ano <- year(ymd(df$fecha))
df$fecha <- ymd(df$fecha)
# Dia semana
df <- df %>% mutate(dia = case_when(
  dia_semana == "LUNES" ~ 1,
  dia_semana == "MARTES" ~ 2,
  dia_semana == "MIERCOLES" ~ 3,
  dia_semana == "JUEVES" ~ 4,
  dia_semana == "VIERNES" ~ 5,
  dia_semana == "SABADO" ~ 6,
  dia_semana == "DOMINGO" ~ 7
))

df <- df %>% mutate(mes_etiqueta = case_when(
  mes == 1 ~ "Enero",
  mes == 2 ~ "Febrero",
  mes == 3 ~ "Marzo",
  mes == 4 ~ "Abril",
  mes == 5 ~ "Mayo",
  mes == 6 ~ "Junio",
  mes == 7 ~ "Julio",
  mes == 8 ~ "Agosto",
  mes == 9 ~ "Septiembre",
  mes == 10 ~ "Octubre",
  mes == 11 ~ "Noviembre",
  mes == 12 ~ "Diciembre"
))
# Etiquetas de la gravedad del accidente
df <- df %>%  mutate(gravedad = case_when(
  lesividad == "HL" ~ "Leve",
  lesividad == "HG" ~ "Grave",
  lesividad == "IL" ~ "Ileso",
  lesividad == "MT" ~ "Fallecido",
  lesividad == "NO ASIGNADA" ~ "Sin información"
))

df$tramo_edad %>% unique
df$edad <- substr(df$tramo_edad, 4,5)
df$edad %>% unique
df <- df %>%  
  mutate(edad = case_when(grepl("MA", edad) ~ "85",
                           grepl("CO", edad) ~"NA",
         TRUE ~edad))

df <- df %>% mutate(edad=as.numeric(str_replace(edad, " ", "")))

df$hora.c <- sin(2*pi*df$hora/24 )
df$mes.c <- sin(2*pi*df$mes/12 )



df$gravedad <- as.factor(df$gravedad )
levels(df$gravedad) =  c("Sin información", "Ileso", "Leve", "Grave", "Fallecido")
#dfSummary(df)

lesividad <- df %>% group_by(tipo_persona, gravedad) %>% 
  summarise(n = n())

with(df, 
     print(ctable(tipo_persona, gravedad, prop = 'r', totals = FALSE), 
           omit.headings = TRUE))
with(df, 
     print(ctable(gravedad, sexo, prop = "c", totals = FALSE), 
           omit.headings = TRUE))
# Quitamos los testigos pues solo aparecen y no participan ni son lesionados 
# df = df %>% filter(tipo_persona != "TESTIGO")

meses  <- c("Enero", "Febrero", "Marzo", "Abril", "Junio", "Julio", 
            "Agosto", "Septiembre", "Octubre", "Noviembre", "Diciembre")

df.tiempo <- df %>% dplyr::select(n_parte, fecha, ano, mes, mes_etiqueta, dia, dia_semana, gravedad) %>%
  distinct() 
levels(df.tiempo$mes_etiqueta) <- meses


df.dia <- df.tiempo %>% group_by(ano, mes, dia) %>%
  summarise(n=n()) 



save(df, df.dia, df.tiempo, file = "df.Rdata")
 
