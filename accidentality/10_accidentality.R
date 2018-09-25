rm(list=ls());gc()
if (!require('pacman')) install.packages('pacman'); library('pacman') 
p_load(readxl, mgcv, tidyverse, knitr)
# datos tomados de https://datos.madrid.es/portal/site/egob/menuitem.c05c1f754a33a9fbe4b2e4b284f1a5a0/?vgnextoid=7c2843010d9c3610VgnVCM2000001f4a900aRCRD&vgnextchannel=374512b9ace9f310VgnVCM100000171f5a0aRCRD&vgnextfmt=default
load(file = "df.Rdata")

theme_set( theme_bw() )
# gráfico global para explorar
# truco para poner las etiquetas en cada facet
meses <- c("1"="Enero", "2"="Febrero", "3"="Marzo", "4"="Abril", "5"="Mayo", 
           "6"="Junio", "7"="Julio", "8"="Agosto", "9"="Septiembre", 
           "10"="Octubre", "11"="Noviembre", "12"="Diciembre")
accidentes_exploratorio <- ggplot(df.dia, aes(x=dia, y = n, col =  factor(ano) )) +
  geom_smooth(se=FALSE) +
  facet_wrap(.~mes, ncol = 4, labeller = as_labeller(meses)) +
  ggtitle("Accidentes por día de la semana en Madrid en 2016-2017") +
  scale_x_continuous(labels = c("L", "M", "M", "J", "V", "S", "D"), breaks = c(1:7))+
  scale_color_discrete(name="Año")
  
accidentes_exploratorio
ggsave(accidentes_exploratorio, filename = "plots/accidentes_exploratorio.png")

# Veamos en detalle la variación por año
df.ano <- df.dia %>%  group_by(ano) %>% summarise(n = sum(n))
accidentes_año <- ggplot(df.ano , aes(x=ano, y = n)) + 
  geom_point(size = 4) +
  geom_smooth(method="lm") +
  ggtitle("Número total de accidente por año en Madrid") +
  ylim(0,21000)
accidentes_año 
ggsave(accidentes_año , filename = "plots/accidentes_año.png")

# seguimos explorando
df.dia.sm <- df.dia %>% group_by(ano, dia) %>%  summarise(media = mean(n))
df.dia.sm %>% kable
media_dia <- df.dia.sm %>% ggplot(aes(x=factor(dia), y=media)) +
  geom_point() + coord_flip() + facet_wrap(~ano) +  
  scale_x_discrete(labels = c("L", "M", "M", "J", "V", "S", "D")) + xlab("Día") +
  ylab("") + ggtitle("Media de accidentes por día cada año")
media_dia
ggsave(media_dia, filename = "plots/media_dia.png")

# obtenemos el numero de accidentes por hora
df.hora <- df %>% dplyr::select(n_parte, fecha, ano, mes, 
                                mes_etiqueta, dia, dia_semana, hora, hora.c, mes.c, gravedad) %>%
  distinct() 
df.hora.n <- df.hora %>% group_by(ano, mes, dia, hora) %>%
  summarise(n=n()) 

df.hora.media <- df.hora.n %>% group_by(mes, dia, hora) %>% summarise(media=mean(n))
# 
media_hora_mes <- ggplot(df.hora.media, aes(x=hora, y = media)) +
  geom_smooth() + 
  facet_wrap(.~mes, ncol = 4) +
  ggtitle("Media de accidentes por hora y mes en Madrid en 2010-2017") 
media_hora_mes
ggsave(media_hora_mes, filename="plots/media_hora_mes.png")

# Vamos a por el GAM 

df.hora.n$ano_int <- as.integer(df.hora.n$ano)
df.hora.n$mes <- factor(df.hora.n$mes)
contrasts(df.hora.n$mes) <- contr.sum

# Separamos en datos de entrenamiento y de testeo
train <- ifelse(runif(nrow(df.hora.n)) < 0.8, TRUE, FALSE)
df.hora.train <-  df.hora.n[train,]
df.hora.train %>% head

# Vamos a poner el año y el mes como efecto aleatorio y 
# establecer la hora como un predictor con un máximo de 
# 23 'nudos' que vienen a ser las 24 horas, menos uno. 
# y añadir el año como efecto fijo (a raíz de lo visto en 
# en el analisis exploratorio)
# La función de enlace será una quassipoisson, pues 
# no contamos con que la media sea igual a la varianza
mod.horas.gam <- gam(n ~ s(hora, k = 23) +  
                       s(mes, bs="re") + 
                       mes +
                       ano,
                         family=quasipoisson(link=log),
                         data = df.hora.train)
(gam.summary <- summary(mod.horas.gam))
gam.summary$r.sq


# Hacemos predicciones con el modelo
df.hora.train$pred <- predict(mod.horas.gam, type = "response" )
df.hora.train$res <- df.hora.train$n - df.hora.train$pred 
# calculamos el RMSE
sqrt(mean(df.hora.train$res**2 )) 
# [1] 5.612995

# ahora vamos a ver que tal se porta el modelo con datos 
# que no son los de entrenamiento : ) 
df.hora.test <-   df.hora.n[!train,]
df.hora.test$pred_n <- predict(mod.horas.gam, df.hora.test, type="response")
df.hora.test$res_n <- df.hora.test$n - df.hora.test$pred_n
hist(df.hora.test$res_n)

sqrt(mean(df.hora.test$res_n**2 )) 
# [1] 5.773248
#  Vemos que un RMSE muy parecido
qqnorm(df.hora.test$res_n)

# Ahora vamos a por un gráfico que nos diga las horas que hay más accidentes
# en Madrid
df.hora.n$pred <- predict(mod.horas.gam, df.hora.n, type="response" )
predicciones <- ggplot(df.hora.n, aes(x=hora, y=pred)) + 
  geom_line(size=2, alpha=.5, col = "red") + 
  geom_point(aes(y=n), col = "black", alpha=0.03) +
  facet_wrap(~mes, labeller = as_labeller(meses)) +
  ggtitle("Predicción (GAM) de accidentes por hora del día en Madrid 2010-17 en cada mes") +
  ylab("Número de accidentes") +
  xlab("Hora del accidente") + ylim(0,40)

predicciones
ggsave(predicciones, filename = "plots/predicciones.png")
save(list=ls(), file =  "accidentality_models.RData")

# Como es esperable, las horas que mas accidentes tienen son la de ir a trabajr
# y llevar los niños (~0900), la de ir a comer (~1400) y la de volver a casa (~2000)
# Además el mes que parece ser el diferente es agosto con picos mucho
# menos pronunciados. En la salida del modelo podemos ver como agosto tiene una 
# la beta es de -0.4, es decir se desvía de la media de meses 1.5 accidentes 


# Más adelante me gustaría incorporar en el modelo más predictores interesantes
# ¿Llovía? ¿Había más taxis?




