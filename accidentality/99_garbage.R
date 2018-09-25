
df.hora.gravedad <- df.hora %>% group_by(ano, mes, dia, hora, gravedad) %>% 
  summarise(n=n()) %>% spread(key = gravedad, value = n ) 

df.hora.gravedad %>% head

df.hora.gravedad = df.hora.gravedad %>%  mutate_all(funs(replace(., is.na(.), 0)))
div <- function(x) x/df.hora.n$n

df.hora.n <- left_join(df.hora.n, df.hora.gravedad)
df.hora.n %>% glimpse
#df.hora.n[,6:10] <- apply(df.hora.n[,6:10], 2, div)




df.gravedad.plot <- gather(df.hora.n, "gravedad", "n", 6:10)
df.gravedad.plot <- df.gravedad.plot %>% 
  filter(gravedad != "Sin información")
ggplot(df.gravedad.plot, aes(x=hora, y = n, col = gravedad)) +
  geom_smooth(alpha=0.7, se = FALSE) + facet_wrap(~mes) +
  ggtitle("Gravedad de los heridos en Madrid en 2010-2017 según hora") 
