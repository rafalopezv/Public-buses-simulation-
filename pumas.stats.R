#--------------------------
# Estadísticas Puma Katari/Stats 
#--------------------------
rm(list = ls())

# Fijar locale/set locale
Sys.setlocale(locale = "es_ES.UTF-8")

# Cargar librerías/load libraries
pkgs <- c("magrittr", "tidyverse", "lubridate", "rio", "highcharter")
lapply(pkgs, function(x) require(x, character.only = TRUE))
rm(pkgs)

# Importar base generada/import data
puma <- import("/Users/rafalopezv/Dropbox/PUMA KATARI/presentation/puma.csv")
lapply(puma, class)
for(i in 1:4) {
  puma[, i] <- as.POSIXct(puma[, i])
}
rm(i)


# Renombrar variables y corregir rts y rta para parada 21
# Rename variables and fix rts and rta for stop 21
names(puma)
puma %<>% rename(parada = parada.i., vuelta = vuelta.x., distancia = distancia.i.)
temp <- puma$parada == 21
puma[temp, c("rts", "its")] <- NA
rm(temp)

# Crear nuevas variables/new variables
puma %<>% mutate(atraso.salidas.minutos = round((rts - its)/60, 2) %>% as.numeric(),
                 atraso.llegada.minutos = round((rta - ita)/60, 2) %>% as.numeric(),
                 lag.ideal.salida = lag(its, 1),
                 tiempo.ideal.tramos.min = round(ita - lag.ideal.salida, 2) %>% as.numeric(),
                 lag.real.salida = lag(rts, 1),
                 tiempo.real.tramos.min = round(rta - lag.real.salida, 2) %>% as.numeric(),
                 atraso.real.de.ideal.min = round(tiempo.real.tramos.min - tiempo.ideal.tramos.min, 2) %>% as.numeric(),
                 tiempo.espera.ideal.segundos = as.numeric(its - ita),
                 tiempo.espera.real.segundos = as.numeric(rts - rta),
                 atraso.esperas.segundos = tiempo.espera.real.segundos - tiempo.espera.ideal.segundos,
                 `KM/H.ideal` =  round((distancia/1000) / (as.numeric(tiempo.ideal.tramos.min/60))),
                 `KM/H.real` =  round((distancia/1000) / (as.numeric(tiempo.real.tramos.min/60))),
                 dia = wday(rts),
                 hora = hour(rts),
                 tramo1 = lag(parada, 1),
                 tramo = paste(tramo1, "-", parada)) %>%
  select(-lag.ideal.salida, -lag.real.salida, -tramo1)

# rellenar NA en horas y dias/complete vector of hours and days
puma$dia[is.na(puma$dia)] <- wday(puma$rta[is.na(puma$dia)]) 
puma$dia[is.na(puma$hora)] <- hour(puma$rta[is.na(puma$hora)]) 

# cambiar días de la semana de forma literal/change weekdays as names
puma$dia.literal[puma$dia == 1] <- "domingo"
puma$dia.literal[puma$dia == 2] <- "lunes"
puma$dia.literal[puma$dia == 3] <- "martes"
puma$dia.literal[puma$dia == 4] <- "miercoles"
puma$dia.literal[puma$dia == 5] <- "jueves"
puma$dia.literal[puma$dia == 6] <- "viernes"
puma$dia.literal[puma$dia == 7] <- "sabado"

#-------------------
# graficos/graphs
summary(puma)

# tiempo de partida a llegada final
a <- puma %>% select(rts, rta, parada, vuelta) %>%
  filter(parada == 1 | parada == 21) %>%
  mutate(rts = lag(rts, 1), tiempo = as.numeric(rta - rts)) %>% 
  filter(parada == 21)  

b <- hchist(a$tiempo, showInLegend = FALSE, breaks = 20) %>%
  hc_subtitle(text = "4676 viajes en un mes") %>%
  hc_title(text = "Tiempo de toda la ruta") %>%
  hc_add_theme(hc_theme_db()) %>% #hc_theme_db
  hc_xAxis(title = list(text = "Minutos")) %>%
  hc_yAxis(title = list(text = "Viajes")) %>% 
  hc_credits(enabled = TRUE, text = "Hacer click sobre el area que desea agrandar")

sum(a$tiempo <=60)/nrow(a) # % de veces que el tiempo está por debajo de la hora
a$tiempo %>% min
a$tiempo %>% max

# detalle de tiempos de parada 1 a parada final
c <- a %>% select(rts, tiempo)
rownames(c) <- c[, 1] 
c$tiempo %<>% round(., 1)
c[, 1] <- NULL
c %<>% xts::as.xts()

d <- highchart(type = "stock") %>% 
  hc_title(text = "Tiempo de viajes desde primera a última parada") %>% 
  hc_subtitle(text = "4676 viajes en un mes") %>%
  hc_add_series(c, id = "tiempo", name = "Tiempo") %>%
  hc_add_theme(hc_theme_economist()) %>%
  hc_xAxis(title = list(text = "Fecha y hora de la partida del bus")) %>%
  hc_yAxis(title = list(text = "Minutos")) 

# tiempo excluyendo días conflictivos
a$dia <- day(a$rts)
a %<>% filter(dia != 3 & dia != 4 & dia != 13 & dia != 14)

# detalle de tiempos de parada 1 a parada final en dias no conflictivos
c <- a %>% select(rts, tiempo)
rownames(c) <- c[, 1] 
c$tiempo %<>% round(., 1)
c[, 1] <- NULL
c %<>% xts::as.xts()

noconf <- highchart(type = "stock") %>% 
  hc_title(text = "Tiempo de viajes desde la primera a la última parada en días normales") %>% 
  hc_subtitle(text = "4029 viajes en un mes") %>%
  hc_add_series(c, id = "tiempo", name = "Tiempo") %>%
  hc_add_theme(hc_theme_economist()) %>%
  hc_xAxis(title = list(text = "Fecha y hora de la partida del bus")) %>%
  hc_yAxis(title = list(text = "Minutos")) 

# Boxplots por tramos tramos en dias sin  protestas 
tramos <- puma %>% 
  mutate(dia.del.mes = day(rts)) %>%
  filter(dia.del.mes != 3 & dia.del.mes != 4 & dia.del.mes != 13 & dia.del.mes != 14 &
           parada != 1) %>%
  arrange(parada)

tramos.noconf <- hcboxplot(x = tramos$tiempo.real.tramos.min, var = tramos$tramo, column = T) %>%  
  hc_add_theme(hc_theme_elementary()) %>%
  hc_xAxis(title = list(text = "Tramos")) %>%
  hc_yAxis(title = list(text = "Minutos")) %>%
  hc_title(text = "Tiempo de recorrido por tramos en días normales") %>%
  hc_subtitle(text = "76562 tramos recorridos por mes")

# Boxplots por tramos tramos en dias sin  protestas  quitando tramos 1-2 y 3-4
tramos1 <- tramos %>% 
  filter(tramo != "1 - 2" & tramo != "3 - 4")

tramos.noconf1 <- hcboxplot(x = tramos1$tiempo.real.tramos.min, var = tramos1$tramo) %>%  
  hc_add_theme(hc_theme_elementary()) %>%
  hc_xAxis(title = list(text = "Tramos")) %>%
  hc_yAxis(title = list(text = "Minutos")) %>%
  hc_title(text = "Tiempo de recorrido por tramos en días normales") %>%
  hc_subtitle(text = "68504 tramos recorridos por mes (excluyendo tramos con más demoras)")

# boxplot por hora para tramos 1-2 3-4 en normales
tramos2 <- tramos %>% 
  filter(tramo == "1 - 2")

tramos.noconf2 <- hcboxplot(x = tramos2$tiempo.real.tramos.min, var = tramos2$hora) %>%  
  hc_add_theme(hc_theme_elementary()) %>%
  hc_xAxis(title = list(text = "Hora")) %>%
  hc_yAxis(title = list(text = "Minutos")) %>%
  hc_title(text = "Tiempo de recorrido en el Plaza Camacho-Transferencia PUC en días normales") %>%
  hc_subtitle(text = "4029 tramos recorridos por mes")

tramos2 <- tramos %>% 
  filter(tramo == "3 - 4")

tramos.noconf3 <- hcboxplot(x = tramos2$tiempo.real.tramos.min, var = tramos2$hora) %>%  
  hc_add_theme(hc_theme_elementary()) %>%
  hc_xAxis(title = list(text = "Hora")) %>%
  hc_yAxis(title = list(text = "Minutos")) %>%
  hc_title(text = "Tiempo de recorrido en el tramo Cancha Zapata-Curva de Holguín en días normales") %>%
  hc_subtitle(text = "4029 tramos recorridos por mes")

# tiempo en tramos y atraso en paradas
e <- puma %>%
  group_by(parada, tramo) %>%
  summarize(minimo = min(tiempo.real.tramos.min, na.rm = T),
            maximo = max(tiempo.real.tramos.min, na.rm = T),
            promedio = round(mean(tiempo.real.tramos.min, na.rm = T), 2),
            mediana = median(tiempo.real.tramos.min, na.rm = T)) %>%
  filter(parada != 1)
  
f <- highchart() %>%
  hc_xAxis(categories = e$tramo, title = list(text = "Tramo entre paradas")) %>%
  hc_yAxis(title = list(text = "Minutos")) %>%
  hc_title(text = "Tiempo de viaje en los 20 tramos de la ruta") %>%
  hc_subtitle(text = "93520 tramos en un mes") %>%
  hc_add_series(name = "Tiempo minimo", data = e$minimo) %>%
  hc_add_series(name = "Tiempo promedio", data = e$promedio) %>%
  hc_add_series(name = "Tiempo maximo", data = e$maximo) %>%
  hc_add_theme(hc_theme_economist()) %>%
  hc_tooltip(croshairs = TRUE, backgroundColor = "white",
               shared = TRUE, borderWidth = 1) %>%
  hc_credits(enabled = TRUE, text = "Seleccionar variable")

# tiempos en tramos 1-2, 2-3, 3-4
g <- puma %>% filter(tramo == "1 - 2"|
                       tramo == "2 - 3"|
                       tramo == "3 - 4")

g$tramo %<>% gsub("1 - 2", "Plaza Camacho-Transferencia PUC", .)
g$tramo %<>% gsub("2 - 3", "Transferencia PUC-Cancha Zapata", .)
g$tramo %<>% gsub("3 - 4", "Cancha Zapata-Curva de Holguín", .)

h <- hcboxplot(x = g$tiempo.real.tramos.min, var = g$hora, var2 = g$tramo,
          outliers = T) %>% 
  hc_chart(type = "column") %>%
  hc_add_theme(hc_theme_elementary()) %>%
  hc_xAxis(title = list(text = "Hora del día")) %>%
  hc_yAxis(title = list(text = "Minutos")) %>%
  hc_title(text = "Distribución de tiempos en tramos problemáticos (1-4)") %>%
  hc_subtitle(text = "14028 viajes en total, 4676 viajes por tramo") %>%
  hc_credits(enabled = TRUE, text = "Seleccionar tramo")

i <- hcboxplot(x = g$tiempo.real.tramos.min, var = g$hora, var2 = g$tramo,
               outliers = T) %>% 
  hc_chart(type = "column") %>%
  hc_add_theme(hc_theme_elementary()) %>%
  hc_xAxis(title = list(text = "Hora del día")) %>%
  hc_yAxis(title = list(text = "Minutos"), max = 14) %>%
  hc_title(text = "Distribución de tiempos en tramos problemáticos (1-4)") %>%
  hc_subtitle(text = "14028 viajes en total, 4676 viajes por tramo") %>%
  hc_credits(enabled = TRUE, text = "Seleccionar tramo")

# tiempo para subir pasajeros
pasajeros <- hchist(puma$tiempo.espera.real.segundos, showInLegend = FALSE, breaks = 20) %>%
  hc_subtitle(text = "98196 paradas en un mes") %>%
  hc_title(text = "Tiempo para subir y bajar pasajeros") %>%
  hc_add_theme(hc_theme_smpl()) %>% #hc_theme_sandsignika hc_theme_smpl
  hc_xAxis(title = list(text = "Segundos")) %>%
  hc_yAxis(title = list(text = "Paradas")) 

# kilometros por hora
hchist(puma$`KM/H.real`, showInLegend = FALSE, breaks = 40) %>%
  hc_subtitle(text = "98195 tramos en un mes") %>%
  hc_title(text = "Velocidad entre tramos") %>%
  hc_add_theme(hc_theme_db()) %>% #hc_theme_d
  hc_xAxis(title = list(text = "Kilómetros por hora")) %>%
  hc_yAxis(title = list(text = "Tramos")) 

# atrasos en salidas
hchist(puma$atraso.salidas.minutos, showInLegend = FALSE) %>%
  hc_title(text = "Minutos de atraso en las salidas de cada parada") %>%
  hc_subtitle(text = "93520 salidas en un mes") %>%
  hc_add_theme(hc_theme_db()) %>% #hc_theme_db
  hc_xAxis(title = list(text = "Minutos"), max = 130) %>%
  hc_yAxis(title = list(text = "Salidas"))

# cálculo de horas perdidas
temp <- puma %>% 
  mutate(dia.mes = day(rts)) %>%
  filter(dia.mes == 3 | dia.mes == 4 | dia.mes == 13 | dia.mes == 14) %>%
  summarize(sum(atraso.real.de.ideal.min, na.rm = T))

((temp/60)*10)/24 # perdida de tiempo pasajeros en días

(temp/60)

temp <- puma %>% 
  mutate(dia.mes = day(rts)) %>%
  filter(dia.mes != 3 & dia.mes != 4 & dia.mes != 13 & dia.mes != 14) %>%
  summarize(sum(atraso.real.de.ideal.min, na.rm = T))


((temp/60)*10)/24
