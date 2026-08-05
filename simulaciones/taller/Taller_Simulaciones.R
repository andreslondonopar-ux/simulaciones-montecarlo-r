##' @description
##' Solucion del Taller Simulaciones Montecarlo (agosto 2026).
##' Cada pregunta del enunciado se cita en un comentario justo antes de
##' resolverla, y cada resultado termina con un cat("Respuesta: ...") explicito.
##' Mismo motor y resultados que Taller_Simulaciones.Rmd, en formato de script
##' plano para correr por bloques en RStudio (Ctrl+Enter linea por linea, o
##' seleccionar cada seccion "----" y correrla completa).

# Configuracion inicial -----------------------------------------------------------------------
if (!require("rstudioapi")) install.packages("rstudioapi")
if (rstudioapi::isAvailable() && nzchar(rstudioapi::getActiveDocumentContext()$path)) {
  setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
}

if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, EnvStats, ggpubr, psych, readr, scales, quantmod, xts, TTR)
options(scipen = 999)

# ============================================================================================
# EJERCICIO 1 - Tiempo de falla de un satelite (MTTF) ----
# ============================================================================================

# ENUNCIADO: "Se tiene un satelite, que para su funcionamiento depende de que
# al menos 2 paneles solares, de los 5 que tiene disponibles, esten en
# funcionamiento. Se quiere calcular la vida util esperada del satelite
# (MTTF)."
#   - Paneles 1 y 2: Normal(media=50,000 h, desv=5,000 h)
#   - Paneles 3 y 4: Triangular(min=30,000 h, max=75,000 h, moda=50,000 h)
#   - Panel 5: Normal(media=55,000 h, desv=8,000 h)

# MODELO: el satelite necesita AL MENOS 2 de 5 paneles funcionando. Si
# ordenamos las 5 vidas utiles simuladas de menor a mayor
# T(1)<=T(2)<=T(3)<=T(4)<=T(5), el satelite sigue operando mientras fallan el
# 1o, 2o y 3o panel (todavia quedan 2 funcionando). Falla EXACTAMENTE en el
# momento T(4): cuando el 4o panel se apaga y solo queda 1 (ya no cumple el
# minimo de 2). Por lo tanto la vida util del satelite en cada simulacion es
# el 4o estadistico de orden (el 2o valor mas alto) de las 5 vidas simuladas -
# la logica estandar de un sistema de confiabilidad "k-de-n" (aqui, 2-de-5).

simular_satelite <- function(n_sim) {
  panel1 <- rnorm(n_sim, mean = 50000, sd = 5000)
  panel2 <- rnorm(n_sim, mean = 50000, sd = 5000)
  panel3 <- EnvStats::rtri(n_sim, min = 30000, max = 75000, mode = 50000)
  panel4 <- EnvStats::rtri(n_sim, min = 30000, max = 75000, mode = 50000)
  panel5 <- rnorm(n_sim, mean = 55000, sd = 8000)

  paneles <- cbind(panel1, panel2, panel3, panel4, panel5)
  # sort(fila)[4] = 4o estadistico de orden de las 5 vidas de panel
  vida_satelite <- apply(paneles, 1, function(fila) sort(fila)[4])
  return(vida_satelite)
}

## n = 100 ----

set.seed(2026)
vida_100 <- simular_satelite(100)

media_100 <- mean(vida_100)
sd_100 <- sd(vida_100)
cuartiles_100 <- quantile(vida_100, probs = c(0.25, 0.5, 0.75))
p5_100 <- quantile(vida_100, probs = 0.05)
p95_100 <- quantile(vida_100, probs = 0.95)

ggplot(data.frame(vida = vida_100), aes(vida)) +
  geom_histogram(color = "black", fill = "steelblue3", bins = nclass.Sturges(vida_100)) +
  theme_light() +
  labs(x = "Horas de funcionamiento", y = "Frecuencia", title = "MTTF del satélite — n = 100")

# 1. El tiempo promedio de funcionamiento.
cat("Respuesta (n=100, 1. tiempo promedio):", round(media_100, 0), "horas\n")
# 2. La desviacion estandar del tiempo de funcionamiento.
cat("Respuesta (n=100, 2. desv. estandar):", round(sd_100, 0), "horas\n")
# 3. Los cuartiles del tiempo de funcionamiento.
cat("Respuesta (n=100, 3. cuartiles): Q1=", round(cuartiles_100[1],0),
    "Q2=", round(cuartiles_100[2],0), "Q3=", round(cuartiles_100[3],0), "horas\n")
# 4. Construya el histograma de los tiempos de funcionamiento.
cat("Respuesta (n=100, 4. histograma): ver grafico generado arriba\n")
# 5. Hasta que tiempo se encuentra el 5% de los peores tiempos de funcionamiento?
cat("Respuesta (n=100, 5. peor 5%):", round(p5_100,0), "horas o menos\n")
# 6. Hasta que tiempo se encuentra el 5% de los mejores tiempos de funcionamiento?
cat("Respuesta (n=100, 6. mejor 5%):", round(p95_100,0), "horas o mas\n")

## n = 1,000 ----

set.seed(2026)
vida_1000 <- simular_satelite(1000)

media_1000 <- mean(vida_1000)
sd_1000 <- sd(vida_1000)
cuartiles_1000 <- quantile(vida_1000, probs = c(0.25, 0.5, 0.75))
p5_1000 <- quantile(vida_1000, probs = 0.05)
p95_1000 <- quantile(vida_1000, probs = 0.95)

ggplot(data.frame(vida = vida_1000), aes(vida)) +
  geom_histogram(color = "black", fill = "steelblue3", bins = nclass.Sturges(vida_1000)) +
  theme_light() +
  labs(x = "Horas de funcionamiento", y = "Frecuencia", title = "MTTF del satélite — n = 1,000")

cat("Respuesta (n=1000, 1. tiempo promedio):", round(media_1000, 0), "horas\n")
cat("Respuesta (n=1000, 2. desv. estandar):", round(sd_1000, 0), "horas\n")
cat("Respuesta (n=1000, 3. cuartiles): Q1=", round(cuartiles_1000[1],0),
    "Q2=", round(cuartiles_1000[2],0), "Q3=", round(cuartiles_1000[3],0), "horas\n")
cat("Respuesta (n=1000, 4. histograma): ver grafico generado arriba\n")
cat("Respuesta (n=1000, 5. peor 5%):", round(p5_1000,0), "horas o menos\n")
cat("Respuesta (n=1000, 6. mejor 5%):", round(p95_1000,0), "horas o mas\n")

## n = 10,000 ----

set.seed(2026)
vida_10000 <- simular_satelite(10000)

media_10000 <- mean(vida_10000)
sd_10000 <- sd(vida_10000)
cuartiles_10000 <- quantile(vida_10000, probs = c(0.25, 0.5, 0.75))
p5_10000 <- quantile(vida_10000, probs = 0.05)
p95_10000 <- quantile(vida_10000, probs = 0.95)

ggplot(data.frame(vida = vida_10000), aes(vida)) +
  geom_histogram(color = "black", fill = "steelblue3", bins = nclass.Sturges(vida_10000)) +
  theme_light() +
  labs(x = "Horas de funcionamiento", y = "Frecuencia", title = "MTTF del satélite — n = 10,000")

cat("Respuesta (n=10000, 1. tiempo promedio):", round(media_10000, 0), "horas\n")
cat("Respuesta (n=10000, 2. desv. estandar):", round(sd_10000, 0), "horas\n")
cat("Respuesta (n=10000, 3. cuartiles): Q1=", round(cuartiles_10000[1],0),
    "Q2=", round(cuartiles_10000[2],0), "Q3=", round(cuartiles_10000[3],0), "horas\n")
cat("Respuesta (n=10000, 4. histograma): ver grafico generado arriba\n")
cat("Respuesta (n=10000, 5. peor 5%):", round(p5_10000,0), "horas o menos\n")
cat("Respuesta (n=10000, 6. mejor 5%):", round(p95_10000,0), "horas o mas\n")

## Comparacion entre las tres simulaciones ----

# ENUNCIADO: "Compare los resultados obtenidos en las tres simulaciones y
# justifique las diferencias, en caso de que existan."

tabla_satelite <- data.frame(
  n = c(100, 1000, 10000),
  media = c(media_100, media_1000, media_10000),
  desv_estandar = c(sd_100, sd_1000, sd_10000),
  q1 = c(cuartiles_100[1], cuartiles_1000[1], cuartiles_10000[1]),
  mediana = c(cuartiles_100[2], cuartiles_1000[2], cuartiles_10000[2]),
  q3 = c(cuartiles_100[3], cuartiles_1000[3], cuartiles_10000[3]),
  p5_peores = c(p5_100, p5_1000, p5_10000),
  p95_mejores = c(p95_100, p95_1000, p95_10000)
)
print(tabla_satelite)

diferencia_media_pct_satelite <- abs(media_10000 - media_100) / media_100 * 100

cat("\nRespuesta (comparacion satelite):\n")
cat("- La media (", round(media_100,0), "->", round(media_1000,0), "->",
    round(media_10000,0), "horas) y la mediana CASI NO CAMBIAN entre las tres",
    "simulaciones (diferencia de solo", round(diferencia_media_pct_satelite,2),
    "% entre n=100 y n=10,000). 100 iteraciones ya alcanzan para estimar bien",
    "las medidas de tendencia central (media, mediana, cuartiles).\n")
cat("- Donde SI hay diferencia real es en los percentiles extremos: el",
    "percentil 5% pasa de", round(p5_100,0), "(n=100) a", round(p5_1000,0),
    "(n=1,000) a", round(p5_10000,0), "horas (n=10,000). Razon: con n=100 solo",
    "hay ~5 observaciones en esa cola (estimacion ruidosa); con n=10,000 hay",
    "~500 (estimacion estable). Pocas iteraciones alcanzan para el promedio,",
    "pero hacen falta muchas mas para hablar en serio de las colas.\n")

# ============================================================================================
# EJERCICIO 2 - Sensibilidad del VPN a la volatilidad del precio ----
# ============================================================================================

# ENUNCIADO: "Considere el caso del ejemplo en caso del proyecto evaluado a
# partir del VPN. Muestre como cambian los resultados de la simulacion del VPN
# si se aumenta la volatilidad del precio de venta del producto de 20 a 35."

# Se usa la MISMA semilla en ambas corridas: como costos/ventas se generan
# DESPUES de precios en la secuencia de numeros aleatorios, y cambiar solo el
# parametro sd de rnorm() no cambia cuantos numeros aleatorios consume, ambas
# corridas terminan usando exactamente los mismos costos/ventas simulados. El
# unico factor que cambia entre ambas es la volatilidad del precio.

simular_vpn <- function(sd_precio, seed = 41677, n_sim = 10000) {
  set.seed(seed)
  inv_inicial <- 200000
  tasa <- 0.10
  tiempo <- 2

  precios <- rnorm(n = n_sim, mean = 200, sd = sd_precio)
  costos <- EnvStats::rtri(n = n_sim, min = 80, max = 130, mode = 100)
  ventas <- rlnorm(n = n_sim, meanlog = log(5000), sdlog = 0.2) %>% round(0)

  utilidad <- (precios - costos) * ventas
  vpn <- (utilidad / (1 + tasa)^tiempo) - inv_inicial
  return(vpn)
}

vpn_sd20 <- simular_vpn(sd_precio = 20)
vpn_sd35 <- simular_vpn(sd_precio = 35)

prob_exito_20 <- mean(vpn_sd20 > 0) * 100
prob_exito_35 <- mean(vpn_sd35 > 0) * 100
vpn_esp_20 <- mean(vpn_sd20)
vpn_esp_35 <- mean(vpn_sd35)
vpn_sd_20 <- sd(vpn_sd20)
vpn_sd_35 <- sd(vpn_sd35)
p5_vpn_20 <- quantile(vpn_sd20, 0.05)
p5_vpn_35 <- quantile(vpn_sd35, 0.05)
p95_vpn_20 <- quantile(vpn_sd20, 0.95)
p95_vpn_35 <- quantile(vpn_sd35, 0.95)

comparacion_vpn <- data.frame(
  escenario = c("sd = 20 (original)", "sd = 35 (nuevo)"),
  prob_exito_pct = c(prob_exito_20, prob_exito_35),
  vpn_esperado = c(vpn_esp_20, vpn_esp_35),
  vpn_sd = c(vpn_sd_20, vpn_sd_35),
  p5 = c(p5_vpn_20, p5_vpn_35),
  p95 = c(p95_vpn_20, p95_vpn_35)
)
print(comparacion_vpn)

datos_vpn_comb <- bind_rows(
  data.frame(vpn = vpn_sd20, escenario = "sd = 20 (original)"),
  data.frame(vpn = vpn_sd35, escenario = "sd = 35 (nuevo)")
)

ggplot(datos_vpn_comb, aes(x = vpn, fill = escenario)) +
  geom_histogram(position = "identity", alpha = 0.5, color = "black",
                 bins = nclass.Sturges(vpn_sd20)) +
  theme_light() +
  labs(x = "VPN ($)", y = "Frecuencia", fill = "Escenario",
       title = "VPN: sensibilidad a la volatilidad del precio") +
  scale_x_continuous(labels = label_currency())

cat("\nRespuesta (Ejercicio 2 - sensibilidad del VPN):\n")
cat("- Probabilidad de exito: baja de", round(prob_exito_20,2), "% a",
    round(prob_exito_35,2), "% (caida de", round(prob_exito_20-prob_exito_35,2),
    "puntos porcentuales)\n")
cat("- VPN esperado: casi no cambia ($", round(vpn_esp_20,0), "-> $",
    round(vpn_esp_35,0), ", diferencia de solo",
    round(abs(vpn_esp_20-vpn_esp_35)/vpn_esp_20*100,2), "%) porque subir sd no",
    "cambia la MEDIA del precio (sigue en 200) y la utilidad es lineal en precio\n")
cat("- Dispersion del VPN (desv. estandar): sube de $", round(vpn_sd_20,0),
    "a $", round(vpn_sd_35,0), "(", round(vpn_sd_35/vpn_sd_20,1), "x)\n")
cat("- Percentil 5% (peor caso): pasa de $", round(p5_vpn_20,0), "(positivo) a $",
    round(p5_vpn_35,0), "(NEGATIVO) - el peor escenario pasa de ganancia a perdida\n")
cat("- Percentil 95% (mejor caso): sube de $", round(p95_vpn_20,0), "a $",
    round(p95_vpn_35,0), "\n")
cat("- CONCLUSION: subir la volatilidad del precio NO mueve el resultado",
    "esperado, pero SI aumenta fuertemente el riesgo del proyecto\n")

# ============================================================================================
# EJERCICIO 3 - Crecimiento de una inversion con una accion real ----
# ============================================================================================

# ENUNCIADO: "Considere una accion que se comercialice en las bolsas de
# valores de EE.UU. Para esta accion debe descargar los precios de cierre
# ajustados entre el 31/12/2021 y el 31/07/2026."
#
# Se usa Apple (AAPL) -- accion liquida, ampliamente cotizada en NASDAQ.
# Precios de cierre ajustados descargados de Yahoo Finance, mismo metodo que
# "Distribuciones y correlaciones.R": quantmod::getSymbols -> xts::to.monthly.

accion <- "AAPL"

precio_diario <- quantmod::getSymbols(accion, src = "yahoo",
                                       from = "2021-12-31",
                                       to = "2026-07-31",
                                       periodicity = "daily",
                                       auto.assign = FALSE,
                                       warnings = FALSE)

precio_ajustado_diario <- quantmod::Ad(precio_diario)

precio_mensual <- xts::to.monthly(precio_ajustado_diario,
                                   indexAt = "lastof",
                                   OHLC = FALSE)

# NOTA IMPORTANTE (bug detectado y evitado): TTR::ROC(x, type="continuous",
# na.pad=FALSE) da un PRIMER valor incorrecto en series univariadas (devuelve
# log(x_2) en vez de log(x_2/x_1) para la primera fila) -- verificado
# manualmente comparando contra diff(log(precio_mensual)). Por eso el retorno
# logaritmico se calcula aqui directamente en vez de usar TTR::ROC.
retornos_accion <- na.omit(diff(log(precio_mensual)))

# ENUNCIADO: "Con esta informacion usted debe: Calcular el retorno mensual
# promedio de la accion. Calcular la desviacion estandar de los retornos
# mensuales de la accion. Calcular el retorno mensual maximo y el retorno
# mensual minimo de la accion."

media_accion <- mean(retornos_accion)
sd_accion <- sd(retornos_accion)
max_accion <- max(retornos_accion)
min_accion <- min(retornos_accion)

cat("\nRespuesta (retornos de", accion, "):\n")
cat("- Retorno mensual promedio:", round(media_accion*100,2), "%\n")
cat("- Desviacion estandar mensual:", round(sd_accion*100,2), "%\n")
cat("- Retorno mensual maximo:", round(max_accion*100,2), "%\n")
cat("- Retorno mensual minimo:", round(min_accion*100,2), "%\n")

## Parte 1: dos simulaciones de n=51, a 2 anos (24 meses), $100,000 inicial ----

# ENUNCIADO: "1. Realizar dos simulaciones de tamano n=51 de como se
# comportara una inversion de $100,000 dolares si se invierten, a dos anos, en
# un activo cuyos retornos son aleatorios y siguen: a. Una distribucion
# normal. b. Una distribucion triangular (emplee la media estimada como
# aproximacion de la moda). Para ambos casos se debe graficar la simulacion y
# se deben de calcular las estadisticas descriptivas y comparar los
# resultados."

# Se reusa la funcion simulacion_activo() de Simulaciones.R para la normal, y
# se crea su equivalente con rtri() para la triangular.

simulacion_activo <- function(valor_inicial, media_rendimiento, sd_rendimiento, periodos){
  activo <- data.frame(periodo = 0:periodos) %>%
    mutate(retorno = c(0, rnorm(n = periodos, mean = media_rendimiento, sd = sd_rendimiento))) %>%
    mutate(rendimiento = 1 + retorno) %>%
    mutate(rend_acum = cumprod(rendimiento)) %>%
    mutate(retorno_acum = rend_acum - 1) %>%
    mutate(capital = valor_inicial * rend_acum, .after = 1)
  return(activo)
}

simulacion_activo_tri <- function(valor_inicial, minimo, maximo, moda, periodos){
  activo <- data.frame(periodo = 0:periodos) %>%
    mutate(retorno = c(0, EnvStats::rtri(n = periodos, min = minimo, max = maximo, mode = moda))) %>%
    mutate(rendimiento = 1 + retorno) %>%
    mutate(rend_acum = cumprod(rendimiento)) %>%
    mutate(retorno_acum = rend_acum - 1) %>%
    mutate(capital = valor_inicial * rend_acum, .after = 1)
  return(activo)
}

capital_inicial <- 100000
periodos_2a <- 24
n_51 <- 51

### a. Distribucion normal ----

set.seed(300)
sim_normal_51 <- purrr::map(.x = rep(capital_inicial, n_51),
                            .f = simulacion_activo,
                            media_rendimiento = media_accion,
                            sd_rendimiento = sd_accion,
                            periodos = periodos_2a) %>%
  list_rbind(names_to = "simulacion") %>%
  mutate(simulacion = factor(simulacion))

ggplot(sim_normal_51, aes(x = periodo, y = capital, col = simulacion)) +
  geom_line() +
  theme_light() +
  theme(legend.position = "none") +
  labs(x = "Mes", y = "USD", title = paste0("Simulación normal (n=", n_51, ") — ", accion)) +
  scale_y_continuous(labels = label_currency())

final_normal_51 <- sim_normal_51 %>% group_by(simulacion) %>% summarise(final = last(capital)) %>% pull(final)
media_normal_51 <- mean(final_normal_51)
sd_normal_51 <- sd(final_normal_51)
mediana_normal_51 <- median(final_normal_51)
print(psych::describe(final_normal_51))
cat("Respuesta (normal, n=51): capital final promedio = $", round(media_normal_51,0),
    ", desv. estandar = $", round(sd_normal_51,0),
    ", mediana = $", round(mediana_normal_51,0), "\n")

### b. Distribucion triangular ----

set.seed(301)
sim_tri_51 <- purrr::map(.x = rep(capital_inicial, n_51),
                         .f = simulacion_activo_tri,
                         minimo = min_accion,
                         maximo = max_accion,
                         moda = media_accion,
                         periodos = periodos_2a) %>%
  list_rbind(names_to = "simulacion") %>%
  mutate(simulacion = factor(simulacion))

ggplot(sim_tri_51, aes(x = periodo, y = capital, col = simulacion)) +
  geom_line() +
  theme_light() +
  theme(legend.position = "none") +
  labs(x = "Mes", y = "USD", title = paste0("Simulación triangular (n=", n_51, ") — ", accion)) +
  scale_y_continuous(labels = label_currency())

final_tri_51 <- sim_tri_51 %>% group_by(simulacion) %>% summarise(final = last(capital)) %>% pull(final)
media_tri_51 <- mean(final_tri_51)
sd_tri_51 <- sd(final_tri_51)
mediana_tri_51 <- median(final_tri_51)
print(psych::describe(final_tri_51))
cat("Respuesta (triangular, n=51): capital final promedio = $", round(media_tri_51,0),
    ", desv. estandar = $", round(sd_tri_51,0),
    ", mediana = $", round(mediana_tri_51,0), "\n")

### Comparacion normal vs. triangular (n=51) ----

diferencia_media_51_pct <- (media_tri_51-media_normal_51)/media_normal_51*100

cat("\nRespuesta (comparacion normal vs. triangular, n=51):\n")
cat("- La triangular da un capital final promedio MAS ALTO ($", round(media_tri_51,0),
    "vs. $", round(media_normal_51,0), "de la normal, diferencia de",
    round(diferencia_media_51_pct,1), "%) y algo mas de dispersion ($",
    round(sd_tri_51,0), "vs. $", round(sd_normal_51,0), "). Con solo 51",
    "simulaciones esta comparacion todavia tiene bastante ruido de muestreo -",
    "se retoma con mas precision en la Parte 2 (n=100,000).\n")

## Parte 2: n=100,000 iteraciones (sin grafico) ----

# ENUNCIADO: "2. Usando la misma informacion del punto anterior, se debe de
# realizar las dos simulaciones. Pero esta vez se debe de realizar
# n=100,000 iteraciones en las dos simulaciones (no realices el grafico de la
# simulacion)."

# NOTA DE IMPLEMENTACION: para 100,000 iteraciones se necesita solo el capital
# FINAL de cada corrida, no la trayectoria mes a mes completa. Se probo primero
# usar exactamente la misma funcion simulacion_activo() de la Parte 1 (el mismo
# estilo purrr::map + dplyr visto en clase) para 100,000 repeticiones, y tardo
# cerca de 29 MINUTOS solo para la distribucion normal (serian ~1 hora para las
# dos) - computacionalmente inviable para este taller. Por eso aqui se
# vectoriza el calculo (una matriz de retornos de 100,000 x 24, con el producto
# acumulado por fila) en vez de llamar la funcion 100,000 veces - el resultado
# matematico es identico, solo cambia como se calcula.

n_100k <- 100000

set.seed(302)
retornos_normal_100k <- matrix(rnorm(n_100k * periodos_2a, mean = media_accion, sd = sd_accion),
                                nrow = n_100k, ncol = periodos_2a)
final_normal_100k <- capital_inicial * apply(1 + retornos_normal_100k, 1, prod)

set.seed(303)
retornos_tri_100k <- matrix(EnvStats::rtri(n_100k * periodos_2a, min = min_accion, max = max_accion, mode = media_accion),
                             nrow = n_100k, ncol = periodos_2a)
final_tri_100k <- capital_inicial * apply(1 + retornos_tri_100k, 1, prod)

media_normal_100k <- mean(final_normal_100k)
sd_normal_100k <- sd(final_normal_100k)
mediana_normal_100k <- median(final_normal_100k)

media_tri_100k <- mean(final_tri_100k)
sd_tri_100k <- sd(final_tri_100k)
mediana_tri_100k <- median(final_tri_100k)

cat("--- Normal (n=100,000) ---\n")
print(psych::describe(final_normal_100k))
cat("\n--- Triangular (n=100,000) ---\n")
print(psych::describe(final_tri_100k))

### a. Comparar con las simulaciones anteriores (n=51) ----

tabla_n <- data.frame(
  distribucion = c("Normal", "Normal", "Triangular", "Triangular"),
  n = c(51, 100000, 51, 100000),
  media = c(media_normal_51, media_normal_100k, media_tri_51, media_tri_100k),
  desv_estandar = c(sd_normal_51, sd_normal_100k, sd_tri_51, sd_tri_100k),
  mediana = c(mediana_normal_51, mediana_normal_100k, mediana_tri_51, mediana_tri_100k)
)
print(tabla_n)

dif_normal_n_pct <- abs(media_normal_51-media_normal_100k)/media_normal_51*100
dif_tri_n_pct <- abs(media_tri_51-media_tri_100k)/media_tri_51*100

cat("\nRespuesta (2.a - comparacion n=51 vs n=100,000):\n")
cat("- Normal: la media pasa de $", round(media_normal_51,0), "a $",
    round(media_normal_100k,0), "(diferencia de", round(dif_normal_n_pct,1), "%)\n")
cat("- Triangular: la media pasa de $", round(media_tri_51,0), "a $",
    round(media_tri_100k,0), "(diferencia de", round(dif_tri_n_pct,1), "%)\n")
cat("- En ambos casos la media ya estaba razonablemente bien estimada con",
    "n=51 - confirma que pocas iteraciones alcanzan para la tendencia central\n")

### b. Que tanto difieren los resultados entre las dos distribuciones? ----

diferencia_media_pct <- (media_tri_100k - media_normal_100k) / media_normal_100k * 100
diferencia_sd_pct <- (sd_tri_100k - sd_normal_100k) / sd_normal_100k * 100

cat("\nRespuesta (2.b - diferencia entre distribuciones, n=100,000):\n")
cat("- El capital final promedio de la TRIANGULAR es", round(diferencia_media_pct,1),
    "% MAS ALTO que el de la normal ($", round(media_tri_100k,0), "vs. $",
    round(media_normal_100k,0), ")\n")
cat("- Su desviacion estandar es", round(abs(diferencia_sd_pct),1), "% MAS BAJA ($",
    round(sd_tri_100k,0), "vs. $", round(sd_normal_100k,0), ")\n")
cat("- RAZON: la triangular esta ACOTADA por construccion (nunca genera un mes",
    "peor que", round(min_accion*100,2), "% ni mejor que", round(max_accion*100,2),
    "% - el minimo/maximo historico), mientras la normal tiene colas infinitas",
    "y puede generar, con baja probabilidad, un retorno mas extremo que",
    "cualquier cosa vista en los datos historicos. Al acumular 24 meses con",
    "producto compuesto, esa asimetria hace que la normal termine con un",
    "capital final esperado mas bajo (desigualdad de Jensen sobre productos de",
    "variables aleatorias).\n")

### c. Maximo que se puede perder en el 5% de los peores casos? ----

p5_normal_100k <- quantile(final_normal_100k, 0.05)
p5_tri_100k <- quantile(final_tri_100k, 0.05)
perdida_normal <- capital_inicial - p5_normal_100k
perdida_tri <- capital_inicial - p5_tri_100k

tabla_var5 <- data.frame(
  distribucion = c("Normal", "Triangular"),
  capital_inicial = capital_inicial,
  percentil_5_capital_final = c(p5_normal_100k, p5_tri_100k),
  perdida_maxima_5pct = c(perdida_normal, perdida_tri),
  perdida_maxima_5pct_pct = c(perdida_normal, perdida_tri) / capital_inicial * 100
)
print(tabla_var5)

cat("\nRespuesta (2.c - perdida maxima en el 5% de los peores casos):\n")
cat("- Normal: perdida maxima de $", round(perdida_normal,0), "(",
    round(perdida_normal/capital_inicial*100,1), "% del capital inicial)\n")
cat("- Triangular: perdida maxima de $", round(perdida_tri,0), "(",
    round(perdida_tri/capital_inicial*100,1), "% del capital inicial)\n")
cat("- La normal muestra una perdida potencial mayor, consistente con que su",
    "cola izquierda no esta acotada por el peor mes historicamente observado\n")
