##' @author Andres Salas
##' @description
##' Codigo para practica en clase de graficar distribuciones teóricas.

# Configuracion inicial -----------------------------------------------------------------------
if (!require("rstudioapi")) install.packages("rstudioapi")
if (rstudioapi::isAvailable() && nzchar(rstudioapi::getActiveDocumentContext()$path)) {
  setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
}

if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, EnvStats, ggpubr, quantmod, TTR, ggcorrplot, xts)
options(scipen = 999)

# Distribuciones discretas --------------------------------------------------------------------
precios <- data.frame(precio = seq(from = 450, to = 500, by = 10),
                      prob = c(0.1, 0.15, 0.25, 0.3, 0.1, 0.1))
View(precios)

# Grafico simple (No recomendado)
grafico_masa <- ggplot2::ggplot(data = precios, mapping = aes(x = precio, y = prob)) +
  ggplot2::geom_col()
grafico_masa

# Mejoremos el grafico
grafico_masa <- ggplot2::ggplot(data = precios, mapping = aes(x = precio, y = prob)) +
  ggplot2::geom_col(color = "steelblue3", fill = "steelblue3", width = 0.4) +
  ggplot2::labs(x = "Precio", y = "Probabilidad", title = "Distribución de probabilidad MSFT")
grafico_masa

# Unas mejoras finales
grafico_masa <- ggplot2::ggplot(data = precios, mapping = aes(x = precio, y = prob)) +
  ggplot2::geom_col(color = "steelblue3", fill = "steelblue3", width = 0.4) +
  theme_light() +
  ggplot2::labs(x = "Precio", y = "Probabilidad", title = "Distribución de probabilidad MSFT") + 
  theme(axis.title = element_text(size = 9),
        axis.text = element_text(size = 9),
        plot.title = element_text(size = 11),
        panel.grid = element_blank())
grafico_masa

# Grafico de probabilidad acumulada
grafico_prob <- ggplot2::ggplot(data = precios, mapping = aes(x = precio, y = cumsum(prob))) +
  ggplot2::geom_step(color = "steelblue3", lwd = 1) +
  theme_light() +
  ggplot2::labs(x = "Precio", y = "Probabilidad", title = "Probabilidad acumulada MSFT") + 
  theme(axis.title = element_text(size = 9),
        axis.text = element_text(size = 9),
        plot.title = element_text(size = 11),
        panel.grid = element_blank())
grafico_prob

ggpubr::ggarrange(ncol = 2, plotlist = list(grafico_masa, grafico_prob)) %>% 
  ggpubr::annotate_figure(top = "Distribucion discreta")

# Distribución triangular ---------------------------------------------------------------------

minimo <- 460
moda <- 495
maximo <- 510

densidad_tri <- ggplot(data = data.frame(x = c(minimo, maximo)), mapping = aes(x)) +
  stat_function(fun = EnvStats::dtri, 
                n=1001, 
                args = list(min = minimo, max = maximo, mode = moda), 
                color = "steelblue3",
                lwd = 1) +
  theme_light() +
  labs(y = "Densidad", x = "Precio", title = "Densidad") +
  theme(axis.title = element_text(size = 9),
        axis.text = element_text(size = 9),
        plot.title = element_text(size = 11),
        panel.grid = element_blank())
densidad_tri

probabilidad_tri <- ggplot(data = data.frame(x = c(minimo, maximo)), mapping = aes(x)) +
  stat_function(fun = EnvStats::ptri, 
                n=1001, 
                args = list(min = minimo, max = maximo, mode = moda), 
                color = "steelblue3",
                lwd = 1) +
  theme_light() +
  labs(y = "Probabilidad", x = "Precio", title = "Probabilidad acumulada") +
  theme(axis.title = element_text(size = 9),
        axis.text = element_text(size = 9),
        plot.title = element_text(size = 11),
        panel.grid = element_blank())
probabilidad_tri

ggpubr::ggarrange(ncol = 2, plotlist = list(densidad_tri, probabilidad_tri)) %>% 
  ggpubr::annotate_figure(top = "Distribucion triangular")

# Distribución normal -------------------------------------------------------------------------

media <- 485
desv.estandar <- 17.5
rango_min <- media - 4*desv.estandar
rango_max <- media + 4*desv.estandar

densidad_norm <- ggplot(data = data.frame(x = c(rango_min, rango_max)), mapping = aes(x)) +
  stat_function(fun = dnorm,
                n=1001,
                args = list(mean = media, sd = desv.estandar),
                color = "steelblue3",
                lwd = 1) +
  theme_light() +
  labs(y = "Densidad", x = "Precio", title = "Densidad") +
  theme(axis.title = element_text(size = 9),
        axis.text = element_text(size = 9),
        plot.title = element_text(size = 11),
        panel.grid = element_blank())
densidad_norm

probabilidad_norm <- ggplot(data = data.frame(x = c(rango_min, rango_max)), mapping = aes(x)) +
  stat_function(fun = pnorm, 
                n=1001, 
                args = list(mean = media, sd = desv.estandar), 
                color = "steelblue3",
                lwd = 1) +
  theme_light() +
  labs(y = "Probabilidad", x = "Precio", title = "Probabilidad acumulada") +
  theme(axis.title = element_text(size = 9),
        axis.text = element_text(size = 9),
        plot.title = element_text(size = 11),
        panel.grid = element_blank())
probabilidad_norm

ggpubr::ggarrange(ncol = 2, plotlist = list(densidad_norm, probabilidad_norm)) %>% 
  ggpubr::annotate_figure(top = "Distribucion normal")

# Calculo de percentiles teoricos -------------------------------------------------------------

# Al conocer la distribucion, es posible calcular sus percentiles teoricos

## Normal ----

cuartil <- c(0.25, 0.5, 0.75)
valor_cuartil <- qnorm(p = cuartil, mean = media, sd = desv.estandar)
cuartil_normal <- data.frame(cuartil, valor_cuartil)
View(cuartil_normal)

percentil <- seq(from = 0.05, to = 0.95, by = 0.05)
valor_percentil <- qnorm(p = percentil, mean = media, sd = desv.estandar)
percentil_normal <- data.frame(percentil, valor_percentil)
View(percentil_normal)

## Triangular ----

tri_cuartil <- EnvStats::qtri(p = cuartil, min = minimo, max = maximo, mode = moda)
cuartil_tri <- data.frame(cuartil, tri_cuartil)
View(cuartil_tri)

tri_percentil <- EnvStats::qtri(p = percentil, min = minimo, max = maximo, mode = moda)
percentil_tri <- data.frame(percentil, tri_percentil)
View(percentil_tri)

# Calculo de medidas de relacion ----

## Descarga de los precios ----

# Si queremos descargar una base de datos con los tickers de acciones que se transan de diferentes mercados

symbols <- stockSymbols(exchange = c("NASDAQ", "NYSE"))
View(symbols)

acciones <- c("NVDA", "AAPL", "AMZN", "TSLA", "PG", "LLY", "NFLX", "ABT", "MSFT", "INTC", "BBVA", "CRSR", "DB", "DUOL", "HOG") 

precio_diario <- quantmod::getSymbols(acciones, src = "yahoo",
                               from = "2018-12-31",
                               #to = "2026-08-04",
                               periodicity = "daily",
                               auto.assign = TRUE,
                               warnings = FALSE) %>% 
  purrr::map(~quantmod::Ad(get(.))) %>% # Extrae solo el precio ajustado de cada una de las acciones
  purrr::reduce(merge.xts) %>% # Fusiona los diferentes items de la lista creada por map usando las fechas como indices
  `colnames<-`(acciones) # Cambia los nombres de las columnas a los simbolos

precio <- xts::to.monthly(x = precio_diario, 
                          indexAt = "lastof",
                          OHLC = FALSE)

View(precio)

## Calculo de los retornos ----

# NOTA: TTR::ROC(x, type="continuous", na.pad=F) tiene un bug real sobre series
# convertidas con to.monthly() - no solo calcula mal la primera fila, desalinea
# toda la serie (ver Distribuciones_correlaciones_notebook.Rmd, Parte 5, para el
# diagnostico completo). Se calcula el retorno logaritmico directamente en su lugar.
retornos <- diff(log(precio)) %>%
  na.omit() %>%
  `colnames<-`(acciones)

View(retornos)

## Calculo de la matriz de covarianzas ----
varianzas <- var(retornos)
varianzas

## Coeficiente de correlacion ----
correlaciones <- cor(retornos)
correlaciones

## Grafico de correlaciones ----
ggcorrplot::ggcorrplot(correlaciones, 
                       lab = T, 
                       type = "lower", 
                       lab_size = 3,
                       tl.cex = 7,
                       title = "Matriz de correlaciones")

# Si queremos ver la correlaciones no significativas 

p_mat <- cor_pmat(retornos)

ggcorrplot::ggcorrplot(correlaciones, 
                       lab = T, 
                       type = "lower", 
                       lab_size = 3,
                       tl.cex = 7,
                       title = "Matriz de correlaciones",
                       p.mat = p_mat,
                       sig.level = 0.05)

