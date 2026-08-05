# Simulaciones.R — Guía de repaso

Explicación enfocada en **qué simula cada bloque, qué fórmula aplica, y por qué esa
distribución/decisión tiene sentido en teoría** — no en sintaxis de R. Pensada para
repasar junto al script (`Simulaciones.R`), no como sustituto de leerlo.

---

## Idea general del script

El script sube en complejidad en 4 escalones:

1. Simular una distribución simple y comparar el histograma contra la fórmula teórica.
2. Combinar dos variables inciertas multiplicándolas (café).
3. Monte Carlo de un **resultado único** (VPN de un proyecto).
4. Monte Carlo de **trayectorias completas en el tiempo** (crecimiento de una inversión).

El hilo conductor: cada simulación es solo generar muchos números aleatorios de una
distribución elegida; lo interesante en cada etapa es *qué distribución elegir y por
qué*, y *qué se hace con esos números una vez generados*.

---

## Parte 1 — Comparación distribución teórica vs. simulada

**Qué hace**: para Uniforme, Binomial, Triangular y Normal, genera 100,000 (10,000 en
la normal) valores aleatorios y compara el histograma resultante contra la curva de
densidad teórica exacta.

**Por qué (teoría)**: ilustra la **Ley de los Grandes Números** — a mayor tamaño de
muestra, la distribución empírica converge a la teórica. Es la base de confianza para
todo lo que sigue: si más adelante vas a simular 10,000 escenarios de costo asumiendo
una triangular, necesitas creer que "simular muchas veces" reproduce de verdad esa
distribución.

**Lo único que realmente hace estadística** en cada bloque (el resto es graficar):
- La llamada `r___()` que genera la muestra: `runif`, `rbinom`, `rtri`, `rnorm`.
- `psych::describe()` y `quantile()` — resumen numérico para verificar que
  media/varianza/forma coinciden con lo esperado.

**Por qué cada distribución, en teoría**:

| Distribución | Cuándo se usa | Parámetros que pide |
|---|---|---|
| Uniforme | Máxima incertidumbre dentro de un rango, sin razón para preferir ningún valor | mínimo, máximo |
| Binomial | Conteo de éxitos en `n` ensayos independientes con probabilidad `p` fija | tamaño `n`, probabilidad `p` |
| Triangular | No hay histórico, solo el juicio de un experto: mínimo, más probable, máximo (**estimación PERT / de tres puntos**, típica en gestión de proyectos) | mínimo, moda, máximo |
| Normal | La variable resulta de sumar muchas causas pequeñas independientes (Teorema del Límite Central) | media, desviación |

**Qué cambiaría**: el bloque se repite casi idéntico 4 veces — en producción valdría la
pena una función `comparar_distribucion(...)`. Para clase, dejarlo explícito así tiene
sentido pedagógico (ver cada paso), no lo tocaría.

---

## Parte 2 — Ejemplo del café

**Qué hace**: simula dos variables inciertas *independientes* — millones de
consumidores y tazas por persona — y las **multiplica** para obtener el total de tazas
consumidas.

```
tasas_total = personas × tasas
```

**Por qué es el paso conceptual clave**: es la primera vez que el código **combina**
incertidumbres en vez de solo mostrarlas por separado. Si `Y = f(X1, X2)` y ambas
`X1, X2` son inciertas, la incertidumbre de `Y` no se calcula con álgebra simple (salvo
casos triviales) — se **propaga** simulando miles de combinaciones plausibles de `X1`
y `X2` a la vez, y viendo qué forma toma `Y`. Esa es la definición operativa de un
Monte Carlo.

**Supuesto implícito (y cuestionable)**: al simular `personas` y `tasas` por separado y
multiplicarlas directamente, el código asume que son **independientes** entre sí — un
valor alto de "personas" no está correlacionado con "tasas". En la realidad podría no
ser cierto, pero para este ejercicio la independencia es una simplificación razonable.
Modelar correlación exigiría copulas o una matriz de correlación explícita.

---

## Parte 3 — VPN de un proyecto (la parte con más teoría financiera)

**Qué hace**: simula 10,000 escenarios de precio, costo y ventas; calcula la utilidad y
el VPN en cada uno; resume qué tan probable es que el proyecto sea rentable.

**Las distribuciones elegidas, y por qué importan**:

- **Precio → Normal**: razonable si el precio fluctúa por factores de mercado
  simétricos alrededor de un valor esperado, sin sesgo fuerte hacia arriba o abajo.
- **Costo → Triangular**: misma lógica PERT de la Parte 1 — un costo unitario suele
  venir de una estimación de ingeniería/compras (peor caso / esperado / mejor caso), no
  de un histórico con media/varianza conocidas.
- **Ventas → Lognormal**: la elección más importante. Los volúmenes de venta **no
  pueden ser negativos** y suelen tener cola larga a la derecha (es más común vender
  "mucho más de lo esperado" en un buen escenario que tener ventas negativas). Una
  normal permitiría valores negativos absurdos; la lognormal los excluye por
  construcción. Es la elección estándar para precios de activos, ingresos, tamaños de
  mercado.

**El motor del cálculo**:

```
utilidad = (precio − costo) × ventas
VPN      = utilidad / (1 + tasa)^tiempo − inversión_inicial
```

- Dividir por `(1+tasa)^tiempo` trae la utilidad futura a **valor presente** — un dólar
  futuro vale menos que uno hoy, descontado a la tasa de costo de oportunidad (10% EA).
  Esto asume, de forma simplificada, que *toda* la utilidad llega de golpe al final del
  año 2 (un solo flujo, no flujos anuales descontados por separado) — razonable para un
  ejercicio, pero en un caso real normalmente se descuenta flujo por año.
- `mean(vpn > 0) * 100` es el truco central: en vez de una fórmula analítica de
  probabilidad, cuenta directamente qué fracción de las 10,000 simulaciones dio VPN
  positivo. Esto reemplaza al VPN de "un solo número" del enfoque tradicional de
  finanzas corporativas — en vez de una respuesta binaria ("el VPN es $X, luz verde"),
  obtienes una **distribución de resultados posibles** y puedes preguntar "¿qué tan
  probable es fracasar?", no solo "¿el caso esperado es positivo?".

**Bug menor de documentación**: el comentario dice "media 150" pero el código usa
`mean = 200` para el precio — el número que manda es el del código (200); vale la pena
corregir el comentario para que no confunda a quien lea el script después.

---

## Parte 4 — Monte Carlo de trayectorias (crecimiento de una inversión)

**Qué hace distinto de la Parte 3**: en el VPN se simulan 10,000 escenarios de un
**resultado final único**. Aquí se simulan 51 **trayectorias completas en el tiempo**
(mes a mes, 10 años) — ahora no solo importa el destino final, sino el camino
recorrido (qué tan profundo puede caer antes de recuperarse).

**La decisión teórica clave**: usar `rnorm(mean = media_portfolio, sd = sd_portfolio)`,
con media y desviación **calculadas de retornos históricos reales**, para simular
retornos futuros. Es un enfoque **paramétrico**: asume que los retornos futuros siguen
una normal con los mismos dos parámetros del pasado.

⚠️ Simplificación fuerte: los retornos financieros reales suelen tener colas más
pesadas que una normal (eventos extremos más frecuentes de lo que predice) y pueden
estar autocorrelacionados en el tiempo (rachas) — cosas que este modelo ignora. Una
alternativa más robusta es el **bootstrap histórico** (remuestrear los retornos reales
observados, con reemplazo, en vez de asumir normalidad) — así está construido, de
hecho, el motor de Monte Carlo que ya se usa en el proyecto QUANT para las estrategias
de trading.

**Crecimiento compuesto**:

```
capital_acumulado = (1 + r₁)(1 + r₂) ⋯ (1 + r_t)
```

El capital no crece sumando retornos, sino **multiplicando** factores de crecimiento
uno tras otro (interés compuesto). El script lo calcula 3 veces con métodos distintos
(`accumulate` con función custom, `accumulate` con `` `*` ``, `cumprod`) solo para
enseñar que dan lo mismo — en la práctica basta con `cumprod`.

**Por qué 51 simulaciones y qué significa el resultado**: las 51 corridas usan la
*misma* media/desviación pero retornos aleatorios distintos cada mes, así que
divergen entre sí solo por azar. El "gráfico de abanico" muestra visualmente el rango
de resultados posibles, y el resumen de percentiles es conceptualmente un
**Value-at-Risk** informal: en vez de calcular analíticamente "¿cuál es el peor 5% de
los casos?", se lee directamente de la distribución simulada de capitales finales.

**Qué cambiaría**: 51 simulaciones es un número bajo para un Monte Carlo serio
(usualmente 1,000-10,000+ para que los percentiles de cola —como el 0.5%/99.5% que
calcula— sean estables). Con solo 51 corridas, esos percentiles extremos son poco
confiables estadísticamente. Si el objetivo es solo ilustrar el concepto del abanico,
51 está bien (además así el gráfico no se satura visualmente); para un análisis de
riesgo real, subiría el número de simulaciones a costa de graficar solo
percentiles/bandas en vez de cada trayectoria individual.

---

## Resumen de fórmulas clave

| Concepto | Fórmula |
|---|---|
| VPN | `utilidad / (1+tasa)^tiempo − inversión_inicial` |
| Probabilidad de éxito | `mean(VPN > 0) × 100` |
| Crecimiento compuesto | `(1+r₁)(1+r₂)⋯(1+r_t)` |
| CAGR | `(crecimiento_total^(1/años) − 1) × 100` |
