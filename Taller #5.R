#------------------------------------------------------#
#-- Universidad del Valle                            --#
#-- Asignatura: Control de Calidad                   --#
#-- Solución Taller Cartas de Control por Variables  --#
#------------------------------------------------------#

library(qcc)

datos <- read.table("clipboard", header=TRUE, dec=",")

head(datos)
str(datos)

# b) Gráficos de control
# Gráfico de control para α=0.0027 --> Z(α/2)=3 --> k=3

q_xbar <- qcc(datos[,-1], type = "xbar",
              title = "Carta de Control para la Media")

q_xR   <- qcc(datos[,-1], type = "R",
              title = "Carta de Control para el Rango")

# Gráfico de control para α=0.05 --> Z(α/2)=1.96 --> k=1.96
q_xbar <- qcc(datos[,-1], type = "xbar", nsigmas = 1.96,
              title = "Carta de Control para la Media")

q_xR   <- qcc(datos[,-1], type = "R", nsigmas = 1.96,
              title = "Carta de Control para el Rango")

# c) Estimación de parámetros del proceso
# Solo van las muestras en control (iteraciones hasta 0 puntos fuera)

# Carta k=3 
# Fuera de control: muestras 32,33,34,35,36,37 (6 puntos rojos carta X̄)
fuera_k3  <- c(32, 33, 34, 35, 36, 37)
datos_k3  <- datos[-fuera_k3, ]

q_c_k3    <- qcc(datos_k3[,-1], type = "xbar",
                 title = "Parametros k=3")
mu0_k3    <- q_c_k3$center
sigma0_k3 <- q_c_k3$std.dev
cat("k=3    | mu0:", round(mu0_k3,5), "| sigma0:", round(sigma0_k3,5), "\n")

# Carta k=1.96 iteración final 
# intento 1: muestras 12,31,32,33,34,35,36,37 (9 puntos rojos carta X̄)
# intento 2: se agrega muestra 5 
# intento 3: se agrega muestra 28 

fuera_k196  <- c(5, 12, 28, 31, 32, 33, 34, 35, 36, 37)
datos_k196  <- datos[-fuera_k196, ]

q_c_196    <- qcc(datos_k196[,-1], type = "xbar", nsigmas = 1.96,
                  title = "Parametros k=1.96 (iteracion final)")
mu0_196    <- q_c_196$center
sigma0_196 <- q_c_196$std.dev
cat("k=1.96 | mu0:", round(mu0_196,5), "| sigma0:", round(sigma0_196,5), "\n")


# d) Curvas Características de Operación (CO) - ambas en la misma gráfica
n <- 4  # tamaño de subgrupo

# Función beta (probabilidad de no detectar un desplazamiento delta)
beta_co <- function(delta, k, n) {
  pnorm(k - delta * sqrt(n)) - pnorm(-k - delta * sqrt(n))
}

# Rango de desplazamientos a evaluar (en múltiplos de sigma)
delta_seq <- seq(0, 4, by = 0.01)

# Beta para cada carta
beta_k3   <- beta_co(delta_seq, k = 3,    n = n)
beta_k196 <- beta_co(delta_seq, k = 1.96, n = n)

# Graficar las dos curvas juntas
plot(delta_seq, beta_k3,
     type = "l", col = "blue", lwd = 2,
     xlab = expression(delta ~ "(desplazamiento en múltiplos de " * sigma[0] * ")"),
     ylab = expression(beta ~ "(Probabilidad de No Detectar)"),
     main = "Curvas Características de Operación - Carta X\u0305",
     ylim = c(0, 1))

grid(col = "gray85", lty = 3)

lines(delta_seq, beta_k196, col = "red", lwd = 2, lty = 2)

legend("topright",
       legend = c("alpha = 0.0027 (k=3)",
                  "alpha = 0.05 (k=1.96)"),
       col  = c("blue", "red"),
       lty  = c(1, 2),
       lwd  = 2,
       bty  = "n")
# =============================================================================
# TALLER 5 – GRÁFICOS DE CONTROL POR VARIABLES
# Parte 2: Condiciones de Operación del Gráfico
# Valery Rivera Lopez – Diego Fernando Muñoz Portela
# Prof. Ivan Mauricio Bermudez Vera | Ingeniería Industrial – Univalle
# =============================================================================

# -----------------------------------------------------------------------------
# FUNCIONES BASE (template del profesor)
# -----------------------------------------------------------------------------

# Probabilidad de detectar un desplazamiento de delta sigmas en la media
potencia <- function(n, delta, alpha) {
  z      <- -qnorm(alpha / 2)          # z_alpha/2  (valor positivo)
  p      <- 1 - (pnorm(z - delta * sqrt(n)) - pnorm(-z - delta * sqrt(n)))
  return(p)
}

# Límites de control de la carta X-barra
LControl <- function(n, alpha, mu0, sigma0) {
  z      <- -qnorm(alpha / 2)
  LCS    <- mu0 + z * sigma0 / sqrt(n)
  LCI    <- mu0 - z * sigma0 / sqrt(n)
  Limites <- c(LCI, LCS)
  names(Limites) <- c("LCI", "LCS")
  return(Limites)
}

# =============================================================================
# PARÁMETROS DEL PROCESO (obtenidos en ítem c – fase de instalación depurada)
# =============================================================================
# CASO 1 – alpha = 0.0027  (k = 3)
#   31 muestras en control tras eliminar subgrupos 32–37
mu0_1    <- 9.953611
sigma0_1 <- 0.8516857
n        <- 4          # tamaño de subgrupo (común a ambos casos)

# CASO 2 – alpha = 0.05  (k = 1.96)
#   27 muestras en control tras 3 iteraciones (eliminados 5, 12, 28, 31–37)
mu0_2    <- 9.958425
sigma0_2 <- 0.8302513

# Cambio en la media a detectar
cambio   <- 1.4        # 1.4 % de humedad hacia arriba

# Delta expresado en múltiplos de sigma_0 (unidades que exige la fórmula)
delta_1  <- cambio / sigma0_1   # = 1.4 / 0.8517 ≈ 1.6438 sigmas
delta_2  <- cambio / sigma0_2   # = 1.4 / 0.8303 ≈ 1.6861 sigmas

alpha_1  <- 0.0027
alpha_2  <- 0.05

# =============================================================================
# ÍTEM e – Probabilidad de detectar el cambio de 1.4 % en la PRIMERA muestra
# =============================================================================
# P(detección) = potencia = 1 – β  en una sola muestra
p1 <- potencia(n = n, delta = delta_1, alpha = alpha_1)
p2 <- potencia(n = n, delta = delta_2, alpha = alpha_2)

cat("============================================================\n")
cat("  ÍTEM e – Potencia (prob. de detectar en 1.ª muestra)\n")
cat("============================================================\n")
cat(sprintf("  Caso 1 (alpha=0.0027, k=3):   delta=%.4f sigmas  -->  potencia = %.4f  (%.2f %%)\n",
            delta_1, p1, p1 * 100))
cat(sprintf("  Caso 2 (alpha=0.05,   k=1.96): delta=%.4f sigmas  -->  potencia = %.4f  (%.2f %%)\n\n",
            delta_2, p2, p2 * 100))

# =============================================================================
# ÍTEM f – Probabilidad de detección EXACTAMENTE en la 4.ª muestra
# =============================================================================
# Distribución geométrica: P(X = k) = (1–p)^(k–1) * p
# k = 4 → P(X=4) = (1–p)^3 * p

k_muestra <- 4
P_f1 <- (1 - p1)^(k_muestra - 1) * p1
P_f2 <- (1 - p2)^(k_muestra - 1) * p2

cat("============================================================\n")
cat("  ÍTEM f – P(detección exactamente en la 4.ª muestra)\n")
cat("============================================================\n")
cat(sprintf("  Caso 1 (alpha=0.0027): P(X=4) = (1-%.4f)^3 * %.4f = %.4f  (%.2f %%)\n",
            p1, p1, P_f1, P_f1 * 100))
cat(sprintf("  Caso 2 (alpha=0.05):   P(X=4) = (1-%.4f)^3 * %.4f = %.4f  (%.2f %%)\n\n",
            p2, p2, P_f2, P_f2 * 100))

# =============================================================================
# ÍTEM g – ARL₁: muestras esperadas para DETECTAR el cambio
# =============================================================================
# Distribución geométrica: E[X] = 1/p
ARL1_1 <- 1 / p1
ARL1_2 <- 1 / p2

cat("============================================================\n")
cat("  ÍTEM g – ARL₁ (muestras esperadas para detectar cambio)\n")
cat("============================================================\n")
cat(sprintf("  Caso 1 (alpha=0.0027): ARL₁ = 1/%.4f = %.4f muestras\n", p1, ARL1_1))
cat(sprintf("  Caso 2 (alpha=0.05):   ARL₁ = 1/%.4f = %.4f muestras\n\n", p2, ARL1_2))

# =============================================================================
# ÍTEM h – ARL₀: muestras esperadas para emitir una FALSA ALARMA
# =============================================================================
# Cuando el proceso está en control: P(falsa alarma) = alpha
# ARL₀ = 1/alpha
ARL0_1 <- 1 / alpha_1
ARL0_2 <- 1 / alpha_2

cat("============================================================\n")
cat("  ÍTEM h – ARL₀ (muestras esperadas para falsa alarma)\n")
cat("============================================================\n")
cat(sprintf("  Caso 1 (alpha=0.0027): ARL₀ = 1/0.0027 = %.2f muestras\n", ARL0_1))
cat(sprintf("  Caso 2 (alpha=0.05):   ARL₀ = 1/0.05   = %.2f  muestras\n\n", ARL0_2))

# =============================================================================
# RESUMEN COMPARATIVO – TABLA GENERAL
# =============================================================================
cat("============================================================\n")
cat("  RESUMEN – Métricas de operación comparadas\n")
cat("============================================================\n")
cat(sprintf("  %-35s  %10s  %10s\n", "Métrica", "k=3 (0.0027)", "k=1.96 (0.05)"))
cat(sprintf("  %-35s  %10s  %10s\n", "-----------------------------------",
            "----------", "----------"))
cat(sprintf("  %-35s  %10.4f  %10.4f\n", "mu0 estimado",              mu0_1,   mu0_2))
cat(sprintf("  %-35s  %10.4f  %10.4f\n", "sigma0 estimado",           sigma0_1, sigma0_2))
cat(sprintf("  %-35s  %10.4f  %10.4f\n", "delta (sigmas)",            delta_1,  delta_2))
cat(sprintf("  %-35s  %10.4f  %10.4f\n", "e) Potencia (1 muestra)",   p1,       p2))
cat(sprintf("  %-35s  %10.4f  %10.4f\n", "f) P(detección en 4.ª)",    P_f1,     P_f2))
cat(sprintf("  %-35s  %10.4f  %10.4f\n", "g) ARL1 (detectar cambio)", ARL1_1,   ARL1_2))
cat(sprintf("  %-35s  %10.2f  %10.2f\n", "h) ARL0 (falsa alarma)",    ARL0_1,   ARL0_2))

# =============================================================================
# LÍMITES DE CONTROL (verificación con parámetros depurados)
# =============================================================================
cat("\n============================================================\n")
cat("  LÍMITES DE CONTROL (con parámetros depurados del ítem c)\n")
cat("============================================================\n")
lim1 <- LControl(n = n, alpha = alpha_1, mu0 = mu0_1, sigma0 = sigma0_1)
lim2 <- LControl(n = n, alpha = alpha_2, mu0 = mu0_2, sigma0 = sigma0_2)
cat(sprintf("  Caso 1 (k=3):    LCI = %.4f   LC = %.4f   LCS = %.4f\n",
            lim1["LCI"], mu0_1, lim1["LCS"]))
cat(sprintf("  Caso 2 (k=1.96): LCI = %.4f   LC = %.4f   LCS = %.4f\n",
            lim2["LCI"], mu0_2, lim2["LCS"]))

# =============================================================================
# GRÁFICA – Curva de potencia en función del número de muestras acumuladas
# (probabilidad de haber detectado el cambio en ≤ m muestras)
# =============================================================================
m_seq    <- 1:10
P_acum_1 <- 1 - (1 - p1)^m_seq   # P(detectar en ≤ m muestras) | caso 1
P_acum_2 <- 1 - (1 - p2)^m_seq   # idem | caso 2

par(mar = c(5, 5, 4, 2))
plot(m_seq, P_acum_1,
     type  = "b", pch = 16, col = "steelblue", lwd = 2,
     ylim  = c(0, 1),
     xlab  = "Número de muestras (m)",
     ylab  = "P(detectar el cambio en ≤ m muestras)",
     main  = "Probabilidad acumulada de detección\nCambio de 1.4 % en la media",
     las   = 1, xaxt = "n")
axis(1, at = m_seq)
lines(m_seq, P_acum_2,
      type = "b", pch = 17, col = "firebrick", lwd = 2, lty = 2)
abline(h  = c(0.5, 0.9, 0.99), lty = 3, col = "gray60")
legend("bottomright",
       legend = c(expression(alpha == 0.0027 ~ "(k=3)"),
                  expression(alpha == 0.05   ~ "(k=1.96)")),
       col    = c("steelblue", "firebrick"),
       lty    = c(1, 2), pch = c(16, 17), lwd = 2,
       bty    = "n")

# Parámetros
n <- 4
delta_1 <- 1.4 / 0.8517   # 1.6438
delta_2 <- 1.4 / 0.8303   # 1.6862

delta_seq <- seq(0, 4, by = 0.01)
beta_1 <- pnorm(3    - delta_seq * sqrt(n)) - pnorm(-3    - delta_seq * sqrt(n))
beta_2 <- pnorm(1.96 - delta_seq * sqrt(n)) - pnorm(-1.96 - delta_seq * sqrt(n))

# Beta en el punto específico del cambio de 1.4%
beta_punto_1 <- pnorm(3    - delta_1 * sqrt(n)) - pnorm(-3    - delta_1 * sqrt(n))
beta_punto_2 <- pnorm(1.96 - delta_2 * sqrt(n)) - pnorm(-1.96 - delta_2 * sqrt(n))

plot(delta_seq, beta_1,
     type = "l", col = "steelblue", lwd = 2,
     ylim = c(0, 1), las = 1,
     xlab = expression(delta ~ "(desplazamiento en múltiplos de" ~ sigma[0]*")"),
     ylab = expression(beta ~ "(Probabilidad de No Detectar)"),
     main = "Curvas CO con desplazamiento de 1.4% marcado")
lines(delta_seq, beta_2, col = "firebrick", lwd = 2, lty = 2)

# Líneas verticales en el delta del cambio
abline(v = delta_1, lty = 3, col = "steelblue")
abline(v = delta_2, lty = 3, col = "firebrick")

# Puntos sobre las curvas
points(delta_1, beta_punto_1, pch = 16, col = "steelblue", cex = 1.8)
points(delta_2, beta_punto_2, pch = 17, col = "firebrick",  cex = 1.8)

# Etiquetas con beta y potencia
text(delta_1, beta_punto_1 + 0.07,
     labels = paste0("β=", round(beta_punto_1, 4), "\n(potencia=61.32%)"),
     col = "steelblue", cex = 0.85, adj = 0)
text(delta_2, beta_punto_2 + 0.07,
     labels = paste0("β=", round(beta_punto_2, 4), "\n(potencia=92.11%)"),
     col = "firebrick", cex = 0.85, adj = 1)

legend("topright",
       legend = c("α = 0.0027 (k=3)", "α = 0.05 (k=1.96)"),
       col = c("steelblue", "firebrick"),
       lty = c(1, 2), lwd = 2, pch = c(16, 17), bty = "n")

p1 <- 0.6132
p2 <- 0.9211
k  <- 1:10

P_geo_1 <- (1 - p1)^(k - 1) * p1
P_geo_2 <- (1 - p2)^(k - 1) * p2

par(mfrow = c(1, 2))

# Caso k=3
barplot(P_geo_1,
        names.arg = k,
        col = ifelse(k == 4, "steelblue", "lightblue"),
        border = "white",
        main = expression(alpha == 0.0027 ~ "(k=3)"),
        xlab = "Muestra de detección (k)",
        ylab = "P(X = k)",
        ylim = c(0, 0.7),
        las = 1)
text(x = 4 * 1.2 - 0.1, y = P_geo_1[4] + 0.03,
     labels = paste0("P(X=4)\n= ", round(P_geo_1[4], 4)),
     col = "steelblue", cex = 0.85)

# Caso k=1.96
barplot(P_geo_2,
        names.arg = k,
        col = ifelse(k == 4, "firebrick", "#f5a9a9"),
        border = "white",
        main = expression(alpha == 0.05 ~ "(k=1.96)"),
        xlab = "Muestra de detección (k)",
        ylab = "P(X = k)",
        ylim = c(0, 1),
        las = 1)
text(x = 4 * 1.2 - 0.1, y = P_geo_2[4] + 0.04,
     labels = paste0("P(X=4)\n= ", round(P_geo_2[4], 4)),
     col = "firebrick", cex = 0.85)

par(mfrow = c(1, 1))



###############
p1 <- 0.6132
p2 <- 0.9211
m  <- 1:10

P_acum_1 <- 1 - (1 - p1)^m
P_acum_2 <- 1 - (1 - p2)^m

plot(m, P_acum_1,
     type = "b", pch = 16, col = "steelblue", lwd = 2,
     ylim = c(0, 1), las = 1, xaxt = "n",
     xlab = "Número de muestras (m)",
     ylab = "P(detectar en ≤ m muestras)",
     main = "Probabilidad acumulada de detección\nCambio de 1.4% en la media")
axis(1, at = m)
lines(m, P_acum_2,
      type = "b", pch = 17, col = "firebrick", lwd = 2, lty = 2)

# Líneas de referencia
abline(h = c(0.90, 0.95, 0.99), lty = 3, col = "gray60")
text(x = 10.2, y = c(0.90, 0.95, 0.99),
     labels = c("90%", "95%", "99%"),
     col = "gray40", cex = 0.8, adj = 0)

# ARL1 marcado
abline(v = 1/p1, lty = 2, col = "steelblue", lwd = 1)
abline(v = 1/p2, lty = 2, col = "firebrick",  lwd = 1)
text(1/p1, 0.05, labels = paste0("ARL₁=", round(1/p1, 2)),
     col = "steelblue", cex = 0.8, adj = -0.1)
text(1/p2, 0.15, labels = paste0("ARL₁=", round(1/p2, 2)),
     col = "firebrick", cex = 0.8, adj = -0.1)

legend("bottomright",
       legend = c("α = 0.0027 (k=3)", "α = 0.05 (k=1.96)"),
       col = c("steelblue", "firebrick"),
       lty = c(1, 2), pch = c(16, 17), lwd = 2, bty = "n")

# Tabla resumen final
metricas <- data.frame(
  Metrica  = c("Potencia (1 muestra)", "P(detección en k=4)",
               "ARL1 (detectar cambio)", "ARL0 (falsa alarma)"),
  k3       = c("61.32%", "3.55%", "1.63 muestras", "370.37 muestras"),
  k1.96    = c("92.11%", "0.05%", "1.09 muestras", "20.00 muestras")
)

colnames(metricas) <- c("Métrica", "α=0.0027 (k=3)", "α=0.05 (k=1.96)")

# Visualizar como tabla gráfica
library(gridExtra)
grid.table(metricas)