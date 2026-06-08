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


# d) Comparación de límites entre los dos alphas DE ACA PARA ABAJO ESTOY 0 SEGURA


q_xbar_196 <- qcc(datos[,-1], type = "xbar", nsigmas = 1.96,
                  title = "Carta de Control para la Media (k=1.96)")

cat("\n--- Límites Carta X̄ con α=0.0027 (k=3) ---\n")
cat("LCI:", round(q_xbar_3$limits[1], 4), "\n")
cat("LCS:", round(q_xbar_3$limits[2], 4), "\n")

cat("\n--- Límites Carta X̄ con α=0.05 (k=1.96) ---\n")
cat("LCI:", round(q_xbar_196$limits[1], 4), "\n")
cat("LCS:", round(q_xbar_196$limits[2], 4), "\n")


# e) Curvas de Operación (CO) y condiciones de operación
# Desplazamiento: media sube 1.4% sobre mu0

n <- ncol(datos) - 1   # tamaño de subgrupo = 4

# Función potencia para carta X̄
potencia <- function(delta, k, n) {
  1 - (pnorm(k - delta * sqrt(n)) - pnorm(-k - delta * sqrt(n)))
}

# Rango de deltas a graficar
delta_seq <- seq(0, 4, by = 0.1)

beta_3   <- 1 - potencia(delta_seq, k = 3,    n = n)
beta_196 <- 1 - potencia(delta_seq, k = 1.96, n = n)

# Graficar las dos curvas CO juntas
plot(delta_seq, beta_3,
     type = "l", col = "steelblue", lwd = 2,
     xlab = expression(delta ~ "(múltiplos de " * sigma[0] * ")"),
     ylab = expression(beta ~ "(Error Tipo II)"),
     main = "Curvas Características de Operación (CO) - Carta X̄",
     ylim = c(0, 1))
lines(delta_seq, beta_196, col = "firebrick", lwd = 2, lty = 2)
legend("topright",
       legend = c("α=0.0027 (k=3)", "α=0.05 (k=1.96)"),
       col    = c("steelblue", "firebrick"),
       lty    = c(1, 2), lwd = 2)

# --- Condiciones de operación: desplazamiento del 1.4% ---
delta_e <- (mu0 * 0.014) / sigma0

cat("\n=== Condiciones de operación (desplazamiento 1.4% de mu0) ===\n")
cat("Desplazamiento absoluto :", round(mu0 * 0.014, 5), "\n")
cat("delta (en sigmas)       :", round(delta_e, 4), "\n")

# α=0.0027 (k=3)
pot_3  <- potencia(delta_e, k = 3,    n = n)
ARL1_3 <- 1 / pot_3

cat("\n--- α=0.0027 (k=3) ---\n")
cat("Potencia (1-β) :", round(pot_3, 6), "\n")
cat("β              :", round(1 - pot_3, 6), "\n")
cat("ARL0           :", round(1/0.0027, 2), "muestras\n")
cat("ARL1           :", round(ARL1_3, 4), "muestras\n")

# α=0.05 (k=1.96)
pot_196  <- potencia(delta_e, k = 1.96, n = n)
ARL1_196 <- 1 / pot_196

cat("\n--- α=0.05 (k=1.96) ---\n")
cat("Potencia (1-β) :", round(pot_196, 6), "\n")
cat("β              :", round(1 - pot_196, 6), "\n")
cat("ARL0           :", round(1/0.05, 2), "muestras\n")
cat("ARL1           :", round(ARL1_196, 4), "muestras\n")

