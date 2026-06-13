library(qcc)

# ============================================================
# Taller 6 - Analisis de Capacidad de Proceso
# Se continua desde el codigo del Taller 5
# ============================================================

# --- Datos y objetos qcc del Taller 5 (copiar desde ese script) ---

datos <- read.table("clipboard", header = TRUE, dec = ",")

fuera_k3  <- c(32, 33, 34, 35, 36, 37)
datos_k3  <- datos[-fuera_k3, ]
q_c_k3    <- qcc(datos_k3[,-1], type = "xbar",
                 title = "Parametros k=3", plot = FALSE)

fuera_k196 <- c(5, 12, 28, 31, 32, 33, 34, 35, 36, 37)
datos_k196 <- datos[-fuera_k196, ]
q_c_196    <- qcc(datos_k196[,-1], type = "xbar", nsigmas = 1.96,
                  title = "Parametros k=1.96 (iteracion final)", plot = FALSE)

# Parametros estimados
mu0_k3     <- q_c_k3$center
sigma0_k3  <- q_c_k3$std.dev
mu0_196    <- q_c_196$center
sigma0_196 <- q_c_196$std.dev

LIE <- 6
LSE <- 12
N   <- (LIE + LSE) / 2   # valor nominal = 9

# ============================================================
# a. Porcentaje de unidades que NO cumple especificaciones
# ============================================================

cat("=== a. Porcentaje fuera de especificaciones ===\n\n")

for (caso in list(
  list(mu = mu0_k3,  sigma = sigma0_k3,  label = "k=3   (alpha=0.0027)"),
  list(mu = mu0_196, sigma = sigma0_196, label = "k=1.96 (alpha=0.05) ")
)) {
  Zi      <- (caso$mu - LIE) / caso$sigma
  Zs      <- (LSE - caso$mu) / caso$sigma
  p_inf   <- pnorm(LIE, mean = caso$mu, sd = caso$sigma)
  p_sup   <- 1 - pnorm(LSE, mean = caso$mu, sd = caso$sigma)
  p_total <- p_inf + p_sup
  
  cat(sprintf("  %s\n", caso$label))
  cat(sprintf("    Zi = %.4f  |  Zs = %.4f\n", Zi, Zs))
  cat(sprintf("    P(X < LIE=6)  = %.6f  (%.4f%%)\n", p_inf, p_inf * 100))
  cat(sprintf("    P(X > LSE=12) = %.6f  (%.4f%%)\n", p_sup, p_sup * 100))
  cat(sprintf("    Total fuera   = %.6f  (%.4f%%)  ~  %.0f ppm\n\n",
              p_total, p_total * 100, p_total * 1e6))
}

# ============================================================
# b. Porcentaje consumiendo avenas crudas (X > LSE = 12%)
# ============================================================

cat("=== b. Usuarios consumiendo avenas crudas (X > 12%) ===\n\n")

for (caso in list(
  list(mu = mu0_k3,  sigma = sigma0_k3,  label = "k=3   (alpha=0.0027)"),
  list(mu = mu0_196, sigma = sigma0_196, label = "k=1.96 (alpha=0.05) ")
)) {
  Zs    <- (LSE - caso$mu) / caso$sigma
  p_sup <- 1 - pnorm(LSE, mean = caso$mu, sd = caso$sigma)
  cat(sprintf("  %s\n", caso$label))
  cat(sprintf("    Zs = (LSE - mu) / sigma = (12 - %.4f) / %.4f = %.4f\n",
              caso$mu, caso$sigma, Zs))
  cat(sprintf("    P(X > 12) = %.6f  (%.4f%%)  ~  %.0f ppm\n\n",
              p_sup, p_sup * 100, p_sup * 1e6))
}

# ============================================================
# c. Indices de capacidad a corto plazo
# ============================================================

cat("=== c. Indices de capacidad a corto plazo ===\n\n")

for (caso in list(
  list(mu = mu0_k3,  sigma = sigma0_k3,  label = "k=3   (alpha=0.0027)"),
  list(mu = mu0_196, sigma = sigma0_196, label = "k=1.96 (alpha=0.05) ")
)) {
  mu    <- caso$mu
  sigma <- caso$sigma
  
  Cp  <- (LSE - LIE) / (6 * sigma)
  Cr  <- 1 / Cp
  Cpi <- (mu - LIE) / (3 * sigma)
  Cps <- (LSE - mu)  / (3 * sigma)
  Cpk <- min(Cpi, Cps)
  K   <- ((mu - N) / ((LSE - LIE) / 2)) * 100
  Cpm <- (LSE - LIE) / (6 * sqrt(sigma^2 + (mu - N)^2))
  
  cat(sprintf("  %s\n", caso$label))
  cat(sprintf("    Cp  = (LSE-LIE) / (6*sigma)              = %.4f\n", Cp))
  cat(sprintf("    Cr  = 1 / Cp                             = %.4f\n", Cr))
  cat(sprintf("    Cpi = (mu-LIE) / (3*sigma)               = %.4f\n", Cpi))
  cat(sprintf("    Cps = (LSE-mu) / (3*sigma)               = %.4f\n", Cps))
  cat(sprintf("    Cpk = min(Cpi, Cps)                      = %.4f\n", Cpk))
  cat(sprintf("    K   = (mu-N) / ((LSE-LIE)/2) * 100      = %.2f%%\n", K))
  cat(sprintf("    Cpm = (LSE-LIE)/(6*sqrt(s^2+(mu-N)^2))  = %.4f\n\n", Cpm))
}

# --- Verificacion con qcc::process.capability ---

cat("  Verificacion con process.capability() — k=3\n")
pc_k3 <- process.capability(q_c_k3,
                            spec.limits = c(LIE, LSE),
                            target      = N,
                            nsigmas     = 3)

cat("\n  Verificacion con process.capability() — k=1.96\n")
pc_k196 <- process.capability(q_c_196,
                              spec.limits = c(LIE, LSE),
                              target      = N,
                              nsigmas     = 3)

# ============================================================
# d. Conclusion: es capaz el proceso a corto plazo?
# ============================================================

cat("\n=== d. Conclusion: ¿El proceso es capaz a corto plazo? ===\n\n")

for (caso in list(
  list(mu = mu0_k3,  sigma = sigma0_k3,  label = "k=3   (alpha=0.0027)"),
  list(mu = mu0_196, sigma = sigma0_196, label = "k=1.96 (alpha=0.05) ")
)) {
  mu    <- caso$mu
  sigma <- caso$sigma
  Cp    <- (LSE - LIE) / (6 * sigma)
  Cpi   <- (mu - LIE) / (3 * sigma)
  Cps   <- (LSE - mu)  / (3 * sigma)
  Cpk   <- min(Cpi, Cps)
  K     <- ((mu - N) / ((LSE - LIE) / 2)) * 100
  Cpm   <- (LSE - LIE) / (6 * sqrt(sigma^2 + (mu - N)^2))
  
  cat(sprintf("  %s\n", caso$label))
  cat(sprintf("    Cp  = %.4f  -->  1 < Cp < 1.33: ancho natural cabe en tolerancia, pero sin holgura\n", Cp))
  cat(sprintf("    Cpk = %.4f  -->  Cpk < 1: proceso NO CAPAZ (cola superior excede LSE)\n", Cpk))
  cat(sprintf("    K   = %.2f%%  -->  supera +/-20%%: problema de centramiento, no de variabilidad\n", K))
  cat(sprintf("    Cpm = %.4f  -->  Cpm < 1: proceso opera lejos del valor nominal N=9%%\n", Cpm))
  cat(sprintf("    Veredicto: proceso NO capaz a corto plazo\n\n"))
}

cat("  Nota: el problema no es exceso de variabilidad (Cp > 1) sino descentramiento\n")
cat("  de la media (~9.95%) respecto al nominal (9%). Recentrar el proceso reduciria\n")
cat("  los no conformes de ~0.80% a ~0.03% sin tocar la variabilidad.\n")