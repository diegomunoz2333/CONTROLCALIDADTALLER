#------------------------------------------------------#
#-- Universidad del Valle                            --#
#-- Asignatura: Control de Calidad                   --#
#-- Profesor: Ivan Mauricio Bermudez Vera            --#
#------------------------------------------------------#


# Libreria especial para hacer carga automática de librerias
if (!require("pacman")) install.packages("pacman")

# Verificación, instalación y carga de librerias.
pacman::p_load(ggplot2, patchwork, qcc)


#=====================================#
# Graficos de control por variables   #
#=====================================#

# Datos en el archivo de excel compartido en el campus

# Ejemplo1: Peso de los granulos de PVC.

datos_ejemplo1 <- read.table("clipboard", header=TRUE, dec=",")
datos <- datos_ejemplo1


#=============================#
# Carta de control Xbarra-R   #
#agregado avr si pasa algo:
str(datos)
head(datos)
readClipboard()
# Parámetros
X_bar <- mean(datos$medias)
R_bar <- mean(datos$rangos)

# Constantes para n=4 (Se buscan en la tabla)

A2 <-0.729
D3 <-0
D4 <-2.282

# Límites para carta X̄
LCS_x <- X_bar + (A2*R_bar)
LCI_x <- X_bar - (A2*R_bar)

# Límites para carta R
LCS_R <- D4*R_bar
LCI_R <- D3*R_bar

# Data frame
df <- data.frame(datos,
                 Fuera_X = (datos$medias<LCI_x | datos$medias>LCS_x),
                 Fuera_R = (datos$rangos<LCI_R | datos$rangos>LCS_R)
)


LCS_x; LCI_x; LCS_R; LCI_R 


# Cartas de Control:

# Gráfico X-barra
q_xbar <- ggplot(df, aes(x = muestra, y = medias)) +
  geom_line() +
  geom_point(aes(color = Fuera_X), size = 3) +
  scale_color_manual(values = c("FALSE" = "blue", "TRUE" = "red")) +
  geom_hline(yintercept = X_bar, linetype = "dashed", color = "black", size = 1) +
  geom_hline(yintercept = LCS_x, linetype = "dashed", color = "red", size = 1) +
  geom_hline(yintercept = LCI_x, linetype = "dashed", color = "red", size = 1) +
  
  labs(title = "Carta de Control X̄ (n = 4)", y = "Media", x = "Subgrupo") +
  theme_minimal()+
  theme(legend.position = "none")


# Gráfico R
q_xr <- ggplot(df, aes(x = muestra, y = rangos)) +
  geom_line() +
  geom_point(aes(color = Fuera_R), size = 3) +
  scale_color_manual(values = c("FALSE" = "blue", "TRUE" = "red")) +
  geom_hline(yintercept = R_bar, linetype = "dashed", color = "black", size = 1) +
  geom_hline(yintercept = LCS_R, linetype = "dashed", color = "red", size = 1) +
  geom_hline(yintercept = LCI_R, linetype = "dashed", color = "red", size = 1) +
  
  labs(title = "Carta de Control R (n = 4)", y = "Rango", x = "Subgrupo") +
  theme_minimal()+
  theme(legend.position = "none")

# Combinar ambos gráficos en una sola ventana

q_xbar / q_xr


# Identificamos cuales medias se salen de los limites de control

which(datos$medias>LCS_x)

# Eliminamos los datos fuera de control
datos <- datos[datos$medias<=LCS_x , ]   

# Se vuelven a estimar los limites de control 
# Se comprueba que las cartas esten bajo control
# Se fijan los limites de esta carta para controlar el proceso.
LCS_x; LCI_x; LCS_R; LCI_R 




# Ejemplo2

#=============================#
# Carta de control Xbarra-S   #
#=============================#

# Datos

datos_ejemplo2 <- read.table("clipboard", header=TRUE, dec=",")
datos <- datos_ejemplo2


# Parámetros
n <- 10
x_barra <- mean(datos$medias)
s_barra <- mean(datos$desviaciones)

# Constantes para n=10
A3 <- 0.975
B3 <- 0.284
B4 <- 1.716

# Límites carta S
CL_s <- s_barra
UCL_s <- B4 * s_barra
LCL_s <- B3 * s_barra

# Límites carta X̄
CL_x <- x_barra
UCL_x <- x_barra + (A3*s_barra)
LCL_x <- x_barra - (A3*s_barra)



# Data frame
df <- data.frame(datos,
                 Fuera_S = (datos$desviaciones < LCL_s | datos$desviaciones > UCL_s),
                 Fuera_X = (datos$medias < LCL_x | datos$medias > UCL_x)
)


# Graficas:

# Carta X̄
q_xbar <- ggplot(df, aes(x = muestra, y = medias)) +
  geom_line(color = "blue") +
  geom_point(aes(color = Fuera_X), size = 2) +
  scale_color_manual(values = c("FALSE" = "purple", "TRUE" = "red")) +
  geom_hline(yintercept = UCL_x, linetype = "dashed", color = "red") +
  geom_hline(yintercept = CL_x, linetype = "dashed", color = "black") +
  geom_hline(yintercept = LCL_x, linetype = "dashed", color = "red") +
  labs(title = "Carta de Control X̄", x = "Subgrupo", y = "Media") +
  theme_minimal() +
  theme(legend.position = "none")

# Carta S
q_xs <- ggplot(df, aes(x = muestra, y = desviaciones)) +
  geom_line(color = "blue") +
  geom_point(aes(color = Fuera_S), size = 2) +
  scale_color_manual(values = c("FALSE" = "purple", "TRUE" = "red")) +
  geom_hline(yintercept = UCL_s, linetype = "dashed", color = "red") +
  geom_hline(yintercept = CL_s, linetype = "dashed", color = "black") +
  geom_hline(yintercept = LCL_s, linetype = "dashed", color = "red") +
  labs(title = "Carta de Control S", x = "Subgrupo", y = "S") +
  theme_minimal() +
  theme(legend.position = "none")


# Combinar ambos gráficos en una sola ventana

q_xbar / q_xs




# Ejemplo3: Cantidad de llenado bebidas

datos_ejemplo3 <- read.table("clipboard", header=TRUE, dec=".")

datos <- datos_ejemplo3

q_xbar <- qcc(datos[,-1], type = "xbar",
              title = "Carta de Control para la Media")

q_xs <-  qcc(datos[,-1], type = "S",
             title = "Carta de Control para la Desviación Estándar")




#---FIN-----

