# ============================================================
# 0. Objetivo del análisis
# ============================================================
# Identificar, caracterizar y fechar un cambio de régimen
# en un proceso temporal, separando:
#   - un régimen "antes"
#   - un régimen "después"
# con evidencia estadística reproducible.

# ============================================================
# 1. Datos observados
# ============================================================

# Tiempo (semanas)
w <- 1:52

# Concentraciones observadas
conc <- c(
  35.8, 33.0, 33.6, 35.0, 33.5, 34.7, 33.6, 36.9, 38.8, 35.5,  
  32.2, 32.2, 33.3, 33.5, 33.0, 33.1, 33.5, 31.9, 31.7, 32.4, 
  34.8, 33.5, 33.9, 32.8, 34.2, 33.4, 31.1, 33.6, 28.9, 35.6, 
  32.9, 31.8, 37.4, 32.0, 34.8, 31.7, 32.7, 36.0, 34.2, 30.3, 
  39.6, 34.6, 31.7, 30.3, 34.4, 32.4, 31.1, 36.5, 33.2, 34.3, 
  35.8, 32.4
)

# Exploración visual inicial
plot(w, conc, type = "b", pch = 19,
     xlab = "Semana",
     ylab = "Concentración",
     main = "Serie temporal observada")

# ============================================================
# 2. Modelo de referencia del proceso (baseline)
# ============================================================

library(mgcv)

# Modelo GAM: captura la tendencia suave esperada del proceso
mod_ref <- gam(conc ~ s(w, k = 6), method = "REML")
summary(mod_ref)

# Visualización del baseline
plot(w, conc, pch = 19,
     xlab = "Semana",
     ylab = "Concentración",
     main = "Modelo de referencia (baseline del proceso)")
lines(w, fitted(mod_ref), col = "red", lwd = 2)

# ============================================================
# 3. Residuales: desviación respecto al régimen esperado
# ============================================================

# Residuales del modelo de referencia
res <- resid(mod_ref)

# Diagnóstico visual
plot(w, res, type = "b", pch = 19,
     xlab = "Semana",
     ylab = "Residual",
     main = "Residuales respecto al baseline")
abline(h = 0, col = "gray")

# ============================================================
# 4. Reducción no paramétrica: signos
# ============================================================

# Dirección del error (−1, 0, +1)
z <- sign(res)

# ============================================================
# 5. CUSUM no paramétrico: evidencia global de cambio
# ============================================================

cusum <- cumsum(z)

plot(w, cusum, type = "l", lwd = 2,
     xlab = "Semana",
     ylab = "CUSUM (signos)",
     main = "CUSUM no paramétrico (evidencia acumulada)")
abline(h = 0, col = "gray")

# ============================================================
# 6. MOSUM no paramétrico: fase de transición
# ============================================================

# Tamaño de ventana (escala local del proceso)
h <- 8

mosum <- sapply(h:length(z), function(t) {
  sum(z[(t - h + 1):t])
})

w_mosum <- w[h:length(w)]

plot(w_mosum, mosum, type = "l", lwd = 2,
     xlab = "Semana",
     ylab = "MOSUM (signos)",
     main = paste("MOSUM no paramétrico (h =", h, ")"))
abline(h = 0, col = "gray")

# ============================================================
# 7. Detección explícita del change-point (decisión)
# ============================================================

library(changepoint)

# AMOC + AIC: enfoque diagnóstico orientado a decisión
cp <- cpt.mean(
  res,
  method  = "AMOC",
  penalty = "AIC"
)

cp_index <- cpts(cp)
cp_index

# ============================================================
# 8. Integración visual centrada en el change-point
# ============================================================

# Residuales con punto de quiebre
plot(w, res, type = "b", pch = 19,
     xlab = "Semana",
     ylab = "Residual",
     main = "Change-point detectado en residuales")
abline(v = cp_index, col = "red", lwd = 2, lty = 2)
abline(h = 0, col = "gray")

# MOSUM con punto de quiebre
plot(w_mosum, mosum, type = "l", lwd = 2,
     xlab = "Semana",
     ylab = "MOSUM (signos)",
     main = "MOSUM y fase de transición")
abline(h = 0, col = "gray")
abline(v = cp_index, col = "red", lwd = 2, lty = 2)







