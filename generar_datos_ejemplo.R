# Genera datos_ejemplo.xlsx: lecturas de temperatura inventadas para dos estufas.
# Ejecutar desde la carpeta sonda/.
#
# Los nombres de las estufas, las sondas, los rangos de trabajo y los factores de corrección
# también son inventados; si se cambian acá, hay que copiarlos en datos_equipos() de app3.R.

library(dplyr)
library(writexl)

set.seed(1955)

equipos <- tibble(
  EquipoD       = c("Estufa 3817", "Estufa 2290"),
  IdEquipo      = c("SN-48213", "SN-71604"),
  factor_correc = c(0.32, -0.18),
  lim_inf       = c(34.65, 43.80),
  lim_sup       = c(35.40, 44.70)
)

inicio <- as.POSIXct("2026-03-02 00:00:00", tz = "UTC")
tiempos <- seq(inicio, by = 10 * 60, length.out = 21 * 24 * 6)   # 3 semanas, cada 10 minutos
n <- length(tiempos)
hora <- as.numeric(format(tiempos, "%H"))
dia_semana <- as.numeric(format(tiempos, "%u"))

# Ruido suave (autorregresivo)
ruido <- function(n, phi = 0.9, sd = 0.08) {
  x <- numeric(n); x[1] <- rnorm(1, 0, sd)
  for (i in 2:n) x[i] <- phi * x[i - 1] + rnorm(1, 0, sd * sqrt(1 - phi^2))
  x
}

# Apertura de puerta: baja de golpe y se recupera en ~40 minutos
aperturas <- function() {
  efecto <- numeric(n)
  laborable <- which(hora >= 8 & hora < 17 & dia_semana <= 5)
  for (i in sample(laborable, 45)) {
    caida <- runif(1, 0.4, 2.8)
    k <- 0:8
    idx <- i + k
    idx <- idx[idx <= n]
    efecto[idx] <- efecto[idx] - caida * exp(-k[seq_along(idx)] / 2)
  }
  efecto
}

# Episodio de sobrecalentamiento de algunas horas
sobrecalentamiento <- function() {
  efecto <- numeric(n)
  for (i in sample(seq_len(n - 40), 2)) {
    largo <- sample(12:36, 1)
    forma <- sin(seq(0, pi, length.out = largo))
    efecto[i:(i + largo - 1)] <- efecto[i:(i + largo - 1)] + runif(1, 1.4, 2.4) * forma
  }
  efecto
}

lecturas <- bind_rows(lapply(seq_len(nrow(equipos)), function(e) {
  centro <- mean(c(equipos$lim_inf[e], equipos$lim_sup[e]))
  temp_real <- centro + ruido(n) + aperturas() + sobrecalentamiento()
  temp <- round(temp_real - equipos$factor_correc[e], 2)   # lo que mide la sonda (sin corregir)
  temp[sample(n, 3)] <- 0                                    # lecturas en 0 (fallas del registrador)
  tibble(Eq_Date_Id = tiempos, EquipoD = equipos$EquipoD[e],
         IdEquipo = equipos$IdEquipo[e], Temperatura = temp)
}))

write_xlsx(lecturas, "datos_ejemplo.xlsx")
cat("Escrito datos_ejemplo.xlsx:", nrow(lecturas), "lecturas\n")
lecturas %>%
  left_join(equipos, by = c("EquipoD", "IdEquipo")) %>%
  filter(Temperatura > 0) %>%
  mutate(corr = Temperatura + factor_correc) %>%
  group_by(EquipoD) %>%
  summarise(min = min(corr), media = round(mean(corr), 2), max = max(corr),
            debajo = sum(corr < lim_inf), debajo_1 = sum(corr < lim_inf - 1),
            encima = sum(corr > lim_sup), encima_1 = sum(corr > lim_sup + 1)) %>%
  print()
