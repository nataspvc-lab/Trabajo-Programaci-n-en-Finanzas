# Desafio 4 - Programacion en Finanzas
#
# Entradas esperadas en el directorio de ejecucion:
#   maestro_emisores.csv, precios_SCL.csv, precios_NYC.csv,
#   tipo_cambio.csv, eventos_corporativos.csv, salidas_de_bolsa.csv,
#   parametros_riesgo.csv y cartera.csv.
#
# Salidas:
#   riesgo.csv, calidad_datos.csv y backtest.csv.

suppressPackageStartupMessages(library(data.table))

options(scipen = 999)

archivos_requeridos <- c(
  "maestro_emisores.csv",
  "precios_SCL.csv",
  "precios_NYC.csv",
  "tipo_cambio.csv",
  "eventos_corporativos.csv",
  "salidas_de_bolsa.csv",
  "parametros_riesgo.csv",
  "cartera.csv"
)

faltantes <- archivos_requeridos[!file.exists(archivos_requeridos)]
if (length(faltantes) > 0L) {
  stop("Faltan archivos requeridos: ", paste(faltantes, collapse = ", "))
}

requerir_columnas <- function(x, columnas, nombre) {
  faltan <- setdiff(columnas, names(x))
  if (length(faltan) > 0L) {
    stop(nombre, " no contiene: ", paste(faltan, collapse = ", "))
  }
}

convertir_fecha <- function(x, formato, nombre) {
  if (inherits(x, "Date")) {
    if (anyNA(x)) stop("Hay fechas invalidas en ", nombre)
    return(as.IDate(x))
  }
  tokens <- unique(x)
  fechas <- as.IDate(tokens, format = formato)
  if (anyNA(fechas) || any(format(fechas, formato) != tokens)) {
    stop("Hay fechas invalidas en ", nombre)
  }
  fechas[match(x, tokens)]
}

secuencia <- function(desde, hasta) {
  if (desde > hasta) integer() else seq.int(desde, hasta)
}

cuantil <- function(x, alfa) {
  as.numeric(quantile(x, probs = alfa, type = 7, names = FALSE))
}

x_log_p <- function(x, p) {
  if (x == 0) {
    return(0)
  }
  if (p <= 0) {
    return(-Inf)
  }
  x * log(p)
}

prueba_kupiec <- function(incumplimiento, alfa) {
  incumplimiento <- as.logical(incumplimiento)
  t_obs <- length(incumplimiento)
  if (!t_obs) return(list(lr = NA_real_, p_valor = NA_real_))
  x <- sum(incumplimiento)
  p_esperada <- 1 - alfa
  p_observada <- x / t_obs

  ll_0 <- x_log_p(x, p_esperada) +
    x_log_p(t_obs - x, 1 - p_esperada)
  ll_1 <- x_log_p(x, p_observada) +
    x_log_p(t_obs - x, 1 - p_observada)

  lr <- max(0, -2 * (ll_0 - ll_1))
  list(lr = lr, p_valor = pchisq(lr, df = 1, lower.tail = FALSE))
}

prueba_christoffersen <- function(incumplimiento) {
  incumplimiento <- as.integer(incumplimiento)
  if (length(incumplimiento) < 2L) {
    return(list(lr = NA_real_, p_valor = NA_real_))
  }

  anterior <- incumplimiento[-length(incumplimiento)]
  actual <- incumplimiento[-1L]
  n00 <- sum(anterior == 0L & actual == 0L)
  n01 <- sum(anterior == 0L & actual == 1L)
  n10 <- sum(anterior == 1L & actual == 0L)
  n11 <- sum(anterior == 1L & actual == 1L)

  pi_01 <- if (n00 + n01 > 0L) n01 / (n00 + n01) else 0
  pi_11 <- if (n10 + n11 > 0L) n11 / (n10 + n11) else 0
  pi_total <- (n01 + n11) / (n00 + n01 + n10 + n11)

  ll_restringida <- x_log_p(n01 + n11, pi_total) +
    x_log_p(n00 + n10, 1 - pi_total)
  ll_libre <- x_log_p(n01, pi_01) +
    x_log_p(n00, 1 - pi_01) +
    x_log_p(n11, pi_11) +
    x_log_p(n10, 1 - pi_11)

  lr <- max(0, -2 * (ll_restringida - ll_libre))
  list(lr = lr, p_valor = pchisq(lr, df = 1, lower.tail = FALSE))
}

maestro <- fread(
  "maestro_emisores.csv",
  colClasses = c(id = "character", ticker = "character"),
  showProgress = FALSE
)
requerir_columnas(
  maestro,
  c("id", "ticker", "bolsa", "moneda", "desde", "hasta"),
  "maestro_emisores.csv"
)
maestro[, desde := convertir_fecha(desde, "%Y-%m-%d", "maestro_emisores.csv")]
maestro[, hasta := convertir_fecha(hasta, "%Y-%m-%d", "maestro_emisores.csv")]
maestro[, bolsa := toupper(trimws(bolsa))]
if (anyNA(maestro[, .(id, ticker, bolsa)]) || any(maestro$id == "") ||
    any(maestro$ticker == "") || any(!maestro$bolsa %chin% c("SCL", "NYC")) ||
    any(maestro$desde > maestro$hasta)) {
  stop("Hay identificadores, bolsas o vigencias invalidos en el maestro")
}
setorder(maestro, bolsa, ticker, desde)
solapamientos <- maestro[, .(
  solape = any(desde <= shift(cummax(as.integer(hasta))), na.rm = TRUE)
), by = .(bolsa, ticker)]
if (any(solapamientos$solape)) {
  stop("Hay vigencias superpuestas para un mismo ticker dentro de una bolsa")
}
ids_multibolsa <- maestro[, .(n_bolsas = uniqueN(bolsa)), by = id][
  n_bolsas > 1L, id
]

cartera <- fread(
  "cartera.csv", colClasses = c(id = "character"), showProgress = FALSE
)
requerir_columnas(cartera, c("id", "peso"), "cartera.csv")
cartera <- cartera[, .(id = as.character(id), peso = as.numeric(peso))]
if (nrow(cartera) != 20L || uniqueN(cartera$id) != 20L) {
  stop("cartera.csv debe contener 20 ids distintos")
}
if (anyNA(cartera$id) || any(cartera$id == "") ||
    any(!is.finite(cartera$peso) | cartera$peso < 0)) {
  stop("cartera.csv contiene valores invalidos")
}
if (abs(sum(cartera$peso) - 1) > 1e-8) {
  stop("Los pesos de cartera.csv no suman 1")
}
if (any(!cartera$id %chin% maestro$id)) {
  stop("cartera.csv contiene ids ausentes del maestro")
}
ids_cartera <- cartera$id

parametros <- fread("parametros_riesgo.csv", showProgress = FALSE)
requerir_columnas(
  parametros,
  c("alfa", "ventana", "inicio", "lambda", "garch_alfa", "garch_beta"),
  "parametros_riesgo.csv"
)
if (nrow(parametros) != 1L) {
  stop("parametros_riesgo.csv debe tener exactamente una fila")
}
alfa <- as.numeric(parametros$alfa)
ventana <- as.numeric(parametros$ventana)
inicio <- as.IDate("2014-01-02")
lambda <- as.numeric(parametros$lambda)
garch_alfa <- as.numeric(parametros$garch_alfa)
garch_beta <- as.numeric(parametros$garch_beta)
if (any(!is.finite(c(alfa, ventana, lambda, garch_alfa, garch_beta))) ||
    alfa <= 0 || alfa >= 1 || ventana < 2L || ventana != trunc(ventana) ||
    lambda <= 0 || lambda >= 1) {
  stop("Parametros de riesgo fuera de rango")
}
if (garch_alfa < 0 || garch_beta < 0 || garch_alfa + garch_beta >= 1) {
  stop("Los parametros GARCH deben ser no negativos y sumar menos de 1")
}

tipo_cambio <- fread(
  "tipo_cambio.csv",
  sep = ";",
  dec = ",",
  showProgress = FALSE
)
requerir_columnas(tipo_cambio, c("fecha", "clp_por_usd"), "tipo_cambio.csv")
tipo_cambio[, fecha := convertir_fecha(fecha, "%d-%m-%Y", "tipo_cambio.csv")]
tipo_cambio[, clp_por_usd := as.numeric(clp_por_usd)]
if (any(!is.finite(tipo_cambio$clp_por_usd) | tipo_cambio$clp_por_usd <= 0)) {
  stop("tipo_cambio.csv contiene tipos de cambio invalidos")
}
setorder(tipo_cambio, fecha)
if (anyDuplicated(tipo_cambio$fecha)) {
  tipo_cambio <- unique(tipo_cambio, by = c("fecha", "clp_por_usd"))
  if (anyDuplicated(tipo_cambio$fecha)) {
    stop("Hay tipos de cambio distintos para una misma fecha")
  }
}

eventos <- fread(
  "eventos_corporativos.csv",
  colClasses = c(ticker = "character", tipo = "character", valor = "numeric"),
  showProgress = FALSE
)
requerir_columnas(eventos, c("ticker", "fecha", "tipo", "valor"),
                  "eventos_corporativos.csv")
eventos[, fecha := convertir_fecha(fecha, "%Y-%m-%d",
                                   "eventos_corporativos.csv")]
eventos[, tipo := toupper(trimws(tipo))]
if (any(!eventos$tipo %chin% c("SPLIT", "DIVIDENDO"))) {
  stop("Hay tipos de evento desconocidos")
}
if (any(!is.finite(eventos$valor) | eventos$valor < 0 |
        (eventos$tipo == "SPLIT" & eventos$valor == 0))) {
  stop("Hay valores de eventos invalidos")
}

eventos_mapeados <- maestro[
  eventos,
  on = .(ticker, desde <= fecha, hasta >= fecha),
  .(
    id,
    bolsa,
    fecha = i.fecha,
    ticker = i.ticker,
    tipo = i.tipo,
    valor = i.valor
  ),
  nomatch = NA
]
if (nrow(eventos_mapeados) != nrow(eventos) || anyNA(eventos_mapeados$id)) {
  stop("Algun evento queda sin id unico; el archivo no identifica su bolsa")
}

splits_cartera <- eventos_mapeados[
  id %chin% ids_cartera & tipo == "SPLIT", .N
]
dividendos_cartera <- eventos_mapeados[
  id %chin% ids_cartera & tipo == "DIVIDENDO", .N
]

eventos_dia <- eventos_mapeados[, .(
  factor_split = prod(fifelse(tipo == "SPLIT", valor, 1)),
  dividendo = sum(fifelse(tipo == "DIVIDENDO", valor, 0)),
  n_eventos = .N
), by = .(bolsa, id, fecha)]

salidas <- fread(
  "salidas_de_bolsa.csv",
  colClasses = c(id = "character", retorno_final = "numeric"),
  showProgress = FALSE
)
requerir_columnas(
  salidas,
  c("id", "ultimo_dia", "retorno_final"),
  "salidas_de_bolsa.csv"
)
salidas[, ultimo_dia := convertir_fecha(
  ultimo_dia, "%Y-%m-%d", "salidas_de_bolsa.csv"
)]
if (any(!is.finite(salidas$retorno_final) | salidas$retorno_final < -1) ||
    any(!salidas$id %chin% maestro$id) || anyDuplicated(salidas$id)) {
  stop("salidas_de_bolsa.csv contiene valores o ids invalidos")
}

emisores_con_cambio <- maestro[, .(n_tickers = uniqueN(ticker)), by = id][
  n_tickers > 1L, .N
]

calcular_retornos <- function(precios, origen) {
  setorder(precios, id, fecha)
  if (anyDuplicated(precios, by = c("id", "fecha"))) {
    stop("Hay mas de un cierre para un mismo id y fecha en ", origen)
  }
  precios[, retorno :=
    ((precio * factor_split) / fx) /
      (shift(precio) / shift(fx)) - 1,
  by = id]
  if (any(is.nan(precios$retorno) | is.infinite(precios$retorno))) {
    stop("Se obtuvieron retornos no finitos en ", origen)
  }
  precios[!is.na(retorno), .(id, fecha, retorno)]
}

procesar_bolsa <- function(archivo, bolsa_obj) {
  es_scl <- bolsa_obj == "SCL"
  columna_fecha <- if (es_scl) "fecha" else "date"
  columna_precio <- if (es_scl) "cierre" else "close"
  columnas <- c(columna_fecha, "ticker", columna_precio, "version")
  precios <- fread(
    archivo,
    sep = if (es_scl) ";" else ",",
    dec = if (es_scl) "," else ".",
    select = columnas,
    colClasses = c(ticker = "character"),
    showProgress = FALSE
  )
  requerir_columnas(precios, columnas, archivo)
  setnames(precios, c(columna_fecha, columna_precio), c("fecha", "precio"))
  formato_fecha <- if (es_scl) "%d/%m/%Y" else "%Y-%m-%d"
  precios[, fecha := convertir_fecha(fecha, formato_fecha, archivo)]

  filas_originales <- nrow(precios)
  dias_archivo <- unique(precios$fecha)

  if (anyNA(precios$ticker) || any(precios$ticker == "") ||
      (nrow(precios) && !is.numeric(precios$version)) ||
      any(!is.finite(precios$version))) {
    stop(archivo, " contiene tickers o versiones invalidos")
  }

  setorder(precios, fecha, ticker, version)
  repetida <- duplicated(precios, by = c("fecha", "ticker"), fromLast = TRUE)
  pares_duplicados <- sum(repetida)
  # Las versiones empatadas con cierres distintos son ambiguas solo si
  # corresponden a la version maxima que se usara.
  filas_rep <- which(repetida)
  izquierda <- precios$precio[filas_rep]
  derecha <- precios$precio[filas_rep + 1L]
  distintos <- xor(is.na(izquierda), is.na(derecha)) |
    (!is.na(izquierda) & !is.na(derecha) & izquierda != derecha)
  conflictos <- filas_rep[distintos &
    precios$version[filas_rep] == precios$version[filas_rep + 1L]]
  if (length(conflictos)) {
    claves <- c("fecha", "ticker", "version")
    if (nrow(precios[!repetida, ..claves][precios[conflictos, ..claves],
                                         on = claves, nomatch = NULL])) {
      stop(archivo, " tiene cierres distintos en una misma version maxima")
    }
  }
  precios <- precios[!repetida]

  maestro_bolsa <- maestro[bolsa == bolsa_obj]
  precios <- maestro_bolsa[
    precios,
    on = .(ticker, desde <= fecha, hasta >= fecha),
    .(
      id,
      fecha = i.fecha,
      precio = i.precio
    ),
    nomatch = NA
  ]
  if (nrow(precios) != filas_originales - pares_duplicados || anyNA(precios$id)) {
    stop("No fue posible asignar cada precio de ", archivo,
         " a un unico id vigente")
  }

  precios[salidas, on = .(id), ultimo_dia := i.ultimo_dia]
  filas_post_salida <- precios[
    !is.na(ultimo_dia) & fecha > ultimo_dia, .N
  ]
  precios <- precios[is.na(ultimo_dia) | fecha <= ultimo_dia]
  precios[, ultimo_dia := NULL]
  if (nrow(precios) && (!is.numeric(precios$precio) ||
      any(!is.finite(precios$precio) | precios$precio <= 0))) {
    stop(archivo, " contiene cierres vigentes no positivos o no finitos")
  }
  precios[, precio := as.numeric(precio)]

  eventos_bolsa <- eventos_dia[bolsa == bolsa_obj]
  filas_evento <- precios[eventos_bolsa, on = .(id, fecha),
                          which = TRUE, mult = "first"]
  sin_precio <- is.na(filas_evento)
  precios[, `:=`(factor_split = 1, dividendo = 0)]
  precios[eventos_bolsa, on = .(id, fecha), `:=`(
    factor_split = i.factor_split,
    dividendo = i.dividendo
  )]

  if (es_scl) {
    fila_fx <- findInterval(as.integer(precios$fecha),
                            as.integer(tipo_cambio$fecha))
    if (any(fila_fx == 0L)) {
      stop("Hay precios SCL sin un tipo de cambio anterior disponible")
    }
    precios[, fx := tipo_cambio$clp_por_usd[fila_fx]]
  } else {
    precios[, fx := 1]
  }

  precios_multibolsa <- precios[0L]
  if (length(ids_multibolsa)) {
    multibolsa <- precios$id %chin% ids_multibolsa
    precios_multibolsa <- precios[multibolsa]
    precios <- precios[!multibolsa]
  }
  retornos <- calcular_retornos(precios, archivo)

  rm(precios)
  gc(verbose = FALSE)

  list(
    retornos = retornos,
    precios_multibolsa = precios_multibolsa,
    dias = dias_archivo,
    filas = filas_originales,
    duplicados = pares_duplicados,
    post_salida = filas_post_salida,
    eventos_sin_precio = sum(eventos_bolsa$n_eventos[sin_precio]),
    eventos_cartera_sin_precio = sum(
      eventos_bolsa$n_eventos[sin_precio & eventos_bolsa$id %chin% ids_cartera]
    )
  )
}

resultado_scl <- procesar_bolsa("precios_SCL.csv", "SCL")
resultado_nyc <- procesar_bolsa("precios_NYC.csv", "NYC")

eventos_sin_precio <- resultado_scl$eventos_sin_precio +
  resultado_nyc$eventos_sin_precio
if (eventos_sin_precio) {
  eventos_cartera_sin_precio <- resultado_scl$eventos_cartera_sin_precio +
    resultado_nyc$eventos_cartera_sin_precio
  warning(eventos_sin_precio, " eventos sin cierre vigente en su fecha (",
          eventos_cartera_sin_precio, " en cartera). Se aplica la regla ",
          "de retorno solo en fechas con cierre.", call. = FALSE)
}
retornos_multibolsa <- calcular_retornos(
  rbindlist(list(resultado_scl$precios_multibolsa,
                 resultado_nyc$precios_multibolsa)),
  "emisores con cambio de bolsa"
)

dias_fondo <- sort(unique(c(resultado_scl$dias, resultado_nyc$dias)))
dias_sin_tc <- sum(!dias_fondo %in% tipo_cambio$fecha)

indice_pago <- findInterval(salidas$ultimo_dia, dias_fondo) + 1L
salidas_validas <- indice_pago <= length(dias_fondo)
retornos_salida <- data.table(
  id = salidas$id[salidas_validas],
  fecha = dias_fondo[indice_pago[salidas_validas]],
  retorno = salidas$retorno_final[salidas_validas]
)

retornos_evento <- rbindlist(
  list(
    resultado_scl$retornos,
    resultado_nyc$retornos,
    retornos_multibolsa,
    retornos_salida
  ),
  use.names = TRUE
)
if (anyDuplicated(retornos_evento, by = c("id", "fecha"))) {
  stop("Un emisor tiene mas de un retorno asignado en la misma fecha")
}

retorno_mercado <- retornos_evento[, .(
  retorno_mercado = mean(retorno)
), by = fecha]

retorno_cartera_evento <- merge(
  retornos_evento,
  cartera,
  by = "id",
  all = FALSE,
  sort = FALSE
)
retorno_cartera <- retorno_cartera_evento[, .(
  retorno_cartera = sum(peso * retorno)
), by = fecha]

muestra <- data.table(fecha = dias_fondo[dias_fondo >= inicio])
muestra[retorno_cartera, on = .(fecha),
        retorno_cartera := i.retorno_cartera]
muestra[retorno_mercado, on = .(fecha),
        retorno_mercado := i.retorno_mercado]
muestra[is.na(retorno_cartera), retorno_cartera := 0]
muestra[is.na(retorno_mercado), retorno_mercado := 0]
setorder(muestra, fecha)

# ============================================================
# Clase 5 - Bloque 2
# Cartera del Desafio 4, cuota sin dividendos
# ============================================================

fecha_inicio <- as.IDate("2014-01-02")
fecha_valoracion <- as.IDate("2022-06-30")
ventana_b2 <- 250L
lambda_b2 <- 0.94
garch_a_b2 <- 0.08
garch_b_b2 <- 0.90
horizonte_b2 <- 126L

muestra <- muestra[
  fecha >= fecha_inicio & fecha <= fecha_valoracion
]
setorder(muestra, fecha)

r <- muestra$retorno_cartera
n <- length(r)

if (n < ventana_b2) {
  stop("No existen al menos 250 retornos hasta la fecha de valoracion")
}
if (max(muestra$fecha) != fecha_valoracion) {
  warning(
    "La ultima fecha disponible es ",
    as.character(max(muestra$fecha)),
    call. = FALSE
  )
}
if (any(!is.finite(r)) || any(r <= -1)) {
  stop("La serie contiene retornos invalidos")
}

# Cuota sin dividendos, normalizada en 1 al inicio.
serie_cuota <- cumprod(1 + r)
q <- tail(serie_cuota, 1L)

# Volatilidad historica diaria con los ultimos 250 retornos.
retornos_250 <- tail(r, ventana_b2)
sigma_historica <- sd(retornos_250)

# Inicializacion RiskMetrics con lambda = 0,94.
varianza_rm <- var(r[seq_len(ventana_b2)])
if (n > ventana_b2) {
  for (t in seq.int(ventana_b2 + 1L, n)) {
    varianza_rm <- lambda_b2 * varianza_rm +
      (1 - lambda_b2) * r[t - 1L]^2
  }
}

# GARCH(1,1) hasta el 30-06-2022.
varianza_largo_plazo <- var(retornos_250)
omega_b2 <- (1 - garch_a_b2 - garch_b_b2) *
  varianza_largo_plazo

varianza_garch <- varianza_rm
if (n > ventana_b2) {
  for (t in seq.int(ventana_b2 + 1L, n)) {
    varianza_garch <- omega_b2 +
      garch_a_b2 * r[t - 1L]^2 +
      garch_b_b2 * varianza_garch
  }
}

# Pronostico y agregacion de la varianza para 126 dias.
varianzas_futuras <- numeric(horizonte_b2)
varianzas_futuras[1L] <- omega_b2 +
  garch_a_b2 * r[n]^2 +
  garch_b_b2 * varianza_garch

if (horizonte_b2 > 1L) {
  for (h in 2:horizonte_b2) {
    varianzas_futuras[h] <- omega_b2 +
      (garch_a_b2 + garch_b_b2) *
      varianzas_futuras[h - 1L]
  }
}

sigma_garch_horizonte <- sqrt(sum(varianzas_futuras))

resultados_bloque2 <- data.table(
  q = round(q, 4),
  sigma_historica = round(sigma_historica, 4),
  sigma_garch_126_dias = round(sigma_garch_horizonte, 4)
)

fwrite(resultados_bloque2, "resultados_bloque2.csv")

cat("\nClase 5 - Bloque 2\n")
cat("------------------------------\n")
cat("Inicio:", as.character(min(muestra$fecha)), "\n")
cat("Fecha de valoracion:", as.character(max(muestra$fecha)), "\n")
cat("Observaciones:", n, "\n\n")
cat(sprintf("q: %.4f\n", q))
cat(sprintf("Sigma historica: %.4f\n", sigma_historica))
cat(sprintf(
  "Sigma GARCH del horizonte de 126 dias: %.4f\n",
  sigma_garch_horizonte
))
cat("\nTambien se guardo resultados_bloque2.csv\n")
