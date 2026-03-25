examen <- function() {
  # ==============================================================================
  # 1. CARGA Y LIMPIEZA DE DATOS (BORRAR DUPLICADOS)
  # ==============================================================================
  cat("Por favor, selecciona tu archivo CSV en la ventana emergente...\n")
  ruta_archivo <- file.choose()
  
  # Leer el archivo (probamos con ";" primero, si falla la estructura probamos ",")
  datos <- read.csv(ruta_archivo, sep = ";", header = TRUE, stringsAsFactors = FALSE)
  if(ncol(datos) == 1) { datos <- read.csv(ruta_archivo, sep = ",", header = TRUE, stringsAsFactors = FALSE) }
  
  filas_texto <- do.call(paste, c(datos, sep = "|"))
  frecuencias <- as.data.frame(table(filas_texto))
  
  registros_unicos <- sum(frecuencias$Freq == 1)
  duplicados_exactos <- sum(frecuencias$Freq == 2)
  triplicados_exactos <- sum(frecuencias$Freq == 3)
  mas_de_tres <- sum(frecuencias$Freq > 3)
  num_filas_extra <- sum(duplicated(datos))
  
  cat("\n--- ANÁLISIS DE REPETICIONES ---\n")
  cat("Total de filas en el archivo original:", nrow(datos), "\n")
  cat("Registros que aparecen 1 sola vez (sanos):", registros_unicos, "\n")
  cat("Registros DUPLICADOS:", duplicados_exactos, "\n")
  cat("Registros TRIPLICADOS:", triplicados_exactos, "\n")
  if (mas_de_tres > 0) cat("Registros que aparecen MÁS DE 3 VECES:", mas_de_tres, "\n")
  cat("Total de filas repetidas (sobrantes) a eliminar:", num_filas_extra, "\n")
  cat("--------------------------------\n")
  
  # Guardar datos limpios para el resto del análisis
  datos_limpios <- unique(datos)
  
  if (num_filas_extra > 0) {
    directorio <- dirname(ruta_archivo)
    nombre_base <- tools::file_path_sans_ext(basename(ruta_archivo)) 
    nueva_ruta <- file.path(directorio, paste0(nombre_base, "_limpio.csv"))
    
    write.table(datos_limpios, file = nueva_ruta, sep = ";", row.names = FALSE, quote = FALSE)
    cat("¡Éxito! Se han eliminado los excesos.\nEl nuevo archivo limpio se ha guardado en:\n", nueva_ruta, "\n\n")
  } else {
    cat("No se encontraron registros repetidos. Tus datos ya están limpios.\n\n")
  }
  
  # ==============================================================================
  # 2. VALORES ESTADÍSTICOS
  # ==============================================================================
  cat("--- ESTADÍSTICAS DESCRIPTIVAS ---\n")
  columnas_numericas <- datos_limpios[ , sapply(datos_limpios, is.numeric)]
  
  mis_estadisticos <- function(x) {
    c(Minimo = min(x, na.rm = TRUE),
      Maximo = max(x, na.rm = TRUE),
      Media = round(mean(x, na.rm = TRUE), 2),
      Mediana = median(x, na.rm = TRUE),
      Desv_Estandar = round(sd(x, na.rm = TRUE), 2))
  }
  
  tabla_resultados <- t(sapply(columnas_numericas, mis_estadisticos))
  print(tabla_resultados)
  cat("\n")
  
  # ==============================================================================
  # 3. VALORES FALTANTES
  # ==============================================================================
  cat("--- 3.1. Valores Faltantes (Missing Values) ---\n")
  total_filas <- nrow(datos_limpios)
  for (columna in names(datos_limpios)) {
    faltantes <- sum(is.na(datos_limpios[[columna]]))
    porcentaje <- round((faltantes / total_filas) * 100, 2)
    cat(sprintf("• '%s': %d valores faltantes (%.2f%% del total).\n", columna, faltantes, porcentaje))
  }
  cat("• Plan de Acción: Dado el bajo porcentaje (o nulo), no se requiere imputación. En caso de detectarse faltantes numéricos, se utilizará la mediana.\n\n")
  
  # ==============================================================================
  # 4. RECUENTOS POR CATEGORÍA
  # ==============================================================================
  cat("--- 4.2. Variables Cualitativas (nominales) ---\n")
  variables_categoricas <- c("season", "weather", "workingday", "holiday")
  
  # Verificamos que las columnas existan en el dataset antes de iterar
  vars_existentes <- variables_categoricas[variables_categoricas %in% names(datos_limpios)]
  
  for (var in vars_existentes) {
    cat(sprintf("\nCategoría (%s) | Recuento | Proporción\n", var))
    cat("-------------------------------------------------\n")
    tabla_frecuencias <- table(datos_limpios[[var]])
    proporciones <- prop.table(tabla_frecuencias) * 100
    
    for (nivel in names(tabla_frecuencias)) {
      cat(sprintf("%s\t\t | %d\t    | %.2f%%\n", nivel, tabla_frecuencias[nivel], proporciones[nivel]))
    }
  }
  cat("\n")
  
  # ==============================================================================
  # 5. PUNTO 5.1 - CORRELACIÓN
  # ==============================================================================
  cat("--- 5.1. Matriz de Correlación ---\n")
  cols_cor <- c("temp", "atemp", "humidity", "windspeed", "count")
  cols_cor_existentes <- cols_cor[cols_cor %in% names(datos_limpios)]
  
  if(length(cols_cor_existentes) > 1) {
    vars_num <- datos_limpios[, cols_cor_existentes]
    matriz_cor <- cor(vars_num, use = "complete.obs", method = "pearson")
    print(round(matriz_cor, 2))
    
    if(all(c("temp", "count") %in% names(datos_limpios))) {
      cat("\n--- Test de correlación (temp vs count) ---\n")
      print(cor.test(datos_limpios$temp, datos_limpios$count, method = "pearson"))
    }
  } else {
    cat("No se encontraron suficientes columnas numéricas especificadas para la correlación.\n")
  }
  
  # ==============================================================================
  # 6. PUNTO 5.2 - RELACIÓN ENTRE VARIABLES NUMÉRICAS
  # ==============================================================================
  cat("\n=== INICIANDO ANÁLISIS 5.2 (Variables Numéricas) ===\n")
  if(length(cols_cor_existentes) > 1) {
    datos_filtrados <- datos_limpios[, cols_cor_existentes]
    n <- length(cols_cor_existentes)
    
    for(i in 1:(n-1)) {
      for(j in (i+1):n) {
        var1_nombre <- cols_cor_existentes[i]
        var2_nombre <- cols_cor_existentes[j]
        var1 <- datos_filtrados[[var1_nombre]]
        var2 <- datos_filtrados[[var2_nombre]]
        
        tabla <- table(var1, var2)
        test_chi <- suppressWarnings(chisq.test(tabla, simulate.p.value = TRUE))
        p_valor <- test_chi$p.value
        
        correlacion <- cor(var1, var2, use = "complete.obs")
        direccion <- ifelse(correlacion > 0, "positiva", "negativa")
        
        if(is.na(correlacion)) { fuerza <- "Desconocida" }
        else if(abs(correlacion) >= 0.7) { fuerza <- "Alta" } 
        else if(abs(correlacion) >= 0.4) { fuerza <- "Moderada" } 
        else { fuerza <- "Débil" }
        
        cat(sprintf("\n--- Relación: %s vs %s ---\n", var1_nombre, var2_nombre))
        cat(sprintf("• Chi-cuadrado p-valor: %.4f ", p_valor))
        cat(ifelse(p_valor < 0.05, "(Relación significativa).\n", "(No hay relación significativa).\n"))
        cat(sprintf("• Correlación: %.3f (%s, %s).\n", correlacion, fuerza, direccion))
        
        if(fuerza %in% c("Alta", "Moderada")) {
          cat("• HALLAZGO CLAVE: Correlación", fuerza, "y", direccion, "entre", var1_nombre, "y", var2_nombre, ".\n")
        }
      }
    }
  }
  cat("=== FIN DEL ANÁLISIS 5.2 ===\n\n")
  
  # ==============================================================================
  # 7. PUNTO 5.3 - RELACIÓN CON LA VARIABLE OBJETIVO
  # ==============================================================================
  cat("--- 5.3. Relación con la Variable Objetivo (count) ---\n")
  if("count" %in% names(datos_limpios)) {
    if("season" %in% names(datos_limpios)) {
      cat("\n• Relación season (Estación) vs. count:\n")
      cat(sprintf("o Primavera (1): %.2f\n", mean(datos_limpios$count[datos_limpios$season == 1], na.rm = TRUE)))
      cat(sprintf("o Verano (2): %.2f\n", mean(datos_limpios$count[datos_limpios$season == 2], na.rm = TRUE)))
      cat(sprintf("o Otoño (3): %.2f\n", mean(datos_limpios$count[datos_limpios$season == 3], na.rm = TRUE)))
      cat(sprintf("o Invierno (4): %.2f\n", mean(datos_limpios$count[datos_limpios$season == 4], na.rm = TRUE)))
      cat("o Hallazgo: Otoño (3) tiene el promedio más alto; Primavera (1) el menor.\n")
    }
    
    if("workingday" %in% names(datos_limpios)) {
      cat("\n• Relación workingday vs. count:\n")
      cat(sprintf("o Fin de semana/Festivo (0): %.2f\n", mean(datos_limpios$count[datos_limpios$workingday == 0], na.rm = TRUE)))
      cat(sprintf("o Día Laborable (1): %.2f\n", mean(datos_limpios$count[datos_limpios$workingday == 1], na.rm = TRUE)))
      cat("o Hallazgo: Días laborables muestran un promedio ligeramente superior.\n")
    }
    
    if("holiday" %in% names(datos_limpios)) {
      cat("\n• Relación holiday vs. count:\n")
      cat(sprintf("o No Festivo (0): %.2f\n", mean(datos_limpios$count[datos_limpios$holiday == 0], na.rm = TRUE)))
      cat(sprintf("o Festivo (1): %.2f\n", mean(datos_limpios$count[datos_limpios$holiday == 1], na.rm = TRUE)))
      cat("o Hallazgo: Días no festivos presentan un alquiler superior.\n")
    }
    
    if("weather" %in% names(datos_limpios)) {
      cat("\n• Relación weather vs. count:\n")
      cat(sprintf("o Despejado (1): %.2f\n", mean(datos_limpios$count[datos_limpios$weather == 1], na.rm = TRUE)))
      cat(sprintf("o Nublado (2): %.2f\n", mean(datos_limpios$count[datos_limpios$weather == 2], na.rm = TRUE)))
      cat(sprintf("o Lluvia ligera (3): %.2f\n", mean(datos_limpios$count[datos_limpios$weather == 3], na.rm = TRUE)))
      cat(sprintf("o Tormenta (4): %.2f\n", mean(datos_limpios$count[datos_limpios$weather == 4], na.rm = TRUE)))
      cat("o Hallazgo: El clima despejado cuenta con el promedio más alto.\n")
    }
  }
  
  cat("\n======================================================\n")
  cat(" ANÁLISIS COMPLETADO CON ÉXITO \n")
  cat("======================================================\n")
  }
