examen <- function() {
  # Variable para almacenar los datos
  datos_limpios <- NULL
  
  # Función auxiliar para pedir columnas por consola
  pedir_columnas <- function(mensaje, opciones_validas) {
    cat("\n>>", mensaje, "\n")
    cat("Columnas disponibles:", paste(opciones_validas, collapse = ", "), "\n")
    entrada <- readline(prompt = "Escribe los nombres separados por comas (o 'todas'): ")
    
    if (tolower(trimws(entrada)) == "todas") {
      return(opciones_validas)
    }
    
    cols_elegidas <- trimws(unlist(strsplit(entrada, ",")))
    cols_validas <- cols_elegidas[cols_elegidas %in% opciones_validas]
    
    if (length(cols_validas) == 0) {
      cat("⚠️ Ninguna columna válida introducida. Inténtalo de nuevo.\n")
    } else if (length(cols_validas) < length(cols_elegidas)) {
      cat("⚠️ Se omitieron algunas columnas porque no existen en los datos.\n")
    }
    
    return(cols_validas)
  }

  # Bucle principal del menú
  while (TRUE) {
    cat("\n======================================================\n")
    cat("                 MENÚ DE ANÁLISIS DE DATOS            \n")
    cat("======================================================\n")
    cat("1. Carga y limpieza de datos (Borrar duplicados)\n")
    cat("2. Valores estadísticos y visualización de distribución\n")
    cat("3. Valores faltantes\n")
    cat("4. Recuentos por categoría y visualización\n")
    cat("5. Matriz de correlación\n")
    cat("6. Relación entre variables numéricas\n")
    cat("7. Relación con la variable objetivo\n")
    cat("8. Normalizar (Clasificar numérica en intervalos)\n")
    cat("0. Salir\n")
    cat("======================================================\n")
    
    opcion <- readline(prompt = "Elige una opción (0-8): ")
    
    if (opcion == "0") {
      cat("¡Saliendo del programa! Hasta pronto.\n")
      break
    }
    
    # Validar opción
    if (!(opcion %in% as.character(1:8))) {
      cat("⚠️ Opción no válida. Por favor, elige un número del 0 al 8.\n")
      next
    }
    
    # ==============================================================================
    # CARGA DE DATOS CENTRALIZADA (Se ejecuta en cualquier opción del 1 al 8)
    # ==============================================================================
    cat("\nPor favor, selecciona el archivo CSV a procesar en la ventana emergente...\n")
    ruta_archivo <- file.choose()
    
    datos_limpios <- read.csv(ruta_archivo, sep = ";", header = TRUE, stringsAsFactors = FALSE)
    if(ncol(datos_limpios) == 1) { 
      datos_limpios <- read.csv(ruta_archivo, sep = ",", header = TRUE, stringsAsFactors = FALSE) 
    }
    
    # Conversión silenciosa de comas a puntos para forzar numéricos en los análisis
    for (col in names(datos_limpios)) {
      if (is.character(datos_limpios[[col]])) {
        intento_num <- suppressWarnings(as.numeric(gsub(",", ".", datos_limpios[[col]])))
        # Si al convertir no se rompe la columna llenándose de NAs, aplicamos el cambio
        if (!all(is.na(intento_num) & !is.na(datos_limpios[[col]]))) {
          datos_limpios[[col]] <- intento_num
        }
      }
    }
    
    # ==============================================================================
    # 1. ANÁLISIS DE REPETICIONES Y LIMPIEZA
    # ==============================================================================
    if (opcion == "1") {
      filas_texto <- do.call(paste, c(datos_limpios, sep = "|"))
      frecuencias <- as.data.frame(table(filas_texto))
      
      registros_unicos <- sum(frecuencias$Freq == 1)
      duplicados_exactos <- sum(frecuencias$Freq == 2)
      triplicados_exactos <- sum(frecuencias$Freq == 3)
      mas_de_tres <- sum(frecuencias$Freq > 3)
      num_filas_extra <- sum(duplicated(datos_limpios))
      
      cat("\n--- ANÁLISIS DE REPETICIONES ---\n")
      cat("Total de filas:", nrow(datos_limpios), "\n")
      cat("Registros únicos:", registros_unicos, "\n")
      cat("Duplicados:", duplicados_exactos, "\n")
      cat("Triplicados:", triplicados_exactos, "\n")
      if (mas_de_tres > 0) cat("Más de 3 veces:", mas_de_tres, "\n")
      cat("Total sobrantes a eliminar:", num_filas_extra, "\n")
      cat("--------------------------------\n")
      
      # Eliminar duplicados
      datos_limpios <- unique(datos_limpios)
      
      if (num_filas_extra > 0) {
        directorio <- dirname(ruta_archivo)
        nombre_base <- tools::file_path_sans_ext(basename(ruta_archivo)) 
        nueva_ruta <- file.path(directorio, paste0(nombre_base, "_limpio.csv"))
        write.table(datos_limpios, file = nueva_ruta, sep = ";", row.names = FALSE, quote = FALSE)
        cat("¡Éxito! Archivo limpio y sin duplicados guardado en:\n", nueva_ruta, "\n")
      } else {
        cat("Los datos ya estaban limpios (sin filas duplicadas).\n")
      }
    }
    
    # ==============================================================================
    # 2. ESTADÍSTICAS Y DISTRIBUCIÓN
    # ==============================================================================
    else if (opcion == "2") {
      cat("\n--- ESTADÍSTICAS DESCRIPTIVAS ---\n")
      cols_solo_num <- names(datos_limpios)[sapply(datos_limpios, is.numeric)]
      cols_num <- pedir_columnas("¿Qué columnas numéricas quieres analizar?", cols_solo_num)
      
      if (length(cols_num) > 0) {
        datos_num <- datos_limpios[, cols_num, drop = FALSE]
        
        mis_estadisticos <- function(x) {
          c(Minimo = min(x, na.rm = TRUE),
            Maximo = max(x, na.rm = TRUE),
            Media = round(mean(x, na.rm = TRUE), 2),
            Mediana = median(x, na.rm = TRUE),
            Desv_Estandar = round(sd(x, na.rm = TRUE), 2))
        }
        
        tabla_resultados <- t(sapply(datos_num, mis_estadisticos))
        print(tabla_resultados)
        
        cat("\n=> Generando gráficos. Cierra el gráfico para continuar o pulsa ENTER si R te lo pide.\n")
        old_par <- par(ask = TRUE, mfrow = c(1, 2))
        
        for (col in cols_num) {
          hist(datos_num[[col]], main = paste("Distribución:", col), xlab = col, col = "lightblue", border = "black")
          boxplot(datos_num[[col]], main = paste("Boxplot:", col), ylab = col, col = "lightgreen", outcol = "red", outpch = 19)
        }
        par(old_par)
      }
    }
    
    # ==============================================================================
    # 3. VALORES FALTANTES
    # ==============================================================================
    else if (opcion == "3") {
      cat("\n--- VALORES FALTANTES ---\n")
      valor_na <- readline(prompt = "¿Qué texto/valor representa los faltantes? (Deja vacío para considerar solo los 'NA' de R): ")
      valor_na <- trimws(valor_na)
      
      total_filas <- nrow(datos_limpios)
      for (columna in names(datos_limpios)) {
        if (valor_na == "") {
          # Si se deja vacío, solo contamos los NA por defecto
          faltantes <- sum(is.na(datos_limpios[[columna]]))
        } else {
          # Si el usuario introdujo un valor (ej: "?"), buscamos ese valor y también los NA nativos por seguridad
          faltantes <- sum(is.na(datos_limpios[[columna]]) | datos_limpios[[columna]] == valor_na, na.rm = TRUE)
        }
        
        porcentaje <- round((faltantes / total_filas) * 100, 2)
        cat(sprintf("• '%s': %d valores faltantes (%.2f%% del total).\n", columna, faltantes, porcentaje))
      }
    }
    
    # ==============================================================================
    # 4. RECUENTOS CATEGÓRICOS
    # ==============================================================================
    else if (opcion == "4") {
      cat("\n--- VARIABLES CUALITATIVAS ---\n")
      cols_cat <- pedir_columnas("¿Qué variables categóricas deseas visualizar?", names(datos_limpios))
      
      if (length(cols_cat) > 0) {
        old_par <- par(ask = TRUE, mfrow = c(1, 2))
        for (var in cols_cat) {
          cat(sprintf("\nCategoría (%s) | Recuento | Proporción\n", var))
          cat("-------------------------------------------------\n")
          tabla_frecuencias <- table(datos_limpios[[var]])
          proporciones <- prop.table(tabla_frecuencias) * 100
          
          for (nivel in names(tabla_frecuencias)) {
            cat(sprintf("%s\t\t | %d\t    | %.2f%%\n", nivel, tabla_frecuencias[nivel], proporciones[nivel]))
          }
          
          barplot(tabla_frecuencias, main = paste("Barras:", var), col = "coral", xlab = var, ylab = "Frecuencia")
          pie(tabla_frecuencias, main = paste("Proporción:", var), col = rainbow(length(tabla_frecuencias)))
        }
        par(old_par)
      }
    }
    
    # ==============================================================================
    # 5. MATRIZ DE CORRELACIÓN
    # ==============================================================================
    else if (opcion == "5") {
      cat("\n--- MATRIZ DE CORRELACIÓN ---\n")
      cols_solo_num <- names(datos_limpios)[sapply(datos_limpios, is.numeric)]
      cols_cor <- pedir_columnas("¿Qué variables numéricas quieres incluir en la correlación?", cols_solo_num)
      
      if(length(cols_cor) > 1) {
        vars_num <- datos_limpios[, cols_cor, drop = FALSE]
        matriz_cor <- cor(vars_num, use = "complete.obs", method = "pearson")
        print(round(matriz_cor, 2))
      } else {
        cat("⚠️ Necesitas al menos 2 columnas numéricas para una correlación.\n")
      }
    }
    
    # ==============================================================================
    # 6. RELACIÓN ENTRE VARIABLES NUMÉRICAS
    # ==============================================================================
    else if (opcion == "6") {
      cat("\n--- RELACIÓN ENTRE VARIABLES NUMÉRICAS ---\n")
      cols_solo_num <- names(datos_limpios)[sapply(datos_limpios, is.numeric)]
      cols_rel <- pedir_columnas("¿Qué variables numéricas quieres comparar entre sí?", cols_solo_num)
      
      if(length(cols_rel) > 1) {
        datos_filtrados <- datos_limpios[, cols_rel, drop = FALSE]
        n <- length(cols_rel)
        
        for(i in 1:(n-1)) {
          for(j in (i+1):n) {
            var1_nombre <- cols_rel[i]
            var2_nombre <- cols_rel[j]
            var1 <- datos_filtrados[[var1_nombre]]
            var2 <- datos_filtrados[[var2_nombre]]
            
            tabla <- table(var1, var2)
            test_chi <- suppressWarnings(chisq.test(tabla, simulate.p.value = TRUE))
            
            correlacion <- cor(var1, var2, use = "complete.obs")
            direccion <- ifelse(correlacion > 0, "positiva", "negativa")
            
            if(is.na(correlacion)) { fuerza <- "Desconocida" }
            else if(abs(correlacion) >= 0.7) { fuerza <- "Alta" } 
            else if(abs(correlacion) >= 0.4) { fuerza <- "Moderada" } 
            else { fuerza <- "Débil" }
            
            cat(sprintf("\n--- %s vs %s ---\n", var1_nombre, var2_nombre))
            cat(sprintf("• Chi-cuadrado p-valor: %.4f ", test_chi$p.value))
            cat(ifelse(test_chi$p.value < 0.05, "(Significativa).\n", "(No significativa).\n"))
            cat(sprintf("• Correlación: %.3f (%s, %s).\n", correlacion, fuerza, direccion))
          }
        }
      } else {
        cat("⚠️ Necesitas al menos 2 columnas.\n")
      }
    }
    
    # ==============================================================================
    # 7. RELACIÓN CON LA VARIABLE OBJETIVO
    # ==============================================================================
    else if (opcion == "7") {
      cat("\n--- RELACIÓN CON VARIABLE OBJETIVO ---\n")
      target <- readline(prompt = "¿Cuál es el nombre de tu variable objetivo (ej. count)? ")
      
      if (target %in% names(datos_limpios)) {
        cols_cat_target <- pedir_columnas("¿Con qué variables categóricas quieres agruparla?", names(datos_limpios))
        
        for (cat_var in cols_cat_target) {
          cat(sprintf("\n• Promedio de '%s' según '%s':\n", target, cat_var))
          agrupado <- aggregate(datos_limpios[[target]] ~ datos_limpios[[cat_var]], FUN = mean, na.rm = TRUE)
          colnames(agrupado) <- c(cat_var, paste("Media de", target))
          print(agrupado, row.names = FALSE)
        }
      } else {
        cat("⚠️ La variable objetivo introducida no existe en el archivo seleccionado.\n")
      }
    }
    
    # ==============================================================================
    # 8. NORMALIZAR / CLASIFICAR EN INTERVALOS (Personalizado)
    # ==============================================================================
    else if (opcion == "8") {
      cat("\n--- CLASIFICAR VARIABLE NUMÉRICA ---\n")
      cols_solo_num <- names(datos_limpios)[sapply(datos_limpios, is.numeric)]
      
      if (length(cols_solo_num) == 0) {
        cat("⚠️ No hay variables numéricas en el dataset para clasificar.\n")
      } else {
        cat("Columnas numéricas disponibles:", paste(cols_solo_num, collapse = ", "), "\n")
        var_norm <- readline(prompt = "¿Qué variable numérica quieres clasificar? ")
        var_norm <- trimws(var_norm)
        
        if (var_norm %in% cols_solo_num) {
          
          # 1. Pedir el nombre de la nueva columna
          nueva_columna <- readline(prompt = sprintf("¿Qué nombre quieres ponerle a la nueva variable? (Deja en blanco para usar '%s_clasificada'): ", var_norm))
          nueva_columna <- trimws(nueva_columna)
          if (nueva_columna == "") {
            nueva_columna <- paste0(var_norm, "_clasificada")
          }
          
          # 2. Pedir el número de intervalos
          n_grupos_str <- readline(prompt = "¿En cuántas categorías o intervalos quieres dividirla? (ej. 3, 4, 5...): ")
          n_grupos <- suppressWarnings(as.integer(trimws(n_grupos_str)))
          
          if (is.na(n_grupos) || n_grupos < 2) {
            cat("⚠️ El número de grupos debe ser un número entero mayor o igual a 2. Operación cancelada.\n")
          } else {
            
            # 3. Pedir el nombre de las etiquetas/categorías
            cat(sprintf("\nVas a dividir los datos en %d categorías.\n", n_grupos))
            etiquetas_str <- readline(prompt = "Escribe los nombres de las categorías separados por comas (o deja en blanco para automático): ")
            etiquetas_str <- trimws(etiquetas_str)
            
            if (etiquetas_str == "") {
              mis_etiquetas <- paste("Grupo", 1:n_grupos)
              if (n_grupos == 3) {
                mis_etiquetas <- c("Bajo", "Medio", "Alto") # Automático clásico si son 3
              }
            } else {
              mis_etiquetas <- trimws(unlist(strsplit(etiquetas_str, ",")))
              # Validar que introdujo la cantidad correcta de nombres
              if (length(mis_etiquetas) != n_grupos) {
                cat(sprintf("⚠️ Has introducido %d nombres, pero pediste %d grupos. Usaremos nombres automáticos por seguridad.\n", length(mis_etiquetas), n_grupos))
                mis_etiquetas <- paste("Grupo", 1:n_grupos)
              }
            }
            
            # 4. Clasificar los datos
            datos_limpios[[nueva_columna]] <- cut(
              datos_limpios[[var_norm]], 
              breaks = n_grupos, 
              labels = mis_etiquetas,
              include.lowest = TRUE
            )
            
            cat(sprintf("\n¡Éxito! Se ha creado la columna '%s' dividida en %d grupos.\n", nueva_columna, n_grupos))
            cat("Resumen de la nueva distribución:\n")
            cat("-------------------------------------------------\n")
            
            tabla_clasif <- table(datos_limpios[[nueva_columna]], useNA = "ifany")
            print(tabla_clasif)
            
            # 5. Generar y guardar automáticamente el nuevo archivo CSV
            directorio <- dirname(ruta_archivo)
            nombre_base <- tools::file_path_sans_ext(basename(ruta_archivo)) 
            nueva_ruta <- file.path(directorio, paste0(nombre_base, "_clasificado.csv"))
            write.table(datos_limpios, file = nueva_ruta, sep = ";", row.names = FALSE, quote = FALSE)
            
            cat(sprintf("\n✅ ¡Archivo actualizado con éxito! Se ha guardado un nuevo CSV con la columna extra en:\n%s\n", nueva_ruta))
          }
        } else {
          cat("⚠️ La variable introducida no es válida o no es numérica.\n")
        }
      }
    }
  }
}
