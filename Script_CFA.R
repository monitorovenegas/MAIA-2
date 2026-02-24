#------------------------------------------------
# 0. Paquetes
#------------------------------------------------
packs <- c(
  "lavaan", "semTools", "dplyr", "tibble", "psych",
  "openxlsx", "tidyr", "semPlot", "readr"
)

to_install <- packs[!packs %in% rownames(installed.packages())]
if (length(to_install) > 0) install.packages(to_install)
invisible(lapply(packs, library, character.only = TRUE))

#------------------------------------------------
# 1. Datos
#------------------------------------------------
df <- readr::read_csv("/Users/ccortesr/Library/CloudStorage/GoogleDrive-cacorte1@gmail.com/Mi unidad/00 SEMESTRES/2025 - 2do Semestre/PAPERS/Validación MAIA-2 CL/Nuevos Resultados/Base MAIA_AFC 6 factores.csv")

#------------------------------------------------
# 2. Definición de ítems
#------------------------------------------------
items_all <- sprintf("ITEM%02d", 1:37)

items_excluir <- c("ITEM07", "ITEM11")
items_usados  <- setdiff(items_all, items_excluir)

items_reversos <- c("ITEM05","ITEM06","ITEM08","ITEM09","ITEM10","ITEM12","ITEM15")

stopifnot(all(items_usados %in% names(df)))
stopifnot(all(items_reversos %in% names(df)))

#------------------------------------------------
# 3. Preparación: asegurar numéricos + reversión 0–5
#------------------------------------------------
min_scale <- 0L
max_scale <- 5L

df <- df %>%
  mutate(across(all_of(items_usados), ~ as.integer(.))) %>%
  mutate(across(all_of(items_reversos), ~ (max_scale + min_scale) - .))

#------------------------------------------------
# 3b. Promedio y SD por subescala (descriptivos)
#------------------------------------------------
subescalas <- list(
  F1_Awareness       = c("ITEM01","ITEM02","ITEM03","ITEM04"),
  F2_NotDistracting  = c("ITEM05","ITEM06","ITEM08","ITEM09","ITEM10"),
  F3_NotWorrying     = c("ITEM12","ITEM13","ITEM14","ITEM15"),
  F4_AttentionReg    = c("ITEM16","ITEM17","ITEM18","ITEM19","ITEM20","ITEM21","ITEM22"),
  F5_EmotionalAware  = c("ITEM23","ITEM24","ITEM25","ITEM26","ITEM27"),
  F6_SelfRegulation  = c("ITEM28","ITEM29","ITEM30","ITEM31"),
  F7_BodyListening   = c("ITEM32","ITEM33","ITEM34"),
  F8_Trusting        = c("ITEM35","ITEM36","ITEM37")
)

df_subescalas <- tibble::as_tibble(
  lapply(subescalas, function(it) {
    rowMeans(as.data.frame(df[, it]), na.rm = TRUE)
  })
)

descriptivos_subescalas <- df_subescalas %>%
  summarise(across(everything(),
                   list(Media = mean, SD = sd),
                   na.rm = TRUE)) %>%
  tidyr::pivot_longer(
    cols = everything(),
    names_to = c("Subescala","Estadistico"),
    names_sep = "_",
    values_to = "Valor"
  ) %>%
  tidyr::pivot_wider(
    names_from = Estadistico,
    values_from = Valor
  )

#------------------------------------------------
# 4. Declarar ordinal (solo ítems usados)
#------------------------------------------------
df <- df %>%
  mutate(across(all_of(items_usados), ordered))

#------------------------------------------------
# 5. Modelo CFA (8 factores) – SIN ITEM07 e ITEM11
#------------------------------------------------
modelo_8f <- '
F1 =~ ITEM01 + ITEM02 + ITEM03 + ITEM04
F2 =~ ITEM05 + ITEM06 + ITEM08 + ITEM09 + ITEM10
F3 =~ ITEM12 + ITEM13 + ITEM14 + ITEM15
F4 =~ ITEM16 + ITEM17 + ITEM18 + ITEM19 + ITEM20 + ITEM21 + ITEM22
F5 =~ ITEM23 + ITEM24 + ITEM25 + ITEM26 + ITEM27
F6 =~ ITEM28 + ITEM29 + ITEM30 + ITEM31
F7 =~ ITEM32 + ITEM33 + ITEM34
F8 =~ ITEM35 + ITEM36 + ITEM37
'

#------------------------------------------------
# 6. Ajuste CFA ordinal (WLSMV) - SIN missing=... (no hay NA)
#------------------------------------------------
fit <- lavaan::cfa(
  model     = modelo_8f,
  data      = df,
  ordered   = items_usados,
  estimator = "WLSMV",
  std.lv    = TRUE
)

#------------------------------------------------
# 7. Índices de ajuste
#------------------------------------------------
fit_indices <- lavaan::fitMeasures(
  fit,
  c("chisq","df","pvalue","cfi","tli",
    "rmsea","rmsea.ci.lower","rmsea.ci.upper","srmr")
) %>%
  as.data.frame() %>%
  tibble::rownames_to_column("indice") %>%
  dplyr::rename(valor = 2)

#------------------------------------------------
# 8. Cargas estandarizadas
#------------------------------------------------
cargas <- lavaan::standardizedSolution(fit) %>%
  dplyr::filter(op == "=~") %>%
  dplyr::select(Factor = lhs, Item = rhs,
                Carga = est.std, SE = se, z = z, p = pvalue) %>%
  dplyr::arrange(Factor, dplyr::desc(abs(Carga)))

#------------------------------------------------
# 9. Correlaciones entre factores
#------------------------------------------------
cor_latentes <- lavaan::standardizedSolution(fit) %>%
  dplyr::filter(op == "~~", lhs != rhs) %>%
  dplyr::select(Factor1 = lhs, Factor2 = rhs,
                r = est.std, SE = se, z = z, p = pvalue) %>%
  dplyr::arrange(dplyr::desc(abs(r)))

#------------------------------------------------
# 10. Fiabilidad: Omega (CFA) por factor
#------------------------------------------------
omega_obj <- semTools::reliability(fit)

omega_tabla <- data.frame(
  Factor = colnames(omega_obj),
  Omega  = as.numeric(omega_obj["omega", ])
)

#------------------------------------------------
# 11. Puntajes latentes + medias y SD por factor
#------------------------------------------------
scores <- lavaan::lavPredict(fit)

resumen_factores <- as.data.frame(scores) %>%
  summarise(across(everything(),
                   list(media = mean, sd = sd),
                   .names = "{.col}_{.fn}")) %>%
  tidyr::pivot_longer(cols = everything(),
                      names_to = "estadistico",
                      values_to = "valor")

#------------------------------------------------
# 12. Alfa ordinal (policórico) – total y por factor
#------------------------------------------------
df_alpha <- df[, items_usados]

poly_total <- psych::polychoric(df_alpha)$rho
alpha_total_ordinal <- psych::alpha(poly_total)$total

alpha_total_ordinal_tabla <- data.frame(
  escala = "Total",
  tipo   = "Alfa ordinal (policórico)",
  raw_alpha = unname(alpha_total_ordinal["raw_alpha"]),
  std_alpha = unname(alpha_total_ordinal["std.alpha"]),
  average_r = unname(alpha_total_ordinal["average_r"]),
  n_items   = length(items_usados)
)

factores <- list(
  F1 = c("ITEM01","ITEM02","ITEM03","ITEM04"),
  F2 = c("ITEM05","ITEM06","ITEM08","ITEM09","ITEM10"),
  F3 = c("ITEM12","ITEM13","ITEM14","ITEM15"),
  F4 = c("ITEM16","ITEM17","ITEM18","ITEM19","ITEM20","ITEM21","ITEM22"),
  F5 = c("ITEM23","ITEM24","ITEM25","ITEM26","ITEM27"),
  F6 = c("ITEM28","ITEM29","ITEM30","ITEM31"),
  F7 = c("ITEM32","ITEM33","ITEM34"),
  F8 = c("ITEM35","ITEM36","ITEM37")
)

alpha_factores_ordinal_tabla <- do.call(rbind, lapply(names(factores), function(f) {
  it <- factores[[f]]
  poly_f <- psych::polychoric(df[, it])$rho
  tot <- psych::alpha(poly_f)$total
  
  data.frame(
    escala = f,
    tipo   = "Alfa ordinal (policórico)",
    raw_alpha = unname(tot["raw_alpha"]),
    std_alpha = unname(tot["std.alpha"]),
    average_r = unname(tot["average_r"]),
    n_items   = length(it)
  )
}))

alpha_ordinal_tabla <- rbind(alpha_total_ordinal_tabla, alpha_factores_ordinal_tabla)

#------------------------------------------------
# 13. Exportar TODO a Excel
#------------------------------------------------
out_file <- "CFA_MAIA_8F_WLSMV_sin07_sin11_confiabilidad.xlsx"
out_path <- file.path(getwd(), out_file)

wb <- openxlsx::createWorkbook()

openxlsx::addWorksheet(wb, "Fit_Indices")
openxlsx::writeData(wb, "Fit_Indices", fit_indices)

openxlsx::addWorksheet(wb, "Cargas_STD")
openxlsx::writeData(wb, "Cargas_STD", cargas)

openxlsx::addWorksheet(wb, "Cor_Latentes")
openxlsx::writeData(wb, "Cor_Latentes", cor_latentes)

openxlsx::addWorksheet(wb, "Omega")
openxlsx::writeData(wb, "Omega", omega_tabla)

openxlsx::addWorksheet(wb, "Alpha_Ordinal")
openxlsx::writeData(wb, "Alpha_Ordinal", alpha_ordinal_tabla)

openxlsx::addWorksheet(wb, "Scores_Latentes")
openxlsx::writeData(wb, "Scores_Latentes", as.data.frame(scores))

openxlsx::addWorksheet(wb, "Resumen_Scores")
openxlsx::writeData(wb, "Resumen_Scores", resumen_factores)

openxlsx::addWorksheet(wb, "Descriptivos_Subescalas")
openxlsx::writeData(wb, "Descriptivos_Subescalas", descriptivos_subescalas)

openxlsx::saveWorkbook(wb, out_path, overwrite = TRUE)

cat("✅ Listo. Archivo guardado en:", out_path, "\n")

#------------------------------------------------
# 14. Plot
#------------------------------------------------
semPlot::semPaths(
  fit,
  what = "std",
  whatLabels = "std",
  layout = "tree",
  rotation = 2,
  style = "lisrel",
  residuals = FALSE,
  intercepts = FALSE,
  nCharNodes = 0,
  edge.label.cex = 0.9,
  sizeLat = 8,
  sizeMan = 5
)
getwd()

#------------------------------------------------
# 15. Plot 2
#------------------------------------------------
# Visualización del modelo

semPaths(fit, 
         whatLabels = "std",    # Muestra cargas factoriales estandarizadas
         layout = "tree2",      # Distribución similar a la imagen (jerárquica)
         rotation = 1,          # Rota el gráfico para que el factor general esté abajo o arriba
         edge.label.cex = 0.8,  # Tamaño de la fuente de las cargas
         curvePivot = TRUE,     # Curva las líneas para evitar solapamiento
         sizeMan = 4,           # Tamaño de los ítems (cuadrados)
         sizeLat = 6,          # Tamaño de los factores (círculos)
         mar = c(5, 1, 5, 1),   # Márgenes
         theme = "colorblind",     # Estilo visual limpio
         nCharNodes = 0,        # No abreviar nombres de nodos
         shapeLat = "ellipse", 
         edge.color = "black")


#------------------------------------------------
# 16. Plot 3
#------------------------------------------------
# Visualización del modelo
semPaths(fit, 
         # --- 1. DATOS Y ETIQUETAS ---
         whatLabels = "std",       # Qué mostrar en las líneas: "std" (estandarizado), "est" (bruto), "label", "no"
         what = "paths",           # Qué dibujar: "paths" (flechas), "est" (grosores según valor)
         nCharNodes = 0,           # Número de caracteres en nodos: 0 para nombre completo
         nCharEdges = 0,           # Número de caracteres en etiquetas de flechas
         
         # CAMBIO CLAVE: 'label.cex' controla el tamaño de fuente de ítems y factores
         label.cex = 1.5,          # Aumentado a 1.5 (ajusta según prefieras)
         
         # --- 2. ESTRUCTURA Y DISEÑO (LAYOUT) ---
         layout = "tree2",         # Tipo de dibujo: "tree", "tree2", "circle", "spring", "spring2"
         rotation = 2,             # Orientación: 1 (vertical), 2 (horizontal), 3 (invertido), 4 (izq)
         levels = c(1, 2, 2),      # Distancia relativa entre capas (Ítems, Factores, Factor G)
         cardinal = FALSE,         # FALSE: flechas salen en cualquier ángulo; TRUE: solo ejes 90°
         bifactor = "General",     # Nombre del factor global para optimizar el diseño Bi-factor
         
         # --- 3. ESTÉTICA DE NODOS (Círculos y Cuadrados) ---
         shapeLat = "ellipse",     # Forma factores latentes: "ellipse", "circle", "rect"
         shapeMan = "rectangle",   # Forma variables manifiestas (ítems): "rect", "square"
         sizeLat = 6,              # Diámetro/ancho de los factores
         sizeMan = 4,              # Diámetro/ancho de los ítems
         color = "white",          # Color de fondo de los nodos
         border.color = "black",   # Color del borde de los nodos
         border.width = 1.5,       # Grosor del borde de los nodos
         
         # --- 4. ESTÉTICA DE FLECHAS (Edges) ---
         edge.color = "black",     # Color de las líneas
         edge.width = 1,           # Grosor base de las líneas
         asize = 1.5,              # Tamaño de las puntas de las flechas
         curve = 1.5,              # Curvatura general de las líneas (0 es recto)
         curvePivot = TRUE,        # Si TRUE, las líneas se curvan para no chocar con nodos
         fade = FALSE,             # TRUE: líneas más claras si la carga es pequeña; FALSE: color sólido
         directed = TRUE,          # Mostrar flechas de dirección
         
         # --- 5. ELEMENTOS A OCULTAR (Limpieza) ---
         residuals = FALSE,        # Mostrar varianzas residuales (errores de ítems)
         intercepts = FALSE,       # Mostrar interceptos/medias
         thresholds = FALSE,       # Mostrar umbrales (para datos categóricos)
         fixedStyle = 1,           # Estilo de líneas fijadas (1: sólido, 2: guiones)
         freeStyle = 1,            # Estilo de líneas libres
         
         # --- 6. LIENZO Y MÁRGENES ---
         mar = c(2, 8, 2, 8),    # Márgenes del papel: c(abajo, izquierda, arriba, derecha)
         theme = "colorblind",     # Tema predefinido: "default", "gray", "colorblind"
         bg = "white",             # Color de fondo de todo el gráfico
         
         # --- 7. AGRUPACIÓN (Opcional) ---
         # groups = lista_grupos,  # Para colorear por grupos de factores
         legend = FALSE            # Mostrar leyenda de grupos
)
############################################
# FIN
############################################
