############################################
# DETALLES CÓDIGO CP REVISADO 20_12_2025
# CFA ORDINAL (ESTIMADOR: WLSMV) – 8 FACTORES (MAIA)
# Ítems: ITEM01 ... ITEM37
# Ítems EXCLUIDOS: ITEM07, ITEM11
# Ítems REVERSOS (excluyendo 7 y 11):
#   ITEM05, ITEM06, ITEM08, ITEM09, ITEM10, ITEM12, ITEM15
# + Alfa de Cronbach (ORDINAL policórico + clásico opcional)
# + Exporta TODO a Excel
############################################

#------------------------------------------------
# 0. Paquetes
#------------------------------------------------
packs <- c("lavaan", "semTools", "dplyr", "tibble", "psych", "openxlsx","tidyr", "semPlot")
to_install <- packs[!packs %in% rownames(installed.packages())]
if (length(to_install) > 0) install.packages(to_install)
invisible(lapply(packs, library, character.only = TRUE))

#------------------------------------------------
# 1. Datos
#------------------------------------------------
#  Cargar base
library(readr)
Base_MAIA_imputada <- read_csv("C:/Users/clau_/OneDrive/Desktop/MAIA 2024/Base MAIA imputada.csv")

# Renombrar base
df <- Base_MAIA_imputada
names(df)
#------------------------------------------------
# 2. Definición de ítems
#------------------------------------------------
items_all <- sprintf("ITEM%02d", 1:37)

items_excluir <- c("ITEM07", "ITEM11")
items_usados  <- setdiff(items_all, items_excluir)

items_reversos <- c("ITEM05","ITEM06","ITEM08","ITEM09","ITEM10","ITEM12","ITEM15")

# Chequeos 
stopifnot(all(items_usados %in% names(df)))
stopifnot(all(items_reversos %in% names(df)))

#------------------------------------------------
# 3. Reversión (ajusta K según escala)
#------------------------------------------------
df <- df %>%
  mutate(across(all_of(items_reversos), ~ 5 - as.numeric(.)))

#------------------------------------------------
# 3b. Promedio y SD por subescala (descriptivos)
#     (usar ítems ya revertidos)
#------------------------------------------------
subescalas <- list(
  F1_Awareness = c("ITEM01","ITEM02","ITEM03","ITEM04"),
  F2_NotDistracting = c("ITEM05","ITEM06","ITEM08","ITEM09","ITEM10"),
  F3_NotWorrying = c("ITEM12","ITEM13","ITEM14","ITEM15"),
  F4_AttentionReg = c("ITEM16","ITEM17","ITEM18","ITEM19","ITEM20","ITEM21","ITEM22"),
  F5_EmotionalAware = c("ITEM23","ITEM24","ITEM25","ITEM26","ITEM27"),
  F6_SelfRegulation = c("ITEM28","ITEM29","ITEM30","ITEM31"),
  F7_BodyListening = c("ITEM32","ITEM33","ITEM34"),
  F8_Trusting = c("ITEM35","ITEM36","ITEM37")
)

# Puntaje promedio por persona
df_subescalas <- as.data.frame(
  lapply(subescalas, function(it) {
    rowMeans(as.data.frame(lapply(df[, it], as.numeric)), na.rm = TRUE)
  })
)

# Descriptivos finales
descriptivos_subescalas <- df_subescalas %>%
  summarise(
    across(everything(),
           list(Media = mean, SD = sd),
           na.rm = TRUE)
  ) %>%
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
df[items_usados] <- lapply(df[items_usados], ordered)

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
# 6. Ajuste CFA ordinal (WLSMV)
#------------------------------------------------
fit <- cfa(
  model     = modelo_8f,
  data      = df,
  ordered   = items_usados,
  estimator = "WLSMV",
  std.lv    = TRUE,
  missing   = "pairwise"
)

#------------------------------------------------
# 7. Índices de ajuste
#------------------------------------------------
fit_indices <- fitMeasures(
  fit,
  c("chisq","df","pvalue","cfi","tli",
    "rmsea","rmsea.ci.lower","rmsea.ci.upper","srmr")
) %>%
  as.data.frame() %>%
  tibble::rownames_to_column("indice") %>%
  dplyr::rename(valor = 2)

#------------------------------------------------
# 8. Cargas estandarizadas (tabla)
#------------------------------------------------
cargas <- standardizedSolution(fit) %>%
  dplyr::filter(op == "=~") %>%
  dplyr::select(Factor = lhs, Item = rhs,
                Carga = est.std, SE = se, z = z, p = pvalue) %>%
  dplyr::arrange(Factor, dplyr::desc(abs(Carga)))

#------------------------------------------------
# 9. Correlaciones entre factores (tabla)
#------------------------------------------------
cor_latentes <- standardizedSolution(fit) %>%
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
scores <- lavPredict(fit)  # matriz con columnas F1..F8

resumen_factores <- as.data.frame(scores) %>%
  summarise(across(everything(),
                   list(media = mean, sd = sd),
                   .names = "{.col}_{.fn}")) %>%
  tidyr::pivot_longer(cols = everything(),
                      names_to = "estadistico",
                      values_to = "valor")

#------------------------------------------------
# 12. Alfa de Cronbach (ORDINAL policórico) – total y por factor
#------------------------------------------------
# Subconjunto de datos para alfa (solo ítems usados)
df_alpha <- df[, items_usados]

# 12a) Alfa total ordinal (policórico)
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

# 12b) Alfa ordinal por factor
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

alpha_factores_ordinal_tabla <- do.call(rbind, lapply(names(factores), function(f){
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
archivo_salida <- "CFA_MAIA_8F_WLSMV_sin07_sin11_confiabilidad.xlsx"

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

openxlsx::saveWorkbook(wb, archivo_salida, overwrite = TRUE)

cat("✅ Listo. Archivo guardado como:", archivo_salida, "\n")

getwd()

#14. Plotear
library(semPlot)

semPaths(
  fit,
  what = "std",          # coeficientes estandarizados
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

############################################
# FIN
############################################
