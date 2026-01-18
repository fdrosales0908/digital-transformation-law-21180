# =====================================================
# Digital Government Indicators Study 2023 (ref. 2022)
# Quarto script: load + clean + charts + index
# (Does not print charts; it leaves objects ready to use in the .qmd)
# =====================================================

# -----------------------------
# 0) Libraries
# -----------------------------
library(readxl)
library(dplyr)
library(janitor)
library(stringr)
library(readr)
library(tidyr)
library(ggplot2)
library(forcats)
library(scales)

# -----------------------------
# 1) Data
# -----------------------------
ruta <- "data/bbdd_estudio_indicadores.xlsx"
datos_raw <- read_excel(ruta) %>% clean_names()

# -----------------------------
# 2) Early selection (analysis variables)
# -----------------------------
vars_originales <- c(
  "tipo",
  "P2.GC.1","P2.M.1.1","P2.M.1.2",
  "P2.6a.1","P2.6a.2",
  "P2.2",
  "P2.7.3","P2.7.5",
  "P3.1","P4.1","P5.5","P5.8","P7.3","P8.2",
  "P9.1.1.1","P9.1.1.2","P9.1.1.3","P9.1.1.5",
  "P9.1.2.1","P9.1.2.2","P9.1.2.3","P9.1.2.4","P9.1.2.5",
  "P9.1.6.1","P9.1.6.2","P9.1.6.3","P9.1.6.4","P9.1.6.5",
  "P9.3.3","P9.4","P9.5","P9.7","P9.9","P9.10","P9.12","P9.14",
  "P10.4","P10.6.1","P10.6.2","P10.6.3","P10.6.4","P10.6.5","P10.6.6","P10.7",
  "P11.1.3","P11.7",
  "P12.1","P12.2.1","P12.2.2","P12.3","P12.4","P12.5"
)

to_clean_code <- function(x) tolower(gsub("\\.", "_", x))
vars_clean <- unique(c("tipo", sapply(vars_originales, to_clean_code)))

datos <- datos_raw %>% select(any_of(vars_clean))

# -----------------------------
# 3) Types of variables
# -----------------------------
vars_numericas <- c(
  "p2_gc_1","p2_m_1_1","p2_m_1_2",
  "p2_6a_1","p2_6a_2",
  "p2_7_3","p2_7_5",
  "p9_1_1_1","p9_1_1_2","p9_1_1_3","p9_1_1_5",
  "p9_1_2_1","p9_1_2_2","p9_1_2_3","p9_1_2_4","p9_1_2_5",
  "p9_1_6_1","p9_1_6_2","p9_1_6_3","p9_1_6_4","p9_1_6_5"
)

vars_sino <- c(
  "p2_2",
  "p3_1","p4_1","p5_5","p8_2",
  "p9_4","p9_5","p9_7","p9_10","p9_12","p9_14",
  "p10_4",
  "p10_6_1","p10_6_2","p10_6_3","p10_6_4","p10_6_5","p10_6_6",
  "p10_7",
  "p11_1_3","p11_7",
  "p12_1","p12_2_1","p12_2_2","p12_3","p12_4","p12_5"
)

vars_internet_fibra <- "p9_3_3"

# -----------------------------
# 4) Robust cleaning helpers
# -----------------------------
limpia_a_na <- function(x) {
  x <- as.character(x)
  x <- str_squish(x)
  x <- str_to_lower(x)
  
  # remove accents (simple)
  x <- str_replace_all(x, fixed("á"), "a")
  x <- str_replace_all(x, fixed("é"), "e")
  x <- str_replace_all(x, fixed("í"), "i")
  x <- str_replace_all(x, fixed("ó"), "o")
  x <- str_replace_all(x, fixed("ú"), "u")
  
  # special codes
  x <- na_if(x, "#s.i.")
  x <- na_if(x, "s.i.")
  x <- na_if(x, "sin informacion")
  x <- na_if(x, "sin informacion.")
  
  x <- na_if(x, "#n.a.")
  x <- na_if(x, "n.a.")
  x <- na_if(x, "no aplica")
  
  x <- na_if(x, "#n.s.")
  x <- na_if(x, "n.s.")
  x <- na_if(x, "no sabe")
  x <- na_if(x, "ns")
  
  x <- na_if(x, "")
  x
}

# -----------------------------
# 5) Numeric conversion / yes-no (binary)
# -----------------------------
datos <- datos %>%
  mutate(across(any_of(vars_numericas), ~ parse_number(limpia_a_na(.))))

datos <- datos %>%
  mutate(across(any_of(c(vars_sino, vars_internet_fibra)), ~ {
    x <- limpia_a_na(.)
    case_when(
      is.na(x) ~ NA_real_,
      x %in% c("si","sí","s","1","true") ~ 1,
      x %in% c("no","n","0","false") ~ 0,
      str_detect(x, "fibra") ~ 1,
      TRUE ~ NA_real_
    )
  }))

# -----------------------------
# 6) Derived variables (in code)
# -----------------------------
datos <- datos %>%
  mutate(
    total_funcionarios = rowSums(cbind(p2_gc_1, p2_m_1_1, p2_m_1_2), na.rm = TRUE),
    total_funcionarios_ti = rowSums(cbind(p2_6a_1, p2_6a_2), na.rm = TRUE),
    funcionarios_formacion_ti = rowSums(cbind(p2_7_3, p2_7_5), na.rm = TRUE),
    
    proporcion_ti_en_institucion = ifelse(total_funcionarios > 0,
                                          total_funcionarios_ti / total_funcionarios,
                                          NA_real_),
    proporcion_ti_formacion = ifelse(total_funcionarios_ti > 0,
                                     funcionarios_formacion_ti / total_funcionarios_ti,
                                     NA_real_)
  )

# -----------------------------
# 7) Rename to analysis-friendly names
# -----------------------------
df <- datos %>%
  rename(
    tipo_institucion = tipo,
    
    funcionarios_gc = p2_gc_1,
    funcionarios_m_1_1 = p2_m_1_1,
    funcionarios_m_1_2 = p2_m_1_2,
    
    ti_mujeres = p2_6a_1,
    ti_hombres = p2_6a_2,
    tiene_area_ti = p2_2,
    
    ti_formacion_tic_no_uni = p2_7_3,
    ti_formacion_ciencia_tec_uni = p2_7_5,
    
    plan_ley_21180 = p3_1,
    firma_electronica_avanzada = p4_1,
    politica_gestion_documental = p5_5,
    
    formato_expedientes = p5_8,
    mecanismo_intercambio_info = p7_3,
    plan_mejora_plataformas = p8_2,
    
    pc_escritorio_menos_3 = p9_1_1_1,
    pc_escritorio_3a5 = p9_1_1_2,
    pc_escritorio_mas_5 = p9_1_1_3,
    pc_escritorio_total = p9_1_1_5,
    
    portatil_menos_3 = p9_1_2_1,
    portatil_3a5 = p9_1_2_2,
    portatil_mas_5 = p9_1_2_3,
    portatil_sin_info = p9_1_2_4,
    portatil_total = p9_1_2_5,
    
    servidores_menos_3 = p9_1_6_1,
    servidores_3a5 = p9_1_6_2,
    servidores_mas_5 = p9_1_6_3,
    servidores_sin_info = p9_1_6_4,
    servidores_total = p9_1_6_5,
    
    internet_fibra_optica = p9_3_3,
    tiene_vpn = p9_4,
    data_center_propio = p9_5,
    data_center_externo = p9_7,
    tipo_servicio_data_center = p9_9,
    data_center_en_chile = p9_10,
    nube_donde_datos = p9_12,
    lineamientos_tecnicos_software = p9_14,
    
    app_movil_usuarios = p10_4,
    diseno_auditoria_accesibilidad = p10_6_1,
    diseno_investigacion_cualitativa = p10_6_2,
    diseno_investigacion_cuantitativa = p10_6_3,
    diseno_prototipos_testeo = p10_6_4,
    diseno_pruebas_usabilidad = p10_6_5,
    diseno_ninguna = p10_6_6,
    mide_satisfaccion_usuaria = p10_7,
    
    politica_seguridad_ciber = p11_1_3,
    inventario_activos_info = p11_7,
    
    publica_datos_abiertos_2022 = p12_1,
    datos_abiertos_portal_propio = p12_2_1,
    datos_abiertos_datos_gob_cl = p12_2_2,
    estrategia_datos = p12_3,
    anonimiza_datos = p12_4,
    area_gestion_datos = p12_5
  )

# -----------------------------
# 8) Translation dictionaries + English factor
# -----------------------------
tipo_dict <- c(
  "Gobierno Central"   = "Central Government",
  "Educación Superior" = "Higher Education",
  "Municipalidades"    = "Municipalities",
  "Nivel regional"     = "Regional level",
  "Nivel Regional"     = "Regional level"
)

tipo_order_en <- c("Central Government", "Higher Education", "Municipalities", "Regional level")

df <- df %>%
  mutate(
    tipo_institucion_en = recode(tipo_institucion, !!!tipo_dict),
    tipo_institucion_en = factor(tipo_institucion_en, levels = tipo_order_en)
  )

# IMPORTANT: use this one for plots
df_en <- df

# -----------------------------
# 9) Colors (MUST match English labels)
# -----------------------------
pal_tipo <- c(
  "Central Government" = "#1b9e77",
  "Higher Education"   = "#7570b3",
  "Municipalities"     = "#d95f02",
  "Regional level"     = "#e7298a"
)

# =====================================================
# 10) Index (create BEFORE summaries)
# =====================================================
df_en <- df_en %>%
  mutate(
    ti_norm = (total_funcionarios_ti - min(total_funcionarios_ti, na.rm = TRUE)) /
      (max(total_funcionarios_ti, na.rm = TRUE) - min(total_funcionarios_ti, na.rm = TRUE))
  ) %>%
  mutate(
    D1_capacidad_institucional = rowMeans(cbind(proporcion_ti_en_institucion, proporcion_ti_formacion), na.rm = TRUE),
    D2_capacidad_ti = rowMeans(cbind(tiene_area_ti, ti_norm), na.rm = TRUE),
    D3_ley_21180 = rowMeans(cbind(plan_ley_21180, firma_electronica_avanzada, politica_gestion_documental, plan_mejora_plataformas), na.rm = TRUE),
    D4_infraestructura = rowMeans(cbind(internet_fibra_optica, tiene_vpn, data_center_propio, data_center_externo, lineamientos_tecnicos_software), na.rm = TRUE),
    D5_datos = rowMeans(cbind(publica_datos_abiertos_2022, estrategia_datos, anonimiza_datos, area_gestion_datos), na.rm = TRUE)
  ) %>%
  mutate(
    indice_madurez_digital = rowMeans(
      cbind(D1_capacidad_institucional, D2_capacidad_ti, D3_ley_21180, D4_infraestructura, D5_datos),
      na.rm = TRUE
    ) * 100
  )

# =====================================================
# 11) Graph objects (English labels)
# =====================================================

# D1
p_tamano <- ggplot(df_en,
                   aes(x = fct_reorder(tipo_institucion_en, total_funcionarios, median, na.rm = TRUE),
                       y = total_funcionarios,
                       fill = tipo_institucion_en)) +
  geom_boxplot(alpha = 0.8, outlier.alpha = 0.4) +
  scale_y_log10(labels = comma) +
  scale_fill_manual(values = pal_tipo) +
  coord_flip() +
  labs(
    title = "Institution size by type",
    subtitle = "Total staff (log scale)",
    x = "",
    y = "Number of employees"
  ) +
  theme_minimal() +
  theme(legend.position = "none")

# D2
p_prop_ti <- ggplot(
  df_en %>% filter(!is.na(proporcion_ti_en_institucion)),
  aes(x = tipo_institucion_en, y = proporcion_ti_en_institucion, fill = tipo_institucion_en)
) +
  geom_violin(trim = TRUE, alpha = 0.85) +
  stat_summary(fun = median, geom = "point", size = 3, color = "black") +
  scale_y_continuous(limits = c(0, 0.06), labels = percent_format(accuracy = 0.1)) +
  scale_fill_manual(values = pal_tipo) +
  coord_flip() +
  labs(
    title = "Share of IT staff by institution type",
    subtitle = "Violin plot + median (black dot)",
    x = "",
    y = "% of total staff"
  ) +
  theme_minimal() +
  theme(legend.position = "none")

# D3

df_en <- df_en %>%
  mutate(
    flag_formacion_inconsistente = !is.na(proporcion_ti_formacion) & proporcion_ti_formacion > 1,
    proporcion_ti_formacion_adj = case_when(
      is.na(proporcion_ti_formacion) ~ NA_real_,
      proporcion_ti_formacion < 0 ~ NA_real_,
      TRUE ~ pmin(proporcion_ti_formacion, 1)
    )
  )


p_formacion <- ggplot(
  df_en %>% filter(!is.na(proporcion_ti_formacion_adj)),
  aes(x = tipo_institucion_en, y = proporcion_ti_formacion_adj, fill = tipo_institucion_en)
) +
  geom_violin(trim = TRUE, alpha = 0.85) +
  stat_summary(fun = median, geom = "point", size = 3, color = "black") +
  scale_y_continuous(labels = percent_format(accuracy = 1), limits = c(0, 1)) +
  scale_fill_manual(values = pal_tipo) +
  coord_flip() +
  labs(
    title = "IT staff training by institution type",
    subtitle = "Share of IT staff with related training (capped at 100% when inconsistent)",
    x = "",
    y = "% of IT staff"
  ) +
  theme_minimal() +
  theme(legend.position = "none")

df_en %>% summarise(n_inconsistentes = sum(flag_formacion_inconsistente, na.rm = TRUE))





# D4 (Law 21,180 heatmap)
ley_dict <- c(
  "Plan_21180"         = "Law 21,180 plan",
  "Firma_Electronica"  = "Advanced e-signature",
  "Gestion_Documental" = "E-document management",
  "Mejora_Plataformas" = "Platform improvement"
)

ley_heat <- df_en %>%
  group_by(tipo_institucion_en) %>%
  summarise(
    Plan_21180 = mean(plan_ley_21180, na.rm = TRUE),
    Firma_Electronica = mean(firma_electronica_avanzada, na.rm = TRUE),
    Gestion_Documental = mean(politica_gestion_documental, na.rm = TRUE),
    Mejora_Plataformas = mean(plan_mejora_plataformas, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  pivot_longer(-tipo_institucion_en, names_to = "dimension", values_to = "percent") %>%
  mutate(dimension = recode(dimension, !!!ley_dict))

p_ley <- ggplot(
  ley_heat,
  aes(
    x = dimension,
    y = fct_reorder(tipo_institucion_en, percent, mean),
    fill = percent
  )
) +
  geom_tile(color = "white") +
  scale_fill_gradient(low = "#fde0dd", high = "#08519c", labels = percent) +
  scale_x_discrete(labels = \(x) str_wrap(x, width = 14)) +  # <- WRAP
  labs(
    title = "Implementation of Law 21,180",
    subtitle = "Share of institutions by type",
    x = "",
    y = ""
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 20, hjust = 1, vjust = 1), # <- ROTATE
    plot.margin = margin(t = 10, r = 10, b = 20, l = 10)          # <- SPACE
  )
# D5 (Infrastructure heatmap)
infra_dict <- c(
  "Fibra"           = "Fiber internet",
  "VPN"             = "VPN",
  "DC_propio"       = "Own data center",
  "DC_externo"      = "External data center",
  "Lineamientos_SW" = "Software guidelines"
)

infra_heat <- df_en %>%
  group_by(tipo_institucion_en) %>%
  summarise(
    Fibra = mean(internet_fibra_optica, na.rm = TRUE),
    VPN = mean(tiene_vpn, na.rm = TRUE),
    DC_propio = mean(data_center_propio, na.rm = TRUE),
    DC_externo = mean(data_center_externo, na.rm = TRUE),
    Lineamientos_SW = mean(lineamientos_tecnicos_software, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  pivot_longer(-tipo_institucion_en, names_to = "component", values_to = "percent") %>%
  mutate(component = recode(component, !!!infra_dict))

p_infra <- ggplot(
  infra_heat,
  aes(
    x = component,
    y = fct_reorder(tipo_institucion_en, percent, mean),
    fill = percent
  )
) +
  geom_tile(color = "white") +
  scale_fill_gradient(low = "#fff7bc", high = "#238b45", labels = percent) +
  scale_x_discrete(labels = \(x) str_wrap(x, width = 14)) +
  labs(
    title = "Technology infrastructure by institution type",
    subtitle = "Share of institutions with each component",
    x = "",
    y = ""
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 20, hjust = 1, vjust = 1),
    plot.margin = margin(t = 10, r = 10, b = 20, l = 10)
  )

# D6 (Data governance)
datos_dict <- c(
  "Publica_datos"    = "Publishes open data (2022)",
  "Estrategia_datos" = "Data strategy",
  "Anonimizacion"    = "Data anonymization",
  "Area_datos"       = "Data management unit"
)

dim7 <- df_en %>%
  group_by(tipo_institucion_en) %>%
  summarise(
    Publica_datos = mean(publica_datos_abiertos_2022, na.rm = TRUE),
    Estrategia_datos = mean(estrategia_datos, na.rm = TRUE),
    Anonimizacion = mean(anonimiza_datos, na.rm = TRUE),
    Area_datos = mean(area_gestion_datos, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  pivot_longer(-tipo_institucion_en, names_to = "dimension", values_to = "percent") %>%
  mutate(dimension = recode(dimension, !!!datos_dict))

p_datos <- ggplot(dim7,
                  aes(x = percent,
                      y = fct_reorder(tipo_institucion_en, percent, mean),
                      color = dimension)) +
  geom_segment(aes(x = 0, xend = percent, yend = tipo_institucion_en),
               linewidth = 0.8, alpha = 0.6) +
  geom_point(size = 4) +
  scale_x_continuous(labels = percent) +
  scale_color_brewer(palette = "Set2") +
  labs(
    title = "Data governance and management",
    subtitle = "Share of institutions by dimension",
    x = "% of institutions",
    y = ""
  ) +
  theme_minimal()

# =====================================================
# 12) Index summaries (NOW df_en has the index)
# =====================================================
resumen_indice_total <- df_en %>%
  summarise(
    n = sum(!is.na(indice_madurez_digital)),
    promedio = mean(indice_madurez_digital, na.rm = TRUE),
    mediana = median(indice_madurez_digital, na.rm = TRUE),
    p25 = quantile(indice_madurez_digital, 0.25, na.rm = TRUE),
    p75 = quantile(indice_madurez_digital, 0.75, na.rm = TRUE)
  )

indice_por_tipo <- df_en %>%
  group_by(tipo_institucion_en) %>%
  summarise(
    n = sum(!is.na(indice_madurez_digital)),
    promedio = mean(indice_madurez_digital, na.rm = TRUE),
    mediana = median(indice_madurez_digital, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(desc(promedio))
