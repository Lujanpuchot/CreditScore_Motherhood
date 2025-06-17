
'Para este trabajo se utilizan 4 bases de datos: 
- Survey of Consumer Expectations (SCE_2013) I: desde 2013 a 2016
- Survey of Consumer Expectations II (SCE_2017): desde 2017 a 2019
- Dentro del módulo de SCE, SCE-Credit Acces (SCE_CA): 2013 a 2023
- Dentro del módulo de SCE, SCE-Household Spending (SCE_HS): 2014 a 2022
'

'Datos extraídos del Survey of Consumer Expectations (SCE), incluyendo los 
módulos de Household Spending y Credit Access, disponibles en el CMD Data
Bank de la Federal Reserve Bank of New York 
(https://www.newyorkfed.org/microeconomics/sce).' 

'Explorar las distintas bases y elegir las variables relevantes para analizar las hipotesis principales 
Todas estas bases pueden mergearse utilizando un identificador unico de individuos y las fechas'
'
SCE es la base de Survey of Consumer Expectations (base madre). Entre otras variables, incluye:
-"Q1": Percepción del hogar sobre si están mejor o peor financieramente que hace 12 meses.
-"Q2": Expectativa sobre si estarán mejor o peor financieramente dentro de 12 meses.
- Se releva la situación laboral actual: empleo a tiempo completo/parcial, desempleo, retiro, estudio, etc.
- Se pregunta por la cantidad de trabajos y se identifica el empleo principal (el de más horas).
- Se indaga si el trabajo es por cuenta propia o para otra persona.
- Si es autoempleo, se consulta el tipo (freelance, negocio propio, empresa familiar, otro),
- el número de empleados (si corresponde) y las expectativas de expansión.
- Se preguntan las horas trabajadas en las últimas 4 semanas y las esperadas para las próximas 4.
- Para personas con empleo, se releva la probabilidad subjetiva de:
- perder el trabajo en los próximos 12 meses,
- dejar el trabajo voluntariamente,
- conseguir uno aceptable en caso de pérdida (en 3 meses).
- Para quienes no trabajan pero desean hacerlo, se pregunta si están buscando activamente,

'
"Combinamos las dos bases madres, para los dos rangos de años. Luego, 
la idea es matchear por id con las demás."

"Un mismo individuo puede aparecer en las distintas bases mas de una vez o en meses distintos.
Agregamos la informacion a nivel anual y nos quedamos con una observacion por id/año
"

# --------------------------------------------
# 1) CARGA DE BASES Y PAQUETES
# --------------------------------------------

library(tidyverse)
library(readxl)
library(stringr)
library(lmtest)
library(broom)
library(sandwich)
library(plm)


'Cargo los datos del Survey of Consumer Expectations (SCE), en tres bloques diferentes:
* Dos archivos contienen microdatos del panel 2013–2019, y los otros dos son módulos separados: 
* uno de acceso al crédito y otro de gastos del hogar.'

SCE_2013 <- read_excel("C:/Users/luli_/OneDrive/Documentos/Tesis/Scoring/Datos/Federal Reserve Bank of NY/SCE-Public-Microdata-Complete-2013-2016.xlsx", skip=1, guess_max = 100000)

SCE_2017 <- read_excel("C:/Users/luli_/OneDrive/Documentos/Tesis/Scoring/Datos/Federal Reserve Bank of NY/SCE-Public-Microdata-Complete 2017-2019.xlsx", skip=1, guess_max = 100000)

SCE_CA <- read_excel("C:/Users/luli_/OneDrive/Documentos/Tesis/Scoring/Datos/Federal Reserve Bank of NY/SCE-Credit-Access-complete_microdata.xlsx", skip=1, sheet="Data", guess_max = 100000)

SCE_HS <- read_excel("C:/Users/luli_/OneDrive/Documentos/Tesis/Scoring/Datos/Federal Reserve Bank of NY/Household-Spending- Microdata.xlsx", skip=1, sheet="Data", guess_max = 100000)


'View(SCE_2013)
View(SCE_2017)
View(SCE_CA)
View(SCE_HS)'



# --------------------------------------------
# 2) A- PRE PROCESO
# --------------------------------------------

# Unifico los dos bloques de SCE (2013–2016 y 2017–2019) en una sola base
'Creo las variables de año y mes a partir de la variable date (formato AAAAMM),
y reordenamos las columnas para dejar year y month al principio:'

SCE_ <- bind_rows(SCE_2013, SCE_2017)
'View(SCE_)'

'De esta base, extraemos la última observación disponible por usuario y por año.
Esto lo hago para evitar múltiples observaciones dentro de un mismo año.
Nos quedamos con la del mes más alto (es decir, la más reciente).'

SCE_S <- SCE_ %>%
  mutate(year = substr(as.character(date), 1, 4)) %>%
  mutate(month = substr(as.character(date), 5, 6)) %>%
  select(date, year, month, everything())

'View(SCE_S)'

'
SCE_ es la base de datos para Survey of Consumer Expectations desde 2013 a 2019,
con mes y año segregado
'


# BASE MADRE FINAL
sce <- SCE_S %>%
  group_by(year, userid) %>%
  slice_max(order_by = month, n=1) %>%
  ungroup()


'Hago lo mismo para la base de crédito (CREDIT ACCESS):
creo variables de año y mes y reordenamos columnas.'

'View(SCE_CA)'
CREDIT_ACCESS <- SCE_CA %>%
  mutate(year = substr(as.character(date), 1, 4)) %>%
  mutate(month = substr(as.character(date), 5, 6)) %>%
  select(date, year, month, everything())


##############################################################################
# chequeo users
contar <- CREDIT_ACCESS %>%
  count(userid, name = "contador")

###############################################################################

'Agrupo por año y usuario, y nos quedamos con la última observación disponible'
sce_ca <- CREDIT_ACCESS %>%
  group_by(year, userid) %>%
  slice_max(order_by = month, n=1) %>%
  ungroup()

'View(sce_ca)'

'Repito lo mismo para la base de Household Spending'

HOUSEHOLD_SPENDING <- SCE_HS %>%
  mutate(year = substr(as.character(date), 1, 4)) %>%
  mutate(month = substr(as.character(date), 5, 6)) %>%
  select(date, year, month, everything())


#################################################################################
contar_hs <- HOUSEHOLD_SPENDING %>%
  count(userid, name = "contador")

#################################################################################

sce_hs <- HOUSEHOLD_SPENDING %>%
  group_by(year, userid) %>%
  slice_max(order_by = month, n=1) %>%
  ungroup()

'View(sce_hs)'


# --------------------------------------------
# 2) B- MERGE FINAL DE BASES
# --------------------------------------------

'A la base principal de SCE le agregamos una variable auxiliar (indicador)
y reordenamos columnas. Esta será la base madre sobre la cual hago el merge.'

sce_prueba <- sce %>%
  mutate(indicador = 1)%>%
  select(date, year, month, indicador, everything())

'View(sce_prueba)'

'Ahora realizamos el merge de las tres bases (SCE, Credit Access y Household Spending):
Hacemos left_join por año y userid, porque queremos empalmar las respuestas que cada individuo dio en ese año.
No importa si el mes no coincide exactamente; simplemente usamos la última observación del año en cada base.'

bbdd_2 <- sce_prueba %>%
  left_join(sce_ca, by = c("year", "userid")) %>%
  left_join(sce_hs, by = c("year", "userid"))

'View(bbdd_2)

View(sce)
View(sce_ca)
View(sce_hs)'

# --------------------------------------------
# 2) D - SELECCIÓN DE VARIABLES 
# --------------------------------------------

# Seleccioono las variables que quiero conservar.
# INCLUYO: datos sociodemográficos, preguntas clave de SCE, variables de los módulos adicionales (CA y HS)
# Eliminamos otras variables que no son relevantes para este análisis o que están duplicadas.

View(bbdd_2)
bbdd_final <- bbdd_2 %>%
  select(date.x, year, month.x, userid, weight.x,Q1, Q2, starts_with("Q10"), Q11, Q12new, starts_with("ES1"), Q13new, Q15, Q16, Q19, Q23v2, 
         Q25v2, Q26v2, Q28, Q29, Q30new, Q31v2,
         QRA1, QRA2, Q47, survey_date, `_AGE_CAT`, `_NUM_CAT`,
         `_REGION_CAT`, `_COMMUTING_ZONE`, `_EDU_CAT`, `_HH_INC_CAT`,
         weight.y, starts_with("N1"), starts_with("N2"), N3, starts_with("N4"),starts_with("N5"),
         starts_with("N6"), starts_with("N7"), starts_with("N8"), starts_with("N9"), starts_with("N10"),
         starts_with("N14"), N15, N16, starts_with("N17a"), starts_with("N18"),
         starts_with("N19"), starts_with("N20"), N22, N23,
         qsp1, starts_with("qsp3"), starts_with("qsp5"), starts_with("qsp6b"),
         starts_with("qsp7"), starts_with("qsp10new"), starts_with("qsp11n"),
         starts_with("qsp12n"), starts_with("qsp13new"), qsp14new, k2e, starts_with("k2f"))

# id, Q32, Q33, Q34, starts_with("Q35"), Q36, Q37, Q38, starts_with("HH2"), `_STATE`, Q43, Q43a, 
#Q44, starts_with("Q45"), Q46, Q47

View(bbdd_final)

'Como hay algunas preguntas que solo se hacen la primera vez que el individuo responde
(preguntas del onboarding),generamos una base que contenga únicamente esa primera observación por usuario.'

# Ordeno por userid y fecha para asegurarnos que la primera fila por userid sea efectivamente su primera respuesta:
sce_firstt <- SCE_S %>%
  arrange(userid, date) %>%
  group_by(userid) %>%
  slice(1)

# Seleccionamos solo las preguntas que se hacen la primera vez (Q32 a Q47 y algunas variables demográficas):
sce_1 <- sce_firstt %>%
  select(userid, Q32, Q33, Q34, starts_with("Q35"), Q36, Q37, Q38, starts_with("HH2"), `_STATE`,
         Q43, Q43a, Q44, starts_with("Q45"), Q46, Q47)

# Finalmente, hacemos un merge con la base final para incorporar estas preguntas a la base completa:
base_mergeada_final <- bbdd_final %>%
  left_join(sce_1, by = "userid")

# --------------------------------------------
# 3) CHECKS
# --------------------------------------------

# Dimensiones generales
dim(sce)      # Base madre
dim(sce_ca)   # Módulo Credit Access
dim(sce_hs)   # Módulo Household Spending
dim(base_mergeada_final)   # Base final mergeada

'Merge exitoso --> mismo número de filas que la base madre y se sumaron col.'

# Variables disponibles en cada base
names(sce)
names(sce_ca)
names(sce_hs)
names(base_mergeada_final)

# Usuarios únicos en cada base
n_distinct(sce$userid)
n_distinct(sce_ca$userid)
n_distinct(sce_hs$userid)

# Usuarios únicos en la base final
n_distinct(base_mergeada_final$userid)

'Todos los userid de bbdd_2 coinciden con sce,
como debe ser (porque es left_join() sobre esa base).'

# ¿Cuántos usuarios están en todas las bases?
interseccion_total <- Reduce(intersect, list(
  unique(sce$userid),
  unique(sce_ca$userid),
  unique(sce_hs$userid)
))
length(interseccion_total)  # usuarios que aparecen en las 3 bases

length(intersect(intersect(unique(sce$userid), unique(sce_ca$userid)), unique(sce_hs$userid)))

# Chequeo de duplicados después del merge

base_mergeada_prueba <- base_mergeada_final
base_mergeada_prueba %>%
  count(userid, year) %>%
  filter(n > 1)

'View(base_mergeada_prueba)'

# Años
range(as.numeric(base_mergeada_final$year)) # bien

base_mergeada_final %>%
  count(year)

# Duración del panel (debería ser 1 o 2 como mucho)
base_mergeada_final %>%
  group_by(userid) %>%
  summarise(n_years = n_distinct(year)) %>%
  count(n_years)


# --------------------------------------------
# 4) RENOMBRAR Y REVISO VALORES RAROS
# --------------------------------------------

base_final <- base_mergeada_final

# Vector con los patrones y sus respectivos nuevos prefijos o nombres
patrones <- c(
  "^Q10_" = "employmentsit_",
  "^N1_" = "debt_situation_",
  "^N2b_" = "debt_situation_cat_",
  "^N2_" = "debt_situation_amount_",
  "^N10_" = "loan_partlygranted_amount_",
  "^N12_" = "reasonifnotref_",
  "^N17a_" = "likelihood_12_f_",
  "^N17b_" = "percentchance_12_f_",
  "^N18_" = "reasonifnotlikely_appl_",
  "^N19_" = "reason_notlikely_appl2_",
  "^N20_" = "needbut_unlikely_approved_type_",
  "^N21_" = "percchanc_request_granted_12_f_",
  "^N4_" = "did_apply_request_12_p_",
  "^N5_" = "did_not_reason_",
  "^N6_" = "notapproved_thought_typeofloan_",
  "^N7_" = "notapproved_thought_typeofloan2_",
  "^N9_" = "request_granted_",
  "^qsp3_" = "largepurchases_4_p_",
  "^qsp5_" = "purchases_propotion_",
  "^qsp6bx1_" = "purchases_propotionchange_12_f_upby_",
  "^qsp6bx2_" = "purchases_propotionchange_12_f_downby_",
  "^qsp7dens_" = "totalspending_change_house_12_f_",
  "^qsp10new_" = "typeofspending_increase_12_f_",
  "^qsp11n_" = "typeofspending_decrease_12_f_",
  "^k2f_" = "family_plan_",
  "^Q35_" = "ethnicity_",
  "^HH2_" = "partner_emp_sit_",
  "^Q45new_" = "quant_people_living"
)

# Renombrar prefijos usando un loop sobre patrones
for(pat in names(patrones)) {
  names(base_final) <- sub(pat, patrones[pat], names(base_final))
}

# Diccionario para renombrar variables específicas
nombres_ <- c(
  "weight.x" = "weight", "Q1" = "finbetter_12m_p", "Q2" = "finbetter_12m_p",
  "Q11" = "num_jobs", "Q12new" = "self_empl", "Q13new" = "job_lost_f",
  "Q15" = "search_job", "Q16" = "howlong_search_job", "Q19" = "howlong_outofwork",
  "Q23v2" = "earningspercep_12_f", "Q25v2" = "houseincomepercep_12_f",
  "Q26v2" = "housespendingpercep_12_f", "Q28" = "creditaccesspercep_12_p",
  "Q29" = "creditaccesspercep_12_f", "Q30new" = "unablepayment_3_f",
  "QRA1" = "willingness_risk_finmatt", "QRA2" = "willingness_risk_dailymatt",
  "N11" = "requesto_refinance", "N15" = "late_loan_by30_p",
  "N16" = "late_loan_by90_p", "N22" = "credit_score_cat",
  "N23" = "lastime_checked_score_cat", "N3" = "borrowout_limit_creditcard_12_p",
  "qsp1" = "currspending_vs12_p", "qsp13new" = "10perc_less_nextyear",
  "qsp14new" = "variationofincome_permonth", "k2e" = "spending_saving_plan",
  "Q32" = "age", "Q33" = "gender", "Q34" = "hispaniclatinoorspanish",
  "Q36" = "highest_level_school", "Q37" = "howlong_job", "Q38" = "living_with_partner",
  "Q43" = "rent", "Q43a" = "names_primaryresid", "Q44" = "another_house",
  "Q45b" = "health_perception", "Q46" = "decision_maker"
)

# Renombrar columnas con el diccionario
names(base_final) <- ifelse(
  names(base_final) %in% names(nombres_),
  nombres_[names(base_final)],
  names(base_final)
)

# Consultar manuales y codebook para variables no renombradas, en caso de utilizarse

names(base_final)


# --------------------------------------------
# 5) ESTADÍSTICAS DESCRIPTIVAS
# --------------------------------------------

# - mínimo (min)
# - máximo (max)
# - media (mean)
# - desviación estándar (sd)
# - cantidad de valores faltantes (n_missing)
# - porcentaje de valores faltantes (pct_missing)

any(duplicated(names(base_final)))
names(base_final)[duplicated(names(base_final))]
base_final <- base_final[ , !duplicated(names(base_final))]
'"finbetter_12m_p" estaba duplicado, eliminé la segunda aparición'
base_numericas <- base_final %>%
  select(where(is.numeric))

resumen_desc <- base_numericas %>%
  summarise(across(everything(), list(
    min = ~min(., na.rm = TRUE),
    max = ~max(., na.rm = TRUE),
    mean = ~mean(., na.rm = TRUE),
    sd = ~sd(., na.rm = TRUE),
    n_missing = ~sum(is.na(.)),
    pct_missing = ~mean(is.na(.)) * 100
  ), .names = "{.col}_{.fn}"))

# Trasponemos la variable para facilitar su interpretacion
resumen_tidy <- resumen_desc %>%
  t() %>%
  as.data.frame() %>%
  tibble::rownames_to_column(var = "variable_estadistica") %>%
  rename(valor = V1)

'View(resumen_tidy)'

# Tabla de estadisticas descriptivas
resumen_desc <- base_numericas %>%
  summarise(across(everything(), list(
    min = ~min(., na.rm = TRUE),
    max = ~max(., na.rm = TRUE),
    mean = ~mean(., na.rm = TRUE),
    sd = ~sd(., na.rm = TRUE),
    n_missing = ~sum(is.na(.)),
    pct_missing = ~mean(is.na(.)) * 100
  ), .names = "{.col}_{.fn}"))

resumen_long <- resumen_desc %>%
  pivot_longer(everything(), 
               names_to = c("variable", "estadistico"),
               names_sep = "_") %>%
  pivot_wider(names_from = estadistico, values_from = value) %>%
  arrange(variable)

'View(resumen_long)'

'ACLARACION: algunas variables tienen muchos missings. Revisar en caso de utilizarlas en el analisis'


# Histogramas y boxplot para cada variable numérica

ruta <- "C:/Users/luli_/OneDrive/Documentos/Tesis/Scoring/Datos/Federal Reserve Bank of NY"

for (var in names(base_numericas)) {
  
  p_hist <- ggplot(base_numericas, aes_string(x = var)) + 
    geom_histogram(bins = 30, fill = "skyblue", color = "black") + 
    ggtitle(paste("Histograma de", var)) +
    theme_classic() +  # Fondo blanco y líneas clásicas
    theme(
      plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
      axis.title = element_text(size = 12),
      axis.text = element_text(size = 10)
    )
  
  p_box <- ggplot(base_numericas, aes_string(y = var)) + 
    geom_boxplot(fill = "lightgreen", color = "darkgreen", outlier.color = "red") + 
    ggtitle(paste("Boxplot de", var)) +
    theme_classic() +
    theme(
      plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
      axis.title = element_text(size = 12),
      axis.text = element_text(size = 10)
    )
  
  ggsave(filename = paste0(ruta, "/histograma_", var, ".png"), plot = p_hist, width = 6, height = 4)
  ggsave(filename = paste0(ruta, "/boxplot_", var, ".png"), plot = p_box, width = 4, height = 6)
}


# --------------------------------------------
# 6) ANÁLISIS, Y SEGUNDA PARTE DE MODIFICACIÓN DE BASE (ULTIMOS)
# --------------------------------------------

# Copia de seguridad del dataset

base_final_analisis <- base_final

# A) LIMPIEZA DE NOMBRES DE VARIABLES
any(duplicated(names(base_final_analisis)))
names(base_final_analisis)[duplicated(names(base_final_analisis))]
base_final_analisis <- base_final_analisis[ , !duplicated(names(base_final_analisis))]
'"finbetter_12m_p" estaba duplicado, eliminé la segunda aparición'
'Nota: ya lo hice anteriormente (linea X)'


# B) FILTRO CREDIT SCORE MENOR A 6 (=6 es dont know)
base_final_analisis <- base_final_analisis %>%
  filter(credit_score_cat < 6)

PRUEBA <- base_final %>%
  select(gender, credit_score_cat) # tenemos 11,380 obs donde tenemos género y el score

'View(PRUEBA)'

# C) Creo variable 'female' (0,1) y renombrar 'gender'
# ESTE CÓDIGO SE CORRE UNA VEZ Y SOLO UNA VEZ
base_final_analisis <- base_final_analisis %>%
  mutate(gender = ifelse(gender == 2, 0, 1)) %>%
  rename(female = gender)

print(base_final_analisis$female)
print(base_final_analisis$hispaniclatinoorspanish)

# D) HAGO DUMMYS PARA ETHNICITY 
base_final_analisis <- base_final_analisis %>%
  mutate(
    white = as.integer(hispaniclatinoorspanish == 1),
    black = as.integer(hispaniclatinoorspanish == 2),
    american_indian = as.integer(hispaniclatinoorspanish == 3),
    asian = as.integer(hispaniclatinoorspanish == 4),
    pacific_islander = as.integer(hispaniclatinoorspanish == 5),
    other = as.integer(hispaniclatinoorspanish == 6)
  )

# E) Dummies de hijos
# Variables agregadas: hijos

'View(base_final_analisis)'


base_final_analisis <- base_final_analisis %>%
  mutate(
    parent_5orless = ifelse(quant_people_living5 >= 1, 1, 0),
    parent_living_children = ifelse(
      quant_people_living3 >= 1 | quant_people_living4 >= 1 | quant_people_living5 >= 1,
      1, 0
    ),
    sum_totalchildren = rowSums(across(c(quant_people_living3, quant_people_living4, quant_people_living5)), na.rm = TRUE),
    sum_totalchildren2 = sum_totalchildren^2
  )

"Algunas aclaraciones:

Ahí se crea variables indicadoras y de conteo relacionadas con la presencia de niños en el hogar.

La variable parent_5orless toma el valor 1 si hay al menos un niño menor de 5 años (quant_people_living5 >= 1), 

parent_living_children vale 1 si hay al menos un niño entre 0 y 17 años en el hogar, sumando las categorías 
quant_people_living3, quant_people_living4 y quant_people_living5. 
Además, se calcula sum_totalchildren como la sumatotal de niños en esas tres categorías, 
ignorando los NA, y sum_totalchildren2 como el cuadrado de esa suma, lo cual 
permite modelar posibles efectos no lineales del número de niños en análisis posteriores."


# F) PARA MARRIAGE
# Variables adicionales: estado civil (solo quienes declararon 1)
base_final_analisis <- base_final_analisis %>%
  mutate(
    quant_people_living1 = ifelse(quant_people_living1 >= 2, NA, quant_people_living1)
  )


# F) Transformar 'living_with_partner' en binario
base_final_analisis <- base_final_analisis %>%
  mutate(
    living_with_partner_ = case_when(
      living_with_partner == 1 ~ 1,
      living_with_partner == 2 ~ 0,
      TRUE ~ NA_real_
    )
  )


# G) EMPLOYMENT
variables <- c("employmentsit_1", "employmentsit_2", "employmentsit_3", "employmentsit_4", "employmentsit_5",
               "employmentsit_6", "employmentsit_7", "employmentsit_8", "employmentsit_9")


result <- base_final_analisis %>%
  summarise(across(all_of(variables), sum, na.rm = TRUE))

View(result)

# H) Owner 
base_final_analisis <- base_final_analisis %>%
  mutate(owner_= case_when(
    rent == 1 ~ 1,
    rent == 2 ~ 0,
    rent == 3 ~ NA))

# I)
colnames(base_final_analisis)

"X_STATE" %in% colnames(base_final_analisis)

which(colnames(base_final_analisis) == "X_STATE")

# J) Educacion
base_final_analisis <- base_final_analisis %>%
  mutate(highest_level_school_2 = case_when(
    highest_level_school == 1 ~ 1,
    highest_level_school == 2 ~ 2,
    highest_level_school == 3 ~ 3,
    highest_level_school == 4 ~ 4,
    highest_level_school == 5 ~ 5,
    highest_level_school %in% c(6, 7, 8) ~ 6,
    highest_level_school == 9 ~ NA_real_  # Asiggno missing
  ))

base_final_analisis <- base_final_analisis %>%
  mutate(year_2014 = case_when(
    year == 2014 ~ 1,
    highest_level_school %in% c(2015, 2016, 2017, 2018, 2019) ~ 0))

# K) 

base_final_analisis <- base_final_analisis[ , !duplicated(names(base_final_analisis))]
'"2014" estaba duplicado, eliminé la segunda aparición'

# Crear índice combinando estado y año

indice <- paste(base_final_analisis$X_STATE, base_final_analisis$year)
base_final_analisis <- base_final_analisis[ , !duplicated(names(base_final_analisis))]
table(index = indice, useNA = "ifany")

table(index = paste(base_final_analisis$X_STATE, base_final_analisis$year), useNA = "ifany")

# L) Años

class(base_final_analisis$year)
levels(base_final_analisis$year)

levels(base_final_analisis$year)

class(base_final_analisis$year)
base_final_analisis$year <- factor(base_final_analisis$year)

dummies <- model.matrix(~ year - 1, data = base_final_analisis)
colnames(dummies) <- paste0("year_", unique(base_final_analisis$year))
base_final_analisis <- cbind(base_final_analisis, dummies)

any(duplicated(names(base_final_analisis)))
names(base_final_analisis)[duplicated(names(base_final_analisis))]
# M) ESTADO

base_final_analisis <- base_final_analisis[ , !duplicated(names(base_final_analisis))]

print(base_final_analisis$`_STATE`)
base_final_analisis$`_STATE` <- as.factor(base_final_analisis$`_STATE`)
dummies_estado <- model.matrix(~ `_STATE` - 1, data = base_final_analisis)

colnames(dummies_estado) <- paste0("STATE_", levels(base_final_analisis$`_STATE`))

nrow(base_final_analisis)
nrow(model.matrix(~ `_STATE` - 1, data = base_final_analisis))
base_final_analisis <- base_final_analisis %>%
  filter(!is.na(`_STATE`))
dummies_estado <- model.matrix(~ `_STATE` - 1, data = base_final_analisis)

################  MODELOS ######################################################

'Primeras regresiones para evaluar las hipotesis principales. Todas las regresiones por OLS con errores robustos'


### Estimación de modelos PARTE I ###

run_robust_lm <- function(formula, data) {
  model <- lm(formula, data = data)
  coeftest(model, vcov = vcovHC(model, type = "HC1"))
}

# Modelo simple: género y crédito
modelo_fem_score <- run_robust_lm(credit_score_cat ~ female, base_final_analisis)

print(modelo_fem_score)

# Combinar variables Q47 (ingreso): controlando por ingreso
base_final_analisis$Q47 <- coalesce(base_final_analisis$Q47.x, base_final_analisis$Q47.y)

# Modelos adicionales con controles
modelo_con_Q47 <- run_robust_lm(credit_score_cat ~ female + Q47, base_final_analisis)
modelo_con_h <- run_robust_lm(credit_score_cat ~ black, base_final_analisis)

# Interacciones
modelo_interacciones <- run_robust_lm(credit_score_cat ~ black + female + black:female, base_final_analisis)

# Model con componente de hijos
modelo_children <- run_robust_lm(credit_score_cat ~ parent_5orless, base_final_analisis)
modelo_children2 <- run_robust_lm(credit_score_cat ~ parent_living_children, base_final_analisis)
modelo_children_cuadratico <- run_robust_lm(credit_score_cat ~ sum_totalchildren + sum_totalchildren2, base_final_analisis)

# Segregación por género para análisis específico
base_final_analisis <- base_final_analisis[ , !duplicated(names(base_final_analisis))]
base_mujeres <- base_final_analisis %>% filter(female == 1)
base_hombres <- base_final_analisis %>% filter(female == 0)

# Modelos por género (segrego muestra)
modelo_mujer <- run_robust_lm(credit_score_cat ~ parent_living_children, base_mujeres)
modelo_hombre <- run_robust_lm(credit_score_cat ~ parent_living_children, base_hombres)

# Modelos con variables sociales y de control
resultados_models <- list(
  modelo_fem_score = tidy(modelo_fem_score),
  modelo_con_Q47 = tidy(modelo_con_Q47),
  modelo_con_h = tidy(modelo_con_h),
  modelo_interacciones = tidy(modelo_interacciones),
  modelo_children = tidy(modelo_children),
  modelo_children2 = tidy(modelo_children2),
  modelo_children_cuadratico = tidy(modelo_children_cuadratico),
  modelo_mujer = tidy(modelo_mujer),
  modelo_hombre = tidy(modelo_hombre)
)

# Presentar los resultados de todos los modeloes anteriores en un data frame
tabla_resultados <- bind_rows(resultados_models, .id = "Modelo")
View(tabla_resultados)


### Estimación de modelos PARTE II: controles extra ###


# 1) Si viven con alguien

modelo_rob1 <- run_robust_lm(credit_score_cat ~ quant_people_living1 + parent_living_children + Q47, data = base_final_analisis)
print(modelo_rob1)

# 2) Control por sexo y viviendo con alguien

modelo_rob2 <- run_robust_lm(credit_score_cat ~ quant_people_living1 + parent_living_children + female, data = base_final_analisis)
print(modelo_rob2)

# 3) Control por ingreso y viviendo con alguien

modelo_rob_2 <- run_robust_lm(credit_score_cat ~ quant_people_living1 + Q47, data = base_final_analisis)
print(modelo_rob_2)

# 4) Agregando interacción entre sexo y padres con hijos
modelo_rob_3 <- run_robust_lm(credit_score_cat ~ quant_people_living1 + female + parent_living_children * female, data = base_final_analisis)
print(modelo_rob_3)

# 5) Control por interacción entre sexo y vivir con alguien, más otros controles
modelo_rob_4 <- run_robust_lm(credit_score_cat ~ quant_people_living1 + female + Q47 + parent_living_children * female + female * quant_people_living1, data = base_final_analisis)
print(modelo_rob_4)


### Analisis de la muestra ###

# Chequear los conteos de quienes tienen información en ciertas variables
count_quant_people <- base_final_analisis %>%
  filter(!is.na(quant_people_living1)) %>%
  summarise(count = n())

count_living_partner <- base_final_analisis %>%
  filter(!is.na(living_with_partner)) %>%
  summarise(count = n())

# Mostrar los resultados juntos
resultados <- bind_cols(count_quant_people, count_living_partner)
 print(resultados)

which(colnames(base_final_analisis) == "purchases_propotion_6")

# Agrego interacciones que elijo ponerlas "a mano"
# 
base_final_analisis$int_children_partner <- base_final_analisis$parent_living_children * 
  base_final_analisis$living_with_partner_

base_final_analisis$int_female_partner <- base_final_analisis$female * 
  base_final_analisis$living_with_partner_


base_hombres$int_children_partner <- base_hombres$parent_living_children * 
  base_hombres$living_with_partner_

base_hombres$int_female_partner <- base_hombres$female * 
  base_hombres$living_with_partner_


base_mujeres$int_children_partner <- base_mujeres$parent_living_children * 
  base_mujeres$living_with_partner_

base_mujeres$int_female_partner <- base_mujeres$female * 
  base_mujeres$living_with_partner_

 
### Estimación de modelos PARTE III: controles extra y otras variables ###
 
# base_final_analisis$X_STATE <- as.factor(base_final_analisis$X_STATE)
# base_final_analisis$year <- as.factor(base_final_analisis$year)
# str(base_final_analisis$X_STATE)
# str(base_final_analisis$year)
# base_final_analisis$X_STATE <- as.factor(base_final_analisis$X_STATE)
# class(year)
 
 # Efectos fijos por estado
 modelo_fx <- lm(
   credit_score_cat ~ living_with_partner_ + Q47 + female + employmentsit_1 + 
     employmentsit_2 + employmentsit_3 + employmentsit_6 + employmentsit_7 +  
     employmentsit_9 + num_jobs + age + highest_level_school_2 + hispaniclatinoorspanish + 
     ethnicity_2 + partner_emp_sit_1 + partner_emp_sit_2 + partner_emp_sit_3 +
     partner_emp_sit_4 + partner_emp_sit_10 + howlong_job + 
     parent_living_children * living_with_partner_ + female * living_with_partner_ +
     `_STATE` , 
   data = base_final_analisis
 )
 
 # Efectos fijos por estado y efectos temporales por año
 base_final_analisis <- base_final_analisis %>%
   mutate(year = factor(year))
 
 modelo_fx1 <- lm(
   credit_score_cat ~ living_with_partner_ + parent_living_children + Q47 + female + 
     employmentsit_1 + employmentsit_2 + employmentsit_3 + employmentsit_6 + employmentsit_7 +  
     employmentsit_9 + num_jobs + age + highest_level_school_2 + hispaniclatinoorspanish + 
     ethnicity_2 + howlong_job + int_children_partner +
     int_female_partner + female * parent_living_children + 
     `_STATE` + year_2014 + year_2015 + year_2016 + year_2017 +
     year_2018, 
   data = base_final_analisis
 )
 

# Agregando controles economicos
 modelo_fx2 <- lm(
   credit_score_cat ~ living_with_partner_ + parent_living_children + Q47 + female + 
     employmentsit_1 + employmentsit_2 + employmentsit_3 + employmentsit_6 + employmentsit_7 +  
     employmentsit_9 + num_jobs + age + highest_level_school_2 + hispaniclatinoorspanish + 
     ethnicity_2 + howlong_job + int_children_partner +
     int_female_partner + female * parent_living_children + owner_ +
     purchases_propotion_6 + purchases_propotion_7 +
     `_STATE` + year_2014 + year_2015 + year_2016 + year_2017 +
     year_2018, 
   data = base_final_analisis
 )
 
 
 # cuantas obs? ########################################
 observaciones_usadasfx_ <- model.frame(modelo_fx1)     #
 #
 # Ver las primeras filas                              #
 head(observaciones_usadasfx_)                           #
 #
 # O contar el número total de observaciones usadas    #
 nrow(observaciones_usadasfx_)                           #
 #######################################################
 
 
 # cuantas obs? ########################################
 observaciones_usadasfx2_ <- model.frame(modelo_fx2)     #
 #
 # Ver las primeras filas                              #
 head(observaciones_usadasfx2_)                           #
 #
 # O contar el número total de observaciones usadas    #
 nrow(observaciones_usadasfx2_)                           #
 #######################################################
 
 
# Modelo completo para hombres
 modelo_fx_male <- lm(
   credit_score_cat ~ living_with_partner_ + parent_living_children + Q47 + 
     employmentsit_1 + employmentsit_2 + employmentsit_3 + employmentsit_6 + employmentsit_7 +  
     employmentsit_9 + num_jobs + age + highest_level_school_2 + hispaniclatinoorspanish + 
     ethnicity_2 + howlong_job + int_children_partner + female * parent_living_children + owner_ +
     purchases_propotion_6 + purchases_propotion_7 +
     `_STATE` + year_2014 + year_2015 + year_2016 + year_2017 +
     year_2018, 
   data = base_hombres
 )
 modelo_fx_male <- coeftest(modelo_fx_male, vcov = vcovHC(modelo_fx_male, type = "HC1"))
 modelo_fx_male
 
 # Modelo completo para mujeres
modelo_fx_female <- lm(
   credit_score_cat ~ living_with_partner_ + parent_living_children + Q47 + 
     employmentsit_1 + employmentsit_2 + employmentsit_3 + employmentsit_6 + employmentsit_7 +  
     employmentsit_9 + num_jobs + age + highest_level_school_2 + hispaniclatinoorspanish + 
     ethnicity_2 + howlong_job + int_children_partner + owner_ +
     purchases_propotion_6 + purchases_propotion_7 +
     `_STATE` + year_2014 + year_2015 + year_2016 + year_2017 +
     year_2018, 
   data = base_mujeres
 )
 modelo_fx_female <- coeftest(modelo_fx_female, vcov = vcovHC(modelo_fx_female, type = "HC1"))
 modelo_fx_female 
 
 # Modelo completo para mujeres sin interacción
 
 modelo_fx_female0 <- lm(
   credit_score_cat ~ living_with_partner_ + parent_living_children + 
     employmentsit_1 + employmentsit_2 + employmentsit_3 + employmentsit_6 + employmentsit_7 +  
     employmentsit_9 + num_jobs + age + highest_level_school_2 + hispaniclatinoorspanish + 
     ethnicity_2 +
     purchases_propotion_6 + purchases_propotion_7 +
     `_STATE` + year_2014 + year_2015 + year_2016 + year_2017 +
     year_2018, 
   data = base_mujeres
 )
 modelo_fx_female0 <- coeftest(modelo_fx_female0, vcov = vcovHC(modelo_fx_female0, type = "HC1"))
 modelo_fx_female0
 
 # En tanto saco el ingreso, el living_with_partner se vuelve positivo
 
 
 ##############
 
'En sintesis: el gender gap en el credit score, que se puede ver en el coeficiente negativo de female, es robusto a distintas especificaciones y controles
 
 Extension: pooled DiD usando la expansion de Medicaid. Ayudo a reducir el credit gender gap?'
 











 
 
 




