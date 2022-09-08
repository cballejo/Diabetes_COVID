library(tidyverse)
library(readxl)
library(dlookr)
library(skimr)
library(gtsummary)
library(flextable)
library(lubridate)
library(rstatix)
library(janitor)
library(effectsize)
library(ggstatsplot)
library(ggforce)

## exploraciion

datos <- read_excel("R_31_08_22/DM_COVID_30_8_22.xlsx")

glimpse(datos)

skim(datos)

diagnose_report(.data=datos, output_file = "diagnostico dlookr")

diagnose_outlier(datos)

## tabla inicial

datos_generales<- datos %>% dplyr::select(-dplyr::starts_with("pan"),
                                          -dplyr::starts_with("pre"))

datos_generales <- datos_generales %>% 
  mutate(Cobertura_salud=if_else(ID < 500, "Sin cobertura", "Con cobertura"),
         Y = year(Ingreso),
         Tiempo_dg = Y-año_diag)


#tabla descriptiva linea de base

tabla <- datos_generales %>% dplyr::select(`Edad revisado`, Sexo, tipo_DM,
                                           Comorbilidades, `COVID-19`, 
                                           Cobertura_salud, Tiempo_dg) %>% 
  tbl_summary(
    by=Cobertura_salud,
    missing= "no",
  ) %>% 
  add_n () %>% # agrega frecuencia
  add_p() %>% # agrega p valor
  modify_header(label="**variable**") %>% 
  bold_labels() %>% 
  as_flex_table()

save_as_docx(tabla, path="tabla.docx")



## division en prepandemia - pandemia

datos_pre <- datos %>% select(dplyr::starts_with("pre"),ID, Ingreso, año_diag, Sexo, `Edad revisado`)

datos_pre <- datos_pre %>% 
  mutate(Cobertura_salud=if_else(ID < 500, "Sin cobertura", "Con cobertura"))

datos_pan<-datos %>% select(dplyr::starts_with("pan"),ID, Ingreso, año_diag, Sexo, `Edad revisado`)

datos_pan <- datos_pan %>% 
  mutate(Cobertura_salud=if_else(ID < 500, "Sin cobertura", "Con cobertura"))

plot_na_pareto(datos_pre)



###construccion de variables

##IMC

datos_pre <- datos_pre %>% mutate(IMC = pre_peso/(pre_talla)^2)

summary(datos_pre$IMC)  # 1 NA

datos_pan <- datos_pan %>% mutate(IMC = pan_peso/(pan_talla)^2)

summary(datos_pan$IMC)

##tiempo desde el dg de la enf

# datos <- datos %>% mutate(Y = year(Ingreso))
# 
# datos <- datos %>% mutate(Tiempo_dg=Y-a?o_diag)
# 
# datos %>% count(Tiempo_dg)

datos_pre <- datos_pre %>% mutate(Y = year(Ingreso),
                                  Tiempo_dg = Y-año_diag)

datos_pan <- datos_pan %>% mutate(Y = year(Ingreso),
                                  Tiempo_dg = Y-año_diag)
#variable: esquema de tto

#variables
#pan_dieta
#pan_act_fisica
#pan_tto_farmacologico
#pan-tipo_tto

#pre pandemia

datos_pre <- datos_pre %>% 
  mutate(esquema_tto = case_when(
    pre_dieta=="Si"& pre_act_fisica== "No" & pre_tto_farmacologico == "No" ~ "Dieta",
    pre_dieta=="No"& pre_act_fisica== "Si" & pre_tto_farmacologico == "No" ~ "AF",
    pre_dieta=="No"& pre_act_fisica== "No" & pre_tto_farmacologico == "Si" ~ "Farmacos",
    pre_dieta=="Si"& pre_act_fisica== "Si" & pre_tto_farmacologico == "No" ~ "Dieta+AF",
    pre_dieta=="Si"& pre_act_fisica== "No" & pre_tto_farmacologico == "Si" ~ "Dieta+Farmacos",
    pre_dieta=="No"& pre_act_fisica== "Si" & pre_tto_farmacologico == "Si" ~ "AF+Farmacos",
    pre_dieta=="Si"& pre_act_fisica== "Si" & pre_tto_farmacologico == "Si" ~ "Dieta+AF+Farmacos",
    pre_dieta=="No"& pre_act_fisica== "No" & pre_tto_farmacologico == "No" ~ "Sin tratamiento"))

#pandemia

datos_pan <- datos_pan %>% 
  mutate(esquema_tto = case_when(
    pan_dieta=="Si"& pan_act_fisica== "No" & pan_tto_farmacologico == "No" ~ "Dieta",
    pan_dieta=="No"& pan_act_fisica== "Si" & pan_tto_farmacologico == "No" ~ "AF",
    pan_dieta=="No"& pan_act_fisica== "No" & pan_tto_farmacologico == "Si" ~ "Farmacos",
    pan_dieta=="Si"& pan_act_fisica== "Si" & pan_tto_farmacologico == "No" ~ "Dieta+AF",
    pan_dieta=="Si"& pan_act_fisica== "No" & pan_tto_farmacologico == "Si" ~ "Dieta+Farmacos",
    pan_dieta=="No"& pan_act_fisica== "Si" & pan_tto_farmacologico == "Si" ~ "AF+Farmacos",
    pan_dieta=="Si"& pan_act_fisica== "Si" & pan_tto_farmacologico == "Si" ~ "Dieta+AF+Farmacos",
    pan_dieta=="No"& pan_act_fisica== "No" & pan_tto_farmacologico == "No" ~ "Sin tratamiento")) 

####

datos_pre <- datos_pre |> mutate(momento = "prepandemia")

datos_pan <- datos_pan |> mutate(momento = "pandemia")

datos_pre <- datos_pre |> 
  mutate(control_metabolico = if_else(pre_HbA1c > 7, "mal", "bien"))

datos_pan <- datos_pan |> 
  mutate(control_metabolico = if_else(pan_HbA1c > 7, "mal", "bien"))

## unificacion de nombres de variables (ver cuales otras mas)

names(datos_pan)[25] <- "HbA1c"

names(datos_pre)[23] <- "HbA1c"

names(datos_pan)[8] <- "tipo_tto"

names(datos_pre)[8] <- "tipo_tto"

names(datos_pan)[9] <- "nro_consultas"

names(datos_pre)[9] <- "nro_consultas"

names(datos_pan)[12] <- "internacion"

names(datos_pre)[11] <- "internacion"

names(datos_pan)[26] <- "glucemia"

names(datos_pre)[24] <- "glucemia"

names(datos_pan)[27] <- "trigliceridos"

names(datos_pre)[25] <- "trigliceridos"

names(datos_pan)[28] <- "HDL"

names(datos_pre)[26] <- "HDL"

names(datos_pan)[29] <- "LDL"

names(datos_pre)[27] <- "LDL"

names(datos_pan)[30] <- "IAC"

names(datos_pre)[28] <- "IAC"

names(datos_pan)[31] <- "fondo"

names(datos_pre)[29] <- "fondo"

names(datos_pan)[33] <- "fondo_categor"

names(datos_pre)[31] <- "fondo_categor"


## seleccion de variables participantes del analisis final

datos_pre <- datos_pre |> 
  select(ID, momento, Tiempo_dg, IMC, esquema_tto, Cobertura_salud, 
         control_metabolico, HbA1c, Sexo, `Edad revisado`, tipo_tto, 
         nro_consultas, internacion, glucemia, trigliceridos, HDL, LDL, 
         IAC, fondo, fondo_categor)

datos_pan <- datos_pan |> 
  select(ID, momento, Tiempo_dg, IMC, esquema_tto, Cobertura_salud, 
         control_metabolico, HbA1c, Sexo, `Edad revisado`, tipo_tto, 
         nro_consultas, internacion, glucemia, trigliceridos, HDL, LDL, 
         IAC, fondo, fondo_categor)

## union tidy

datos_completos <- datos_pre |> 
  bind_rows(datos_pan)

## union no tidy

datos_no_tidy <- datos_pre |> 
  inner_join(datos_pan, by = "ID") |> 
  select(ID, control_metabolico.x, control_metabolico.y, Cobertura_salud.x, HbA1c.x, HbA1c.y, Sexo.x) |> 
  rename(control_met_pre = "control_metabolico.x",
         control_met_pan = "control_metabolico.y",
         cobertura_salud = "Cobertura_salud.x",
         HbA1c_pre = "HbA1c.x",
         HbA1c_pan = "HbA1c.y",
         Sexo = "Sexo.x")

# conversion a factor

datos_completos <- datos_completos |> 
  mutate(momento = factor(momento, levels = c("prepandemia", "pandemia")),
         Cobertura_salud = factor(Cobertura_salud))

#####

## univariado

## esquema_tto

datos_completos |> 
  count(momento, esquema_tto)

## 7 NAs

datos_completos |>  tabyl(esquema_tto, momento) |> 
  adorn_percentages(denominator = "col") |> 
  adorn_pct_formatting() 

## Cobertura_salud

datos_completos |> 
  count(momento, Cobertura_salud)


datos_completos |>  tabyl(Cobertura_salud, momento) |> 
  adorn_percentages(denominator = "col") |> 
  adorn_pct_formatting() 

## control metabolico

datos_completos |> 
  count(momento, control_metabolico)


datos_completos |>  tabyl(control_metabolico, momento) |> 
  adorn_percentages(denominator = "col") |> 
  adorn_pct_formatting() 

## IMC

datos_completos |> 
  group_by(momento) |> 
  summarise(mediana = median(IMC, na.rm = T),
            min = min(IMC, na.rm = T),
            max = max(IMC, na.rm = T),
            RIC = IQR(IMC, na.rm = T))


datos_completos |> 
  ggplot(aes(x = momento, y = IMC, fill = momento)) +
  geom_boxplot() +
  scale_fill_brewer(palette = "Set1")

datos_completos |> 
  group_by(momento) |> 
  shapiro_test(IMC)

## tiempo de diagnostico

datos_completos |> 
  filter(momento == "prepandemia") |> 
  summarise(mediana = median(Tiempo_dg, na.rm = T),
            min = min(Tiempo_dg, na.rm = T),
            max = max(Tiempo_dg, na.rm = T),
            RIC = IQR(Tiempo_dg, na.rm = T))


datos_completos |> 
  filter(momento == "prepandemia") |> 
  ggplot(aes(y = Tiempo_dg)) +
  geom_boxplot()

datos_completos |> 
  filter(momento == "prepandemia") |> 
  shapiro_test(Tiempo_dg)

## Hba1C

datos_completos |> 
  group_by(momento) |> 
  summarise(mediana = median(HbA1c, na.rm = T),
            min = min(HbA1c, na.rm = T),
            max = max(HbA1c, na.rm = T),
            RIC = IQR(HbA1c, na.rm = T)) 


datos_completos |> 
  ggplot(aes(x = momento, y = HbA1c, fill = momento)) +
  geom_boxplot() +
  scale_fill_brewer(palette = "Set1")

datos_completos |> 
  group_by(momento) |> 
  shapiro_test(HbA1c)


## test de hipotesis

# McNemar sobre control metabolico 


datos_completos |> 
  pairwise_mcnemar_test(control_metabolico ~ momento | ID, correct = F)


## tamaño de efecto

tabla <- xtabs( ~ control_met_pre + control_met_pan, data = datos_no_tidy)

tabla

cohens_g(tabla)

interpret_cohens_g(0.31)

oddsratio(tabla)

mcnemar.test(tabla, correct = F)

# Effect size
# Appropriate effect sizes for data subjected to McNemar, McNemar–Bowker, 
# or equivalent exact tests include Cohen’s g and odds ratio.

# Considering a 2 x 2 table, with a and d being the concordant cells and b and 
# c being the discordant cells, the odds ratio is simply the greater of (b/c) 
# or (c/b), and P is the greater of (b/(b+c)) or (c/ b+c)).  Cohen’s g is P – 0.5.  
# These statistics can be extended to larger tables.

# Cohen (1988) lists interpretations for Cohen’s g.  
# These can easily translated into interpretations for the odds ratio.  
# For example, the cutoff for the “small” interpretation is g ≥ 0.05, 
# which would be 0.55 : 0.45 odds, or 0.55/0.45, or 1.22.

#    Small           Medium            Large
# 
# Cohen’s g
# 
# 0.05 – < 0.15   0.15 – < 0.25        ≥ 0.25
# 
# Odds ratio (OR)
# 
# 1.22 – < 1.86   1.86 – < 3.00        ≥ 3.00

library(ggstatsplot)

ggbarstats(
  data = datos_no_tidy,
  x    = control_met_pre, 
  y    = control_met_pan,
  paired = TRUE, 
  label = "both", 
  xlab = "Control metabólico - pandemia", 
  legend.title = "Control metabólico \n prepandemia"
)


## HbAc1

datos_completos |> 
  group_by(momento) |> 
  shapiro_test(HbA1c)


datos_completos |> 
  pairwise_wilcox_test(formula = HbA1c ~ momento, paired = T)

rank_biserial(x = HbA1c ~ momento, data = datos_completos, paired = T)

interpret_rank_biserial(-0.47)

interpret_rank_biserial(-0.47, rules = "lovakov2021")

ggwithinstats(
  data = datos_completos,
  x    = momento,  
  y    = HbA1c, 
  type = "nonparametric", 
  point.path = F, 
  ggplot.component = geom_hline(yintercept = 7, alpha = 0.4)
)
 
## estratificado

## cobertura de salud

datos_completos |> 
  filter(Cobertura_salud == "Sin cobertura") |> 
  pairwise_mcnemar_test(control_metabolico ~ momento | ID, correct = T)

datos_completos |> 
  filter(Cobertura_salud == "Con cobertura") |> 
  pairwise_mcnemar_test(control_metabolico ~ momento | ID, correct = T)


tabla_sin_cobertura <- xtabs( ~ control_met_pre + control_met_pan, 
                              data = datos_no_tidy[datos_no_tidy$cobertura_salud=="Sin cobertura",])

tabla_sin_cobertura

cohens_g(tabla_sin_cobertura)

interpret_cohens_g(0.12)

oddsratio(tabla_sin_cobertura)

mcnemar.test(tabla_sin_cobertura)

datos_completos |> filter(is.na(IMC))

tabla_con_cobertura <- xtabs( ~ control_met_pre + control_met_pan, 
                              data = datos_no_tidy[datos_no_tidy$cobertura_salud=="Con cobertura",])

tabla_con_cobertura

cohens_g(tabla_con_cobertura)

interpret_cohens_g(0.37)

oddsratio(tabla_con_cobertura)

mcnemar.test(tabla_con_cobertura)

ggbarstats(
  data = datos_no_tidy[datos_no_tidy$cobertura_salud == "Sin cobertura",],
  x    = control_met_pre, 
  y    = control_met_pan,
  paired = TRUE, 
  label = "both", 
  xlab = "Control metabólico - postpandemia", 
  legend.title = "Control metabólico \n prepandemia"
)

datos_completos |> count(momento, control_metabolico)

datos_completos |> count(momento, control_metabolico, Cobertura_salud)

## cuidado aplica test mcnemar sin correccion de Yates

ggbarstats(
  data = datos_no_tidy[datos_no_tidy$cobertura_salud == "Con cobertura",],
  x    = control_met_pre, 
  y    = control_met_pan,
  paired = TRUE, 
  label = "both", 
  xlab = "Control metabólico - postpandemia", 
  legend.title = "Control metabólico \n prepandemia"
)

## cuidado aplica test mcnemar sin correccion de Yates

### según Sexo

datos_completos |> 
  filter(Sexo == "Mujer") |> 
  pairwise_mcnemar_test(control_metabolico ~ momento | ID, correct = T)

datos_completos |> 
  filter(Sexo == "Varón") |> 
  pairwise_mcnemar_test(control_metabolico ~ momento | ID, correct = T)


tabla_mujer <- xtabs( ~ control_met_pre + control_met_pan, 
                              data = datos_no_tidy[datos_no_tidy$Sexo=="Mujer",])

tabla_mujer

cohens_g(tabla_mujer)

interpret_cohens_g(0.26)

oddsratio(tabla_mujer)

mcnemar.test(tabla_mujer)


tabla_varon <- xtabs( ~ control_met_pre + control_met_pan, 
                              data = datos_no_tidy[datos_no_tidy$Sexo=="Varón",])

tabla_varon

cohens_g(tabla_varon)

interpret_cohens_g(0.34)

oddsratio(tabla_con_cobertura)

mcnemar.test(tabla_varon)

datos_completos |> 
  filter(momento == "prepandemia") |> 
  count(Sexo)

ggbarstats(
  data = datos_no_tidy[datos_no_tidy$Sexo == "Mujer",],
  x    = control_met_pre, 
  y    = control_met_pan,
  paired = TRUE, 
  label = "both", 
  xlab = "Control metabólico - postpandemia", 
  legend.title = "Control metabólico \n prepandemia"
)

## cuidado aplica test mcnemar sin correccion de Yates

ggbarstats(
  data = datos_no_tidy[datos_no_tidy$Sexo == "Varón",],
  x    = control_met_pre, 
  y    = control_met_pan,
  paired = TRUE, 
  label = "both", 
  xlab = "Control metabólico - postpandemia", 
  legend.title = "Control metabólico \n prepandemia"
)

## cuidado aplica test mcnemar sin correccion de Yates

datos_completos |> 
  filter(Cobertura_salud == "Sin cobertura") |> 
  pairwise_wilcox_test(formula = HbA1c ~ momento, paired = T)

rank_biserial(x = HbA1c ~ momento, 
              data = datos_completos[datos_completos$Cobertura_salud=="Sin cobertura",], 
              paired = T)

interpret_rank_biserial(-0.48)

datos_completos |> 
  filter(Cobertura_salud == "Con cobertura") |> 
  pairwise_wilcox_test(formula = HbA1c ~ momento, paired = T)

rank_biserial(x = HbA1c ~ momento, 
              data = datos_completos[datos_completos$Cobertura_salud=="Con cobertura",], 
              paired = T)

interpret_rank_biserial(-0.47)

## Sexo

datos_completos |> 
  filter(Sexo == "Mujer") |> 
  pairwise_wilcox_test(formula = HbA1c ~ momento, paired = T)


rank_biserial(x = HbA1c ~ momento, 
              data = datos_completos[datos_completos$Sexo=="Mujer",], 
              paired = T)

interpret_rank_biserial(-0.53)


datos_completos |> 
  filter(Sexo == "Varón") |> 
  pairwise_wilcox_test(formula = HbA1c ~ momento, paired = T)


rank_biserial(x = HbA1c ~ momento, 
              data = datos_completos[datos_completos$Sexo=="Varón",], 
              paired = T)

interpret_rank_biserial(-0.42)


## medidas resumen HbA1c según subconjuntos

tabla1 <- datos_completos |> 
  group_by(momento, Cobertura_salud) |> 
  summarise(Mediana = median(HbA1c),
            Min = min(HbA1c),
            Max = max(HbA1c),
            RIC = IQR(HbA1c), 
            n = n()) |> 
  flextable()

save_as_docx(tabla1, path = "tabla HbA1c segun cobertura.docx")

tabla2 <- datos_completos |> 
  group_by(momento, Sexo) |> 
  summarise(Mediana = median(HbA1c),
            Min = min(HbA1c),
            Max = max(HbA1c),
            RIC = IQR(HbA1c),
            n = n()) |> 
  flextable()

save_as_docx(tabla2, path = "tabla HbA1c segun sexo.docx")

tabla3 <- datos_completos |> 
  group_by(momento, Cobertura_salud, Sexo) |> 
  summarise(Mediana = median(HbA1c),
            Min = min(HbA1c),
            Max = max(HbA1c),
            RIC = IQR(HbA1c),
            n = n()) |> 
  flextable()

save_as_docx(tabla3, path = "tabla HbA1c segun cobertura y sexo.docx")

### con otras variables secundarias

### IMC

datos_completos_IMC <- datos_completos |> filter(!ID == 31)

datos_completos_IMC |> 
  group_by(momento) |> 
  summarise(Mediana = median(IMC),
            Min = min(IMC),
            Max = max(IMC),
            RIC = IQR(IMC),
            Cuartil_1 = quantile(IMC, probs = 0.25),
            Cuartil_3 = quantile(IMC, probs = 0.75),
            n = n())

datos_completos_IMC |> 
  group_by(momento, Cobertura_salud) |> 
  summarise(Mediana = median(IMC),
            Min = min(IMC),
            Max = max(IMC),
            RIC = IQR(IMC),
            Cuartil_1 = quantile(IMC, probs = 0.25),
            Cuartil_3 = quantile(IMC, probs = 0.75),
            n = n())

datos_completos_IMC |> 
  pairwise_wilcox_test(formula = IMC ~ momento, paired = T)

rank_biserial(x = IMC ~ momento, data = datos_completos_IMC, paired = T)

interpret_rank_biserial(-0.21)

ggwithinstats(
  data = datos_completos_IMC,
  x    = momento,  
  y    = IMC, 
  type = "nonparametric", 
  point.path = F, 
  ggplot.component = geom_hline(yintercept = 7, alpha = 0.4)
)

datos_completos_IMC |> 
  filter(Cobertura_salud == "Sin cobertura") |> 
  pairwise_wilcox_test(formula = IMC ~ momento, paired = T)

rank_biserial(x = IMC ~ momento, 
              data = datos_completos_IMC[datos_completos_IMC$Cobertura_salud=="Sin cobertura",], 
              paired = T)

interpret_rank_biserial(-0.23)

datos_completos_IMC |> 
  filter(Cobertura_salud == "Con cobertura") |> 
  pairwise_wilcox_test(formula = IMC ~ momento, paired = T)

rank_biserial(x = IMC ~ momento, 
              data = datos_completos_IMC[datos_completos_IMC$Cobertura_salud=="Con cobertura",], 
              paired = T)

interpret_rank_biserial(-0.18)


datos_completos_IMC |> 
  ggplot(aes(x = momento, y = IMC)) +
  geom_violin(scale = "width") +
  geom_sina(aes(color = momento), alpha = 0.6) +
  facet_wrap(Cobertura_salud ~ .) + 
  scale_color_manual(values = c("forestgreen", "dodgerblue4"),
                     name = "") +
  stat_summary(fun = median,
               fun.min = function(z) { quantile(z, 0.25)},
               fun.max = function(z) { quantile(z, 0.75)},
               geom = "pointrange", color="firebrick4", linetype = 2, width = 0.5)

## trigliceridos

ID_trigli <- datos_completos |> 
  filter(is.na(trigliceridos)) |> 
  select(ID) |> 
  distinct() 

datos_completos_trigliceridos <- datos_completos |> 
  anti_join(ID_trigli)

datos_completos_trigliceridos |> 
  group_by(momento) |> 
  shapiro_test(trigliceridos)

datos_completos_trigliceridos |> 
  group_by(momento) |> 
  summarise(Mediana = median(trigliceridos),
            Min = min(trigliceridos),
            Max = max(trigliceridos),
            RIC = IQR(trigliceridos),
            Cuartil_1 = quantile(trigliceridos, probs = 0.25),
            Cuartil_3 = quantile(trigliceridos, probs = 0.75),
            n = n())

datos_completos_trigliceridos |> 
  group_by(momento, Cobertura_salud) |> 
  summarise(Mediana = median(trigliceridos),
            Min = min(trigliceridos),
            Max = max(trigliceridos),
            RIC = IQR(trigliceridos),
            Cuartil_1 = quantile(trigliceridos, probs = 0.25),
            Cuartil_3 = quantile(trigliceridos, probs = 0.75),
            n = n())

datos_completos_trigliceridos |> 
  group_by(momento, Cobertura_salud) |> 
  shapiro_test(trigliceridos)

datos_completos_trigliceridos |> 
  pairwise_wilcox_test(formula = trigliceridos ~ momento, paired = T)

rank_biserial(x = trigliceridos ~ momento, 
              data = datos_completos_trigliceridos,
              paired = T)

interpret_rank_biserial(-0.31)


datos_completos_trigliceridos |> 
  filter(Cobertura_salud == "Sin cobertura") |> 
  pairwise_wilcox_test(formula = trigliceridos ~ momento, paired = T)

rank_biserial(x = trigliceridos ~ momento, 
              data = datos_completos_trigliceridos[datos_completos_trigliceridos$Cobertura_salud=="Sin cobertura",], 
              paired = T)

interpret_rank_biserial(-0.19)


datos_completos_trigliceridos |> 
  filter(Cobertura_salud == "Con cobertura") |> 
  pairwise_wilcox_test(formula = trigliceridos ~ momento, paired = T)

rank_biserial(x = trigliceridos ~ momento, 
              data = datos_completos_trigliceridos[datos_completos_trigliceridos$Cobertura_salud=="Con cobertura",], 
              paired = T)

interpret_rank_biserial(-0.57)

datos_completos_trigliceridos |> 
  ggplot(aes(x = momento, y = trigliceridos)) +
  geom_violin(scale = "width") +
  geom_sina(aes(color = momento), alpha = 0.6) +
  facet_wrap(Cobertura_salud ~ .) + 
  scale_color_manual(values = c("forestgreen", "dodgerblue4"),
                     name = "") +
  stat_summary(fun = median,
               fun.min = function(z) { quantile(z, 0.25)},
               fun.max = function(z) { quantile(z, 0.75)},
               geom = "pointrange", color="firebrick4", linetype = 2, width = 0.5)


## colesterol HDL

ID_HDL <- datos_completos |> 
  filter(is.na(HDL)) |> 
  select(ID) |> 
  distinct() 

datos_completos_HDL <- datos_completos |> 
  anti_join(ID_HDL)

datos_completos_HDL |> 
  group_by(momento) |> 
  shapiro_test(HDL)

datos_completos_HDL |> 
  group_by(momento) |> 
  summarise(Mediana = median(HDL),
            Min = min(HDL),
            Max = max(HDL),
            RIC = IQR(HDL),
            Cuartil_1 = quantile(HDL, probs = 0.25),
            Cuartil_3 = quantile(HDL, probs = 0.75),
            n = n())

datos_completos_HDL |> 
  group_by(momento, Cobertura_salud) |> 
  summarise(Mediana = median(HDL),
            Min = min(HDL),
            Max = max(HDL),
            RIC = IQR(HDL),
            Cuartil_1 = quantile(HDL, probs = 0.25),
            Cuartil_3 = quantile(HDL, probs = 0.75),
            n = n())


datos_completos_HDL |> 
  pairwise_wilcox_test(formula = HDL ~ momento, paired = T)

rank_biserial(x = HDL ~ momento, 
              data = datos_completos_HDL,
              paired = T)

interpret_rank_biserial(-0.40)


datos_completos_HDL |> 
  filter(Cobertura_salud == "Sin cobertura") |> 
  pairwise_wilcox_test(formula = HDL ~ momento, paired = T)

rank_biserial(x = HDL ~ momento, 
              data = datos_completos_HDL[datos_completos_HDL$Cobertura_salud=="Sin cobertura",], 
              paired = T)

interpret_rank_biserial(-0.53)

datos_completos_HDL |> 
  filter(Cobertura_salud == "Con cobertura") |> 
  pairwise_wilcox_test(formula = HDL ~ momento, paired = T)

rank_biserial(x = HDL ~ momento, 
              data = datos_completos_HDL[datos_completos_HDL$Cobertura_salud=="Con cobertura",], 
              paired = T)

interpret_rank_biserial(-0.15)


datos_completos_HDL |> 
  ggplot(aes(x = momento, y = HDL)) +
  geom_violin(scale = "width") +
  geom_sina(aes(color = momento), alpha = 0.6) +
  facet_wrap(Cobertura_salud ~ .) + 
  scale_color_manual(values = c("forestgreen", "dodgerblue4"),
                     name = "") +
  stat_summary(fun = median,
               fun.min = function(z) { quantile(z, 0.25)},
               fun.max = function(z) { quantile(z, 0.75)},
               geom = "pointrange", color="firebrick4", linetype = 2, width = 0.5)


## colesterol LDL

ID_LDL <- datos_completos |> 
  filter(is.na(LDL)) |> 
  select(ID) |> 
  distinct() 

datos_completos_LDL <- datos_completos |> 
  anti_join(ID_LDL)

datos_completos_LDL |> 
  group_by(momento) |> 
  shapiro_test(LDL)


datos_completos_LDL |> 
  group_by(momento) |> 
  summarise(Mediana = median(LDL),
            Min = min(LDL),
            Max = max(LDL),
            RIC = IQR(LDL),
            Cuartil_1 = quantile(LDL, probs = 0.25),
            Cuartil_3 = quantile(LDL, probs = 0.75),
            n = n())

datos_completos_LDL |> 
  group_by(momento, Cobertura_salud) |> 
  summarise(Mediana = median(LDL),
            Min = min(LDL),
            Max = max(LDL),
            RIC = IQR(LDL),
            Cuartil_1 = quantile(LDL, probs = 0.25),
            Cuartil_3 = quantile(LDL, probs = 0.75),
            n = n())


datos_completos_LDL |> 
  pairwise_wilcox_test(formula = LDL ~ momento, paired = T)

rank_biserial(x = LDL ~ momento, 
              data = datos_completos_LDL,
              paired = T)

interpret_rank_biserial(-0.18)


datos_completos_LDL |> 
  filter(Cobertura_salud == "Sin cobertura") |> 
  pairwise_wilcox_test(formula = LDL ~ momento, paired = T)

rank_biserial(x = LDL ~ momento, 
              data = datos_completos_LDL[datos_completos_LDL$Cobertura_salud=="Sin cobertura",], 
              paired = T)

interpret_rank_biserial(-0.18)


datos_completos_LDL |> 
  filter(Cobertura_salud == "Con cobertura") |> 
  pairwise_wilcox_test(formula = LDL ~ momento, paired = T)

rank_biserial(x = LDL ~ momento, 
              data = datos_completos_LDL[datos_completos_LDL$Cobertura_salud=="Con cobertura",], 
              paired = T)

interpret_rank_biserial(-0.18)

datos_completos_LDL |> 
  ggplot(aes(x = momento, y = LDL)) +
  geom_violin(scale = "width") +
  geom_sina(aes(color = momento), alpha = 0.6) +
  facet_wrap(Cobertura_salud ~ .) + 
  scale_color_manual(values = c("forestgreen", "dodgerblue4"),
                     name = "") +
  stat_summary(fun = median,
               fun.min = function(z) { quantile(z, 0.25)},
               fun.max = function(z) { quantile(z, 0.75)},
               geom = "pointrange", color="firebrick4", linetype = 2, width = 0.5)

### indice albumina

summary(datos_completos$IAC) 


ID_IAC <- datos_completos |> 
  filter(is.na(IAC)) |> 
  select(ID) |> 
  distinct() 

datos_completos_IAC <- datos_completos |> 
  anti_join(ID_IAC)

datos_completos_IAC |> 
  group_by(momento) |> 
  shapiro_test(IAC)

datos_completos_IAC |> 
  group_by(momento) |> 
  summarise(Mediana = median(IAC),
            Min = min(IAC),
            Max = max(IAC),
            RIC = IQR(IAC),
            Cuartil_1 = quantile(IAC, probs = 0.25),
            Cuartil_3 = quantile(IAC, probs = 0.75),
            n = n())

datos_completos_IAC |> 
  group_by(momento, Cobertura_salud) |> 
  summarise(Mediana = median(IAC),
            Min = min(IAC),
            Max = max(IAC),
            RIC = IQR(IAC),
            Cuartil_1 = quantile(IAC, probs = 0.25),
            Cuartil_3 = quantile(IAC, probs = 0.75),
            n = n())

datos_completos_IAC |> 
  pairwise_wilcox_test(formula = IAC ~ momento, paired = T)

rank_biserial(x = IAC ~ momento, 
              data = datos_completos_IAC,
              paired = T)

interpret_rank_biserial(-0.24)


datos_completos_IAC |> 
  filter(Cobertura_salud == "Sin cobertura") |> 
  pairwise_wilcox_test(formula = IAC ~ momento, paired = T)

rank_biserial(x = IAC ~ momento, 
              data = datos_completos_IAC[datos_completos_IAC$Cobertura_salud=="Sin cobertura",], 
              paired = T)

interpret_rank_biserial(-0.27)

datos_completos_IAC |> 
  filter(Cobertura_salud == "Con cobertura") |> 
  pairwise_wilcox_test(formula = IAC ~ momento, paired = T)

rank_biserial(x = IAC ~ momento, 
              data = datos_completos_IAC[datos_completos_IAC$Cobertura_salud=="Con cobertura",], 
              paired = T)

interpret_rank_biserial(-0.16)

datos_completos_IAC |> 
  ggplot(aes(x = momento, y = IAC)) +
  geom_violin(scale = "width") +
  geom_sina(aes(color = momento), alpha = 0.6) +
  facet_wrap(Cobertura_salud ~ .) + 
  scale_color_manual(values = c("forestgreen", "dodgerblue4"),
                     name = "") +
  stat_summary(fun = median,
               fun.min = function(z) { quantile(z, 0.25)},
               fun.max = function(z) { quantile(z, 0.75)},
               geom = "pointrange", color="firebrick4", linetype = 2, width = 0.5)

### Fondo de ojo

datos_completos |> count(fondo_categor)

datos_completos <- datos_completos |> 
  mutate(fondo_dico = case_when(
    fondo_categor == "NORMAL" ~ "Normal",
    fondo_categor != "NORMAL" ~ "Anormal",
    TRUE ~ NA_character_
  ))


datos_completos |> count(momento, fondo_dico)



ID_fondo <- datos_completos |> 
  filter(is.na(fondo_dico)) |> 
  select(ID) |> 
  distinct() 

datos_completos_fondo <- datos_completos |> 
  anti_join(ID_fondo)

datos_completos_fondo |>  tabyl(fondo_dico, momento) |> 
  adorn_percentages(denominator = "col") |> 
  adorn_pct_formatting() 


datos_completos_fondo |> 
  pairwise_mcnemar_test(fondo_dico ~ momento | ID, correct = F)

datos_completos_fondo |> count(momento)


## Glucemia


summary(datos_completos$glucemia) 


ID_glucemia <- datos_completos |> 
  filter(is.na(glucemia)) |> 
  select(ID) |> 
  distinct() 

datos_completos_glucemia <- datos_completos |> 
  anti_join(ID_glucemia)

datos_completos_glucemia |> 
  group_by(momento) |> 
  shapiro_test(glucemia)

datos_completos_glucemia |> 
  group_by(momento) |> 
  summarise(Mediana = median(glucemia),
            Min = min(glucemia),
            Max = max(glucemia),
            RIC = IQR(glucemia),
            Cuartil_1 = quantile(glucemia, probs = 0.25),
            Cuartil_3 = quantile(glucemia, probs = 0.75),
            n = n())

datos_completos_glucemia |> 
  group_by(momento, Cobertura_salud) |> 
  summarise(Mediana = median(glucemia),
            Min = min(glucemia),
            Max = max(glucemia),
            RIC = IQR(glucemia),
            Cuartil_1 = quantile(glucemia, probs = 0.25),
            Cuartil_3 = quantile(glucemia, probs = 0.75),
            n = n())

datos_completos_glucemia |> 
  pairwise_wilcox_test(formula = glucemia ~ momento, paired = T)

rank_biserial(x = glucemia ~ momento, 
              data = datos_completos_glucemia,
              paired = T)

interpret_rank_biserial(-0.31)


datos_completos_glucemia |> 
  filter(Cobertura_salud == "Sin cobertura") |> 
  pairwise_wilcox_test(formula = glucemia ~ momento, paired = T)

rank_biserial(x = glucemia ~ momento, 
              data = datos_completos_glucemia[datos_completos_glucemia$Cobertura_salud=="Sin cobertura",], 
              paired = T)

interpret_rank_biserial(-0.12)


datos_completos_glucemia |> 
  filter(Cobertura_salud == "Con cobertura") |> 
  pairwise_wilcox_test(formula = glucemia ~ momento, paired = T)

rank_biserial(x = glucemia ~ momento, 
              data = datos_completos_glucemia[datos_completos_glucemia$Cobertura_salud=="Con cobertura",], 
              paired = T)

interpret_rank_biserial(-0.49)


datos_completos_glucemia |> 
  ggplot(aes(x = momento, y = glucemia)) +
  geom_violin(scale = "width") +
  geom_sina(aes(color = momento), alpha = 0.6) +
  facet_wrap(Cobertura_salud ~ .) + 
  scale_color_manual(values = c("forestgreen", "dodgerblue4"),
                     name = "") +
  stat_summary(fun = median,
               fun.min = function(z) { quantile(z, 0.25)},
               fun.max = function(z) { quantile(z, 0.75)},
               geom = "pointrange", color="firebrick4", linetype = 2, width = 0.5)


## Nro Consultas


summary(datos_completos$nro_consultas) 

datos_completos |> 
  group_by(momento) |> 
  shapiro_test(nro_consultas)


datos_completos |> 
  group_by(momento) |> 
  summarise(Mediana = median(nro_consultas),
            Min = min(nro_consultas),
            Max = max(nro_consultas),
            RIC = IQR(nro_consultas),
            Cuartil_1 = quantile(nro_consultas, probs = 0.25),
            Cuartil_3 = quantile(nro_consultas, probs = 0.75),
            n = n())

datos_completos |> 
  group_by(momento, Cobertura_salud) |> 
  summarise(Mediana = median(nro_consultas),
            Min = min(nro_consultas),
            Max = max(nro_consultas),
            RIC = IQR(nro_consultas),
            Cuartil_1 = quantile(nro_consultas, probs = 0.25),
            Cuartil_3 = quantile(nro_consultas, probs = 0.75),
            n = n())

datos_completos |> 
  pairwise_wilcox_test(formula = nro_consultas ~ momento, paired = T)

rank_biserial(x = nro_consultas ~ momento, 
              data = datos_completos_glucemia,
              paired = T)

interpret_rank_biserial(0.63)


datos_completos |> 
  filter(Cobertura_salud == "Sin cobertura") |> 
  pairwise_wilcox_test(formula = nro_consultas ~ momento, paired = T)

rank_biserial(x = nro_consultas ~ momento, 
              data = datos_completos[datos_completos$Cobertura_salud=="Sin cobertura",], 
              paired = T)

interpret_rank_biserial(0.86)


datos_completos |> 
  filter(Cobertura_salud == "Con cobertura") |> 
  pairwise_wilcox_test(formula = nro_consultas ~ momento, paired = T)

rank_biserial(x = nro_consultas ~ momento, 
              data = datos_completos[datos_completos$Cobertura_salud=="Con cobertura",], 
              paired = T)

interpret_rank_biserial(0.44)


datos_completos |> 
  ggplot(aes(x = momento, y = nro_consultas)) +
  geom_violin(scale = "width") +
  geom_sina(aes(color = momento), alpha = 0.6) +
  facet_wrap(Cobertura_salud ~ .) + 
  scale_color_manual(values = c("forestgreen", "dodgerblue4"),
                     name = "") +
  stat_summary(fun = median,
               fun.min = function(z) { quantile(z, 0.25)},
               fun.max = function(z) { quantile(z, 0.75)},
               geom = "pointrange", color="firebrick4", linetype = 2, width = 0.5)


## tratamiento

tto_agregado <- datos_completos |> 
  select(ID, momento, esquema_tto, Cobertura_salud) |> 
  mutate(Freq = 1)

library(ggalluvial)
library(ggrepel)
library(scales)

tto_agregado |> 
  ggplot(aes(x = momento, stratum = esquema_tto, alluvium = ID,
           y = Freq,
           fill = esquema_tto, label = esquema_tto)) +
  scale_x_discrete(expand = c(.1, .1)) +
  geom_stratum(alpha = .5) +
  geom_text(stat = "stratum", aes(label = percent(after_stat(prop), accuracy = .1))) +
  geom_flow() + 
 # guides(fill = "none") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  xlab(label = "Variación del esquema tratamiento") +
  ylab(label = "Frecuencia") + 
  scale_fill_brewer(palette = "Dark2", name = "Esquema")
  
tto_agregado |> 
  ggplot(aes(x = momento, stratum = esquema_tto, alluvium = ID,
             y = Freq,
             fill = esquema_tto, label = esquema_tto)) +
  scale_x_discrete(expand = c(.1, .1)) +
  geom_stratum(alpha = .5) +
  geom_text(stat = "stratum", aes(label = percent(after_stat(prop), accuracy = .1))) +
  geom_flow() + 
  # guides(fill = "none") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  xlab(label = "Variación del esquema tratamiento") +
  ylab(label = "Frecuencia") + 
  scale_fill_brewer(palette = "Dark2", name = "Esquema") +
  facet_wrap(. ~ Cobertura_salud)






ggsave(filename = "Esquema.jpg", 
       device = "jpeg", 
       dpi = 600)

tto_agregado |> 
  ggplot(aes(x = momento, stratum = esquema_tto, alluvium = ID,
             y = Freq,
             fill = esquema_tto, label = esquema_tto)) +
  scale_x_discrete(expand = c(.1, .1)) +
  geom_stratum(alpha = .5) +
  geom_text(stat = "stratum", aes(label = after_stat(n))) +
  geom_flow() + 
  # guides(fill = "none") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  xlab(label = "Variación del esquema tratamiento") +
  ylab(label = "Frecuencia") + 
  scale_fill_brewer(palette = "Dark2", name = "Esquema")


tto_no_tidy <- datos_pre |> 
  inner_join(datos_pan, by = "ID") |> 
  select(ID, esquema_tto.x, esquema_tto.y, Cobertura_salud.x) |> 
  rename(esquema_tto_pre = "esquema_tto.x",
         esquema_tto_pan = "esquema_tto.y",
         Cobertura_salud = "Cobertura_salud.x")

tabla <- xtabs( ~ esquema_tto_pre + esquema_tto_pan, data = tto_no_tidy[tto_no_tidy$Cobertura_salud=="Con cobertura",])

addmargins(tabla, margin = c(1,2), FUN = list(Total = sum))


tabla <- xtabs( ~ esquema_tto_pre + esquema_tto_pan, 
                data = tto_no_tidy[tto_no_tidy$Cobertura_salud=="Sin cobertura",])

addmargins(tabla, margin = c(1,2), FUN = list(Total = sum))

###  grafico HbA1c segun cobertura

theme_set(theme_void(base_family = "Roboto"))

theme_custom <- theme_gray() + theme(
  axis.text.x = element_text(face = "plain", family = "Roboto Mono", size = 30),
  axis.text.y = element_text(face = "plain", family = "Roboto", size = 30),
  axis.title.y = element_text(face = "plain", family = "Roboto", size = 34),
  #panel.grid.major.x = element_line(color = "grey90", size = .6),
  #panel.grid.major.y = element_blank(),
  legend.position = "bottom", 
  legend.text = element_text(family = "Roboto Mono", size = 30),
  legend.title = element_text(face = "bold", size = 30, margin = margin(b = 25)),
  strip.text.x = element_text(size = 36)
)



library(ggforce)

graf_boxplot <- datos_completos |> 
  ggplot(aes(x = momento, y = HbA1c)) +
  geom_boxplot() +
  geom_sina(aes(color = momento), maxwidth = 0.6, 
            scale = "count", 
            seed = 1,
            size = 3, 
            alpha = 0.4) +
  geom_sina(maxwidth = 0.6, 
            scale = "count", 
            seed = 1, 
            size = 3, 
            shape = 1, 
            color = "black", 
            stroke = 0.7) +
  geom_hline(yintercept = 7, color = "red", linetype="dashed", alpha = 0.6) +
  facet_wrap(Cobertura_salud ~ .) + 
  scale_color_manual(values = c("forestgreen", "dodgerblue4"),
                     name = "", guide = F) +
  scale_y_continuous(limits = c(0,15), breaks = seq(0,15, by = 1)) +
  xlab(label = "") +
  theme_custom


ggsave(plot = graf_boxplot, filename = "Boxplot HbA1c segun cobertura.jpg",
       device = "jpeg", 
       dpi = 300, 
       units = "cm", 
       width = 20, 
       height = 15)

graf_violin <- datos_completos |> 
  ggplot(aes(x = momento, y = HbA1c)) +
  geom_violin(width = 1) +
  geom_sina(aes(color = momento), maxwidth = 0.6, 
            scale = "count", 
            seed = 1,
            size = 4, 
            alpha = 0.4) +
  geom_sina(maxwidth = 0.6, 
            scale = "count", 
            seed = 1, 
            size = 4, 
            shape = 1, 
            color = "black", 
            alpha = 0.5,
            stroke = 0.7) +
  geom_hline(yintercept = 7, color = "gray60", linetype="dashed") +
  facet_wrap(Cobertura_salud ~ .) + 
  scale_color_manual(values = c("forestgreen", "dodgerblue4"),
                     name = "", guide = F) +
  scale_y_continuous(limits = c(0,15), breaks = seq(0,15, by = 1)) +
  stat_summary(fun = median,
               fun.min = function(z) { quantile(z, 0.25)},
               fun.max = function(z) { quantile(z, 0.75)},
               geom = "pointrange", color="firebrick4", linetype = 1) +
  xlab(label = "") +
  theme_custom

ggsave(plot = graf_violin, filename = "Violin HbA1c segun cobertura.jpg",
       device = "jpeg", 
       dpi = 300, 
       units = "cm", 
       width = 20, 
       height = 15)




# Comorbilidades

datos_generales %>% filter(Comorbilidades=="Si") %>% 
  count(`ASMA/EPOC`,TBC, HTA, Inmunodeficiencia,
        `Otra Enfermedad cardiovascular`,`Enfermedad oncol?gica`,
        Dislipemia, Otra)




