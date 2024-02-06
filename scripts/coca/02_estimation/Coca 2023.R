#####
#VErsión 2. Coca y REDD+
#Evaluación del proyecto REDD+ en el Pacífico
#####Ensayo GitHub


#Librerias
pacman::p_load(tidyverse,stargazer,modelsummary,haven,lmtest,fastDummies,
               estimatr,magrittr,kableExtra,car, arrow, SparseM, broom, did, lfe)



####Opciones modelsummary
options("modelsummary_format_numeric_latex" = "plain")
f <- function(x) format(round(x, 3), big.mark=",")
gm <- list(
  list("raw" = "nobs", "clean" = "N", "fmt" = 0),
  #list("raw" = "r.squared", "clean"="R$^2$", "fmt" = f),
  #list("raw" = "adj.r.squared", "clean"="R$^2$ Ajustado", "fmt" = f),
  list("raw" = "F_t", "clean"="F","fmt"=f))

# Datos - Estimación TWFE --------------------------------------------

if('carolinacastroosorio'%in%getwd()){
  inputs<-"/Users/carolinacastroosorio/Library/CloudStorage/OneDrive-UniversidaddelosAndes/REDD+ bases/Outputs Bases"
  outputs<-"/Users/carolinacastroosorio/Library/CloudStorage/OneDrive-UniversidaddelosAndes/REDD+ bases/Outputs Estimaciones"
} else{if('santiagoherreragarcia'%in%getwd()){
  inputs<-'/Users/santiagoherreragarcia/Library/CloudStorage/OneDrive-Bibliotecascompartidas:Universidaddelosandes/Carolina Castro Osorio - REDD_CALIMA_VCS_docu/2023/Base_trabajo_Santi/inputs2'
  outputs<-'/Users/santiagoherreragarcia/Library/CloudStorage/OneDrive-Bibliotecascompartidas:Universidaddelosandes/Carolina Castro Osorio - REDD_CALIMA_VCS_docu/2023/Base_trabajo_Santi/outputs2'
} else{if('brigittecastaneda'%in%getwd()){
  inputs<-'/Users/brigittecastaneda/Library/CloudStorage/OneDrive-Universidaddelosandes/REDD+ bases/Inputs'
  outputs<-'/Users/brigittecastaneda/Library/CloudStorage/OneDrive-Universidaddelosandes/REDD+ bases/Outputs Estimaciones'
}}}


inputs<-'/Users/carolinacastroosorio/Library/CloudStorage/OneDrive-UniversidaddelosAndes/REDD+ bases/Outputs Bases'
outputs<-'/Users/carolinacastroosorio/Library/CloudStorage/OneDrive-UniversidaddelosAndes/REDD+ bases/Outputs Estimaciones'
setwd(inputs)


#panel_cc<-read_parquet("base_coca.parquet")
base_coca<-read_parquet("base_coca.parquet")%>%
  select(-c("geometry", "codmpio", "__index_level_0__"))%>%
  as.data.frame()%>%
  relocate(Di, .after=ano)%>%
  relocate(NOMBRE_COM, .after = Di)%>%
  mutate(relative_t=ano-2013)

##########################PISTAS
# Usar la función unique para ver los valores únicos de mi_variable
  valores_unicos <- unique(base_coca$NOMBRE_COM)
  
# Imprimir los valores únicos
  print(valores_unicos)
################################
### Breves descriptivas-coca
base_coca<-read_parquet("base_coca.parquet")%>%
  select(-c("geometry", "codmpio", "__index_level_0__"))%>%
  mutate(CC=ifelse(is.na(NOMBRE_COM) | NOMBRE_COM == 0,"Fuera de Consejos Comunitarios","Consejos Comunitarios"))%>%
  as.data.frame()

stats_2013<-base_coca%>%
  subset(ano==2013)

stats_2013_cc<-base_coca%>%
  subset(ano==2013&CC=="Consejos Comunitarios")

mean2013<-stats_2013%>%
  group_by(CC)%>%
  summarise(totcoca=sum(areacoca))


stats_2013_cc<-base_coca%>%
  subset(ano==2013&CC=="Consejos Comunitarios")

stats_2018_cc<-base_coca%>%
  subset(ano==2018&CC=="Consejos Comunitarios")
mean2018<-stats_2018_cc%>%
  group_by(Di)%>%
  summarise(totcoca=sum(areacoca))

conteo_por_cc <- table(stats_2013$CC)
print(conteo_por_cc)




stats_2021<-base_coca%>%
  subset(ano==2021)

base_coca2012 <- base_coca_cs %>%
  filter(ano >= 2001 & ano <= 2012)

# Filtra las filas para incluir solo los años 2018 para hacer un promedio y sacar quantiles
base_coca2018 <- base_coca %>%
  filter(ano ==2018)

# Calcula el promedio del área para cada id
promedio <- base_coca2012_20 %>%
  group_by(id, CC) %>%
  summarize(promedio_area = mean(areacoca, na.rm = TRUE))

# Calcula el promedio del área para cada NOMBRE_COM
promedio2<- base_coca2012 %>%
  group_by(NOMBRE_COM) %>%
  summarize(promedio_area = mean(areacoca, na.rm = TRUE))

################################
#OJOJOJO xq sólo aparecen datos de 134.285 puntos cuaando tenemos 2819985 puntos?
#porque estamos tomando solo los valores de un ano especifico, 134285*21 anos = 2819985
################################

base_coca<-read_parquet("base_coca.parquet")%>%
  select(-c("geometry", "codmpio", "__index_level_0__"))%>%
  mutate(CC=ifelse(is.na(NOMBRE_COM) | NOMBRE_COM == 0,"Fuera de Consejos Comunitarios","Consejos Comunitarios"))%>%
  as.data.frame()

stats_2013<-base_coca%>%
  subset(ano==2013)

dif_area_2013<-ggplot(stats_2013)+
  geom_histogram(aes(x=areacoca, fill=CC, col=CC), binwidth = 1, alpha=0.6, position = "identity")+
  scale_x_continuous(limits = c(2,50))+
  labs(x="Hectáreas de coca por km2", y="Frecuencia")+
  scale_fill_viridis_d()+
  scale_color_viridis_d()+
  #scale_color_manual(values=c("purple", "yellow"))+
  #scale_fill_manual(values=c("purple", "yellow"))+
  theme_minimal()+
  theme(legend.title = element_blank(), legend.position = c(0.9, 0.9),
        legend.background = element_rect(linewidth =0.2, linetype="solid", 
                                         colour ="black"))

dif_area_2021<-ggplot(stats_2021)+
  geom_histogram(aes(x=areacoca, fill=CC, col=CC), binwidth = 1, alpha=0.6, position = "identity")+
  scale_x_continuous(limits = c(2,50))+
  labs(x="Hectáreas de coca por km2", y="Frecuencia")+
  scale_fill_viridis_d()+
  scale_color_viridis_d()+
  #scale_color_manual(values=c("purple", "yellow"))+
  #scale_fill_manual(values=c("purple", "yellow"))+
  theme_minimal()+
  theme(legend.title = element_blank(), legend.position = c(0.9, 0.9),
        legend.background = element_rect(linewidth = 0.2, linetype="solid", 
                                         colour ="black"))

line_coca_cons<-base_coca%>%
  group_by(ano, CC)%>%
  summarise(meancoca=mean(areacoca))%>%
  ggplot()+
  geom_line(aes(y=meancoca, col=CC, ano))+
  labs(x="Año", y="Promedio de Hectáreas de coca por km2")+
  scale_color_viridis_d()+
  theme_minimal()+
  theme(legend.title = element_blank(), legend.position = c(0.22, 0.8),
        legend.background = element_rect(linewidth = 0.2, linetype="solid", 
                                         colour ="black"))


print(line_coca_cons)

setwd(outputs)
ggsave("Trayectorias coca Pacifico2.png",line_coca_cons, width = 15, height = 8, units = "cm")

line_contarm_cons<-base_coca%>%
  group_by(ano, CC)%>%
  summarise(contarm=mean(cont_arm, na.rm=T))%>%
  ggplot()+
  geom_line(aes(y=contarm, col=CC, ano))+
  labs(x="Año", y="Promedio de contactos armados")+
  scale_color_viridis_d()+
  theme_minimal()+
  theme(legend.title = element_blank(), legend.position = c(0.1, 0.9),
        legend.background = element_rect(linewidth = 0.2, linetype="solid", 
                                         colour ="black"))

print(line_contarm_cons)
variables <- names(base_coca)
print(variables)


#2013 medias

bal_vars_2013<-as.data.frame(stats_2013[,c(colnames(base_coca)[42:48])])
datasummary_balance(~CC, bal_vars_2013, fmt=2,dinm_statistic="p.value", 
                    output=paste0(outputs, "/Dif medias 2013.tex"))


#2021 medias
bal_vars_2021<-as.data.frame(stats_2021[,c(colnames(base_coca)[42:48])])
datasummary_balance(~CC, bal_vars_2021, fmt=2,dinm_statistic="p.value", 
                    output=paste0(outputs, "/Dif medias 2021.tex"))




#Trayectorias tratados y no tratados-coca
tray<-base_coca%>%
  group_by(ano, Di)%>%
  summarise(mean_coca=mean(areacoca))%>%
  mutate(Di=ifelse(Di==1,"CC con REDD+ (Tratados)", "CC sin REDD+ (No Tratados)"))

tray_plot<-ggplot(tray)+
  geom_line(aes(x=ano, y=mean_coca, color=Di))+
  scale_color_manual(values=c("purple", "black"))+
  geom_vline(xintercept = 2013, color="red")+
  labs(y="Promedio de hectáreas de coca por km2", x="Año")+
  theme_minimal()+
  theme(legend.title = element_blank(), legend.position = c(0.2, 0.8),
        legend.background = element_rect(linewidth =0.2, linetype="solid", 
                                         colour ="black"))
print(tray_plot)
setwd(outputs)
ggsave("Trayectorias coca CC2.png",tray_plot, width = 15, height = 8, units = "cm")

# Misma grafica, pero con incluyendo los que están fuera de consejos comunitarios
# Filtra los datos para incluir solo los puntos fuera de consejos comunitarios
tray_fuera <- base_coca%>% filter(CC == "Fuera de Consejos Comunitarios")%>%
  group_by(ano, Di)%>%
  summarise(mean_coca=mean(areacoca))%>%
  mutate(Di=ifelse(Di==1,"CC con REDD+ (Tratados)", "CC sin REDD+ (No Tratados)"))

tray_plot_todos<-ggplot()+
  geom_line(data=tray, aes(x=ano, y=mean_coca, color=Di))+
  geom_line(data=tray_fuera, aes(x=ano, y=mean_coca, color=Di), linetype = "dashed") + # Línea adicional para fuera de consejos comunitarios
  scale_color_manual(values=c("purple", "black"))+
  geom_vline(xintercept = 2011, color="red")+
  labs(y="Promedio de hectáreas de coca por km2", x="Año")+
  theme_minimal()+
  theme(legend.title = element_blank(), legend.position = c(0.2, 0.8),
        legend.background = element_rect(linewidth =0.2, linetype="solid", 
                                         colour ="black"))

  
print(tray_plot_todos)
setwd(outputs)
ggsave("Trayectorias coca CCyotros.png",tray_plot, width = 15, height = 8, units = "cm")

#Trayectorias tratados y no tratados-deforestación
tray<-base_coca%>%
  group_by(ano, Di)%>%
  summarise(mean_defo=mean(deforestacion_ha))%>%
  mutate(Di=ifelse(Di==1,"CC con REDD+ (Tratados)", "CC sin REDD+ (No Tratados)"))

tray_plot<-ggplot(tray)+
  geom_line(aes(x=ano, y=mean_defo, color=Di))+
  scale_color_manual(values=c("gray35", "black"))+
  geom_vline(xintercept = 2013, color="red")+
  labs(y="Promedio de hectáreas deforestadas por km2", x="Año")+
  theme_minimal()+
  theme(legend.title = element_blank(), legend.position = c(0.2, 0.8),
        legend.background = element_rect(size=0.2, linetype="solid", 
                                         colour ="black"))

setwd(outputs)
ggsave("Trayectorias defo CC2.png",tray_plot, width = 15, height = 8, units = "cm")

#Dummies de periodo
base_coca<-dummy_cols(base_coca, "relative_t")
colnames(base_coca)<-gsub("t_-", "lead_", colnames(base_coca))
colnames(base_coca)<-gsub("t_", "lag_", colnames(base_coca))
colnames(base_coca)<-gsub("lag_0", "t_0", colnames(base_coca))

#write_csv(panel_cc, "panel_cc_ll.csv")

#Interactuar Di con las demás variables
base_coca<-base_coca%>%
  mutate_at(vars(matches("relative_")), ~Di*.)

#Formula
x_names<-colnames(panel_cc)[7:35]
dyn_names<-colnames(panel_cc)[38:56]
r_names_for<-as.formula(paste("areacoca ~ Evap_tavg_Q1", paste0("+",x_names, collapse = ""), 
                              paste0("+", dyn_names, collapse = ""), " | NOMBRE_COM + ano | 0 | NOMBRE_COM"))

f_x<-as.formula(paste("~Evap_tavg_Q1", paste0("+",x_names, collapse = "")))
alt_f_dyn<-as.formula(paste("areacoca~", dyn_names[1],paste0("+",dyn_names[2:19], collapse = ""),
                                                             " | NOMBRE_COM + ano | 0 | NOMBRE_COM"))

#estimaciones dinámicas
twfe_treat<-felm(formula = r_names_for,
               data = panel_cc)


twfe_alt_dyn<-felm(formula = alt_f_dyn,
                 data = panel_cc)


#Estimaciones no dinámicas (TWFE normal)
panel_cc1<-panel_cc%>%
  mutate(post=ifelse(ano<=2013, 0,1))

nond_x<-as.formula(paste("areacoca~Di:post+Evap_tavg_Q1", paste0("+",x_names, collapse = ""),
                         " | NOMBRE_COM + ano | 0 | NOMBRE_COM"))
nond_nx<-as.formula(paste("areacoca~Di:post"," | NOMBRE_COM + ano | 0 | NOMBRE_COM")) 

#Controles
twfe_nond_x<-felm(formula = nond_x,
                  data=panel_cc1)

#Sin Controles
twfe_nond_nx<-felm(formula = nond_nx,
                data=panel_cc1)

#Exportar usando modelsummary (Sólo el coeficiente principal para el texto, y la tabla completa
#con los controles para los anexos)
cf_map1<-c("Di:post"="$D_{ct}$: Participación en REDD+")
cf_map2<-c("Di:post"="$D_{ct}$: Participación en REDD+", "Evap_tavg_Q1"="Evapotranspiración (Trimestre 1)",           
           "Evap_tavg_Q2"="Evapotranspiración (Trimestre 2)", "Evap_tavg_Q3"="Evapotranspiración (Trimestre 3)",  "Evap_tavg_Q4"="Evapotranspiración (Trimestre 4)",        
           "Qair_f_tavg_Q1"="Humedad (Trimestre 1)", "Qair_f_tavg_Q2"="Humedad (Trimestre 2)","Qair_f_tavg_Q3"= "Humedad (Trimestre 3)", "Qair_f_tavg_Q4"="Humedad (Trimestre 4)",        
           "Rainf_f_tavg_Q1"="Precipitación (Trimestre 1)", "Rainf_f_tavg_Q2"="Precipitación (Trimestre 2)","Rainf_f_tavg_Q3"="Precipitación (Trimestre 3)", "Rainf_f_tavg_Q4"="Precipitación (Trimestre 4)",       
           "Tair_f_tavg_Q1"="Temperatura (Trimestre 1)", "Tair_f_tavg_Q2"="Temperatura (Trimestre 2)", "Tair_f_tavg_Q3"= "Temperatura (Trimestre 3)", "Tair_f_tavg_Q4"="Temperatura (Trimestre 4)",        
           "Wind_f_tavg_Q1"="Velocidad Viento (Trimestre 1)", "Wind_f_tavg_Q2"="Velocidad Viento (Trimestre 2)", "Wind_f_tavg_Q3"="Velocidad Viento (Trimestre 3)", "Wind_f_tavg_Q4"="Velocidad Viento (Trimestre 4)",        
           "SoilMoi10_40cm_tavg_Q1"="Humedad del suelo (Trimestre 1)", "SoilMoi10_40cm_tavg_Q2"="Humedad del suelo (Trimestre 2)","SoilMoi10_40cm_tavg_Q3"="Humedad del suelo (Trimestre 3)", "SoilMoi10_40cm_tavg_Q4"="Humedad del suelo (Trimestre 4)",
           "terrorismot"="Actos de terrorismo", "secuestros"="Secuestros", "acc_subversivas"="Acciones Subversivas", "conlag_arm"="Contactos Armados",            
           "errad_manual"="Erradicación Manual (Ha por Municipio)", "homicidios"="Homicidios")             

output_twfe<-modelsummary(list("Con Controles"=twfe_nond_x, "Sin Controles"=twfe_nond_nx), 
                     output = "latex",
                     stars = c("*"=0.1, "**"=0.05, "***"=0.01),
                     coef_map = cf_map1, 
                     gof_map = gm,
                     escape = F)%>%
  add_header_above(c(" " = 1, "Var. Dep.: Área de Coca Cultivada (Ha)" =2))%>%
  add_footnote("\\textit{Errores Estándar Clusterizados por Consejo Comunitario.}", escape = F, notation = "alphabet")%>%
  add_footnote("\\textit{En ambas estimaciones se usan efectos fijos por Consejo Comunitario y año.}", escape = F, notation = "alphabet")
  
output_twfe_anexos<-modelsummary(list("Con Controles"=twfe_nond_x), 
                          output = "latex",
                          stars = c("*"=0.1, "**"=0.05, "***"=0.01),
                          coef_map = cf_map2, 
                          gof_map = gm,
                          escape = F)%>%
  add_header_above(c(" " = 1, "Var. Dep.: Área de Coca Cultivada (Ha)" =1))%>%
  add_footnote("\\textit{Errores Estándar Clusterizados por Consejo Comunitario.}", escape = F, notation = "alphabet")%>%
  add_footnote("\\textit{Efectos fijos por Consejo Comunitario y año.}", escape = F, notation = "alphabet")


save_kable(output_twfe, file.path(outputs,"TWFE Estatico.tex"))
save_kable(output_twfe_anexos, file.path(outputs,"TWFE Estatico Anexos.tex"))

#Grafica de efectos leads y lags

#Con controles 
twfe_plot<-tidy(twfe_treat, conf.int = T, conf.level = 0.95)%>%
  as.data.frame()%>%
  subset(term%in%dyn_names)
twfe_plot$relative_t<-c(-2:-12, 0, 1:7)
twfe_plot<-twfe_plot%>%
  mutate(treat_status=ifelse(relative_t<=0, "pre", "post"))

twfe_g<-ggplot(twfe_plot)+
  geom_point(aes(y=estimate, x=relative_t, color=treat_status))+
  scale_color_manual(values=c("black", "gray35"))+
  geom_vline(xintercept = 0, linetype="solid", color="red")+
  geom_hline(yintercept = 0, linetype="dashed", color="black")+
  geom_linerange(aes(x=relative_t, ymin=conf.low, ymax=conf.high, color=treat_status))+
  labs(x="Periodo relativo a la llegada de REDD+\n(2013)", y="Coeficientes DID")+
  theme_minimal()+
  theme(legend.position="none")

#Sin controles
twfe_plot2<-tidy(twfe_alt_dyn, conf.int = T, conf.level = 0.95)%>%
  as.data.frame()%>%
  subset(term%in%dyn_names)
twfe_plot2$relative_t<-c(-2:-12, 0, 1:7)
twfe_plot2<-twfe_plot2%>%
  mutate(treat_status=ifelse(relative_t<=0, "pre", "post"))

twfe_g2<-ggplot(twfe_plot2)+
  geom_point(aes(y=estimate, x=relative_t, color=treat_status))+
  scale_color_manual(values=c("black", "gray35"))+
  geom_vline(xintercept = 0, linetype="solid", color="red")+
  geom_hline(yintercept = 0, linetype="dashed", color="black")+
  geom_linerange(aes(x=relative_t, ymin=conf.low, ymax=conf.high, color=treat_status))+
  labs(x="Periodo relativo a la llegada de REDD+\n(2013)", y="Coeficientes DID")+
  theme_minimal()+
  theme(legend.position="none")

setwd(outputs)
ggsave("TWFE Dinamicos con Controles.png",twfe_g, width = 10, height = 8, units = "cm")
ggsave("TWFE Dinamicos sin Controles.png",twfe_g2, width = 10, height = 8, units = "cm")


# Callaway Sant'Anna ------------------------------------------------------

#Para estimar este modelo hay que hacer unos cambios a los datos

cohorte_2009=c("La Cuenca Del Río Tolo Y Zona Costera Sur")
cohorte_2011=c("General De La Costa Pacífica Del Norte - Los Delfines")
cohorte_2014=c("Cuenca Baja Del Río Calima", "Bajo Mira Y Frontera", "Acapa","Consejo Comunitario De Chicao","Consejo Comunitario De La Madre",
               "Apartadó Buenavista", "Río Domingodó", "Vígia De Curvaradó Y Santa Rosa De Limón", "Río Montaño",
               "Río Pepe", "Río Baudó  Acaba","Bahía Málaga -La Plata", "El Río Cajambre", "Concosta", "Sivirù","San Andrés De Usaragá", "Pizarro", "Río  Pilizá" )
cohorte_2015=c('Mayor Del Cantón De San Pablo "Acisanp"', 'Consejo Comunitario Integral De Lloro_Cocoillo', "Cértegui", 'General Del Municipìo De Nuquí - Los Riscales')
cohorte_2016=c('Rio Naya', "Mator De Novita", 'Mayor Del Municipio De Juradó')
cohorte_2017=c('Mayor Del Medio Atrato - Acia')
cohorte_2018=c('Alto Guapi')
cohorte_2019=c("Mayor Del Municipio De Condoto")
cohorte_2020=c('Pedeguita Y Mancilla','Río Jiguamiandó')


base_coca_cs<-base_coca%>%
  mutate(timing=ifelse(dumm_2009==1,2009,NA))%>%
  mutate(timing=ifelse(dumm_2011==1,2011,timing))%>%
  mutate(timing=ifelse(dumm_2014==1,2014,timing))%>%
  mutate(timing=ifelse(dumm_2015==1,2015,timing))%>%
  mutate(timing=ifelse(dumm_2016==1,2016,timing))%>%
  mutate(timing=ifelse(dumm_2017==1,2017,timing))%>%
  mutate(timing=ifelse(dumm_2018==1,2018,timing))%>%
  mutate(timing=ifelse(dumm_2019==1,2019,timing))%>%
  mutate(timing=ifelse(dumm_2020==1,2020,timing))%>%
  mutate(timing=ifelse(is.na(timing),0,timing))

#Borramos mientras los missing de Di, esto para el event study, pero ojo
#para el RD espacial sí nos sirven
base_coca_cs<-base_coca_cs%>%
  drop_na(Di)
base_coca_cs<-base_coca_cs%>%
  drop_na(NOMBRE_COM)

unique_values=unique(base_coca_cs$areacoca)
print(unique_values)
#Sin Controles
atts1 <- att_gt(yname = "areacoca", 
               tname = "ano", 
               idname = "id",
               gname = "timing", 
               data = base_coca_cs,
               est_method = "dr",
               clustervars = c("NOMBRE_COM"),
              control_group = c("nevertreated")) 



atts2 <- att_gt(yname = "areacoca", 
                tname = "ano", 
                idname = "id",
                gname = "timing", 
                data = base_coca_cs,
                est_method = "dr", 
                clustervars = c("NOMBRE_COM"),
                control_group = c("notyettreated")) 

agg_att1 <- aggte(atts1, type = "dynamic") #Encontramos los efectos dinámicos
summary(agg_att1)

agg_att2 <- aggte(atts2, type = "dynamic") #Encontramos los efectos dinámicos
summary(agg_att2)

csdid_plot1<-ggdid(agg_att1, theming=F)+
  labs(title = "")+
  scale_color_manual(values=c("gray35", "black"))+
  geom_vline(xintercept = 0, linetype="solid", color="red")+
  geom_hline(yintercept = 0, linetype="dashed", color="black")+
  ylab("Efectos Estimados\n Callaway-Sant'Anna")+
  xlab("Periodo relativo a la llegada de REDD+\n(2009, 2011, 2014, 2015, 2016, 2017, 2018, 2019)")+
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 0), legend.position="none")

print(csdid_plot1)

csdid_plot2<-ggdid(agg_att2, theming=F)+
  labs(title = "")+
  scale_color_manual(values=c("gray35", "black"))+
  geom_vline(xintercept = 0, linetype="solid", color="red")+
  geom_hline(yintercept = 0, linetype="dashed", color="black")+
  ylab("Efectos Estimados\n Callaway-Sant'Anna")+
  xlab("Periodo relativo a la llegada de REDD+\n(2009, 2011, 2014, 2015, 2016, 2017, 2018, 2019)")+
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 0), legend.position="none")

ggsave("Estimadores Callaway SantAnna2.png",csdid_plot1, width = 15, height = 8, units = "cm")
ggsave("Estimadores Callaway SantAnna2 (notyettreated).png",csdid_plot2, width = 15, height = 8, units = "cm")


#OJOJOJO el profe de acá sugiere efectos estimados agregados

#Descriptivas
unique(base_coca_cs$NOMBRE_COM)

# Para contar las observaciones con Di==1 y Di==0
observaciones_Di_1 <- sum(base_coca_cs$Di == 1)





test<-base_coca_cs %>% filter(Di==0,ano==2010)

observaciones_Di_0 <- sum(base_coca_cs$Di == 0)

# Imprimir los resultados
cat("Observaciones con Di==1:", observaciones_Di_1, "\n")
cat("Observaciones con Di==0:", observaciones_Di_0, "\n")
# Pixeles Di=0: 698460 di=1: 450618 como cada pixel es de 1km2 eso es 698460km2 mno tratados


#Borramos mientras los missing de Di, esto para el event study, pero ojo
#para el RD espacial sí nos sirven
panel_cc_cs<-panel_cc_cs%>%
  drop_na(Di)

#Sin Controles
atts_deforesta <- att_gt(yname = "deforestacion_ha", 
                tname = "ano", 
                idname = "id_left",
                gname = "timing", 
                data = panel_cc_cs,
                est_method = "dr", 
                clustervars = c("NOMBRE_COM"),
                control_group = c("nevertreated")) 

agg_att_deforesta <- aggte(atts_deforesta, type = "dynamic") #Encontramos los efectos dinámicos
summary(agg_att1)

csdid_plot_deforesta<-ggdid(agg_att_deforesta, theming=F)+
  labs(title = "")+
  scale_color_manual(values=c("gray35", "black"))+
  geom_vline(xintercept = 0, linetype="solid", color="red")+
  geom_hline(yintercept = 0, linetype="dashed", color="black")+
  ylab("Efectos Estimados\n Callaway-Sant'Anna")+
  xlab("Periodo relativo a la llegada de REDD+\n(2009, 2011, 2014, 2015, 2016, 2017, 2018, 2019)")+
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 0), legend.position="none")

#Not yet treated
atts_deforesta1.1 <- att_gt(yname = "deforestacion_ha", 
                         tname = "ano", 
                         idname = "id",
                         gname = "timing", 
                         data = panel_cc_cs,
                         est_method = "dr", 
                         clustervars = c("NOMBRE_COM"),
                         control_group = c("notyettreated")) 

agg_att_deforesta1.1 <- aggte(atts_deforesta1.1, type = "dynamic") #Encontramos los efectos dinámicos
summary(agg_att_deforesta1.1)

csdid_plot_deforesta1.1<-ggdid(agg_att_deforesta1.1, theming=F)+
  labs(title = "")+
  scale_color_manual(values=c("gray35", "black"))+
  geom_vline(xintercept = 0, linetype="solid", color="red")+
  geom_hline(yintercept = 0, linetype="dashed", color="black")+
  ylab("Efectos Estimados\n Callaway-Sant'Anna")+
  xlab("Periodo relativo a la llegada de REDD+\n(2009, 2011, 2014, 2015, 2016, 2017, 2018, 2019)")+
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 0), legend.position="none")

ggsave("Estimadores Callaway SantAnna2_defo (notyettreated).png",csdid_plot_deforesta1.1, width = 15, height = 8, units = "cm")

#Otros Callaway
# Filtra las filas para incluir solo los años 2001 a 2012 para hacer un promedio y sacar quantiles
base_coca2012 <- base_coca_cs %>%
  filter(ano >= 2013 & ano <= 2021)

base_coca2012_trata <- base_coca_cs %>%
  filter(ano <= timing)
promedio <- base_coca2012 %>%
  summarize(promedio_area = mean(areacoca, na.rm = TRUE))
# Calcula el promedio del área para cada id
promedio <- base_coca2012 %>%
  group_by(Di) %>%
  summarize(promedio_area = mean(areacoca, na.rm = TRUE))

promedio2 <- base_coca2012_trata %>%
  summarize(promedio_area = mean(areacoca, na.rm = TRUE))

# Calcula el promedio del área para cada NOMBRE_COM
promedio2<- base_coca2012 %>%
  group_by(NOMBRE_COM) %>%
  summarize(promedio_area = mean(areacoca, na.rm = TRUE))

# Calcular los cuantiles del vector promedio_area
quantiles_promedio <- quantile(promedio2$promedio_area, probs = c(0.50, 0.75, 0.9))

# Identificar los NOMBRE_COM que están en los dos cuantiles superiores
nombre_com_cuanti_superiores <- promedio2 %>%
  filter(promedio_area >= quantiles_promedio[1])
unique(promedio2$promedio_area)
# Ver los resultados
nombre_com_cuanti_superiores



# Ver los resultados
head(base_filtrada)  # Puedes usar head() para ver las primeras filas de la base filtrada

# Filtrar la base original para dejar solo las observaciones de NOMBRE_COM en nombre_com_cuanti_superiores
base_filtrada <- base_coca_cs%>%
  filter(NOMBRE_COM%in%nombre_com_cuanti_superiores$NOMBRE_COM | Di == 0)
unique(base_filtrada$NOMBRE_COM)
unique(base_filtrada$timing)
unique(base_coca_cs$Di)

atts1 <- att_gt(yname = "areacoca", 
                tname = "ano", 
                idname = "id",
                gname = "timing", 
                data = base_filtrada,
                est_method = "dr",
                clustervars = c("NOMBRE_COM"),
                control_group = c("notyettreated")) 

agg_att <- aggte(atts1, type = "dynamic") #Encontramos los efectos dinámicos
summary(agg_att)

csdid_plot<-ggdid(agg_att, theming=F)+
  labs(title = "")+
  scale_color_manual(values=c("gray35", "black"))+
  geom_vline(xintercept = 0, linetype="solid", color="red")+
  geom_hline(yintercept = 0, linetype="dashed", color="black")+
  ylab("Efectos Estimados\n Callaway-Sant'Anna")+
  xlab("Periodo relativo a la llegada de REDD+\n(2009, 2011, 2014, 2015, 2016, 2017, 2018, 2019)")+
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 0), legend.position="none")
print(csdid_plot)
setwd(outputs)
ggsave("Esti_Calla_mascoca (nevertreated).png",csdid_plot, width = 15, height = 8, units = "cm")



# Calcular los cuantiles del vector promedio_area
quantiles_promedio <- quantile(promedio2$promedio_area, probs = c(0.1, 0.25))

# Identificar los NOMBRE_COM que están en los dos cuantiles inferiores
nombre_com_cuanti_inferiores <- promedio2 %>%
  filter(promedio_area >= quantiles_promedio[1])



# Filtrar la base original para dejar solo las observaciones de NOMBRE_COM en nombre_com_cuanti_superiores
base_filtrada <- base_coca_cs%>%
  filter(NOMBRE_COM %in% nombre_com_cuanti_inferiores$NOMBRE_COM | Di == 0)
unique(base_filtrada$NOMBRE_COM)
unique(base_filtrada$timing)

base_filtrada <- base_coca_cs%>%
  filter(NOMBRE_COM %in% nombre_com_cuanti_inferiores$NOMBRE_COM)

atts1 <- att_gt(yname = "areacoca", 
                tname = "ano", 
                idname = "id",
                gname = "ANO", 
                data = base_filtrada,
                est_method = "dr",
                clustervars = c("NOMBRE_COM"),  
                control_group = c("nevertreated"))
agg_att <- aggte(atts1, type = "dynamic") #Encontramos los efectos dinámicos
summary(agg_att)

csdid_plot<-ggdid(agg_att, theming=F)+
  labs(title = "")+
  scale_color_manual(values=c("gray35", "black"))+
  geom_vline(xintercept = 0, linetype="solid", color="red")+
  geom_hline(yintercept = 0, linetype="dashed", color="black")+
  ylab("Efectos Estimados\n Callaway-Sant'Anna")+
  xlab("Periodo relativo a la llegada de REDD+\n(2009, 2011, 2014, 2015, 2016, 2017, 2018, 2019)")+
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 0), legend.position="none")
print(csdid_plot)


ggsave("Esti_Calla_menoscoca (notyettreated).png",csdid_plot, width = 15, height = 8, units = "cm")
ggsave("Esti_Calla_menoscoca (nevertreated).png",csdid_plot, width = 15, height = 8, units = "cm")

tabla = base_coca_cs[,c(4,5,30,32,40,43,51:59)]
colnames(tabla)

##Deforesta
defo_1km_con_info <- readRDS("~/Library/CloudStorage/OneDrive-UniversidaddelosAndes/REDD+ bases/Outputs Bases/defo_1km_con_info.rds")
defo_1km_con_info$firma <- ifelse(is.na(defo_1km_con_info$firma), 0, defo_1km_con_info$firma)
defo_1km_con_info_2002 <- defo_1km_con_info %>%
  filter(Ano_defo2 > 2000)

atts1 <- att_gt(yname = "Valor_defo", 
                tname = "Ano_defo2", 
                idname = "id",
                gname = "firma", 
                data = defo_1km_con_info_2002,
                est_method = "dr",
                clustervars = c("nombre_com"),  
                control_group = c("nevertreated"))
agg_att <- aggte(atts1, type = "dynamic") #Encontramos los efectos dinámicos
summary(agg_att)

unique(defo_1km_con_info$firma)

csdid_plot<-ggdid(agg_att, theming=F)+
  labs(title = "Deforestación")+
  scale_color_manual(values=c("gray35", "black"))+
  geom_vline(xintercept = 0, linetype="solid", color="red")+
  geom_hline(yintercept = 0, linetype="dashed", color="black")+
  ylab("Efectos Estimados\n Callaway-Sant'Anna")+
  xlab("Periodo relativo a la llegada de REDD+\n(2009, 2011, 2014, 2015, 2016, 2017, 2018, 2019)")+
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 0), legend.position="none")
print(csdid_plot)



###creo tablas 
db_pre_trat = subset(tabla, tabla$ano >= tabla$timing & tabla$timing!=0) #
db_post_trat = subset(tabla, tabla$ano < tabla$timing & tabla$timing!=0) #
db_controles = subset(tabla, tabla$timing==0)

#calculo medias
mean(db_pre_trat$areacoca)
mean(db_post_trat$areacoca)
mean(db_controles$areacoca)




library(arrow)
library(leaflet)
library(sf)

install.packages("arrow")
install.packages("leaflet")
install.packages("sp")

library(arrow)
library(leaflet)
library(sp)

parquet_df <- arrow::read_parquet("your_data.parquet")

# Create an sf object from the centroids column
sf_object <- st_as_sf(base_coca2012, coords = c("centroids"))