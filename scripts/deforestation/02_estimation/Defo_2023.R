#####
#VErsión 1. Defo y REDD+ (análisis a 90*90 IDEAM)
#Evaluación del proyecto REDD+ en el Pacífico
#####

#Librerias
pacman::p_load(tidyverse,stargazer,modelsummary,haven,lmtest,fastDummies,
               estimatr,magrittr,kableExtra,car, arrow, SparseM, broom, did, lfe)



####Directorios
if('carolinacastroosorio'%in%getwd()){
  inputs<-"/Users/carolinacastroosorio/Library/CloudStorage/OneDrive-UniversidaddelosAndes/REDD+ bases/Outputs Bases"
  outputs<-"/Users/carolinacastroosorio/Library/CloudStorage/OneDrive-UniversidaddelosAndes/REDD+ bases/Outputs Estimaciones"
} else{if('santiagoherreragarcia'%in%getwd()){
  inputs<-'/Users/santiagoherreragarcia/Library/CloudStorage/OneDrive-Bibliotecascompartidas:Universidaddelosandes/Carolina Castro Osorio - REDD_CALIMA_VCS_docu/2023/Base_trabajo_Santi/inputs2'
  outputs<-'/Users/santiagoherreragarcia/Library/CloudStorage/OneDrive-Bibliotecascompartidas:Universidaddelosandes/Carolina Castro Osorio - REDD_CALIMA_VCS_docu/2023/Base_trabajo_Santi/outputs2'
} else{if('c.castroo'%in%getwd()){
  inputs<- 'C:\\Users\\c.castroo\\OneDrive - Universidad de los andes\\REDD+ bases\\Outputs Bases'
  outputs<-'C:\\Users\\c.castroo\\OneDrive - Universidad de los andes\\REDD+ bases\\Outputs Estimaciones'
}

panel_defo_CC=read_parquet("C:\\Users\\c.castroo\\OneDrive - Universidad de los andes\\REDD+ bases\\Outputs Bases\\panel_deforestacion_consejos.parquet")

names(panel_defo_CC)
####descriptivas
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


#Borramos mientras los missing de Di, esto para el event study, pero ojo
#para el RD espacial sí nos sirven
panel_defo_CC<-panel_defo_CC%>%
  drop_na(ano)
base_coca_cs<-base_coca_cs%>%
  drop_na(NOMBRE_COM)

unique_values=unique(base_coca_cs$areacoca)
print(unique_values)
#Sin Controles
#ojo year== año de deforestación y ano==a año de firma proyecto REDD
atts1 <- att_gt(yname = "defo", 
                tname = "year", 
                idname = "id",
                gname = "ano", 
                data = panel,
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

