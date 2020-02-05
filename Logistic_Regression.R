
#install.packages("VIM")
####### LIBRERÍAS #######
library(inspectdf)
library(ggplot2)
library(mice)
library(VIM)
library(caret)
library(PerformanceAnalytics)
library(corrplot)
library(GGally)
library(ROCR)
library(InformationValue)
library(ResourceSelection)
library(e1071)
library(streamlineR)
library(glmnet)
library(scorecard)
########################

rm(list=ls(all=TRUE))
morosidad = read.csv("hmeq.csv", header = TRUE, na.strings=c(""," ","NA"))
str(morosidad)
summary(morosidad)

###################### Descripción Variables ###############
#1 BAD: 1 = applicant defaulted on loan or seriously delinquent; 0 = applicant paid loan 
#2 LOAN: Amount of the loan request - NUM
#3 MORTDUE: Amount due on existing mortgage- NUM
#4 VALUE: Value of current property - NUM
#5 REASON: DebtCon = debt consolidation/HomeImp = home improvement- FACTOR 2 niveles
#6 JOB: Occupational categories - FACTOR 6 niveles
#7 YOJ: Years at present job - NUM
#8 DEROG: Number of major derogatory reports. # Reportes de crédito no pagado - NUM
#9 DELINQ: Number of delinquent credit lines. #Lineas de crédito que no ha pagado - NUM
#10 CLAGE: Age of oldest credit line in months - NUM
#11 NINQ: Number of recent credit inquiries/consultas por créditos - NUM
#12 CLNO: Number of credit lines - NUM
#13 DEBTINC: Debt-to-income ratio - NUM
###########################################################

### Revisón NA
# entrega info de datos faltantes en las variables
morosidad %>% inspect_na

# grafica el numero de datos faltantes x variable
morosidad %>% inspect_na %>% show_plot

### Porcentaje de Na
p <- function(x) {sum(is.na(x))/length(x)*100}
apply(morosidad, 2, p)
#apply(morosidad, 1, p)
md.pattern(morosidad)
#md.pairs(morosidad[,-13])

#BAD    LOAN   MORTDUE  VALUE    REASON   JOB       YOJ   DEROG     DELINQ 
#0.00  0.000   8.6912  1.8791   4.2281   4.6812   8.6409  11.8791  9.731544 
#CLAGE    NINQ    CLNO   DEBTINC 
#5.1677  8.5570  3.7248  21.2583 


aggr_plot <- aggr(morosidad, col=c('navyblue','red'), numbers=TRUE, sortVars=TRUE, 
                  labels=names(morosidad), cex.axis=.7, gap=3, 
                  ylab=c("Histogram of missing data","Pattern"))


#Eliminación de filas con mas de un 50% de Na
morosidad_rows = morosidad[-which(rowMeans(is.na(morosidad)) > 0.5),]
head(morosidad_rows)

## Imputación de Datos
set.seed(103)
imputed = mice(morosidad_rows, m = 5)
imputed

#Imputation methods:
#  BAD      LOAN   MORTDUE     VALUE    REASON       JOB       YOJ     DEROG    DELINQ 
#   ""        ""     "pmm"     "pmm"  "logreg" "polyreg"     "pmm"     "pmm"     "pmm" 
#CLAGE      NINQ      CLNO   DEBTINC 
#"pmm"     "pmm"     "pmm"     "pmm" 

#Nuevo data frame con datos imputados
morosidad_imp = complete(imputed,1)

### Revisón NA
# entrega info de datos faltantes en las variables
morosidad_imp %>% inspect_na

# grafica el numero de datos faltantes x variable
morosidad_imp %>% inspect_na %>% show_plot

head(morosidad_rows)
head(morosidad_imp)


## Ingeniería de Variables ##
table(morosidad_imp$BAD)
#0    1 
#4673 1161 

prop.table(table(morosidad_imp$BAD))
#   0         1 
# 0.8009942 0.1990058 

## Revisión frecuencia variables factor
table(morosidad_imp$JOB) #factor
#Mgr  Office   Other ProfExe   Sales    Self 
#786     971    2453    1311     114     199 

table(morosidad_imp$REASON) #factor
#DebtCon HomeImp 
#4019    1815 
table(morosidad_imp$YOJ)
table(morosidad_imp$DEROG)
table(morosidad_imp$DELINQ)
table(morosidad_imp$NINQ)
table(morosidad_imp$CLNO)

morosidad_imp$BAD = as.factor(morosidad_imp$BAD)
str(morosidad_imp)


ggplot(data = morosidad_imp, aes(x = factor(BAD), y = ..count.., fill = factor(BAD))) +
  geom_bar() +
  scale_fill_manual(values = c("Blue", "darkgreen")) +
  labs(title = "Clientes en Morosidad\n", x = "Morosidad", y = "Cantidad") +
  theme_bw() +
  theme(legend.position = c(0.8,0.85))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  theme(plot.title = element_text(hjust = 0.5))



############ 2.- Análisis de Correlaciones

#### Plot correlaciones todas las variables
corrplot(cor(morosidad_imp[,-c(1,5,6)]),type="upper",diag = FALSE,method = "color",
         addCoef.col = "black")

chart.Correlation(morosidad_imp[,-c(1,5,6)])


morosidad_imp[,-c(1,5,6)] %>% 
  inspect_cor() %>%
  show_plot()

ggpairs(morosidad_imp[,c(2:4,7:13,1)], aes(color=BAD, alpha=0.75), 
        lower=list(continuous="smooth"))+ theme_bw()+
  labs(title="Morosidad")+
  theme(plot.title=element_text(face='bold',color='black',hjust=0.5,size=12))


#################### Gráficas Univariadas #####################

# Histograma LOAN
p1 = ggplot(data = morosidad_imp , aes(x=LOAN))+
  geom_histogram(bins = 20, fill="darkgreen", col = "black")+
  labs(title = "Distribución LOAN", x = "LOAN", y = "Frecuencia")+
  theme_bw()+
  scale_x_continuous(breaks = seq(0, 90000, by = 10000))+ 
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())

# Boxplot LOAN
p2 = ggplot(data = morosidad_imp, aes(x="", y=LOAN))+
  geom_boxplot(fill="darkgreen", col = "black")+
  coord_flip()+ theme_bw()+
  scale_y_continuous(breaks = seq(0, 90000, by = 10000))+
  theme(axis.text.x = element_text(angle = 90)) +
  labs(y = "LOAN")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) 

grid.arrange(p1, p2, ncol = 1, nrow = 2)
summary(morosidad_imp$LOAN)


# Histograma MORTDUE
p3 = ggplot(data = morosidad_imp , aes(x=MORTDUE))+
  geom_histogram(bins = 20, fill="darkgreen", col = "black")+
  labs(title = "Distribución MORTDUE", x = "MORTDUE", y = "Frecuencia")+
  theme_bw()+
  scale_x_continuous(breaks = seq(0, 400000, by = 50000))+ 
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())

# Boxplot MORTDUE
p4 = ggplot(data = morosidad_imp, aes(x="", y=MORTDUE))+
  geom_boxplot(fill="darkgreen", col = "black")+
  coord_flip()+ theme_bw()+
  scale_y_continuous(breaks = seq(0, 400000, by = 50000))+
  theme(axis.text.x = element_text(angle = 90)) +
  labs(y = "MORTDUE")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) 

grid.arrange(p3, p4, ncol = 1, nrow = 2)
summary(morosidad_imp$MORTDUE)


# Histograma VALUE
p5 = ggplot(data = morosidad_imp , aes(x=VALUE))+
  geom_histogram(bins = 20, fill="darkgreen", col = "black")+
  labs(title = "Distribución VALUE", x = "VALUE", y = "Frecuencia")+
  theme_bw()+
  scale_x_continuous(breaks = seq(0, 900000, by = 200000))+ 
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())

# Boxplot VALUE
p6 = ggplot(data = morosidad_imp, aes(x="", y=VALUE))+
  geom_boxplot(fill="darkgreen", col = "black")+
  coord_flip()+ theme_bw()+
  scale_y_continuous(breaks = seq(0, 900000, by = 200000))+
  theme(axis.text.x = element_text(angle = 90)) +
  labs(y = "VALUE")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) 

grid.arrange(p5, p6, ncol = 1, nrow = 2)
summary(morosidad_imp$VALUE)


# Histograma YOJ
p7 = ggplot(data = morosidad_imp , aes(x=YOJ))+
  geom_histogram(bins = 20, fill="darkgreen", col = "black")+
  labs(title = "Distribución YOJ", x = "YOJ", y = "Frecuencia")+
  theme_bw()+
  scale_x_continuous(breaks = seq(0, 45, by = 5))+ 
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())

# Boxplot YOJ
p8 = ggplot(data = morosidad_imp, aes(x="", y=YOJ))+
  geom_boxplot(fill="darkgreen", col = "black")+
  coord_flip()+ theme_bw()+
  scale_y_continuous(breaks = seq(0, 45, by = 5))+
  theme(axis.text.x = element_text(angle = 90)) +
  labs(y = "YOJ")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) 

grid.arrange(p7, p8, ncol = 1, nrow = 2)
summary(morosidad_imp$YOJ)



# Histograma DEROG
p9 = ggplot(data = morosidad_imp , aes(x=DEROG))+
  geom_histogram(bins = 10, fill="darkgreen", col = "black")+
  labs(title = "Distribución DEROG", x = "DEROG", y = "Frecuencia")+
  theme_bw()+
  scale_x_continuous(breaks = seq(0, 10, by = 1))+ 
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())

# Boxplot DEROG
p10 = ggplot(data = morosidad_imp, aes(x="", y=DEROG))+
  geom_boxplot(fill="darkgreen", col = "black")+
  coord_flip()+ theme_bw()+
  scale_y_continuous(breaks = seq(0, 10, by = 1))+
  theme(axis.text.x = element_text(angle = 90)) +
  labs(y = "DEROG")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) 

grid.arrange(p9, p10, ncol = 1, nrow = 2)
summary(morosidad_imp$DEROG)

# Histograma DELINQ
p13 = ggplot(data = morosidad_imp , aes(x=DELINQ))+
  geom_histogram(bins = 15, fill="darkgreen", col = "black")+
  labs(title = "Distribución DELINQ", x = "DELINQ", y = "Frecuencia")+
  theme_bw()+
  scale_x_continuous(breaks = seq(0, 15, by = 1))+ 
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())

# Boxplot DELINQ
p14 = ggplot(data = morosidad_imp, aes(x="", y=DELINQ))+
  geom_boxplot(fill="darkgreen", col = "black")+
  coord_flip()+ theme_bw()+
  scale_y_continuous(breaks = seq(0, 15, by = 1))+
  theme(axis.text.x = element_text(angle = 90)) +
  labs(y = "DELINQ")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) 

grid.arrange(p13, p14, ncol = 1, nrow = 2)
summary(morosidad_imp$DELINQ)


# Histograma CLAGE
p11 = ggplot(data = morosidad_imp , aes(x=CLAGE))+
  geom_histogram(bins = 15, fill="darkgreen", col = "black")+
  labs(title = "Distribución CLAGE", x = "CLAGE", y = "Frecuencia")+
  theme_bw()+
  scale_x_continuous(breaks = seq(0, 1200, by = 100))+ 
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())

# Boxplot CLAGE
p12 = ggplot(data = morosidad_imp, aes(x="", y=CLAGE))+
  geom_boxplot(fill="darkgreen", col = "black")+
  coord_flip()+ theme_bw()+
  scale_y_continuous(breaks = seq(0, 1200, by = 100))+
  theme(axis.text.x = element_text(angle = 90)) +
  labs(y = "CLAGE")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) 

grid.arrange(p11, p12, ncol = 1, nrow = 2)
summary(morosidad_imp$CLAGE)


# Histograma NINQ
p15 = ggplot(data = morosidad_imp , aes(x=NINQ))+
  geom_histogram(bins = 15, fill="darkgreen", col = "black")+
  labs(title = "Distribución NINQ", x = "NINQ", y = "Frecuencia")+
  theme_bw()+
  scale_x_continuous(breaks = seq(0, 17, by = 1))+ 
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())

# Boxplot NINQ
p16 = ggplot(data = morosidad_imp, aes(x="", y=NINQ))+
  geom_boxplot(fill="darkgreen", col = "black")+
  coord_flip()+ theme_bw()+
  scale_y_continuous(breaks = seq(0, 17, by = 1))+
  theme(axis.text.x = element_text(angle = 90)) +
  labs(y = "NINQ")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) 

grid.arrange(p15, p16, ncol = 1, nrow = 2)
summary(morosidad_imp$NINQ)


# Histograma CLNO
p17 = ggplot(data = morosidad_imp , aes(x=CLNO))+
  geom_histogram(bins = 15, fill="darkgreen", col = "black")+
  labs(title = "Distribución CLNO", x = "CLNO", y = "Frecuencia")+
  theme_bw()+
  scale_x_continuous(breaks = seq(0, 75, by = 5))+ 
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())

# Boxplot CLNO
p18 = ggplot(data = morosidad_imp, aes(x="", y=CLNO))+
  geom_boxplot(fill="darkgreen", col = "black")+
  coord_flip()+ theme_bw()+
  scale_y_continuous(breaks = seq(0, 75, by = 5))+
  theme(axis.text.x = element_text(angle = 90)) +
  labs(y = "CLNO")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) 

grid.arrange(p17, p18, ncol = 1, nrow = 2)
summary(morosidad_imp$CLNO)


# Histograma DEBTINC
p19 = ggplot(data = morosidad_imp , aes(x=DEBTINC))+
  geom_histogram(bins = 30, fill="darkgreen", col = "black")+
  labs(title = "Distribución DEBTINC", x = "DEBTINC", y = "Frecuencia")+
  theme_bw()+
  scale_x_continuous(breaks = seq(0, 220, by = 20))+ 
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())

# Boxplot DEBTINC
p20 = ggplot(data = morosidad_imp, aes(x="", y=DEBTINC))+
  geom_boxplot(fill="darkgreen", col = "black")+
  coord_flip()+ theme_bw()+
  scale_y_continuous(breaks = seq(0, 220, by = 20))+
  theme(axis.text.x = element_text(angle = 90)) +
  labs(y = "DEBTINC")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) 

grid.arrange(p19, p20, ncol = 1, nrow = 2)
summary(morosidad_imp$DEBTINC)

##################################################################
## Relaciones con "BAD"
### LOAN
ggplot(data = morosidad_imp, aes(y = LOAN ,x  = factor(BAD), fill = factor(BAD))) +
  geom_boxplot() +
  labs(title = "Tamaño del crédito solicitado (LOAN)\n", y = "Crédito solicitado", x = "Morosidad") +
  theme_bw() +
  scale_fill_manual(values = c("Blue", "darkgreen"))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  theme(plot.title = element_text(hjust = 0.5), legend.position = "none")+
  scale_y_continuous(breaks = seq(0, 100000, by = 20000))
summary(morosidad_imp$LOAN[morosidad_imp$BAD == 1])
summary(morosidad_imp$LOAN[morosidad_imp$BAD == 0])

### MORTDUE
ggplot(data = morosidad_imp, aes(y = MORTDUE ,x  = factor(BAD), fill = factor(BAD))) +
  geom_boxplot() +
  labs(title = "Hipoteca Existente (MORTDUE)\n", y = "Valor Hipoteca Existente", x = "Morosidad") +
  theme_bw() +
  scale_fill_manual(values = c("Blue", "darkgreen"))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  theme(plot.title = element_text(hjust = 0.5), legend.position = "none")+
  scale_y_continuous(breaks = seq(0, 400000, by = 50000))
summary(morosidad_imp$MORTDUE[morosidad_imp$BAD == 1])
summary(morosidad_imp$MORTDUE[morosidad_imp$BAD == 0])

#VALUE
ggplot(data = morosidad_imp, aes(y = VALUE ,x  = factor(BAD), fill = factor(BAD))) +
  geom_boxplot() +
  labs(title = "Valor propuedad actual (VALUE)\n", y = "Valor", x = "Morosidad") +
  theme_bw() +
  scale_fill_manual(values = c("Blue", "darkgreen"))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  theme(plot.title = element_text(hjust = 0.5), legend.position = "none")
  #scale_y_continuous(breaks = seq(0, 900000, by = 200000))
summary(morosidad_imp$VALUE[morosidad_imp$BAD == 1])
summary(morosidad_imp$VALUE[morosidad_imp$BAD == 0])

#YOJ
ggplot(data = morosidad_imp, aes(y = YOJ ,x  = factor(BAD), fill = factor(BAD))) +
  geom_boxplot() +
  labs(title = "Años en trabajo actual (YOJ)\n", y = "Años", x = "Morosidad") +
  theme_bw() +
  scale_fill_manual(values = c("Blue", "darkgreen"))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  theme(plot.title = element_text(hjust = 0.5), legend.position = "none")
summary(morosidad_imp$YOJ[morosidad_imp$BAD == 1])
summary(morosidad_imp$YOJ[morosidad_imp$BAD == 0])

#DEROG
ggplot(data = morosidad_imp, aes(y = DEROG ,x  = factor(BAD), fill = factor(BAD))) +
  geom_boxplot() +
  labs(title = "Reportes de créditos no pagados (DEROG)\n", y = "DEROG", x = "Morosidad") +
  theme_bw() +
  scale_fill_manual(values = c("Blue", "darkgreen"))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  theme(plot.title = element_text(hjust = 0.5), legend.position = "none")
summary(morosidad_imp$DEROG[morosidad_imp$BAD == 1])
summary(morosidad_imp$DEROG[morosidad_imp$BAD == 0])

#DELINQ
ggplot(data = morosidad_imp, aes(y = DELINQ ,x  = factor(BAD), fill = factor(BAD))) +
  geom_boxplot() +
  labs(title = "Líneas de créditos no pagados (DELINQ)\n", y = "DELINQ", x = "Morosidad") +
  theme_bw() +
  scale_fill_manual(values = c("Blue", "darkgreen"))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  theme(plot.title = element_text(hjust = 0.5), legend.position = "none")
summary(morosidad_imp$DELINQ[morosidad_imp$BAD == 1])
summary(morosidad_imp$DELINQ[morosidad_imp$BAD == 0])

#CLAGE
ggplot(data = morosidad_imp, aes(y = CLAGE ,x  = factor(BAD), fill = factor(BAD))) +
  geom_boxplot() +
  labs(title = "Antiguedad en meses de la\n línea de crédito más antigua (CLAGE)", y = "CLAGE", x = "Morosidad") +
  theme_bw() +
  scale_fill_manual(values = c("Blue", "darkgreen"))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  theme(plot.title = element_text(hjust = 0.5), legend.position = "none")
summary(morosidad_imp$CLAGE[morosidad_imp$BAD == 1])
summary(morosidad_imp$CLAGE[morosidad_imp$BAD == 0])

#NINQ
ggplot(data = morosidad_imp, aes(y = NINQ ,x  = factor(BAD), fill = factor(BAD))) +
  geom_boxplot() +
  labs(title = "Número de consultas por créditos recientes (NINQ)\n", y = "NINQ", x = "Morosidad") +
  theme_bw() +
  scale_fill_manual(values = c("Blue", "darkgreen"))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  theme(plot.title = element_text(hjust = 0.5), legend.position = "none")
summary(morosidad_imp$NINQ[morosidad_imp$BAD == 1])
summary(morosidad_imp$NINQ[morosidad_imp$BAD == 0])


#CLNO
ggplot(data = morosidad_imp, aes(y = CLNO ,x  = factor(BAD), fill = factor(BAD))) +
  geom_boxplot() +
  labs(title = "Número de líneas de crédito (CLNO)\n", y = "CLNO", x = "Morosidad") +
  theme_bw() +
  scale_fill_manual(values = c("Blue", "darkgreen"))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  theme(plot.title = element_text(hjust = 0.5), legend.position = "none")
summary(morosidad_imp$CLNO[morosidad_imp$BAD == 1])
summary(morosidad_imp$CLNO[morosidad_imp$BAD == 0])


#DEBTINC
ggplot(data = morosidad_imp, aes(y = DEBTINC ,x  = factor(BAD), fill = factor(BAD))) +
  geom_boxplot() +
  labs(title = "Ratio Deuda/Ingreso (DEBTINC)\n", y = "DEBTINC", x = "Morosidad") +
  theme_bw() +
  scale_fill_manual(values = c("Blue", "darkgreen"))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  theme(plot.title = element_text(hjust = 0.5), legend.position = "none")
summary(morosidad_imp$DEBTINC[morosidad_imp$BAD == 1])
summary(morosidad_imp$DEBTINC[morosidad_imp$BAD == 0])


## Visualización BAD con variables categóricas
ggplot(morosidad_imp, aes(REASON,fill=factor(BAD))) +
  geom_bar() +theme(legend.position="none") +
  labs(title = 'BAD en REASON', x = 'REASON') +
  theme_bw()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())

ggplot(morosidad_imp, aes(REASON,fill=factor(BAD))) +
  labs(title = 'BAD en REASON', x = 'REASON', y = "porcentaje") +
  geom_bar(position = "fill") +theme(legend.position="none") +
  theme_bw()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())



ggplot(morosidad_imp, aes(JOB,fill=factor(BAD)))+
  labs(title = 'BAD en JOB', x = 'JOB') +
  geom_bar() +theme(legend.position="none") +
  theme_bw()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())


ggplot(morosidad_imp, aes(JOB,fill=factor(BAD))) +
  labs(title = 'BAD en JOB', x = 'JOB', y = "porcentaje") +
  geom_bar(position = "fill") +theme(legend.position="none") +
  theme_bw()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())





### Separación data Entrenamiento y Validación
set.seed(1234)
trainIndex = createDataPartition(morosidad_imp$BAD, p=0.8, list=FALSE)
data_train = morosidad_imp[trainIndex,]
data_test = morosidad_imp[-trainIndex,]
dim(data_train)

dim(data_train)



# Ajuste de modelos univariados y calculo de AUC y KS
AUC_train = numeric()
AUC_test = numeric()
KS_train = numeric()
KS_test = numeric()
for(i in 2:13) {
  x = data_train[,i]
  mod_train = glm(BAD~x,data=data_train,family=binomial)
  # calcular AUC para muestra de entrenamiento
  predictions = predict(mod_train, newdata=data.frame(x=data_train[,i]), type="response")
  ROCRpred = prediction(predictions, mod_train$y)
  ROCRperf = performance(ROCRpred, measure = "tpr", x.measure = "fpr")
  aucTrain = performance(ROCRpred, measure = "auc")
  aucTrain = aucTrain@y.values[[1]]
  AUC_train[i] = aucTrain
  KS_train[i] = ks_stat(actuals=mod_train$y, predictedScores=fitted(mod_train))
  # calcular AUC para muestra de validacion
  predictions = predict(mod_train, newdata=data.frame(x=data_test[,i]), type="response")
  ROCRpred = prediction(predictions, data_test$BAD)
  ROCRperf = performance(ROCRpred, measure = "tpr", x.measure = "fpr")
  aucTest = performance(ROCRpred, measure = "auc")
  aucTest = aucTest@y.values[[1]]
  AUC_test[i] = aucTest
  KS_test[i]= ks_stat(actuals=data_test$BAD, predictedScores=predict(mod_train, newdata=data.frame(x=data_test[,i]), type="response"))
}

# ordenamos los atributos por AUC y KS en muestras train y test
nombreVar = names(morosidad_imp)[1:13]
o = order(AUC_train,decreasing = T)[-13]
cbind(nombreVar[o],AUC_train[o],AUC_test[o], KS_train[o],  KS_test[o])



## Modelo Multivariado
modelo_M = glm(BAD~.,data=data_train,family=binomial)
summary(modelo_M)

#Coefficients:
#  Estimate Std. Error z value Pr(>|z|)    
#(Intercept)   -3.376e+00  2.841e-01 -11.882  < 2e-16 ***
#  LOAN          -2.629e-05  5.073e-06  -5.182 2.20e-07 ***
#  MORTDUE       -6.552e-06  2.274e-06  -2.882 0.003956 ** 
#  VALUE          6.296e-06  1.650e-06   3.816 0.000136 ***
#  REASONHomeImp  2.419e-01  9.844e-02   2.457 0.014003 *  
#  JOBOffice     -5.464e-01  1.708e-01  -3.199 0.001380 ** 
#  JOBOther      -2.395e-04  1.337e-01  -0.002 0.998571    
#JOBProfExe     1.399e-01  1.534e-01   0.912 0.361660    
#JOBSales       1.009e+00  3.016e-01   3.345 0.000824 ***
#  JOBSelf        6.221e-01  2.491e-01   2.498 0.012499 *  
#  YOJ           -5.906e-03  6.457e-03  -0.915 0.360339    
#DEROG          5.080e-01  5.109e-02   9.943  < 2e-16 ***
#  DELINQ         7.546e-01  4.327e-02  17.438  < 2e-16 ***
#  CLAGE         -6.374e-03  6.366e-04 -10.013  < 2e-16 ***
#  NINQ           1.438e-01  2.322e-02   6.192 5.93e-10 ***
#  CLNO          -1.834e-02  4.970e-03  -3.691 0.000224 ***
#  DEBTINC        8.119e-02  6.346e-03  12.794  < 2e-16 ***
  
## Step para selección de Variables 
modelo.nulo = glm(BAD~1,data= data_train,family=binomial)
modelo.nulo

# Modelo completo: modelo con todos los atributos
modelo.completo = glm(BAD~.,data= data_train,family=binomial)
modelo.completo

# Método forward ---------------------------------------------------------------------------
step(modelo.nulo, scope=list(lower=modelo.nulo, upper=modelo.completo), direction="forward")

#Call:  glm(formula = BAD ~ DELINQ + DEBTINC + CLAGE + DEROG + JOB + 
#             LOAN + NINQ + CLNO + REASON + VALUE + MORTDUE, family = binomial, 
#           data = data_train)

#Coefficients:
#  (Intercept)         DELINQ        DEBTINC          CLAGE          DEROG      JOBOffice  
#-3.418e+00      7.520e-01      8.150e-02     -6.461e-03      5.103e-01     -5.365e-01  
#JOBOther     JOBProfExe       JOBSales        JOBSelf           LOAN           NINQ  
#1.603e-03      1.453e-01      1.019e+00      6.377e-01     -2.681e-05      1.445e-01  
#CLNO  REASONHomeImp          VALUE        MORTDUE  
#-1.847e-02      2.389e-01      6.158e-06     -6.302e-06 

#Degrees of Freedom: 4667 Total (i.e. Null);  4652 Residual
#Null Deviance:	    4637 
#Residual Deviance: 3435 	AIC: 3467

# Método backward  ---------------------------------------------------------------------------------
step(modelo.completo, data=data_train, direction="backward")

#Call:  glm(formula = BAD ~ LOAN + MORTDUE + VALUE + REASON + JOB + DEROG + 
#             DELINQ + CLAGE + NINQ + CLNO + DEBTINC, family = binomial, 
#           data = data_train)

#Coefficients:
#  (Intercept)           LOAN        MORTDUE          VALUE  REASONHomeImp      JOBOffice  
#-3.418e+00     -2.681e-05     -6.302e-06      6.158e-06      2.389e-01     -5.365e-01  
#JOBOther     JOBProfExe       JOBSales        JOBSelf          DEROG         DELINQ  
#1.603e-03      1.453e-01      1.019e+00      6.377e-01      5.103e-01      7.520e-01  
#CLAGE           NINQ           CLNO        DEBTINC  
#-6.461e-03      1.445e-01     -1.847e-02      8.150e-02  

#Degrees of Freedom: 4667 Total (i.e. Null);  4652 Residual
#Null Deviance:	    4637 
#Residual Deviance: 3435 	AIC: 3467



# Método stepwise  -----------------------------------------------------------------------------------
step(modelo.nulo, scope = list(upper=modelo.completo), data= data_train, direction="both")

#Call:  glm(formula = BAD ~ DELINQ + DEBTINC + CLAGE + DEROG + JOB + 
#             LOAN + NINQ + CLNO + REASON + VALUE + MORTDUE, family = binomial, 
#           data = data_train)

#Coefficients:
#  (Intercept)         DELINQ        DEBTINC          CLAGE          DEROG      JOBOffice  
#-3.418e+00      7.520e-01      8.150e-02     -6.461e-03      5.103e-01     -5.365e-01  
#JOBOther     JOBProfExe       JOBSales        JOBSelf           LOAN           NINQ  
#1.603e-03      1.453e-01      1.019e+00      6.377e-01     -2.681e-05      1.445e-01  
#CLNO  REASONHomeImp          VALUE        MORTDUE  
#-1.847e-02      2.389e-01      6.158e-06     -6.302e-06  

#Degrees of Freedom: 4667 Total (i.e. Null);  4652 Residual
#Null Deviance:	    4637 
#Residual Deviance: 3435 	AIC: 3467

### Los 3 métodos entregaron mismo resultado, dejando fuera solo la variable YOJ 
### Similar a lo visto en el modelo multivariado que indicaba esa variable como
### no significativa


# modelo final -------------------------
modelo.final = glm(formula = BAD ~ DELINQ + DEBTINC + CLAGE + DEROG + JOB + LOAN + 
                     NINQ + CLNO + REASON + VALUE + MORTDUE , family = binomial, data = data_train)
summary(modelo.final)


## AUC (train y test)y KS (train y test) modelo.final
# calculo AUC para train data
predictions = predict(modelo.final, newdata=data_train, type="response")
ROCRpred = prediction(predictions, modelo.final$y)

ROCRperf = performance(ROCRpred, measure = "tpr", x.measure = "fpr")
auc = performance(ROCRpred, measure = "auc")
auc = auc@y.values[[1]]
auc
#0.8231358

# AUC para test dataset
predictions = predict(modelo.final, newdata=data_test, type="response")
ROCRpred = prediction(predictions, data_test$BAD)
ROCRperf = performance(ROCRpred, measure = "tpr", x.measure = "fpr")
auc = performance(ROCRpred, measure = "auc")
auc = auc@y.values[[1]]
auc
#0.8162257

#KS
# train dataset
ks_stat(actuals=modelo.final$y, predictedScores=fitted(modelo.final))
#0.5175

# test dataset
ks_stat(actuals=data_test$BAD, predictedScores=predict(modelo.final, newdata=data_test, type="response"))
#0.4866


## grafico curva roc(train y test en mismo gráfico)

perf.auc(model = modelo.final, data_train, data_test)

## Matriz de confusión (entrenamiento y validacion)
# matriz de confusion entrenamiento
caret::confusionMatrix(as.factor(data_train$BAD),as.factor((predict(modelo.final, newdata=data_train, type="response")>=0.5)*1))

# calculamos la matriz de confusion en la muestra de validacion
caret::confusionMatrix(as.factor(data_test$BAD),as.factor((predict(modelo.final, newdata=data_test, type="response")>=0.5)*1))


## OR Coeficientes Modelo Final 
exp(7.520e-01) #DELINQ = 2.121238
exp(8.150e-02) #DEBTINC = 1.084913
exp(-6.461e-03) #CLAGE = 0.9935598
exp(5.103e-01) #DEROG = 1.665791

################ Regularización 
# Ridge
# regresion logistica con Ridge
dummy = model.matrix(~REASON+JOB-1,data=data_train)
numericas = scale(data_train[,c(2,3,4,7,8,9,10,11,12,13)])
x = data.frame(dummy,numericas)
x = as.matrix(x)
y = data_train[,"BAD"]

fitridge = glmnet(x,y,alpha=0,family="binomial")
names(fitridge)
fitridge

par(mfrow=c(1,2))
plot(fitridge,xvar="lambda")
plot(fitridge,xvar="norm")

dev.off()
set.seed(124)
cvRidge = cv.glmnet(x,y,alpha=0,nfolds=5,family="binomial")
# lambda gorro
cvRidge$lambda.min  
which(cvRidge$lambda==cvRidge$lambda.min)


# este es el valor de lambda que minimiza el error de testeo estimado
cvRidge$cvm[100]

# este es el estimador del error de testeo cuando lambda=100.
plot(cvRidge)

# coeficientes estimados por ridge 
coef(fitridge,s=cvRidge$lambda.min)



# predicciones en la muestra entrenamiento
prob.pred.ridge.train = predict(fitridge, newx=x, s=cvRidge$lambda.min, type = "response")

# vamos a calcular la matriz de confusion
clase.pred.train = ifelse(prob.pred.ridge.train>=0.5,1,0)
# resultados
caret::confusionMatrix(as.factor(clase.pred.train), as.factor(y))


# calculamos la matriz de confusion en la muestra de validacion
dummy_test = model.matrix(~REASON+JOB-1,data=data_test)
numericas_test = scale(data_test[,c(2,3,4,7,8,9,10,11,12,13)])
x2 = data.frame(dummy_test,numericas_test)
x2= as.matrix(x2)
y2 = data_test[,"BAD"]
# predicciones en la muestra validacion
prob.pred.ridge.test = predict(fitridge, newx=x2, s=cvRidge$lambda.min, type = "response")

# vamos a calcular la matriz de confusion
clase.pred.test = ifelse(prob.pred.ridge.test>=0.5,1,0)
# resultados
caret::confusionMatrix(as.factor(clase.pred.test), as.factor(y2))


# AUC y KS con Ridge
# Train
ROCRpred = prediction(prob.pred.ridge.train, y)
ROCRperf = performance(ROCRpred, measure = "tpr", x.measure = "fpr")
auc = performance(ROCRpred, measure = "auc")
auc = auc@y.values[[1]]
auc
ks_stat(actuals=y, predictedScores=as.vector(prob.pred.ridge.train))

# Test
ROCRpred = prediction(prob.pred.ridge.test, y2)
ROCRperf = performance(ROCRpred, measure = "tpr", x.measure = "fpr")
auc = performance(ROCRpred, measure = "auc")
auc = auc@y.values[[1]]
auc
ks_stat(actuals=y2, predictedScores=as.vector(prob.pred.ridge.test))


############### Regularización (Lasso)
# regresion logistica con Lasso
fitLasso = glmnet(x,y,alpha=1,family="binomial")

par(mfrow=c(1,2))
plot(fitLasso,xvar="lambda")
plot(fitLasso,xvar="norm")

set.seed(124)
cvLasso = cv.glmnet(x,y,alpha=1,nfolds=5,family="binomial")
# lambda gorro
cvLasso$lambda.min  
which(cvLasso$lambda==cvLasso$lambda.min)

# este es el valor de lambda que minimiza el error de testeo estimado
cvLasso$cvm[64]

# este es el estimador del error de testeo cuando lambda=99.
plot(cvLasso)

# coeficientes estimados por ridge
coef(fitLasso,s=cvLasso$lambda.min)


# predicciones en la muestra entrenamiento
prob.pred.lasso.train = predict(fitLasso, newx=x, s=cvLasso$lambda.min, type = "response")

# vamos a calcular la matriz de confusion
clase.pred.train = ifelse(prob.pred.lasso.train>=0.5,1,0)
# resultados
caret::confusionMatrix(as.factor(clase.pred.train), as.factor(y))


# calculamos la matriz de confusion en la muestra de validacion

# predicciones en la muestra validacion
prob.pred.lasso.test = predict(fitLasso, newx=x2, s=cvLasso$lambda.min, type = "response")

# vamos a calcular la matriz de confusion
clase.pred.test = ifelse(prob.pred.lasso.test>=0.5,1,0)
# resultados
caret::confusionMatrix(as.factor(clase.pred.test), as.factor(y2))


# AUC y KS con Lasso
#Train
ROCRpred = prediction(prob.pred.lasso.train, y)
ROCRperf = performance(ROCRpred, measure = "tpr", x.measure = "fpr")
auc = performance(ROCRpred, measure = "auc")
auc = auc@y.values[[1]]
auc
ks_stat(actuals=y, predictedScores=as.vector(prob.pred.lasso.train))

#Test
ROCRpred = prediction(prob.pred.lasso.test, y2)
ROCRperf = performance(ROCRpred, measure = "tpr", x.measure = "fpr")
auc = performance(ROCRpred, measure = "auc")
auc = auc@y.values[[1]]
auc
ks_stat(actuals=y2, predictedScores=as.vector(prob.pred.lasso.test))

# comentarios: los resultados de GLM, Ridge y Lasso son muy similares en AUC y KS



## WOE y IV 

# Ahora calculamos el WOE para las variables que quedaron de stepwise
# Calculo del WOE para la variable 
#DELINQ + DEBTINC + CLAGE + DEROG + JOB 
#             LOAN + NINQ + CLNO + REASON + VALUE + MORTDUE
bins = woebin(data_train[,c("DELINQ","DEBTINC","CLAGE","DEROG", 
                            "JOB","LOAN","NINQ","CLNO" , 
                            "REASON","VALUE", "MORTDUE", "BAD")], y = "BAD")
bins

# Calculo del IV para 
iv = iv(data_train[,c("DELINQ","DEBTINC","CLAGE","DEROG", 
                      "JOB","LOAN","NINQ","CLNO" , 
                      "REASON","VALUE", "MORTDUE", "BAD")], y = "BAD") %>%
  as_tibble() %>%
  mutate( info_value = round(info_value, 3) ) %>%
  arrange( desc(info_value) )
iv

# Calculo del IV para todos los atributos
iv = iv(data_train, y = 'BAD') %>%
  as_tibble() %>%
  mutate( info_value = round(info_value, 3) ) %>%
  arrange( desc(info_value) )
iv


# Calculo del WOE para todas menos REASON ya que tiene un IV menor a 0.02
bins = woebin(data_train[,-c(5)], y = 'BAD')
bins
# Guardamos los valores del WOE como variable
morosidad_woe_train = woebin_ply(data_train[,-c(5)], bins ) %>%
  as_tibble()

morosidad_woe_test = woebin_ply(data_test[,-c(5)], bins ) %>%
  as_tibble()

# SCORECARD
modeloWOE = glm(BAD~., morosidad_woe_train, family = 'binomial')
summary(modeloWOE)

modeloWOE = glm(BAD~.-MORTDUE_woe, morosidad_woe_train, family = 'binomial')
summary(modeloWOE)

vif(modeloWOE)

# Model Performance for train data: 
perf_eva(label=modeloWOE$y, 
         pred=predict(modeloWOE,type="response",  morosidad_woe_train), 
         binomial_metric = c("ks"),
         title = "Train Data")

# Model Performance for test data: 
perf_eva(label=as.integer(morosidad_woe_test$BAD), 
         pred=predict(modeloWOE,type="response", morosidad_woe_test), 
         binomial_metric = c("ks"),
         title = "Test Data")

pred.woe = predict(modeloWOE, morosidad_woe_train, type = "response")
ROCRpred = prediction(pred.woe, y)
ROCRperf = performance(ROCRpred, measure = "tpr", x.measure = "fpr")
auc = performance(ROCRpred, measure = "auc")
auc = auc@y.values[[1]]
auc
ks_stat(actuals=y, predictedScores=as.vector(pred.woe))


pred.woe = predict(modeloWOE, morosidad_woe_test, type = "response")
ROCRpred = prediction(pred.woe, morosidad_woe_test$BAD)
ROCRperf = performance(ROCRpred, measure = "tpr", x.measure = "fpr")
auc = performance(ROCRpred, measure = "auc")
auc = auc@y.values[[1]]
auc
ks_stat(actuals=morosidad_woe_test$BAD, predictedScores=as.vector(pred.woe))




# matriz de confusion entrenamiento
caret::confusionMatrix(as.factor(morosidad_woe_train$BAD),as.factor((predict(modeloWOE, newdata=morosidad_woe_train, type="response")>=0.5)*1))

# matriz de confusion test
caret::confusionMatrix(as.factor(morosidad_woe_test$BAD),as.factor((predict(modeloWOE, newdata=morosidad_woe_test, type="response")>=0.5)*1))

coef(modeloWOE)


## Scorecard modelo woe
 
logit = predict(modeloWOE)

points0 = 600
odds0 = 50
pdo = 20

my_card = scorecard( bins , modeloWOE
                     , points0 = points0 
                     , odds0 = 1/odds0 # scorecard wants the inverse
                     , pdo = pdo 
)

score = scorecard_ply(data_train, my_card )
score

my_card[[2]]

# media del score
mean(score$score)
median(score$score)
quantile(score$score, .25)
quantile(score$score, .75)

hist(score$score, col = "lightblue", border = "white", main = "Distribución Puntajes",
     xlab = "Score", ylab = "Frecuencia")
abline(v=median(score$score),lwd=2,col="yellow") #519
abline(v=quantile(score$score, .25),lwd=2,col="red") #552
abline(v=quantile(score$score, .75),lwd=2,col="blue") #573

