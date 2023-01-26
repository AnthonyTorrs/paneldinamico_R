##### EXAMEN FINAL - ECONOMETRÍA DE CORTE TRANSVERSAL Y DATOS DE PANEL #########

# PROFESOR: BUSTAMANTE ROMANÍ, RAFAEL 
#
# INTEGRANTES: TORRES ARIAS ANGEL ANTHONY    15120320
#              HUALLPA FERROA ALEXIS         15120282
#              VERGARAY CHAVEZ JOSÉ ARTURO   15120321
#
# A continuación desarrollaremos un modelo de datos de panel qué describe mejor la demanda de gasolina en los
# 18 países de la OCDE presentados en la Tabla panel, específicamente hallaremos un modelo de efectos fijos y 
# y uno de efectos aleatorios, para luego compararlos entre ellos y elegir el más significativo.
#
# La data ha sido extraída del paper:
#                       T1  - Gasoline Demand in the OECD: An Application of Pooling and Testing Procedures
#                       AU  - Baltagi, Badi
#                       AU  - Griffin, James
#                       PY  - 1983/07/01
#                       SP  - 117
#                       EP  - 137
#                       DO  - 10.1016/0014-2921(83)90077-6
#                       JO  - European Economic Review

# Comenzaremos 
    
      library(foreign)
      load("C:/Users/PC/Downloads/Table16_17.rda")


# 1. Exploración y presentación de los datos de panel

      coplot(LGASPCAR ~ YEAR|Ctry_Code, type="l", data=Table16_17) 
      coplot(LGASPCAR ~ YEAR|Ctry_Code, type="b", data=Table16_17) 

# 2. Bars at top indicates corresponding graph (i.e. countries) from left to right starting on the bottom row (Muenchen/Hilbe:355)

      library(foreign)
      library(readxl)
      panel <- read_excel("corte y datos de paenl/Table 16_17.xls", 
                    col_types = c("text", "numeric", "numeric", 
                                  "numeric", "numeric", "numeric", 
                                  "numeric"))
      install.packages("car")
      library(car)
      scatterplot(LGASPCAR~YEAR|Ctry_Code, boxplots=FALSE, smooth=TRUE, reg.line=FALSE, data=tabla1)
      

# convirtiendo la variable Ctry_Code a factor y el añadido de etiquetas para un mejor entedimiento del gráfico
      
      library(foreign)
      tabla1$Ctry_Code <- as.factor(tabla1$Ctry_Code)
      str(tabla1$Ctry_Code)
      tabla1$Ctry_Code=factor(tabla1$Ctry_Code, levels = levels(tabla1$Ctry_Code), labels = c("AUSTRIA", "BÉLGICA", "CANADÁ", "DINAMARCA", "FRANCIA", "ALEMANIA", "GRECIA", "IRLANDA","ITALIA", "JAPÓN", "HOLANDA", "NORUEGA", "ESPAÑA", "SUECIA", "SUIZA", "TURQUÍA", "REINO UNIDO", "ESTADOS UNIDOS"), ordered = F)
      str(tabla1$Ctry_Code)
      table(tabla1$Ctry_Code)
      
      
# 3. FIXED-EFFECTS MODEL(Covariance Model, Within Estimator, Individual Dummy Variable Model, 
# Least Squares Dummy Variable Model)
   
    # 3.1 Fixed effects: Heterogeneity across Ctry_code    
       
      library(foreign)
      install.packages("gplots")
      library(gplots)
      plotmeans(LGASPCAR ~ Ctry_Code, main="Heterogeineityacross GASTO EN GASOLINA PARA LOS PAÍSES DE LA OCDE 1960-1978 ", data=tabla1)

      
    # 3.2 Fixed effects: Heterogeneity across years
      
      library(foreign)
      library(gplots)
      plotmeans(LGASPCAR ~ YEAR, main="Heterogeineityacross years GASTO EN GASOLINA PARA LOS PAÍSES DE LA OCDE 1960-1978", data=tabla1)
      
          
# plotmeansdraw a 95% confidence interval around the means
      
      detach("package:gplots")
      
# Remove package 'gplots' from the workspace

            
# 3.3 OLS model 
# Recordar que La regresión de MCO regular no considera la heterogeneidad entre grupos o tiempo.     
  
      
      library(foreign)
      ols<-lm(LGASPCAR ~ LINCOMEP, data=tabla1)
      summary(ols) 
      
      yhat <-ols$fitted
      plot(tabla1$LINCOMEP, tabla1$LGASPCAR, pch=19, xlab="INGRESO REAL PER CÁPITA", ylab="CONSUMO DE GASOLINA POR AUTOMÓVIL")
      abline(lm(tabla1$LGASPCAR~tabla1$LINCOMEP),lwd=3, col="red")
      
# 3.4 LSDV model
# Efectos fijos usando el modelo de variable dummy de mínimos cuadrados
      
      
      library(foreign)
      fixed.dum <-lm(LGASPCAR ~ LINCOMEP + factor(Ctry_Code) -1, data=tabla1)
      summary(fixed.dum)
      
      
      
      
# 3.5 Modelo de variable dummy de mínimos cuadrados
      
      yhat<-fixed.dum$fitted
      library(car)
      scatterplot(yhat~tabla1$LINCOMEP|tabla1$Ctry_Code, boxplots=FALSE, xlab="INGRESO REAL PER CÁPITA", ylab="yhat",smooth=FALSE)
      abline(lm(tabla1$LGASPCAR~tabla1$LINCOMEP),lwd=3, col="red") 
      
#   este comando verifica el ajuste comparando los datos observados con los pronósticos.
      
      
    
# 3.6 Comparando  OLS vs LSDV model    
#      Cada componente de la variable factor (país) está absorbiendo los efectos particulares de cada país. 
#      El predictor INGRESO REAL PER CÁPITA no fue significativo en el modelo OLS, una vez que se controlaron las diferencias entre países, 
#     INGRESO REAL PER CÁPITA se volvió significativo en el OLS_DUM (es decir, el modelo LSDV).   
      
      install.packages("apsrtable")
      library(apsrtable)
      apsrtable(ols,fixed.dum, model.names= c("OLS", "OLS_DUM")) 
    
        
# 3.7 Fixed effects: nentity-specific intercepts     
      
      install.packages("plm")
      library(plm)
      fixed <-plm(LGASPCAR ~ LINCOMEP, data=tabla1, index=c("Ctry_Code", "YEAR"), model="within")
      summary(fixed)  
      
#  Mostrar los efectos fijos (constantes para cada país) 
      
      fixef(fixed)
      
# Prueba de efectos fijos, nulo: OLS mejor que fijo      
      
      pFtest(fixed, ols)
      
# 4. RANDOM-EFFECTS MODEL(Random Intercept, Partial Pooling Model)      
# Random effects (using plm)
      
      random <-plm(LGASPCAR ~ LINCOMEP, data=tabla1, index=c("Ctry_Code", "YEAR"), model="random")
      summary(random)
      
#  Establecer como datos de panel (una forma alternativa de ejecutar el modelo anterior)      
      
      Panel.set<-plm.data(tabla1, index = c("Ctry_Code", "YEAR"))
      summary(Panel.set)
      
#  Efectos aleatorios usando la configuración del panel (la misma salida que la anterior)      
      
      random.set<-plm(LGASPCAR ~ LINCOMEP, data = Panel.set, model="random")
      summary(random.set)
      
      
# 5. FIXED OR RANDOM?       
# Fixed or Random: Hausman test
# Para decidir entre efectos fijos o aleatorios, puede ejecutar un Hausmantest donde la hipótesis nula es 
# que el modelo preferido son los efectos aleatorios frente a la alternativa los efectos fijos. Básicamente, 
# prueba si los errores únicos (ui) están correlacionados con los regresores, la hipótesis nula es que no lo están.
# Ejecute un modelo de efectos fijos y guarde las estimaciones, luego ejecute un modelo aleatorio y guarde las estimaciones, 
# luego realice la prueba. Si el valor p es significativo (por ejemplo, <0,05), utilice efectos fijos, si no, utilice efectos aleatorios.     
      
      phtest(fixed, random)   
      
# Evaluando el p-value, arroja que se acepta la hipótesis nula, la cual resulta que el modelo de efectos fijos
# es más significativo para predecir y describe mejor la demanda de gasolina en los 18 países de la OCDE consultados
# para los años 1960 - 1978.

      
# 6. OTHER TESTS/ DIAGNOSTICS
# 6.1 Testing for serial correlation
      
      library(plm)
      pbgtest(fixed)

# 6.2 Testing for unit roots/stationarity
      
      library(plm)
      Panel.set<-plm.data(tabla1, index = c("Ctry_Code", "YEAR"))
      install.packages("tseries")
      library(tseries)
      adf.test(Panel.set$LGASPCAR, k=2)
      
# 6.3 Testing for heteroskedasticity
      
      library(lmtest)
      bptest(LGASPCAR ~ LINCOMEP + factor(Ctry_Code), data = tabla1, studentize=F)
      
      
      
      
      
      
      
          
      
      
      