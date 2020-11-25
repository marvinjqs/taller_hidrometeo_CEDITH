#######################################
# PROCESAMIENTO DE DATOS HIDROMETEOROLOGICOS DEL SENAMHI CON R
# Autor: Marvin J. Quispe Sedano
# Correo: marvinjqs@gmail.com
#######################################

# NOTA: Tildes omitidas en todos los parrafos del presente codigo
#######################################

# Instalar e importar los paquetes a nuestra sesión de R

pkgs = c("tidyverse", "data.table", "openxlsx", "mice")
# install.packages(pkgs)
lapply(pkgs, library, character.only = TRUE)

######################################

# Configurar nuestro directorio de trabajo (ruta de la carpeta)

setwd("D:/TALLER-R-HIDROMETEO/R")

df <- read.csv("MATUCANA-HISTORICO.csv", 
               header = F, 
               sep = ";", 
               stringsAsFactors = F,
               na.strings = "-99.9")

names(df) <- c("ANHO", "MES", "DIA", "PP", "TMAX", "TMIN")

# Crear una columna con las fechas incluidas

df$FECHA <- paste(df$ANHO, df$MES, df$DIA, sep = "-")

# Cambiar el formato de la columna a uno reconocible por R

df$FECHA <- as.Date(df$FECHA, format = "%Y-%m-%d")

######################################

# Observar valores NA en nuestra base de datos

summary(df)
md.pattern(df)

# Completar los valores faltantes de ser necesario

df_mice <- mice(data,m=5,maxit=50,meth='pmm',seed=500)
summary(tempData)

df_complete <- complete(df_mice,1)

######################################

# Obtener una columna con las fechas en formato mensual o anual

df$FECHA_m <- format(df$FECHA, format = "%Y-%m")

# Obtener los promedios, maximos, minimos, etc

PP_PROM_m <- aggregate(PP ~ FECHA_m, df, mean)

# Completar dias faltantes y asignar NA a las variables
df2 <- df %>%
  complete(FECHA = seq(min(FECHA), max(FECHA), by = "1 day"),
           fill = list(VALOR = NA))

# Obtener los promedios, maximos, minimos, etc (CON RESTRICCIONES)

data_prom_m <- setDT(df2)[,lapply(.SD, function(x) if(length(na.omit(x)) >=15)
  mean(x, na.rm=TRUE) else NA_real_) ,
  by = .(Month = format(FECHA, '%m-%Y'))]

data_max_m <- setDT(df2)[,lapply(.SD, function(x) if(length(na.omit(x)) >=15)
  max(x, na.rm=TRUE) else NA_real_) ,
  by = .(Month = format(FECHA, '%m-%Y'))]

data_min_m <- setDT(df2)[,lapply(.SD, function(x) if(length(na.omit(x)) >=15)
  min(x, na.rm=TRUE) else NA_real_) ,
  by = .(Month = format(FECHA, '%m-%Y'))]

# Crear graficos para visualizar los datos

plot(df$FECHA, df$PP, type = "l")

# Crear el marco de datos final

data_final <- data.frame(data_prom_m, data_min_m[,2], data_max_m[,2])
names(data_final) <- c("FECHA", "PROM", "MIN", "MAX")

# Guardar la base de datos final

write.xlsx (data_final, "data-final.xlsx", row.names = F)


