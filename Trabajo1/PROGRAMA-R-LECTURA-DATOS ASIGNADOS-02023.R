rm(list=ls(all=TRUE))
#Leer anex-EMC-SeriesIndiceEmpalmados-vtas nominales lineas mercancia-may2023.csv, columna 3: Total comercio minorista sin otros vehículos
Datos1=read.table(file.choose(),header=T,sep=";",skip=10,dec=",",colClasses=c(rep("NULL",2),"numeric",rep("NULL",19)))
Datos1=ts(Datos1,freq=12,start=c(2013,1))
plot(Datos1)

#-------------------------------------------------------------------------------------------------------------------
rm(list=ls(all=TRUE))
#Leer anex-EMC-SeriesIndiceEmpalmados-vtas nominales lineas mercancia-may2023.csv, columna 4: Total comercio minorista sin vehículos
Datos2=read.table(file.choose(),header=T,sep=";",skip=10,dec=",",colClasses=c(rep("NULL",3),"numeric",rep("NULL",18)))
Datos2=ts(Datos2,freq=12,start=c(2013,1))
plot(Datos2)

#-------------------------------------------------------------------------------------------------------------------
rm(list=ls(all=TRUE))
#Leer anex-EMC-SeriesIndiceEmpalmados-vtas nominales lineas mercancia-may2023.csv, columna 5: Total comercio minorista sin otros vehículos y sin combustibles
Datos3=read.table(file.choose(),header=T,sep=";",skip=10,dec=",",colClasses=c(rep("NULL",4),"numeric",rep("NULL",17)))
Datos3=ts(Datos3,freq=12,start=c(2013,1))
plot(Datos3)

#-------------------------------------------------------------------------------------------------------------------
rm(list=ls(all=TRUE))
#Leer anex-EMC-SeriesIndiceEmpalmados-vtas nominales lineas mercancia-may2023.csv, columna 6: Total comercio minorista sin combustibles ni vehículos
Datos4=read.table(file.choose(),header=T,sep=";",skip=10,dec=",",colClasses=c(rep("NULL",5),"numeric",rep("NULL",16)))
Datos4=ts(Datos4,freq=12,start=c(2013,1))
plot(Datos4)

#-------------------------------------------------------------------------------------------------------------------
rm(list=ls(all=TRUE))
#Leer anex-EMC-SeriesIndiceEmpalmados-vtas nominales lineas mercancia-may2023.csv, columna 7: 1. Alimentos (víveres en general) y bebidas no alcohólicas
Datos5=read.table(file.choose(),header=T,sep=";",skip=10,dec=",",colClasses=c(rep("NULL",6),"numeric",rep("NULL",15)))
Datos5=ts(Datos5,freq=12,start=c(2013,1))
plot(Datos5)

#-------------------------------------------------------------------------------------------------------------------
rm(list=ls(all=TRUE))
#Leer anex-EMC-SeriesIndiceEmpalmados-vtas nominales lineas mercancia-may2023.csv, columna 8: 2. Bebidas alcohólicas, cigarros, cigarrillos y productos del tabaco
Datos6=read.table(file.choose(),header=T,sep=";",skip=10,dec=",",colClasses=c(rep("NULL",7),"numeric",rep("NULL",14)))
Datos6=ts(Datos6,freq=12,start=c(2013,1))
plot(Datos6)

#-------------------------------------------------------------------------------------------------------------------
rm(list=ls(all=TRUE))
#Leer anex-EMC-SeriesIndiceEmpalmados-vtas nominales lineas mercancia-may2023.csv, columna 9: 3. Prendas de vestir y textiles
Datos7=read.table(file.choose(),header=T,sep=";",skip=10,dec=",",colClasses=c(rep("NULL",8),"numeric",rep("NULL",13)))
Datos7=ts(Datos7,freq=12,start=c(2013,1))
plot(Datos7)

#-------------------------------------------------------------------------------------------------------------------
rm(list=ls(all=TRUE))
#Leer anex-EMC-SeriesIndiceEmpalmados-vtas nominales lineas mercancia-may2023.csv, columna 10: 4. Calzado, artículos de cuero y sucedáneos del cuero
Datos8=read.table(file.choose(),header=T,sep=";",skip=10,dec=",",colClasses=c(rep("NULL",9),"numeric",rep("NULL",12)))
Datos8=ts(Datos8,freq=12,start=c(2013,1))
plot(Datos8)

#-------------------------------------------------------------------------------------------------------------------
rm(list=ls(all=TRUE))
#Leer anex-EMC-SeriesIndiceEmpalmados-vtas nominales lineas mercancia-may2023.csv, columna 11: 5. Productos farmacéuticos y medicinales
Datos9=read.table(file.choose(),header=T,sep=";",skip=10,dec=",",colClasses=c(rep("NULL",10),"numeric",rep("NULL",11)))
Datos9=ts(Datos9,freq=12,start=c(2013,1))
plot(Datos9)

#-------------------------------------------------------------------------------------------------------------------
rm(list=ls(all=TRUE))
#Leer anex-EMC-SeriesIndiceEmpalmados-vtas nominales lineas mercancia-may2023.csv, columna 12: 6. Productos de aseo personal, cosméticos y perfumería
Datos10=read.table(file.choose(),header=T,sep=";",skip=10,dec=",",colClasses=c(rep("NULL",11),"numeric",rep("NULL",10)))
Datos10=ts(Datos10,freq=12,start=c(2013,1))
plot(Datos10)

#-------------------------------------------------------------------------------------------------------------------
rm(list=ls(all=TRUE))
#Leer anex-EMC-SeriesIndiceEmpalmados-vtas nominales lineas mercancia-may2023.csv, columna 14: 8. Artículos y utensilios de uso doméstico
Datos11=read.table(file.choose(),header=T,sep=";",skip=10,dec=",",colClasses=c(rep("NULL",13),"numeric",rep("NULL",8)))
Datos11=ts(Datos11,freq=12,start=c(2013,1))
plot(Datos11)

#-------------------------------------------------------------------------------------------------------------------
rm(list=ls(all=TRUE))
#Leer anex-EMC-SeriesIndiceEmpalmados-vtas nominales lineas mercancia-may2023.csv, columna 15: 9. Productos para el aseo del hogar
Datos12=read.table(file.choose(),header=T,sep=";",skip=10,dec=",",colClasses=c(rep("NULL",14),"numeric",rep("NULL",7)))
Datos12=ts(Datos12,freq=12,start=c(2013,1))
plot(Datos12)

#-------------------------------------------------------------------------------------------------------------------
rm(list=ls(all=TRUE))
#Leer anex-EMC-SeriesIndiceEmpalmados-vtas nominales lineas mercancia-may2023.csv, columna 17: 11. Libros, papelería, periódicos, revistas y útiles escolares
Datos13=read.table(file.choose(),header=T,sep=";",skip=10,dec=",",colClasses=c(rep("NULL",16),"numeric",rep("NULL",5)))
Datos13=ts(Datos13,freq=12,start=c(2013,1))
plot(Datos13)

#-------------------------------------------------------------------------------------------------------------------
rm(list=ls(all=TRUE))
#Leer anex-EMC-SeriesIndiceEmpalmados-vtas nominales lineas mercancia-may2023.csv, columna 19: 13. Otras mercancías para uso personal o doméstico, no especificadas anteriormente
Datos14=read.table(file.choose(),header=T,sep=";",skip=10,dec=",",colClasses=c(rep("NULL",18),"numeric",rep("NULL",3)))
Datos14=ts(Datos14,freq=12,start=c(2013,1))
plot(Datos14)

#-------------------------------------------------------------------------------------------------------------------
rm(list=ls(all=TRUE))
#Leer anex-EMC-SeriesIndiceEmpalmados-vtas reales lineas mercancia-may2023.csv, columna 4: Total comercio minorista sin vehículos
Datos16=read.table(file.choose(),header=T,sep=";",skip=10,dec=",",colClasses=c(rep("NULL",3),"numeric",rep("NULL",18)))
Datos16=ts(Datos16,freq=12,start=c(2013,1))
plot(Datos16)

#-------------------------------------------------------------------------------------------------------------------
rm(list=ls(all=TRUE))
#Leer anex-EMC-SeriesIndiceEmpalmados-vtas reales lineas mercancia-may2023.csv, columna 5: Total comercio minorista sin otros vehículos y sin combustibles
Datos17=read.table(file.choose(),header=T,sep=";",skip=10,dec=",",colClasses=c(rep("NULL",4),"numeric",rep("NULL",17)))
Datos17=ts(Datos17,freq=12,start=c(2013,1))
plot(Datos17)

#-------------------------------------------------------------------------------------------------------------------
rm(list=ls(all=TRUE))
#Leer anex-EMC-SeriesIndiceEmpalmados-vtas reales lineas mercancia-may2023.csv, columna 6: Total comercio minorista sin combustibles ni vehículos
Datos18=read.table(file.choose(),header=T,sep=";",skip=10,dec=",",colClasses=c(rep("NULL",5),"numeric",rep("NULL",16)))
Datos18=ts(Datos18,freq=12,start=c(2013,1))
plot(Datos18)

#-------------------------------------------------------------------------------------------------------------------
rm(list=ls(all=TRUE))
#Leer anex-EMC-SeriesIndiceEmpalmados-vtas reales lineas mercancia-may2023.csv, columna 7: 1. Alimentos (víveres en general) y bebidas no alcohólicas
Datos19=read.table(file.choose(),header=T,sep=";",skip=10,dec=",",colClasses=c(rep("NULL",6),"numeric",rep("NULL",15)))
Datos19=ts(Datos19,freq=12,start=c(2013,1))
plot(Datos19)

#-------------------------------------------------------------------------------------------------------------------
rm(list=ls(all=TRUE))
#Leer anex-EMC-SeriesIndiceEmpalmados-vtas reales lineas mercancia-may2023.csv, columna 8: 2. Bebidas alcohólicas, cigarros, cigarrillos y productos del tabaco
Datos20=read.table(file.choose(),header=T,sep=";",skip=10,dec=",",colClasses=c(rep("NULL",7),"numeric",rep("NULL",14)))
Datos20=ts(Datos20,freq=12,start=c(2013,1))
plot(Datos20)

#-------------------------------------------------------------------------------------------------------------------
rm(list=ls(all=TRUE))
#Leer anex-EMC-SeriesIndiceEmpalmados-vtas reales lineas mercancia-may2023.csv, columna 10: 4. Calzado, artículos de cuero y sucedáneos del cuero
Datos22=read.table(file.choose(),header=T,sep=";",skip=10,dec=",",colClasses=c(rep("NULL",9),"numeric",rep("NULL",12)))
Datos22=ts(Datos22,freq=12,start=c(2013,1))
plot(Datos22)

#-------------------------------------------------------------------------------------------------------------------
rm(list=ls(all=TRUE))
#Leer anex-EMC-SeriesIndiceEmpalmados-vtas reales lineas mercancia-may2023.csv, columna 11: 5. Productos farmacéuticos y medicinales
Datos23=read.table(file.choose(),header=T,sep=";",skip=10,dec=",",colClasses=c(rep("NULL",10),"numeric",rep("NULL",11)))
Datos23=ts(Datos23,freq=12,start=c(2013,1))
plot(Datos23)

#-------------------------------------------------------------------------------------------------------------------
rm(list=ls(all=TRUE))
#Leer anex-EMC-SeriesIndiceEmpalmados-vtas reales lineas mercancia-may2023.csv, columna 12: 6. Productos de aseo personal, cosméticos y perfumería
Datos24=read.table(file.choose(),header=T,sep=";",skip=10,dec=",",colClasses=c(rep("NULL",11),"numeric",rep("NULL",10)))
Datos24=ts(Datos24,freq=12,start=c(2013,1))
plot(Datos24)

#-------------------------------------------------------------------------------------------------------------------
rm(list=ls(all=TRUE))
#Leer anex-EMC-SeriesIndiceEmpalmados-vtas reales lineas mercancia-may2023.csv, columna 17: 11. Libros, papelería, periódicos, revistas y útiles escolares
Datos27=read.table(file.choose(),header=T,sep=";",skip=10,dec=",",colClasses=c(rep("NULL",16),"numeric",rep("NULL",5)))
Datos27=ts(Datos27,freq=12,start=c(2013,1))
plot(Datos27)

