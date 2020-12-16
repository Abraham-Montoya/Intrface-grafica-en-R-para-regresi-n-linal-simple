rm(list=ls())
RGLS=function()
{
library(rpanel)
library(nortest)
library(lmtest)
##########################################################################
panel=rp.control(title="REGRESIÓN LINEAL",size=c(500,400),estimar=FALSE,pred=FALSE,
conf=FALSE,x1=0,diag=FALSE,linea=FALSE,cr=FALSE,resumen=FALSE,rho=FALSE,
betas=FALSE,con=.95,anova=FALSE,gra=FALSE,Kolmogorov=FALSE,Shapiro=FALSE,
Anderson=FALSE,Breusch=FALSE,Durbin=FALSE)
rp.textentry(panel,variable=x1,labels="Valor de x",
pos=c(190,65,120,20),foreground="blue",background="white",
keydown=TRUE)
rp.checkbox(panel,variable=estimar,labels="",
action=modelo,pos=c(170,105-40,20,20),background="white",foreground="blue")
rp.checkbox(panel,variable=pred,labels="Predicción",
action=modelo,pos=c(370,90-40,80,20),background="white",foreground="blue")
rp.checkbox(panel,variable=conf,labels="Confianza",
action=modelo,pos=c(370,120-40,80,20),background="white",foreground="blue")
rp.textentry(panel,variable=con,labels="Confianza",
pos=c(20,105-40,100,20),foreground="blue",background="white",
keydown=TRUE)
rp.checkbox(panel,variable=diag,labels="Diagrama de dispersión",
action=modelo,pos=c(20,180,150,20),background="white",foreground="blue")
rp.checkbox(panel,variable=linea,labels="Ajustar línea",
action=modelo,pos=c(20,210,100,20),background="white",foreground="blue")
rp.checkbox(panel,variable=cr,labels="Coeficiente de correlación",
action=modelo,pos=c(20,240,160,20),background="white",foreground="blue")
rp.checkbox(panel,variable=resumen,labels="Resumen del modelo",
action=modelo,pos=c(20,270,140,20),background="white",foreground="blue")
rp.checkbox(panel,variable=rho,labels="Prueba para Rho",
action=modelo,pos=c(200,180,110,20),background="white",foreground="blue")
rp.checkbox(panel,variable=betas,labels="Intervalos para los ß",
action=modelo,pos=c(200,210,130,20),background="white",foreground="blue")
rp.checkbox(panel,variable=anova,labels="ANOVA",
action=modelo,pos=c(200,240,70,20),background="white",foreground="blue")
rp.checkbox(panel,variable=gra,labels="Gráficos",
action=modelo,pos=c(350,180,70,20),background="white",foreground="blue")
rp.checkbox(panel,variable=Kolmogorov,labels="Kolmogorov-Smirnov",
action=modelo,pos=c(350,210,140,20),background="white",foreground="blue")
rp.checkbox(panel,variable=Shapiro,labels="Shapiro-Wilk",
action=modelo,pos=c(350,240,100,20),background="white",foreground="blue")
rp.checkbox(panel,variable=Anderson,labels="Anderson-Darling",
action=modelo,pos=c(350,270,120,20),background="white",foreground="blue")
rp.checkbox(panel,variable=Breusch,labels="Breusch-Pagan",
action=modelo,pos=c(350,300,120,20),background="white",foreground="blue")
rp.checkbox(panel,variable=Durbin,labels="Durbin-Watson",
action=modelo,pos=c(350,330,120,20),background="white",foreground="blue")
rp.text(panel,text="ANÁLISIS DE RESIDUALES",pos=c(350,150,70,20),foreground="black")
rp.text(panel,text="GENERAL",pos=c(50,150,70,20),foreground="black")
rp.text(panel,text="SIGNIFICANCIA DEL MODELO",pos=c(175,150,70,20),
foreground="black")





modelo=function(panel)
{
datos=read.table("clipboard",header=T)
x=datos[,1]
y=datos[,2]
##############################################
x1=as.numeric(panel$x1)
con=as.numeric(panel$con)
################################################################################
reg=lm(y~x)###################Modelo 
if(panel$cr==TRUE)
	{
	print(cor(x,y))######################Coeficiente de correlación
	}
if(panel$rho==TRUE)
	{
	print(cor.test(x,y))#################Prueba para el coeficiente de correlación
	}
if(panel$diag==TRUE)
{
plot(y~x,main="Diagrama de dispersión")#####################Diagrama de disperción
}
if(panel$linea==TRUE)
	{	
	lines(reg$fitted~x)###########Linea ajustada
	}  
if(panel$resumen==TRUE)
	{
	print(summary(reg))##################Resumen del modelo
	}
if(panel$betas==TRUE)
	{
	print(confint(reg,level=con))##################Intervalos para los beta estimados 
	}
xe=data.frame(x=c(x1))###Valores de x a estimar
if(panel$estimar==TRUE)
	{
	print(predict(reg,xe,interval="none"))#Estimación puntual
	}
if(panel$pred==TRUE)
	{
	print(predict(reg,xe,interval="prediction",level=con))#Intervalo de predicción
	}
if(panel$conf==TRUE)
	{
	print(predict(reg,xe,interval="confidence",level=con))#Intervalo de confianza
	}
###########################ANOVA#################
ANOVA=aov(reg)###Se crea el modelo anova
if(panel$anova==TRUE)
	{
	print(summary(ANOVA))###Resumen del analisis de varianza
	}
#############Analisis de residuos##########

	
####SUPUESTO DE NORMALIDAD
####PRUEBAS DE NORMALIDAD
if(panel$Kolmogorov==TRUE)
	{
	print(lillie.test(reg$residuals))#Kolmogorov-Smirnov
	}
if(panel$Shapiro==TRUE)
	{
	print(shapiro.test(reg$residuals))#Shapiro-Wilk
	}
if(panel$Anderson==TRUE)
	{
	print(ad.test(reg$residuals))#Anderson-Darling
	}
###SUPUESTO DE HOMOCEDASTICIDAD
if(panel$Breusch==TRUE)
	{
	print(bptest(reg))#Prueba Breusch-Pagan
	}
###SUPUESTO DE INDEPENDENCIA
if(panel$Durbin==TRUE)
	{
	print(dwtest(y~x,alternative="two.sided"))#Prueba Durbin-Watson
	}
orden<-1:length(x)

if(panel$gra==TRUE)
	{
		x11()
		plot(y~x)
		par(mfrow=c(2,2))
		plot(reg)
		x11()
		plot(orden,reg$residuals,main="Grafico de orden contra residuales")
		lines(orden,reg$residuals)
	}
return(panel)
	
}
}
RGLS()






