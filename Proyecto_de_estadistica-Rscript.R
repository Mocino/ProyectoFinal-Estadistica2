require(stats)
library (ggplot2)
library(lubridate)
library(forecast)
library (dplyr)
library(tidyverse)

#Marco Ederson Josue Yol Raxcaco - 2490 19 19523 // Emerson Christian Andres Perez Chen 2490-19-13220

Facebook <- read.csv("Facebook")
Banco <- read.csv("Banco")


# No.1

#El genero puede ser un factor que altere de manera drastica las oportundades de los 
#usuarios a ser tenderncia en Facebook

#H0: El genero no afecta (Genero = Tendencia)
#H1: El genero si afecta (Genero =/= Tendencia)

#Grafico (ggplot)
ggplot(data=Facebook, aes(x=gender,y=tenure, color=gender ))+ geom_boxplot()+theme_bw()

#Metodo (Anova)
anova<- aov(Facebook$tenure~Facebook$gender)
summary(anova)

#P-value = 2e-16

#el P-value es mucho menor a 0.05 esto quiere decir que se rechaza la hipotesis nula esto quiere decir
#que segun el genero afecta las posibilidades de ser tendencia

#Analisis
#Segun el analisis anova el genero si afecta en ser tendencia en Facebook pero como se puede ver
#en la grafica, los usuarios que si dicen su genero (masculino y gemenino) tiene la misma media de tendencia
#en donde el genero femenino tiene mas datos atipicos que el masculino que significa que hay mas mujeres 
#siendo tendencia por distintas cuestriones que los hombres y tambien podemos notar que los usuaio que no definieron
#su genero tiene una mayor media de ser tendencia esto significa que muchos usuasios sin definir genero en su perfil 
#tienen a ser mas tendencia e igual tienen datos atipicos bajos en donde se ve que no siempre se es tendencia con ese
#factor

#segun nuestros datos de investigacion
#https://www.euskadi.eus/contenidos/noticia/liburua_sexismoa_gazteak_7/es_def/adjuntos/sexismo_gizarte_sareetan_c.pdf
#Este comportamiento es generado gracias al miedo o desigualdad que existe en las redes sociales ya que las personas
#pueden presentar en redes sociales ya que al tener mas informacion en su perfil pueden ser un mayor blanco de burlas o 
#molestias de los demas usuarios y a diferencia de los que no comparten la informacion de su genero se sienten mas libres 
#con respecto a dar sus opiniones que pueden llegar a ser mas tendencia a diferencia de los que si definiron su genero
#pero como en todo no siempre es asi ya que tambien se dice que las mujeres tienen a dar mas su opinion o publicar mas a 
#diferencia de los hombres asi que hay mas datos atipicos en mujeres que en hombres a pesar de tener una media que rosa lo
#similar

# No.2

#En Facebok el genero es un factor que puede alterar los likes que da un usuario

#H0: El genero no afecta (Genero = likes)
#H1: El genero si afecta (Genero =/= likes)


#Grafico (ggplot)
ggplot(data=Facebook, aes(x=gender,y=likes, color=gender ))+ geom_boxplot()+theme_bw()

#Metodo (Anova)
anova<- aov(Facebook$likes~Facebook$gender)
summary(anova)

#P-value = 2e-16

#Al se el P-value mucho menor a 0.05 quiere decir que se rechaza la hipotesis nula que significa que el 
#genero afecta en los likes que realiza un usuario 

#como se ve en la grafica casi todos los generos tiene una meedia simirar pero con gran diferencia en datos atipicos
#se nota que el genero masculino tiene likes mas altos pero menos constantes a diferencia del genero fenenino
#en donde los likes no son tan altos mero mas constantes 

#https://www.vice.com/es/article/zndpjy/por-que-los-hombres-dan-like-literalmente-a-todos-los-perfiles-de-tinder
#Esto puede deberse a que los gustos y posteos de los usuarios, los hombres ven mas publicaciones de humor en donde 
#se vuelven mas tendencia y como va al publico de humor los hombres destacan en esto a diferencia de las mujeres que 
#son mas reservadas con los gustos que comparte que genere menos likes que puede dar, a diferencia de el analisis anterior
#los que no definin su genero no realizan muchos likes que es a causa de mantener su privasidad con respecto a quien es 


# No. 3

#El genero afecta en la en la cantidad de amigos que conoce esto quiere decir que no son amistades que facebook le recomendo
#si no que a amistades que el conoce o viven cerca a el usuario que es gracias a facebook

#H0: El genero no afecta (Genero = conteo de amistades)
#H1: El genero si afecta (Genero =/= conteo de amistades)

#Grafico (ggplot)
ggplot(data=Facebook, aes(x=gender,y=friend_count, color=gender ))+ geom_boxplot()+theme_bw()

#Metodo (Anova)
anova<- aov(Facebook$friend_count~Facebook$gender)
summary(anova)

#P-value = 2e-16
#Al ser el P-value mucho menor a 0.05 se rechaza la hipotesis nunla lo que quiere decir que el genero si
#afecta en en el conteo de amistades de los usuarios

#La grafica indica que tanto masculino como femenino tiene nos datos similares tanto en media como en los datos atipicos
#pero hay una peque?a diferencia que entre las mujeres los datos atipicos son mas constantes que los hombres que quiere decir que
#las mujeres en un minimo caso puede tener mas amistade que los hombre, a gran diferencia de los usuarios sin su genero de definido
#en donde tiene menos amistades en donde siempre es una constante que quiere tener su privacidad apartada de las 
#redes sociales compartiendo menos informacion con personas o amistades que puede tener


# No.4


#El gener puede afectar en la edad de los usuarios 

#H0: El genero no afecta (Genero = conteo de amistades)
#H1: El genero si afecta (Genero =/= conteo de amistades)

#Grafico (ggplot)
ggplot(data=Facebook, aes(x=gender,y=age, color=gender ))+ geom_boxplot()+theme_bw()

#Metodo (Anova)
anova<- aov(Facebook$age~Facebook$gender)
summary(anova)
#P-value = 2e-16

#al ser e P-value mucho menor a 0.05 quiere decir que se rechaza la hipotesis nula que significa que 
#dependiendo la edad si hay diferencia de edades

#como se ve en la grafica el genero femenino tieneuna media un poco mas alta que los hombres que significa 
#existen mas usuarios feneminos con una edad que ronda los 30 a?os y menos hombres pero tambien se nota que los 
#usuarios sin genero definido hay una mayor cantidad con edades mayores esto puede deberse a que son cuentas en dode
#queren tener privacidad y no compartin sus datos que causa que coloquen edades mayores o que no corresonden y colocan
#mas altas para desapersivir a atacantes 

# No.5

#La edad puede ser un factor que tiene relacion on el conteo de amigos que se tiene en la red
#social Facebook

#H0 = Si tiene conrrelacion entre los datos 
#H0 = No tiene correclacion entre los datos

#Grafico(Plot)
plot(Facebook$age,Facebook$friend_count, col="Blue", main = "Correlacion Genero y Conteo de Amigos", ylab = "Conteo de Amigos", xlab = "Edad")
modelolineal<-lm(Facebook$friend_count~Facebook$age)
abline(modelolineal,col ="red")

#Metodo(Correlacion)
cor(Facebook$friend_count,Facebook$age)

#-0.02740737

#en este caso el valor de correlacion es negativo esto quiere decir que se rechaza la hipotesis nula 

#La correacion negativa quiere decir que entre mas alto es un dato el otro es bajo como en este ejmplo se nota que 
#la correlacion es negatica en donde se ve un gran conteo de amistades en usuarios entre los 20 a?os y otro pico en 
#los que sobrepasan lo 100 a?os y la linea de correlacion siempre se mantiene bajo por no tener ni un tipo de asociacion

#A a edad de 15 a 30 a?os los jovenes y adultos tienden a socializar mas y tener mas amistades que decae en los 40 por cuestiones
#de trabajo o problemas que se nota un pico entre los 70 y otro gran pico en los mayores de 100 a?os en donde se son influidos a 
#las redes sociales y familiares queridos o amigos tiende a amistarse en dicha red social


# No.6

#el conteo de amigos total tiene alguna correlacion con los likes que da 

#H0 = Si tiene conrrelacion entre los datos 
#H0 = No tiene correclacion entre los datos

#Grafico(Plot)
plot(Facebook$friend_count,Facebook$likes, col="Blue", main = "Correlacion likes y Conteo de Amigos", ylab = "Likes", xlab = "Conteo de amigos")
modelolineal<-lm(Facebook$likes~Facebook$friend_count)
abline(modelolineal,col ="red")

#Metodo(Correlacion)
cor(Facebook$friend_count,Facebook$likes)

#0.2980169

#La correlacion es minima asi que se puede decir que se hacepta la hipotesis nula pero de manera minima 
#ya que si existe una correlacion que no es negativa pero lo tiene de manera minima 

#en la grafica se explica que menos de 1000 amigos se tienden muchos usuarios a dar pocos likes pero tiene un pico elevado
#en donde se da mucho en unas ocaiones pero a muchos amigos menos usuarios dan likes y si lo dan no sobrepasa de
#gran manera


# No.7

#Los likes que da tiene alguna correlacion con los amigos que inicio en facebook esto quiere decir que son amigos que conocio
#gracias a la red social y 

#H0 = Si tiene conrrelacion entre los datos 
#H0 = No tiene correclacion entre los datos

#Grafico(Plot)
plot(Facebook$friendships_initiated,Facebook$likes, col="Blue", main = "Correlacion Likes y Amistades iniciadas", ylab = "Likes", xlab = "Amigos Iniciados")
modelolineal<-lm(Facebook$likes~Facebook$friendships_initiated)
abline(modelolineal,col ="red")

#Metodo(Correlacion)
cor(Facebook$friendships_initiated,Facebook$likes)

#0.2855923

#en ese caso el valor de correlacion es minima pero no negativa asi que se nota que los usuarios que no tienen
#muchos amigos iniciados tienden a dar mas likes que los que tienen mas amigos iniciados en donde el promedio y la 
#constante de likes es menos esto es causa de que se inician amigos pero no se tiene mucha confianza para publicar o 
#dar opiniones de los posteos de los amigos

# No.8

#Tiene alguna correlacion el conteo de amigos total con los amigos iniciados

#H0 = Si tiene conrrelacion entre los datos 
#H0 = No tiene correclacion entre los datos

#Grafico(Plot)
plot(Facebook$friendships_initiated,Facebook$friend_count, col="Blue", main = "Correlacion Genero y Conteo de Amigos", ylab = "Conteo de Amigos", xlab = "Edad")
modelolineal<-lm(Facebook$friend_count~Facebook$friendships_initiated)
abline(modelolineal,col ="red")

#Metodo(Correlacion)
cor(Facebook$friend_count,Facebook$friendships_initiated)

# 0.82585

#En este caso se nota que el valor de correlacion es mayor que quiere decir que si tiene correlacion en este caso
#se nota que el total de amigos que se tiene porque concoce previamente tiene una gran correlacion con los nuevos amigos
#inicados recomendados por la plataforma

#esto da a entender que el que tiene mas amigos esta mas abierto a inicar mas amigos nuevos en redes sociales a los que son 
#cerrados que tambien tienden a ser menos abiertos a conocer nuevos amigos 

# No.9

#El Genero es un factor que afecta con los amigos que inicia en la plataforma de facebook

#H0: El genero no afecta (Genero = Amigos Iniciados)
#H1: El genero si afecta (Genero =/= Amigos Iniciados)

#Grafico (ggplot)
ggplot(data=Facebook, aes(x=gender,y=friendships_initiated, color=gender ))+ geom_boxplot()+theme_bw()

#Metodo (Chi2)
anova<- aov(Facebook$friendships_initiated~Facebook$gender)
summary(anova)

#Pvalue = 2e-16
#En este casi se nota que el P-value es el valor minimo esto quiere decir que no se acepta la hipotesis nula
#esto da a entender que el genero afecta con los amigos que se inician en la plataforma de fecebook en donde se 
#nota en la grafica que en el genero mastulino tiene un mayor pico de amigos iniciados pero menos constantes y las 
#mujeres tiene un menor pico de amistades iniciadas pero mas constantes con respecto a los datos atipicos 
#y se nota que en los usuarios que no definieron su genero tienen menos datos atipicos en donde no se inician amigos nuegos
#que sigue siendo una constante en todo el dataset, en donde los que no tiene su Genero definido comparten menos informacion
#obre ellos para mantener su privacidad apartada de las redes sociales


# No.10

#Los likes desde internet tienen una correlacion con el conteo de amigos 

#H0 = Si tiene conrrelacion entre los datos 
#H0 = No tiene correclacion entre los datos

#Grafico(Plot)
plot(Facebook$friend_count,Facebook$www_likes, col="Blue", main = "Correlacion Genero y Conteo de Amigos", ylab = "Conteo de Amigos", xlab = "Edad")
modelolineal<-lm(Facebook$www_likes~Facebook$friend_count)
abline(modelolineal,col ="red")

#Metodo(Correlacion)
cor(Facebook$www_likes,Facebook$friend_count)

# 0.2298027

#En este caso se nota que no tienen mucha correlacion ya que es muy poco el valor correlativo que significa que no importa
#de que plataforma (mobile o PC) se este siempre tendra diferenties amigos totales ya que no afectaria por cuestiones de acceisbilidad



# No.11

#El genero afecta en los likes recibidos 

#Grafico (ggplot)
ggplot(data=Facebook, aes(x=gender,y=likes_received, color=gender ))+ geom_boxplot()+theme_bw()

#Metodo (Chi2)
anova<- aov(Facebook$friendships_initiated~Facebook$gender)
summary(anova)

#P-Value = 2e-16
#En este caso el P-values es menor a 0.05 entonce se rechaza la hipotesis nula que significa que el genero
#si afecta en los likes recividos 

#En donde se not que las mujeres tiene un mayor pico de likes recividos e igual un poco mas constantes a diferencia
#que los hombres en donde tiene  un menor pico de likes recibidos y menos constantes segun los datos atipicos y los 
#usuairos ue no definieron su genero tienen menos likes recividos tanto en el pico maximo como en la constancia

#esto se explica gracias a la constante de edades que es de 18 a 22 a?os que es en donde segun estudiso tien mas curiosisdad
#y atraccion el sus gustos y las estadisticas muestran que los hombres tiene mas intereses en las mujeres y por eso reciven mas 
#likes por y las mujeres tieden a compartir ese tipo de informacion ya que tiene un mayor valor a la privacidad y tambien es un dato
#importante en el sexismo o violencia en internet en donde las mujeres reciven mas atencion ya sea deceada o no


# No.12

#El genero afecta si tiene mas likes hechos desde la aplicacion mobile de facebook

#H0: El genero no afecta (Genero = Likes desde Mobile)
#H1: El genero si afecta (Genero =/= Likes desde Mobile)

#Grafico (ggplot)
ggplot(data=Facebook, aes(x=gender,y=mobile_likes, color=gender ))+ geom_boxplot()+theme_bw()

#Metodo (Anova)
anova<- aov(Facebook$mobile_likes~Facebook$gender)
summary(anova)

#P-value = 2e-16

#Se nota que el P-value es el minimo y por ende es menor a 0.05 que quiere decir que se rechaza la hipotesis nula
#en dondoe da a etender que dependiedo el genero afecta si usa mas mobile o no

#En la grafica se nota que los hombres tiene un mayor pico de likes desde el telefono pero tiene menos constancia
#en las mujeres tiene menos pico de likes mobiles pero una mayor constancia 


# No.13

#El genero afecta si tiene mas likes hechos desde la pagina web de facebook

#H0: El genero no afecta (Genero = likes desde la pagina web)
#H1: El genero si afecta (Genero =/= likes desde la pagina web)

#Grafico (ggplot)
ggplot(data=Facebook, aes(x=gender,y=www_likes, color=gender ))+ geom_boxplot()+theme_bw()

#Metodo (Chi2)
anova<- aov(Facebook$www_likes~Facebook$gender)
summary(anova)

#P-alue = 2e-16 

#Al ser el P-value el minimo quiere decir que es menor a 0.05 y por ende no se acepta la hipotesis
#nula lo que significa que el genero si afecta en los likes realizados desde la pagina web de facebook

#En esta grafica se nota que el genero femenino tiende a dar mas likes desde la pagina web con constancia 
#y un pico maximo mayor al genero masculino, esto puede explicarse a la privasidad o seguridad que se tiene desde 
# la pagina web, los hombres tienden a querer todo con facilidad sin importar la seguiridad que se tenga ya que se a probado
#que desde aplicaciones es mas accesible a ataques o robo de informacion a diferencia de las mujeres quienes aprecian
#mas la privacidad por eso utilizan paginas web. Aunque en los ultimos estudios se nota que hay un crecimiento de uso
#de telefonos en mujeres esto puede ocacionar un aumento en este tipo de dato que afectaria en un dataset mas actualizado


# No.14

#Â¿Cual es la edad con mas promedio que usa la red social Facebook?

#Shapiro
summary(Facebook)
hist(Facebook$age)
# shapiro.test(Facebook$age) (el test por medio de shapiro no es posible realizarlo por tener un limite
#de 3 a 5000 datos en donde nosotros excedemos la cantidad maxima)

#Como se nota en la grafica la edad con mayor promedio de uso de facebook es entre 18 a 22 aÃ±os
#esto se explica que las nuevas generaciones ya crecen con la tecnologia al alcanze de la mano 
#esto ocaciona que se sientan mas comodos o familiarizado de uso de redes sociales y tienen un mayor aumento
#a este rango de edades a la accesibilidad que tiene en esta edad en donde son adolecentes y quiren conocer 
#personas que compartan sus gustos para poder compartir o sentirse adentro de una comunidad


# No.15

#Â¿habra dependencia entre grado segun la region?

#h0 = Las variables son Independientes, El grado no depende de la region
#h1 = Las variables no son Independientes , El grado depende de la region


RG <- table(Banco$grade, Banco$region, dnn=c("Grado", "Region"))
RG

#Grafico (Mosaico)
mosaicplot(RG, main=deparse("Relacion entre grado y region"), col=c("blue","purple", "red", "orange", "yellow"))

#Metodo (Chi2)
chisq.test(RG)

#2.2e-16
2.2e-16 < 0.05

#El pvalue es menor a 0.05
#El grado depende de la region
#se rechaza la hipotesis nula 

#---analisis---#

#-Considero que existen diferencias de rango, grado o categorÃ­a dependiendo el sector o regiÃ³n, porque considero que en algunas regiones/sectores hay mayor nivel y calidad de vida que en otro sector/regiÃ³n.
#Nos podemos fijar en sector urbano y otro rural, donde Estratos sociales influyen en un territorio 
#âla regiÃ³n presenta caracterÃ­sticas especÃ­ficas que reclaman adaptar y generar una teorÃ­a sobre la estratificaciÃ³n desde la casa.â
#(Maria Marinho, VeronicaQuiroz, pÃ¡g. 20).
#fuente: https://repositorio.cepal.org/bitstream/handle/11362/44328/1/S1801180_es.pdf

# No.16

#Â¿Las variables son independientes entre condicion de pretamos y categoria de ingresos?

#h0 = Las variables son independientes, la condicion de prestamo no depende de la categoria de ingreso
#h1 = Las variables no son independientes, la condicion de prestamo depende de la categoria de ingreso

Loip <- table(Banco$income_category, Banco$loan_condition, dnn=c("Categoria de Ingreso", "Condicion de prestamo"))
Loip

#Grafico (Mosaico)
mosaicplot(Loip, main=deparse("Relacion categoria de ingresos y condicion de prestamos"), col=c("blue","dark green"))

#Metodo (Chi2)
chisq.test(Loip)

#2.2e-16
2.2e-16 < 0.05

#El pvalue es menor a 0.05
#la condicion de prestamo depende de la categoria de ingreso
#se rechaza la hipotesis nula

#---analisis---#

#Hay dependencia entre el ingreso y la condiciÃ³n de prÃ©stamo, pongamos como ejemplo el prÃ©stamo de un local:
# *una persona con Ingresos bajos, tiene mÃ¡s necesidad de pedir un prÃ©stamo para poder poner un local
#*una persona con Ingresos Medios, puede tener o no tener la necesidad de pedir un prÃ©stamo
#*una persona con Ingresos Altos, tiene poca necesidad de adquirir un prÃ©stamo
#Mientras tanto en la condiciÃ³n de prÃ©stamo indica que hay mayor aceptaciÃ³n a un buen prÃ©stamo que un mal prÃ©stamo
#âNo todas las personas que solicitan un crÃ©dito lo consiguen ni obtienen los mismos tÃ©rminos y condiciones: existen algunos factores que son tomados en consideraciÃ³n por los prestadores para determinar su solvencia, como por ejemplo, el nivel de ingresos, gastos, deudas y antecedentes de crÃ©dito.â


#Fuente: https://www.consumidor.ftc.gov/articulos/s0347-sus-derechos-de-igualdad-de-oportunidad-de-credito

# No.17

#Â¿3. los datos de taza de interes y pagos de interÃ©s estan en normalidad?

#los datos si presentan normalidad
#los datos no presentan normalidad
summary(Banco)


IR <- split(Banco$interest_rate, Banco$interest_payments)
par(mfrow=c(1,2))

#histogramas
hist(IR$High)

#histogramas
hist(IR$Low)

par(mfrow=c(1,1))

#los datos no presentan normalidad segun la grafica
#se rechaza la hipotesis nula

# No.18

#Â¿los datos de monto del prÃ©stamo por Plazo presentan normalidad?

#los datos si presentan normalidad
#los datos no presentan normalidad
summary(Banco)

IR <- split(Banco$loan_amount, Banco$term)
par(mfrow=c(1,2))

#histogramas
hist(IR$` 36 months`)

#histogramas
hist(IR$` 60 months`)

par(mfrow=c(1,1))

#los datos presentan normalidad segun la grafica
#no se rechaza la hipotesis nula

# No.19

#Â¿tiene dependencia el interes con los aÃ±os que pasen

#h0=Las dos variable son independientes, el interes no tiene dependencia con los aÃ±os que pasen
#h1=las 2 variables no son independientes, el interes tiene dependencia con los aÃ±os que pasen 

#Grafico = Plot
plot(Banco$year, Banco$interest_rate, col="orange", main = "Correlacion aÃ±o e intereses", ylab = "Tasa de Interes", xlab = "AÃ±os")

YL<-lm(Banco$interest_rate~Banco$year)


abline(YL,col ="dark green")

#Metodo = Correlacion
cor(Banco$year,Banco$interest_rate)
-0.06363243<0.05

# la correlacion nos da un resultado de -0.06363243
# si, si hay dependencia en los interes mientras transcuren los aÃ±os
# Se rechaza la hipotesis nula

#---analisis---#

#-Las 2 variables no son independientes, el interÃ©s tiene dependencia con los aÃ±os que pasen
#Considero que hay dependencia entre el interÃ©s y los aÃ±os que pasen, considero que es por la popularidad y la demanda que cada aÃ±o tiene un banco
#âUna de las principales razones de cambios en las tasas de interÃ©s es por la demanda y oferta de crÃ©dito en el mercado.â
#Fuente: https://www.afipopular.com.do/app/do/consejo_experto.aspx?id=10545


# No.20
#Â¿ hay independencia en el monto del prestamo con los aÃ±os que pasen

#h0=las variables son independientes, el monto del prestamo no depende  con los aÃ±os
#h1=las variables NO son independientes, el monto del prestamo depende  con los aÃ±os 

#Grafico = Plot
plot(Banco$year, Banco$loan_amount, col="purple", main = "Correlacion aÃ±os e monto del prestamo", ylab = "AÃ±os", xlab = "monto del prestamo")
LY<-lm(Banco$loan_amount~Banco$year)


abline(LY,col ="blue")

#Metodo = Correlacion
cor(Banco$year,Banco$loan_amount)
0.1037222<0.05

# la correlacion nos da un resultado de 0.1037222
# significa que si hay una gran relacion entre monto del prestamo y aÃ±os
# no Se rechaza la hipotesis nula

#---analisis---#
#No, no hay relaciÃ³n del monto de prÃ©stamo con los aÃ±os, considero mas que es un factor externo, como con el ente o sociedad que se asociÃ³ o la economÃ­a del paÃ­s


# No.21

#Â¿hay similitud en el monto de prestamo segun la propiedad de vivienda?

#Ho: no hay diferencias en el monto de prestamo por la propiedad de vivienda 
#H1: si hay diferencias en el monto de prestamo por la propiedad de vivienda

#Grafico (ggplot)
ggplot(data=Banco, aes(x=home_ownership, y=loan_amount, color=home_ownership ))+ geom_boxplot()+theme_bw()

#Metodo (Chi2)
anova<-aov(Banco$loan_amount~Banco$home_ownership)
summary(anova)

# Pr(>F)
# 2e-16
# (2e-16> 0.05)
2e-16< 0.05

#el pvalue es menor a 0.05 la grafica y el analisis indican que hay diferencias
#Esto indica que no hay similitud de monto de prestamos por la propiedad de vivienda
#se rechaza la hipotesis nula

#---analisis---#

#Pueden existir diferencias en el monto de prÃ©stamo dependiendo las necesidades
#personales de la persona segÃºn la propiedad de vivienda que tenga


# No.22

#Â¿hay igual en la taza de intereses no dependiendo el grado?

#Ho: no hay diferencias en la taza de intereses por el grado
#H1: si hay diferencias en la taza de intereses por el grado   

#Grafico (ggplot)
ggplot(data=Banco, aes(x=grade, y=interest_rate, color=grade ))+ geom_boxplot()+theme_bw()

#Metodo (Chi2)
anova<-aov(Banco$interest_rate~Banco$grade)
summary(anova)

# Pr(>F)
# 2e-16
# (2e-16> 0.05)
2e-16<0.05

#el pvalue es menor a 0.05 la grafica y el analisis indican que si hay diferencias
#Esto indica que no hay similitud en la taza de intereses por el grado
#se rechaza la hipotesis nula

#---analisis---#

#Si hay diferencias, porque el nivel o jerarquÃ­a que alguien tenga, puede generarle 
#mÃ¡s beneficios, en este caso la taza de intereses para los usuarios A es menor a 10%,
#a diferencia de los usuarios G que no bajan de 20%


# No.23

#Â¿hay diferencia en el monto del prestamo por el tipo de aplicacion
#  H0= No, no hay difenencia en el monto del prestamo por el tipo de aplicacion 
#  H1= si, si hay diferencias en el monto del prestamo por el tipo de aplicacion

#Grafica(Boxplot)
boxplot(loan_amount~application_type, data = Banco, col = c("light green","orange"))

#Metodo(Test de Hipotesis / T.test)
t.test(Banco$loan_amount~Banco$application_type, alternative = "two.sided")

# Pr(>F)
# 2.2e-16
# (2.2e-16 > 0.05)
2.2e-16<0.05
#El P-value es menor al 0.05
#esto quiere que si hay diferencia en el monto por el prestamo
#se rechaza la hipotesis nula

#---analisis---#

#Es posible que cuando se una solicitud conjunta al momento de un
#prÃ©stamo, como estÃ¡ asociada se debe decir mayor monto para poder repartirse en partes moderadas para cada sujeto.


# No.24

#Â¿hay diferencia en la condicion de prestamo con el monto de prestamo 
#  H0= No, no hay difenencia en la condicion de prestamo por el monto del tiempo 
#  H1= si, si hay difenencia en la condicion de prestamo por el monto del tiempo

#Grafica(Boxplot)
boxplot(loan_amount~loan_condition, data = Banco, col = c("blue","dark green"))

#Metodo(Test de Hipotesis / T.test)
t.test(Banco$loan_amount~Banco$loan_condition, alternative = "two.sided")

# Pr(>F)
# 0.0009836
# (0.0009836 > 0.05)
0.0009836<0.05
#El P-value es menor al 0.05
#esto quiere que si hay diferencia en el monto de prestamo con la condicion de prestamo
#se rechaza la hipotesis nula

#---analisis---#
#Hay diferencia ya que al momento de condicionar un prÃ©stamo se deberÃ¡ de ver si a cumple,
#en este caso, se puede ver que hay menos aceptaciÃ³n de prÃ©stamo

# No.25


#Â¿ay diferencia en pagos de interes por la tasa de interes 
#  H0= No, no hay diferencia en pagos de interes por la tasa de interes 
#  H1= si, si hay diferencia en pagos de interes por la tasa de interes

#Grafica(Boxplot)
boxplot(interest_rate~interest_payments, data = Banco, col = c("blue","dark green"))

#Metodo(Test de Hipotesis / T.test)
t.test(Banco$interest_rate~Banco$interest_payments, alternative = "two.sided")

# Pr(>F)
# 2.2e-16
# (2.2e-16 > 0.05)
2.2e-16<0.05


#el pvalue ES menor a 0.05
#esto quiere decir que hay diferencia en pagos de interes por la tasa de interes 
#se rechaza la hipotesis nula

#---analisis---#

#Al parecer el pago de intereses por lo regularmente es alto, puede deberse a 
#que hay mucho dinero circulando en ese banco

# No.26
tasas <-  read.csv("E:/Escritorio/Marco/Estadisitca/Proyecto/loan_final313.csv")

Banco$Fecha = as.Date(tasas$issue_d,format="%d/%m/%Y")

Banco$sem = week(Banco$Fecha)

Banco$Anio = year(Banco$Fecha)

interesemanal = Banco %>% group_by(Anio,sem) %>% summarise(media=mean(interest_rate))

serieinteres = ts(interesemanal$media, start=c(2008,1),frequency = 50)

#grafica de serie de tiempo de la tasa de interes
plot(serieinteres)
#----

componente = decompose(serieinteres)

tasas811 = window(serieinteres, start=2008,end=2010)

primerP = forecast(tasas811,11)

#Vemos la prediccion que tomara apartir de 2010
plot(primerP)

#---

modd = auto.arima(tasas811)

segundoP = forecast(modd,10)

plot(segundoP)

moddHT = HoltWinters(tasas811)

tercerP = forecast(moddHT,10)

plot(tercerP)

accuracy(primerP)

accuracy(segundoP)

accuracy(tercerP)

```