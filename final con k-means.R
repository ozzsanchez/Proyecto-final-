# Proyecto con  k medias 
# Cargar la matriz de datos.

# base de datos 
library(readxl)
base_1_1 <- read_excel("proyecto final dendograma/base 1.1.xlsx")

# convertir a freme
basefreme<-data.frame(base_1_1)
basefreme[c("track_name")]<-NULL 
basefreme
# etiquetas 
rownames(basefreme)=paste(base_1_1$track_name)
basefreme
#
colnames(basefreme)
#---------------------------------
#    Metodo k-means
#---------------------------------

#1.- Separacion de filas y columnas.

dim(basefreme)
# filas 
n<-dim(basefreme)[1]
# columnas 
p<-dim(basefreme)[2]


# 2.- Estandarizacion univariante.
X.s<-scale(basefreme)

# 3.- Algoritmo k-medias (3 grupos)
# nstart: cantidad de subconjuntos aleatorios que se escogen
# para realizar los calculos de algoritmo.
# X.S: estadarizacion de los datos 
Kmeans.3<-kmeans(X.s, 2, nstart=25)

# centroides
Kmeans.3$centers

# cluster de pertenencia
Kmeans.3$cluster


# 4.- suma de cuadrados dentro de los grupos SCDG

SCDG<-sum(Kmeans.3$withinss)
SCDG

# 5.- separa los Clusters
cl.kmeans<-Kmeans.3$cluster
cl.kmeans

# 6.- Scatter plot con la division de grupos
# obtenidos (se utiliza la matriz de datos centrados).
col.cluster<-c("orange", "green")[cl.kmeans]
pairs(X.s, col=col.cluster, main="k-meadias", pch=19)

#-----------------------------------------
#  Visualizacion con las dos componentes principales
#-----------------------------------------

library(cluster) 

clusplot(X.s, cl.kmeans, 
         main="Dos primeras componentes principales")

text(princomp(X.s)$score[,1:2],
     labels=rownames(X.s), pos=1, col="orange")

#-------------------------------------
#  Silhouette
#--------------------------------------
# Representacion grafica de la eficacia de
# clasificacion de una observacion dentro de un
# grupo.

# 1.- Generacion de los calculos
dist.Euc<-dist(X.s, method = "euclidean")
Sil.kmeans<-silhouette(cl.kmeans, dist.Euc)

#2.- Generacion del grafico
plot(Sil.kmeans, main="Silhouette for k-means", 
     col="orange")
