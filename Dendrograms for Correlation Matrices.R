#Insalling Packages
pcks<-c("Hmisc", "readxl", "Igraph", "fBasics", "vegan", "factoextra")
install.packages(pcks)

#Reading Tidy Data 
library(readxl)                                       
Pre_Crise <- read_excel("D:/Pre_Crise.xlsx",                         #My file´s name is Pre_Crise and has 199 columns, one for each asset. 
                        col_types = c("skip", rep("numeric", 199)))  #The first column, the date, skiped

# Building the Matrices
library(Hmisc)
correlation=rcorr(as.matrix(Pre_Crise), type="pearson")              #Pearson Correlation Matrix
dist_cor=sqrt(2*(1-correlation$r))                                   #Creates the Distance Correlation Matrix, based on GOWER,1966

#Building the Dendrograms (KASSAMBARA, 2017)
library(vegan)
library(factoextra)
dist_a<-as.dist(dist_cor)                                              #Creates a compatible distance objetct
cluster1<-hclust(dist_a, method = "complete")                          #Creates a simple dendrogram named Cluster1 by complete linkage method
plot(cluster1, cex=0.5)                                                #Plots de Dendrograms using font size = 0.5
cluster2<-rect.hclust(cluster1, k=10)                                  #Cuts de Dendrogram in 10 groups and separate them using a red line
cluster2<-rect.hclust(cluster1, h=1.4)                                 #Cuts de Dendrogram in distance correlation = 1.4 and separate them using a red line
clusterx<-fviz_dend(hc, k = 4, cex = 0.5, 
          k_colors = c("#2E9FDF", "#00AFBB", "#E7B800", "#FC4E07"),
          color_labels_by_k = TRUE,                                       #Color labels by groups
          rect = TRUE,                                                    #Add rectangle around groups
          rect_border = c("#2E9FDF", "#00AFBB", "#E7B800", "#FC4E07"),
          rect_fill = TRUE)

#To change the plot theme, use the argument ggtheme, which allowed values include ggplot2
#official themes [ theme_gray(), theme_bw(), theme_minimal(), theme_classic(),
#theme_void()] or any other user-defined ggplot2 themes.

clusterx_color<-fviz_dend(cluster1, k = 25, cex = 0.5, k_colors = "jco")     #Colored links
clusterx_circle<-fviz_dend(cluster1, k = 25, cex = 0.5, 
          k_colors = "jco", rect = T, type = "circular")                     #Circular
clusterx_philo<-fviz_dend(cluster1, k=20,cex=0.55,
          k_colors = "uchicago",type = "phylogenic", 
          repel=TRUE, phylo_layout = "layout_as_tree")                       #Dendrograma Phylo

