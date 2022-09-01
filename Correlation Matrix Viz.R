#This was a script I created for an Atlanta paratransit research project,
#where the goal was to look for patterns in paratransit trip Origins and Destinations.
#The findings contributed to policy decision making for 5 Atlanta counties' paratransit systems.
#This script consists of different ways to visualize the Origin Destination data
#using heatmaps and correlation matrices.

MATRIX_TEST<-read.csv(file="C:/xxxx/xxxx/Paratransit Study/MARTA Files/MATRIX_MARTA_by_Jurisdiction.csv", header=TRUE, sep=",")
MATRIX_TEST
names(MATRIX_TEST)
View(MATRIX_TEST)

#compute correlation matrix

ncol(MATRIX_TEST)

#50 COLUMNS

mydata <- MATRIX_TEST[, c(1:50)]
head(mydata)

#find which column is not numeric

#all are integers, must be converted to numeric

sapply(mydata, is.factor)

#creates correlation matrix
sapply(mydata, class)


cor(mydata[sapply(mydata, function(x) !is.factor(x))])


#visualize correlation matrix
install.packages("ggplot2")
library(ggplot2)

# Get lower triangle of the correlation matrix

get_lower_tri<-function(jurisdiction){
  jurisdiction[upper.tri(jurisdiction)] <- NA
  return(jurisdiction)
  }

# Get upper triangle of the correlation matrix

get_upper_tri <- function(jurisdiction){
  jurisdiction[lower.tri(jurisdiction)]<- NA
  return(jurisdiction)
  }
upper_tri <- get_upper_tri(jurisdiction)
upper_tri

get_lower_tri <- function(jurisdiction){
  jurisdiction[upper.tri(jurisdiction)]<- NA
  return(jurisdiction)
}
lower_tri <- get_lower_tri(jurisdiction)
lower_tri

# Melt the correlation matrix

library(reshape2)
melted_jurisdiction_2 <- melt(upper_tri, na.rm = TRUE)
melted_jurisdiction_2

# Heatmap
library(ggplot2)
ggplot(data = melted_jurisdiction_2, aes(Var2, Var1, fill = value))+
  geom_tile(color = "white")+
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                       midpoint = 0, limit = c(-1,1), space = "Lab", 
                       name="Pearson\nCorrelation") +
  theme_minimal()+ 
  theme(axis.text.x = element_text(angle = 45, vjust = 1, 
                                   size = 12, hjust = 1))+
  coord_fixed()

#reorder corr matrix
reorder_jurisdiction <- function(jurisdiction){
  # Use correlation between variables as distance
  dd <- as.dist((1-jurisdiction)/2)
  hc <- hclust(dd)
  jurisdiction <-jurisdiction[hc$order, hc$order]
}

#visualization
# Reorder the correlation matrix
jurisdiction <- reorder_jurisdiction(jurisdiction)
upper_tri <- get_upper_tri(jurisdiction)

# Melt the correlation matrix
melted_jurisdiction_2 <- melt(upper_tri, na.rm = TRUE)
# Create a ggheatmap

ggheatmap <- ggplot(melted_jurisdiction_2, aes(Var2, Var1, fill = value))+
  geom_tile(color = "white")+
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                       midpoint = 0, limit = c(-1,1), space = "Lab", 
                       name="Pearson\nCorrelation") +
  theme_minimal()+ # minimal theme
  theme(axis.text.x = element_text(angle = 45, vjust = 1, 
                                   size = 6, hjust = 1))+
  coord_fixed()
# Print the heatmap
print(ggheatmap)
#............................................................................
#other method of plotting
my_data <- read.csv(file.choose())
# Load data
attach(names(my_data))
names(my_data)
View(my_data)
my_data <- my_data[, c(all())]
# print the first 6 rows
head(my_data, 6)
res <- cor(my_data)
round(res, 2)
as.matrix(sapply(my_data, as.numeric)) 
my_data<-matrix(as.numeric(unlist(my_data)),nrow=nrow(my_data))
cor(my_data, use = "complete.obs")

#Correlation matrix with significance levels (p-value)
install.packages("Hmisc")
library("Hmisc")
res2 <- rcorr(as.matrix(my_data))
res2
# ++++++++++++++++++++++++++++
# flattenCorrMatrix
# ++++++++++++++++++++++++++++
# cormat : matrix of the correlation coefficients
# pmat : matrix of the correlation p-values
flattenCorrMatrix <- function(cormat, pmat) {
  ut <- upper.tri(cormat)
  data.frame(
    row = rownames(cormat)[row(cormat)[ut]],
    column = rownames(cormat)[col(cormat)[ut]],
    cor  =(cormat)[ut],
    p = pmat[ut]
  )
}
library(Hmisc)
res2<-rcorr(my_data)

install.packages("corrplot")
library(corrplot)
corrplot(res2, type = "upper", order = "hclust", tl.col = "black", tl.srt = 45)




