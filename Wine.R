library(caret)
library(factoextra)
library(GGally)
library(class)
library(rpart)
library(rpart.plot)
library(randomForest)
library(e1071)
library(dplyr)
library(FactoMineR)


## Load data
#write.csv(wine,file = "C://Users//PhucLeTuan//Downloads//wine.csv" )
wine <- read.csv(file = "C://Users//PhucLeTuan//Downloads//wine.csv",check.names=FALSE)
wine <- wine[,-1]

#names(wine) <- c("Type","Alcohol","Malic","Ash","Alcalinity","Magnesium","Phenols","Flavanoids","Nonflavanoids","Proanthocyanins","Color","Hue","Dilution","Proline")
wine$Type <- as.factor(wine$Type)


## Tính trung bình xác định thuộc tính cảm quan đặc trưng của các loại rượu
wine_mean <- aggregate(wine[,-1], by = list(wine$Type), FUN = mean)
row_name_sensory <- wine_mean$Group.1
wine_mean <- wine_mean[,-1]
row.names(wine_mean) <- row_name_sensory
res.pca2 <- prcomp(wine_mean, scale = TRUE)

fviz_pca_biplot(res.pca2, repel = TRUE,
                col.var = "#2E9FDF", # Variables color
                col.ind = "#696969"  # Individuals color
)


#### Toàn bộ dữ liệu
res.pca <- prcomp(wine[,-1], scale = TRUE)
fviz_eig(res.pca, addlabels = TRUE, ylim = c(0, 100))

fviz_pca_ind(res.pca, 
             col.ind = wine$Type,
             addEllipses = T,
             ellipse.type = "confidence",
             ellipse.level = 0.95,
             repel = TRUE )

fviz_pca_var(res.pca,
             col.var = "contrib",
             repel = TRUE     
)

# Create a scatter plot
colnames(wine)
ggplot(data = wine, aes(x = `Malic`, y = `Alcalinity`, color = `Type`)) +
  geom_point() +
  labs(x = "X-Axis Label", y = "Y-Axis Label", title = "Scatter Plot with Color by Type") +
  theme_minimal()


## Sử dụng cây quyết định cho 3 PC đầu tiên
scores<-res.pca$x[,c(1:3)]
final_set<-cbind(wine$Type,as.data.frame(scores))
colnames(final_set)[1]<-c("labels")


tree_grown <- rpart(labels~PC1+PC2+PC3,
                    data = final_set, method = "class",
                    cp=-1,minsplit = 2, minbucket = 1)
rpart.plot(tree_grown,type=2,extra=4, tweak=1.7, under=TRUE)

