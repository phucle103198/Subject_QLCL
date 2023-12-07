library(knitr)
library(readr)
library(dplyr)
library(tidyr)
library(FactoMineR)
library(factoextra)
library(SensoMineR)
library(ggplot2)
library(fmsb)
library(car)
library(readxl)
library(tidyverse)
library(psych)


write.csv(df, "D:/Project/SENSORY/SENSORY_BEER_HABECO_09-23/Data_tree.csv", row.names=FALSE)


## Đọc dữ liệu từ file excel
df <- read_excel("D:/Project/SENSORY/SENSORY_BEER_HABECO_09-23/HuongDanSuDungR/Data_1.xlsx")

colnames(df)
attach(df)
describe.by(`Chiều dài`, `Loại`)


## Tách dữ liệu ###
gachien_1 <- df[(1:19),(4:6)]

## Cách 1
gachien_2 <- df[(1:19),]
## Cách 2
gachien_2 <- df[df$Loại == "Gà chiên",]



## Mô tả dữ liệu bằng hàm describe ###
attach(gachien_2)
describe(gachien_2)

attach(df)
## Vẽ biểu đồ hộp khẩu phần ăn
ggplot(data= df, mapping =aes(x=`Loại`, y = `Kích thước`, fill = `Loại`)) + geom_boxplot() + labs(title = "Biểu đồ so sánh kích thước khẩu phần ăn", x = "Món ăn", y = "Khẩu phần ăn (gram)")

## Vẽ biểu đồ hộp chiều rộng
p1 <- df %>%
  ggplot(aes(`Loại`, `Chiều rộng`, fill = `Loại`)) + geom_boxplot() + labs(title = "Biểu đồ so sánh chiều rộng món ăn", x = "Loài thực vật", y = "Chiều rộng (cm)")
p1


## Tính trung bình chiều rộng
chieu_rong_tb_df <-df %>%
  group_by(`Loại`) %>%
  summarise(chieu_rong_trung_binh = mean(`Chiều rộng`)) %>%
  as.data.frame


## Biểu diễn giá trị trung bình của chiều rộng theo loài lên biểu đồ p1 đã vẽ

p2 <- p1 + geom_point(aes(x = `Loại`, y = chieu_rong_trung_binh), data = chieu_rong_tb_df, col = "red")
p2
p2 + geom_text(aes(label = round(chieu_rong_trung_binh,1), x = `Loại`, y = round(chieu_rong_trung_binh,1)), data = chieu_rong_tb_df, col="black", check_overlap = TRUE, vjust = 1.5, size = 5)


## Tính giá trị nhỏ nhất của chiều rộng phiến lá theo loài
chieu_rong_min_df <- df %>%
  group_by(`Loại`) %>%
  summarise(chieu_rong_min = min(`Chiều rộng`)) %>%
  as.data.frame

## Biểu diễn giá trị nhỏ nhất của chiều rộng theo loài lên biểu đồ p1
p3 <- p1 + geom_point(aes(x = `Loại`, y = round(chieu_rong_min,1)), data = chieu_rong_min_df, col = "blue")
p3
## Thể hiện chữ số của giá trị nhỏ nhất lên biểu đồ
p3 + geom_text(aes(label = round(chieu_rong_min,1), x = `Loại`, y = round(chieu_rong_min,1)), data = chieu_rong_min_df, col="blue", check_overlap = TRUE, vjust = -0.5)




