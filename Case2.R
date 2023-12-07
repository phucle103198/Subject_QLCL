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

### Đọc dữ liệu
df2 <- read_excel("D:/Project/SENSORY/SENSORY_BEER_HABECO_09-23/HuongDanSuDungR/Data_2.xlsx", sheet = "choco1")
### Chuyển về định dạng tiếng anh
df2 <- df2 %>% janitor::clean_names()




df2 <- as.data.frame(df2)
colnames(df2)
## Vẽ biểu đồ điểm
ggplot(data = df2, mapping = aes(x = ca_cao, y = xep_hang))+
  geom_point()

ggplot(data = df2, mapping = aes(x = ca_cao, , fill = san_pham))+
  geom_histogram()

#Histogram
ggplot(df2, aes(x = san_pham, y = xep_hang)) +
  geom_col(width = 1, fill = "darkred") +       # plot the count data as columns
  theme_minimal()+                              # simplify the background panels
  labs(                                         # add plot labels, title, etc.
    x = "intensity",
    y = "liking",
    title = "...") +
  facet_wrap(~nguoi_thu)

#Đường mật độ phân bố
ggplot(data = df2, mapping = aes(x = sua)) +
  geom_density(size = 2, alpha = 0.2)+
  labs(title = "Biểu đồ mật độ phân phối thuộc tính sữa trong toàn bộ dữ liệu")


#đường mật đô phân bố theo biến khác
ggplot(data = df2, mapping = aes(x = sua, fill = san_pham)) +
  geom_density(size = 2, alpha = 0.2, position = "stack")+
  labs(title = "Biểu đồ mật độ phân phối thuộc tính sữa trong từng loại sản phẩm")

# "Stacked" histogram
ggplot(data = df2, mapping = aes(x = sua, fill = san_pham)) +
  geom_histogram(binwidth = 2)+
  labs(title = "'Stacked' histogram")

# Frequency
ggplot(data = df2, mapping = aes(x = caramen, color = san_pham)) +
  geom_freqpoly(binwidth = 2, size = 2)+
  labs(title = "Freqpoly")

# Frequency with proportion axis
ggplot(data = df2, mapping = aes(x = caramen, y = after_stat(density), color = san_pham)) +
  geom_freqpoly(binwidth = 5, size = 2)+
  labs(title = "Proportional freqpoly")

# Frequency with proportion axis, smoothed
ggplot(data = sensochoc, mapping = aes(x = milk_a, y = after_stat(density), fill = product)) +
  geom_density(size = 2, alpha = 0.2)+
  labs(title = "Proportional, smoothed with geom_density()")




# box plot
# A) Overall boxplot
ggplot(data = sensochoc)+
  geom_boxplot(mapping = aes(y = milk_a))+   # only y axis mapped (not x)
  labs(title = "A) Overall boxplot")

# B) Box plot by group
ggplot(data = sensochoc, mapping = aes(y = milk_a, x = product, fill = product)) +
  geom_boxplot()+
  theme(legend.position = "none")+   # remove legend (redundant)
  labs(title = "B) Boxplot by gender")


# A) Jitter plot by group
ggplot(data =sensochoc %>% drop_na(product),      # remove missing values
       mapping = aes(y = rank,                     # Continuous variable
                     x = product,                           # Grouping variable
                     color = product))+                     # Color variable
  geom_jitter()+                                  # Create the violin plot
  labs(title = "A) jitter plot by gender")



# B) Violin plot by group
ggplot(data = sensochoc %>% drop_na(product),       # remove missing values
       mapping = aes(y = rank,                      # Continuous variable
                     x = product,                            # Grouping variable
                     fill = product))+                       # fill variable (color)
  geom_violin()+                                   # create the violin plot
  labs(title = "B) violin plot by gender")


#geom_bar
library(forcats)
# A)
ggplot(sensochoc %>% drop_na(rank)) +
  geom_bar(aes(y = fct_rev(product)), width = 0.7) +
  theme_minimal()+
  labs(title = "...",
       y = ".")


# B)
ggplot(sensochoc %>% drop_na(rank)) +
  geom_bar(aes(y = fct_rev(product), fill = rank), width = 0.7) +
  theme_minimal()+
  theme(legend.position = "bottom") +
  labs(title = "B)",
       y = "H")


#
ggplot(sensochoc) +
  geom_col(aes(x=product, y = milk_a)) +
  labs(subtitle = "N")
class(sensochoc$rank)
sensochoc[,3] <- as.numeric(sensochoc[,3])




