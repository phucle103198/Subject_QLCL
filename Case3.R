library(tidyverse)

# Tạo dữ liệu mẫu
Du_lieu_ban_hang <- data.frame(
  Cua_hang = c("HoangHoaTham", "MeLinh"),
  HaNoi = c(1000, 885),
  TrucBach = c(1500, 125),
  HaNoiPre = c(508, 168)
)

# Sử dụng gather để chuyển đổi thành dạng tidy
tidy_Du_lieu_ban_hang <- Du_lieu_ban_hang %>%
  gather(key = "SanPham", value = "SoLuong", -Cua_hang)

# Sử dụng spread để chuyển đổi trở lại dạng ban đầu (hoặc chuyển đổi thành dạng khác)
spread_Du_lieu_ban_hang <- tidy_Du_lieu_ban_hang %>%
  spread(key = "SanPham", value = "SoLuong")



# Dữ liệu không tidy
set.seed(123)
sales_data <- data.frame(
  Store = rep(c("A", "B"), each = 5),
  Month = rep(c("Jan", "Feb", "Mar", "Apr", "May"), times = 2),
  Apple = rpois(1000, lambda = 200),
  Banana = rpois(1000, lambda = 150),
  Orange = rpois(1000, lambda = 250)
)

write.csv(sales_data, "D:/Project/SENSORY/SENSORY_BEER_HABECO_09-23/Data 3.csv", row.names=FALSE)
