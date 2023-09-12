# Вопрос 7: Записи дистанции (в км), ежедневно проезжаемой велосипедистом, 
#          содержатся в файле данных: data_file.dat
data_file <- "/new_data.dat"
imported_data <- read.csv(paste0(dirname(rstudioapi::getSourceEditorContext()$path), "/", data_file)); rm(data_file)

# Виды шкал:
# 1) Процентили (шкала по 1%)
percentiles_1 <- quantile(imported_data$distance, probs = seq(0, 1, by = 0.01))

# 2) Децили (шкала по 10%)
deciles <- quantile(imported_data$distance, probs = seq(0, 1, by = 0.1))

# 3) Квартили (шкала поделена на четверти)
quartiles <- quantile(imported_data$distance, probs = seq(0, 1, by = 1/4))

# Сводная статистика
summary_data <- summary(imported_data$distance)

# Меры центральной тенденции:

# Среднее арифметическое μ :
mean_distance <- mean(imported_data$distance)

# Меры положения:

# Минимальное minimum Xmin :
min_distance <- min(imported_data$distance)

# Максимальное maximum Xmax:
max_distance <- max(imported_data$distance)

# Квантили:

# 1-й, или нижний квартиль lower quartile Q1:
q1 <- quantile(imported_data$distance, 0.25)

# 3-й, или верхний квартиль upper quartile Q3:
q3 <- quantile(imported_data$distance, 0.75)

# 3-й дециль 3th decile D3:
d3 <- quantile(imported_data$distance, probs = seq(0, 1, by = 0.1))[3 + 1]

# 5-й процентиль 5th percentile P5:
p5 <- quantile(imported_data$distance, probs = seq(0, 1, by = 0.01))[5 + 1]

# 38-й процентиль P38:
p38 <- quantile(imported_data$distance, probs = seq(0, 1, by = 0.01))[38 + 1]

# Вывод результатов в консоль
cat("Процентили (шкала по 1%):\n")
print(percentiles_1)

cat("\nДецили (шкала по 10%):\n")
print(deciles)

cat("\nКвартили (шкала поделена на четверти):\n")
print(quartiles)

cat("\nСводная статистика:\n")
print(summary_data)

cat("\nСреднее арифметическое μ:\n")
print(mean_distance)

cat("\nМинимальное minimum Xmin:\n")
print(min_distance)

cat("\nМаксимальное maximum Xmax:\n")
print(max_distance)

cat("\n1-й, или нижний квартиль lower quartile Q1:\n")
print(q1)

cat("\n3-й, или верхний квартиль upper quartile Q3:\n")
print(q3)

cat("\n3-й дециль 3th decile D3:\n")
print(d3)

cat("\n5-й процентиль 5th percentile P5:\n")
print(p5)

cat("\n38-й процентиль P38:\n")
print(p38)
