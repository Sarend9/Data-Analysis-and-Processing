#Вопрос 7: Записи дистанции (в км), ежедневно проезжаемой велосипедистом, 
#          содержатся в файле данных: data_file.dat
file <- "new_data.dat"
imported.data <- read.csv(paste0(dirname(rstudioapi::getSourceEditorContext()$path), "/", file)); rm(file)

# Виды шкал:
# 1) Процентили (шкала по 1%)
     quantile(imported.data$distance, probs=seq(0, 1, by=0.01))
     
# 2) Децили     (шкала по 10%)
     quantile(imported.data$distance, probs=seq(0, 1, by=0.1))

# 3) Квартили   (шкала поделена на четверти)
     quantile(imported.data$distance, probs=seq(0, 1, by=1/4))

# Сводная статистика
  summary(imported.data$distance)

# Меры центральной тенденции:

  # Среднее арифметическое μ :
  mean(imported.data$distance)

# Меры положения:
  
  # Минимальное minimum Xmin :
  min(imported.data$distance)
  
  # Максимальное maximum Xmax:
  max(imported.data$distance)

# Квантили:
  
  # 1-й, или нижний квартиль lower quartile Q1:
  quantile(imported.data$distance, 0.25)
  
  # 3-й, или верхний квартиль upper quartile Q3:
  quantile(imported.data$distance, 0.75)
  
  # 3-й дециль 3th decile D3:
  quantile(imported.data$distance, probs=seq(0, 1, by=0.1))[3+1]
  
  # 5-й процентиль 5th percentile P5:
  quantile(imported.data$distance, probs=seq(0, 1, by=0.01))[5+1]
  
  # 38-й процентиль P38:
  quantile(imported.data$distance, probs=seq(0, 1, by=0.01))[38+1]
  
  
  
  
