# Вопрос 6: Результаты измерения нейротизма при помощи опросника по 60-балльной шкале Ликерта 
# (7 градаций ответов на пункт) на группе добровольцев сохранены в файле: data_file.dat


file <- "data_file6.dat"
x6.6 <- read.csv(paste0(dirname(rstudioapi::getSourceEditorContext()$path), "/", file)); rm(file)
sort(x6.6$score)

par(mfrow = c(3,1))

hist(x6.6$score, breaks = 15)

x6.6.boxlpot <- boxplot(x6.6$score,
                      horizontal = TRUE,
                      col = "cornsilk1",
                      main = "Сводные характеристики 
                      распределения нейротизма                      ",
                      xlab = "шкала Ликерта ",
                      ylab = 'измерения нейротизма'
)
print(str(x6.6.boxlpot))

# Определите, целесообразно ли с точки зрения дальнейшего анализа очистить данные от выбросов. 
# Если да, то исключите соответствующие наблюдения.


  print('Изначальные данные'); print(sort(x6.6$score)); cat("\n")
  
  x6.6.sort <- sort(x6.6$score[x6.6$score>=0 & x6.6$score<=60])
  
  hist(x6.6.sort, xlim = c(0,60))
  
  print('Очищенные данные от выбросов:'); print(x6.6.sort); cat("\n")

  
x6.6.result <-list(
  "медиана:"                   = x6.6.median <- median(x6.6.sort)   ,
  "межквартильный размах:"     = x6.6.IQR    <- IQR(x6.6.sort)                     ,
  "коэффициент асимметрии γ1:" = x6.6.skew   <- skewness(x6.6.sort)
  ) #конец 1

print(x6.6.result)


library(moments)  #загрузка пакета moments
skewness(x6.6$score, na.rm = TRUE) # коэффициент асимметрии γ1:
