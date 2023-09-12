# Вопрос 3-4: Один из играющих в боулинг записывал в течение вечера, сколько кеглей он сбивает с первого броска. По этим данным была составлена таблица частот:
bowls.n.freq <- c(1, 3, 8, 13, 11, 12, 2) # вектор частот
bowls.n.freq <- as.table(bowls.n.freq)    # перевод в таблицу частот
bowls.n <- 3:9                            # вектор значений сбитых кеглей
names(bowls.n.freq) <- bowls.n            # присоединяем значения к их частотам

# Исходные данные из примера
#bowls.n.freq <- c(2, 3, 5, 4, 3, 2, 2)    
#bowls.n.freq <- as.table(bowls.n.freq)    
#bowls.n <- 0:6                            
#names(bowls.n.freq) <- bowls.n            

# Исходные данные (еще одни)
# bowls.n.freq <- c(3, 8, 13, 12, 10, 4)    
# bowls.n.freq <- as.table(bowls.n.freq)   
# bowls.n <- 0:5                            
# names(bowls.n.freq) <- bowls.n

# Исходные данные (еще одни)
# bowls.n.freq <- c(3, 5, 8, 6, 3) 
# bowls.n.freq <- as.table(bowls.n.freq)      
# bowls.n <- 2:6                              
# names(bowls.n.freq) <- bowls.n    
 
#  Исходные данные (еще одни)
#  bowls.n.freq <- c(60.8, 63.5, 66.25, 68.98, 71.7, 74.4, 77.15, 79.88) 
#  bowls.n.freq <- as.table(bowls.n.freq)      
#  bowls.n <- c(6, 13, 22, 37, 18, 12, 27, 15)                              
#  names(bowls.n.freq) <- bowls.n


# Библиотека функций для поиска медианы:
# Удаляю имя папки и получаю путь в общий каталог
  catalog <- gsub( basename(dirname(rstudioapi::getSourceEditorContext()$path)), '', paste0(dirname(rstudioapi::getSourceEditorContext()$path))) 
  file <- "Функции для R/R_function.R"
# Открываю библиотеку с пользовательскими функциями
  source(paste0(catalog, file));
  
  
  rm(file)


bowls.result <-list(
# Определите:           
    "тип шкалы:"                      = "отношений"       ,
    "тип переменной:"                 = "дискретная"      ,
    "распределение частот:"           = "биноминальное"   ,
    "форма распределения:"            = "унимодальное"    ,                                                     
   
     
# Найдите для числа сбитых кеглей:
    "среднее арифметическое:"         = mean.bowls   <- mean.freq(bowls.n.freq),
    
# Медиана (по таблице частот):                      
      "Медиана:"                      = median.bowls <- median.freq(bowls.n.freq)                                                       
)

print(bowls.result)
print(bowls.n.freq)


#DF <- data.frame(as.table(cumsum(bowls.n.freq)))
#names(DF) <- c("сбитые кегли", "частота")
#print(DF)



#График: 
plot(bowls.n.freq,
     main = "Распределение результатов",
     ylim = c(0, max(bowls.n.freq)+1),   # диапазон оси y
     xlab = "результат, cбитые кегли",
     ylab = "n",
     lwd  = 10,                # толщина линий увеличена в 12 раз
     col  = "lightblue")  # голубой цвет столбиков

# Нанесение среднего арифметического (μ) на график:
abline(v=mean.bowls, col = 'red')
text(x = mean.bowls, y = max(bowls.n.freq)-1, 'среднее', col = 'red')

# Нанесение медианы (второй квантиль Q2) на график:
abline(v=median.bowls, col = 'blue')
text(x = median.bowls, y = max(bowls.n.freq)-3, 'медиана', col = 'blue')

# colours()
