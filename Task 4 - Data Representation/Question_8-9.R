# Вопрос 8-9: Замерялась длительность выполнения испытуемым каждого 
#             из серии однотипных заданий, в секундах:

duration <-c(32, 33.7, 38.5, 33, 37, 31.2, 38.4, 37.7, 41.5, 35.9, 40.6, 32.9, 33.9, 31.4, 35.8, 31.5, 38.7, 33.8, 34.2, 34.2, 23.7, 35.1, 40.9, 40.9, 33.9, 32.3, 30.7, 39.2, 33.8, 32.3, 37.8, 38.9, 32.9, 36.9, 45.4, 32.2, 34.3, 40.2, 40.4, 41.2, 38.6, 43, 37.6, 42.5, 32.1, 34.9) 

# Подберите имя для переменной (например, duration); 
# введите данные в R при помощи c() и <- и определите:

  # Объем наблюдений:
  obs_count <- length(duration)
  print(paste("Объем наблюдений:", obs_count))
  
  # Минимальное значение элемента в диапазоне вектора:
  min_value <- min(duration[20:25])
  print(paste("Минимальное значение элемента в диапазоне вектора (20:25):", min_value))
  
  # Значение элемента упорядоченного вектора:
  sorted_duration <- sort(duration)
  value_at_25th_position <- sorted_duration[25]
  print(paste("Значение элемента упорядоченного вектора на 25-й позиции:", value_at_25th_position))


  # Максимальное значение элемента в векторе:
  max_value <- max(sorted_duration)
  print(paste("Максимальное значение элемента в векторе:", max_value))
  

# Составьте таблицу частот с интервалом группирования 2, включив в разряд его левую границу.  
  
  # Таблица частот с интервалом группирования 2 (с левой границей):
  boundary <- seq(23, 47, by=2)
  duration.grouped <- cut(sorted_duration, breaks = boundary, right = FALSE)
  freq_table <- table(duration.grouped)
  print("Таблица частот с интервалом группирования 2 (с левой границей):")
  print(freq_table)
  
  # Частота 4-ого разряда:
  freq_4th_group <- freq_table[4]
  print(paste("Частота 4-ого разряда:", freq_4th_group))
  
  # Таблица накопленных частот:
  cumulative_freq <- cumsum(freq_table)
  print("Таблица накопленных частот:")
  print(cumulative_freq)
  

#Построение гистограммы
  
# Абсолютная частота   
duration.hst.abs<-hist(duration, 
                       breaks = boundary, 
                       right = FALSE, #какую из границ включать в интервал группирования
                       main="Распределение длительности выполнения задачи", 
                       xlab = "длительность, с", 
                       ylab = "абсолютная частота")

# Плотность вероятности 
duration.hst.den<-hist(duration, 
                       breaks = boundary, 
                       right = FALSE,
                       freq = FALSE, # плотность вероятности — это производная кумулятивной функции распределения, т.е. мгновенная скорость роста накопленной вероятности.
                       main="Распределение длительности выполнения задачи", 
                       xlab = "длительность, с",
                       ylab = "плотность вероятности",
                       )

# Относительная частота 
duration.hst.rel<-hist(duration, 
                       breaks = boundary, 
                       right = FALSE,
                       plot = FALSE
                       )

duration.hst.rel$density <- duration.hst.rel$density * 2  # замена плотностей относительными  частотами
                 
plot(duration.hst.rel, 
     freq = FALSE,
     main="Распределение длительности выполнения задачи", 
     xlab = "длительность, с",
     ylab = "относительная частота",)

