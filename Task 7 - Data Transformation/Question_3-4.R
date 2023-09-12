# Вопрос 3-4: Замеры шума в помещении производились в течение дня по шкале интенсивности звука, в Вт/м²;
# результаты сохранены в файле: data_file.dat
data_file_name <- "data_file3-4.dat"
sound_data <- read.csv(paste0(dirname(rstudioapi::getSourceEditorContext()$path), "/", data_file_name))
rm(data_file_name)

# Преобразуйте данные из шкалы "интенсивности звука" в шкалу "уровня звука", в дБ.
intensity <- sound_data$sound           # - раздражитель
intensity_threshold <- 10**(-12)        # - граничное значение интенсивности раздражителя
sound_level_db <- 10*log10(intensity/intensity_threshold) # - Закон Вебера-Фехнера

# Преобразуйте те же исходные данные в шкалу "уровня громкости", в сонах.
# Закон Стивенса
loudness_sones <- (intensity/intensity_threshold)**0.3    # - Закон Стивенса

# Переведите их же в шкалу рангов.
sound_level_db_rank <- rank(sound_level_db)
loudness_sones_rank <- rank(loudness_sones)
# !ОНИ ОДИНАКОВЫ!

# Найдите значения элементов исходного и преобразованных векторов с индексами:

# ВВЕДИТЕ ИНДЕКСЫ!
indices <- c(160, 250)

data_frame_function <- function(index){
  data_frame <- data.frame(
    "индекс"      =  index,
    "I,  [Вт/м²]" =  intensity[index],
    "Lp, [дБ]"    =  sound_level_db[index],
    "Ls, [соны]"  =  loudness_sones[index],
    "ранг"        =  sound_level_db_rank[index] # или loudness_sones_rank[index]
  )
  names(data_frame) <- c("индекс", "I,[Вт/м²]", "Lp,[дБ]", "Ls,[соны]",  "ранг")
  return(data_frame)
}

result_data_frame <- data_frame_function(indices); result_data_frame;
View(result_data_frame)



# Вопрос 4: Постройте по диаграмме для распределения исходных данных
#           из предыдущего вопроса про ощущения интенсивности звука 
#           и для каждого варианта преобразованных данных. 

#           Наложите на обе диаграммы преобразованных данных кривые 
#           плотности нормального распределения. 

#           Оформите все три диаграммы как следует и разместите их
#           на одной странице в один ряд (в три колонки). 

#           Сохраните в .png и загрузите сюда.



par(mfrow = c(1, 3))  # 6 диаграмм на стр., в 2 ряда по 3 колонки

# гистограмма исходных данных (интенсивности звука)
hist(intensity,
     freq = FALSE,            # плотность вместо абсолютных частот
     breaks = 15,             # число разрядов
     col = "light green",
     main = "Распределение интенсивности звука",
     ylim = c(0, 7000000),
     xlab = "Интенсивность звука I, [Вт/м²]",
     ylab =  "доля"
)
curve(dnorm(x,
            mean = mean(sound_data$sound),  # среднее распределения как у данных
            sd = sqrt(sum((sound_data$sound - mean(sound_data$sound)) ^ 2)/length(sound_data$sound))),  # стандартное отклонение как у данных
      add = TRUE,  # добавление к текущей диаграмме
      col = "red")

# гистограмма уровня громкости
hist(sound_level_db,
     freq = FALSE,
     breaks = 15,
     #  оформление
     col = "papayawhip",
     main = "Распределение уровня звука",
     ylim = c(0, 0.08),
     xlim = c(0, 60),
     xlab = "Уровнь звука Lp, [дБ]",
     ylab = "доля"
)
curve(dnorm(x,
            mean = mean(sound_level_db),  # среднее распределения как у данных
            sd = sqrt(sum((sound_level_db - mean(sound_level_db)) ^ 2)/length(sound_level_db))),  # стандартное отклонение как у данных
      add = TRUE,  # добавление к текущей диаграмме
      col = "red")

# гистограмма уровня громкости
# -----------------------------
hist(loudness_sones,
     freq = F,
     col = "light blue",
     main = "Распределение уровня громкости",
     ylim = c(0, 0.05),
     xlim = c(0, 60),
     xlab = "Громкость звука Ls, [соны]",
     ylab = "доля"
)
curve(dnorm(x,
            mean = mean(loudness_sones),
            sd = sqrt(sum((loudness_sones - mean(loudness_sones)) ^ 2)/length(loudness_sones))),
      add = TRUE,
      col = "red")

dev.copy(png, width = 1920, height = 704,paste0(dirname(rstudioapi::getSourceEditorContext()$path), "/Task_7.4.png"))
dev.off()
par(mfrow = c(1, 1))