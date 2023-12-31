# Вопрос 5-6: В ходе измерения уровня интеллекта получены данные
#             о выполнении мыслительных задач,
#             в сырых баллах: data_file.dat

data_file_name <- "data_file5-6.dat"
x5.7 <- read.csv(paste0(dirname(rstudioapi::getSourceEditorContext()$path), "/", data_file_name))
rm(data_file_name)

# Загрузка пользовательских функций
directory_path <- getwd()
functions_file <- "/resources/R_function.R"
source(paste0(directory_path, functions_file))
rm(functions_file)


# ВВЕДИТЕ СЫРЫЕ БАЛЛЫ:
raw.scores <- c(40, 68, 69, 76, 95)

# Выполните квантильную [нелинейную] нормализацию в стандартную Z-шкалу, 
# затем переведите нормализованные значения в стандартные баллы шкалы интеллекта Векслера:
# Перевод z-значений в шкалы интеллекта производится обычным линейным преобразованием: 
# каждое  z-значение умножают на стандартное отклонение новой шкалы и складывают с её средним.

# Функция для квантильной нормализации и перевода в IQ-шкалы:
x5.7.DF <- z.IQ.scale(x5.7$trait, raw.scores); x5.7.DF; View(x5.7.DF)


# Вопрос 6:
# Постройте по исходным данным из предыдущего вопроса диаграмму распределения. 
# Наложите на неё кривую плотности нормального распределения.

# Постройте отдельную диаграмму [теоретической] плотности нормального распределения шкалы Векслера.
# Нанесите на диаграммы найденные в предыдущем вопросе соответственно сырые и стандартные баллы в виде цветных вертикальных линий с метками букв (a,b..).

# Функция квантильной нормализации в стандартную z-шкалу
z5.7.scores     <- z.quan.normaliz(x5.7$trait)
z5.7.Wexler     <- (z5.7.scores * 15) + 100
z5.7.Stanf.Bin  <- (z5.7.scores * 16) + 100


par(mfrow = c(2, 1))


abline.text <- function(x, y, text, col = 'firebrick1', lwd = 1){
  abline(v = x, col = col, lwd = lwd)
  text  (x = x,  y = y, text, col = col)
}



# гистограмма исходных данных 
hist(x5.7$trait,               # сырые данные 
     freq = FALSE,             # плотность вместо абсолютных частот 
     breaks = 15,              # число разрядов 
     #  оформление
     ylim = c(0, 0.025),
     col = "light blue", 
     main = "Диаграмма распределения исходных данных", 
     xlab = "результат, сырые баллы", 
     ylab =  "доля" 
) 
x5.7.var <- sum((x5.7$trait - mean(x5.7$trait)) ^ 2)/length(x5.7$trait) 
x5.7.sd  <- sqrt(x5.7.var)
# наложение кривой нормального распределения 
curve(dnorm(x, 
            mean = mean(x5.7$trait),  # среднее распределения как у данных 
            sd = x5.7.sd),            # стандартное отклонение как у данных 
      add = TRUE,                     # добавление к текущей диаграмме 
      col = "red")

abline.text(x = x5.7.DF$`сырой балл`, y = 0.015, text = x5.7.DF$балл)

# Диаграмма плотности нормального распределения шкалы Векслера
curve(dnorm(x, 
            mean = 100, 
            sd = 15),
      xlim = c(40,160),
      main = "Диаграмма плотности 
      нормального распределения шкалы Векслера     ",
      xlab = "результат, стандартный балл", 
      ylab =  "доля" 
)                    
abline.text(x = x5.7.DF$`стандартный балл (Векслер)`, y = 0.015, text = x5.7.DF$балл)

par(mfrow = c(1, 1))

dev.copy(png, width = 1001, height = 1008, paste0(dirname(rstudioapi::getSourceEditorContext()$path), "/Task_7.6.png"))
dev.off()

