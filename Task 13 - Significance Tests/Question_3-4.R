# Вопрос 3: Пользователи выбирали между вариантами интерфейса
#           ПО: светлым (1), тёмным (2), контрастным (3) и
#           нейтральным (4). Результаты: skin.dat.

# 1) Дано:
     x3 <- "skin.dat"; x3   <- read.csv(paste0(dirname(rstudioapi::getSourceEditorContext()$path), "/", x3))
     str(x3)  #  вывод сведений о структуре объекта

     P_true <- 1/4
     
#    1.1) Вычислим X2 и p-значение:
#         ¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯              
     freq <- table(x3) # Аргумент freq функции chisq.test() — частоты эмпирического распределения, 
                       # которые нам были даны изначально, а в общем случае можно получить с помощью table(). 
                       # Если не задавать других аргументов, то по умолчанию chisq.test() проверяет, 
                       # насколько этим частотам соответствует равномерное теоретическое распределение. 
     

     chisq.test(freq)  # критерий согласия хи-квадрат Пирсона
     X2      <- chisq.test(freq)[[1]]; X2
     df      <- chisq.test(freq)[[2]]; df
     p_value <- chisq.test(freq)[[3]]; p_value
     
     cat("\n   • X2:", X2,      "\n")
     cat(  "   • df:", df,      "\n")
     cat(  "   • p: ", p_value, "\n")
     
#    1.2) Cудим по p-значению о том, насколько адекватна нулевая гипотеза и значим полученный результат.     
     
     cat("\nДоли составили:\n")
     print(table(x3) / sum(table(x3)) * 100)
     
       if    (p_value < 0.05) 
       {cat("\nНулевая гипотеза H0 - \033[1mотвергается\033[0m\n")
         cat("Результат оказался статистически \033[1mзначимым\033[0m\n")
         cat("Таким образом, гипотеза о том, что ... \033[1mподтвертидась\033[0m\n")
       } else 
       {cat("\nНулевая гипотеза H0 - \033[1mне отвергается\033[0m\n")
         cat("Результат оказался статистически \033[1mне значимым\033[0m\n")
         cat("Таким образом, гипотеза о том, что ... \033[1mне подтвертидась\033[0m\n")}
     
     
     
     
# Вопрос 4:
     
     library(ggplot2)
     
     p.curve <- function(x) {
       y <- dchisq(x, df = df)  
       y[x < X2 ] <- NA  
       return(y)
     }
     
     
     plot.x3 <- ggplot() +
       xlim(0, 15) +
       stat_function(fun = dchisq,
                     args = list(df = df),
                     geom = 'area', fill = 'light gray', colour = "black") +
       stat_function(fun = p.curve,
                     geom = 'area', fill = 'steel blue', colour = "navy") +
       theme_bw() +
       theme(panel.grid = element_blank(), plot.title = element_text(face = 'bold', hjust = 0.5))+  # отключение сетки
       labs(title = "Распределение вероятностей распределения χ2 согласно нулевой гипотезе",
                y = 'плотность вероятности', x = 'χ2')
     
     plot(plot.x3)
     
     
     # Для проверки гипотезы о том, что есть варианты, которые выбираются чаще других использовался критерий согласия χ2 Пирсона. Результат оказался статистически 
     # не значимым χ2(3)= 1.072; p= 0.78)
     # Таким образом, гипотеза не подвердилась.
     
     
     