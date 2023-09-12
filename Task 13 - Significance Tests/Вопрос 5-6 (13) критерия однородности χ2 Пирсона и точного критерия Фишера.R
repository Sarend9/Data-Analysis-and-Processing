# Вопрос 5: У двух случайно набранных групп, мужской и женской,
#           собраны данные об обуви, которая надета на участников:
#           кеды (1), кроссовки (2), ботинки (3), туфли(4). Данные
#           записаны в два файла: female.dat (женщины) и male.dat (мужчины).

# 1) Дано:

     file <- "male.dat"
     x5_1 <- read.csv(paste0(dirname(rstudioapi::getSourceEditorContext()$path), "/", file)); rm(file)
     file <- "female.dat"
     x5_2 <- read.csv(paste0(dirname(rstudioapi::getSourceEditorContext()$path), "/", file)); rm(file)
     
     str(c(x5_1, x5_2))  #  вывод сведений о структуре объекта
     
     P_true <- 1/4
     
     
# 2) Для использования функции chisq.test() преобразуем переменные:
#    ¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯             
     x5_1$sex <- "male"
     x5_2$sex <- "female"
     names(x5_1)<- c("shoes", "sex")
     names(x5_2)<- c("shoes", "sex")
     x5 <- rbind(x5_1, x5_2)
     
     
     
     x5.table <- table(x5)
     print(chisq.test(x5.table))  # критерий согласия хи-квадрат Пирсона
     p_value <- chisq.test(x5.table)[3]; p_value
     fisher.test(x5.table) # точный критерий Фишера
     
     
     cat("\n   • Критерий однородности χ2 Пирсона:", as.numeric(chisq.test(x5.table)[[1]]),      "\n")
     cat(  "   • df (χ2 Пирсона):  ", as.numeric(chisq.test(x5.table)[[2]]),      "\n")
     cat(  "   • p  (χ2 Пирсона):  ", as.numeric(chisq.test(x5.table)[[3]]), "\n")
     cat(  "   • p  (Фишера):      ",  fisher.test(x5.table)[[1]], "\n")
     
     if    (p_value < 0.05) 
     {cat("\nНулевая гипотеза H0 - \033[1mотвергается\033[0m\n")
       cat("Результат оказался статистически \033[1mзначимым\033[0m\n")
       cat("Таким образом, гипотеза о том, что ... \033[1mподтвертидась\033[0m\n")
     } else 
     {cat("\nНулевая гипотеза H0 - \033[1mне отвергается\033[0m\n")
       cat("Результат оказался статистически \033[1mне значимым\033[0m\n")
       cat("Таким образом, гипотеза о том, что ... \033[1mне подтвертидась\033[0m\n")}
     
     cat("\nОтносительные частоты\n")
     cat("\nгруппа:", names(table(x5[[2]])[1]),"\n")
     for (i in 1:nrow(table(x5[[1]]))){
     (cat("группа:",i,"частота",(length(x5[[1]][x5[[1]]==i & x5[[2]]==names(table(x5[[2]])[1])])/nrow(x5)*100), "\n"))}
    
     cat("\nгруппа:", names(table(x5[[2]])[2]),"\n")
     for (i in 1:nrow(table(x5[[1]]))){
     (cat("группа:",i,"частота",(length(x5[[1]][x5[[1]]==i & x5[[2]]==names(table(x5[[2]])[2])])/nrow(x5)*100), "\n"))}
     
     
     
     