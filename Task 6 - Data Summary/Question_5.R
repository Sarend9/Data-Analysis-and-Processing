# Вопрос 5: Результаты измерения у группы добровольцев уровня интеллекта по стандартной шкале от 0 до 60 баллов сохранены в файле: data_file.dat

file <- "data_file5.dat"
x5.6 <- read.csv(paste0(dirname(rstudioapi::getSourceEditorContext()$path), "/", file)); rm(file)

print(sort(x5.6$score)); cat("\n")

print("Определите, целесообразно ли с точки зрения дальнейшего анализа очистить данные от выбросов.")
print("Если да, то исключите соответствующие наблюдения")
print("Затем выберите адекватные сводные характеристики распределения и найдите их:") ; cat("\n")


x5.6.result <-list(
    "медиана:"                = median(x5.6$score[x5.6$score>=0 & x5.6$score<=60])   ,
    "межквартильный размах:"  = IQR(x5.6$scor[x5.6$score<=60])   
) #конец 1

print(x5.6.result)


# График
par(mfrow = c(3,1))

x5.6.sort <- x5.6$score[x5.6$score<=60]

hist(x5.6$score, 15)
hist(x5.6.sort, breaks = 15)


x5.6.boxlpot <- boxplot(x5.6$score,
                      horizontal = TRUE,
                      col = "cornsilk1",
                      main = "Сводные характеристики 
                      распределения уровня интелекта                      ",
                      xlab = "уровень интеллекта",
                      ylab = 'измерение 1'
)

abline(v = median(x5.6.sort), col = 'red', lwd = 1)
text  (x = median(x5.6.sort), y = 1.3, 
       'медиана', 
       col = 'red')                      
