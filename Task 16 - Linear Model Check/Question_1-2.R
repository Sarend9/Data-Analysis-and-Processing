# Вопрос 1: Участники исследования выполняли тестовое задание. При этом каждому
#           случайным образом доставался один из трёх вариантов задания. Вариант
#           задания и успешность его выполнения (в %) участником записаны в файле:
#           task.dat. 
#          

# 1) Дано:
     file <- "task.dat"
     x1   <- read.csv(paste0(dirname(rstudioapi::getSourceEditorContext()$path), "/", file)); rm(file)
     str(x1)  #  вывод сведений о структуре объекта
   
     
#    Поскольку групп больше двух, для проверки 
#    нулевой гипотезы H0:μ1=μ2=μ3 применяется дисперсионный анализ. 
     
     
# 2) Проверьте гипотезу о зависимости успешности выполнения от варианта задания.    
#    ¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
#    Сравнение средних нескольких групп выполняется посредством "дисперсионного анализа".

#    Для этого используем функцию aov (analysis of variance) аналогична функции lm() в регрессионном анализе.
     
#    2.1) Её первый аргумент — зависимая переменная, а второй, после ~ — фактор.
#         Второй аргумент должен быть номинальным, т.е. в факторном либо текстовом формате, а не в числовом,
#         иначе aov() вместо ANOVA будет строить регрессионную модель.
     
#         Переводим второй аргумент в факторный формат:
          x1[ , 1] <- factor(x1[ , 1])
     
#    2.2) Затем, чтобы получить сводку по модели, к результату применяем summary()   
          summary(aov(x1[[2]] ~ x1[[1]], data = x1))  # зависимость результата от варианта
          
          df1     <- (summary(aov(x1[[2]] ~ x1[[1]], data = x1))[[1]])[[1]][1]; df1
          df2     <- (summary(aov(x1[[2]] ~ x1[[1]], data = x1))[[1]])[[1]][2]; df2
          F       <- (summary(aov(x1[[2]] ~ x1[[1]], data = x1))[[1]])[[4]][1]; F
          p.value <- (summary(aov(x1[[2]] ~ x1[[1]], data = x1))[[1]])[[5]][1]; p.value
     
#    2.3)  Помимо проверки достоверности различий средних нам
#          нужно определить относительную величину этих
#          различий, то есть размер эффекта. Функция aov() не
#          вычисляет η2 , но его легко посчитать самим по формулам:
           SSeffect   <- (summary(aov(x1[[2]] ~ x1[[1]], data = x1))[[1]])[[2]][1]
           SSresidual <- (summary(aov(x1[[2]] ~ x1[[1]], data = x1))[[1]])[[2]][2]
           η2 <- SSeffect / (SSeffect + SSresidual); η2
           
           
# 3) Выясните, какие варианты задания достоверно различаются по успешности выполнения.
#    ¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯          
#    Введите скорректированные p-значения
#
#    Для вычислений скорректированных p-значений 
#    попарных сравнений всех групп используем команду:
     pairwise.t.test(x1[[2]], g = x1[[1]])  # множественные сравнения с помощью t-критериев (с поправкой Хольма)
     
#    Аргумент g = функции pairwise.t.test() задаёт группирующую переменную, т.е.
#    фактор, по уровням которого наблюдения делятся на группы. По умолчанию вычисляется поправка Хольма.
     
# 4) Укажите средние и доверительные интервалы для успешности выполнения вариантов*:     
     print("Доверительные интервалы:")
     DF <- data.frame()
     for (i in 1:length(table(x1[[1]]))){
       DF[i,1]  <- t.test(x1[[2]][x1[[1]]             == i] , conf.level = 0.9) [[5]][[1]]
       DF[i,2]  <- paste0(attr(t.test(x1[[2]][x1[[1]] == i] , conf.level = 0.9) [[4]], "conf.level")*100, "%")
       DF[i,3]  <- t.test(x1[[2]][x1[[1]]             == i] , conf.level = 0.9) [[4]][[1]]
       DF[i,4]  <- "—"
       DF[i,5]  <-  t.test(x1[[2]][x1[[1]]  == i] , conf.level = 0.9) [[4]][[2]]}
       names(DF) <- c("варианты x", "CI", " ", " ", " ")
      DF
      
      # или моя функция:
     script_dir <- dirname(rstudioapi::getActiveDocumentContext()$path)
     script_path <- file.path(script_dir, "..", "resources", "R_function.R")
     source(script_path)

      
     Static_output(x1, CL_n_group = 90, CL_n = 90)
      
# 5) Интепретация:

#    Функция   Anova.disp() может выдать уже интерпетацию по ANOVA:        
     Anova.disp(x1)
     # 
     # С помощью доверительных интервалов CI определили диапазоны успешности выполнения каждого варианта, в которых с CL= 90%-ой вероятностью заключены параметры генеральной совокупности.
     # Выборочные стандартные успешности выполнения задания каждого варианта составили соответственно 69.575%, 67.985 и 73.050%.
     # Для проверки гипотезы о зависимости успешности выполнения от варианта задания, использовался однофакторный дисперсионный анализ.
     # Результат оказался статистически значимым: F(2,57) = 1.597 , p = 0.211.
     # Попарные пост-хок сравнения при помощи t-критериев (с поправкой Хольма). показали достоверные различия между вариантами 1 и 2 (p = 0.47), 2 и 3 (p = 0.26).
     # Таким образом, гипотеза о зависимости успешности выполнения от варианта задания подтвердилась.
     # Обнаружен слабый эффект зависимости разницы между вариантами и успешностью выполнения (η2 = 0.053). Статистически достоверна успешность от варианта задания лишь при 1 и 3 варианте и при 2 и 3.
    





