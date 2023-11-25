# Вопрос 9: У водителей двух машин в случайно выбранные даты
#           отслеживали дневной километаж: drivers.dat. Проверьте
#           предположение, что эти водители в целом различаются по
#           распределению протяжённости своих дневных поездок.

# 1) Дано:
     # x9 <- "drivers.dat"; x9   <- read.csv(paste0(dirname(rstudioapi::getSourceEditorContext()$path), "/", x9))
     x9 <- "results.dat"; x9   <- read.csv(paste0(dirname(rstudioapi::getSourceEditorContext()$path), "/", x9))
     str(x9)  #  вывод сведений о структуре объекта
     P_true <- 0.5
          
#    1.1) 
#         ¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯              
     car1 <- x9[[2]][x9[[1]]=="A"]
     car2 <- x9[[2]][x9[[1]]=="B"]
     
     
     ks.test(car1,car2) # критерий однородности Смирнова
     
     ks.test(x9[[2]]~x9[[1]])
     
     p_value <- ks.test(car1,car2)[[2]]
     
     
     cat("\n   • D (Смирнова):", ks.test(x9[[2]]~x9[[1]])[[1]][[1]],      "\n")
     cat(  "   • p:", p_value, "\n")
     
     
     if    (p_value < 0.05) 
     {cat("\nНулевая гипотеза H0 - \033[1mотвергается\033[0m\n")
       cat("Результат оказался статистически \033[1mзначимым\033[0m\n")
     } else 
     {cat("\nНулевая гипотеза H0 - \033[1mне отвергается\033[0m\n")
       cat("Результат оказался статистически \033[1mне значимым\033[0m\n")}     
     
     
     
     
     
     
# Вопрос 10:  
     # Удаляю имя папки и получаю путь в общий каталог
     script_dir <- dirname(rstudioapi::getActiveDocumentContext()$path)
     script_path <- file.path(script_dir, "..", "resources", "R_function.R")
     source(script_path)

     x.result(x9[[2]][x9[[1]]=="A"])
     x.result(x9[[2]][x9[[1]]=="B"])
     