# Вопрос 1: Результаты наблюдений переменной сохранены в файле: data_file.dat



file <- "data_file1.dat"
x1.7 <- read.csv(paste0(dirname(rstudioapi::getSourceEditorContext()$path), "/", file)); rm(file)

# Выполните линейные преобразования, каждое — с исходными данными:
  


# Библиотека функций для поиска медианы:
# Удаляю имя папки и получаю путь в общий каталог
catalog <- gsub( basename(dirname(rstudioapi::getSourceEditorContext()$path)), '', paste0(dirname(rstudioapi::getSourceEditorContext()$path))) 
file <- "Функции для R/R_function.R"
# Открываю библиотеку с пользовательскими функциями
source(paste0(catalog, file));


  # • центрацию и найдите Q1 1-й, или нижний квартиль
  x1.7.centr <- x.centering(x1.7$variable)
  Q2       <- quantile(x1.7.centr, 0.25); Q2
  
  # • нормирование и найдите Q2 2-й квартиль (медиана)
  x1.7.norm <- x.scaling(x1.7$variable)
  Q2      <- quantile(x1.7.norm, 0.5); Q2 # или median(x1.7.norm)
  
  # • стандартизацию (перевод в значения стандартной Z-шкалы) и найдите P31-й процентиль:
  z1.7.scores <- z.scores(x1.7$variable)
  P31       <- quantile(z1.7.scores, probs=seq(0, 1, by=0.01))[31+1]; P31
  
