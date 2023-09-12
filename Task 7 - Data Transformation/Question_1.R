# Вопрос 1: Результаты наблюдений переменной сохранены в файле: data_file.dat

data_file <- "data_file1.dat"
data_frame <- read.csv(paste0(dirname(rstudioapi::getSourceEditorContext()$path), "/", data_file))
rm(data_file)

# Загрузка пользовательских функций
directory_path <- getwd()
functions_file <- "/resources/R_function.R"
source(paste0(directory_path, functions_file))
rm(functions_file)

# • Централизация и найдите Q1 - 1-й, или нижний квартиль
centered_data <- x.centering(data_frame$variable)
Q1 <- quantile(centered_data, 0.25)

# • Нормирование и найдите Q2 - 2-й квартиль (медиану)
normalized_data <- x.scaling(data_frame$variable)
Q2 <- quantile(normalized_data, 0.5) # или median(normalized_data)

# • Стандартизация (перевод в значения стандартной Z-шкалы) и найдите P31 - 1-й процентиль:
z_scores <- z.scores(data_frame$variable)
P31 <- quantile(z_scores, probs = seq(0, 1, by = 0.01))[31 + 1]

# Вывод результатов:
cat("Q1 (нижний квартиль):", Q1, "\n")
cat("Q2 (медиана):", Q2, "\n")
cat("P31 (1-й процентиль):", P31, "\n")
