# Вопрос 5: Регистрировалась дистанция, преодолённая каждым из участников, в км:
distance <- c(12, 11, 2, 9.3, 9.8, 12.1, 8.4, 12.7, 11.7, 12)

# Группирование с интервалом 2:
distance_grouped <- cut(sort(distance), breaks = boundary, right = FALSE)

# Количество разрядов:
num_groups <- length(table(distance_grouped))

# Частота 2 разряда:
freq_2nd_group <- table(distance_grouped)[2]

# Накопленная частота 3 разряда:
cum_freq_3rd_group <- cumsum(table(distance_grouped))[3]

# Относительная частота 1 разряда:
rel_freq_1st_group <- (table(distance_grouped)/length(distance))[1]

# Относительная накопленная частота 4 разряда:
rel_cum_freq_4th_group <- (cumsum(table(distance_grouped)/length(distance))[4])*100

# Вывод результатов:
cat("Группирование с интервалом 2: ", distance_grouped, "\n")
cat("Количество разрядов: ", num_groups, "\n")
cat("Частота 2 разряда: ", freq_2nd_group, "\n")
cat("Накопленная частота 3 разряда: ", cum_freq_3rd_group, "\n")
cat("Относительная частота 1 разряда: ", rel_freq_1st_group, "\n")
cat("Относительная накопленная частота 4 разряда: ", rel_cum_freq_4th_group, "\n")
