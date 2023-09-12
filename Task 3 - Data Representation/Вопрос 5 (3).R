# Вопрос 5: Регистрировалась дистанция, преодолённая каждым из участников, в км:
distant <- c(12, 11, 2, 9.3, 9.8, 12.1, 8.4, 12.7, 11.7, 12)

# Группирование с интервалом 2:
boundary <-c(2, 4, 6, 8, 10, 12, 14)
distant.grouped <- cut(sort(distant), breaks = boundary, right = FALSE)

# Количество разрядов:
length(table(distant.grouped))

# Частота 2 разряда:
table(distant.grouped)[2]

# Накопленная частота 3 разряда:
cumsum(table(distant.grouped))[3]

# Относительная частота 1 разряда:
(table(distant.grouped)/length(distant))[1]

# Относительная накопленная частота 4 разряда:
(cumsum(table(distant.grouped)/length(distant))[4])*100
