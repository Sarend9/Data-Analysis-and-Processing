# Вопрос 3: Получены следующие данные о географии заявок:
request <- c("МСК", "МСК", "СПБ", "СПБ", "СПБ", "МСК", "СПБ", "СПБ", "МСК", "ЕКБ")

# Определите:
# Объем наблюдений:
obs_count <- length(request)
cat("Объем наблюдений: ", obs_count, "\n")

# Абсолютная частота определенного элемента вектора:
spb_freq <- table(request)["СПБ"]
cat("Абсолютная частота для СПБ: ", spb_freq, "\n")

# Относительная частота определенного элемента вектора:
msk_rel_freq <- (table(request) / obs_count)["МСК"]
cat("Относительная частота для МСК: ", msk_rel_freq, "\n")

# Относительная частота заявок определенного элемента вектора в процентах:
ekb_rel_freq_percent <- (table(request) / obs_count)["ЕКБ"] * 100
cat("Относительная частота для ЕКБ в процентах: ", ekb_rel_freq_percent, "%\n")
