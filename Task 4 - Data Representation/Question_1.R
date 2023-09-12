# Вопрос 1: Упаковка минеральной воды весит 107 кг,
#           а упаковка растительного масла — на 10% меньше.

# Введя эти веса в R как константы, вычислите,
# сколько весит груз из упаковок:

water_weight <- 107
oil_weight <- water_weight - (water_weight * 0.10)

cat("2 упаковки воды (", water_weight, "кг) и 14 упаковок масла (", oil_weight, "кг) весят в сумме:", 2 * water_weight + 14 * oil_weight, "кг\n")
cat("3 упаковки воды (", water_weight, "кг) и 19 упаковок масла (", oil_weight, "кг) весят в сумме:", 3 * water_weight + 19 * oil_weight, "кг\n")
cat("4 упаковки воды (", water_weight, "кг) и 13 упаковок масла (", oil_weight, "кг) весят в сумме:", 4 * water_weight + 13 * oil_weight, "кг\n")
