# Вопрос 3: Результаты измерения длины волос у студентов вуза
#           сохранены в файле: data_file2.dat

file <- "data_file3.dat"
x3.6 <- read.csv(paste0(dirname(rstudioapi::getSourceEditorContext()$path), "/", file)); rm(file)

x3.6$hair

# Мера центральной тенденции: мода

# №1
  unique(x3.6$hair)[which.max(tabulate(match(x3.6$hair, unique(x3.6$hair))))]

# №2
  table(x3.6$hair)[table(x3.6$hair) == max(table(x3.6$hair))]


boxplot(x3.6$hair,
        horizontal = TRUE,  # горизонтальная ориентация диаграммы
        col = "cornsilk1",
        main = "Сводные характеристики распределения волос у студентов вуза",
        xlab = "длина волос",
        ylab = 'измерение 1'
  )

hist(x3.6$hair, 15, ylim = c(0,60))


x3.6.group <- table(cut(sort(x3.6$hair), breaks = 15, right = FALSE))
max(x3.6.group)


x3.6.hist <- hist(x3.6$hair, 15)
x3.6.hist.t <- as.table(x3.6.hist$counts)
names(x3.6.hist.t) <- x3.6.hist$mids
x3.6.hist.t

# Моды мультимодального распределения не обязательно имеют одну и ту же частоту, 
# это просто достаточно удалённые друг от друга вершины. 
# Условимся в ответе указывать нижнюю границу модального интервала, 
# окрулённую до целого в нижнюю сторону.

x3.6.seq<-seq(floor(min(x3.6$hair)), ceiling(max(x3.6$hair))) # ceiling - округление в большую сторону
x3.6.cut <- cut(x3.6$hair, breaks = x3.6.seq, right = FALSE)
x3.6.cut.t <- table(x3.6.cut)
x3.6.cut.t

par(mfrow = c(1, 3))
plot(x3.6.hist.t)
plot(x3.6.cut.t)
hist(x3.6$hair, 15, ylim = c(0,60))

