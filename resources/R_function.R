# source("C:/Users/ak179/OneDrive/Магистратура/2 КУРС/1 СЕМЕСТР/Лекции (2 курс)/Язык R (шпоры и задачи)/Скрипты/Функции для R/R_function.R")

# var - дисперсия;
# sd  - среднеквадратичное отклонение;
# x   - число или вектор;


x.var <- function(x){sum((x - mean(x))^2)/length(x)}
x.sd  <- function(x){sqrt(sum((x - mean(x))^2)/length(x))}

x.centering <- function(x) # #центрация (centering)
{
  # центрация (centering) — такое изменение шкалы, а фактически — данных, 
  # по ней измеренных, чтобы среднее стало равным 0. 
  # Для этого достаточно вычесть среднее из каждого значения переменной:
  
  x.centr <- (x-mean(x))
  return(x.centr)
}


x.scaling <- function(x, mode=TRUE)   # нормирование [на стандартное отклонение] 
{
  # (scaling [by standard deviation]) — изменение масштаба, т.е. 
  # единицы измерения, так, чтобы стандартное отклонение стало равным 1.
  # Для этого каждое значение переменной делится на 
  # её стандартное отклонение:
  
  if (mode == TRUE){
    x.var  <- sum((x - mean(x)) ^ 2)/length(x)  
    x.sd   <- sqrt(x.var)
    x.scal <- x/x.sd
    
  } else if (mode==FALSE){x.scal<- x/sd(x)}
  
  return(x.scal)
}



z.scores <- function(x, mode=TRUE) # Шкала стандартного нормального распределения (стандартизация)
{
  # Шкалу стандартного нормального распределения принято 
  # обозначать буквой Z и так и называть — Z-шкалой (Z-scale), 
  # а его значения — z-значениями, z-оценками или z-баллами (z-scores).
  
  # z-значения — это разновидность мер положения типа квантилей, но в отличие от квантилей, 
  # z-значения описывают расположение распределения относительно его центра, а не минимума,        т.е. mean(z.scroes) = 0 
  # и единицей измерения при этом вместо единиц шкалы переменной служит стандартное отклонение.           sd(z.scroes) = 1
  
  if (mode == TRUE){
    x.var  <- sum((x - mean(x)) ^ 2)/length(x)  
    x.sd   <- sqrt(x.var)
    z.scor <- (x-mean(x))/x.sd
    
  } else if (mode==FALSE){z.scor <- (x-mean(x))/sd(x)}
  
  return(z.scor)
  
}


z.quan.normaliz <- function(vec, raw.scores=vec) # Функция квантильной нормализации в стандартную z-шкалу
{
  raw.scores <- unique(raw.scores)
  # vec - вектор сырых данных;
  # raw.scores - конкретные сырые баллы, по умолчанию весь вектор
  
  # Порядок действий при квантильной нормализации:
  # 1. составляем таблицу относительных накопленных частот показателя
  # 2. определяем по ней относительную накопленную частоту интересующей оценки;
  # 3. находим квантиль стандартного нормального распределения c кумулятивной вероятностью, равной этой частоте.
  
  vec.cumsum <- cumsum(table(vec)/length(vec))     # накопленная частота
  z.scores <- qnorm(vec.cumsum)                # "qnorm()" - квантиль (quantile)
  return(z.scores[as.character(raw.scores)])
}



z.IQ.scale <- function(x, raw.scores=(x)) # Шкалы интеллекта
{
  raw.scores <- unique(raw.scores)        # удаление повторений
  
  # x - вектор сырых данных;
  # raw.scores - конкретные сырые баллы, по умолчанию весь вектор
  
  x.DF <- data.frame(
    "raw.scores"     = raw.scores,  
    "z.scores"       = z.scores     <- z.quan.normaliz(x)[as.character(raw.scores)],
    "z5.Wexler"      = z.Wexler     <- (z.scores * 15) + 100, 
    "z5.Stanf.Bin"   = z.Stanf.Bin  <- (z.scores * 16) + 100 
  )
  names(x.DF) <- c("сырой балл", "z-значение", "стандартный балл (Векслер)", "стандартный балл (Стэнфорд—Бине)")
  
  alf <- a.letters.eng(); i <- 1
  
  while (length(raw.scores)>length(alf)){alf <- c(alf, a.letters.eng(i)); i <- i+1} #Если количество символов больше алфавита, то увеличить алфавит
  
  x.df <- data.frame(alf[1:length(raw.scores)])
  x.df[c(2:(length(x.DF)+1))] <- x.DF
  names(x.df)[1] <- c("балл")
  return(x.df)
  
}





z.sound <- function(x, x.index=c(1:length(x))){  # Преобразование шкалы интенсивности звука
  
  # x.index - порядковый номер значения
  
  x.I  <- x                    # - раздражитель
  x.I0 <- 10**(-12)            # - граничное значение интенсивности раздражителя
  x.Lp <- 10*log10(x.I/x.I0)   # - Закон Вебера-Фехнера (уровень звука)
  
  # Преобразуйте те же исходные данные в шкалу "уровня громкости", в сонах. 
  x.Ls <- (x.I/x.I0)**0.3      # - Закон Стивенса (уровень громкости)
  
  # Переведите их же в шкалу рангов.
  x.Lp.rank <- rank(x.Lp)      # !ОНИ ОДИНАКОВЫ! 
  x.Ls.rank <- rank(x.Ls)      # !ОНИ ОДИНАКОВЫ!
  
  # Найдите значения элементов исходного и преобразованных векторов с индексами:
  
  DF.sound <- data.frame(
    "индекс"      =  x.index,
    "I,  [Вт/м²]" =  x.I [x.index],
    "Lp, [дБ]"    =  x.Lp[x.index],
    "Ls, [соны]"  =  x.Ls[x.index],
    "ранг"        =  x.Lp.rank[x.index] # или x.Ls.rank[x.index]
  )
  names(DF.sound) <- c("индекс", "I,[Вт/м²]", "Lp,[дБ]", "Ls,[соны]",  "ранг")
  return(DF.sound)
}


table.sort <- function(x){ # функция для сортировки таблицы по верхней строке (для чисел)
  return(x[as.character(sort(as.numeric(names(x))))])
}




mean.freq <- function(x.freq){  # Среднее по таблице частот (АБСОЛЮТНЫЕ)
  if (!is.table(x.freq)) {return(("Не таблица частот!")); break}
  sum(as.numeric(names(x.freq))* x.freq) / sum(x.freq)  
}



median.freq <- function(x.freq, summary = FALSE) # Функция «медиана для таблицы частот»
{
  # где:
  # x.freq - таблица частот
  
  # Медиана  вариационного ряда – это значение, которая делит его
  # на две равные части (по количеству вариант) 
  
  if (!is.table(x.freq)){return(("Не таблица частот!")); break}
  
  median.result <-list(
    "Результат поиска медианы:"="↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓",
    "Изначальная таблица частот:"     = x.freq,
    "Отсортированная таблица частот"  = x.freq <- table.sort(x.freq),
    "Объем наблюдений:"               = n <- sum(x.freq),
    "Индекс медианы:"                 = i <- (n+1)/2,
    "Накопленные частоты::"           = x.freq <- cumsum(x.freq)
  )
  if  (summary == TRUE){
  print(median.result) 
  }
  
  i1<-0
  i2<-0
  a1 <- ceiling(i) - 1
  a2 <- ceiling(i)
  if (i == ceiling(i)) {
    for(i1 in x.freq){
      i2<-i2+1
      if (i1>i)
      {return(as.numeric(names(x.freq[i2])))
        break
      }
    }
  } else if(i != ceiling(i)){ 
    for(i1 in x.freq){
      i2<-i2+1
      if (i1>i){
        if (a1 >  (as.numeric(x.freq[i2-1]))) {return(as.numeric(names(x.freq[i2])))}
        if (a1 <  (as.numeric(x.freq[i2-1]))) {return(as.numeric(names(x.freq[i2-1])))}
        if (a1 == (as.numeric(x.freq[i2-1]))) {return(sum(as.numeric(names(x.freq[i2])) + as.numeric(names(x.freq[i2-1])))/2)}
        break
      }
    } 
  }
}

# Функция для группирования
x.group <- function(vec, x.from = (floor(min(vec))), x.to = (ceiling(max(vec))+1), x.by = 1){
  x.seq<-seq(from = x.from, to = x.to, by = x.by); x.seq
  x.cut <- cut(vec, breaks = x.seq, right = FALSE)
  x.cut.t <- table(x.cut)
  
  
  x.list <- list(
    "Границы изначального вектора:" = range(vec),
    "Границы группирования:"        = (c(names(c(x.cut.t)[1]), names((x.cut.t)[length(x.cut.t)])))
  )
  print(x.list)
  return(x.cut.t)
}

# Поиск моды
x.mode <-function(x){
  # Моды мультимодального распределения не обязательно имеют одну и ту же частоту, 
  # это просто достаточно удалённые друг от друга вершины. 
  # Условимся в ответе указывать нижнюю границу модального интервала, 
  # окрулённую до целого в нижнюю сторону.
  if (!is.table(x)){
    x.mod <- table(x)[table(x)==max(table(x))]
    x.mod <-as.integer(names(floor(x.mod)[1]))
  }
  if (is.table(x)){
    x.mod <- (x)[(x)==max((x))]
    x.mod <-as.integer(names(floor(x.mod)[1]))
  }
  return(x.mod)
}



# линия с текстом
abline.text <- function(x, y, text, col = 'firebrick1', lwd = 1){
  abline(v = x, col = col, lwd = lwd)
  text  (x = x,  y = y, text, col = col)
}


x.result <- function(x, help = FALSE){
  # install.packages("moments")
  # library(moments)  #загрузка пакета moments
  
  x.skewness <- sum((x - mean(x)) ^ 3)/(length(x)*(sqrt(x.var(x)))^3)     # skewness(x, na.rm = TRUE)
  x.kurtosis <- sum((x - mean(x)) ^ 4)/(length(x)*(sqrt(x.var(x)))^4)-3   # kurtosis(x, na.rm = TRUE)-3
  if (x.skewness < (0.1) &  x.skewness>(-0.1)){x.skew<-c("близко к симметричному")}
  if (x.skewness < (-0.1)){x.skew<-c("левосторонняя асимметрия")}
  if (x.skewness > (0.1)) {x.skew<-c("правосторонняя асимметрия")}
  
  cat('\033[1m', "меры центральной тенденции", '\033[0;0m'                         ,"\n")
  cat("---------------------------------------------------------"                                 ,"\n")
  cat("  мода:                        ", x.mode        <- x.mode(x)                     ,"\n")
  cat("\n")      
  cat("  медаина (2-й квартиль) Q2:   ", x.median      <- median(x)                     ,"\n")
  cat("  медаина (для таблицы частот):", x.median.freq <- median.freq(x)           ,"\n")
  cat("\n")      
  cat("  среднее арифметическое μ:    ", x.mean        <- mean(x)                       ,"\n")
  cat("  среднее (для таблицы частот):", x.mean.freq   <- mean.freq(x)               ,"\n")
  cat("---------------------------------------------------------"                                 ,"\n")       
  cat("\n")                           
  cat('\033[1m', "меры изменчивости   ", '\033[0;0m'                               ,"\n")
  cat("---------------------------------------------------------"                                 ,"\n")
  cat("  размах range:                ", x.range  <- range(x)[2]-range(x)[1]          ,"\n")
  cat("  дисперсия σ2:                ", x.var    <- sum((x - mean(x)) ^ 2)/length(x) ,"\n")
  cat("  стандартное отклонение σ:    ", x.sd     <- sqrt(x.var)                      ,"\n")
  cat("  межквартильный размах IQR:   ", x.IQR    <- IQR(x)                           ,"\n")                                                     
  cat("---------------------------------------------------------"                                 ,"\n")       
  cat("\n")                               
  cat('\033[1m', "меры формы", '\033[0;0m'                                         ,"\n")     
  cat("---------------------------------------------------------"                                 ,"\n")       
  cat("  вид асимметрии:              ", x.skew                                    ,"\n")     
  cat("  коэффициент асимметрии γ1:   ", x.skewness                                ,"\n")     
  cat("  коэффициент эксцесса   γ2:   ", x.kurtosis                                ,"\n")   
  cat("---------------------------------------------------------"                                 ,"\n")       
  cat("\n")
  
  
  
  if (help==TRUE){
    cat("---","\033[3;34m", "ПОМОЩЬ", "\x1B[0m", "---", "\n\n") 
    cat("• Робастность   - устойчивость;","\n")
    cat("• Неробастность - неустойчивость.","\n")
    cat("\n")
    cat("• Для","\033[3;34m", "асимметричных", "\x1B[0m", "— распределений адекватны РОБАСТНЫЕ", "\n") 
    cat("  меры изменчивости, основанные на квантилях: \033[4mразмах\033[0m, \033[4mразмах IQR\033[0m;", "\n") 
    cat("\n")
    cat("• Для","\033[3;34m", "симметричных", "\x1B[0m", " — более чувствительные, основанные на", "\n")  
    cat("  среднем арифметическом: \033[4mдисперсия\033[0m и \033[4mстандартное отклонение\033[0m.", "\n")
    cat("\n")
    cat('\033[1m', "меры центральной тенденции", '\033[0;0m'                         ,"\n")
    cat("---------------------------------------------------------"                                 ,"\n")
    cat(" •","\033[3;34m", "МОДА", "\x1B[0m", "является самой универсальной, т.е. может использоваться с", "\n")
    cat("    данными всех типов, но и наименее точной.", "\n") 
    cat("    Рекомендуется использовать при НОМИНАЛЬНОЙ шкале или", "\n")  
    cat("    при УНИМОДАЛЬНОМ распределении", "\n")
    cat("\n")  
    cat(" •","\033[3;34m", "МЕДИАНА", "\x1B[0m", "точнее моды, но применима только к количественным данным,", "\n")
    cat("    т.е. начиная с порядковых, а её точность ограничена интервалом", "\n")
    cat("    между соседними значениями переменной.", "\n")
    cat("    Медиана является, напротив, РОБАСТНОЙ мерой центра", "\n")
    cat("\n")
    cat(" •","\033[3;34m", "СРЕДНЕЕ АРИФМЕТИЧЕСКОЕ", "\x1B[0m", "— потенциально самая точная мера центра,", "\n")
    cat("    но применима только к метрическим данным и распределениям,", "\n")
    cat("    обладающим СИММЕТРИЕЙ, поскольку НЕРОБАСТНА к её нарушениям.", "\n")
    cat("---------------------------------------------------------"                                 ,"\n")
    cat("\n") 
    cat('\033[1m', "меры изменчивости", '\033[0;0m'                         ,"\n")
    cat("---------------------------------------------------------"                                 ,"\n")
    cat(" •","\033[3;34m", "Межквартильный размах ", "\x1B[0m", "— робастная мера изменчивости,", "\n")
    cat("    поскольку использует только  информацию о порядке", "\n")
    cat("    возрастания наблюдений, но не об интервалах между ними.", "\n")
    cat("    Поэтому её точность ограничена шагом между соседними наблюдениями.  ", "\n")
    cat("\n")  
    
    cat(" •","\033[3;34m", "Стандартное отклонение ", "\x1B[0m", "— неробастно к асимметричности распределения.", "\n")
    cat("    Стандартное отклонение можно интерпетировать, как отклонение,", "\n")
    cat("    в котором вес отдельных отклонений увеличивается по мере", "\n")
    cat("    их удаления от центра распределения.", "\n")
    cat("---------------------------------------------------------"                                 ,"\n")
    cat("\n") 
  }
}




#  x.result <- function(x){
#   # install.packages("moments")
#   # library(moments)  #загрузка пакета moments
#   
#   list(
#     # меры центральной тенденции
#     "медаина (2-й квартиль) Q2:"  = x.median   <- median(x),
#     "среднее арифметическое μ:"   = x.mean     <- mean(x),
#     "мода:"                       = x.mode     <- x.mode(x),
#     
#     # меры изменчивости    
#     "размах range:"               = x.range    <- range(x)[2]-range(x)[1],
#     "дисперсия σ2:"               = x.var      <- sum((x - mean(x)) ^ 2)/length(x),  
#     "стандартное отклонение σ:"   = x.sd       <- sqrt(x.var),
#     "межквартильный размах IQR:"  = x.IQR      <- IQR(x),                                                      
#     
#     # меры формы"                            
#     "коэффициент асимметрии γ1:"  = x.skewness <- sum((x - mean(x)) ^ 3)/(length(x)*(sqrt(x.var(x)))^3),     # skewness(x, na.rm = TRUE)
#     "коэффициент эксцесса   γ2:"  = x.kurtosis <- sum((x - mean(x)) ^ 4)/(length(x)*(sqrt(x.var(x)))^4)-3   # kurtosis(x, na.rm = TRUE)-3
#   )
# }


# ЭКСПЕРИМЕНТАЛЬНЫЕ ФУНКЦИИ ____________________________


# DF.search <- function(df, scores, column_vec, column_scores){
#   df <- (df[c(column_vec,column_scores)])
#   df <- as.table((df[[1]])[df[[2]]==scores])
#   names(df) <- scores
#   return(df)
# }


# Алфавит
a.letters.eng <- function(i=NULL){c(paste0("a", i), paste0("b", i), paste0("c", i), paste0("d", i),paste0("e", i), paste0("f", i), paste0("g", i), paste0("h", i), paste0("i", i), paste0("j", i), paste0("k", i), paste0("l", i), paste0("m", i), paste0("n", i), paste0("o", i), paste0("p", i), paste0("q", i), paste0("r", i), paste0("s", i), paste0("t", i), paste0("u", i), paste0("v", i), paste0("w", i), paste0("x", i), paste0("y", i), paste0("z", i))}
a.LETTERS.ENG <- function(i=NULL){c(paste0("A", i), paste0("B", i), paste0("C", i), paste0("D", i),paste0("E", i), paste0("F", i), paste0("G", i), paste0("H", i), paste0("I", i), paste0("J", i), paste0("K", i), paste0("L", i), paste0("M", i), paste0("N", i), paste0("O", i), paste0("P", i), paste0("Q", i), paste0("R", i), paste0("S", i), paste0("T", i), paste0("U", i), paste0("V", i), paste0("W", i), paste0("X", i), paste0("Y", i), paste0("Z", i))}
a.letters.rus <- function(i=NULL){c(paste0("а", i), paste0("б", i), paste0("в", i), paste0("г", i),paste0("д", i), paste0("е", i), paste0("ё", i), paste0("ж", i), paste0("з", i), paste0("и", i), paste0("й", i), paste0("к", i), paste0("л", i), paste0("м", i), paste0("н", i), paste0("о", i), paste0("п", i), paste0("р", i), paste0("с", i), paste0("т", i), paste0("у", i), paste0("ф", i), paste0("х", i), paste0("ц", i), paste0("ч", i), paste0("ш", i),paste0("щ", i),paste0("ъ", i),paste0("ы", i),paste0("ь", i),paste0("э", i),paste0("я", i))}
a.LETTERS.RUS <- function(i=NULL){c(paste0("А", i), paste0("Б", i), paste0("В", i), paste0("Г", i),paste0("Д", i), paste0("Е", i), paste0("Ё", i), paste0("Ж", i), paste0("З", i), paste0("И", i), paste0("Й", i), paste0("к", i), paste0("Л", i), paste0("М", i), paste0("Н", i), paste0("О", i), paste0("П", i), paste0("Р", i), paste0("С", i), paste0("Т", i), paste0("У", i), paste0("Ф", i), paste0("Х", i), paste0("Ц", i), paste0("Ч", i), paste0("Ш", i),paste0("Щ", i),paste0("Ъ", i),paste0("Ы", i),paste0("Ь", i),paste0("Э", i),paste0("Ю", i),paste0("Я", i))}



