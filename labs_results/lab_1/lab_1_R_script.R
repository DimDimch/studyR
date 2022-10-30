
# ----------------
# ЗАДАНИЕ 1
# ----------------

fix_data <- function(df) {
  # идем по всем столбцам
  for (i in names(df)) {
    # проверяем нет ли буквенных символов в столбце
    if (length(grep('[a-z]', df[[i]], ignore.case = T)) == 0) {
      # убираем пробелы и приводим к даблу
      df[[i]] <- as.double(gsub(" ", "", df[[i]], fixed = TRUE))
    }
  }
  return (df)
}

# считываем данные
df_e1 <- read.csv('data/lab1_e1.csv')

# убедимся, что все столбцы считались как строки
sapply(df_e1, typeof)

# применяем fix_data
df_e1_fixed <- fix_data(df_e1)

# убедимся, что все теперь с типами все в порядке
sapply(df_e1_fixed, typeof)


# ----------------
# ЗАДАНИЕ 2
# ----------------

get_id <- function(df) {
  # сливаем все в один датафрейм
  full_df <- do.call("rbind", df)
  
  # группируем по id и считаем среднюю температуру
  mean_temp_df <- setNames(aggregate(temp ~ id, full_df, mean), c("id", "mean_temp"))
  
  # добавляем столбец с числом встречаний id в full_df
  id_count_df <- setNames(aggregate(temp ~ id, full_df, length), c("id", "count"))
  
  # джойним эти фреймы
  result <- merge(mean_temp_df, id_count_df, by = 'id')
  
  # выбираем только те, где пациент появлялся 7 раз
  result <- subset(result, count == 7, select = c('id', 'mean_temp'))
  
  # меняю старую нумерацию строк на новую
  row.names(result) <- c(1:nrow(result))
  return (result)
}

load('data/lab1_e2.Rdata')

df_e2 <- get_id(all_data)



