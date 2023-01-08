reforest = read_csv2('data/reforest.csv')

reforest <- reforest[rowSums(
    !is.na(reforest) &
    reforest[,] != '…2)' &
    reforest[,] != '-'
    ) == ncol(reforest), ] 

reforest[, c(2:ncol(reforest))] <- sapply(reforest[, c(2:ncol(reforest))], 
                                          function(x) as.double(gsub(",", ".", x)))

reforest <- as_tibble(reforest)

need_regions = c(
  'Республика Карелия',
  'Республика Коми',
  'Архангельская область',
  'Вологодская область',
  'Новгородская область',
  'Псковская область'
)

reforest <- reforest %>%
  filter(Region %in% need_regions) %>%
  mutate(Region = paste(
    Region, '( суммарно:', rowSums(across(where(is.numeric))), ')')) %>% 
  pivot_longer(cols = `2005`:`2016`,
               names_to = 'year', 
               values_to = "value")

ggplot(reforest, aes(x = year, y = value, group = Region)) +
  geom_line(aes(color = Region)) +
  geom_point(aes(color = Region)) +
  scale_colour_manual(values = 
                        c('magenta','royalblue','green','red','cyan','orange')) +
  labs(title = 'Лесовосстановление в Сереверо-заподном ФО',
       x = 'Дата', 
       y = 'Гектары (тыс.)',
       color = 'Регионы') + 
  theme_classic()
  
 