library(tidyverse)

load('data/trades.RData')

trades <- bind_rows(trades)

trades <- trades %>%
  select(!geo)

export <- trades %>%
  filter(indic_et == 'Exports in million of ECU/EURO') %>%
  select(!indic_et)

import <- trades %>%
  filter(indic_et == 'Imports in million of ECU/EURO') %>%
  select(!indic_et)


big_partners <- export %>%
  group_by(partner) %>%
  summarise(values = sum(values)) %>%
  slice_max(values, n = 7)

export <- export %>%
  filter(partner %in% big_partners$partner) %>%
  group_by(sitc06) %>%
  mutate(total_value = sum(values)) %>%
  group_by(partner, sitc06, total_value) %>%
  summarise(values = sum(values))

export <- export %>%
  mutate(percent = round(values / total_value * 100, 1)) %>%
  select(!total_value)

ggplot(data = export, aes(x = values, y = partner, fill = sitc06)) +
  geom_col(position = "dodge") +
  geom_text(aes(label = paste(values, '(', percent, '%)')), 
                size = 3.2,
                position = position_dodge(0.95), 
                hjust = 0, 
                check_overlap = TRUE) +
  labs(title = 'Structure of exports of products from the European Union', 
       subtitle = 'By leading partners',
       x = 'Sum Values', 
       y = 'Partners', 
       fill = 'Export Groups') +
  xlim(c(0, 1750000)) +
  theme_classic() +
  theme(legend.position = "bottom")
  

  


