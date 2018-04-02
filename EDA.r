library(tidyverse)
library(data.table)

data <- read_rds("data2017.Rds") 
call <- read_rds('911.Rds') 

# add events' detail to data
detail <- str_trim(str_split(data$title,pattern = ':',n=2,simplify = TRUE)[,2])
data$detail <- detail
data$detail <- data$detail %>% str_replace_all(pattern = '-',replacement = '')

# transform data type
factor <- c('borough','year','month','week','weekday','hour',
            'season','period','event.type','detail')
data[factor] <- lapply(data[factor],as.factor)

# temperture difference
data <- data %>% mutate(diff.temp = tmax-tmin)
# average temperature
data <- data %>% mutate(ave.temp = (tmax+tmin)/2)



##### plot
# total number of calls broken down by event type
data %>%
  mutate(event.type = event.type %>% fct_infreq() %>% fct_rev()) %>%
  ggplot(aes(x=event.type)) +
  geom_bar(aes(fill=event.type),alpha=0.7)+
  scale_fill_brewer(palette = 'Blues') + theme_minimal()

# Top three detail by event type
# fall victim ?
# reorder 
detail_top5 <- data %>% 
  group_by(event.type) %>%
  count(detail) %>%
  arrange(desc(n), .by_group = TRUE) %>%
  top_n(5,wt=n) 

detail_top5 %>%
  filter(event.type=='EMS') %>%
  ggplot(aes(x=detail,y=n)) +
  geom_bar(aes(fill=detail),alpha=0.7,stat='identity',show.legend = FALSE)+
  scale_fill_brewer(palette="Blues") + theme_minimal()+
  coord_flip()

# trend by class
data %>%
  ggplot(aes(x=date)) + 
  geom_freqpoly()+
  facet_grid(~event.type)

# hourly
call %>%
  ggplot(aes(x=hour)) +
  geom_bar(aes(fill=class))

# monthly
call %>%
  ggplot(aes(x=month)) +
  geom_bar(aes(fill=class))

# temperature difference in each month
data %>%
  ggplot(aes(x=month,y=diff.temp))+
  geom_boxplot()

# two plots: line(average temperature difference in each month) +
# bar_char: top 1 detail in each event type

data %>%
  group_by(month) %>%
  summarise(diff = median(diff.temp)) %>%
  ggplot(aes(x=month,y=diff,group=1))+
  geom_line(color='dark grey') +
  theme_minimal()

t <- data %>%
  group_by(month,event.type,detail) %>%
  summarise(count = n()) %>%
  arrange(desc(count), .by_group = TRUE) %>%
  top_n(1,wt=count) 

t %>%
  ggplot(aes(x=month,y=count,fill=detail)) +
  geom_bar(stat='identity',position = 'dodge')
