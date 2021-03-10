library(dslabs)
library(lubridate)
library(tidyverse)
options(digits = 3)    # 3 significant digits

data(brexit_polls)

brexit_polls %>% mutate(month = month(brexit_polls$startdate)) %>% filter(month ==4)

brexit_polls %>% mutate(week = round_date(enddate, unit = "week")) %>% filter(week == "2016-06-12")

brexit_polls %>% mutate(wkday = weekdays(enddate)) %>% group_by(wkday) %>% summarize(n = n()) %>% arrange(n)

data(movielens)

movielens %>% mutate(timestamp = as_datetime(timestamp), year = year(timestamp)) %>%
  group_by(year) %>% summarise(n = n()) %>% arrange(-n)

movielens %>% mutate(timestamp = as_datetime(timestamp), hour = hour(timestamp)) %>%
  group_by(hour) %>% summarise(n = n()) %>% arrange(-n)

install.packages('textdata')
library(tidyverse)
library(gutenbergr)
library(tidytext)
options(digits = 3)

gutenberg_metadata %>%
  filter(str_detect(gutenberg_metadata$title,"Pride and Prejudice"))

gutenberg_works(title == 'Pride and Prejudice')

words <- gutenberg_download(1342) %>% unnest_tokens(word,text)

df <-  words %>% anti_join(stop_words, by = 'word')
dig <- words %>% anti_join(stop_words, by = 'word') %>% 
  filter(!str_detect(word,'\\d+'))

words_only <- words %>% anti_join(stop_words, by = 'word') %>% 
filter(!str_detect(word,'\\d+'))
 words_only %>% count(word, sort = T) %>%filter(n > 100)
 
afinn <- get_sentiments("afinn")

afinn_sentiments <- inner_join(afinn, words, by = 'word') 
 
unique(afinn_sentiments$word) %>% length()
 
mean(afinn_sentiments$value > 0)
 
afinn_sentiments %>% filter(value == 4) %>% nrow()

install.packages('pdftools')
library(tidyverse)
library(pdftools)
library(ggplot2)
options(digits = 3)

fn <- system.file("extdata", "RD-Mortality-Report_2015-18-180531.pdf", package="dslabs")
# system("cmd.exe", input = paste("start", fn))

txt <- pdf_text(fn)
txt
x <- txt[9] %>% str_split('\n') 
length(x)

s <- x[[1]]
s
typeof(s)
length(s)
s <- str_trim(s)
s[1]

header_index <- str_which(s, '2015') %>% min()
header_index
tmp <- str_split(s[header_index],'\\s+', simplify = T)
month <- tmp[1]
header <- tmp[-1]
month
header

tail_index <- str_which(s, 'Total')
tail_index

n <- str_count(s, '^\\d+$')
n

s <- s[n == 0] %>% .[(header_index+1):(tail_index-1-sum(n))]
s <- str_remove_all(s,'[^\\d\\s]')
s

s <- str_split_fixed(s, "\\s+", n = 6)[,1:5]
tab <- apply(s,2,as.numeric)
tab <- cbind(tab, month)
colnames(tab) <- c('day', header, 'month')
mean(tab[,'2015'])
mean(tab[,'2016'])
mean(tab[1:19,'2017'])
mean(tab[20:30,'2017'])

tab <- as_data_frame(tab) %>% mutate(month = month)
tab <- tab %>% gather(year, deaths, -c(day,month)) %>%
  mutate(deaths = as.numeric(deaths))
tab %>% ggplot(aes(day, deaths, color=year)) + geom_line()
