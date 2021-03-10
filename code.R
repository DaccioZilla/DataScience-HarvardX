library(tidyverse)
path <- system.file("extdata", package = "dslabs")
list.files(path)

filename <- "murders.csv"
fullpath <- file.path(path, filename)
setwd("Data Wrangling")
file.copy(fullpath, getwd())

url <- "https://archive.ics.uci.edu/ml/machine-learning-databases/breast-cancer-wisconsin/wdbc.data"
read_lines(url, n_max = 3)

dat <- read_csv(url, col_names = F)

co2

library(tidyverse)
library(dslabs)

co2_wide <- data.frame(matrix(co2, ncol = 12, byrow = TRUE)) %>% 
  setNames(1:12) %>%
  mutate(year = as.character(1959:1997))

co2_wide
co2_tidy <- gather(co2_wide,month,co2,-year)
co2_tidy

co2_tidy %>% ggplot(aes(as.numeric(month), co2, color = year)) + geom_line()

data(admissions)

dat <- admissions %>% select(-applicants)
dat

dat_tidy <- spread(dat, gender, admitted)
dat_tidy

tmp <- gather(admissions, key, value, admitted:applicants)
tmp2 <- unite(tmp,column_name, c(key, gender))
tmp2

install.packages("Lahman")
library(Lahman)

top <- Batting %>% 
  filter(yearID == 2016) %>%
  arrange(desc(HR)) %>%    # arrange by descending HR count
  slice(1:10)    # take entries 1-10
top %>% as_tibble()

Master %>% as_tibble()

awards2016 <- AwardsPlayers %>% filter(yearID == 2016) %>% group_by(playerID) %>% summarize(n_awards = n())

top_awards <- left_join(top, awards2016) %>% filter(n_awards > 0)

library(rvest)
url <- "https://web.archive.org/web/20181024132313/http://www.stevetheump.com/Payrolls.htm"
h <- read_html(url)

nodes <- html_nodes(h, "table")
tab1 <- html_table(nodes[[1]])
tab2 <- html_table(nodes[[2]])
tab3 <- html_table(nodes[[3]])
tab4 <- html_table(nodes[[4]])

tab1 <- html_table(nodes[[10]])
tab1 <- tab1[-1,-1]
colnames(tab1) <- c("Team", "Payroll", "Average")
tab2 <- html_table(nodes[[19]])
colnames(tab2) <- c("Team", "Payroll", "Average")
tab2 <- tab2[-1,]

fj <- full_join(tab1,tab2, by = "Team")
dim(fj)


url <- "https://en.wikipedia.org/w/index.php?title=Opinion_polling_for_the_United_Kingdom_European_Union_membership_referendum&oldid=896735054"
tab <- read_html(url) %>% html_nodes("table")

head(html_table(tab[[5]], fill = T))
dim(html_table(tab[[5]], fill = T))
