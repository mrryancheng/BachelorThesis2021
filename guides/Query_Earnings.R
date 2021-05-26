library(tidyverse)

data_library = "optionm"
data_set = "secprd"

#Determine the data libraries available at WRDS:
res <- dbSendQuery(wrds, "select distinct table_schema
                   from information_schema.tables
                   where table_type ='VIEW'
                   or table_type ='FOREIGN TABLE'
                   order by table_schema") %>%
  dbFetch(n=-1) %>%
  as_tibble()
res

#Determine the datasets within a given library:
res <- dbSendQuery(wrds, paste0("select distinct table_name
                   from information_schema.columns
                   where table_schema='",data_library,"'
                   order by table_name")) %>%
  dbFetch(n=-1) %>%
  as_tibble()
res

#Determine the variables (column headers) within a given dataset:
res <- dbSendQuery(wrds, paste0("select column_name
                   from information_schema.columns
                   where table_schema='",data_library,"'
                   and table_name='",data_set,"'
                   order by column_name")) %>%
  dbFetch(n=-1) %>%
  as_tibble()
res


res <- dbSendQuery(wrds, paste0("select ","*"," from ",paste0(data_library,".",data_set))) %>%
  dbFetch(n=100) %>%
  as_tibble()
res

res <- dbSendQuery(wrds, paste0("select ","*"," from ",paste0(data_library,".",data_set, " where secid = 106581"))) %>%
  dbFetch(n=-1) %>%
  as_tibble()
res








ggplot(data = res) +
  geom_point(aes(x=as.Date(date,'%Y-%m-%d'),
                 y=close), 
             size = 0.1) +
  xlab("date") +
  ylab("price")
