library(tidyverse)
# library(cowplot)
# library(htmltab)
# library(bizdays)
library(timeDate)
library(lubridate)
# library(beepr)

library(RPostgres)
wrds <- dbConnect(Postgres(),
                  host='wrds-pgdata.wharton.upenn.edu',
                  port=9737,
                  dbname='wrds',
                  sslmode='require',
                  user='ryanc196')

y = 2001

print(paste0("start - ", y))
options_df =
  dbSendQuery(wrds,
              paste0("select * from optionm.opprcd", y)) %>%
  dbFetch(n = -1) %>%
  as_tibble()
print(paste0("end - ", y))

options_df

eval(parse(text = paste0("options_all_df_",y," = options_df")))

eval(parse(text = paste0("save(options_all_df_",y,", file = \'./results/options_all_df_", y,".RData\')")))

#eval(parse(text = paste0("write_csv(options_df_orig, \'./results/options_df_", y,".csv\')")))