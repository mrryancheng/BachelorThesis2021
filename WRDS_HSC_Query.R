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
# 
# 
# res <- dbSendQuery(wrds, "SELECT date,dji FROM djones.djdaily")
# data <- dbFetch(res)
# dbClearResult(res)
# 
# save(data, file = "./results/datatest.RData")



# Load Data ---------------------------------------------------------------

# load("/scratch/unisg/earnings/data/secid_df_spx.RData")




# Query -------------------------------------------------------------------

# y = 2000
# options_df_list = lst()
# 
# for (i in 1:nrow(secid_df_spx)){
# 
#   options_df_list[[i]] =
# dbSendQuery(wrds,
#             paste0("select * from optionm.opprcd", y,
#                    " where secid = ", secid_df_spx$secid[i])) %>%
# dbFetch(n = -1) %>%
# as_tibble()
# 
#   print(paste(y, "-", i))
# }
# 
# options_df_orig = bind_rows(options_df_list)
# 
# rm(options_df_list)
# 
# eval(parse(text = paste0("save(options_df_orig, file = \'./results/options_df_", y,".RData\')")))
# eval(parse(text = paste0("write_csv(options_df_orig, \'./results/options_df_", y,".csv\')")))
  




# secprd_sp500_orig = dbSendQuery(wrds, 
#   paste0("select * from optionm.secprd where secid = ", paste(secid_df_spx$secid,
#                                           collapse = " or secid = "))) %>%
#   dbFetch(n = -1) %>%
#   as_tibble()
# 
# save(secprd_sp500_orig, file = "./results/secprd_sp500_orig.RData")
# write_csv(secprd_sp500_orig, "./results/secprd_sp500_orig.csv")

# secprd_all_orig = dbSendQuery(wrds, "select * from optionm.secprd") %>%
#   dbFetch(n = -1) %>%
#   as_tibble()
# 
# save(secprd_all_orig, file = "./results/secprd_all_orig.RData")
# write_csv(secprd_all_orig, "./results/secprd_all_orig.csv")



# options_df_list = lst()

# for (y in 2019:2020){
#   eval(parse(text = paste0("load(\'./results/changed/options_df_", y, ".RData\')")))
#   
#   #eval(parse(text = paste0("options_df_", y, " = options_df_orig")))
#   #eval(parse(text = paste0("save(options_df_", y, ", file = \'./results/changed/options_df_", y,".RData\')")))
#   
#   print(y)
#   eval(parse(text = paste0("options_df_list[[", y, "]] = options_df_", y)))
#   print(options_df_list[[y]])
# }

# options_df_all = bind_rows(options_df_list)
# 
# save(options_df_all, "./results/changed/options_df_all.RData")

# -------------------------------------------------------------------------


# load("./data/earnings_df.RData")
# 
# earnings_df = earnings_df %>%
#   mutate(before_year = year(weekday_before),
#          after_year  = year(weekday_after))
# 
# options_list_orig = lst()
# x = 0
# 
# for (i in 1:nrow(earnings_df)){
#     
#   x=x+1
#   options_list_orig[[x]] = dbSendQuery(wrds,
#     paste0("select * from optionm.opprcd", earnings_df$before_year[i],"
#     where secid = ", earnings_df$secid[i],"
#     and date = \'", earnings_df$weekday_before[i], "\'")) %>%
#       dbFetch(n = -1) %>%
#       as_tibble()
#   
#   x=x+1
#   options_list_orig[[x]] = dbSendQuery(wrds,
#     paste0("select * from optionm.opprcd", earnings_df$after_year[i],"
#     where secid = ", earnings_df$secid[i],"
#     and date = \'", earnings_df$weekday_after[i], "\'")) %>%
#       dbFetch(n = -1) %>%
#       as_tibble()
#   
#   print(paste(i, "-", x))
# }
# 
# options_df_orig = bind_rows(options_list_orig)
# 
# save(options_df_orig, file = "./data/options_df_orig_lst.RData")


# -------------------------------------------------------------------------

# load("./data/earnings_df.RData")
# load("./data/options_df_orig.RData")
# 
# earnings_options_df = tibble(b_call_bid       = numeric(),
#                              b_call_offer     = numeric(),
#                              b_call_strike    = numeric(),
#                              b_call_exdate    = date(),
#                              b_call_volume    = numeric(),
#                              b_call_opint     = numeric(),
#                              b_call_iv        = numeric(),
#                              b_call_optionid  = numeric(),
#                              
#                              b_put_bid       = numeric(),
#                              b_put_offer     = numeric(),
#                              b_put_strike    = numeric(),
#                              b_put_exdate    = date(),
#                              b_put_volume    = numeric(),
#                              b_put_opint     = numeric(),
#                              b_put_iv        = numeric(),
#                              b_put_optionid  = numeric(),
#                              
#                              a_call_bid       = numeric(),
#                              a_call_offer     = numeric(),
#                              a_call_strike    = numeric(),
#                              a_call_exdate    = date(),
#                              a_call_volume    = numeric(),
#                              a_call_opint     = numeric(),
#                              a_call_iv        = numeric(),
#                              a_call_optionid  = numeric(),
#                              
#                              a_put_bid       = numeric(),
#                              a_put_offer     = numeric(),
#                              a_put_strike    = numeric(),
#                              a_put_exdate    = date(),
#                              a_put_volume    = numeric(),
#                              a_put_opint     = numeric(),
#                              a_put_iv        = numeric(),
#                              a_put_optionid  = numeric()) %>%
#   
#   mutate_at(vars(b_call_exdate,
#                  b_put_exdate,
#                  a_call_exdate,
#                  a_put_exdate),
#             list(date))
# 
# for(i in 1:nrow(earnings_df)){
#   
#   b_call = options_df_orig %>% 
#     filter(secid == earnings_df$secid[i]) %>%
#     filter(date == earnings_df$weekday_before[i]) %>%
#     filter(exdate > earnings_df$weekday_after[i]) %>%
#     filter(cp_flag == "C") %>%
#     filter(strike_price/1000 > earnings_df$close_weekday_before[i]) %>%
#     slice(which.min(abs(strike_price/1000 - earnings_df$close_weekday_before[i]))) %>% #test obtain min exdate
#     slice(which.min(exdate)) %>% select(best_bid, 
#                                  best_offer, 
#                                  strike_price, 
#                                  exdate, 
#                                  volume, 
#                                  open_interest, 
#                                  impl_volatility, 
#                                  optionid)
#   
#   b_put = options_df_orig %>% 
#     filter(secid == earnings_df$secid[i]) %>%
#     filter(date == earnings_df$weekday_before[i]) %>%
#     filter(exdate > earnings_df$weekday_after[i]) %>%
#     filter(cp_flag == "P") %>%
#     filter(strike_price/1000 < earnings_df$close_weekday_before[i]) %>%
#     slice(which.min(abs(strike_price/1000 - earnings_df$close_weekday_before[i]))) %>%
#     slice(which.min(exdate)) %>% select(best_bid, 
#                                  best_offer, 
#                                  strike_price, 
#                                  exdate, 
#                                  volume, 
#                                  open_interest, 
#                                  impl_volatility, 
#                                  optionid)
#   if (nrow(b_call) != 0 && nrow(b_put) != 0){
#     a_call = options_df_orig %>% 
#       filter(optionid == b_call %>% select(optionid) %>% pull(1)) %>%
#       filter(date == earnings_df$weekday_after[i]) %>% select(best_bid, 
#                                                               best_offer, 
#                                                               strike_price, 
#                                                               exdate, 
#                                                               volume, 
#                                                               open_interest, 
#                                                               impl_volatility, 
#                                                               optionid)
#     
#     a_put  = options_df_orig %>% 
#       filter(optionid == b_put %>% select(optionid) %>% pull(1)) %>%
#       filter(date == earnings_df$weekday_after[i]) %>% select(best_bid, 
#                                                               best_offer, 
#                                                               strike_price, 
#                                                               exdate, 
#                                                               volume, 
#                                                               open_interest, 
#                                                               impl_volatility, 
#                                                               optionid)
#   }
#   
#   if (nrow(b_call) != 0 && nrow(b_put) != 0){
#     earnings_options_df[i, 1:8] = b_call 
#     earnings_options_df[i, 9:16] = b_put 
#   }
#   
#   if (nrow(a_call) != 0 && nrow(a_put) != 0){
#     earnings_options_df[i, 17:24] = a_call 
#     earnings_options_df[i, 25:32] = a_put 
#   }
#   print(i)
# }
# 
# save(earnings_options_df, file = "./data/earnings_options_df.RData")

load("./results/sp500_options_closing_prices_2000-2020/options_df_2020.RData")

options_df_2020

dim(options_df_2020)

