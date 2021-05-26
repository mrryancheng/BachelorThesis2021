
test = secprd_sp500_orig %>% 
  filter(secid == loop_sec_id) %>%
  filter(date == loop_weekday_before)
test2 = read_csv("wheql3leabe3hm71.csv")




for(i in 1:nrow(earnings_dates_df)){
  
  loop_sec_id         = earnings_dates_df$secid[i]
  loop_weekday_before = earnings_dates_df$weekday_before[i]
  loop_weekday_after  = earnings_dates_df$weekday_after[i]
  
  earnings_dates_df$close_weekday_before[i] = secprd_sp500_orig %>% 
    filter(secid == loop_sec_id) %>%
    filter(date == loop_weekday_before) %>%
    select(close) %>%
    pull(1)
  
  earnings_dates_df$open_weekday_after[i]   = secprd_sp500_orig %>% 
    filter(secid == loop_sec_id) %>%
    filter(date == loop_weekday_after) %>%
    select(open) %>%
    pull(1)
  
}

cwb = function(i){
  secprd_sp500_orig %>% 
    filter(secid == earnings_dates_df$secid[i]) %>%
    filter(date == earnings_dates_df$weekday_before[i]) %>%
    select(close) %>%
    pull(1)
}

test = unlist(map(348:350, cwb))

# earnings_dates_df$close_weekday_before[i] = dbSendQuery(wrds, paste0(
#                        "select close from optionm.secprd where 
#                        secid = ", sec_id_loop ," and 
#                        date = \'", weekday_before_loop, "\'")) %>% 
#                        dbFetch(n = -1) %>% pull(1)
# 
# earnings_dates_df$open_weekday_after[i]   = dbSendQuery(wrds, paste0(
#                        "select open from optionm.secprd where 
#                        secid = ", sec_id_loop ," and 
#                        date = \'", weekday_after_loop, "\'")) %>% 
#                        dbFetch(n = -1) %>% pull(1)


test = earnings_returns %>%
  filter(month(report_date) == 1) %>%
  filter(day(report_date) == 2)








test = options_df_orig %>% 
  filter(secid == earnings_returns_options$secid[i]) %>%
  filter(date == earnings_returns_options$weekday_before[i]) %>%
  filter(cp_flag == "P") %>%
  filter(strike_price/1000 < earnings_returns_options$close_weekday_before[i]) %>%
  slice_min(abs(strike_price/1000 - earnings_returns_options$close_weekday_before[i])) %>%
  select(best_bid, best_offer, strike_price, open_interest, exdate)


test1 = earnings_returns_options %>%
  slice(which((earnings_returns_options$close_weekday_before - trunc(earnings_returns_options$close_weekday_before)) < 0.0000001))

test2 = earnings_returns_options %>%
  slice(which(earnings_returns_options$b_call_exdate != earnings_returns_options$b_put_exdate))


test3 = earnings_returns_options %>% mutate(date_diff = report_date-b_call_exdate)

test4 = options_df_orig %>% filter(optionid == 10355668)

test5 = dbSendQuery(wrds, "select * from optionm.opprcd2000 where secid = 107616 and date = '2000-01-25'") %>%
  dbFetch(n = -1) %>%
  as_tibble() %>%
  filter(exdate == min(exdate))

res <- dbSendQuery(wrds, "select * from optionm.opprcd2000 where optionid = 10355668") %>%
  dbFetch(n=100) %>%
  as_tibble()
res

earnings_df = earnings_df%>% select(-(returns))

earnings_df$close_weekday_before = abs(earnings_df$close_weekday_before)
earnings_df$close_weekday_after = abs(earnings_df$close_weekday_after)

# -------------------------------------------------------------------------



tic()
for(i in 1:10){
  
  b_call = options_df_orig %>% 
    filter(secid == earnings_df$secid[i]) %>%
    filter(date == earnings_df$weekday_before[i]) %>%
    filter(exdate > earnings_df$weekday_after[i]) %>%
    filter(cp_flag == "C") %>%
    filter(strike_price/1000 > earnings_df$close_weekday_before[i]) %>%
    slice_min(abs(strike_price/1000 - earnings_df$close_weekday_before[i])) %>% #test obtain min exdate
    slice_min(exdate) %>% select(best_bid, 
                                 best_offer, 
                                 strike_price, 
                                 exdate, 
                                 volume, 
                                 open_interest, 
                                 impl_volatility, 
                                 optionid)
  
  b_put = options_df_orig %>% 
    filter(secid == earnings_df$secid[i]) %>%
    filter(date == earnings_df$weekday_before[i]) %>%
    filter(exdate > earnings_df$weekday_after[i]) %>%
    filter(cp_flag == "P") %>%
    filter(strike_price/1000 < earnings_df$close_weekday_before[i]) %>%
    slice_min(abs(strike_price/1000 - earnings_df$close_weekday_before[i])) %>%
    slice_min(exdate) %>% select(best_bid, 
                                 best_offer, 
                                 strike_price, 
                                 exdate, 
                                 volume, 
                                 open_interest, 
                                 impl_volatility, 
                                 optionid)
  
  a_call = options_df_orig %>% 
    filter(optionid == b_call %>% select(optionid) %>% pull(1)) %>%
    filter(date == earnings_df$weekday_after[i]) %>% select(best_bid, 
                                                            best_offer, 
                                                            strike_price, 
                                                            exdate, 
                                                            volume, 
                                                            open_interest, 
                                                            impl_volatility, 
                                                            optionid)
  
  a_put  = options_df_orig %>% 
    filter(optionid == b_put %>% select(optionid) %>% pull(1)) %>%
    filter(date == earnings_df$weekday_after[i]) %>% select(best_bid, 
                                                            best_offer, 
                                                            strike_price, 
                                                            exdate, 
                                                            volume, 
                                                            open_interest, 
                                                            impl_volatility, 
                                                            optionid)
  
  if (nrow(b_call) != 0 && nrow(b_put) != 0){
    
    for (j in 1:8){
      earnings_options_df[i, j]   = b_call[1, j]
      earnings_options_df[i, j+8] = b_put[1, j]
    }
    
  }
  
  if (nrow(a_call) != 0 && nrow(a_put) != 0){
    for (j in 1:8){
      earnings_options_df[i, j+16] = a_call[1, j]
      earnings_options_df[i, j+24] = a_put[1, j]
    }
    
  }
  print(i)
}
toc()



# -------------------------------------------------------------------------



load("./data/earnings_df.RData")
load("./data/options_df_orig.RData")

earnings_df_test = earnings_df %>%
  
  mutate(b_call_price     = NA,
         b_put_price      = NA,
         b_call_strike    = NA,
         b_put_strike     = NA,
         b_call_opint     = NA,
         b_put_opint      = NA,
         b_call_exdate    = NA,
         b_put_exdate     = NA,
         b_call_optionid  = NA,
         b_put_optionid   = NA,
         
         a_call_price     = NA,
         a_put_price      = NA,
         a_call_strike    = NA,
         a_put_strike     = NA,
         a_call_opint     = NA,
         a_put_opint      = NA,
         a_call_exdate    = NA,
         a_put_exdate     = NA,
         a_call_optionid  = NA,
         a_put_optionid   = NA) %>%
  
  mutate_at(vars(b_call_exdate,
                 b_put_exdate,
                 a_call_exdate,
                 a_put_exdate),
            list(date))


for(i in 1:nrow(earnings_df_options)){
  
  b_call = options_df_orig %>% 
    filter(secid == earnings_df_options$secid[i]) %>%
    filter(date == earnings_df_options$weekday_before[i]) %>%
    filter(exdate > earnings_df_options$weekday_after[i]) %>%
    filter(cp_flag == "C") %>%
    filter(strike_price/1000 > earnings_df_options$close_weekday_before[i]) %>%
    slice_min(abs(strike_price/1000 - earnings_df_options$close_weekday_before[i])) %>% #test obtain min exdate
    slice_min(exdate)
  
  b_put = options_df_orig %>% 
    filter(secid == earnings_df_options$secid[i]) %>%
    filter(date == earnings_df_options$weekday_before[i]) %>%
    filter(exdate > earnings_df_options$weekday_after[i]) %>%
    filter(cp_flag == "P") %>%
    filter(strike_price/1000 < earnings_df_options$close_weekday_before[i]) %>%
    slice_min(abs(strike_price/1000 - earnings_df_options$close_weekday_before[i])) %>%
    slice_min(exdate)
  
  a_call = options_df_orig %>% 
    filter(optionid == b_call %>% select(optionid) %>% pull(1)) %>%
    filter(date == earnings_df_options$weekday_after[i])
  
  a_put  = options_df_orig %>% 
    filter(optionid == b_put %>% select(optionid) %>% pull(1)) %>%
    filter(date == earnings_df_options$weekday_after[i])
  
  if (nrow(b_call) != 0 && nrow(b_put) != 0){
    
    earnings_df_options$b_call_price[i]   = b_call %>% select(best_bid, best_offer) %>% rowMeans()
    earnings_df_options$b_call_strike[i]  = b_call %>% select(strike_price)  %>% pull(1)
    earnings_df_options$b_call_opint[i]   = b_call %>% select(open_interest) %>% pull(1)
    earnings_df_options$b_call_exdate[i]  = b_call %>% select(exdate) %>% pull(1)
    earnings_df_options$b_call_optionid[i]= b_call %>% select(optionid) %>% pull(1)
    
    earnings_df_options$b_put_price[i]    = b_put  %>% select(best_bid, best_offer) %>% rowMeans()
    earnings_df_options$b_put_strike[i]   = b_put  %>% select(strike_price)  %>% pull(1)
    earnings_df_options$b_put_opint[i]    = b_put  %>% select(open_interest) %>% pull(1)
    earnings_df_options$b_put_exdate[i]   = b_put  %>% select(exdate) %>% pull(1)
    earnings_df_options$b_put_optionid[i] = b_put  %>% select(optionid) %>% pull(1)
    
  }
  
  if (nrow(a_call) != 0 && nrow(a_put) != 0){
    
    earnings_df_options$a_call_price[i]   = a_call %>% select(best_bid, best_offer) %>% rowMeans()
    earnings_df_options$a_call_strike[i]  = a_call %>% select(strike_price)  %>% pull(1)
    earnings_df_options$a_call_opint[i]   = a_call %>% select(open_interest) %>% pull(1)
    earnings_df_options$a_call_exdate[i]  = a_call %>% select(exdate) %>% pull(1)
    earnings_df_options$a_call_optionid[i]= a_call %>% select(optionid) %>% pull(1)
    
    earnings_df_options$a_put_price[i]    = a_put  %>% select(best_bid, best_offer) %>% rowMeans()
    earnings_df_options$a_put_strike[i]   = a_put  %>% select(strike_price)  %>% pull(1)
    earnings_df_options$a_put_opint[i]    = a_put  %>% select(open_interest) %>% pull(1)
    earnings_df_options$a_put_exdate[i]   = a_put  %>% select(exdate) %>% pull(1)
    earnings_df_options$a_put_optionid[i] = a_put  %>% select(optionid) %>% pull(1)
    
  }
  print(i)
}

save(earnings_df_options, file = "./data/earnings_df_options.RData")


# -------------------------------------------------------------------------

earnings_options_df = tibble(b_call_bid       = numeric(),
                             b_call_offer     = numeric(),
                             b_call_strike    = numeric(),
                             b_call_exdate    = date(),
                             b_call_volume    = numeric(),
                             b_call_opint     = numeric(),
                             b_call_iv        = numeric(),
                             b_call_optionid  = numeric(),
                             
                             b_put_bid       = numeric(),
                             b_put_offer     = numeric(),
                             b_put_strike    = numeric(),
                             b_put_exdate    = date(),
                             b_put_volume    = numeric(),
                             b_put_opint     = numeric(),
                             b_put_iv        = numeric(),
                             b_put_optionid  = numeric(),
                             
                             a_call_bid       = numeric(),
                             a_call_offer     = numeric(),
                             a_call_strike    = numeric(),
                             a_call_exdate    = date(),
                             a_call_volume    = numeric(),
                             a_call_opint     = numeric(),
                             a_call_iv        = numeric(),
                             a_call_optionid  = numeric(),
                             
                             a_put_bid       = numeric(),
                             a_put_offer     = numeric(),
                             a_put_strike    = numeric(),
                             a_put_exdate    = date(),
                             a_put_volume    = numeric(),
                             a_put_opint     = numeric(),
                             a_put_iv        = numeric(),
                             a_put_optionid  = numeric()) %>%
  
  mutate_at(vars(b_call_exdate,
                 b_put_exdate,
                 a_call_exdate,
                 a_put_exdate),
            list(date))

earnings_options_df = bind_rows(earnings_options_df, 
                                as_tibble(matrix(NA, nrow = nrow(earnings_df), ncol = 32)))

tic()
for(i in 1:10){
  
  b_call = options_df_orig %>% 
    filter(secid == earnings_df$secid[i]) %>%
    filter(date == earnings_df$weekday_before[i]) %>%
    filter(exdate > earnings_df$weekday_after[i]) %>%
    filter(cp_flag == "C") %>%
    filter(strike_price/1000 > earnings_df$close_weekday_before[i]) %>%
    slice_min(abs(strike_price/1000 - earnings_df$close_weekday_before[i])) %>% #test obtain min exdate
    slice_min(exdate) %>% select(best_bid, 
                                 best_offer, 
                                 strike_price, 
                                 exdate, 
                                 volume, 
                                 open_interest, 
                                 impl_volatility, 
                                 optionid)
  
  b_put = options_df_orig %>% 
    filter(secid == earnings_df$secid[i]) %>%
    filter(date == earnings_df$weekday_before[i]) %>%
    filter(exdate > earnings_df$weekday_after[i]) %>%
    filter(cp_flag == "P") %>%
    filter(strike_price/1000 < earnings_df$close_weekday_before[i]) %>%
    slice_min(abs(strike_price/1000 - earnings_df$close_weekday_before[i])) %>%
    slice_min(exdate) %>% select(best_bid, 
                                 best_offer, 
                                 strike_price, 
                                 exdate, 
                                 volume, 
                                 open_interest, 
                                 impl_volatility, 
                                 optionid)
  
  a_call = options_df_orig %>% 
    filter(optionid == b_call %>% select(optionid) %>% pull(1)) %>%
    filter(date == earnings_df$weekday_after[i]) %>% select(best_bid, 
                                                            best_offer, 
                                                            strike_price, 
                                                            exdate, 
                                                            volume, 
                                                            open_interest, 
                                                            impl_volatility, 
                                                            optionid)
  
  a_put  = options_df_orig %>% 
    filter(optionid == b_put %>% select(optionid) %>% pull(1)) %>%
    filter(date == earnings_df$weekday_after[i]) %>% select(best_bid, 
                                                            best_offer, 
                                                            strike_price, 
                                                            exdate, 
                                                            volume, 
                                                            open_interest, 
                                                            impl_volatility, 
                                                            optionid)
  
  if (nrow(b_call) != 0 && nrow(b_put) != 0){
    
    earnings_options_df$b_call_bid[i]      = b_call %>% select(best_bid) %>% pull(1)
    earnings_options_df$b_call_offer[i]    = b_call %>% select(best_offer)  %>% pull(1)
    earnings_options_df$b_call_strike[i]   = b_call %>% select(strike_price) %>% pull(1)
    earnings_options_df$b_call_exdate[i]   = b_call %>% select(exdate) %>% pull(1)
    earnings_options_df$b_call_volume[i]   = b_call %>% select(volume) %>% pull(1)
    earnings_options_df$b_call_opint[i]    = b_call %>% select(open_interest)  %>% pull(1)
    earnings_options_df$b_call_iv[i]       = b_call %>% select(impl_volatility) %>% pull(1)
    earnings_options_df$b_call_optionid[i] = b_call %>% select(optionid) %>% pull(1)
    
    earnings_options_df$b_put_bid[i]      = b_put %>% select(best_bid) %>% pull(1)
    earnings_options_df$b_put_offer[i]    = b_put %>% select(best_offer)  %>% pull(1)
    earnings_options_df$b_put_strike[i]   = b_put %>% select(strike_price) %>% pull(1)
    earnings_options_df$b_put_exdate[i]   = b_put %>% select(exdate) %>% pull(1)
    earnings_options_df$b_put_volume[i]   = b_put %>% select(volume) %>% pull(1)
    earnings_options_df$b_put_opint[i]    = b_put %>% select(open_interest)  %>% pull(1)
    earnings_options_df$b_put_iv[i]       = b_put %>% select(impl_volatility) %>% pull(1)
    earnings_options_df$b_put_optionid[i] = b_put %>% select(optionid) %>% pull(1)
    
  }
  
  if (nrow(a_call) != 0 && nrow(a_put) != 0){
    
    earnings_options_df$a_call_bid[i]      = a_call %>% select(best_bid) %>% pull(1)
    earnings_options_df$a_call_offer[i]    = a_call %>% select(best_offer)  %>% pull(1)
    earnings_options_df$a_call_strike[i]   = a_call %>% select(strike_price) %>% pull(1)
    earnings_options_df$a_call_exdate[i]   = a_call %>% select(exdate) %>% pull(1)
    earnings_options_df$a_call_volume[i]   = a_call %>% select(volume) %>% pull(1)
    earnings_options_df$a_call_opint[i]    = a_call %>% select(open_interest)  %>% pull(1)
    earnings_options_df$a_call_iv[i]       = a_call %>% select(impl_volatility) %>% pull(1)
    earnings_options_df$a_call_optionid[i] = a_call %>% select(optionid) %>% pull(1)
    
    earnings_options_df$a_put_bid[i]      = a_put %>% select(best_bid) %>% pull(1)
    earnings_options_df$a_put_offer[i]    = a_put %>% select(best_offer)  %>% pull(1)
    earnings_options_df$a_put_strike[i]   = a_put %>% select(strike_price) %>% pull(1)
    earnings_options_df$a_put_exdate[i]   = a_put %>% select(exdate) %>% pull(1)
    earnings_options_df$a_put_volume[i]   = a_put %>% select(volume) %>% pull(1)
    earnings_options_df$a_put_opint[i]    = a_put %>% select(open_interest)  %>% pull(1)
    earnings_options_df$a_put_iv[i]       = a_put %>% select(impl_volatility) %>% pull(1)
    earnings_options_df$a_put_optionid[i] = a_put %>% select(optionid) %>% pull(1)
    
  }
  print(i)
}
toc()

save(earnings_options_df, file = "./data/earnings_options_df.RData")



# -------------------------------------------------------------------------



#Cl(SPY) %>%
earnings_timeseries_xts %>%
  Return.calculate() %>%
  Return.portfolio() %>%
  charts.PerformanceSummary(main = 'Portfolio Performance Summary')


chart.CumReturns(earnings_timeseries_xts)

SPY_change = SPY_close %>% 
  as_tibble() %>%
  transmute(change = (SPY.Close - lag(SPY.Close)) / lag(SPY.Close)) %>%
  pull()

SPY_close$SPY.Close = SPY_change

SharpeRatio(SPY_close,
            annualize = TRUE)

plot(Cl(SPY))




plot(earnings_final_df$cumsum_profit)
sum(earnings_final_df$rel_profit)
mean(earnings_final_df$rel_profit[earnings_final_df$rel_profit>0])
mean(earnings_final_df$rel_profit[earnings_final_df$rel_profit<0])

median(earnings_final_df$b_call_volume, na.rm = TRUE) #53
median(earnings_final_df$a_call_volume, na.rm = TRUE) #33
median(earnings_final_df$b_put_volume, na.rm = TRUE) #30
median(earnings_final_df$a_put_volume, na.rm = TRUE) #17
median(earnings_final_df$b_call_opint, na.rm = TRUE) #456
median(earnings_final_df$a_call_opint, na.rm = TRUE) #619
median(earnings_final_df$b_put_opint, na.rm = TRUE) #276
median(earnings_final_df$a_put_opint, na.rm = TRUE) #411

ggplot(data = earnings_final_df) +
  geom_point(aes(x = report_date,
                 y = cumsum_profit),
             fill = "black") +
  xlab("Date") +
  ylab("Profit")


ggplot(data = earnings_final_df) +
  geom_histogram(aes(x = abs_profit), binwidth = 0.05, fill = "black") +
  xlim(-6,6) +
  xlab("Profit per trade") +
  ylab("Count")

ggplot(data = earnings_final_df) +
  geom_histogram(aes(x = iv_change), binwidth = 0.03, fill = "black") +
  xlim(-1,1) +
  xlab("Profit per trade") +
  ylab("Count")

ggplot(data  = earnings_final_df, 
       aes(x = abs_b_strangle,
           y = abs_returns)) +
  geom_point(size = 0.01) +
  geom_smooth(method = lm) +
  geom_abline(slope = 1, intercept = 0)+
  xlim(0, 5) +
  ylim(0, 5) +
  theme_minimal_grid(12)

ggplot(data  = earnings_final_df, 
       aes(x = abs_profit,
           y = abs_returns)) +
  geom_point(size = 0.01) +
  geom_smooth(method = lm) +
  geom_abline(slope = 1, intercept = 0)+
  # xlim(0, 5) +
  # ylim(0, 5) +
  theme_minimal_grid(12)

lmHeight2 = lm(abs_returns ~ . -ticker -secid -cusip, data = earnings_final_df)
lmHeight2 = lm(abs_returns ~ abs_b_strangle + close_weekday_before, data = earnings_final_df)
lmHeight2 = lm(abs_returns ~ abs_b_strangle + total_volume + total_opint + b_iv, data = earnings_final_df) #Create a linear regression with two variables
summary(lmHeight2) #Review the results

plot(lmHeight2$residuals, pch = 16, col = "red")
plot(cooks.distance(lmHeight2), pch = 16, col = "blue") #Plot the Cooks Distances.
avPlots(lmHeight2, ~ abs_b_strangle)





ggplot(data  = test, 
       aes(x = b_call_iv,
           y = b_put_iv)) +
  geom_point(size = 0.01) +
  geom_smooth(method = lm) +
  # xlim(-200, 200) +
  #ylim(-1, 1) +
  theme_minimal_grid(12)

ggplot(data = test2) +
  geom_histogram(aes(x = profit), binwidth = 0.1, fill = "black") +
  xlim(-6,6) +
  xlab("Profit per share") +
  ylab("Count")

hist(test2$iv_change, breaks = 10000, xlim = c(-0.3,0.3))
hist(test2$profit, breaks = 10000, xlim = c(-6,6))
hist(test2$relative_profit, breaks = 1000, xlim = c(-6,6))

sum(test2$relative_profit, na.rm = TRUE)



fit1 <- lm(returns ~ strangle, data = test2)
summary(fit1)






