rm(list=ls())



# Load Libraries ----------------------------------------------------------

library(tidyverse)
library(cowplot)
library(htmltab)
library(bizdays)
library(timeDate)
library(lubridate)
library(beepr)
library(car)
library(PerformanceAnalytics)
library(tidyquant)
library(tsbox)
library(stargazer)

theme_set(theme_cowplot())



# Load more Libraries -----------------------------------------------------

#devtools::install_github("collectivemedia/tictoc")
library(tictoc)

library(RPostgres)
wrds <- dbConnect(Postgres(),
                  host='wrds-pgdata.wharton.upenn.edu',
                  port=9737,
                  dbname='wrds',
                  sslmode='require',
                  user='ryanc196')



# Load earnings dates -----------------------------------------------------

# earnings_df_orig = read_csv("./data/earnings_sp500_20y.csv", col_names = TRUE) 
# DD only available 2021 onwards

earnings_df = earnings_df_orig %>% 
  mutate(ticker  = replace(ticker, which(ticker == "BRK-B"), "BRK")) %>%
  mutate(ticker  = replace(ticker, which(ticker == "BF-B" ), "BF" )) %>%
  filter(startdatetime >= as.Date("2000-01-01") & 
         startdatetime <= as.Date("2020-12-31"))



# Load list of S&P500 symbols ---------------------------------------------

# sp500_url = 'https://en.wikipedia.org/wiki/List_of_S%26P_500_companies'
# 
# list_sp500_df_orig = htmltab(doc   = sp500_url, 
#                              which = 1         ) %>% as_tibble()
# 
# write_csv(list_sp500_df_orig, "./data/list_sp500_df_orig.csv")

list_sp500_df_orig = read_csv("./data/list_sp500_df_orig.csv", col_names = TRUE)
list_spx_df = list_sp500_df_orig %>%
  filter(Symbol != "FOX"  ) %>% #FOXA
  filter(Symbol != "NWS"  ) %>% #NWSA
  filter(Symbol != "UA"   ) %>% #UAA
  filter(Symbol != "GOOG" ) %>% #GOOGL
  filter(Symbol != "DISCK") %>% #DISCA
  mutate(Symbol  = replace(Symbol, which(Symbol == "BRK.B"), "BRK")) %>%
  mutate(Symbol  = replace(Symbol, which(Symbol == "BF.B" ), "BF" ))



# Load list of S&P100 symbols ---------------------------------------------

# sp100_url = 'https://en.wikipedia.org/wiki/S%26P_100'
# 
# list_sp100_df_orig = htmltab(doc   = sp100_url,
#                              which = 3         ) %>% as_tibble()
# 
# write_csv(list_sp100_df_orig, "./data/list_sp100_df_orig.csv")

# list_sp100_df_orig = read_csv("./data/list_sp100_df_orig.csv", col_names = TRUE)
# list_spx_df = list_sp100_df_orig %>%
#   filter(Symbol != "GOOG" ) %>% #GOOGL
#   mutate(Symbol  = replace(Symbol, which(Symbol == "BRK.B"), "BRK"))



# Load TICKER to SECID conversion table -----------------------------------

secid_df_orig = dbSendQuery(wrds,"select * from optionm.secnmd") %>% 
  dbFetch(n = -1) %>%
  as_tibble()

secid_df = secid_df_orig %>%
  select(ticker, secid, cusip, class, effect_date, issuer) %>%
  arrange(desc(effect_date)) 

secid_df = secid_df %>%
  slice(-(which(secid_df$secid == as.integer(102378)))) #BF Class A

secid_df_spx = tibble()
for (i in list_spx_df %>% pull(Symbol)){
  secid_df_spx = bind_rows(secid_df_spx, filter(secid_df, ticker == i)[1, ])
}



# Manual Inspection of Company Names --------------------------------------

# test_list_spx_df = list_spx_df$Security %>% tolower() %>% substr(1,5)
# test_sec_id_spx = sec_id_spx$issuer %>% tolower() %>% substr(1,5)
# 
# test_company_name_match = bind_cols(sec_id_spx, list_spx_df) %>%
#   select(Security, issuer) %>%
#   mutate(matching = test_list_spx_df == test_sec_id_spx) %>%
#   arrange(matching) #check all false



# Attach SECID to Earnings Dates DF ---------------------------------------

earnings_df = 
  left_join(earnings_df, 
            secid_df_spx %>% select(ticker, secid, cusip), 
            by = "ticker") %>%
  select(ticker, 
         secid, 
         cusip,
         report_date, 
         epssurprisepct)

#trading days calendar
load_rmetrics_calendars(2000:2020)
cal = calendars()[["Rmetrics/NYSE"]]

earnings_df = earnings_df %>% 
  mutate(
    weekday_before = adjust.previous(earnings_df$report_date - 1, cal), 
    weekday_after  = adjust.next    (earnings_df$report_date + 1, cal),
    close_weekday_before = NA,
    close_weekday_after  = NA)



# Download S&P500 price data ----------------------------------------------

# secprd_sp500_orig = dbSendQuery(wrds, paste0("select * from optionm.secprd where 
#                          secid = ", paste(sec_id_sp500$secid, 
#                                           collapse = " or secid = "))) %>% 
#   dbFetch(n = -1) %>%
#   as_tibble()
# 
# write_csv(secprd_sp500_orig, "./data/secprd_sp500_orig.csv")



secprd_sp500_orig = read_csv("./data/secprd_sp500_orig.csv", col_names = TRUE)

tic("Price Before After Loop") #45 min
for(i in 1:nrow(earnings_df)){

  close_weekday_before = secprd_sp500_orig %>%
    filter(secid == earnings_df$secid[i]) %>%
    filter(date == earnings_df$weekday_before[i]) %>%
    select(close) %>%
    pull(1) %>%
    abs() # odd error with BIO(102147) and STZ(103369) in secprd database
          # leads to negative prices

  close_weekday_after = secprd_sp500_orig %>%
    filter(secid == earnings_df$secid[i]) %>%
    filter(date == earnings_df$weekday_after[i]) %>%
    select(close) %>%
    pull(1) %>%
    abs() # odd error with BIO(102147) and STZ(103369) in secprd database
          # leads to negative prices

  if (length(close_weekday_before) != 0){
    earnings_df$close_weekday_before[i] = close_weekday_before}

  if (length(close_weekday_after) != 0){
    earnings_df$close_weekday_after[i]  = close_weekday_after}
}
toc()
beep()

earnings_df = earnings_df %>%
  mutate(returns = ((close_weekday_after/close_weekday_before)-1)*100)

#save(earnings_df, file = "./data/earnings_df.RData")

#load("./data/earnings_df.RData")



# Plot return against EPS surprise %---------------------------------------

ggplot(data  = earnings_df, 
       aes(x = epssurprisepct,
           y = returns)) +
  geom_point(size = 0.01) +
  geom_smooth(method = lm) +
  xlim(-200, 200) +
  ylim(-25, 25) +
  theme_minimal_grid(12)



fit1 <- lm(returns ~ epssurprisepct, data = earnings_df)
summary(fit1)



# Download ATM options data -----------------------------------------------

#load("./data/earnings_df.RData")

earnings_df = earnings_df %>%
  mutate(before_year = year(weekday_before),
         after_year  = year(weekday_after))

options_list_orig = lst()
x = 0

for (i in 1:nrow(earnings_df)){
  
  x=x+1
  options_list_orig[[x]] = dbSendQuery(wrds,
                                       paste0("select * from optionm.opprcd", earnings_df$before_year[i],"
    where secid = ", earnings_df$secid[i],"
    and date = \'", earnings_df$weekday_before[i], "\'")) %>%
    dbFetch(n = -1) %>%
    as_tibble()
  
  x=x+1
  options_list_orig[[x]] = dbSendQuery(wrds,
                                       paste0("select * from optionm.opprcd", earnings_df$after_year[i],"
    where secid = ", earnings_df$secid[i],"
    and date = \'", earnings_df$weekday_after[i], "\'")) %>%
    dbFetch(n = -1) %>%
    as_tibble()
  
  print(paste(i, "-", x))
}

rm(x)
options_df_orig = bind_rows(options_list_orig)

#save(options_df_orig, file = "./data/options_df_orig_lst.RData")



# Add options data --------------------------------------------------------

# load("./data/earnings_df.RData")
# load("./data/options_df_orig.RData")

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

tic()
for(i in 1:nrow(earnings_df)){

  b_call = options_df_orig %>% 
    filter(secid == earnings_df$secid[i]) %>%
    filter(date == earnings_df$weekday_before[i]) %>%
    filter(exdate > earnings_df$weekday_after[i]) %>%
    filter(cp_flag == "C") %>%
    filter(strike_price/1000 > earnings_df$close_weekday_before[i]) %>%
    slice(which.min(abs(strike_price/1000 - earnings_df$close_weekday_before[i]))) %>% #test obtain min exdate
    slice(which.min(exdate)) %>% select(best_bid, 
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
    slice(which.min(abs(strike_price/1000 - earnings_df$close_weekday_before[i]))) %>%
    slice(which.min(exdate)) %>% select(best_bid, 
                                        best_offer, 
                                        strike_price, 
                                        exdate, 
                                        volume, 
                                        open_interest, 
                                        impl_volatility, 
                                        optionid)
  
  if (nrow(b_call) != 0 && nrow(b_put) != 0){
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
  }
  
  if (nrow(b_call) != 0 && nrow(b_put) != 0){
    earnings_options_df[i, 1:8] = b_call 
    earnings_options_df[i, 9:16] = b_put 
  }
  
  if (nrow(a_call) != 0 && nrow(a_put) != 0){
    earnings_options_df[i, 17:24] = a_call 
    earnings_options_df[i, 25:32] = a_put 
  }
  print(i)
}
toc()
beep()

#save(earnings_options_df, file = "./data/earnings_options_df.RData")



# Combine Earnings Date DF and Earnings Options DF ------------------------

# load("./data/earnings_df.RData")
# load("./data/earnings_options_df.RData")

earnings_combined_df = bind_cols(earnings_df, earnings_options_df) %>% 
  rowwise() %>%
  mutate( abs_stock_returns = abs(close_weekday_before-close_weekday_after),
          rel_stock_returns = abs((close_weekday_before-close_weekday_after)/close_weekday_before),
          abs_b_strangle  = mean(c(b_call_bid, b_call_offer)) + mean(c(b_put_bid, b_put_offer)),
          abs_a_strangle  = mean(c(a_call_bid, a_call_offer)) + mean(c(a_put_bid, a_put_offer)),
          b_iv            = mean(c(b_call_iv, b_put_iv)),
          a_iv            = mean(c(a_call_iv, a_put_iv)),
          total_volume    = sum(c(b_call_volume, b_put_volume, a_call_volume, a_put_volume)),
          total_opint     = sum(c(b_call_opint, b_put_opint, a_call_opint, a_put_opint)),
          DTE             = b_call_exdate - report_date
        ) %>%
  mutate(abs_strangle_returns =  abs_b_strangle-abs_a_strangle,
         rel_strangle_returns = (abs_b_strangle-abs_a_strangle)/abs_b_strangle,
         iv_change       = a_iv - b_iv
         )

# save(earnings_combined_df, file = "./data/earnings_combined_df.RData")



# Filter Combined DF ------------------------------------------------------

# load("./data/earnings_combined_df.RData")

min_volume = 100
min_opint  = 100

earnings_final_df = earnings_combined_df %>% 
  filter(!is.na(b_call_iv) & 
         !is.na(b_put_iv) &
         !is.na(a_call_iv) &
         !is.na(a_put_iv)) %>%
  filter(b_call_exdate == b_put_exdate) %>%
  filter(b_call_volume > min_volume &
         b_put_volume  > min_volume #&
         # a_call_volume > min_volume &
         # a_put_volume  > min_volume
         ) %>%
  filter(b_call_opint  > min_opint &
         b_put_opint   > min_opint #&
         # a_call_opint  > min_opint &
         # a_put_opint   > min_opint
         ) %>%
#  filter(DTE < 8) %>%
  arrange(report_date) %>% 
  ungroup() %>% 
  mutate(cumsum_profit = cumsum(rel_strangle_returns))

# save(earnings_final_df, file = "./data/earnings_final_df.RData")



# Paper Plots -------------------------------------------------------------

# load("./data/earnings_final_df.RData")

# yearly_distribution_of_trades
ggplot(data = earnings_final_df %>% 
              transmute(year = year(report_date)) %>% 
              count(year)) +
  geom_col(aes(x = year,
               y = n)) +
  xlab("Year") +
  ylab("Number of Trades") +
  theme(axis.title.x=element_text(hjust = 0.43))



# sp500_year_added
ggplot(data = list_spx_df %>% 
         transmute(year = year(`Date first added`)) %>% 
         count(year)) +
  geom_col(aes(x = year,
               y = n)) +
  xlab("Current S&P500 Constituents: Year Added") +
  ylab("Number of Companies") +
  theme(axis.title.x=element_text(hjust = 0.15))



# iv_crush
ggplot(data = earnings_final_df) +
  geom_histogram(aes(x = iv_change), binwidth = 0.03) +
  xlim(-1,1) +
  xlab("Change in IV") +
  ylab("Frequency") +
  theme(axis.title.x=element_text(hjust = 0.40))



# iv_crush table
stargazer(as.data.frame(select(earnings_final_df,iv_change)))



# abs_strangle_returns
ggplot(data = earnings_final_df) +
  geom_histogram(aes(x = abs_strangle_returns), binwidth = 0.1) +
  xlim(-6,6) +
  xlab("Distribution of absolute strangle returns") +
  ylab("Frequency") +
  theme(axis.title.x=element_text(hjust = 0.20))



# abs_strangle_returns table
stargazer(as.data.frame(select(earnings_final_df,abs_strangle_returns)))



# abs_strangle_return positive returns
sum(earnings_final_df$abs_strangle_returns>0)    #5070
length(earnings_final_df$abs_strangle_returns>0) #7655



# rel_strangle_returns
ggplot(data = earnings_final_df) +
  geom_histogram(aes(x = rel_strangle_returns), binwidth = 0.1) +
  xlab("Distribution of relative strangle returns") +
  ylab("Frequency") +
  theme(axis.title.x=element_text(hjust = 0.20)) +
  scale_x_continuous(breaks = -6:2, limits = c(-6,2))



# abs_strangle_returns table
stargazer(as.data.frame(select(earnings_final_df,rel_strangle_returns)))



# strangle_10k_portfolio
earnings_timeseries = earnings_final_df %>% 
  select(report_date, rel_strangle_returns) %>%
  group_by(report_date) %>% # group by the day column
  summarise(strangle_returns = sum(rel_strangle_returns)) %>% #sum relative returns for each day
  mutate(strangle_10k_portfolio = strangle_returns*100)

earnings_timeseries[1, 3] = earnings_timeseries[1, 3] + 10000 #starting capital

earnings_timeseries = earnings_timeseries %>%
  mutate(strangle_10k_portfolio = cumsum(strangle_10k_portfolio)) %>%
  mutate(strangle_10k_change = (strangle_10k_portfolio - lag(strangle_10k_portfolio)) / lag(strangle_10k_portfolio))

ggplot() +
  geom_line(data = earnings_timeseries,
            aes(x = report_date,
                y = strangle_10k_portfolio))+
  xlab("Date") +
  ylab("Portfolio Return (US$)")



# spy_portfolio
start_date <- '2000-01-01'
end_date   <- '2020-12-31'

getSymbols(Symbols = "SPY", src = "yahoo", from =start_date, to = end_date)

SPY_close = Cl(SPY)

SPY_timeseries = SPY_close %>% 
  as_tibble() %>%
  mutate(spy_change = (SPY.Close - lag(SPY.Close)) / lag(SPY.Close)) %>%
  mutate(spy_10k_portfolio = spy_change)

SPY_timeseries[1, 3] = 10000

SPY_timeseries = SPY_timeseries %>%
  mutate(spy_10k_portfolio = cumprod(1 + spy_10k_portfolio)) %>%
  mutate(date = index(SPY_close))



# strangle_spy_comparison
ggplot() +
  geom_line(data = earnings_timeseries,
            aes(x = report_date,
                y = strangle_10k_portfolio, 
                colour = "Strangle")) +
  geom_line(data = SPY_timeseries,
            aes(x = date,
                y = spy_10k_portfolio,
                colour = "SPY")) +
  labs(colour = "Strategy Used") +
  xlab("Date") +
  ylab("Portfolio Return (US$)") + 
  theme(
    legend.position = c(.15, .80),
    legend.margin = margin(6, 6, 6, 6),
    legend.box.background = element_rect(colour = "black")
  ) + 
  guides(color = guide_legend(reverse = TRUE))



# Sharpe Ratio
earnings_timeseries_xts = earnings_timeseries  %>%
  select(report_date, strangle_10k_change) %>% 
  ts_xts()
SharpeRatio(earnings_timeseries_xts,
            annualize = TRUE)

# strangle_10k_change
# Annualized StdDev Sharpe (Rf=0%, p=95%):            1.451383
# Annualized VaR Sharpe (Rf=0%, p=95%):              14.523975
# Annualized ES Sharpe (Rf=0%, p=95%):                4.828307

SPY_timeseries_xts = SPY_timeseries  %>%
  select(date, spy_change) %>% 
  ts_xts()
SharpeRatio(SPY_timeseries_xts,
            annualize = TRUE)

# spy_change
# Annualized StdDev Sharpe (Rf=0%, p=95%):  0.2298824
# Annualized VaR Sharpe (Rf=0%, p=95%):     2.6502948
# Annualized ES Sharpe (Rf=0%, p=95%):      2.6502948



# Drawdown

table.Drawdowns(earnings_timeseries_xts) %>% stargazer(summary = NULL)
chart.Drawdown(earnings_timeseries_xts)



# Portfolio Beta

merged_timeseries_xts = merge.xts(earnings_timeseries_xts, SPY_timeseries_xts)

ggplot(data = merged_timeseries_xts) +
  geom_point(aes(x = spy_change,
                 y = strangle_10k_change))

lm_model <- lm(strangle_10k_change ~ spy_change, data = merged_timeseries_xts)

model_alpha <- lm_model$coefficients[1]  #  0.0006482487
model_beta <- lm_model$coefficients[2]   # -0.00228818




















