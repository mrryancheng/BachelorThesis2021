import pandas as pd
import datetime as dt
from yahoo_earnings_calendar import YahooEarningsCalendar
import dateutil.parser
import time

#%% List of SP500 Components

spx_df = pd.read_html('https://en.wikipedia.org/wiki/List_of_S%26P_500_companies')[0]

spx_list = spx_df["Symbol"].tolist()
spx_name_list = spx_df["Security"].tolist()
spx_list = [sub.replace(".", "-") for sub in spx_list]

# remove one class of dual class stock
spx_list.remove("FOX")   #FOXA
spx_list.remove("NWS")   #NWSA
spx_list.remove("UA")    #UAA
spx_list.remove("GOOG")  #GOOGL
spx_list.remove("DISCK") #DISCA

#%% Loop

DAYS_AHEAD = 365
yec = YahooEarningsCalendar()
earnings_dict = {}

for key, value in enumerate(spx_list):
    
    if key >= 395:
        TICKER = value
        
        # setting the dates
        start_date = (dt.datetime(2000, 1, 1).date() - dt.timedelta(days=DAYS_AHEAD))
        end_date = dt.datetime(2020, 12, 31).date()
        
        # downloading the earnings calendar
        earnings_list = yec.get_earnings_of(TICKER)
        earnings_df = pd.DataFrame(earnings_list)
        
        # extracting the date from the string and filtering for the period of interest
        earnings_df['report_date'] = earnings_df['startdatetime'].apply(lambda x: dateutil.parser.isoparse(x).date())
        earnings_df = earnings_df.loc[earnings_df['report_date'].between(start_date, end_date)] \
                                 .sort_values('report_date')
        
        earnings_dict[value] = earnings_df
        
        time.sleep(10)
        print(str(key) + " " + value)

#%% Concatenate into large df

earnings_large_df = pd.concat(earnings_dict, ignore_index=True)

earnings_large_df.to_csv(r"./data/earnings_sp500_20y.csv", index=False)
    































