# BacktestWeb
Backtest written in R

TODOs:

projects:
RATINGS, instead of measuring performance when opinion changes we d measure rating changes up to-12 months after we ve seen an outperform or underperform opinion.

performance attribution? basically i think i can give you quarterly spreadsheets. where it has analyst opinions (fundamentals, relative value and total opinion) with a date per company. the idea is to see how they've done versus an index. i think i'll only collect the spreadsheets 1x a quarter so we'll have to multiply 3 months of the performance of the bonds. what else? we need to be able to filter it by analyst and by industry. on an index weighted basis v non weighted basis. comparing it to the industry performance as a whole. and the index as whole for those 3 months. we just have to make standard assumptions what the buy/hold/sell/avoids mean, as you know i do it on a multiple of index weight basis, they seem to do it on a DtS basis, buy being +5DtS. i guess sell being -5DtS and avoid 0 DtS  (so they overweight quite a bit smaller companies and underweight larger companies). i don't have the spreadsheets for you but i'll have them next week, could you start having a think? it should be in a seperate powerbi/R or excel spreadsheet even.

Can we decide analysis period? Monthly/quarterly/ytd and since inception?
3 opinions, start of the period, end of the period date of opinions, important if it changes
Index weight begin monthly period, TRR end of period, PRR end of period
Filter opinion result on fundamental basis, relative value basis and price basis
Filter opinion result index weighted, equal weighed
Show results, multiple of index or DtS exposure., v companies covered and v industry in index
Show industry’s (will be provided)
Filter by analyst

visualization:--------------------
create ratios with last 3, 5 and 10 years
current holding view

documentation:-------------------
bull and  bear market cycle logic:
    start first day as either bull or bear
    if bull, compute % diff from prev peak (since start of bull), 
        once find a date where % diff < -10%
        go back and find the peak date, that's the end of the bull and start of bear
    if bear, compute % diff from prev trough (since start of bear), 
        once find a date where % diff > 10%
        go back and find the trough date, that's the end of the bear and start of bull


bond ticker GT1 periods logic:
    find a list of all bond tickers
    create a list of all "As.of.Date" from min to max
    create a data frame for bond.ticker x As.of.Date using two lists above (df1)
    create a data frame from bondmaster for bond.ticker x As.of.Date (df2)
    merge df1 with df2, add a new column called "present", with value of 1 if data present, otherwise 0
    sort df1 with Ticker and As.of.Date, create df2 by using index + 1
    merge df1 with df2, find pattern "10" - that's the start of the ticker
                        find pattern "01" - that's the end of the ticker
                        only keep "10" and "01"
    any ticker has more than 2 rows
