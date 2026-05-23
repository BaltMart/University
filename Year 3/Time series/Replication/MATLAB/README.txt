Work done by: Martynas Baltramaitis, Adomas Jablonskas, Dominykas Meškauskas, Domantas Vaitkus.

Code done by: Martynas Baltramaitis

CODE

The ZIP file should contain everything needed to run the code. You should change the home_holder location for the codes to work. Please, check whether the use of "\" or "/" is consistent throughout the codes as it might not work as intended if not. It may happen that the code does not create pdfs of the graphs. In this case, the ghostscript application should not be required. Our advice would be to restart MATLAB. Also, you should make sure that Econometrics Toolbox is installed within your MATLAB.

We run 5 VAR models, each in a different script except for models including wara and wara_later variables. Main parts of the code are similar, most changes are changing of which variables are used in the estimations, two of the scripts feature more data preparatory work than others due to necessity of prepared variables.

SpendingShocks.m runs the standard identification VAR model. Also, this code features Granger causality tests for the same variables as the authors do. Their results are available by running the disp(ResultsTable) at the end of this script.

SpendingShocksWar.m runs the War Dates Identification method VAR. Also, the same code runs the War Dates mistimed VAR model. To run this version, you should change 'wara' to 'wara_later' in Line 157 and 'Ramey-Shapiro Narrative variable' to 'Ramey-Shapiro Narrative variable Mistimed'. Also, update the naming scheme on Line 212 to not overwrite the older VAR model impulse response plots.

SpendingShocksSPF.m runs the VAR model with Professional forecaster errors for 1 quarter ahead. The code should run without any changes.

SpendingShocksNews.m runs the VAR model with PDV of expected change in spending Divided by GDP of previous quarter. This model is also unique among the models, as it also uses data before 1947 to 1939. Code should run as is.


DATA
Dataset obtained through authors website. Some edits made removing duplicating time series, also, manually added pdvmilly and wara_later variables. Both can be found in the paper appendix and is the place those data sets were obtained.

Variables:
quarter
pdvmil - PDV of expected change in spending, billions of nominal dollars
pdvmily - PDV of expected change in spending Divided by GDP of previous quarter
wara - Ramey-Shapiro Narrative variable (sudden Business Week forecast of rises in defense spending)
wara_later - Ramey-Shapiro Narrative variable Mistimed
pgdp - GDP price deflator
rgdp - Real GDP
rcons - Real consumption
rcdur - Real consumption of durables
rcnd - Real consumption of nondurables
rcsv - Real consumption of services
rcndsv - Real consumption of nondurables and services
rinv - Real investment
rinvfx - Real fixed investment
rnri - Real nonresidential investment
rres - Real residential investment
rgov - Real government spending
rfed - Real federal spending
rsl - Real state and local spending
kcdur - Capital Stock of Durables
tothours - Total hours worked
milithours - Military hours
totpop - Total population
fr25_64pop16 - Fraction of population aged 25-64 (over 16)
fr25_64pop - Fraction of population aged 25-64
tothoursces - Total hours (CES - Current Employment Statistics)
nwmfg - Nominal wage in manufacturing
pman - Manufacturing price index
cpi - Consumer Price Index
ppi - Producer Price Index
pbus - Business price index
nwbus - Nominal wage in business sector
BAA - BAA corporate bond rate
tb3 - 3-month Treasury bill rate
romerexog - Romer exogenous shock variable
amtss - AMTSS 
amtbr - AMTBR 
spf_rfedlag - Survey of Professional Forecasters: lagged real federal spending
spf_rfed0 - SPF: current quarter real federal spending forecast
spf_rfed1 - SPF: 1-quarter ahead real federal spending forecast
spf_rfed2 - SPF: 2-quarters ahead real federal spending forecast
spf_rfed3 - SPF: 3-quarters ahead real federal spending forecast
spf_rfed4 - SPF: 4-quarters ahead real federal spending forecast
spf_rsllag - SPF: lagged real state and local spending
spf_rsl0 - SPF: current quarter real state and local spending forecast
spf_rsl1 - SPF: 1-quarter ahead real state and local spending forecast
spf_rsl2 - SPF: 2-quarters ahead real state and local spending forecast
spf_rsl3 - SPF: 3-quarters ahead real state and local spending forecast
spf_rsl4 - SPF: 4-quarters ahead real state and local spending forecast
spf_ndeflag - SPF: lagged nominal defense spending
spf_ndef0 - SPF: current quarter nominal defense spending forecast
spf_ndef1 - SPF: 1-quarter ahead nominal defense spending forecast
spf_ndef2 - SPF: 2-quarters ahead nominal defense spending forecast
spf_ndef3 - SPF: 3-quarters ahead nominal defense spending forecast
spf_ndef4 - SPF: 4-quarters ahead nominal defense spending forecast
spf_pgdplag - SPF: lagged GDP price deflator
spf_pgdp0 - SPF: current quarter GDP price deflator forecast
spf_pgdp1 - SPF: 1-quarter ahead GDP price deflator forecast
spf_pgdp2 - SPF: 2-quarters ahead GDP price deflator forecast
spf_pgdp3 - SPF: 3-quarters ahead GDP price deflator forecast
spf_pgdp4 - SPF: 4-quarters ahead GDP price deflator forecast






