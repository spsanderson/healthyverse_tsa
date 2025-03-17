Time Series Analysis and Nested Modeling of the Healthyverse Packages
================
Steven P. Sanderson II, MPH - Date:
17 March, 2025

This analysis follows a *Nested Modeltime Workflow*.

## Get Data

``` r
glimpse(downloads_tbl)
```

    ## Rows: 134,362
    ## Columns: 11
    ## $ date      <date> 2020-11-23, 2020-11-23, 2020-11-23, 2020-11-23, 2020-11-23,…
    ## $ time      <Period> 15H 36M 55S, 11H 26M 39S, 23H 34M 44S, 18H 39M 32S, 9H 0M…
    ## $ date_time <dttm> 2020-11-23 15:36:55, 2020-11-23 11:26:39, 2020-11-23 23:34:…
    ## $ size      <int> 4858294, 4858294, 4858301, 4858295, 361, 4863722, 4864794, 4…
    ## $ r_version <chr> NA, "4.0.3", "3.5.3", "3.5.2", NA, NA, NA, NA, NA, NA, NA, N…
    ## $ r_arch    <chr> NA, "x86_64", "x86_64", "x86_64", NA, NA, NA, NA, NA, NA, NA…
    ## $ r_os      <chr> NA, "mingw32", "mingw32", "linux-gnu", NA, NA, NA, NA, NA, N…
    ## $ package   <chr> "healthyR.data", "healthyR.data", "healthyR.data", "healthyR…
    ## $ version   <chr> "1.0.0", "1.0.0", "1.0.0", "1.0.0", "1.0.0", "1.0.0", "1.0.0…
    ## $ country   <chr> "US", "US", "US", "GB", "US", "US", "DE", "HK", "JP", "US", …
    ## $ ip_id     <int> 2069, 2804, 78827, 27595, 90474, 90474, 42435, 74, 7655, 638…

The last day in the data set is 2025-03-15 20:26:42, the file was
birthed on: 2024-08-07 07:35:44, and at report knit time is -5288.85
hours old. Happy analyzing!

Now that we have our data lets take a look at it using the `skimr`
package.

``` r
skim(downloads_tbl)
```

|                                                  |               |
|:-------------------------------------------------|:--------------|
| Name                                             | downloads_tbl |
| Number of rows                                   | 134362        |
| Number of columns                                | 11            |
| \_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_   |               |
| Column type frequency:                           |               |
| character                                        | 6             |
| Date                                             | 1             |
| numeric                                          | 2             |
| POSIXct                                          | 1             |
| Timespan                                         | 1             |
| \_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_ |               |
| Group variables                                  | None          |

Data summary

**Variable type: character**

| skim_variable | n_missing | complete_rate | min | max | empty | n_unique | whitespace |
|:--------------|----------:|--------------:|----:|----:|------:|---------:|-----------:|
| r_version     |     96555 |          0.28 |   5 |   5 |     0 |       46 |          0 |
| r_arch        |     96555 |          0.28 |   3 |   7 |     0 |        5 |          0 |
| r_os          |     96555 |          0.28 |   7 |  15 |     0 |       21 |          0 |
| package       |         0 |          1.00 |   7 |  13 |     0 |        8 |          0 |
| version       |         0 |          1.00 |   5 |  17 |     0 |       60 |          0 |
| country       |     11304 |          0.92 |   2 |   2 |     0 |      163 |          0 |

**Variable type: Date**

| skim_variable | n_missing | complete_rate | min | max | median | n_unique |
|:---|---:|---:|:---|:---|:---|---:|
| date | 0 | 1 | 2020-11-23 | 2025-03-15 | 2023-05-19 | 1574 |

**Variable type: numeric**

| skim_variable | n_missing | complete_rate | mean | sd | p0 | p25 | p50 | p75 | p100 | hist |
|:---|---:|---:|---:|---:|---:|---:|---:|---:|---:|:---|
| size | 0 | 1 | 1135767.76 | 1525562.02 | 355 | 14701 | 260639 | 2367783.0 | 5677952 | ▇▁▂▁▁ |
| ip_id | 0 | 1 | 10374.56 | 18355.04 | 1 | 303 | 3091 | 11800.5 | 209747 | ▇▁▁▁▁ |

**Variable type: POSIXct**

| skim_variable | n_missing | complete_rate | min | max | median | n_unique |
|:---|---:|---:|:---|:---|:---|---:|
| date_time | 0 | 1 | 2020-11-23 09:00:41 | 2025-03-15 20:26:42 | 2023-05-19 01:17:47 | 81527 |

**Variable type: Timespan**

| skim_variable | n_missing | complete_rate | min | max | median | n_unique |
|:--------------|----------:|--------------:|----:|----:|-------:|---------:|
| time          |         0 |             1 |   0 |  59 |     58 |       60 |

We can see that the following columns are missing a lot of data and for
us are most likely not useful anyways, so we will drop them
`c(r_version, r_arch, r_os)`

## Plots

Now lets take a look at a time-series plot of the total daily downloads
by package. We will use a log scale and place a vertical line at each
version release for each package.

![](man/figures/README-initial_ts_plot-1.png)<!-- -->![](man/figures/README-initial_ts_plot-2.png)<!-- -->

Now lets take a look at some time series decomposition graphs.

![](man/figures/README-ts_decomp_plt-1.png)<!-- -->![](man/figures/README-ts_decomp_plt-2.png)<!-- -->![](man/figures/README-ts_decomp_plt-3.png)<!-- -->![](man/figures/README-ts_decomp_plt-4.png)<!-- -->

## Feature Engineering

Now that we have our basic data and a shot of what it looks like, let’s
add some features to our data which can be very helpful in modeling.
Lets start by making a `tibble` that is aggregated by the day and
package, as we are going to be interested in forecasting the next 4
weeks or 28 days for each package. First lets get our base data.

    ## 
    ## Call:
    ## stats::lm(formula = .formula, data = df)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -152.83  -35.15  -10.19   26.96  811.08 
    ## 
    ## Coefficients:
    ##                                                      Estimate Std. Error
    ## (Intercept)                                        -2.035e+02  7.495e+01
    ## date                                                1.216e-02  3.977e-03
    ## lag(value, 1)                                       1.108e-01  2.499e-02
    ## lag(value, 7)                                       9.128e-02  2.586e-02
    ## lag(value, 14)                                      9.516e-02  2.589e-02
    ## lag(value, 21)                                      6.591e-02  2.597e-02
    ## lag(value, 28)                                      6.330e-02  2.595e-02
    ## lag(value, 35)                                      7.125e-02  2.608e-02
    ## lag(value, 42)                                      5.377e-02  2.621e-02
    ## lag(value, 49)                                      8.411e-02  2.613e-02
    ## month(date, label = TRUE).L                        -1.169e+01  5.172e+00
    ## month(date, label = TRUE).Q                         2.446e+00  5.193e+00
    ## month(date, label = TRUE).C                        -1.138e+01  5.260e+00
    ## month(date, label = TRUE)^4                        -8.094e+00  5.225e+00
    ## month(date, label = TRUE)^5                        -1.203e+01  5.207e+00
    ## month(date, label = TRUE)^6                        -2.516e+00  5.287e+00
    ## month(date, label = TRUE)^7                        -7.537e+00  5.196e+00
    ## month(date, label = TRUE)^8                        -3.566e+00  5.229e+00
    ## month(date, label = TRUE)^9                         4.948e+00  5.276e+00
    ## month(date, label = TRUE)^10                        4.836e+00  5.308e+00
    ## month(date, label = TRUE)^11                       -6.158e+00  5.331e+00
    ## fourier_vec(date, type = "sin", K = 1, period = 7) -1.181e+01  2.401e+00
    ## fourier_vec(date, type = "cos", K = 1, period = 7)  7.839e+00  2.532e+00
    ##                                                    t value Pr(>|t|)    
    ## (Intercept)                                         -2.716 0.006689 ** 
    ## date                                                 3.057 0.002273 ** 
    ## lag(value, 1)                                        4.434 9.94e-06 ***
    ## lag(value, 7)                                        3.530 0.000428 ***
    ## lag(value, 14)                                       3.676 0.000245 ***
    ## lag(value, 21)                                       2.538 0.011252 *  
    ## lag(value, 28)                                       2.439 0.014826 *  
    ## lag(value, 35)                                       2.732 0.006359 ** 
    ## lag(value, 42)                                       2.051 0.040411 *  
    ## lag(value, 49)                                       3.219 0.001313 ** 
    ## month(date, label = TRUE).L                         -2.260 0.023953 *  
    ## month(date, label = TRUE).Q                          0.471 0.637694    
    ## month(date, label = TRUE).C                         -2.163 0.030682 *  
    ## month(date, label = TRUE)^4                         -1.549 0.121581    
    ## month(date, label = TRUE)^5                         -2.311 0.020965 *  
    ## month(date, label = TRUE)^6                         -0.476 0.634232    
    ## month(date, label = TRUE)^7                         -1.451 0.147090    
    ## month(date, label = TRUE)^8                         -0.682 0.495447    
    ## month(date, label = TRUE)^9                          0.938 0.348396    
    ## month(date, label = TRUE)^10                         0.911 0.362347    
    ## month(date, label = TRUE)^11                        -1.155 0.248207    
    ## fourier_vec(date, type = "sin", K = 1, period = 7)  -4.916 9.79e-07 ***
    ## fourier_vec(date, type = "cos", K = 1, period = 7)   3.096 0.001995 ** 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 58.52 on 1502 degrees of freedom
    ##   (49 observations deleted due to missingness)
    ## Multiple R-squared:  0.2578, Adjusted R-squared:  0.247 
    ## F-statistic: 23.72 on 22 and 1502 DF,  p-value: < 2.2e-16

![](man/figures/README-base_data_frame-1.png)<!-- -->

## NNS Forecasting

This is something I have been wanting to try for a while. The `NNS`
package is a great package for forecasting time series data.

[NNS GitHub](https://github.com/OVVO-Financial/NNS)

``` r
library(NNS)

data_list <- base_data |>
    select(package, value) |>
    group_split(package)

data_list |>
    imap(
        \(x, idx) {
            obj <- x
            x <- obj |> pull(value) |> tail(7*52)
            train_set_size <- length(x) - 56
            pkg <- obj |> pluck(1) |> unique()
            sf <- NNS.seas(x, modulo = 7, plot = FALSE)$periods
            
            cat(paste0("Package: ", pkg, "\n"))
            NNS.ARMA.optim(
                variable = x,
                h = 28,
                training.set = train_set_size,
                #seasonal.factor = seq(12, 60, 7),
                seasonal.factor = sf,
                pred.int = 0.95,
                plot = TRUE
            )
            title(
                sub = paste0("\n",
                             "Package: ", pkg, " - NNS Optimization")
            )
        }
    )
```

    ## Package: healthyR
    ## [1] "CURRNET METHOD: lin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 77 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 1.44858751610304"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 77, 98 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 1.25522958148642"
    ## [1] "BEST method = 'lin', seasonal.factor = c( 77, 98 )"
    ## [1] "BEST lin OBJECTIVE FUNCTION = 1.25522958148642"
    ## [1] "CURRNET METHOD: nonlin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'nonlin' , seasonal.factor =  c( 77, 98 ) ...)"
    ## [1] "CURRENT nonlin OBJECTIVE FUNCTION = 1.5458641968819"
    ## [1] "BEST method = 'nonlin' PATH MEMBER = c( 77, 98 )"
    ## [1] "BEST nonlin OBJECTIVE FUNCTION = 1.5458641968819"
    ## [1] "CURRNET METHOD: both"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'both' , seasonal.factor =  c( 77, 98 ) ...)"
    ## [1] "CURRENT both OBJECTIVE FUNCTION = 1.32933445717157"
    ## [1] "BEST method = 'both' PATH MEMBER = c( 77, 98 )"
    ## [1] "BEST both OBJECTIVE FUNCTION = 1.32933445717157"

![](man/figures/README-nns_forecasting-1.png)<!-- -->

    ## Package: healthyR.ai
    ## [1] "CURRNET METHOD: lin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 49 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 1.52203705298649"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 49, 21 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 1.37536199504593"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 49, 21, 77 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 1.30118845098046"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 49, 21, 77, 98 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 1.29935902596388"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 49, 21, 77, 98, 63 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 1.26324423875545"
    ## [1] "BEST method = 'lin', seasonal.factor = c( 49, 21, 77, 98, 63 )"
    ## [1] "BEST lin OBJECTIVE FUNCTION = 1.26324423875545"
    ## [1] "CURRNET METHOD: nonlin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'nonlin' , seasonal.factor =  c( 49, 21, 77, 98, 63 ) ...)"
    ## [1] "CURRENT nonlin OBJECTIVE FUNCTION = 1.60341367261532"
    ## [1] "BEST method = 'nonlin' PATH MEMBER = c( 49, 21, 77, 98, 63 )"
    ## [1] "BEST nonlin OBJECTIVE FUNCTION = 1.60341367261532"
    ## [1] "CURRNET METHOD: both"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'both' , seasonal.factor =  c( 49, 21, 77, 98, 63 ) ...)"
    ## [1] "CURRENT both OBJECTIVE FUNCTION = 1.37835938366073"
    ## [1] "BEST method = 'both' PATH MEMBER = c( 49, 21, 77, 98, 63 )"
    ## [1] "BEST both OBJECTIVE FUNCTION = 1.37835938366073"

![](man/figures/README-nns_forecasting-2.png)<!-- -->

    ## Package: healthyR.data
    ## [1] "CURRNET METHOD: lin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 98 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 1.55327994342338"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 98, 77 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 1.21139883700552"
    ## [1] "BEST method = 'lin', seasonal.factor = c( 98, 77 )"
    ## [1] "BEST lin OBJECTIVE FUNCTION = 1.21139883700552"
    ## [1] "CURRNET METHOD: nonlin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'nonlin' , seasonal.factor =  c( 98, 77 ) ...)"
    ## [1] "CURRENT nonlin OBJECTIVE FUNCTION = 1.38329994965838"
    ## [1] "BEST method = 'nonlin' PATH MEMBER = c( 98, 77 )"
    ## [1] "BEST nonlin OBJECTIVE FUNCTION = 1.38329994965838"
    ## [1] "CURRNET METHOD: both"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'both' , seasonal.factor =  c( 98, 77 ) ...)"
    ## [1] "CURRENT both OBJECTIVE FUNCTION = 1.21711707978754"
    ## [1] "BEST method = 'both' PATH MEMBER = c( 98, 77 )"
    ## [1] "BEST both OBJECTIVE FUNCTION = 1.21711707978754"

![](man/figures/README-nns_forecasting-3.png)<!-- -->

    ## Package: healthyR.ts
    ## [1] "CURRNET METHOD: lin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 14 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 1.59690782312742"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 14, 98 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 1.2944155613209"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 14, 98, 63 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 1.23116325298457"
    ## [1] "BEST method = 'lin', seasonal.factor = c( 14, 98, 63 )"
    ## [1] "BEST lin OBJECTIVE FUNCTION = 1.23116325298457"
    ## [1] "CURRNET METHOD: nonlin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'nonlin' , seasonal.factor =  c( 14, 98, 63 ) ...)"
    ## [1] "CURRENT nonlin OBJECTIVE FUNCTION = 1.58347971376848"
    ## [1] "BEST method = 'nonlin' PATH MEMBER = c( 14, 98, 63 )"
    ## [1] "BEST nonlin OBJECTIVE FUNCTION = 1.58347971376848"
    ## [1] "CURRNET METHOD: both"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'both' , seasonal.factor =  c( 14, 98, 63 ) ...)"
    ## [1] "CURRENT both OBJECTIVE FUNCTION = 1.3370269491364"
    ## [1] "BEST method = 'both' PATH MEMBER = c( 14, 98, 63 )"
    ## [1] "BEST both OBJECTIVE FUNCTION = 1.3370269491364"

![](man/figures/README-nns_forecasting-4.png)<!-- -->

    ## Package: healthyverse
    ## [1] "CURRNET METHOD: lin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 49 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 1.61575614740093"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 49, 77 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 1.45700885829234"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 49, 77, 70 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 1.4491506029377"
    ## [1] "BEST method = 'lin', seasonal.factor = c( 49, 77, 70 )"
    ## [1] "BEST lin OBJECTIVE FUNCTION = 1.4491506029377"
    ## [1] "CURRNET METHOD: nonlin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'nonlin' , seasonal.factor =  c( 49, 77, 70 ) ...)"
    ## [1] "CURRENT nonlin OBJECTIVE FUNCTION = 2.67160419559667"
    ## [1] "BEST method = 'nonlin' PATH MEMBER = c( 49, 77, 70 )"
    ## [1] "BEST nonlin OBJECTIVE FUNCTION = 2.67160419559667"
    ## [1] "CURRNET METHOD: both"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'both' , seasonal.factor =  c( 49, 77, 70 ) ...)"
    ## [1] "CURRENT both OBJECTIVE FUNCTION = 1.89840616147607"
    ## [1] "BEST method = 'both' PATH MEMBER = c( 49, 77, 70 )"
    ## [1] "BEST both OBJECTIVE FUNCTION = 1.89840616147607"

![](man/figures/README-nns_forecasting-5.png)<!-- -->

    ## Package: RandomWalker
    ## [1] "CURRNET METHOD: lin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 35 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 2.60623307772534"
    ## [1] "BEST method = 'lin', seasonal.factor = c( 35 )"
    ## [1] "BEST lin OBJECTIVE FUNCTION = 2.60623307772534"
    ## [1] "CURRNET METHOD: nonlin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'nonlin' , seasonal.factor =  c( 35 ) ...)"
    ## [1] "CURRENT nonlin OBJECTIVE FUNCTION = 3.7574803525372"
    ## [1] "BEST method = 'nonlin' PATH MEMBER = c( 35 )"
    ## [1] "BEST nonlin OBJECTIVE FUNCTION = 3.7574803525372"
    ## [1] "CURRNET METHOD: both"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'both' , seasonal.factor =  c( 35 ) ...)"
    ## [1] "CURRENT both OBJECTIVE FUNCTION = 3.10319784661514"
    ## [1] "BEST method = 'both' PATH MEMBER = c( 35 )"
    ## [1] "BEST both OBJECTIVE FUNCTION = 3.10319784661514"

![](man/figures/README-nns_forecasting-6.png)<!-- -->

    ## Package: tidyAML
    ## [1] "CURRNET METHOD: lin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 56 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 6.58346730649209"
    ## [1] "BEST method = 'lin', seasonal.factor = c( 56 )"
    ## [1] "BEST lin OBJECTIVE FUNCTION = 6.58346730649209"
    ## [1] "CURRNET METHOD: nonlin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'nonlin' , seasonal.factor =  c( 56 ) ...)"
    ## [1] "CURRENT nonlin OBJECTIVE FUNCTION = 5.94289984110086"
    ## [1] "BEST method = 'nonlin' PATH MEMBER = c( 56 )"
    ## [1] "BEST nonlin OBJECTIVE FUNCTION = 5.94289984110086"
    ## [1] "CURRNET METHOD: both"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'both' , seasonal.factor =  c( 56 ) ...)"
    ## [1] "CURRENT both OBJECTIVE FUNCTION = 6.00948428730065"
    ## [1] "BEST method = 'both' PATH MEMBER = c( 56 )"
    ## [1] "BEST both OBJECTIVE FUNCTION = 6.00948428730065"

![](man/figures/README-nns_forecasting-7.png)<!-- -->

    ## Package: TidyDensity
    ## [1] "CURRNET METHOD: lin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 77 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 1.62425508724155"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 77, 63 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 1.3653692496524"
    ## [1] "BEST method = 'lin', seasonal.factor = c( 77, 63 )"
    ## [1] "BEST lin OBJECTIVE FUNCTION = 1.3653692496524"
    ## [1] "CURRNET METHOD: nonlin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'nonlin' , seasonal.factor =  c( 77, 63 ) ...)"
    ## [1] "CURRENT nonlin OBJECTIVE FUNCTION = 2.05683934553318"
    ## [1] "BEST method = 'nonlin' PATH MEMBER = c( 77, 63 )"
    ## [1] "BEST nonlin OBJECTIVE FUNCTION = 2.05683934553318"
    ## [1] "CURRNET METHOD: both"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'both' , seasonal.factor =  c( 77, 63 ) ...)"
    ## [1] "CURRENT both OBJECTIVE FUNCTION = 1.59148001635657"
    ## [1] "BEST method = 'both' PATH MEMBER = c( 77, 63 )"
    ## [1] "BEST both OBJECTIVE FUNCTION = 1.59148001635657"

![](man/figures/README-nns_forecasting-8.png)<!-- -->

    ## [[1]]
    ## NULL
    ## 
    ## [[2]]
    ## NULL
    ## 
    ## [[3]]
    ## NULL
    ## 
    ## [[4]]
    ## NULL
    ## 
    ## [[5]]
    ## NULL
    ## 
    ## [[6]]
    ## NULL
    ## 
    ## [[7]]
    ## NULL
    ## 
    ## [[8]]
    ## NULL

## Pre-Processing

Now we are going to do some basic pre-processing.

``` r
data_padded_tbl <- base_data %>%
  pad_by_time(
    .date_var  = date,
    .pad_value = 0
  )

# Get log interval and standardization parameters
log_params  <- liv(data_padded_tbl$value, limit_lower = 0, offset = 1, silent = TRUE)
limit_lower <- log_params$limit_lower
limit_upper <- log_params$limit_upper
offset      <- log_params$offset

data_liv_tbl <- data_padded_tbl %>%
  # Get log interval transform
  mutate(value_trans = liv(value, limit_lower = 0, offset = 1, silent = TRUE)$log_scaled)

# Get Standardization Params
std_params <- standard_vec(data_liv_tbl$value_trans, silent = TRUE)
std_mean   <- std_params$mean
std_sd     <- std_params$sd

data_transformed_tbl <- data_liv_tbl %>%
  # get standardization
  mutate(value_trans = standard_vec(value_trans, silent = TRUE)$standard_scaled) %>%
  select(-value)
```

Since this is panel data we can follow one of two different modeling
strategies. We can search for a global model in the panel data or we can
use nested forecasting finding the best model for each of the time
series. Since we only have 5 panels, we will use nested forecasting.

To do this we will use the `nest_timeseries` and
`split_nested_timeseries` functions to create a nested `tibble`.

``` r
horizon <- 4*7

nested_data_tbl <- data_transformed_tbl %>%
    
    # 1. Extending: We'll predict n days into the future.
    extend_timeseries(
        .id_var        = package,
        .date_var      = date,
        .length_future = horizon
    ) %>%
    
    # 2. Nesting: We'll group by id, and create a future dataset
    #    that forecasts n days of extended data and
    #    an actual dataset that contains n*2 days
    nest_timeseries(
        .id_var        = package,
        .length_future = horizon
        #.length_actual = horizon*2
    ) %>%
    
   # 3. Splitting: We'll take the actual data and create splits
   #    for accuracy and confidence interval estimation of n das (test)
   #    and the rest is training data
    split_nested_timeseries(
        .length_test = horizon
    )

nested_data_tbl
```

    ## # A tibble: 8 × 4
    ##   package       .actual_data         .future_data      .splits          
    ##   <fct>         <list>               <list>            <list>           
    ## 1 healthyR.data <tibble [1,567 × 2]> <tibble [28 × 2]> <split [1539|28]>
    ## 2 healthyR      <tibble [1,560 × 2]> <tibble [28 × 2]> <split [1532|28]>
    ## 3 healthyR.ts   <tibble [1,504 × 2]> <tibble [28 × 2]> <split [1476|28]>
    ## 4 healthyverse  <tibble [1,475 × 2]> <tibble [28 × 2]> <split [1447|28]>
    ## 5 healthyR.ai   <tibble [1,299 × 2]> <tibble [28 × 2]> <split [1271|28]>
    ## 6 TidyDensity   <tibble [1,150 × 2]> <tibble [28 × 2]> <split [1122|28]>
    ## 7 tidyAML       <tibble [758 × 2]>   <tibble [28 × 2]> <split [730|28]> 
    ## 8 RandomWalker  <tibble [180 × 2]>   <tibble [28 × 2]> <split [152|28]>

Now it is time to make some recipes and models using the modeltime
workflow.

## Modeltime Workflow

### Recipe Object

``` r
recipe_base <- recipe(
  value_trans ~ date
  , data = extract_nested_test_split(nested_data_tbl)
  )

recipe_base

recipe_date <- recipe_base %>%
    step_mutate(date = as.numeric(date))
```

### Models

``` r
# Models ------------------------------------------------------------------

# Auto ARIMA --------------------------------------------------------------

model_spec_arima_no_boost <- arima_reg() %>%
  set_engine(engine = "auto_arima")

wflw_auto_arima <- workflow() %>%
  add_recipe(recipe = recipe_base) %>%
  add_model(model_spec_arima_no_boost)

# NNETAR ------------------------------------------------------------------

model_spec_nnetar <- nnetar_reg(
  mode              = "regression"
  , seasonal_period = "auto"
) %>%
  set_engine("nnetar")

wflw_nnetar <- workflow() %>%
  add_recipe(recipe = recipe_base) %>%
  add_model(model_spec_nnetar)

# TSLM --------------------------------------------------------------------

model_spec_lm <- linear_reg() %>%
  set_engine("lm")

wflw_lm <- workflow() %>%
  add_recipe(recipe = recipe_base) %>%
  add_model(model_spec_lm)

# MARS --------------------------------------------------------------------

model_spec_mars <- mars(mode = "regression") %>%
  set_engine("earth")

wflw_mars <- workflow() %>%
  add_recipe(recipe = recipe_base) %>%
  add_model(model_spec_mars)
```

### Nested Modeltime Tables

``` r
nested_modeltime_tbl <- modeltime_nested_fit(
  # Nested Data
  nested_data = nested_data_tbl,
   control = control_nested_fit(
     verbose = TRUE,
     allow_par = FALSE
   ),
  # Add workflows
  wflw_auto_arima,
  wflw_lm,
  wflw_mars,
  wflw_nnetar
)
```

``` r
nested_modeltime_tbl <- nested_modeltime_tbl[!is.na(nested_modeltime_tbl$package),]
```

### Model Accuracy

``` r
nested_modeltime_tbl %>%
  extract_nested_test_accuracy() %>%
  filter(!is.na(package)) %>%
  knitr::kable()
```

| package | .model_id | .model_desc | .type | mae | mape | mase | smape | rmse | rsq |
|:---|---:|:---|:---|---:|---:|---:|---:|---:|---:|
| healthyR.data | 1 | ARIMA | Test | 0.7520806 | 105.41107 | 0.6464112 | 172.99099 | 0.9170915 | 0.1760373 |
| healthyR.data | 2 | LM | Test | 0.8159665 | 201.02200 | 0.7013210 | 146.83964 | 0.9411962 | 0.0303146 |
| healthyR.data | 3 | EARTH | Test | 0.9276161 | 216.18612 | 0.7972835 | 130.70382 | 1.1784356 | 0.0303146 |
| healthyR.data | 4 | NNAR | Test | 0.7803106 | 97.86243 | 0.6706748 | 167.19996 | 0.9607194 | 0.0407082 |
| healthyR | 1 | ARIMA | Test | 0.7103088 | 120.38468 | 0.7046938 | 149.24356 | 0.9035533 | 0.0627154 |
| healthyR | 2 | LM | Test | 0.7461241 | 105.84881 | 0.7402260 | 185.82640 | 0.9402348 | 0.0073059 |
| healthyR | 3 | EARTH | Test | 1.8455587 | 908.46969 | 1.8309694 | 140.87632 | 2.0914749 | 0.0073059 |
| healthyR | 4 | NNAR | Test | 0.7038026 | 117.47177 | 0.6982390 | 146.52175 | 0.9226956 | 0.0389956 |
| healthyR.ts | 1 | ARIMA | Test | 0.9901950 | 128.08728 | 0.6603626 | 129.30429 | 1.1758963 | 0.0353530 |
| healthyR.ts | 2 | LM | Test | 0.9921694 | 126.28437 | 0.6616793 | 131.37065 | 1.1757392 | 0.0353530 |
| healthyR.ts | 3 | EARTH | Test | 0.9911287 | 129.99607 | 0.6609852 | 128.17731 | 1.1769554 | 0.0353530 |
| healthyR.ts | 4 | NNAR | Test | 0.9649365 | 104.41907 | 0.6435176 | 173.53762 | 1.1647269 | 0.0794237 |
| healthyverse | 1 | ARIMA | Test | 0.6311721 | 121.26805 | 0.9074009 | 106.81434 | 0.7908930 | 0.0643584 |
| healthyverse | 2 | LM | Test | 0.7086052 | 182.00346 | 1.0187221 | 105.66681 | 0.8509474 | 0.0953668 |
| healthyverse | 3 | EARTH | Test | 0.6514351 | 119.97854 | 0.9365319 | 112.67884 | 0.8168544 | 0.0953668 |
| healthyverse | 4 | NNAR | Test | 0.6421310 | 103.58727 | 0.9231559 | 109.91870 | 0.8363151 | 0.0002430 |
| healthyR.ai | 1 | ARIMA | Test | 0.7391234 | 97.96736 | 0.7439344 | 173.17403 | 0.8585951 | 0.2181994 |
| healthyR.ai | 2 | LM | Test | 0.7708377 | 91.52940 | 0.7758552 | 145.19863 | 0.9420319 | 0.0283014 |
| healthyR.ai | 3 | EARTH | Test | 0.7725916 | 91.94526 | 0.7776205 | 154.05288 | 0.9317107 | 0.0283014 |
| healthyR.ai | 4 | NNAR | Test | 0.6876244 | 86.61774 | 0.6921003 | 137.45933 | 0.8329562 | 0.2634864 |
| TidyDensity | 1 | ARIMA | Test | 0.6454500 | 154.65887 | 0.7120457 | 111.79344 | 0.7705907 | 0.0076522 |
| TidyDensity | 2 | LM | Test | 0.6622836 | 196.18396 | 0.7306161 | 105.69675 | 0.7649938 | 0.0418765 |
| TidyDensity | 3 | EARTH | Test | 0.6546244 | 145.38102 | 0.7221666 | 115.25850 | 0.7822984 | 0.0418765 |
| TidyDensity | 4 | NNAR | Test | 0.6728003 | 113.02154 | 0.7422179 | 134.82153 | 0.8494631 | 0.0139041 |
| tidyAML | 1 | ARIMA | Test | 0.6676155 | 271.67439 | 0.6935472 | 99.15499 | 0.7642610 | 0.0150364 |
| tidyAML | 2 | LM | Test | 0.6351561 | 253.31853 | 0.6598269 | 93.73713 | 0.7616646 | 0.0012402 |
| tidyAML | 3 | EARTH | Test | 0.7806080 | 123.28508 | 0.8109285 | 154.86943 | 0.9878576 | 0.0012402 |
| tidyAML | 4 | NNAR | Test | 0.6265685 | 228.77417 | 0.6509058 | 97.93236 | 0.7476482 | 0.0423375 |
| RandomWalker | 1 | ARIMA | Test | 0.8247207 | 68.00311 | 0.4013012 | 79.06008 | 1.0467870 | 0.4601688 |
| RandomWalker | 2 | LM | Test | 1.3426995 | 108.83145 | 0.6533448 | 193.70183 | 1.4437901 | 0.0252182 |
| RandomWalker | 3 | EARTH | Test | 1.2502179 | 92.21914 | 0.6083441 | 168.76820 | 1.4100570 | NA |
| RandomWalker | 4 | NNAR | Test | 1.4089069 | 131.70834 | 0.6855607 | 156.89706 | 1.5929400 | 0.0002202 |

### Plot Models

``` r
nested_modeltime_tbl %>%
  extract_nested_test_forecast() %>%
  group_by(package) %>%
  plot_modeltime_forecast(
    .interactive = FALSE,
    .conf_interval_show  = FALSE,
    .facet_scales = "free"
  ) +
  theme_minimal() +
  theme(legend.position = "bottom")
```

![](man/figures/README-model_plot-1.png)<!-- -->

### Best Model

``` r
best_nested_modeltime_tbl <- nested_modeltime_tbl %>%
  modeltime_nested_select_best(
    metric = "rmse",
    minimize = TRUE,
    filter_test_forecasts = TRUE
  )

best_nested_modeltime_tbl %>%
  extract_nested_best_model_report()
```

    ## # Nested Modeltime Table
    ## 

    ## # A tibble: 8 × 10
    ##   package       .model_id .model_desc .type   mae  mape  mase smape  rmse    rsq
    ##   <fct>             <int> <chr>       <chr> <dbl> <dbl> <dbl> <dbl> <dbl>  <dbl>
    ## 1 healthyR.data         1 ARIMA       Test  0.752 105.  0.646 173.  0.917 0.176 
    ## 2 healthyR              1 ARIMA       Test  0.710 120.  0.705 149.  0.904 0.0627
    ## 3 healthyR.ts           4 NNAR        Test  0.965 104.  0.644 174.  1.16  0.0794
    ## 4 healthyverse          1 ARIMA       Test  0.631 121.  0.907 107.  0.791 0.0644
    ## 5 healthyR.ai           4 NNAR        Test  0.688  86.6 0.692 137.  0.833 0.263 
    ## 6 TidyDensity           2 LM          Test  0.662 196.  0.731 106.  0.765 0.0419
    ## 7 tidyAML               4 NNAR        Test  0.627 229.  0.651  97.9 0.748 0.0423
    ## 8 RandomWalker          1 ARIMA       Test  0.825  68.0 0.401  79.1 1.05  0.460

``` r
best_nested_modeltime_tbl %>%
  extract_nested_test_forecast() %>%
  #filter(!is.na(.model_id)) %>%
  group_by(package) %>%
  plot_modeltime_forecast(
    .interactive = FALSE,
    .conf_interval_alpha = 0.2,
    .facet_scales = "free"
  ) +
  theme_minimal() +
  theme(legend.position = "bottom")
```

![](man/figures/README-best_model-1.png)<!-- -->

## Refitting and Future Forecast

Now that we have the best models, we can make our future forecasts.

``` r
nested_modeltime_refit_tbl <- best_nested_modeltime_tbl %>%
    modeltime_nested_refit(
        control = control_nested_refit(verbose = TRUE)
    )
```

``` r
nested_modeltime_refit_tbl
```

    ## # Nested Modeltime Table
    ## 

    ## # A tibble: 8 × 5
    ##   package       .actual_data .future_data .splits           .modeltime_tables 
    ##   <fct>         <list>       <list>       <list>            <list>            
    ## 1 healthyR.data <tibble>     <tibble>     <split [1539|28]> <mdl_tm_t [1 × 5]>
    ## 2 healthyR      <tibble>     <tibble>     <split [1532|28]> <mdl_tm_t [1 × 5]>
    ## 3 healthyR.ts   <tibble>     <tibble>     <split [1476|28]> <mdl_tm_t [1 × 5]>
    ## 4 healthyverse  <tibble>     <tibble>     <split [1447|28]> <mdl_tm_t [1 × 5]>
    ## 5 healthyR.ai   <tibble>     <tibble>     <split [1271|28]> <mdl_tm_t [1 × 5]>
    ## 6 TidyDensity   <tibble>     <tibble>     <split [1122|28]> <mdl_tm_t [1 × 5]>
    ## 7 tidyAML       <tibble>     <tibble>     <split [730|28]>  <mdl_tm_t [1 × 5]>
    ## 8 RandomWalker  <tibble>     <tibble>     <split [152|28]>  <mdl_tm_t [1 × 5]>

``` r
nested_modeltime_refit_tbl %>%
  extract_nested_future_forecast() %>%
  mutate(across(.value:.conf_hi, .fns = ~ standard_inv_vec(
    x    = .,
    mean = std_mean,
    sd   = std_sd
  )$standard_inverse_value)) %>%
  mutate(across(.value:.conf_hi, .fns = ~ liiv(
    x = .,
    limit_lower = limit_lower,
    limit_upper = limit_upper,
    offset      = offset
  )$rescaled_v)) %>%
  group_by(package) %>%
  plot_modeltime_forecast(
    .interactive = FALSE,
    .conf_interval_alpha = 0.2,
    .facet_scales = "free"
  ) +
  theme_minimal() +
  theme(legend.position = "bottom")
```

![](man/figures/README-refit-1.png)<!-- -->
