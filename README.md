Time Series Analysis and Nested Modeling of the Healthyverse Packages
================
Steven P. Sanderson II, MPH - Date:
12 May, 2025

This analysis follows a *Nested Modeltime Workflow*.

## Get Data

``` r
glimpse(downloads_tbl)
```

    ## Rows: 139,651
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

The last day in the data set is 2025-05-10 23:45:42, the file was
birthed on: 2024-08-07 07:35:44, and at report knit time is -6636.17
hours old. Happy analyzing!

Now that we have our data lets take a look at it using the `skimr`
package.

``` r
skim(downloads_tbl)
```

|                                                  |               |
|:-------------------------------------------------|:--------------|
| Name                                             | downloads_tbl |
| Number of rows                                   | 139651        |
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
| r_version     |    100660 |          0.28 |   5 |   5 |     0 |       46 |          0 |
| r_arch        |    100660 |          0.28 |   3 |   7 |     0 |        5 |          0 |
| r_os          |    100660 |          0.28 |   7 |  15 |     0 |       22 |          0 |
| package       |         0 |          1.00 |   7 |  13 |     0 |        8 |          0 |
| version       |         0 |          1.00 |   5 |  17 |     0 |       60 |          0 |
| country       |     11893 |          0.91 |   2 |   2 |     0 |      163 |          0 |

**Variable type: Date**

| skim_variable | n_missing | complete_rate | min | max | median | n_unique |
|:---|---:|---:|:---|:---|:---|---:|
| date | 0 | 1 | 2020-11-23 | 2025-05-10 | 2023-06-20 | 1630 |

**Variable type: numeric**

| skim_variable | n_missing | complete_rate | mean | sd | p0 | p25 | p50 | p75 | p100 | hist |
|:---|---:|---:|---:|---:|---:|---:|---:|---:|---:|:---|
| size | 0 | 1 | 1134712.73 | 1519946.08 | 355 | 14701 | 289680 | 2367754 | 5677952 | ▇▁▂▁▁ |
| ip_id | 0 | 1 | 10387.27 | 18487.39 | 1 | 292 | 3042 | 11639 | 209747 | ▇▁▁▁▁ |

**Variable type: POSIXct**

| skim_variable | n_missing | complete_rate | min | max | median | n_unique |
|:---|---:|---:|:---|:---|:---|---:|
| date_time | 0 | 1 | 2020-11-23 09:00:41 | 2025-05-10 23:45:42 | 2023-06-20 00:05:14 | 85284 |

**Variable type: Timespan**

| skim_variable | n_missing | complete_rate | min | max |    median | n_unique |
|:--------------|----------:|--------------:|----:|----:|----------:|---------:|
| time          |         0 |             1 |   0 |  59 | 12H 7M 6S |       60 |

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
    ## -146.32  -35.68  -11.28   26.64  814.83 
    ## 
    ## Coefficients:
    ##                                                      Estimate Std. Error
    ## (Intercept)                                        -1.874e+02  7.057e+01
    ## date                                                1.144e-02  3.741e-03
    ## lag(value, 1)                                       1.031e-01  2.458e-02
    ## lag(value, 7)                                       9.651e-02  2.546e-02
    ## lag(value, 14)                                      8.991e-02  2.552e-02
    ## lag(value, 21)                                      6.723e-02  2.561e-02
    ## lag(value, 28)                                      6.975e-02  2.550e-02
    ## lag(value, 35)                                      6.632e-02  2.564e-02
    ## lag(value, 42)                                      4.582e-02  2.574e-02
    ## lag(value, 49)                                      6.732e-02  2.565e-02
    ## month(date, label = TRUE).L                        -1.004e+01  5.131e+00
    ## month(date, label = TRUE).Q                         2.920e+00  5.179e+00
    ## month(date, label = TRUE).C                        -1.246e+01  5.171e+00
    ## month(date, label = TRUE)^4                        -6.654e+00  5.200e+00
    ## month(date, label = TRUE)^5                        -1.214e+01  5.175e+00
    ## month(date, label = TRUE)^6                        -3.249e+00  5.242e+00
    ## month(date, label = TRUE)^7                        -6.427e+00  5.151e+00
    ## month(date, label = TRUE)^8                        -4.263e+00  5.152e+00
    ## month(date, label = TRUE)^9                         5.266e+00  5.148e+00
    ## month(date, label = TRUE)^10                        4.509e+00  5.188e+00
    ## month(date, label = TRUE)^11                       -5.782e+00  5.306e+00
    ## fourier_vec(date, type = "sin", K = 1, period = 7) -1.181e+01  2.362e+00
    ## fourier_vec(date, type = "cos", K = 1, period = 7)  8.304e+00  2.487e+00
    ##                                                    t value Pr(>|t|)    
    ## (Intercept)                                         -2.656 0.007986 ** 
    ## date                                                 3.057 0.002274 ** 
    ## lag(value, 1)                                        4.195 2.88e-05 ***
    ## lag(value, 7)                                        3.791 0.000156 ***
    ## lag(value, 14)                                       3.523 0.000439 ***
    ## lag(value, 21)                                       2.626 0.008737 ** 
    ## lag(value, 28)                                       2.736 0.006298 ** 
    ## lag(value, 35)                                       2.586 0.009791 ** 
    ## lag(value, 42)                                       1.780 0.075286 .  
    ## lag(value, 49)                                       2.625 0.008760 ** 
    ## month(date, label = TRUE).L                         -1.957 0.050539 .  
    ## month(date, label = TRUE).Q                          0.564 0.572990    
    ## month(date, label = TRUE).C                         -2.410 0.016083 *  
    ## month(date, label = TRUE)^4                         -1.280 0.200848    
    ## month(date, label = TRUE)^5                         -2.347 0.019051 *  
    ## month(date, label = TRUE)^6                         -0.620 0.535563    
    ## month(date, label = TRUE)^7                         -1.248 0.212326    
    ## month(date, label = TRUE)^8                         -0.827 0.408091    
    ## month(date, label = TRUE)^9                          1.023 0.306427    
    ## month(date, label = TRUE)^10                         0.869 0.384975    
    ## month(date, label = TRUE)^11                        -1.090 0.276050    
    ## fourier_vec(date, type = "sin", K = 1, period = 7)  -4.999 6.41e-07 ***
    ## fourier_vec(date, type = "cos", K = 1, period = 7)   3.339 0.000861 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 58.87 on 1558 degrees of freedom
    ##   (49 observations deleted due to missingness)
    ## Multiple R-squared:  0.2423, Adjusted R-squared:  0.2316 
    ## F-statistic: 22.65 on 22 and 1558 DF,  p-value: < 2.2e-16

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
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 84 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 3.64088995872574"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 84, 91 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 3.28853126553772"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 84, 91, 63 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 3.0437289592435"
    ## [1] "BEST method = 'lin', seasonal.factor = c( 84, 91, 63 )"
    ## [1] "BEST lin OBJECTIVE FUNCTION = 3.0437289592435"
    ## [1] "CURRNET METHOD: nonlin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'nonlin' , seasonal.factor =  c( 84, 91, 63 ) ...)"
    ## [1] "CURRENT nonlin OBJECTIVE FUNCTION = 3.68212720307295"
    ## [1] "BEST method = 'nonlin' PATH MEMBER = c( 84, 91, 63 )"
    ## [1] "BEST nonlin OBJECTIVE FUNCTION = 3.68212720307295"
    ## [1] "CURRNET METHOD: both"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'both' , seasonal.factor =  c( 84, 91, 63 ) ...)"
    ## [1] "CURRENT both OBJECTIVE FUNCTION = 3.27374489887874"
    ## [1] "BEST method = 'both' PATH MEMBER = c( 84, 91, 63 )"
    ## [1] "BEST both OBJECTIVE FUNCTION = 3.27374489887874"

![](man/figures/README-nns_forecasting-1.png)<!-- -->

    ## Package: healthyR.ai
    ## [1] "CURRNET METHOD: lin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 84 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 4.4765402250236"
    ## [1] "BEST method = 'lin', seasonal.factor = c( 84 )"
    ## [1] "BEST lin OBJECTIVE FUNCTION = 4.4765402250236"
    ## [1] "CURRNET METHOD: nonlin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'nonlin' , seasonal.factor =  c( 84 ) ...)"
    ## [1] "CURRENT nonlin OBJECTIVE FUNCTION = 5.7060096940016"
    ## [1] "BEST method = 'nonlin' PATH MEMBER = c( 84 )"
    ## [1] "BEST nonlin OBJECTIVE FUNCTION = 5.7060096940016"
    ## [1] "CURRNET METHOD: both"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'both' , seasonal.factor =  c( 84 ) ...)"
    ## [1] "CURRENT both OBJECTIVE FUNCTION = 4.97839078003559"
    ## [1] "BEST method = 'both' PATH MEMBER = c( 84 )"
    ## [1] "BEST both OBJECTIVE FUNCTION = 4.97839078003559"

![](man/figures/README-nns_forecasting-2.png)<!-- -->

    ## Package: healthyR.data
    ## [1] "CURRNET METHOD: lin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 21 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 2.75446785464002"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 21, 91 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 2.05838436829375"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 21, 91, 63 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 2.02066663938477"
    ## [1] "BEST method = 'lin', seasonal.factor = c( 21, 91, 63 )"
    ## [1] "BEST lin OBJECTIVE FUNCTION = 2.02066663938477"
    ## [1] "CURRNET METHOD: nonlin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'nonlin' , seasonal.factor =  c( 21, 91, 63 ) ...)"
    ## [1] "CURRENT nonlin OBJECTIVE FUNCTION = 1.7850073809088"
    ## [1] "BEST method = 'nonlin' PATH MEMBER = c( 21, 91, 63 )"
    ## [1] "BEST nonlin OBJECTIVE FUNCTION = 1.7850073809088"
    ## [1] "CURRNET METHOD: both"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'both' , seasonal.factor =  c( 21, 91, 63 ) ...)"
    ## [1] "CURRENT both OBJECTIVE FUNCTION = 1.69581363049881"
    ## [1] "BEST method = 'both' PATH MEMBER = c( 21, 91, 63 )"
    ## [1] "BEST both OBJECTIVE FUNCTION = 1.69581363049881"

![](man/figures/README-nns_forecasting-3.png)<!-- -->

    ## Package: healthyR.ts
    ## [1] "CURRNET METHOD: lin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 63 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 5.19266391388545"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 63, 91 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 4.43967708360788"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 63, 91, 28 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 4.43119379104369"
    ## [1] "BEST method = 'lin', seasonal.factor = c( 63, 91, 28 )"
    ## [1] "BEST lin OBJECTIVE FUNCTION = 4.43119379104369"
    ## [1] "CURRNET METHOD: nonlin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'nonlin' , seasonal.factor =  c( 63, 91, 28 ) ...)"
    ## [1] "CURRENT nonlin OBJECTIVE FUNCTION = 5.30207526838035"
    ## [1] "BEST method = 'nonlin' PATH MEMBER = c( 63, 91, 28 )"
    ## [1] "BEST nonlin OBJECTIVE FUNCTION = 5.30207526838035"
    ## [1] "CURRNET METHOD: both"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'both' , seasonal.factor =  c( 63, 91, 28 ) ...)"
    ## [1] "CURRENT both OBJECTIVE FUNCTION = 4.68335092845376"
    ## [1] "BEST method = 'both' PATH MEMBER = c( 63, 91, 28 )"
    ## [1] "BEST both OBJECTIVE FUNCTION = 4.68335092845376"

![](man/figures/README-nns_forecasting-4.png)<!-- -->

    ## Package: healthyverse
    ## [1] "CURRNET METHOD: lin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 91 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 8.45541666533892"
    ## [1] "BEST method = 'lin', seasonal.factor = c( 91 )"
    ## [1] "BEST lin OBJECTIVE FUNCTION = 8.45541666533892"
    ## [1] "CURRNET METHOD: nonlin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'nonlin' , seasonal.factor =  c( 91 ) ...)"
    ## [1] "CURRENT nonlin OBJECTIVE FUNCTION = 9.67939253632777"
    ## [1] "BEST method = 'nonlin' PATH MEMBER = c( 91 )"
    ## [1] "BEST nonlin OBJECTIVE FUNCTION = 9.67939253632777"
    ## [1] "CURRNET METHOD: both"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'both' , seasonal.factor =  c( 91 ) ...)"
    ## [1] "CURRENT both OBJECTIVE FUNCTION = 9.14356905331368"
    ## [1] "BEST method = 'both' PATH MEMBER = c( 91 )"
    ## [1] "BEST both OBJECTIVE FUNCTION = 9.14356905331368"

![](man/figures/README-nns_forecasting-5.png)<!-- -->

    ## Package: RandomWalker
    ## [1] "CURRNET METHOD: lin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 35 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 3.60283300812599"
    ## [1] "BEST method = 'lin', seasonal.factor = c( 35 )"
    ## [1] "BEST lin OBJECTIVE FUNCTION = 3.60283300812599"
    ## [1] "CURRNET METHOD: nonlin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'nonlin' , seasonal.factor =  c( 35 ) ...)"
    ## [1] "CURRENT nonlin OBJECTIVE FUNCTION = 3.22552830284089"
    ## [1] "BEST method = 'nonlin' PATH MEMBER = c( 35 )"
    ## [1] "BEST nonlin OBJECTIVE FUNCTION = 3.22552830284089"
    ## [1] "CURRNET METHOD: both"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'both' , seasonal.factor =  c( 35 ) ...)"
    ## [1] "CURRENT both OBJECTIVE FUNCTION = 3.16466649117561"
    ## [1] "BEST method = 'both' PATH MEMBER = c( 35 )"
    ## [1] "BEST both OBJECTIVE FUNCTION = 3.16466649117561"

![](man/figures/README-nns_forecasting-6.png)<!-- -->

    ## Package: tidyAML
    ## [1] "CURRNET METHOD: lin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 28 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 2.40934712130303"
    ## [1] "BEST method = 'lin', seasonal.factor = c( 28 )"
    ## [1] "BEST lin OBJECTIVE FUNCTION = 2.40934712130303"
    ## [1] "CURRNET METHOD: nonlin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'nonlin' , seasonal.factor =  c( 28 ) ...)"
    ## [1] "CURRENT nonlin OBJECTIVE FUNCTION = 3.26786378991723"
    ## [1] "BEST method = 'nonlin' PATH MEMBER = c( 28 )"
    ## [1] "BEST nonlin OBJECTIVE FUNCTION = 3.26786378991723"
    ## [1] "CURRNET METHOD: both"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'both' , seasonal.factor =  c( 28 ) ...)"
    ## [1] "CURRENT both OBJECTIVE FUNCTION = 2.53062747753238"
    ## [1] "BEST method = 'both' PATH MEMBER = c( 28 )"
    ## [1] "BEST both OBJECTIVE FUNCTION = 2.53062747753238"

![](man/figures/README-nns_forecasting-7.png)<!-- -->

    ## Package: TidyDensity
    ## [1] "CURRNET METHOD: lin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 84 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 3.31864446402313"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 84, 28 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 2.8558608419993"
    ## [1] "BEST method = 'lin', seasonal.factor = c( 84, 28 )"
    ## [1] "BEST lin OBJECTIVE FUNCTION = 2.8558608419993"
    ## [1] "CURRNET METHOD: nonlin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'nonlin' , seasonal.factor =  c( 84, 28 ) ...)"
    ## [1] "CURRENT nonlin OBJECTIVE FUNCTION = 4.6946563014903"
    ## [1] "BEST method = 'nonlin' PATH MEMBER = c( 84, 28 )"
    ## [1] "BEST nonlin OBJECTIVE FUNCTION = 4.6946563014903"
    ## [1] "CURRNET METHOD: both"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'both' , seasonal.factor =  c( 84, 28 ) ...)"
    ## [1] "CURRENT both OBJECTIVE FUNCTION = 3.21046204083564"
    ## [1] "BEST method = 'both' PATH MEMBER = c( 84, 28 )"
    ## [1] "BEST both OBJECTIVE FUNCTION = 3.21046204083564"

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
    ## 1 healthyR.data <tibble [1,623 × 2]> <tibble [28 × 2]> <split [1595|28]>
    ## 2 healthyR      <tibble [1,616 × 2]> <tibble [28 × 2]> <split [1588|28]>
    ## 3 healthyR.ts   <tibble [1,560 × 2]> <tibble [28 × 2]> <split [1532|28]>
    ## 4 healthyverse  <tibble [1,530 × 2]> <tibble [28 × 2]> <split [1502|28]>
    ## 5 healthyR.ai   <tibble [1,355 × 2]> <tibble [28 × 2]> <split [1327|28]>
    ## 6 TidyDensity   <tibble [1,206 × 2]> <tibble [28 × 2]> <split [1178|28]>
    ## 7 tidyAML       <tibble [814 × 2]>   <tibble [28 × 2]> <split [786|28]> 
    ## 8 RandomWalker  <tibble [236 × 2]>   <tibble [28 × 2]> <split [208|28]>

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
| healthyR.data | 1 | ARIMA | Test | 0.6444343 | 120.97172 | 0.6647098 | 127.71901 | 0.8447779 | 0.0686219 |
| healthyR.data | 2 | LM | Test | 0.6856014 | 195.32007 | 0.7071721 | 120.36374 | 0.8911865 | 0.0010853 |
| healthyR.data | 3 | EARTH | Test | 0.7093243 | 250.91316 | 0.7316415 | 114.76949 | 0.9305679 | 0.0010853 |
| healthyR.data | 4 | NNAR | Test | 0.7368392 | 97.75473 | 0.7600220 | 167.42855 | 0.9201285 | 0.0062219 |
| healthyR | 1 | ARIMA | Test | 0.6784576 | 107.93912 | 0.8345414 | 185.84038 | 0.8296316 | 0.0175955 |
| healthyR | 2 | LM | Test | 0.6435045 | 92.71596 | 0.7915471 | 169.04256 | 0.7953425 | 0.0498665 |
| healthyR | 3 | EARTH | Test | 0.6111642 | 89.30127 | 0.7517668 | 135.51160 | 0.7648660 | 0.0498665 |
| healthyR | 4 | NNAR | Test | 0.6761721 | 91.32624 | 0.8317301 | 145.59785 | 0.8571785 | 0.1066166 |
| healthyR.ts | 1 | ARIMA | Test | 0.7637859 | 105.51748 | 0.7202547 | 174.54865 | 0.9572158 | 0.0165851 |
| healthyR.ts | 2 | LM | Test | 0.9060094 | 247.82443 | 0.8543722 | 151.13203 | 1.1083260 | 0.0165851 |
| healthyR.ts | 3 | EARTH | Test | 0.8164171 | 344.79165 | 0.7698862 | 126.87614 | 1.0156832 | 0.0165851 |
| healthyR.ts | 4 | NNAR | Test | 0.7738193 | 100.99852 | 0.7297162 | 174.45100 | 0.9873720 | 0.1340165 |
| healthyverse | 1 | ARIMA | Test | 0.6552436 | 373.25617 | 1.0239099 | 101.04616 | 0.8048078 | 0.0614021 |
| healthyverse | 2 | LM | Test | 0.6222902 | 372.32416 | 0.9724156 | 96.56636 | 0.7572681 | 0.1384676 |
| healthyverse | 3 | EARTH | Test | 0.7503635 | 552.29469 | 1.1725480 | 99.45177 | 0.8625327 | 0.1384676 |
| healthyverse | 4 | NNAR | Test | 0.6485048 | 241.84935 | 1.0133796 | 111.52255 | 0.8122032 | 0.0484327 |
| healthyR.ai | 1 | ARIMA | Test | 0.7736213 | 124.04394 | 0.9035533 | 174.75905 | 1.0229612 | 0.0000991 |
| healthyR.ai | 2 | LM | Test | 0.7633189 | 138.85228 | 0.8915207 | 159.07348 | 1.0367508 | 0.0010044 |
| healthyR.ai | 3 | EARTH | Test | 0.7679583 | 149.76823 | 0.8969392 | 151.36650 | 1.0519285 | 0.0010044 |
| healthyR.ai | 4 | NNAR | Test | 0.7876588 | 150.33594 | 0.9199485 | 159.16651 | 1.0591366 | 0.0125596 |
| TidyDensity | 1 | ARIMA | Test | 0.5122509 | 261.01602 | 0.9599250 | 109.80438 | 0.6295958 | 0.1232909 |
| TidyDensity | 2 | LM | Test | 0.5539252 | 364.12401 | 1.0380199 | 106.63631 | 0.6688187 | 0.0563363 |
| TidyDensity | 3 | EARTH | Test | 0.4729445 | 236.45303 | 0.8862674 | 107.52109 | 0.5740568 | 0.0563363 |
| TidyDensity | 4 | NNAR | Test | 0.5384552 | 149.53709 | 1.0090301 | 127.63631 | 0.6997179 | 0.1198915 |
| tidyAML | 1 | ARIMA | Test | 0.6580687 | 195.08925 | 0.8956477 | 100.15907 | 0.8021607 | 0.0065104 |
| tidyAML | 2 | LM | Test | 0.6498242 | 198.01564 | 0.8844266 | 95.49967 | 0.7850612 | 0.1494709 |
| tidyAML | 3 | EARTH | Test | 0.6510087 | 197.37990 | 0.8860387 | 95.72602 | 0.7860190 | 0.1494709 |
| tidyAML | 4 | NNAR | Test | 0.6748095 | 190.19741 | 0.9184323 | 102.03437 | 0.8341309 | 0.0338119 |
| RandomWalker | 1 | ARIMA | Test | 1.1621688 | 153.87193 | 0.5699553 | 139.42217 | 1.3962089 | 0.1263230 |
| RandomWalker | 2 | LM | Test | 1.2542265 | 107.83437 | 0.6151025 | 182.88098 | 1.4743768 | 0.0138485 |
| RandomWalker | 3 | EARTH | Test | 1.2546646 | 111.36615 | 0.6153174 | 175.64506 | 1.4841542 | NA |
| RandomWalker | 4 | NNAR | Test | 1.4976228 | 293.27452 | 0.7344699 | 158.20344 | 1.6552527 | 0.0001795 |

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
    ##   package      .model_id .model_desc .type   mae  mape  mase smape  rmse     rsq
    ##   <fct>            <int> <chr>       <chr> <dbl> <dbl> <dbl> <dbl> <dbl>   <dbl>
    ## 1 healthyR.da…         1 ARIMA       Test  0.644 121.  0.665 128.  0.845 6.86e-2
    ## 2 healthyR             3 EARTH       Test  0.611  89.3 0.752 136.  0.765 4.99e-2
    ## 3 healthyR.ts          1 ARIMA       Test  0.764 106.  0.720 175.  0.957 1.66e-2
    ## 4 healthyverse         2 LM          Test  0.622 372.  0.972  96.6 0.757 1.38e-1
    ## 5 healthyR.ai          1 ARIMA       Test  0.774 124.  0.904 175.  1.02  9.91e-5
    ## 6 TidyDensity          3 EARTH       Test  0.473 236.  0.886 108.  0.574 5.63e-2
    ## 7 tidyAML              2 LM          Test  0.650 198.  0.884  95.5 0.785 1.49e-1
    ## 8 RandomWalker         1 ARIMA       Test  1.16  154.  0.570 139.  1.40  1.26e-1

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
    ## 1 healthyR.data <tibble>     <tibble>     <split [1595|28]> <mdl_tm_t [1 × 5]>
    ## 2 healthyR      <tibble>     <tibble>     <split [1588|28]> <mdl_tm_t [1 × 5]>
    ## 3 healthyR.ts   <tibble>     <tibble>     <split [1532|28]> <mdl_tm_t [1 × 5]>
    ## 4 healthyverse  <tibble>     <tibble>     <split [1502|28]> <mdl_tm_t [1 × 5]>
    ## 5 healthyR.ai   <tibble>     <tibble>     <split [1327|28]> <mdl_tm_t [1 × 5]>
    ## 6 TidyDensity   <tibble>     <tibble>     <split [1178|28]> <mdl_tm_t [1 × 5]>
    ## 7 tidyAML       <tibble>     <tibble>     <split [786|28]>  <mdl_tm_t [1 × 5]>
    ## 8 RandomWalker  <tibble>     <tibble>     <split [208|28]>  <mdl_tm_t [1 × 5]>

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
