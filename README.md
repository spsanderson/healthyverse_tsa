Time Series Analysis and Nested Modeling of the Healthyverse Packages
================
Steven P. Sanderson II, MPH - Date:
11 June, 2025

This analysis follows a *Nested Modeltime Workflow*.

## Get Data

``` r
glimpse(downloads_tbl)
```

    ## Rows: 142,034
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

The last day in the data set is 2025-06-09 23:46:08, the file was
birthed on: 2024-08-07 07:35:44, and at report knit time is -7356.17
hours old. Happy analyzing!

Now that we have our data lets take a look at it using the `skimr`
package.

``` r
skim(downloads_tbl)
```

|                                                  |               |
|:-------------------------------------------------|:--------------|
| Name                                             | downloads_tbl |
| Number of rows                                   | 142034        |
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
| r_version     |    102480 |          0.28 |   5 |   5 |     0 |       47 |          0 |
| r_arch        |    102480 |          0.28 |   3 |   7 |     0 |        5 |          0 |
| r_os          |    102480 |          0.28 |   7 |  15 |     0 |       22 |          0 |
| package       |         0 |          1.00 |   7 |  13 |     0 |        8 |          0 |
| version       |         0 |          1.00 |   5 |  17 |     0 |       60 |          0 |
| country       |     12069 |          0.92 |   2 |   2 |     0 |      163 |          0 |

**Variable type: Date**

| skim_variable | n_missing | complete_rate | min | max | median | n_unique |
|:---|---:|---:|:---|:---|:---|---:|
| date | 0 | 1 | 2020-11-23 | 2025-06-09 | 2023-07-02 | 1660 |

**Variable type: numeric**

| skim_variable | n_missing | complete_rate | mean | sd | p0 | p25 | p50 | p75 | p100 | hist |
|:---|---:|---:|---:|---:|---:|---:|---:|---:|---:|:---|
| size | 0 | 1 | 1133298.59 | 1516805.65 | 355 | 14701 | 289710 | 2367727 | 5677952 | ▇▁▂▁▁ |
| ip_id | 0 | 1 | 10427.14 | 18576.09 | 1 | 280 | 3030 | 11655 | 209747 | ▇▁▁▁▁ |

**Variable type: POSIXct**

| skim_variable | n_missing | complete_rate | min | max | median | n_unique |
|:---|---:|---:|:---|:---|:---|---:|
| date_time | 0 | 1 | 2020-11-23 09:00:41 | 2025-06-09 23:46:08 | 2023-07-02 02:44:46 | 87009 |

**Variable type: Timespan**

| skim_variable | n_missing | complete_rate | min | max | median | n_unique |
|:--------------|----------:|--------------:|----:|----:|-------:|---------:|
| time          |         0 |             1 |   0 |  59 |     39 |       60 |

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
    ## -147.18  -35.71  -10.99   26.93  814.99 
    ## 
    ## Coefficients:
    ##                                                      Estimate Std. Error
    ## (Intercept)                                        -1.592e+02  6.786e+01
    ## date                                                9.929e-03  3.595e-03
    ## lag(value, 1)                                       1.048e-01  2.433e-02
    ## lag(value, 7)                                       9.616e-02  2.521e-02
    ## lag(value, 14)                                      8.872e-02  2.520e-02
    ## lag(value, 21)                                      6.822e-02  2.527e-02
    ## lag(value, 28)                                      7.071e-02  2.516e-02
    ## lag(value, 35)                                      6.684e-02  2.526e-02
    ## lag(value, 42)                                      5.003e-02  2.537e-02
    ## lag(value, 49)                                      6.964e-02  2.525e-02
    ## month(date, label = TRUE).L                        -9.595e+00  5.107e+00
    ## month(date, label = TRUE).Q                         3.975e+00  5.110e+00
    ## month(date, label = TRUE).C                        -1.344e+01  5.129e+00
    ## month(date, label = TRUE)^4                        -7.261e+00  5.160e+00
    ## month(date, label = TRUE)^5                        -1.100e+01  5.113e+00
    ## month(date, label = TRUE)^6                        -3.395e+00  5.205e+00
    ## month(date, label = TRUE)^7                        -7.543e+00  5.089e+00
    ## month(date, label = TRUE)^8                        -3.459e+00  5.092e+00
    ## month(date, label = TRUE)^9                         5.921e+00  5.099e+00
    ## month(date, label = TRUE)^10                        2.979e+00  5.075e+00
    ## month(date, label = TRUE)^11                       -4.681e+00  5.178e+00
    ## fourier_vec(date, type = "sin", K = 1, period = 7) -1.159e+01  2.328e+00
    ## fourier_vec(date, type = "cos", K = 1, period = 7)  7.939e+00  2.448e+00
    ##                                                    t value Pr(>|t|)    
    ## (Intercept)                                         -2.346 0.019084 *  
    ## date                                                 2.762 0.005806 ** 
    ## lag(value, 1)                                        4.309 1.74e-05 ***
    ## lag(value, 7)                                        3.815 0.000141 ***
    ## lag(value, 14)                                       3.521 0.000442 ***
    ## lag(value, 21)                                       2.699 0.007022 ** 
    ## lag(value, 28)                                       2.810 0.005011 ** 
    ## lag(value, 35)                                       2.646 0.008217 ** 
    ## lag(value, 42)                                       1.972 0.048804 *  
    ## lag(value, 49)                                       2.758 0.005888 ** 
    ## month(date, label = TRUE).L                         -1.879 0.060425 .  
    ## month(date, label = TRUE).Q                          0.778 0.436749    
    ## month(date, label = TRUE).C                         -2.621 0.008854 ** 
    ## month(date, label = TRUE)^4                         -1.407 0.159598    
    ## month(date, label = TRUE)^5                         -2.152 0.031581 *  
    ## month(date, label = TRUE)^6                         -0.652 0.514335    
    ## month(date, label = TRUE)^7                         -1.482 0.138458    
    ## month(date, label = TRUE)^8                         -0.679 0.497060    
    ## month(date, label = TRUE)^9                          1.161 0.245754    
    ## month(date, label = TRUE)^10                         0.587 0.557303    
    ## month(date, label = TRUE)^11                        -0.904 0.366127    
    ## fourier_vec(date, type = "sin", K = 1, period = 7)  -4.978 7.13e-07 ***
    ## fourier_vec(date, type = "cos", K = 1, period = 7)   3.243 0.001209 ** 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 58.69 on 1588 degrees of freedom
    ##   (49 observations deleted due to missingness)
    ## Multiple R-squared:  0.2388, Adjusted R-squared:  0.2283 
    ## F-statistic: 22.65 on 22 and 1588 DF,  p-value: < 2.2e-16

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
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 21 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 2.89450799647126"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 21, 63 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 2.55436621693604"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 21, 63, 56 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 2.51746914891832"
    ## [1] "BEST method = 'lin', seasonal.factor = c( 21, 63, 56 )"
    ## [1] "BEST lin OBJECTIVE FUNCTION = 2.51746914891832"
    ## [1] "CURRNET METHOD: nonlin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'nonlin' , seasonal.factor =  c( 21, 63, 56 ) ...)"
    ## [1] "CURRENT nonlin OBJECTIVE FUNCTION = 5.0863282220157"
    ## [1] "BEST method = 'nonlin' PATH MEMBER = c( 21, 63, 56 )"
    ## [1] "BEST nonlin OBJECTIVE FUNCTION = 5.0863282220157"
    ## [1] "CURRNET METHOD: both"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'both' , seasonal.factor =  c( 21, 63, 56 ) ...)"
    ## [1] "CURRENT both OBJECTIVE FUNCTION = 3.16414176533463"
    ## [1] "BEST method = 'both' PATH MEMBER = c( 21, 63, 56 )"
    ## [1] "BEST both OBJECTIVE FUNCTION = 3.16414176533463"

![](man/figures/README-nns_forecasting-1.png)<!-- -->

    ## Package: healthyR.ai
    ## [1] "CURRNET METHOD: lin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 63 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 3.76072900712355"
    ## [1] "BEST method = 'lin', seasonal.factor = c( 63 )"
    ## [1] "BEST lin OBJECTIVE FUNCTION = 3.76072900712355"
    ## [1] "CURRNET METHOD: nonlin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'nonlin' , seasonal.factor =  c( 63 ) ...)"
    ## [1] "CURRENT nonlin OBJECTIVE FUNCTION = 4.08904326650757"
    ## [1] "BEST method = 'nonlin' PATH MEMBER = c( 63 )"
    ## [1] "BEST nonlin OBJECTIVE FUNCTION = 4.08904326650757"
    ## [1] "CURRNET METHOD: both"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'both' , seasonal.factor =  c( 63 ) ...)"
    ## [1] "CURRENT both OBJECTIVE FUNCTION = 3.78592294896587"
    ## [1] "BEST method = 'both' PATH MEMBER = c( 63 )"
    ## [1] "BEST both OBJECTIVE FUNCTION = 3.78592294896587"

![](man/figures/README-nns_forecasting-2.png)<!-- -->

    ## Package: healthyR.data
    ## [1] "CURRNET METHOD: lin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 84 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 2.81969718419297"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 84, 63 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 2.3134055959833"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 84, 63, 91 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 2.07974673291271"
    ## [1] "BEST method = 'lin', seasonal.factor = c( 84, 63, 91 )"
    ## [1] "BEST lin OBJECTIVE FUNCTION = 2.07974673291271"
    ## [1] "CURRNET METHOD: nonlin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'nonlin' , seasonal.factor =  c( 84, 63, 91 ) ...)"
    ## [1] "CURRENT nonlin OBJECTIVE FUNCTION = 2.44309979269248"
    ## [1] "BEST method = 'nonlin' PATH MEMBER = c( 84, 63, 91 )"
    ## [1] "BEST nonlin OBJECTIVE FUNCTION = 2.44309979269248"
    ## [1] "CURRNET METHOD: both"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'both' , seasonal.factor =  c( 84, 63, 91 ) ...)"
    ## [1] "CURRENT both OBJECTIVE FUNCTION = 2.11907154552595"
    ## [1] "BEST method = 'both' PATH MEMBER = c( 84, 63, 91 )"
    ## [1] "BEST both OBJECTIVE FUNCTION = 2.11907154552595"

![](man/figures/README-nns_forecasting-3.png)<!-- -->

    ## Package: healthyR.ts
    ## [1] "CURRNET METHOD: lin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 21 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 3.22492153981763"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 21, 56 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 2.80915090668459"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 21, 56, 63 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 2.68561975723904"
    ## [1] "BEST method = 'lin', seasonal.factor = c( 21, 56, 63 )"
    ## [1] "BEST lin OBJECTIVE FUNCTION = 2.68561975723904"
    ## [1] "CURRNET METHOD: nonlin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'nonlin' , seasonal.factor =  c( 21, 56, 63 ) ...)"
    ## [1] "CURRENT nonlin OBJECTIVE FUNCTION = 5.17721935440302"
    ## [1] "BEST method = 'nonlin' PATH MEMBER = c( 21, 56, 63 )"
    ## [1] "BEST nonlin OBJECTIVE FUNCTION = 5.17721935440302"
    ## [1] "CURRNET METHOD: both"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'both' , seasonal.factor =  c( 21, 56, 63 ) ...)"
    ## [1] "CURRENT both OBJECTIVE FUNCTION = 3.10377195126299"
    ## [1] "BEST method = 'both' PATH MEMBER = c( 21, 56, 63 )"
    ## [1] "BEST both OBJECTIVE FUNCTION = 3.10377195126299"

![](man/figures/README-nns_forecasting-4.png)<!-- -->

    ## Package: healthyverse
    ## [1] "CURRNET METHOD: lin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 56 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 4.47283525470645"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 56, 35 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 4.37446944410135"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 56, 35, 28 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 4.3357247058933"
    ## [1] "BEST method = 'lin', seasonal.factor = c( 56, 35, 28 )"
    ## [1] "BEST lin OBJECTIVE FUNCTION = 4.3357247058933"
    ## [1] "CURRNET METHOD: nonlin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'nonlin' , seasonal.factor =  c( 56, 35, 28 ) ...)"
    ## [1] "CURRENT nonlin OBJECTIVE FUNCTION = 5.59387868639517"
    ## [1] "BEST method = 'nonlin' PATH MEMBER = c( 56, 35, 28 )"
    ## [1] "BEST nonlin OBJECTIVE FUNCTION = 5.59387868639517"
    ## [1] "CURRNET METHOD: both"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'both' , seasonal.factor =  c( 56, 35, 28 ) ...)"
    ## [1] "CURRENT both OBJECTIVE FUNCTION = 4.58978079760001"
    ## [1] "BEST method = 'both' PATH MEMBER = c( 56, 35, 28 )"
    ## [1] "BEST both OBJECTIVE FUNCTION = 4.58978079760001"

![](man/figures/README-nns_forecasting-5.png)<!-- -->

    ## Package: RandomWalker
    ## [1] "CURRNET METHOD: lin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 63 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 3.37667338316766"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 63, 70 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 3.22404689697736"
    ## [1] "BEST method = 'lin', seasonal.factor = c( 63, 70 )"
    ## [1] "BEST lin OBJECTIVE FUNCTION = 3.22404689697736"
    ## [1] "CURRNET METHOD: nonlin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'nonlin' , seasonal.factor =  c( 63, 70 ) ...)"
    ## [1] "CURRENT nonlin OBJECTIVE FUNCTION = 2.84440090511113"
    ## [1] "BEST method = 'nonlin' PATH MEMBER = c( 63, 70 )"
    ## [1] "BEST nonlin OBJECTIVE FUNCTION = 2.84440090511113"
    ## [1] "CURRNET METHOD: both"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'both' , seasonal.factor =  c( 63, 70 ) ...)"
    ## [1] "CURRENT both OBJECTIVE FUNCTION = 2.87297369742691"
    ## [1] "BEST method = 'both' PATH MEMBER = c( 63, 70 )"
    ## [1] "BEST both OBJECTIVE FUNCTION = 2.87297369742691"

![](man/figures/README-nns_forecasting-6.png)<!-- -->

    ## Package: tidyAML
    ## [1] "CURRNET METHOD: lin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 84 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 5.99415352932155"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 84, 91 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 5.85906456877955"
    ## [1] "BEST method = 'lin', seasonal.factor = c( 84, 91 )"
    ## [1] "BEST lin OBJECTIVE FUNCTION = 5.85906456877955"
    ## [1] "CURRNET METHOD: nonlin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'nonlin' , seasonal.factor =  c( 84, 91 ) ...)"
    ## [1] "CURRENT nonlin OBJECTIVE FUNCTION = 3.92801814575168"
    ## [1] "BEST method = 'nonlin' PATH MEMBER = c( 84, 91 )"
    ## [1] "BEST nonlin OBJECTIVE FUNCTION = 3.92801814575168"
    ## [1] "CURRNET METHOD: both"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'both' , seasonal.factor =  c( 84, 91 ) ...)"
    ## [1] "CURRENT both OBJECTIVE FUNCTION = 4.59907372821558"
    ## [1] "BEST method = 'both' PATH MEMBER = c( 84, 91 )"
    ## [1] "BEST both OBJECTIVE FUNCTION = 4.59907372821558"

![](man/figures/README-nns_forecasting-7.png)<!-- -->

    ## Package: TidyDensity
    ## [1] "CURRNET METHOD: lin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 63 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 3.73548949228323"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 63, 56 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 3.42461039681502"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 63, 56, 21 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 3.34456853655251"
    ## [1] "BEST method = 'lin', seasonal.factor = c( 63, 56, 21 )"
    ## [1] "BEST lin OBJECTIVE FUNCTION = 3.34456853655251"
    ## [1] "CURRNET METHOD: nonlin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'nonlin' , seasonal.factor =  c( 63, 56, 21 ) ...)"
    ## [1] "CURRENT nonlin OBJECTIVE FUNCTION = 4.65610613151416"
    ## [1] "BEST method = 'nonlin' PATH MEMBER = c( 63, 56, 21 )"
    ## [1] "BEST nonlin OBJECTIVE FUNCTION = 4.65610613151416"
    ## [1] "CURRNET METHOD: both"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'both' , seasonal.factor =  c( 63, 56, 21 ) ...)"
    ## [1] "CURRENT both OBJECTIVE FUNCTION = 3.47321377378165"
    ## [1] "BEST method = 'both' PATH MEMBER = c( 63, 56, 21 )"
    ## [1] "BEST both OBJECTIVE FUNCTION = 3.47321377378165"

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
    ## 1 healthyR.data <tibble [1,652 × 2]> <tibble [28 × 2]> <split [1624|28]>
    ## 2 healthyR      <tibble [1,646 × 2]> <tibble [28 × 2]> <split [1618|28]>
    ## 3 healthyR.ts   <tibble [1,590 × 2]> <tibble [28 × 2]> <split [1562|28]>
    ## 4 healthyverse  <tibble [1,560 × 2]> <tibble [28 × 2]> <split [1532|28]>
    ## 5 healthyR.ai   <tibble [1,385 × 2]> <tibble [28 × 2]> <split [1357|28]>
    ## 6 TidyDensity   <tibble [1,236 × 2]> <tibble [28 × 2]> <split [1208|28]>
    ## 7 tidyAML       <tibble [844 × 2]>   <tibble [28 × 2]> <split [816|28]> 
    ## 8 RandomWalker  <tibble [266 × 2]>   <tibble [28 × 2]> <split [238|28]>

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
| healthyR.data | 1 | ARIMA | Test | 0.5476222 | 110.24542 | 0.6375703 | 118.89067 | 0.7536449 | 0.1136212 |
| healthyR.data | 2 | LM | Test | 0.6081162 | 178.64327 | 0.7080005 | 120.69614 | 0.7595557 | 0.0003039 |
| healthyR.data | 3 | EARTH | Test | 0.6482905 | 247.20979 | 0.7547735 | 117.03469 | 0.7580230 | 0.0003039 |
| healthyR.data | 4 | NNAR | Test | 0.6608111 | 98.91127 | 0.7693506 | 175.78284 | 0.8909841 | 0.0391220 |
| healthyR | 1 | ARIMA | Test | 0.6574880 | 147.14046 | 0.8129698 | 162.70461 | 0.8096188 | 0.0355934 |
| healthyR | 2 | LM | Test | 0.6351490 | 99.19329 | 0.7853480 | 166.73704 | 0.8224152 | 0.0355314 |
| healthyR | 3 | EARTH | Test | 0.6168446 | 142.52600 | 0.7627150 | 138.01649 | 0.7576738 | 0.0355314 |
| healthyR | 4 | NNAR | Test | 0.6601091 | 148.80384 | 0.8162106 | 166.46343 | 0.8173776 | 0.0169235 |
| healthyR.ts | 1 | ARIMA | Test | 0.7347524 | 113.03345 | 0.9140189 | 166.25360 | 0.9167108 | 0.0020889 |
| healthyR.ts | 2 | LM | Test | 0.8631969 | 157.55579 | 1.0738015 | 150.17715 | 1.0818161 | 0.0205920 |
| healthyR.ts | 3 | EARTH | Test | 0.5899198 | 170.89899 | 0.7338497 | 96.00185 | 0.7607616 | 0.0205920 |
| healthyR.ts | 4 | NNAR | Test | 0.7328946 | 103.28845 | 0.9117078 | 174.03086 | 0.9335583 | 0.0000222 |
| healthyverse | 1 | ARIMA | Test | 0.5646179 | 109.51778 | 1.0747664 | 73.00148 | 0.7377056 | 0.0265485 |
| healthyverse | 2 | LM | Test | 0.5446579 | 119.39819 | 1.0367720 | 69.67308 | 0.6783840 | 0.0339079 |
| healthyverse | 3 | EARTH | Test | 0.5503281 | 97.16688 | 1.0475654 | 71.77974 | 0.7312233 | NA |
| healthyverse | 4 | NNAR | Test | 0.6494362 | 91.58103 | 1.2362205 | 89.49089 | 0.8436260 | 0.0797249 |
| healthyR.ai | 1 | ARIMA | Test | 0.5834212 | 109.61550 | 0.8601264 | 156.73230 | 0.7510662 | 0.0013902 |
| healthyR.ai | 2 | LM | Test | 0.5400744 | 94.57645 | 0.7962211 | 126.03738 | 0.7059174 | 0.0107255 |
| healthyR.ai | 3 | EARTH | Test | 2.1056185 | 978.41453 | 3.1042719 | 134.79458 | 2.3786928 | 0.0107255 |
| healthyR.ai | 4 | NNAR | Test | 0.5664970 | 144.29251 | 0.8351754 | 131.38948 | 0.7326022 | 0.0570434 |
| TidyDensity | 1 | ARIMA | Test | 0.4805487 | 122.78672 | 0.9659151 | 109.84229 | 0.6310286 | 0.0004270 |
| TidyDensity | 2 | LM | Test | 0.6344667 | 212.46318 | 1.2752941 | 114.32305 | 0.8067611 | 0.0127696 |
| TidyDensity | 3 | EARTH | Test | 0.4873236 | 128.32711 | 0.9795327 | 108.50449 | 0.6356495 | 0.0127696 |
| TidyDensity | 4 | NNAR | Test | 0.4483564 | 88.89676 | 0.9012077 | 121.21968 | 0.5696629 | 0.0114406 |
| tidyAML | 1 | ARIMA | Test | 0.8051188 | 137.26347 | 0.9067609 | 110.97368 | 1.0390322 | 0.0380752 |
| tidyAML | 2 | LM | Test | 0.7788560 | 137.43356 | 0.8771826 | 101.00106 | 1.0823930 | 0.3748486 |
| tidyAML | 3 | EARTH | Test | 0.7465345 | 104.65636 | 0.8407807 | 107.79903 | 1.0307711 | 0.3748486 |
| tidyAML | 4 | NNAR | Test | 0.7412524 | 131.04602 | 0.8348318 | 107.81310 | 0.9676554 | 0.2044000 |
| RandomWalker | 1 | ARIMA | Test | 1.2020497 | 123.87125 | 0.6515246 | 132.38082 | 1.4657854 | 0.0188988 |
| RandomWalker | 2 | LM | Test | 1.2636765 | 110.52477 | 0.6849271 | 190.82474 | 1.4317614 | 0.0135042 |
| RandomWalker | 3 | EARTH | Test | 1.2069449 | 97.76458 | 0.6541779 | 172.72283 | 1.4291344 | NA |
| RandomWalker | 4 | NNAR | Test | 1.3473267 | 194.63064 | 0.7302664 | 168.06127 | 1.4534412 | 0.0232760 |

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
    ## 1 healthyR.da…         1 ARIMA       Test  0.548 110.  0.638 119.  0.754  0.114 
    ## 2 healthyR             3 EARTH       Test  0.617 143.  0.763 138.  0.758  0.0355
    ## 3 healthyR.ts          3 EARTH       Test  0.590 171.  0.734  96.0 0.761  0.0206
    ## 4 healthyverse         2 LM          Test  0.545 119.  1.04   69.7 0.678  0.0339
    ## 5 healthyR.ai          2 LM          Test  0.540  94.6 0.796 126.  0.706  0.0107
    ## 6 TidyDensity          4 NNAR        Test  0.448  88.9 0.901 121.  0.570  0.0114
    ## 7 tidyAML              4 NNAR        Test  0.741 131.  0.835 108.  0.968  0.204 
    ## 8 RandomWalker         3 EARTH       Test  1.21   97.8 0.654 173.  1.43  NA

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
    ## 1 healthyR.data <tibble>     <tibble>     <split [1624|28]> <mdl_tm_t [1 × 5]>
    ## 2 healthyR      <tibble>     <tibble>     <split [1618|28]> <mdl_tm_t [1 × 5]>
    ## 3 healthyR.ts   <tibble>     <tibble>     <split [1562|28]> <mdl_tm_t [1 × 5]>
    ## 4 healthyverse  <tibble>     <tibble>     <split [1532|28]> <mdl_tm_t [1 × 5]>
    ## 5 healthyR.ai   <tibble>     <tibble>     <split [1357|28]> <mdl_tm_t [1 × 5]>
    ## 6 TidyDensity   <tibble>     <tibble>     <split [1208|28]> <mdl_tm_t [1 × 5]>
    ## 7 tidyAML       <tibble>     <tibble>     <split [816|28]>  <mdl_tm_t [1 × 5]>
    ## 8 RandomWalker  <tibble>     <tibble>     <split [238|28]>  <mdl_tm_t [1 × 5]>

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
