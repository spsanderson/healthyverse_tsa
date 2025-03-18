Time Series Analysis and Nested Modeling of the Healthyverse Packages
================
Steven P. Sanderson II, MPH - Date:
18 March, 2025

This analysis follows a *Nested Modeltime Workflow*.

## Get Data

``` r
glimpse(downloads_tbl)
```

    ## Rows: 134,384
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

The last day in the data set is 2025-03-16 23:08:56, the file was
birthed on: 2024-08-07 07:35:44, and at report knit time is -5315.55
hours old. Happy analyzing!

Now that we have our data lets take a look at it using the `skimr`
package.

``` r
skim(downloads_tbl)
```

|                                                  |               |
|:-------------------------------------------------|:--------------|
| Name                                             | downloads_tbl |
| Number of rows                                   | 134384        |
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
| r_version     |     96563 |          0.28 |   5 |   5 |     0 |       46 |          0 |
| r_arch        |     96563 |          0.28 |   3 |   7 |     0 |        5 |          0 |
| r_os          |     96563 |          0.28 |   7 |  15 |     0 |       21 |          0 |
| package       |         0 |          1.00 |   7 |  13 |     0 |        8 |          0 |
| version       |         0 |          1.00 |   5 |  17 |     0 |       60 |          0 |
| country       |     11313 |          0.92 |   2 |   2 |     0 |      163 |          0 |

**Variable type: Date**

| skim_variable | n_missing | complete_rate | min | max | median | n_unique |
|:---|---:|---:|:---|:---|:---|---:|
| date | 0 | 1 | 2020-11-23 | 2025-03-16 | 2023-05-19 | 1575 |

**Variable type: numeric**

| skim_variable | n_missing | complete_rate | mean | sd | p0 | p25 | p50 | p75 | p100 | hist |
|:---|---:|---:|---:|---:|---:|---:|---:|---:|---:|:---|
| size | 0 | 1 | 1135789.81 | 1525540.17 | 355 | 14701 | 260647.5 | 2367783 | 5677952 | ▇▁▂▁▁ |
| ip_id | 0 | 1 | 10374.08 | 18354.51 | 1 | 303 | 3091.0 | 11799 | 209747 | ▇▁▁▁▁ |

**Variable type: POSIXct**

| skim_variable | n_missing | complete_rate | min | max | median | n_unique |
|:---|---:|---:|:---|:---|:---|---:|
| date_time | 0 | 1 | 2020-11-23 09:00:41 | 2025-03-16 23:08:56 | 2023-05-19 07:21:05 | 81544 |

**Variable type: Timespan**

| skim_variable | n_missing | complete_rate | min | max | median | n_unique |
|:--------------|----------:|--------------:|----:|----:|-------:|---------:|
| time          |         0 |             1 |   0 |  59 |      1 |       60 |

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
    ## -152.99  -35.19  -10.15   27.23  810.99 
    ## 
    ## Coefficients:
    ##                                                      Estimate Std. Error
    ## (Intercept)                                        -2.005e+02  7.492e+01
    ## date                                                1.200e-02  3.975e-03
    ## lag(value, 1)                                       1.092e-01  2.497e-02
    ## lag(value, 7)                                       9.222e-02  2.585e-02
    ## lag(value, 14)                                      9.552e-02  2.589e-02
    ## lag(value, 21)                                      6.460e-02  2.595e-02
    ## lag(value, 28)                                      6.341e-02  2.595e-02
    ## lag(value, 35)                                      7.127e-02  2.608e-02
    ## lag(value, 42)                                      5.334e-02  2.621e-02
    ## lag(value, 49)                                      8.466e-02  2.613e-02
    ## month(date, label = TRUE).L                        -1.152e+01  5.171e+00
    ## month(date, label = TRUE).Q                         2.473e+00  5.194e+00
    ## month(date, label = TRUE).C                        -1.154e+01  5.259e+00
    ## month(date, label = TRUE)^4                        -7.923e+00  5.224e+00
    ## month(date, label = TRUE)^5                        -1.212e+01  5.207e+00
    ## month(date, label = TRUE)^6                        -2.615e+00  5.287e+00
    ## month(date, label = TRUE)^7                        -7.325e+00  5.194e+00
    ## month(date, label = TRUE)^8                        -3.816e+00  5.226e+00
    ## month(date, label = TRUE)^9                         5.138e+00  5.274e+00
    ## month(date, label = TRUE)^10                        4.749e+00  5.308e+00
    ## month(date, label = TRUE)^11                       -6.131e+00  5.332e+00
    ## fourier_vec(date, type = "sin", K = 1, period = 7) -1.184e+01  2.402e+00
    ## fourier_vec(date, type = "cos", K = 1, period = 7)  7.972e+00  2.530e+00
    ##                                                    t value Pr(>|t|)    
    ## (Intercept)                                         -2.676 0.007543 ** 
    ## date                                                 3.019 0.002578 ** 
    ## lag(value, 1)                                        4.376 1.29e-05 ***
    ## lag(value, 7)                                        3.567 0.000372 ***
    ## lag(value, 14)                                       3.689 0.000233 ***
    ## lag(value, 21)                                       2.489 0.012912 *  
    ## lag(value, 28)                                       2.443 0.014666 *  
    ## lag(value, 35)                                       2.733 0.006358 ** 
    ## lag(value, 42)                                       2.035 0.042056 *  
    ## lag(value, 49)                                       3.240 0.001221 ** 
    ## month(date, label = TRUE).L                         -2.227 0.026081 *  
    ## month(date, label = TRUE).Q                          0.476 0.633991    
    ## month(date, label = TRUE).C                         -2.194 0.028392 *  
    ## month(date, label = TRUE)^4                         -1.517 0.129566    
    ## month(date, label = TRUE)^5                         -2.328 0.020032 *  
    ## month(date, label = TRUE)^6                         -0.495 0.620903    
    ## month(date, label = TRUE)^7                         -1.410 0.158648    
    ## month(date, label = TRUE)^8                         -0.730 0.465469    
    ## month(date, label = TRUE)^9                          0.974 0.330166    
    ## month(date, label = TRUE)^10                         0.895 0.371129    
    ## month(date, label = TRUE)^11                        -1.150 0.250329    
    ## fourier_vec(date, type = "sin", K = 1, period = 7)  -4.928 9.22e-07 ***
    ## fourier_vec(date, type = "cos", K = 1, period = 7)   3.151 0.001656 ** 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 58.53 on 1503 degrees of freedom
    ##   (49 observations deleted due to missingness)
    ## Multiple R-squared:  0.2575, Adjusted R-squared:  0.2467 
    ## F-statistic:  23.7 on 22 and 1503 DF,  p-value: < 2.2e-16

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
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 1.40412087929326"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 77, 98 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 1.24773427442261"
    ## [1] "BEST method = 'lin', seasonal.factor = c( 77, 98 )"
    ## [1] "BEST lin OBJECTIVE FUNCTION = 1.24773427442261"
    ## [1] "CURRNET METHOD: nonlin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'nonlin' , seasonal.factor =  c( 77, 98 ) ...)"
    ## [1] "CURRENT nonlin OBJECTIVE FUNCTION = 1.56114410144178"
    ## [1] "BEST method = 'nonlin' PATH MEMBER = c( 77, 98 )"
    ## [1] "BEST nonlin OBJECTIVE FUNCTION = 1.56114410144178"
    ## [1] "CURRNET METHOD: both"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'both' , seasonal.factor =  c( 77, 98 ) ...)"
    ## [1] "CURRENT both OBJECTIVE FUNCTION = 1.33302684465979"
    ## [1] "BEST method = 'both' PATH MEMBER = c( 77, 98 )"
    ## [1] "BEST both OBJECTIVE FUNCTION = 1.33302684465979"

![](man/figures/README-nns_forecasting-1.png)<!-- -->

    ## Package: healthyR.ai
    ## [1] "CURRNET METHOD: lin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 21 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 1.45391057657896"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 21, 77 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 1.28359881460917"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 21, 77, 63 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 1.26131079494075"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 21, 77, 63, 98 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 1.20696537754348"
    ## [1] "BEST method = 'lin', seasonal.factor = c( 21, 77, 63, 98 )"
    ## [1] "BEST lin OBJECTIVE FUNCTION = 1.20696537754348"
    ## [1] "CURRNET METHOD: nonlin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'nonlin' , seasonal.factor =  c( 21, 77, 63, 98 ) ...)"
    ## [1] "CURRENT nonlin OBJECTIVE FUNCTION = 1.46091750833423"
    ## [1] "BEST method = 'nonlin' PATH MEMBER = c( 21, 77, 63, 98 )"
    ## [1] "BEST nonlin OBJECTIVE FUNCTION = 1.46091750833423"
    ## [1] "CURRNET METHOD: both"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'both' , seasonal.factor =  c( 21, 77, 63, 98 ) ...)"
    ## [1] "CURRENT both OBJECTIVE FUNCTION = 1.27131810581523"
    ## [1] "BEST method = 'both' PATH MEMBER = c( 21, 77, 63, 98 )"
    ## [1] "BEST both OBJECTIVE FUNCTION = 1.27131810581523"

![](man/figures/README-nns_forecasting-2.png)<!-- -->

    ## Package: healthyR.data
    ## [1] "CURRNET METHOD: lin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 77 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 2.03885291396855"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 77, 49 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 1.97035984784715"
    ## [1] "BEST method = 'lin', seasonal.factor = c( 77, 49 )"
    ## [1] "BEST lin OBJECTIVE FUNCTION = 1.97035984784715"
    ## [1] "CURRNET METHOD: nonlin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'nonlin' , seasonal.factor =  c( 77, 49 ) ...)"
    ## [1] "CURRENT nonlin OBJECTIVE FUNCTION = 3.92957040660902"
    ## [1] "BEST method = 'nonlin' PATH MEMBER = c( 77, 49 )"
    ## [1] "BEST nonlin OBJECTIVE FUNCTION = 3.92957040660902"
    ## [1] "CURRNET METHOD: both"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'both' , seasonal.factor =  c( 77, 49 ) ...)"
    ## [1] "CURRENT both OBJECTIVE FUNCTION = 2.78289385009029"
    ## [1] "BEST method = 'both' PATH MEMBER = c( 77, 49 )"
    ## [1] "BEST both OBJECTIVE FUNCTION = 2.78289385009029"

![](man/figures/README-nns_forecasting-3.png)<!-- -->

    ## Package: healthyR.ts
    ## [1] "CURRNET METHOD: lin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 14 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 1.58658459833942"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 14, 98 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 1.2771056600188"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 14, 98, 63 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 1.20142161738437"
    ## [1] "BEST method = 'lin', seasonal.factor = c( 14, 98, 63 )"
    ## [1] "BEST lin OBJECTIVE FUNCTION = 1.20142161738437"
    ## [1] "CURRNET METHOD: nonlin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'nonlin' , seasonal.factor =  c( 14, 98, 63 ) ...)"
    ## [1] "CURRENT nonlin OBJECTIVE FUNCTION = 1.56393267638589"
    ## [1] "BEST method = 'nonlin' PATH MEMBER = c( 14, 98, 63 )"
    ## [1] "BEST nonlin OBJECTIVE FUNCTION = 1.56393267638589"
    ## [1] "CURRNET METHOD: both"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'both' , seasonal.factor =  c( 14, 98, 63 ) ...)"
    ## [1] "CURRENT both OBJECTIVE FUNCTION = 1.31100305214451"
    ## [1] "BEST method = 'both' PATH MEMBER = c( 14, 98, 63 )"
    ## [1] "BEST both OBJECTIVE FUNCTION = 1.31100305214451"

![](man/figures/README-nns_forecasting-4.png)<!-- -->

    ## Package: healthyverse
    ## [1] "CURRNET METHOD: lin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 49 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 1.58342298146643"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 49, 77 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 1.39238395855057"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 49, 77, 70 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 1.36668046555046"
    ## [1] "BEST method = 'lin', seasonal.factor = c( 49, 77, 70 )"
    ## [1] "BEST lin OBJECTIVE FUNCTION = 1.36668046555046"
    ## [1] "CURRNET METHOD: nonlin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'nonlin' , seasonal.factor =  c( 49, 77, 70 ) ...)"
    ## [1] "CURRENT nonlin OBJECTIVE FUNCTION = 2.58668262066261"
    ## [1] "BEST method = 'nonlin' PATH MEMBER = c( 49, 77, 70 )"
    ## [1] "BEST nonlin OBJECTIVE FUNCTION = 2.58668262066261"
    ## [1] "CURRNET METHOD: both"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'both' , seasonal.factor =  c( 49, 77, 70 ) ...)"
    ## [1] "CURRENT both OBJECTIVE FUNCTION = 1.8190478361766"
    ## [1] "BEST method = 'both' PATH MEMBER = c( 49, 77, 70 )"
    ## [1] "BEST both OBJECTIVE FUNCTION = 1.8190478361766"

![](man/figures/README-nns_forecasting-5.png)<!-- -->

    ## Package: RandomWalker
    ## [1] "CURRNET METHOD: lin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 35 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 3.0482888777022"
    ## [1] "BEST method = 'lin', seasonal.factor = c( 35 )"
    ## [1] "BEST lin OBJECTIVE FUNCTION = 3.0482888777022"
    ## [1] "CURRNET METHOD: nonlin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'nonlin' , seasonal.factor =  c( 35 ) ...)"
    ## [1] "CURRENT nonlin OBJECTIVE FUNCTION = 4.64496830167847"
    ## [1] "BEST method = 'nonlin' PATH MEMBER = c( 35 )"
    ## [1] "BEST nonlin OBJECTIVE FUNCTION = 4.64496830167847"
    ## [1] "CURRNET METHOD: both"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'both' , seasonal.factor =  c( 35 ) ...)"
    ## [1] "CURRENT both OBJECTIVE FUNCTION = 3.80113458024296"
    ## [1] "BEST method = 'both' PATH MEMBER = c( 35 )"
    ## [1] "BEST both OBJECTIVE FUNCTION = 3.80113458024296"

![](man/figures/README-nns_forecasting-6.png)<!-- -->

    ## Package: tidyAML
    ## [1] "CURRNET METHOD: lin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 56 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 6.4824980148247"
    ## [1] "BEST method = 'lin', seasonal.factor = c( 56 )"
    ## [1] "BEST lin OBJECTIVE FUNCTION = 6.4824980148247"
    ## [1] "CURRNET METHOD: nonlin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'nonlin' , seasonal.factor =  c( 56 ) ...)"
    ## [1] "CURRENT nonlin OBJECTIVE FUNCTION = 6.20077506982306"
    ## [1] "BEST method = 'nonlin' PATH MEMBER = c( 56 )"
    ## [1] "BEST nonlin OBJECTIVE FUNCTION = 6.20077506982306"
    ## [1] "CURRNET METHOD: both"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'both' , seasonal.factor =  c( 56 ) ...)"
    ## [1] "CURRENT both OBJECTIVE FUNCTION = 6.03958613176473"
    ## [1] "BEST method = 'both' PATH MEMBER = c( 56 )"
    ## [1] "BEST both OBJECTIVE FUNCTION = 6.03958613176473"

![](man/figures/README-nns_forecasting-7.png)<!-- -->

    ## Package: TidyDensity
    ## [1] "CURRNET METHOD: lin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 77 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 1.55494400909737"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 77, 63 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 1.23127193810417"
    ## [1] "BEST method = 'lin', seasonal.factor = c( 77, 63 )"
    ## [1] "BEST lin OBJECTIVE FUNCTION = 1.23127193810417"
    ## [1] "CURRNET METHOD: nonlin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'nonlin' , seasonal.factor =  c( 77, 63 ) ...)"
    ## [1] "CURRENT nonlin OBJECTIVE FUNCTION = 1.94055299472383"
    ## [1] "BEST method = 'nonlin' PATH MEMBER = c( 77, 63 )"
    ## [1] "BEST nonlin OBJECTIVE FUNCTION = 1.94055299472383"
    ## [1] "CURRNET METHOD: both"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'both' , seasonal.factor =  c( 77, 63 ) ...)"
    ## [1] "CURRENT both OBJECTIVE FUNCTION = 1.46503974882815"
    ## [1] "BEST method = 'both' PATH MEMBER = c( 77, 63 )"
    ## [1] "BEST both OBJECTIVE FUNCTION = 1.46503974882815"

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
    ## 1 healthyR.data <tibble [1,568 × 2]> <tibble [28 × 2]> <split [1540|28]>
    ## 2 healthyR      <tibble [1,561 × 2]> <tibble [28 × 2]> <split [1533|28]>
    ## 3 healthyR.ts   <tibble [1,505 × 2]> <tibble [28 × 2]> <split [1477|28]>
    ## 4 healthyverse  <tibble [1,476 × 2]> <tibble [28 × 2]> <split [1448|28]>
    ## 5 healthyR.ai   <tibble [1,300 × 2]> <tibble [28 × 2]> <split [1272|28]>
    ## 6 TidyDensity   <tibble [1,151 × 2]> <tibble [28 × 2]> <split [1123|28]>
    ## 7 tidyAML       <tibble [759 × 2]>   <tibble [28 × 2]> <split [731|28]> 
    ## 8 RandomWalker  <tibble [181 × 2]>   <tibble [28 × 2]> <split [153|28]>

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
| healthyR.data | 1 | ARIMA | Test | 0.7793869 | 117.28048 | 0.6529519 | 169.98162 | 0.9396146 | 0.2890112 |
| healthyR.data | 2 | LM | Test | 0.8606640 | 198.36970 | 0.7210440 | 150.31963 | 0.9774412 | 0.0827685 |
| healthyR.data | 3 | EARTH | Test | 0.9417567 | 186.80149 | 0.7889815 | 129.66786 | 1.2130635 | 0.0827685 |
| healthyR.data | 4 | NNAR | Test | 0.8211281 | 94.99377 | 0.6879217 | 165.20162 | 1.0245625 | 0.0340712 |
| healthyR | 1 | ARIMA | Test | 0.6880176 | 125.26028 | 0.6919156 | 153.10729 | 0.8439759 | 0.0292489 |
| healthyR | 2 | LM | Test | 0.7145371 | 106.23361 | 0.7185854 | 183.79690 | 0.8761579 | 0.0394467 |
| healthyR | 3 | EARTH | Test | 0.7189228 | 113.35836 | 0.7229958 | 170.45277 | 0.8839992 | 0.0394467 |
| healthyR | 4 | NNAR | Test | 0.7181873 | 121.27841 | 0.7222562 | 154.81596 | 0.8957548 | 0.0029853 |
| healthyR.ts | 1 | ARIMA | Test | 1.0342193 | 120.58563 | 0.6636729 | 131.16952 | 1.2208423 | 0.0952632 |
| healthyR.ts | 2 | LM | Test | 1.0347932 | 120.45558 | 0.6640412 | 131.46346 | 1.2210952 | 0.0952632 |
| healthyR.ts | 3 | EARTH | Test | 1.0332865 | 123.40827 | 0.6630743 | 128.23463 | 1.2238108 | 0.0952632 |
| healthyR.ts | 4 | NNAR | Test | 0.9678050 | 97.64026 | 0.6210539 | 167.36402 | 1.1418004 | 0.1550056 |
| healthyverse | 1 | ARIMA | Test | 0.6491115 | 117.23270 | 0.8791740 | 119.55094 | 0.8018293 | 0.0638860 |
| healthyverse | 2 | LM | Test | 0.6862354 | 175.34891 | 0.9294556 | 100.95627 | 0.8248753 | 0.0900586 |
| healthyverse | 3 | EARTH | Test | 0.6539270 | 115.26235 | 0.8856962 | 110.33396 | 0.8207354 | 0.0900586 |
| healthyverse | 4 | NNAR | Test | 0.6553605 | 107.88048 | 0.8876378 | 110.47186 | 0.8451655 | 0.0009811 |
| healthyR.ai | 1 | ARIMA | Test | 0.7743391 | 99.95256 | 0.7445680 | 176.68874 | 0.8839752 | 0.2193387 |
| healthyR.ai | 2 | LM | Test | 0.8026549 | 93.35530 | 0.7717951 | 149.31426 | 0.9594987 | 0.0685038 |
| healthyR.ai | 3 | EARTH | Test | 0.8028401 | 92.92541 | 0.7719731 | 156.49627 | 0.9499534 | 0.0685038 |
| healthyR.ai | 4 | NNAR | Test | 0.7250900 | 85.53046 | 0.6972123 | 145.53242 | 0.8560886 | 0.2634570 |
| TidyDensity | 1 | ARIMA | Test | 0.7008403 | 159.84736 | 0.6909101 | 116.82918 | 0.8451414 | 0.0092040 |
| TidyDensity | 2 | LM | Test | 0.7299166 | 201.61299 | 0.7195744 | 111.81216 | 0.8603495 | 0.0948960 |
| TidyDensity | 3 | EARTH | Test | 0.7057754 | 150.57904 | 0.6957753 | 119.62439 | 0.8519202 | 0.0948960 |
| TidyDensity | 4 | NNAR | Test | 0.7020299 | 114.57964 | 0.6920828 | 135.71227 | 0.8887691 | 0.0219808 |
| tidyAML | 1 | ARIMA | Test | 0.6787437 | 250.94120 | 0.6866249 | 100.76948 | 0.7884027 | 0.0152853 |
| tidyAML | 2 | LM | Test | 0.6734447 | 248.27681 | 0.6812644 | 96.88852 | 0.7896160 | 0.0162590 |
| tidyAML | 3 | EARTH | Test | 0.8098624 | 110.02034 | 0.8192660 | 151.65952 | 1.0298192 | 0.0162590 |
| tidyAML | 4 | NNAR | Test | 0.6604508 | 222.69466 | 0.6681196 | 99.73123 | 0.7760251 | 0.0602313 |
| RandomWalker | 1 | ARIMA | Test | 0.9172283 | 73.19819 | 0.4567974 | 86.08889 | 1.1575497 | 0.3750873 |
| RandomWalker | 2 | LM | Test | 1.3963263 | 107.30077 | 0.6953973 | 194.11820 | 1.4853700 | 0.0496308 |
| RandomWalker | 3 | EARTH | Test | 1.3052495 | 93.59462 | 0.6500394 | 172.43843 | 1.4449914 | NA |
| RandomWalker | 4 | NNAR | Test | 1.3189833 | 115.25914 | 0.6568791 | 147.72654 | 1.4978657 | 0.0356474 |

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
    ## 1 healthyR.da…         1 ARIMA       Test  0.779 117.  0.653 170.  0.940 0.289  
    ## 2 healthyR             1 ARIMA       Test  0.688 125.  0.692 153.  0.844 0.0292 
    ## 3 healthyR.ts          4 NNAR        Test  0.968  97.6 0.621 167.  1.14  0.155  
    ## 4 healthyverse         1 ARIMA       Test  0.649 117.  0.879 120.  0.802 0.0639 
    ## 5 healthyR.ai          4 NNAR        Test  0.725  85.5 0.697 146.  0.856 0.263  
    ## 6 TidyDensity          1 ARIMA       Test  0.701 160.  0.691 117.  0.845 0.00920
    ## 7 tidyAML              4 NNAR        Test  0.660 223.  0.668  99.7 0.776 0.0602 
    ## 8 RandomWalker         1 ARIMA       Test  0.917  73.2 0.457  86.1 1.16  0.375

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
    ## 1 healthyR.data <tibble>     <tibble>     <split [1540|28]> <mdl_tm_t [1 × 5]>
    ## 2 healthyR      <tibble>     <tibble>     <split [1533|28]> <mdl_tm_t [1 × 5]>
    ## 3 healthyR.ts   <tibble>     <tibble>     <split [1477|28]> <mdl_tm_t [1 × 5]>
    ## 4 healthyverse  <tibble>     <tibble>     <split [1448|28]> <mdl_tm_t [1 × 5]>
    ## 5 healthyR.ai   <tibble>     <tibble>     <split [1272|28]> <mdl_tm_t [1 × 5]>
    ## 6 TidyDensity   <tibble>     <tibble>     <split [1123|28]> <mdl_tm_t [1 × 5]>
    ## 7 tidyAML       <tibble>     <tibble>     <split [731|28]>  <mdl_tm_t [1 × 5]>
    ## 8 RandomWalker  <tibble>     <tibble>     <split [153|28]>  <mdl_tm_t [1 × 5]>

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
