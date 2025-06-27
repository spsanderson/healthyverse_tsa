Time Series Analysis and Nested Modeling of the Healthyverse Packages
================
Steven P. Sanderson II, MPH - Date:
27 June, 2025

This analysis follows a *Nested Modeltime Workflow*.

## Get Data

``` r
glimpse(downloads_tbl)
```

    ## Rows: 143,816
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

The last day in the data set is 2025-06-25 22:57:22, the file was
birthed on: 2022-07-02 23:58:17.511888, and at report knit time is
-2.613098^{4} hours old. Happy analyzing!

Now that we have our data lets take a look at it using the `skimr`
package.

``` r
skim(downloads_tbl)
```

|                                                  |               |
|:-------------------------------------------------|:--------------|
| Name                                             | downloads_tbl |
| Number of rows                                   | 143816        |
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
| r_version     |    103910 |          0.28 |   5 |   5 |     0 |       48 |          0 |
| r_arch        |    103910 |          0.28 |   3 |   7 |     0 |        5 |          0 |
| r_os          |    103910 |          0.28 |   7 |  15 |     0 |       22 |          0 |
| package       |         0 |          1.00 |   7 |  13 |     0 |        8 |          0 |
| version       |         0 |          1.00 |   5 |  17 |     0 |       60 |          0 |
| country       |     12145 |          0.92 |   2 |   2 |     0 |      163 |          0 |

**Variable type: Date**

| skim_variable | n_missing | complete_rate | min | max | median | n_unique |
|:---|---:|---:|:---|:---|:---|---:|
| date | 0 | 1 | 2020-11-23 | 2025-06-25 | 2023-07-12 | 1676 |

**Variable type: numeric**

| skim_variable | n_missing | complete_rate | mean | sd | p0 | p25 | p50 | p75 | p100 | hist |
|:---|---:|---:|---:|---:|---:|---:|---:|---:|---:|:---|
| size | 0 | 1 | 1132507.48 | 1515034.73 | 355 | 14701 | 289912 | 2367693 | 5677952 | ▇▁▂▁▁ |
| ip_id | 0 | 1 | 10449.78 | 18571.16 | 1 | 292 | 3053 | 11830 | 209747 | ▇▁▁▁▁ |

**Variable type: POSIXct**

| skim_variable | n_missing | complete_rate | min | max | median | n_unique |
|:---|---:|---:|:---|:---|:---|---:|
| date_time | 0 | 1 | 2020-11-23 09:00:41 | 2025-06-25 22:57:22 | 2023-07-12 13:47:57 | 88210 |

**Variable type: Timespan**

| skim_variable | n_missing | complete_rate | min | max | median | n_unique |
|:--------------|----------:|--------------:|----:|----:|-------:|---------:|
| time          |         0 |             1 |   0 |  59 |     25 |       60 |

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
    ## -148.25  -35.92  -11.39   26.72  816.05 
    ## 
    ## Coefficients:
    ##                                                      Estimate Std. Error
    ## (Intercept)                                        -1.754e+02  6.689e+01
    ## date                                                1.080e-02  3.542e-03
    ## lag(value, 1)                                       1.050e-01  2.421e-02
    ## lag(value, 7)                                       9.423e-02  2.505e-02
    ## lag(value, 14)                                      8.627e-02  2.517e-02
    ## lag(value, 21)                                      6.672e-02  2.524e-02
    ## lag(value, 28)                                      7.183e-02  2.515e-02
    ## lag(value, 35)                                      6.769e-02  2.521e-02
    ## lag(value, 42)                                      5.430e-02  2.530e-02
    ## lag(value, 49)                                      6.310e-02  2.518e-02
    ## month(date, label = TRUE).L                        -9.713e+00  5.122e+00
    ## month(date, label = TRUE).Q                         3.241e+00  5.093e+00
    ## month(date, label = TRUE).C                        -1.325e+01  5.140e+00
    ## month(date, label = TRUE)^4                        -6.641e+00  5.148e+00
    ## month(date, label = TRUE)^5                        -1.133e+01  5.124e+00
    ## month(date, label = TRUE)^6                        -4.101e+00  5.193e+00
    ## month(date, label = TRUE)^7                        -7.105e+00  5.090e+00
    ## month(date, label = TRUE)^8                        -2.849e+00  5.084e+00
    ## month(date, label = TRUE)^9                         5.210e+00  5.082e+00
    ## month(date, label = TRUE)^10                        2.515e+00  5.075e+00
    ## month(date, label = TRUE)^11                       -3.577e+00  5.105e+00
    ## fourier_vec(date, type = "sin", K = 1, period = 7) -1.189e+01  2.320e+00
    ## fourier_vec(date, type = "cos", K = 1, period = 7)  8.301e+00  2.444e+00
    ##                                                    t value Pr(>|t|)    
    ## (Intercept)                                         -2.623 0.008808 ** 
    ## date                                                 3.049 0.002336 ** 
    ## lag(value, 1)                                        4.338 1.53e-05 ***
    ## lag(value, 7)                                        3.762 0.000175 ***
    ## lag(value, 14)                                       3.427 0.000625 ***
    ## lag(value, 21)                                       2.644 0.008274 ** 
    ## lag(value, 28)                                       2.856 0.004340 ** 
    ## lag(value, 35)                                       2.685 0.007331 ** 
    ## lag(value, 42)                                       2.146 0.032031 *  
    ## lag(value, 49)                                       2.506 0.012315 *  
    ## month(date, label = TRUE).L                         -1.897 0.058073 .  
    ## month(date, label = TRUE).Q                          0.636 0.524627    
    ## month(date, label = TRUE).C                         -2.577 0.010050 *  
    ## month(date, label = TRUE)^4                         -1.290 0.197212    
    ## month(date, label = TRUE)^5                         -2.211 0.027185 *  
    ## month(date, label = TRUE)^6                         -0.790 0.429844    
    ## month(date, label = TRUE)^7                         -1.396 0.162968    
    ## month(date, label = TRUE)^8                         -0.561 0.575199    
    ## month(date, label = TRUE)^9                          1.025 0.305434    
    ## month(date, label = TRUE)^10                         0.495 0.620319    
    ## month(date, label = TRUE)^11                        -0.701 0.483588    
    ## fourier_vec(date, type = "sin", K = 1, period = 7)  -5.123 3.37e-07 ***
    ## fourier_vec(date, type = "cos", K = 1, period = 7)   3.397 0.000698 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 58.87 on 1604 degrees of freedom
    ##   (49 observations deleted due to missingness)
    ## Multiple R-squared:  0.2382, Adjusted R-squared:  0.2277 
    ## F-statistic: 22.79 on 22 and 1604 DF,  p-value: < 2.2e-16

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
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 63 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 2.76065649741152"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 63, 21 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 2.41950857982988"
    ## [1] "BEST method = 'lin', seasonal.factor = c( 63, 21 )"
    ## [1] "BEST lin OBJECTIVE FUNCTION = 2.41950857982988"
    ## [1] "CURRNET METHOD: nonlin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'nonlin' , seasonal.factor =  c( 63, 21 ) ...)"
    ## [1] "CURRENT nonlin OBJECTIVE FUNCTION = 7.4809931379618"
    ## [1] "BEST method = 'nonlin' PATH MEMBER = c( 63, 21 )"
    ## [1] "BEST nonlin OBJECTIVE FUNCTION = 7.4809931379618"
    ## [1] "CURRNET METHOD: both"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'both' , seasonal.factor =  c( 63, 21 ) ...)"
    ## [1] "CURRENT both OBJECTIVE FUNCTION = 4.1289231345817"
    ## [1] "BEST method = 'both' PATH MEMBER = c( 63, 21 )"
    ## [1] "BEST both OBJECTIVE FUNCTION = 4.1289231345817"

![](man/figures/README-nns_forecasting-1.png)<!-- -->

    ## Package: healthyR.ai
    ## [1] "CURRNET METHOD: lin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 63 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 1.66200480826731"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 63, 35 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 1.62890516061295"
    ## [1] "BEST method = 'lin', seasonal.factor = c( 63, 35 )"
    ## [1] "BEST lin OBJECTIVE FUNCTION = 1.62890516061295"
    ## [1] "CURRNET METHOD: nonlin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'nonlin' , seasonal.factor =  c( 63, 35 ) ...)"
    ## [1] "CURRENT nonlin OBJECTIVE FUNCTION = 13.1193083880865"
    ## [1] "BEST method = 'nonlin' PATH MEMBER = c( 63, 35 )"
    ## [1] "BEST nonlin OBJECTIVE FUNCTION = 13.1193083880865"
    ## [1] "CURRNET METHOD: both"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'both' , seasonal.factor =  c( 63, 35 ) ...)"
    ## [1] "CURRENT both OBJECTIVE FUNCTION = 4.31099610756145"
    ## [1] "BEST method = 'both' PATH MEMBER = c( 63, 35 )"
    ## [1] "BEST both OBJECTIVE FUNCTION = 4.31099610756145"

![](man/figures/README-nns_forecasting-2.png)<!-- -->

    ## Package: healthyR.data
    ## [1] "CURRNET METHOD: lin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 21 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 3.66114646711529"
    ## [1] "BEST method = 'lin', seasonal.factor = c( 21 )"
    ## [1] "BEST lin OBJECTIVE FUNCTION = 3.66114646711529"
    ## [1] "CURRNET METHOD: nonlin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'nonlin' , seasonal.factor =  c( 21 ) ...)"
    ## [1] "CURRENT nonlin OBJECTIVE FUNCTION = 3.80138978534194"
    ## [1] "BEST method = 'nonlin' PATH MEMBER = c( 21 )"
    ## [1] "BEST nonlin OBJECTIVE FUNCTION = 3.80138978534194"
    ## [1] "CURRNET METHOD: both"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'both' , seasonal.factor =  c( 21 ) ...)"
    ## [1] "CURRENT both OBJECTIVE FUNCTION = 3.31828174133394"
    ## [1] "BEST method = 'both' PATH MEMBER = c( 21 )"
    ## [1] "BEST both OBJECTIVE FUNCTION = 3.31828174133394"

![](man/figures/README-nns_forecasting-3.png)<!-- -->

    ## Package: healthyR.ts
    ## [1] "CURRNET METHOD: lin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 63 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 1.57779491082719"
    ## [1] "BEST method = 'lin', seasonal.factor = c( 63 )"
    ## [1] "BEST lin OBJECTIVE FUNCTION = 1.57779491082719"
    ## [1] "CURRNET METHOD: nonlin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'nonlin' , seasonal.factor =  c( 63 ) ...)"
    ## [1] "CURRENT nonlin OBJECTIVE FUNCTION = 8.90668516742694"
    ## [1] "BEST method = 'nonlin' PATH MEMBER = c( 63 )"
    ## [1] "BEST nonlin OBJECTIVE FUNCTION = 8.90668516742694"
    ## [1] "CURRNET METHOD: both"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'both' , seasonal.factor =  c( 63 ) ...)"
    ## [1] "CURRENT both OBJECTIVE FUNCTION = 3.79018096729757"
    ## [1] "BEST method = 'both' PATH MEMBER = c( 63 )"
    ## [1] "BEST both OBJECTIVE FUNCTION = 3.79018096729757"

![](man/figures/README-nns_forecasting-4.png)<!-- -->

    ## Package: healthyverse
    ## [1] "CURRNET METHOD: lin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 63 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 5.07407100054038"
    ## [1] "BEST method = 'lin', seasonal.factor = c( 63 )"
    ## [1] "BEST lin OBJECTIVE FUNCTION = 5.07407100054038"
    ## [1] "CURRNET METHOD: nonlin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'nonlin' , seasonal.factor =  c( 63 ) ...)"
    ## [1] "CURRENT nonlin OBJECTIVE FUNCTION = 5.49957972910155"
    ## [1] "BEST method = 'nonlin' PATH MEMBER = c( 63 )"
    ## [1] "BEST nonlin OBJECTIVE FUNCTION = 5.49957972910155"
    ## [1] "CURRNET METHOD: both"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'both' , seasonal.factor =  c( 63 ) ...)"
    ## [1] "CURRENT both OBJECTIVE FUNCTION = 4.89395074559162"
    ## [1] "BEST method = 'both' PATH MEMBER = c( 63 )"
    ## [1] "BEST both OBJECTIVE FUNCTION = 4.89395074559162"

![](man/figures/README-nns_forecasting-5.png)<!-- -->

    ## Package: RandomWalker
    ## [1] "CURRNET METHOD: lin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 70 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 2.87107923634735"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 70, 49 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 2.84691667886216"
    ## [1] "BEST method = 'lin', seasonal.factor = c( 70, 49 )"
    ## [1] "BEST lin OBJECTIVE FUNCTION = 2.84691667886216"
    ## [1] "CURRNET METHOD: nonlin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'nonlin' , seasonal.factor =  c( 70, 49 ) ...)"
    ## [1] "CURRENT nonlin OBJECTIVE FUNCTION = 3.10045275324349"
    ## [1] "BEST method = 'nonlin' PATH MEMBER = c( 70, 49 )"
    ## [1] "BEST nonlin OBJECTIVE FUNCTION = 3.10045275324349"
    ## [1] "CURRNET METHOD: both"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'both' , seasonal.factor =  c( 70, 49 ) ...)"
    ## [1] "CURRENT both OBJECTIVE FUNCTION = 2.84083704610936"
    ## [1] "BEST method = 'both' PATH MEMBER = c( 70, 49 )"
    ## [1] "BEST both OBJECTIVE FUNCTION = 2.84083704610936"

![](man/figures/README-nns_forecasting-6.png)<!-- -->

    ## Package: tidyAML
    ## [1] "CURRNET METHOD: lin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 84 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 4.92254628251211"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 84, 91 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 4.73963861928437"
    ## [1] "BEST method = 'lin', seasonal.factor = c( 84, 91 )"
    ## [1] "BEST lin OBJECTIVE FUNCTION = 4.73963861928437"
    ## [1] "CURRNET METHOD: nonlin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'nonlin' , seasonal.factor =  c( 84, 91 ) ...)"
    ## [1] "CURRENT nonlin OBJECTIVE FUNCTION = 3.10342036705297"
    ## [1] "BEST method = 'nonlin' PATH MEMBER = c( 84, 91 )"
    ## [1] "BEST nonlin OBJECTIVE FUNCTION = 3.10342036705297"
    ## [1] "CURRNET METHOD: both"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'both' , seasonal.factor =  c( 84, 91 ) ...)"
    ## [1] "CURRENT both OBJECTIVE FUNCTION = 3.65353602152908"
    ## [1] "BEST method = 'both' PATH MEMBER = c( 84, 91 )"
    ## [1] "BEST both OBJECTIVE FUNCTION = 3.65353602152908"

![](man/figures/README-nns_forecasting-7.png)<!-- -->

    ## Package: TidyDensity
    ## [1] "CURRNET METHOD: lin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 63 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 2.75737591646039"
    ## [1] "BEST method = 'lin', seasonal.factor = c( 63 )"
    ## [1] "BEST lin OBJECTIVE FUNCTION = 2.75737591646039"
    ## [1] "CURRNET METHOD: nonlin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'nonlin' , seasonal.factor =  c( 63 ) ...)"
    ## [1] "CURRENT nonlin OBJECTIVE FUNCTION = 9.86526045284667"
    ## [1] "BEST method = 'nonlin' PATH MEMBER = c( 63 )"
    ## [1] "BEST nonlin OBJECTIVE FUNCTION = 9.86526045284667"
    ## [1] "CURRNET METHOD: both"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'both' , seasonal.factor =  c( 63 ) ...)"
    ## [1] "CURRENT both OBJECTIVE FUNCTION = 5.65515049019352"
    ## [1] "BEST method = 'both' PATH MEMBER = c( 63 )"
    ## [1] "BEST both OBJECTIVE FUNCTION = 5.65515049019352"

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
    ## 1 healthyR.data <tibble [1,668 × 2]> <tibble [28 × 2]> <split [1640|28]>
    ## 2 healthyR      <tibble [1,662 × 2]> <tibble [28 × 2]> <split [1634|28]>
    ## 3 healthyR.ts   <tibble [1,606 × 2]> <tibble [28 × 2]> <split [1578|28]>
    ## 4 healthyverse  <tibble [1,576 × 2]> <tibble [28 × 2]> <split [1548|28]>
    ## 5 healthyR.ai   <tibble [1,401 × 2]> <tibble [28 × 2]> <split [1373|28]>
    ## 6 TidyDensity   <tibble [1,252 × 2]> <tibble [28 × 2]> <split [1224|28]>
    ## 7 tidyAML       <tibble [860 × 2]>   <tibble [28 × 2]> <split [832|28]> 
    ## 8 RandomWalker  <tibble [282 × 2]>   <tibble [28 × 2]> <split [254|28]>

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
| healthyR.data | 1 | ARIMA | Test | 0.7084752 | 180.04636 | 0.8028814 | 131.92661 | 0.8917613 | 0.0017965 |
| healthyR.data | 2 | LM | Test | 0.7117286 | 188.25784 | 0.8065684 | 130.53746 | 0.8914170 | 0.0767624 |
| healthyR.data | 3 | NULL | NA | NA | NA | NA | NA | NA | NA |
| healthyR.data | 4 | NNAR | Test | 0.6849291 | 92.85736 | 0.7761977 | 169.97547 | 0.9130926 | 0.0878255 |
| healthyR | 1 | ARIMA | Test | 0.8950958 | 118.16423 | 0.8637916 | 164.04458 | 1.0536521 | 0.0007766 |
| healthyR | 2 | LM | Test | 0.8928538 | 101.29591 | 0.8616279 | 181.82240 | 1.0593622 | 0.1276763 |
| healthyR | 3 | NULL | NA | NA | NA | NA | NA | NA | NA |
| healthyR | 4 | NNAR | Test | 0.8823510 | 103.79713 | 0.8514925 | 168.80321 | 1.0386937 | 0.0164452 |
| healthyR.ts | 1 | ARIMA | Test | 0.9153494 | 112.67787 | 0.6723921 | 158.73166 | 1.1851881 | 0.0359622 |
| healthyR.ts | 2 | LM | Test | 1.0256400 | 180.25187 | 0.7534087 | 157.10469 | 1.2555302 | 0.1432948 |
| healthyR.ts | 3 | NULL | NA | NA | NA | NA | NA | NA | NA |
| healthyR.ts | 4 | NNAR | Test | 0.9293695 | 101.23560 | 0.6826909 | 180.22600 | 1.2051912 | 0.0069497 |
| healthyverse | 1 | ARIMA | Test | 0.8004857 | 154.64057 | 0.9042838 | 94.83049 | 0.9724821 | 0.0000016 |
| healthyverse | 2 | LM | Test | 0.7992049 | 166.72187 | 0.9028370 | 93.24675 | 0.9624445 | 0.0808650 |
| healthyverse | 3 | NULL | NA | NA | NA | NA | NA | NA | NA |
| healthyverse | 4 | NNAR | Test | 0.8590775 | 114.78977 | 0.9704732 | 111.11197 | 1.0606361 | 0.0136796 |
| healthyR.ai | 1 | ARIMA | Test | 0.7126113 | 115.91149 | 0.7533665 | 160.33871 | 0.8311675 | 0.0765775 |
| healthyR.ai | 2 | LM | Test | 0.7171723 | 111.56118 | 0.7581883 | 154.64448 | 0.8510237 | 0.2174532 |
| healthyR.ai | 3 | NULL | NA | NA | NA | NA | NA | NA | NA |
| healthyR.ai | 4 | NNAR | Test | 0.7068912 | 108.44722 | 0.7473193 | 154.13069 | 0.8344099 | 0.0960652 |
| TidyDensity | 1 | ARIMA | Test | 0.5945073 | 91.58512 | 0.8416399 | 109.36258 | 0.8074633 | 0.0865454 |
| TidyDensity | 2 | LM | Test | 0.6519147 | 141.09296 | 0.9229111 | 100.66677 | 0.8774827 | 0.1850670 |
| TidyDensity | 3 | NULL | NA | NA | NA | NA | NA | NA | NA |
| TidyDensity | 4 | NNAR | Test | 0.6134086 | 79.07047 | 0.8683983 | 123.07788 | 0.8328107 | 0.0000062 |
| tidyAML | 1 | ARIMA | Test | 0.7620999 | 106.38254 | 0.7811441 | 110.11480 | 0.9588750 | 0.0296818 |
| tidyAML | 2 | LM | Test | 0.6549575 | 114.37193 | 0.6713243 | 84.19211 | 0.8311724 | 0.0112348 |
| tidyAML | 3 | NULL | NA | NA | NA | NA | NA | NA | NA |
| tidyAML | 4 | NNAR | Test | 0.6963731 | 101.51077 | 0.7137748 | 92.63265 | 0.9041633 | 0.0342396 |
| RandomWalker | 1 | ARIMA | Test | 1.2101774 | 103.43386 | 0.7018038 | 160.67450 | 1.3772782 | 0.0033043 |
| RandomWalker | 2 | LM | Test | 1.2384562 | 109.23434 | 0.7182032 | 184.01031 | 1.3720926 | 0.0487768 |
| RandomWalker | 3 | NULL | NA | NA | NA | NA | NA | NA | NA |
| RandomWalker | 4 | NNAR | Test | 1.0082483 | 149.57343 | 0.5847015 | 121.92270 | 1.1810228 | 0.2830081 |

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
    ## 1 healthyR.data         2 LM          Test  0.712 188.  0.807 131.  0.891 0.0768
    ## 2 healthyR              4 NNAR        Test  0.882 104.  0.851 169.  1.04  0.0164
    ## 3 healthyR.ts           1 ARIMA       Test  0.915 113.  0.672 159.  1.19  0.0360
    ## 4 healthyverse          2 LM          Test  0.799 167.  0.903  93.2 0.962 0.0809
    ## 5 healthyR.ai           1 ARIMA       Test  0.713 116.  0.753 160.  0.831 0.0766
    ## 6 TidyDensity           1 ARIMA       Test  0.595  91.6 0.842 109.  0.807 0.0865
    ## 7 tidyAML               2 LM          Test  0.655 114.  0.671  84.2 0.831 0.0112
    ## 8 RandomWalker          4 NNAR        Test  1.01  150.  0.585 122.  1.18  0.283

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
    ## 1 healthyR.data <tibble>     <tibble>     <split [1640|28]> <mdl_tm_t [1 × 5]>
    ## 2 healthyR      <tibble>     <tibble>     <split [1634|28]> <mdl_tm_t [1 × 5]>
    ## 3 healthyR.ts   <tibble>     <tibble>     <split [1578|28]> <mdl_tm_t [1 × 5]>
    ## 4 healthyverse  <tibble>     <tibble>     <split [1548|28]> <mdl_tm_t [1 × 5]>
    ## 5 healthyR.ai   <tibble>     <tibble>     <split [1373|28]> <mdl_tm_t [1 × 5]>
    ## 6 TidyDensity   <tibble>     <tibble>     <split [1224|28]> <mdl_tm_t [1 × 5]>
    ## 7 tidyAML       <tibble>     <tibble>     <split [832|28]>  <mdl_tm_t [1 × 5]>
    ## 8 RandomWalker  <tibble>     <tibble>     <split [254|28]>  <mdl_tm_t [1 × 5]>

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
