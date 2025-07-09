Time Series Analysis and Nested Modeling of the Healthyverse Packages
================
Steven P. Sanderson II, MPH - Date:
09 July, 2025

This analysis follows a *Nested Modeltime Workflow*.

## Get Data

``` r
glimpse(downloads_tbl)
```

    ## Rows: 144,864
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

The last day in the data set is 2025-07-07 21:30:55, the file was
birthed on: 2024-08-07 07:35:44.428716, and at report knit time is
-8025.92 hours old. Happy analyzing!

Now that we have our data lets take a look at it using the `skimr`
package.

``` r
skim(downloads_tbl)
```

|                                                  |               |
|:-------------------------------------------------|:--------------|
| Name                                             | downloads_tbl |
| Number of rows                                   | 144864        |
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
| r_version     |    104720 |          0.28 |   5 |   5 |     0 |       48 |          0 |
| r_arch        |    104720 |          0.28 |   3 |   7 |     0 |        5 |          0 |
| r_os          |    104720 |          0.28 |   7 |  15 |     0 |       23 |          0 |
| package       |         0 |          1.00 |   7 |  13 |     0 |        8 |          0 |
| version       |         0 |          1.00 |   5 |  17 |     0 |       60 |          0 |
| country       |     12235 |          0.92 |   2 |   2 |     0 |      163 |          0 |

**Variable type: Date**

| skim_variable | n_missing | complete_rate | min | max | median | n_unique |
|:---|---:|---:|:---|:---|:---|---:|
| date | 0 | 1 | 2020-11-23 | 2025-07-07 | 2023-07-19 | 1688 |

**Variable type: numeric**

| skim_variable | n_missing | complete_rate | mean | sd | p0 | p25 | p50 | p75 | p100 | hist |
|:---|---:|---:|---:|---:|---:|---:|---:|---:|---:|:---|
| size | 0 | 1 | 1131584.65 | 1513383.09 | 355 | 14701 | 293058 | 2367682 | 5677952 | ▇▁▂▁▁ |
| ip_id | 0 | 1 | 10454.41 | 18585.19 | 1 | 285 | 3037 | 11827 | 209747 | ▇▁▁▁▁ |

**Variable type: POSIXct**

| skim_variable | n_missing | complete_rate | min | max | median | n_unique |
|:---|---:|---:|:---|:---|:---|---:|
| date_time | 0 | 1 | 2020-11-23 09:00:41 | 2025-07-07 21:30:55 | 2023-07-19 18:19:49 | 88973 |

**Variable type: Timespan**

| skim_variable | n_missing | complete_rate | min | max | median | n_unique |
|:--------------|----------:|--------------:|----:|----:|-------:|---------:|
| time          |         0 |             1 |   0 |  59 |     16 |       60 |

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
    ## -148.78  -36.03  -11.32   26.71  816.07 
    ## 
    ## Coefficients:
    ##                                                      Estimate Std. Error
    ## (Intercept)                                        -1.718e+02  6.584e+01
    ## date                                                1.060e-02  3.486e-03
    ## lag(value, 1)                                       1.044e-01  2.411e-02
    ## lag(value, 7)                                       9.437e-02  2.495e-02
    ## lag(value, 14)                                      8.667e-02  2.494e-02
    ## lag(value, 21)                                      6.554e-02  2.511e-02
    ## lag(value, 28)                                      7.064e-02  2.499e-02
    ## lag(value, 35)                                      6.782e-02  2.508e-02
    ## lag(value, 42)                                      5.648e-02  2.519e-02
    ## lag(value, 49)                                      6.537e-02  2.506e-02
    ## month(date, label = TRUE).L                        -9.740e+00  5.109e+00
    ## month(date, label = TRUE).Q                         3.369e+00  5.057e+00
    ## month(date, label = TRUE).C                        -1.331e+01  5.125e+00
    ## month(date, label = TRUE)^4                        -6.816e+00  5.113e+00
    ## month(date, label = TRUE)^5                        -1.131e+01  5.105e+00
    ## month(date, label = TRUE)^6                        -4.068e+00  5.162e+00
    ## month(date, label = TRUE)^7                        -7.101e+00  5.065e+00
    ## month(date, label = TRUE)^8                        -3.011e+00  5.054e+00
    ## month(date, label = TRUE)^9                         5.271e+00  5.044e+00
    ## month(date, label = TRUE)^10                        2.629e+00  5.050e+00
    ## month(date, label = TRUE)^11                       -3.686e+00  5.023e+00
    ## fourier_vec(date, type = "sin", K = 1, period = 7) -1.180e+01  2.307e+00
    ## fourier_vec(date, type = "cos", K = 1, period = 7)  8.115e+00  2.427e+00
    ##                                                    t value Pr(>|t|)    
    ## (Intercept)                                         -2.610 0.009142 ** 
    ## date                                                 3.042 0.002389 ** 
    ## lag(value, 1)                                        4.331 1.58e-05 ***
    ## lag(value, 7)                                        3.782 0.000161 ***
    ## lag(value, 14)                                       3.475 0.000525 ***
    ## lag(value, 21)                                       2.609 0.009152 ** 
    ## lag(value, 28)                                       2.827 0.004753 ** 
    ## lag(value, 35)                                       2.704 0.006926 ** 
    ## lag(value, 42)                                       2.242 0.025092 *  
    ## lag(value, 49)                                       2.609 0.009178 ** 
    ## month(date, label = TRUE).L                         -1.907 0.056757 .  
    ## month(date, label = TRUE).Q                          0.666 0.505392    
    ## month(date, label = TRUE).C                         -2.596 0.009504 ** 
    ## month(date, label = TRUE)^4                         -1.333 0.182691    
    ## month(date, label = TRUE)^5                         -2.216 0.026844 *  
    ## month(date, label = TRUE)^6                         -0.788 0.430823    
    ## month(date, label = TRUE)^7                         -1.402 0.161088    
    ## month(date, label = TRUE)^8                         -0.596 0.551423    
    ## month(date, label = TRUE)^9                          1.045 0.296219    
    ## month(date, label = TRUE)^10                         0.521 0.602696    
    ## month(date, label = TRUE)^11                        -0.734 0.463117    
    ## fourier_vec(date, type = "sin", K = 1, period = 7)  -5.117 3.47e-07 ***
    ## fourier_vec(date, type = "cos", K = 1, period = 7)   3.344 0.000845 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 58.73 on 1616 degrees of freedom
    ##   (49 observations deleted due to missingness)
    ## Multiple R-squared:  0.2379, Adjusted R-squared:  0.2275 
    ## F-statistic: 22.93 on 22 and 1616 DF,  p-value: < 2.2e-16

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
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 4.06441815427408"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 21, 63 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 3.99997022059981"
    ## [1] "BEST method = 'lin', seasonal.factor = c( 21, 63 )"
    ## [1] "BEST lin OBJECTIVE FUNCTION = 3.99997022059981"
    ## [1] "CURRNET METHOD: nonlin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'nonlin' , seasonal.factor =  c( 21, 63 ) ...)"
    ## [1] "CURRENT nonlin OBJECTIVE FUNCTION = 5.71904938021195"
    ## [1] "BEST method = 'nonlin' PATH MEMBER = c( 21, 63 )"
    ## [1] "BEST nonlin OBJECTIVE FUNCTION = 5.71904938021195"
    ## [1] "CURRNET METHOD: both"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'both' , seasonal.factor =  c( 21, 63 ) ...)"
    ## [1] "CURRENT both OBJECTIVE FUNCTION = 5.14228495373323"
    ## [1] "BEST method = 'both' PATH MEMBER = c( 21, 63 )"
    ## [1] "BEST both OBJECTIVE FUNCTION = 5.14228495373323"

![](man/figures/README-nns_forecasting-1.png)<!-- -->

    ## Package: healthyR.ai
    ## [1] "CURRNET METHOD: lin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 7 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 3.60033053662806"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 7, 42 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 3.19460828948664"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 7, 42, 49 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 2.93008969213022"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 7, 42, 49, 63 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 2.58776659762755"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 7, 42, 49, 63, 98 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 2.51456616972992"
    ## [1] "BEST method = 'lin', seasonal.factor = c( 7, 42, 49, 63, 98 )"
    ## [1] "BEST lin OBJECTIVE FUNCTION = 2.51456616972992"
    ## [1] "CURRNET METHOD: nonlin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'nonlin' , seasonal.factor =  c( 7, 42, 49, 63, 98 ) ...)"
    ## [1] "CURRENT nonlin OBJECTIVE FUNCTION = 12.0371882649282"
    ## [1] "BEST method = 'nonlin' PATH MEMBER = c( 7, 42, 49, 63, 98 )"
    ## [1] "BEST nonlin OBJECTIVE FUNCTION = 12.0371882649282"
    ## [1] "CURRNET METHOD: both"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'both' , seasonal.factor =  c( 7, 42, 49, 63, 98 ) ...)"
    ## [1] "CURRENT both OBJECTIVE FUNCTION = 5.34961924971309"
    ## [1] "BEST method = 'both' PATH MEMBER = c( 7, 42, 49, 63, 98 )"
    ## [1] "BEST both OBJECTIVE FUNCTION = 5.34961924971309"

![](man/figures/README-nns_forecasting-2.png)<!-- -->

    ## Package: healthyR.data
    ## [1] "CURRNET METHOD: lin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 77 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 3.1150174215318"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 77, 42 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 2.80398201202113"
    ## [1] "BEST method = 'lin', seasonal.factor = c( 77, 42 )"
    ## [1] "BEST lin OBJECTIVE FUNCTION = 2.80398201202113"
    ## [1] "CURRNET METHOD: nonlin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'nonlin' , seasonal.factor =  c( 77, 42 ) ...)"
    ## [1] "CURRENT nonlin OBJECTIVE FUNCTION = 2.47617737784653"
    ## [1] "BEST method = 'nonlin' PATH MEMBER = c( 77, 42 )"
    ## [1] "BEST nonlin OBJECTIVE FUNCTION = 2.47617737784653"
    ## [1] "CURRNET METHOD: both"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'both' , seasonal.factor =  c( 77, 42 ) ...)"
    ## [1] "CURRENT both OBJECTIVE FUNCTION = 2.37975545264338"
    ## [1] "BEST method = 'both' PATH MEMBER = c( 77, 42 )"
    ## [1] "BEST both OBJECTIVE FUNCTION = 2.37975545264338"

![](man/figures/README-nns_forecasting-3.png)<!-- -->

    ## Package: healthyR.ts
    ## [1] "CURRNET METHOD: lin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 63 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 2.180961870352"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 63, 49 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 2.11402508580056"
    ## [1] "BEST method = 'lin', seasonal.factor = c( 63, 49 )"
    ## [1] "BEST lin OBJECTIVE FUNCTION = 2.11402508580056"
    ## [1] "CURRNET METHOD: nonlin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'nonlin' , seasonal.factor =  c( 63, 49 ) ...)"
    ## [1] "CURRENT nonlin OBJECTIVE FUNCTION = 14.348684603633"
    ## [1] "BEST method = 'nonlin' PATH MEMBER = c( 63, 49 )"
    ## [1] "BEST nonlin OBJECTIVE FUNCTION = 14.348684603633"
    ## [1] "CURRNET METHOD: both"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'both' , seasonal.factor =  c( 63, 49 ) ...)"
    ## [1] "CURRENT both OBJECTIVE FUNCTION = 5.58645665743536"
    ## [1] "BEST method = 'both' PATH MEMBER = c( 63, 49 )"
    ## [1] "BEST both OBJECTIVE FUNCTION = 5.58645665743536"

![](man/figures/README-nns_forecasting-4.png)<!-- -->

    ## Package: healthyverse
    ## [1] "CURRNET METHOD: lin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 77 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 4.40572305802886"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 77, 63 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 3.49730878545913"
    ## [1] "BEST method = 'lin', seasonal.factor = c( 77, 63 )"
    ## [1] "BEST lin OBJECTIVE FUNCTION = 3.49730878545913"
    ## [1] "CURRNET METHOD: nonlin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'nonlin' , seasonal.factor =  c( 77, 63 ) ...)"
    ## [1] "CURRENT nonlin OBJECTIVE FUNCTION = 4.79994825981482"
    ## [1] "BEST method = 'nonlin' PATH MEMBER = c( 77, 63 )"
    ## [1] "BEST nonlin OBJECTIVE FUNCTION = 4.79994825981482"
    ## [1] "CURRNET METHOD: both"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'both' , seasonal.factor =  c( 77, 63 ) ...)"
    ## [1] "CURRENT both OBJECTIVE FUNCTION = 4.11550241366429"
    ## [1] "BEST method = 'both' PATH MEMBER = c( 77, 63 )"
    ## [1] "BEST both OBJECTIVE FUNCTION = 4.11550241366429"

![](man/figures/README-nns_forecasting-5.png)<!-- -->

    ## Package: RandomWalker
    ## [1] "CURRNET METHOD: lin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 77 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 2.04863313671662"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 77, 49 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 1.87720402400082"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 77, 49, 42 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 1.78624622621924"
    ## [1] "BEST method = 'lin', seasonal.factor = c( 77, 49, 42 )"
    ## [1] "BEST lin OBJECTIVE FUNCTION = 1.78624622621924"
    ## [1] "CURRNET METHOD: nonlin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'nonlin' , seasonal.factor =  c( 77, 49, 42 ) ...)"
    ## [1] "CURRENT nonlin OBJECTIVE FUNCTION = 1.60227694331498"
    ## [1] "BEST method = 'nonlin' PATH MEMBER = c( 77, 49, 42 )"
    ## [1] "BEST nonlin OBJECTIVE FUNCTION = 1.60227694331498"
    ## [1] "CURRNET METHOD: both"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'both' , seasonal.factor =  c( 77, 49, 42 ) ...)"
    ## [1] "CURRENT both OBJECTIVE FUNCTION = 1.52631578009582"
    ## [1] "BEST method = 'both' PATH MEMBER = c( 77, 49, 42 )"
    ## [1] "BEST both OBJECTIVE FUNCTION = 1.52631578009582"

![](man/figures/README-nns_forecasting-6.png)<!-- -->

    ## Package: tidyAML
    ## [1] "CURRNET METHOD: lin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 84 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 4.39880918004636"
    ## [1] "BEST method = 'lin', seasonal.factor = c( 84 )"
    ## [1] "BEST lin OBJECTIVE FUNCTION = 4.39880918004636"
    ## [1] "CURRNET METHOD: nonlin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'nonlin' , seasonal.factor =  c( 84 ) ...)"
    ## [1] "CURRENT nonlin OBJECTIVE FUNCTION = 2.82357455153159"
    ## [1] "BEST method = 'nonlin' PATH MEMBER = c( 84 )"
    ## [1] "BEST nonlin OBJECTIVE FUNCTION = 2.82357455153159"
    ## [1] "CURRNET METHOD: both"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'both' , seasonal.factor =  c( 84 ) ...)"
    ## [1] "CURRENT both OBJECTIVE FUNCTION = 3.29418925930975"
    ## [1] "BEST method = 'both' PATH MEMBER = c( 84 )"
    ## [1] "BEST both OBJECTIVE FUNCTION = 3.29418925930975"

![](man/figures/README-nns_forecasting-7.png)<!-- -->

    ## Package: TidyDensity
    ## [1] "CURRNET METHOD: lin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 63 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 3.02132032187574"
    ## [1] "BEST method = 'lin', seasonal.factor = c( 63 )"
    ## [1] "BEST lin OBJECTIVE FUNCTION = 3.02132032187574"
    ## [1] "CURRNET METHOD: nonlin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'nonlin' , seasonal.factor =  c( 63 ) ...)"
    ## [1] "CURRENT nonlin OBJECTIVE FUNCTION = 9.59992274536936"
    ## [1] "BEST method = 'nonlin' PATH MEMBER = c( 63 )"
    ## [1] "BEST nonlin OBJECTIVE FUNCTION = 9.59992274536936"
    ## [1] "CURRNET METHOD: both"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'both' , seasonal.factor =  c( 63 ) ...)"
    ## [1] "CURRENT both OBJECTIVE FUNCTION = 5.60812282893831"
    ## [1] "BEST method = 'both' PATH MEMBER = c( 63 )"
    ## [1] "BEST both OBJECTIVE FUNCTION = 5.60812282893831"

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
    ## 1 healthyR.data <tibble [1,680 × 2]> <tibble [28 × 2]> <split [1652|28]>
    ## 2 healthyR      <tibble [1,674 × 2]> <tibble [28 × 2]> <split [1646|28]>
    ## 3 healthyR.ts   <tibble [1,618 × 2]> <tibble [28 × 2]> <split [1590|28]>
    ## 4 healthyverse  <tibble [1,588 × 2]> <tibble [28 × 2]> <split [1560|28]>
    ## 5 healthyR.ai   <tibble [1,413 × 2]> <tibble [28 × 2]> <split [1385|28]>
    ## 6 TidyDensity   <tibble [1,264 × 2]> <tibble [28 × 2]> <split [1236|28]>
    ## 7 tidyAML       <tibble [872 × 2]>   <tibble [28 × 2]> <split [844|28]> 
    ## 8 RandomWalker  <tibble [294 × 2]>   <tibble [28 × 2]> <split [266|28]>

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
| healthyR.data | 1 | ARIMA | Test | 0.6387831 | 121.52354 | 0.8043779 | 124.99035 | 0.7748652 | 0.0066078 |
| healthyR.data | 2 | LM | Test | 0.6428732 | 127.06714 | 0.8095284 | 119.11102 | 0.7833459 | 0.0178960 |
| healthyR.data | 3 | NULL | NA | NA | NA | NA | NA | NA | NA |
| healthyR.data | 4 | NNAR | Test | 0.7023367 | 106.53792 | 0.8844069 | 191.94467 | 0.8212247 | 0.0075983 |
| healthyR | 1 | ARIMA | Test | 0.7165756 | 151.03138 | 0.7549643 | 151.49377 | 0.9424499 | 0.0212405 |
| healthyR | 2 | LM | Test | 0.7007660 | 100.86055 | 0.7383077 | 152.03003 | 0.9476467 | 0.0252489 |
| healthyR | 3 | NULL | NA | NA | NA | NA | NA | NA | NA |
| healthyR | 4 | NNAR | Test | 0.7072660 | 115.89929 | 0.7451560 | 149.91539 | 0.9495003 | 0.0012760 |
| healthyR.ts | 1 | ARIMA | Test | 0.8794214 | 138.80343 | 0.7119328 | 162.66473 | 1.1412892 | 0.0060599 |
| healthyR.ts | 2 | LM | Test | 0.8647180 | 156.47097 | 0.7000297 | 139.51937 | 1.1164574 | 0.0786569 |
| healthyR.ts | 3 | NULL | NA | NA | NA | NA | NA | NA | NA |
| healthyR.ts | 4 | NNAR | Test | 0.8357687 | 96.20768 | 0.6765938 | 174.71452 | 1.1226136 | 0.0893112 |
| healthyverse | 1 | ARIMA | Test | 0.6686801 | 228.61384 | 0.7549952 | 90.62072 | 0.8515577 | 0.0014272 |
| healthyverse | 2 | LM | Test | 0.6673951 | 232.01775 | 0.7535444 | 90.31555 | 0.8477696 | 0.0087579 |
| healthyverse | 3 | NULL | NA | NA | NA | NA | NA | NA | NA |
| healthyverse | 4 | NNAR | Test | 0.6570521 | 139.09648 | 0.7418662 | 101.32494 | 0.8665181 | 0.0218361 |
| healthyR.ai | 1 | ARIMA | Test | 0.6284083 | 104.63595 | 0.7283585 | 142.86817 | 0.7779658 | 0.0001081 |
| healthyR.ai | 2 | LM | Test | 0.6252170 | 101.68148 | 0.7246596 | 143.54901 | 0.7752920 | 0.0015936 |
| healthyR.ai | 3 | NULL | NA | NA | NA | NA | NA | NA | NA |
| healthyR.ai | 4 | NNAR | Test | 0.6282327 | 104.29692 | 0.7281549 | 143.41718 | 0.7739091 | 0.0048857 |
| TidyDensity | 1 | ARIMA | Test | 0.4972813 | 85.25293 | 0.7161799 | 107.17167 | 0.7142283 | 0.0123547 |
| TidyDensity | 2 | LM | Test | 0.4793384 | 126.22609 | 0.6903387 | 82.28127 | 0.6872720 | 0.0127604 |
| TidyDensity | 3 | NULL | NA | NA | NA | NA | NA | NA | NA |
| TidyDensity | 4 | NNAR | Test | 0.5424263 | 91.17799 | 0.7811973 | 124.34279 | 0.7490766 | 0.0019546 |
| tidyAML | 1 | ARIMA | Test | 0.5796674 | 140.11576 | 0.8061651 | 89.87722 | 0.7417726 | 0.0176508 |
| tidyAML | 2 | LM | Test | 0.5947299 | 165.19158 | 0.8271131 | 89.56647 | 0.7424083 | 0.0615052 |
| tidyAML | 3 | NULL | NA | NA | NA | NA | NA | NA | NA |
| tidyAML | 4 | NNAR | Test | 0.5807568 | 167.80904 | 0.8076802 | 87.54036 | 0.7223793 | 0.1041650 |
| RandomWalker | 1 | ARIMA | Test | 1.0605624 | 106.27020 | 0.6520745 | 155.27585 | 1.2565860 | 0.0246102 |
| RandomWalker | 2 | LM | Test | 1.1015392 | 100.44640 | 0.6772686 | 179.26902 | 1.2569323 | 0.0489512 |
| RandomWalker | 3 | NULL | NA | NA | NA | NA | NA | NA | NA |
| RandomWalker | 4 | NNAR | Test | 1.0495618 | 133.10017 | 0.6453109 | 140.97182 | 1.1989081 | 0.1292908 |

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
    ## 1 healthyR.da…         1 ARIMA       Test  0.639  122. 0.804 125.  0.775 0.00661
    ## 2 healthyR             1 ARIMA       Test  0.717  151. 0.755 151.  0.942 0.0212 
    ## 3 healthyR.ts          2 LM          Test  0.865  156. 0.700 140.  1.12  0.0787 
    ## 4 healthyverse         2 LM          Test  0.667  232. 0.754  90.3 0.848 0.00876
    ## 5 healthyR.ai          4 NNAR        Test  0.628  104. 0.728 143.  0.774 0.00489
    ## 6 TidyDensity          2 LM          Test  0.479  126. 0.690  82.3 0.687 0.0128 
    ## 7 tidyAML              4 NNAR        Test  0.581  168. 0.808  87.5 0.722 0.104  
    ## 8 RandomWalker         4 NNAR        Test  1.05   133. 0.645 141.  1.20  0.129

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
    ## 1 healthyR.data <tibble>     <tibble>     <split [1652|28]> <mdl_tm_t [1 × 5]>
    ## 2 healthyR      <tibble>     <tibble>     <split [1646|28]> <mdl_tm_t [1 × 5]>
    ## 3 healthyR.ts   <tibble>     <tibble>     <split [1590|28]> <mdl_tm_t [1 × 5]>
    ## 4 healthyverse  <tibble>     <tibble>     <split [1560|28]> <mdl_tm_t [1 × 5]>
    ## 5 healthyR.ai   <tibble>     <tibble>     <split [1385|28]> <mdl_tm_t [1 × 5]>
    ## 6 TidyDensity   <tibble>     <tibble>     <split [1236|28]> <mdl_tm_t [1 × 5]>
    ## 7 tidyAML       <tibble>     <tibble>     <split [844|28]>  <mdl_tm_t [1 × 5]>
    ## 8 RandomWalker  <tibble>     <tibble>     <split [266|28]>  <mdl_tm_t [1 × 5]>

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
