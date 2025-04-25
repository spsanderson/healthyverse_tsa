Time Series Analysis and Nested Modeling of the Healthyverse Packages
================
Steven P. Sanderson II, MPH - Date:
25 April, 2025

This analysis follows a *Nested Modeltime Workflow*.

## Get Data

``` r
glimpse(downloads_tbl)
```

    ## Rows: 137,962
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

The last day in the data set is 2025-04-23 23:50:51, the file was
birthed on: 2024-08-07 07:35:44, and at report knit time is -6228.25
hours old. Happy analyzing!

Now that we have our data lets take a look at it using the `skimr`
package.

``` r
skim(downloads_tbl)
```

|                                                  |               |
|:-------------------------------------------------|:--------------|
| Name                                             | downloads_tbl |
| Number of rows                                   | 137962        |
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
| r_version     |     99291 |          0.28 |   5 |   5 |     0 |       46 |          0 |
| r_arch        |     99291 |          0.28 |   3 |   7 |     0 |        5 |          0 |
| r_os          |     99291 |          0.28 |   7 |  15 |     0 |       22 |          0 |
| package       |         0 |          1.00 |   7 |  13 |     0 |        8 |          0 |
| version       |         0 |          1.00 |   5 |  17 |     0 |       60 |          0 |
| country       |     11652 |          0.92 |   2 |   2 |     0 |      163 |          0 |

**Variable type: Date**

| skim_variable | n_missing | complete_rate | min | max | median | n_unique |
|:---|---:|---:|:---|:---|:---|---:|
| date | 0 | 1 | 2020-11-23 | 2025-04-23 | 2023-06-08 | 1613 |

**Variable type: numeric**

| skim_variable | n_missing | complete_rate | mean | sd | p0 | p25 | p50 | p75 | p100 | hist |
|:---|---:|---:|---:|---:|---:|---:|---:|---:|---:|:---|
| size | 0 | 1 | 1135390.77 | 1521900.90 | 355 | 14701 | 278019.5 | 2367773.00 | 5677952 | ▇▁▂▁▁ |
| ip_id | 0 | 1 | 10378.68 | 18424.62 | 1 | 298 | 3064.0 | 11717.25 | 209747 | ▇▁▁▁▁ |

**Variable type: POSIXct**

| skim_variable | n_missing | complete_rate | min | max | median | n_unique |
|:---|---:|---:|:---|:---|:---|---:|
| date_time | 0 | 1 | 2020-11-23 09:00:41 | 2025-04-23 23:50:51 | 2023-06-08 02:45:25 | 84062 |

**Variable type: Timespan**

| skim_variable | n_missing | complete_rate | min | max | median | n_unique |
|:--------------|----------:|--------------:|----:|----:|-------:|---------:|
| time          |         0 |             1 |   0 |  59 |     30 |       60 |

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
    ## -147.92  -35.72  -10.66   26.64  813.81 
    ## 
    ## Coefficients:
    ##                                                      Estimate Std. Error
    ## (Intercept)                                        -1.840e+02  7.203e+01
    ## date                                                1.121e-02  3.821e-03
    ## lag(value, 1)                                       1.058e-01  2.469e-02
    ## lag(value, 7)                                       9.542e-02  2.557e-02
    ## lag(value, 14)                                      9.707e-02  2.568e-02
    ## lag(value, 21)                                      6.904e-02  2.573e-02
    ## lag(value, 28)                                      6.623e-02  2.570e-02
    ## lag(value, 35)                                      6.757e-02  2.576e-02
    ## lag(value, 42)                                      4.931e-02  2.587e-02
    ## lag(value, 49)                                      6.769e-02  2.570e-02
    ## month(date, label = TRUE).L                        -1.018e+01  5.133e+00
    ## month(date, label = TRUE).Q                         2.787e+00  5.195e+00
    ## month(date, label = TRUE).C                        -1.244e+01  5.195e+00
    ## month(date, label = TRUE)^4                        -6.501e+00  5.197e+00
    ## month(date, label = TRUE)^5                        -1.227e+01  5.199e+00
    ## month(date, label = TRUE)^6                        -3.097e+00  5.250e+00
    ## month(date, label = TRUE)^7                        -6.236e+00  5.167e+00
    ## month(date, label = TRUE)^8                        -4.383e+00  5.167e+00
    ## month(date, label = TRUE)^9                         5.302e+00  5.168e+00
    ## month(date, label = TRUE)^10                        4.595e+00  5.251e+00
    ## month(date, label = TRUE)^11                       -5.912e+00  5.334e+00
    ## fourier_vec(date, type = "sin", K = 1, period = 7) -1.155e+01  2.378e+00
    ## fourier_vec(date, type = "cos", K = 1, period = 7)  8.072e+00  2.505e+00
    ##                                                    t value Pr(>|t|)    
    ## (Intercept)                                         -2.555 0.010711 *  
    ## date                                                 2.934 0.003395 ** 
    ## lag(value, 1)                                        4.285 1.94e-05 ***
    ## lag(value, 7)                                        3.732 0.000197 ***
    ## lag(value, 14)                                       3.780 0.000163 ***
    ## lag(value, 21)                                       2.683 0.007371 ** 
    ## lag(value, 28)                                       2.577 0.010060 *  
    ## lag(value, 35)                                       2.623 0.008804 ** 
    ## lag(value, 42)                                       1.906 0.056804 .  
    ## lag(value, 49)                                       2.634 0.008527 ** 
    ## month(date, label = TRUE).L                         -1.982 0.047609 *  
    ## month(date, label = TRUE).Q                          0.536 0.591745    
    ## month(date, label = TRUE).C                         -2.395 0.016737 *  
    ## month(date, label = TRUE)^4                         -1.251 0.211140    
    ## month(date, label = TRUE)^5                         -2.360 0.018400 *  
    ## month(date, label = TRUE)^6                         -0.590 0.555352    
    ## month(date, label = TRUE)^7                         -1.207 0.227677    
    ## month(date, label = TRUE)^8                         -0.848 0.396391    
    ## month(date, label = TRUE)^9                          1.026 0.305066    
    ## month(date, label = TRUE)^10                         0.875 0.381628    
    ## month(date, label = TRUE)^11                        -1.108 0.267919    
    ## fourier_vec(date, type = "sin", K = 1, period = 7)  -4.858 1.31e-06 ***
    ## fourier_vec(date, type = "cos", K = 1, period = 7)   3.223 0.001297 ** 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 58.77 on 1541 degrees of freedom
    ##   (49 observations deleted due to missingness)
    ## Multiple R-squared:  0.2464, Adjusted R-squared:  0.2357 
    ## F-statistic: 22.91 on 22 and 1541 DF,  p-value: < 2.2e-16

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
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 70 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 3.94315090938525"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 70, 63 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 3.42511996893374"
    ## [1] "BEST method = 'lin', seasonal.factor = c( 70, 63 )"
    ## [1] "BEST lin OBJECTIVE FUNCTION = 3.42511996893374"
    ## [1] "CURRNET METHOD: nonlin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'nonlin' , seasonal.factor =  c( 70, 63 ) ...)"
    ## [1] "CURRENT nonlin OBJECTIVE FUNCTION = 5.18142554228718"
    ## [1] "BEST method = 'nonlin' PATH MEMBER = c( 70, 63 )"
    ## [1] "BEST nonlin OBJECTIVE FUNCTION = 5.18142554228718"
    ## [1] "CURRNET METHOD: both"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'both' , seasonal.factor =  c( 70, 63 ) ...)"
    ## [1] "CURRENT both OBJECTIVE FUNCTION = 4.08806747552172"
    ## [1] "BEST method = 'both' PATH MEMBER = c( 70, 63 )"
    ## [1] "BEST both OBJECTIVE FUNCTION = 4.08806747552172"

![](man/figures/README-nns_forecasting-1.png)<!-- -->

    ## Package: healthyR.ai
    ## [1] "CURRNET METHOD: lin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 28 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 2.99075230277401"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 28, 63 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 2.85541805450539"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 28, 63, 84 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 2.75122004274312"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 28, 63, 84, 91 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 2.66803166786165"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 28, 63, 84, 91, 70 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 2.6266006097587"
    ## [1] "BEST method = 'lin', seasonal.factor = c( 28, 63, 84, 91, 70 )"
    ## [1] "BEST lin OBJECTIVE FUNCTION = 2.6266006097587"
    ## [1] "CURRNET METHOD: nonlin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'nonlin' , seasonal.factor =  c( 28, 63, 84, 91, 70 ) ...)"
    ## [1] "CURRENT nonlin OBJECTIVE FUNCTION = 3.59715096569321"
    ## [1] "BEST method = 'nonlin' PATH MEMBER = c( 28, 63, 84, 91, 70 )"
    ## [1] "BEST nonlin OBJECTIVE FUNCTION = 3.59715096569321"
    ## [1] "CURRNET METHOD: both"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'both' , seasonal.factor =  c( 28, 63, 84, 91, 70 ) ...)"
    ## [1] "CURRENT both OBJECTIVE FUNCTION = 3.00299333204891"
    ## [1] "BEST method = 'both' PATH MEMBER = c( 28, 63, 84, 91, 70 )"
    ## [1] "BEST both OBJECTIVE FUNCTION = 3.00299333204891"

![](man/figures/README-nns_forecasting-2.png)<!-- -->

    ## Package: healthyR.data
    ## [1] "CURRNET METHOD: lin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 91 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 3.23480235802104"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 91, 98 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 2.62385437324976"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 91, 98, 63 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 2.33624578602086"
    ## [1] "BEST method = 'lin', seasonal.factor = c( 91, 98, 63 )"
    ## [1] "BEST lin OBJECTIVE FUNCTION = 2.33624578602086"
    ## [1] "CURRNET METHOD: nonlin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'nonlin' , seasonal.factor =  c( 91, 98, 63 ) ...)"
    ## [1] "CURRENT nonlin OBJECTIVE FUNCTION = 2.81572359284023"
    ## [1] "BEST method = 'nonlin' PATH MEMBER = c( 91, 98, 63 )"
    ## [1] "BEST nonlin OBJECTIVE FUNCTION = 2.81572359284023"
    ## [1] "CURRNET METHOD: both"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'both' , seasonal.factor =  c( 91, 98, 63 ) ...)"
    ## [1] "CURRENT both OBJECTIVE FUNCTION = 2.37001724046608"
    ## [1] "BEST method = 'both' PATH MEMBER = c( 91, 98, 63 )"
    ## [1] "BEST both OBJECTIVE FUNCTION = 2.37001724046608"

![](man/figures/README-nns_forecasting-3.png)<!-- -->

    ## Package: healthyR.ts
    ## [1] "CURRNET METHOD: lin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 63 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 3.57532066442321"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 63, 70 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 3.13114014026799"
    ## [1] "BEST method = 'lin', seasonal.factor = c( 63, 70 )"
    ## [1] "BEST lin OBJECTIVE FUNCTION = 3.13114014026799"
    ## [1] "CURRNET METHOD: nonlin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'nonlin' , seasonal.factor =  c( 63, 70 ) ...)"
    ## [1] "CURRENT nonlin OBJECTIVE FUNCTION = 4.75301423941003"
    ## [1] "BEST method = 'nonlin' PATH MEMBER = c( 63, 70 )"
    ## [1] "BEST nonlin OBJECTIVE FUNCTION = 4.75301423941003"
    ## [1] "CURRNET METHOD: both"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'both' , seasonal.factor =  c( 63, 70 ) ...)"
    ## [1] "CURRENT both OBJECTIVE FUNCTION = 3.75421457231103"
    ## [1] "BEST method = 'both' PATH MEMBER = c( 63, 70 )"
    ## [1] "BEST both OBJECTIVE FUNCTION = 3.75421457231103"

![](man/figures/README-nns_forecasting-4.png)<!-- -->

    ## Package: healthyverse
    ## [1] "CURRNET METHOD: lin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 35 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 5.60205444188786"
    ## [1] "BEST method = 'lin', seasonal.factor = c( 35 )"
    ## [1] "BEST lin OBJECTIVE FUNCTION = 5.60205444188786"
    ## [1] "CURRNET METHOD: nonlin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'nonlin' , seasonal.factor =  c( 35 ) ...)"
    ## [1] "CURRENT nonlin OBJECTIVE FUNCTION = 11.8973605516513"
    ## [1] "BEST method = 'nonlin' PATH MEMBER = c( 35 )"
    ## [1] "BEST nonlin OBJECTIVE FUNCTION = 11.8973605516513"
    ## [1] "CURRNET METHOD: both"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'both' , seasonal.factor =  c( 35 ) ...)"
    ## [1] "CURRENT both OBJECTIVE FUNCTION = 8.87836943844484"
    ## [1] "BEST method = 'both' PATH MEMBER = c( 35 )"
    ## [1] "BEST both OBJECTIVE FUNCTION = 8.87836943844484"

![](man/figures/README-nns_forecasting-5.png)<!-- -->

    ## Package: RandomWalker
    ## [1] "CURRNET METHOD: lin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 42 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 2.94454882162532"
    ## [1] "BEST method = 'lin', seasonal.factor = c( 42 )"
    ## [1] "BEST lin OBJECTIVE FUNCTION = 2.94454882162532"
    ## [1] "CURRNET METHOD: nonlin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'nonlin' , seasonal.factor =  c( 42 ) ...)"
    ## [1] "CURRENT nonlin OBJECTIVE FUNCTION = 3.99374106966977"
    ## [1] "BEST method = 'nonlin' PATH MEMBER = c( 42 )"
    ## [1] "BEST nonlin OBJECTIVE FUNCTION = 3.99374106966977"
    ## [1] "CURRNET METHOD: both"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'both' , seasonal.factor =  c( 42 ) ...)"
    ## [1] "CURRENT both OBJECTIVE FUNCTION = 3.18563502551712"
    ## [1] "BEST method = 'both' PATH MEMBER = c( 42 )"
    ## [1] "BEST both OBJECTIVE FUNCTION = 3.18563502551712"

![](man/figures/README-nns_forecasting-6.png)<!-- -->

    ## Package: tidyAML
    ## [1] "CURRNET METHOD: lin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 28 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 3.84631313983007"
    ## [1] "BEST method = 'lin', seasonal.factor = c( 28 )"
    ## [1] "BEST lin OBJECTIVE FUNCTION = 3.84631313983007"
    ## [1] "CURRNET METHOD: nonlin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'nonlin' , seasonal.factor =  c( 28 ) ...)"
    ## [1] "CURRENT nonlin OBJECTIVE FUNCTION = 3.46240626413802"
    ## [1] "BEST method = 'nonlin' PATH MEMBER = c( 28 )"
    ## [1] "BEST nonlin OBJECTIVE FUNCTION = 3.46240626413802"
    ## [1] "CURRNET METHOD: both"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'both' , seasonal.factor =  c( 28 ) ...)"
    ## [1] "CURRENT both OBJECTIVE FUNCTION = 3.21198912045445"
    ## [1] "BEST method = 'both' PATH MEMBER = c( 28 )"
    ## [1] "BEST both OBJECTIVE FUNCTION = 3.21198912045445"

![](man/figures/README-nns_forecasting-7.png)<!-- -->

    ## Package: TidyDensity
    ## [1] "CURRNET METHOD: lin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 63 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 3.67028448210995"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 63, 35 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 3.52225248312259"
    ## [1] "BEST method = 'lin', seasonal.factor = c( 63, 35 )"
    ## [1] "BEST lin OBJECTIVE FUNCTION = 3.52225248312259"
    ## [1] "CURRNET METHOD: nonlin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'nonlin' , seasonal.factor =  c( 63, 35 ) ...)"
    ## [1] "CURRENT nonlin OBJECTIVE FUNCTION = 4.3142743227548"
    ## [1] "BEST method = 'nonlin' PATH MEMBER = c( 63, 35 )"
    ## [1] "BEST nonlin OBJECTIVE FUNCTION = 4.3142743227548"
    ## [1] "CURRNET METHOD: both"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'both' , seasonal.factor =  c( 63, 35 ) ...)"
    ## [1] "CURRENT both OBJECTIVE FUNCTION = 3.74947524612693"
    ## [1] "BEST method = 'both' PATH MEMBER = c( 63, 35 )"
    ## [1] "BEST both OBJECTIVE FUNCTION = 3.74947524612693"

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
    ## 1 healthyR.data <tibble [1,606 × 2]> <tibble [28 × 2]> <split [1578|28]>
    ## 2 healthyR      <tibble [1,599 × 2]> <tibble [28 × 2]> <split [1571|28]>
    ## 3 healthyR.ts   <tibble [1,543 × 2]> <tibble [28 × 2]> <split [1515|28]>
    ## 4 healthyverse  <tibble [1,514 × 2]> <tibble [28 × 2]> <split [1486|28]>
    ## 5 healthyR.ai   <tibble [1,338 × 2]> <tibble [28 × 2]> <split [1310|28]>
    ## 6 TidyDensity   <tibble [1,189 × 2]> <tibble [28 × 2]> <split [1161|28]>
    ## 7 tidyAML       <tibble [797 × 2]>   <tibble [28 × 2]> <split [769|28]> 
    ## 8 RandomWalker  <tibble [219 × 2]>   <tibble [28 × 2]> <split [191|28]>

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
| healthyR.data | 1 | ARIMA | Test | 0.8119675 | 106.09173 | 0.5667269 | 147.2177 | 0.9791020 | 0.0664329 |
| healthyR.data | 2 | LM | Test | 0.7969567 | 130.14058 | 0.5562499 | 134.5863 | 0.9467616 | 0.0108408 |
| healthyR.data | 3 | EARTH | Test | 0.9761578 | 232.03373 | 0.6813264 | 123.3737 | 1.1697437 | 0.0108408 |
| healthyR.data | 4 | NNAR | Test | 0.8016258 | 103.08275 | 0.5595088 | 179.5996 | 0.9793325 | 0.0000001 |
| healthyR | 1 | ARIMA | Test | 0.6729009 | 94.60582 | 0.6789461 | 171.0713 | 0.8569340 | 0.0016883 |
| healthyR | 2 | LM | Test | 0.6792552 | 97.37531 | 0.6853574 | 179.1648 | 0.8463061 | 0.0033899 |
| healthyR | 3 | EARTH | Test | 0.6816730 | 98.11458 | 0.6877970 | 157.5645 | 0.8432065 | 0.0033899 |
| healthyR | 4 | NNAR | Test | 0.6849794 | 97.20539 | 0.6911331 | 160.0868 | 0.8493549 | 0.1266110 |
| healthyR.ts | 1 | ARIMA | Test | 0.9741876 | 133.90065 | 0.6656652 | 154.3857 | 1.1887227 | 0.0060955 |
| healthyR.ts | 2 | LM | Test | 1.0038903 | 174.81766 | 0.6859612 | 136.2433 | 1.2656331 | 0.0060955 |
| healthyR.ts | 3 | EARTH | Test | 1.1345018 | 281.80037 | 0.7752084 | 134.3926 | 1.3378385 | 0.0060955 |
| healthyR.ts | 4 | NNAR | Test | 0.9563377 | 97.74350 | 0.6534683 | 185.6837 | 1.1519654 | 0.1606090 |
| healthyverse | 1 | ARIMA | Test | 0.7399776 | 256.29751 | 0.7571901 | 115.6270 | 0.8765719 | 0.0027832 |
| healthyverse | 2 | LM | Test | 0.7178546 | 309.84251 | 0.7345525 | 104.1931 | 0.8588975 | 0.0334792 |
| healthyverse | 3 | EARTH | Test | 0.7093389 | 194.79995 | 0.7258387 | 114.6431 | 0.9016571 | 0.0334792 |
| healthyverse | 4 | NNAR | Test | 0.7064777 | 175.92040 | 0.7229110 | 116.5875 | 0.9147379 | 0.0007192 |
| healthyR.ai | 1 | ARIMA | Test | 0.7546859 | 122.83029 | 0.6998472 | 179.0059 | 0.9481992 | 0.0357457 |
| healthyR.ai | 2 | LM | Test | 0.7244442 | 133.74037 | 0.6718030 | 154.4962 | 0.9028161 | 0.0539488 |
| healthyR.ai | 3 | EARTH | Test | 0.7276171 | 133.37527 | 0.6747454 | 155.9489 | 0.9064770 | 0.0539488 |
| healthyR.ai | 4 | NNAR | Test | 0.7187991 | 146.68101 | 0.6665680 | 146.3802 | 0.8932845 | 0.0000589 |
| TidyDensity | 1 | ARIMA | Test | 0.6242475 | 209.09678 | 0.7187209 | 115.4391 | 0.7566124 | 0.0433551 |
| TidyDensity | 2 | LM | Test | 0.6582801 | 275.95266 | 0.7579041 | 105.5566 | 0.8243525 | 0.0019528 |
| TidyDensity | 3 | EARTH | Test | 0.6372743 | 201.13137 | 0.7337192 | 116.5923 | 0.7711928 | 0.0019528 |
| TidyDensity | 4 | NNAR | Test | 0.6375964 | 124.71142 | 0.7340901 | 143.3852 | 0.7761000 | 0.0841651 |
| tidyAML | 1 | ARIMA | Test | 0.6015646 | 319.65245 | 0.8554189 | 103.5759 | 0.7160728 | 0.0529910 |
| tidyAML | 2 | LM | Test | 0.6112991 | 353.96448 | 0.8692614 | 99.1309 | 0.7551168 | 0.0301356 |
| tidyAML | 3 | EARTH | Test | 0.5679999 | 208.44076 | 0.8076902 | 110.9059 | 0.7260562 | 0.0301356 |
| tidyAML | 4 | NNAR | Test | 0.5935106 | 289.76740 | 0.8439662 | 104.8794 | 0.7256868 | 0.0169174 |
| RandomWalker | 1 | ARIMA | Test | 1.4068398 | 170.25603 | 0.6310015 | 144.6026 | 1.7112572 | 0.0592257 |
| RandomWalker | 2 | LM | Test | 1.2610725 | 110.75990 | 0.5656214 | 177.7619 | 1.4854812 | 0.0159481 |
| RandomWalker | 3 | EARTH | Test | 1.2576810 | 113.83585 | 0.5641002 | 172.7444 | 1.4879839 | NA |
| RandomWalker | 4 | NNAR | Test | 1.3874888 | 186.99705 | 0.6223221 | 163.8385 | 1.5117690 | 0.0185977 |

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
    ## 1 healthyR.da…         2 LM          Test  0.797 130.  0.556  135. 0.947 1.08e-2
    ## 2 healthyR             3 EARTH       Test  0.682  98.1 0.688  158. 0.843 3.39e-3
    ## 3 healthyR.ts          4 NNAR        Test  0.956  97.7 0.653  186. 1.15  1.61e-1
    ## 4 healthyverse         2 LM          Test  0.718 310.  0.735  104. 0.859 3.35e-2
    ## 5 healthyR.ai          4 NNAR        Test  0.719 147.  0.667  146. 0.893 5.89e-5
    ## 6 TidyDensity          1 ARIMA       Test  0.624 209.  0.719  115. 0.757 4.34e-2
    ## 7 tidyAML              1 ARIMA       Test  0.602 320.  0.855  104. 0.716 5.30e-2
    ## 8 RandomWalker         2 LM          Test  1.26  111.  0.566  178. 1.49  1.59e-2

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
    ## 1 healthyR.data <tibble>     <tibble>     <split [1578|28]> <mdl_tm_t [1 × 5]>
    ## 2 healthyR      <tibble>     <tibble>     <split [1571|28]> <mdl_tm_t [1 × 5]>
    ## 3 healthyR.ts   <tibble>     <tibble>     <split [1515|28]> <mdl_tm_t [1 × 5]>
    ## 4 healthyverse  <tibble>     <tibble>     <split [1486|28]> <mdl_tm_t [1 × 5]>
    ## 5 healthyR.ai   <tibble>     <tibble>     <split [1310|28]> <mdl_tm_t [1 × 5]>
    ## 6 TidyDensity   <tibble>     <tibble>     <split [1161|28]> <mdl_tm_t [1 × 5]>
    ## 7 tidyAML       <tibble>     <tibble>     <split [769|28]>  <mdl_tm_t [1 × 5]>
    ## 8 RandomWalker  <tibble>     <tibble>     <split [191|28]>  <mdl_tm_t [1 × 5]>

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
