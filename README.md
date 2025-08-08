Time Series Analysis, Modeling and Forecasting of the Healthyverse
Packages
================
Steven P. Sanderson II, MPH - Date:
08 August, 2025

This analysis follows a *Nested Modeltime Workflow*.

## Get Data

``` r
glimpse(downloads_tbl)
```

    ## Rows: 148,225
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

The last day in the data set is 2025-08-06 23:57:13, the file was
birthed on: 2024-08-07 07:35:44.428716, and at report knit time is
-8748.36 hours old. Happy analyzing!

Now that we have our data lets take a look at it using the `skimr`
package.

``` r
skim(downloads_tbl)
```

|                                                  |               |
|:-------------------------------------------------|:--------------|
| Name                                             | downloads_tbl |
| Number of rows                                   | 148225        |
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
| r_version     |    107345 |          0.28 |   5 |   5 |     0 |       48 |          0 |
| r_arch        |    107345 |          0.28 |   3 |   7 |     0 |        5 |          0 |
| r_os          |    107345 |          0.28 |   7 |  15 |     0 |       23 |          0 |
| package       |         0 |          1.00 |   7 |  13 |     0 |        8 |          0 |
| version       |         0 |          1.00 |   5 |  17 |     0 |       61 |          0 |
| country       |     12547 |          0.92 |   2 |   2 |     0 |      165 |          0 |

**Variable type: Date**

| skim_variable | n_missing | complete_rate | min | max | median | n_unique |
|:---|---:|---:|:---|:---|:---|---:|
| date | 0 | 1 | 2020-11-23 | 2025-08-06 | 2023-08-09 | 1718 |

**Variable type: numeric**

| skim_variable | n_missing | complete_rate | mean | sd | p0 | p25 | p50 | p75 | p100 | hist |
|:---|---:|---:|---:|---:|---:|---:|---:|---:|---:|:---|
| size | 0 | 1 | 1131664.14 | 1508646.08 | 355 | 14701 | 305422 | 2367566 | 5677952 | ▇▁▂▁▁ |
| ip_id | 0 | 1 | 10754.97 | 19550.33 | 1 | 286 | 3058 | 11961 | 257393 | ▇▁▁▁▁ |

**Variable type: POSIXct**

| skim_variable | n_missing | complete_rate | min | max | median | n_unique |
|:---|---:|---:|:---|:---|:---|---:|
| date_time | 0 | 1 | 2020-11-23 09:00:41 | 2025-08-06 23:57:13 | 2023-08-09 19:10:04 | 91647 |

**Variable type: Timespan**

| skim_variable | n_missing | complete_rate | min | max |     median | n_unique |
|:--------------|----------:|--------------:|----:|----:|-----------:|---------:|
| time          |         0 |             1 |   0 |  59 | 12H 6M 19S |       60 |

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
    ## -148.03  -36.13  -11.28   26.73  817.72 
    ## 
    ## Coefficients:
    ##                                                      Estimate Std. Error
    ## (Intercept)                                        -1.832e+02  6.412e+01
    ## date                                                1.123e-02  3.394e-03
    ## lag(value, 1)                                       1.029e-01  2.391e-02
    ## lag(value, 7)                                       9.229e-02  2.475e-02
    ## lag(value, 14)                                      8.515e-02  2.477e-02
    ## lag(value, 21)                                      6.535e-02  2.482e-02
    ## lag(value, 28)                                      6.710e-02  2.474e-02
    ## lag(value, 35)                                      6.644e-02  2.479e-02
    ## lag(value, 42)                                      6.014e-02  2.487e-02
    ## lag(value, 49)                                      6.431e-02  2.475e-02
    ## month(date, label = TRUE).L                        -9.573e+00  5.105e+00
    ## month(date, label = TRUE).Q                         3.062e+00  5.006e+00
    ## month(date, label = TRUE).C                        -1.370e+01  5.109e+00
    ## month(date, label = TRUE)^4                        -6.843e+00  5.073e+00
    ## month(date, label = TRUE)^5                        -1.076e+01  5.073e+00
    ## month(date, label = TRUE)^6                        -3.960e+00  5.126e+00
    ## month(date, label = TRUE)^7                        -7.603e+00  5.024e+00
    ## month(date, label = TRUE)^8                        -3.704e+00  5.009e+00
    ## month(date, label = TRUE)^9                         5.404e+00  4.996e+00
    ## month(date, label = TRUE)^10                        3.630e+00  4.996e+00
    ## month(date, label = TRUE)^11                       -2.844e+00  4.877e+00
    ## fourier_vec(date, type = "sin", K = 1, period = 7) -1.156e+01  2.286e+00
    ## fourier_vec(date, type = "cos", K = 1, period = 7)  8.112e+00  2.401e+00
    ##                                                    t value Pr(>|t|)    
    ## (Intercept)                                         -2.858 0.004319 ** 
    ## date                                                 3.308 0.000960 ***
    ## lag(value, 1)                                        4.302 1.79e-05 ***
    ## lag(value, 7)                                        3.728 0.000199 ***
    ## lag(value, 14)                                       3.438 0.000601 ***
    ## lag(value, 21)                                       2.633 0.008544 ** 
    ## lag(value, 28)                                       2.712 0.006749 ** 
    ## lag(value, 35)                                       2.680 0.007426 ** 
    ## lag(value, 42)                                       2.418 0.015723 *  
    ## lag(value, 49)                                       2.598 0.009447 ** 
    ## month(date, label = TRUE).L                         -1.875 0.060934 .  
    ## month(date, label = TRUE).Q                          0.612 0.540817    
    ## month(date, label = TRUE).C                         -2.683 0.007376 ** 
    ## month(date, label = TRUE)^4                         -1.349 0.177538    
    ## month(date, label = TRUE)^5                         -2.122 0.034028 *  
    ## month(date, label = TRUE)^6                         -0.773 0.439918    
    ## month(date, label = TRUE)^7                         -1.513 0.130426    
    ## month(date, label = TRUE)^8                         -0.739 0.459761    
    ## month(date, label = TRUE)^9                          1.081 0.279635    
    ## month(date, label = TRUE)^10                         0.727 0.467511    
    ## month(date, label = TRUE)^11                        -0.583 0.559891    
    ## fourier_vec(date, type = "sin", K = 1, period = 7)  -5.056 4.77e-07 ***
    ## fourier_vec(date, type = "cos", K = 1, period = 7)   3.378 0.000747 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 58.71 on 1646 degrees of freedom
    ##   (49 observations deleted due to missingness)
    ## Multiple R-squared:  0.2348, Adjusted R-squared:  0.2246 
    ## F-statistic: 22.96 on 22 and 1646 DF,  p-value: < 2.2e-16

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
#            sf <- NNS.seas(x, modulo = 7, plot = FALSE)$periods
            seas <- t(
                sapply(
                    1:25, 
                    function(i) c(
                        i,
                        sqrt(
                            mean((
                                NNS.ARMA(x, 
                                         h = 28, 
                                         training.set = train_set_size, 
                                         method = "lin", 
                                         seasonal.factor = i, 
                                         plot=FALSE
                                         ) - tail(AirPassengers, 44)) ^ 2)))
                    )
                )
            colnames(seas) <- c("Period", "RMSE")
            sf <- seas[which.min(seas[, 2]), 1]
            
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
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 20 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 32.697090129822"
    ## [1] "BEST method = 'lin' PATH MEMBER = c( 20 )"
    ## [1] "BEST lin OBJECTIVE FUNCTION = 32.697090129822"
    ## [1] "CURRNET METHOD: nonlin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'nonlin' , seasonal.factor =  c( 20 ) ...)"
    ## [1] "CURRENT nonlin OBJECTIVE FUNCTION = 15.8429830759704"
    ## [1] "BEST method = 'nonlin' PATH MEMBER = c( 20 )"
    ## [1] "BEST nonlin OBJECTIVE FUNCTION = 15.8429830759704"
    ## [1] "CURRNET METHOD: both"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'both' , seasonal.factor =  c( 20 ) ...)"
    ## [1] "CURRENT both OBJECTIVE FUNCTION = 27.9327794475526"
    ## [1] "BEST method = 'both' PATH MEMBER = c( 20 )"
    ## [1] "BEST both OBJECTIVE FUNCTION = 27.9327794475526"

![](man/figures/README-nns_forecasting-1.png)<!-- -->

    ## Package: healthyR.ai
    ## [1] "CURRNET METHOD: lin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 24 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 7.74458712263877"
    ## [1] "BEST method = 'lin' PATH MEMBER = c( 24 )"
    ## [1] "BEST lin OBJECTIVE FUNCTION = 7.74458712263877"
    ## [1] "CURRNET METHOD: nonlin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'nonlin' , seasonal.factor =  c( 24 ) ...)"
    ## [1] "CURRENT nonlin OBJECTIVE FUNCTION = 10.3674067450085"
    ## [1] "BEST method = 'nonlin' PATH MEMBER = c( 24 )"
    ## [1] "BEST nonlin OBJECTIVE FUNCTION = 10.3674067450085"
    ## [1] "CURRNET METHOD: both"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'both' , seasonal.factor =  c( 24 ) ...)"
    ## [1] "CURRENT both OBJECTIVE FUNCTION = 9.49774828912241"
    ## [1] "BEST method = 'both' PATH MEMBER = c( 24 )"
    ## [1] "BEST both OBJECTIVE FUNCTION = 9.49774828912241"

![](man/figures/README-nns_forecasting-2.png)<!-- -->

    ## Package: healthyR.data
    ## [1] "CURRNET METHOD: lin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 21 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 3.47171232725944"
    ## [1] "BEST method = 'lin' PATH MEMBER = c( 21 )"
    ## [1] "BEST lin OBJECTIVE FUNCTION = 3.47171232725944"
    ## [1] "CURRNET METHOD: nonlin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'nonlin' , seasonal.factor =  c( 21 ) ...)"
    ## [1] "CURRENT nonlin OBJECTIVE FUNCTION = 14.9907812746965"
    ## [1] "BEST method = 'nonlin' PATH MEMBER = c( 21 )"
    ## [1] "BEST nonlin OBJECTIVE FUNCTION = 14.9907812746965"
    ## [1] "CURRNET METHOD: both"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'both' , seasonal.factor =  c( 21 ) ...)"
    ## [1] "CURRENT both OBJECTIVE FUNCTION = 9.60766856728515"
    ## [1] "BEST method = 'both' PATH MEMBER = c( 21 )"
    ## [1] "BEST both OBJECTIVE FUNCTION = 9.60766856728515"

![](man/figures/README-nns_forecasting-3.png)<!-- -->

    ## Package: healthyR.ts
    ## [1] "CURRNET METHOD: lin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 20 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 21.6454463412903"
    ## [1] "BEST method = 'lin' PATH MEMBER = c( 20 )"
    ## [1] "BEST lin OBJECTIVE FUNCTION = 21.6454463412903"
    ## [1] "CURRNET METHOD: nonlin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'nonlin' , seasonal.factor =  c( 20 ) ...)"
    ## [1] "CURRENT nonlin OBJECTIVE FUNCTION = 17.9426786874228"
    ## [1] "BEST method = 'nonlin' PATH MEMBER = c( 20 )"
    ## [1] "BEST nonlin OBJECTIVE FUNCTION = 17.9426786874228"
    ## [1] "CURRNET METHOD: both"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'both' , seasonal.factor =  c( 20 ) ...)"
    ## [1] "CURRENT both OBJECTIVE FUNCTION = 24.8480527241383"
    ## [1] "BEST method = 'both' PATH MEMBER = c( 20 )"
    ## [1] "BEST both OBJECTIVE FUNCTION = 24.8480527241383"

![](man/figures/README-nns_forecasting-4.png)<!-- -->

    ## Package: healthyverse
    ## [1] "CURRNET METHOD: lin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 21 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 5.88325013396498"
    ## [1] "BEST method = 'lin' PATH MEMBER = c( 21 )"
    ## [1] "BEST lin OBJECTIVE FUNCTION = 5.88325013396498"
    ## [1] "CURRNET METHOD: nonlin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'nonlin' , seasonal.factor =  c( 21 ) ...)"
    ## [1] "CURRENT nonlin OBJECTIVE FUNCTION = 10.2835669456339"
    ## [1] "BEST method = 'nonlin' PATH MEMBER = c( 21 )"
    ## [1] "BEST nonlin OBJECTIVE FUNCTION = 10.2835669456339"
    ## [1] "CURRNET METHOD: both"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'both' , seasonal.factor =  c( 21 ) ...)"
    ## [1] "CURRENT both OBJECTIVE FUNCTION = 8.53495534609086"
    ## [1] "BEST method = 'both' PATH MEMBER = c( 21 )"
    ## [1] "BEST both OBJECTIVE FUNCTION = 8.53495534609086"

![](man/figures/README-nns_forecasting-5.png)<!-- -->

    ## Package: RandomWalker
    ## [1] "CURRNET METHOD: lin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 19 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 9.74861513027232"
    ## [1] "BEST method = 'lin' PATH MEMBER = c( 19 )"
    ## [1] "BEST lin OBJECTIVE FUNCTION = 9.74861513027232"
    ## [1] "CURRNET METHOD: nonlin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'nonlin' , seasonal.factor =  c( 19 ) ...)"
    ## [1] "CURRENT nonlin OBJECTIVE FUNCTION = 6.05868202382194"
    ## [1] "BEST method = 'nonlin' PATH MEMBER = c( 19 )"
    ## [1] "BEST nonlin OBJECTIVE FUNCTION = 6.05868202382194"
    ## [1] "CURRNET METHOD: both"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'both' , seasonal.factor =  c( 19 ) ...)"
    ## [1] "CURRENT both OBJECTIVE FUNCTION = 7.07860548008846"
    ## [1] "BEST method = 'both' PATH MEMBER = c( 19 )"
    ## [1] "BEST both OBJECTIVE FUNCTION = 7.07860548008846"

![](man/figures/README-nns_forecasting-6.png)<!-- -->

    ## Package: tidyAML
    ## [1] "CURRNET METHOD: lin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 21 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 3.60866682180547"
    ## [1] "BEST method = 'lin' PATH MEMBER = c( 21 )"
    ## [1] "BEST lin OBJECTIVE FUNCTION = 3.60866682180547"
    ## [1] "CURRNET METHOD: nonlin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'nonlin' , seasonal.factor =  c( 21 ) ...)"
    ## [1] "CURRENT nonlin OBJECTIVE FUNCTION = 4.94627269967496"
    ## [1] "BEST method = 'nonlin' PATH MEMBER = c( 21 )"
    ## [1] "BEST nonlin OBJECTIVE FUNCTION = 4.94627269967496"
    ## [1] "CURRNET METHOD: both"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'both' , seasonal.factor =  c( 21 ) ...)"
    ## [1] "CURRENT both OBJECTIVE FUNCTION = 3.19935000173713"
    ## [1] "BEST method = 'both' PATH MEMBER = c( 21 )"
    ## [1] "BEST both OBJECTIVE FUNCTION = 3.19935000173713"

![](man/figures/README-nns_forecasting-7.png)<!-- -->

    ## Package: TidyDensity
    ## [1] "CURRNET METHOD: lin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 21 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 8.77479490522185"
    ## [1] "BEST method = 'lin' PATH MEMBER = c( 21 )"
    ## [1] "BEST lin OBJECTIVE FUNCTION = 8.77479490522185"
    ## [1] "CURRNET METHOD: nonlin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'nonlin' , seasonal.factor =  c( 21 ) ...)"
    ## [1] "CURRENT nonlin OBJECTIVE FUNCTION = 22.0776006088673"
    ## [1] "BEST method = 'nonlin' PATH MEMBER = c( 21 )"
    ## [1] "BEST nonlin OBJECTIVE FUNCTION = 22.0776006088673"
    ## [1] "CURRNET METHOD: both"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'both' , seasonal.factor =  c( 21 ) ...)"
    ## [1] "CURRENT both OBJECTIVE FUNCTION = 17.9960515766965"
    ## [1] "BEST method = 'both' PATH MEMBER = c( 21 )"
    ## [1] "BEST both OBJECTIVE FUNCTION = 17.9960515766965"

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
    ## 1 healthyR.data <tibble [1,710 × 2]> <tibble [28 × 2]> <split [1682|28]>
    ## 2 healthyR      <tibble [1,702 × 2]> <tibble [28 × 2]> <split [1674|28]>
    ## 3 healthyR.ts   <tibble [1,647 × 2]> <tibble [28 × 2]> <split [1619|28]>
    ## 4 healthyverse  <tibble [1,618 × 2]> <tibble [28 × 2]> <split [1590|28]>
    ## 5 healthyR.ai   <tibble [1,443 × 2]> <tibble [28 × 2]> <split [1415|28]>
    ## 6 TidyDensity   <tibble [1,294 × 2]> <tibble [28 × 2]> <split [1266|28]>
    ## 7 tidyAML       <tibble [902 × 2]>   <tibble [28 × 2]> <split [874|28]> 
    ## 8 RandomWalker  <tibble [324 × 2]>   <tibble [28 × 2]> <split [296|28]>

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
| healthyR.data | 1 | ARIMA | Test | 0.6814569 | 157.78780 | 0.7095272 | 148.88894 | 0.8333607 | 0.0717006 |
| healthyR.data | 2 | LM | Test | 0.7480574 | 191.03271 | 0.7788711 | 149.87046 | 0.8746295 | 0.0308890 |
| healthyR.data | 3 | NULL | NA | NA | NA | NA | NA | NA | NA |
| healthyR.data | 4 | NNAR | Test | 0.6437474 | 101.21527 | 0.6702644 | 192.70025 | 0.8489208 | 0.0063225 |
| healthyR | 1 | ARIMA | Test | 0.6007297 | 126.60544 | 0.8742731 | 172.47340 | 0.7268245 | 0.0206853 |
| healthyR | 2 | LM | Test | 0.5830415 | 105.88845 | 0.8485306 | 168.14989 | 0.7218255 | 0.1689759 |
| healthyR | 3 | NULL | NA | NA | NA | NA | NA | NA | NA |
| healthyR | 4 | NNAR | Test | 0.5836182 | 112.40162 | 0.8493699 | 167.35861 | 0.7163611 | 0.0118303 |
| healthyR.ts | 1 | ARIMA | Test | 0.6450621 | 100.02432 | 0.7715047 | 138.69298 | 0.7751863 | 0.0042841 |
| healthyR.ts | 2 | LM | Test | 0.6163796 | 102.93943 | 0.7372000 | 116.85207 | 0.7600079 | 0.1716449 |
| healthyR.ts | 3 | NULL | NA | NA | NA | NA | NA | NA | NA |
| healthyR.ts | 4 | NNAR | Test | 0.6683499 | 99.74497 | 0.7993574 | 190.58733 | 0.7836293 | 0.0634156 |
| healthyverse | 1 | ARIMA | Test | 0.6269623 | 259.65013 | 0.9788935 | 102.39272 | 0.7650970 | 0.0006083 |
| healthyverse | 2 | LM | Test | 0.6386753 | 274.86832 | 0.9971813 | 100.67496 | 0.7835139 | 0.0787578 |
| healthyverse | 3 | NULL | NA | NA | NA | NA | NA | NA | NA |
| healthyverse | 4 | NNAR | Test | 0.6478342 | 193.16667 | 1.0114813 | 121.09069 | 0.7522840 | 0.0010484 |
| healthyR.ai | 1 | ARIMA | Test | 0.7284487 | 122.96240 | 0.9181119 | 155.82508 | 0.8874682 | 0.0249972 |
| healthyR.ai | 2 | LM | Test | 0.7172602 | 118.73086 | 0.9040103 | 153.29909 | 0.8810465 | 0.1845154 |
| healthyR.ai | 3 | NULL | NA | NA | NA | NA | NA | NA | NA |
| healthyR.ai | 4 | NNAR | Test | 0.7243877 | 115.93221 | 0.9129936 | 160.66754 | 0.8864555 | 0.0213679 |
| TidyDensity | 1 | ARIMA | Test | 0.9821185 | 99.05213 | 0.8327896 | 110.04492 | 1.2150535 | 0.0012765 |
| TidyDensity | 2 | LM | Test | 0.8629086 | 111.06757 | 0.7317053 | 86.43647 | 1.1148844 | 0.0313032 |
| TidyDensity | 3 | NULL | NA | NA | NA | NA | NA | NA | NA |
| TidyDensity | 4 | NNAR | Test | 1.0832237 | 98.55678 | 0.9185220 | 137.90068 | 1.3012109 | 0.0020238 |
| tidyAML | 1 | ARIMA | Test | 0.5067095 | 194.98052 | 1.0244792 | 106.41290 | 0.6387994 | 0.0000004 |
| tidyAML | 2 | LM | Test | 0.5488222 | 269.50225 | 1.1096237 | 102.24717 | 0.6389378 | 0.1331598 |
| tidyAML | 3 | NULL | NA | NA | NA | NA | NA | NA | NA |
| tidyAML | 4 | NNAR | Test | 0.5704781 | 237.03661 | 1.1534084 | 112.47896 | 0.6812986 | 0.2244843 |
| RandomWalker | 1 | ARIMA | Test | 1.0848934 | 99.55497 | 0.5412648 | 157.72502 | 1.3194189 | 0.0733815 |
| RandomWalker | 2 | LM | Test | 1.1567761 | 99.79781 | 0.5771278 | 168.18942 | 1.3631585 | 0.0149892 |
| RandomWalker | 3 | NULL | NA | NA | NA | NA | NA | NA | NA |
| RandomWalker | 4 | NNAR | Test | 1.2087100 | 114.92288 | 0.6030382 | 150.15128 | 1.4425071 | 0.0202554 |

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
    ## 1 healthyR.da…         1 ARIMA       Test  0.681 158.  0.710 149.  0.833 7.17e-2
    ## 2 healthyR             4 NNAR        Test  0.584 112.  0.849 167.  0.716 1.18e-2
    ## 3 healthyR.ts          2 LM          Test  0.616 103.  0.737 117.  0.760 1.72e-1
    ## 4 healthyverse         4 NNAR        Test  0.648 193.  1.01  121.  0.752 1.05e-3
    ## 5 healthyR.ai          2 LM          Test  0.717 119.  0.904 153.  0.881 1.85e-1
    ## 6 TidyDensity          2 LM          Test  0.863 111.  0.732  86.4 1.11  3.13e-2
    ## 7 tidyAML              1 ARIMA       Test  0.507 195.  1.02  106.  0.639 4.48e-7
    ## 8 RandomWalker         1 ARIMA       Test  1.08   99.6 0.541 158.  1.32  7.34e-2

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
    ## 1 healthyR.data <tibble>     <tibble>     <split [1682|28]> <mdl_tm_t [1 × 5]>
    ## 2 healthyR      <tibble>     <tibble>     <split [1674|28]> <mdl_tm_t [1 × 5]>
    ## 3 healthyR.ts   <tibble>     <tibble>     <split [1619|28]> <mdl_tm_t [1 × 5]>
    ## 4 healthyverse  <tibble>     <tibble>     <split [1590|28]> <mdl_tm_t [1 × 5]>
    ## 5 healthyR.ai   <tibble>     <tibble>     <split [1415|28]> <mdl_tm_t [1 × 5]>
    ## 6 TidyDensity   <tibble>     <tibble>     <split [1266|28]> <mdl_tm_t [1 × 5]>
    ## 7 tidyAML       <tibble>     <tibble>     <split [874|28]>  <mdl_tm_t [1 × 5]>
    ## 8 RandomWalker  <tibble>     <tibble>     <split [296|28]>  <mdl_tm_t [1 × 5]>

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
