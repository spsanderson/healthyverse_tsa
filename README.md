Time Series Analysis, Modeling and Forecasting of the Healthyverse
Packages
================
Steven P. Sanderson II, MPH - Date:
29 July, 2025

This analysis follows a *Nested Modeltime Workflow*.

## Get Data

``` r
glimpse(downloads_tbl)
```

    ## Rows: 146,779
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

The last day in the data set is 2025-07-27 23:55:21, the file was
birthed on: 2024-08-07 07:35:44.428716, and at report knit time is
-8508.33 hours old. Happy analyzing!

Now that we have our data lets take a look at it using the `skimr`
package.

``` r
skim(downloads_tbl)
```

|                                                  |               |
|:-------------------------------------------------|:--------------|
| Name                                             | downloads_tbl |
| Number of rows                                   | 146779        |
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
| r_version     |    106116 |          0.28 |   5 |   5 |     0 |       48 |          0 |
| r_arch        |    106116 |          0.28 |   3 |   7 |     0 |        5 |          0 |
| r_os          |    106116 |          0.28 |   7 |  15 |     0 |       23 |          0 |
| package       |         0 |          1.00 |   7 |  13 |     0 |        8 |          0 |
| version       |         0 |          1.00 |   5 |  17 |     0 |       61 |          0 |
| country       |     12381 |          0.92 |   2 |   2 |     0 |      163 |          0 |

**Variable type: Date**

| skim_variable | n_missing | complete_rate | min | max | median | n_unique |
|:---|---:|---:|:---|:---|:---|---:|
| date | 0 | 1 | 2020-11-23 | 2025-07-27 | 2023-08-01 | 1708 |

**Variable type: numeric**

| skim_variable | n_missing | complete_rate | mean | sd | p0 | p25 | p50 | p75 | p100 | hist |
|:---|---:|---:|---:|---:|---:|---:|---:|---:|---:|:---|
| size | 0 | 1 | 1131321.37 | 1510832.62 | 355 | 14701 | 302073 | 2367640 | 5677952 | ▇▁▂▁▁ |
| ip_id | 0 | 1 | 10486.04 | 18619.58 | 1 | 280 | 3030 | 11854 | 209747 | ▇▁▁▁▁ |

**Variable type: POSIXct**

| skim_variable | n_missing | complete_rate | min | max | median | n_unique |
|:---|---:|---:|:---|:---|:---|---:|
| date_time | 0 | 1 | 2020-11-23 09:00:41 | 2025-07-27 23:55:21 | 2023-08-01 15:47:43 | 90404 |

**Variable type: Timespan**

| skim_variable | n_missing | complete_rate | min | max |     median | n_unique |
|:--------------|----------:|--------------:|----:|----:|-----------:|---------:|
| time          |         0 |             1 |   0 |  59 | 12H 6M 18S |       60 |

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
    ## -148.23  -36.04  -11.19   26.76  817.19 
    ## 
    ## Coefficients:
    ##                                                      Estimate Std. Error
    ## (Intercept)                                        -1.702e+02  6.457e+01
    ## date                                                1.055e-02  3.418e-03
    ## lag(value, 1)                                       1.000e-01  2.398e-02
    ## lag(value, 7)                                       9.542e-02  2.483e-02
    ## lag(value, 14)                                      8.604e-02  2.484e-02
    ## lag(value, 21)                                      6.541e-02  2.489e-02
    ## lag(value, 28)                                      6.714e-02  2.477e-02
    ## lag(value, 35)                                      6.745e-02  2.481e-02
    ## lag(value, 42)                                      5.618e-02  2.505e-02
    ## lag(value, 49)                                      6.585e-02  2.492e-02
    ## month(date, label = TRUE).L                        -9.760e+00  5.106e+00
    ## month(date, label = TRUE).Q                         3.691e+00  5.019e+00
    ## month(date, label = TRUE).C                        -1.315e+01  5.118e+00
    ## month(date, label = TRUE)^4                        -7.168e+00  5.078e+00
    ## month(date, label = TRUE)^5                        -1.149e+01  5.092e+00
    ## month(date, label = TRUE)^6                        -3.848e+00  5.133e+00
    ## month(date, label = TRUE)^7                        -6.900e+00  5.040e+00
    ## month(date, label = TRUE)^8                        -3.305e+00  5.024e+00
    ## month(date, label = TRUE)^9                         4.950e+00  5.005e+00
    ## month(date, label = TRUE)^10                        2.869e+00  5.030e+00
    ## month(date, label = TRUE)^11                       -3.172e+00  4.917e+00
    ## fourier_vec(date, type = "sin", K = 1, period = 7) -1.163e+01  2.292e+00
    ## fourier_vec(date, type = "cos", K = 1, period = 7)  8.208e+00  2.407e+00
    ##                                                    t value Pr(>|t|)    
    ## (Intercept)                                         -2.636 0.008465 ** 
    ## date                                                 3.087 0.002055 ** 
    ## lag(value, 1)                                        4.170 3.20e-05 ***
    ## lag(value, 7)                                        3.843 0.000126 ***
    ## lag(value, 14)                                       3.464 0.000545 ***
    ## lag(value, 21)                                       2.628 0.008682 ** 
    ## lag(value, 28)                                       2.711 0.006785 ** 
    ## lag(value, 35)                                       2.718 0.006636 ** 
    ## lag(value, 42)                                       2.243 0.025028 *  
    ## lag(value, 49)                                       2.643 0.008306 ** 
    ## month(date, label = TRUE).L                         -1.912 0.056108 .  
    ## month(date, label = TRUE).Q                          0.735 0.462271    
    ## month(date, label = TRUE).C                         -2.569 0.010292 *  
    ## month(date, label = TRUE)^4                         -1.412 0.158258    
    ## month(date, label = TRUE)^5                         -2.257 0.024145 *  
    ## month(date, label = TRUE)^6                         -0.750 0.453533    
    ## month(date, label = TRUE)^7                         -1.369 0.171180    
    ## month(date, label = TRUE)^8                         -0.658 0.510788    
    ## month(date, label = TRUE)^9                          0.989 0.322813    
    ## month(date, label = TRUE)^10                         0.570 0.568486    
    ## month(date, label = TRUE)^11                        -0.645 0.518882    
    ## fourier_vec(date, type = "sin", K = 1, period = 7)  -5.073 4.37e-07 ***
    ## fourier_vec(date, type = "cos", K = 1, period = 7)   3.410 0.000666 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 58.7 on 1636 degrees of freedom
    ##   (49 observations deleted due to missingness)
    ## Multiple R-squared:  0.2343, Adjusted R-squared:  0.224 
    ## F-statistic: 22.75 on 22 and 1636 DF,  p-value: < 2.2e-16

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
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 25 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 14.0482570357609"
    ## [1] "BEST method = 'lin' PATH MEMBER = c( 25 )"
    ## [1] "BEST lin OBJECTIVE FUNCTION = 14.0482570357609"
    ## [1] "CURRNET METHOD: nonlin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'nonlin' , seasonal.factor =  c( 25 ) ...)"
    ## [1] "CURRENT nonlin OBJECTIVE FUNCTION = 10.9445523890634"
    ## [1] "BEST method = 'nonlin' PATH MEMBER = c( 25 )"
    ## [1] "BEST nonlin OBJECTIVE FUNCTION = 10.9445523890634"
    ## [1] "CURRNET METHOD: both"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'both' , seasonal.factor =  c( 25 ) ...)"
    ## [1] "CURRENT both OBJECTIVE FUNCTION = 12.9043704242578"
    ## [1] "BEST method = 'both' PATH MEMBER = c( 25 )"
    ## [1] "BEST both OBJECTIVE FUNCTION = 12.9043704242578"

![](man/figures/README-nns_forecasting-1.png)<!-- -->

    ## Package: healthyR.ai
    ## [1] "CURRNET METHOD: lin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 20 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 18.6768668693206"
    ## [1] "BEST method = 'lin' PATH MEMBER = c( 20 )"
    ## [1] "BEST lin OBJECTIVE FUNCTION = 18.6768668693206"
    ## [1] "CURRNET METHOD: nonlin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'nonlin' , seasonal.factor =  c( 20 ) ...)"
    ## [1] "CURRENT nonlin OBJECTIVE FUNCTION = 8.15242333786902"
    ## [1] "BEST method = 'nonlin' PATH MEMBER = c( 20 )"
    ## [1] "BEST nonlin OBJECTIVE FUNCTION = 8.15242333786902"
    ## [1] "CURRNET METHOD: both"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'both' , seasonal.factor =  c( 20 ) ...)"
    ## [1] "CURRENT both OBJECTIVE FUNCTION = 13.6844510152765"
    ## [1] "BEST method = 'both' PATH MEMBER = c( 20 )"
    ## [1] "BEST both OBJECTIVE FUNCTION = 13.6844510152765"

![](man/figures/README-nns_forecasting-2.png)<!-- -->

    ## Package: healthyR.data
    ## [1] "CURRNET METHOD: lin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 13 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 10.2126275178162"
    ## [1] "BEST method = 'lin' PATH MEMBER = c( 13 )"
    ## [1] "BEST lin OBJECTIVE FUNCTION = 10.2126275178162"
    ## [1] "CURRNET METHOD: nonlin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'nonlin' , seasonal.factor =  c( 13 ) ...)"
    ## [1] "CURRENT nonlin OBJECTIVE FUNCTION = 9.73360215386772"
    ## [1] "BEST method = 'nonlin' PATH MEMBER = c( 13 )"
    ## [1] "BEST nonlin OBJECTIVE FUNCTION = 9.73360215386772"
    ## [1] "CURRNET METHOD: both"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'both' , seasonal.factor =  c( 13 ) ...)"
    ## [1] "CURRENT both OBJECTIVE FUNCTION = 11.8925822777623"
    ## [1] "BEST method = 'both' PATH MEMBER = c( 13 )"
    ## [1] "BEST both OBJECTIVE FUNCTION = 11.8925822777623"

![](man/figures/README-nns_forecasting-3.png)<!-- -->

    ## Package: healthyR.ts
    ## [1] "CURRNET METHOD: lin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 13 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 20.6472739930543"
    ## [1] "BEST method = 'lin' PATH MEMBER = c( 13 )"
    ## [1] "BEST lin OBJECTIVE FUNCTION = 20.6472739930543"
    ## [1] "CURRNET METHOD: nonlin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'nonlin' , seasonal.factor =  c( 13 ) ...)"
    ## [1] "CURRENT nonlin OBJECTIVE FUNCTION = 26.1831263232901"
    ## [1] "BEST method = 'nonlin' PATH MEMBER = c( 13 )"
    ## [1] "BEST nonlin OBJECTIVE FUNCTION = 26.1831263232901"
    ## [1] "CURRNET METHOD: both"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'both' , seasonal.factor =  c( 13 ) ...)"
    ## [1] "CURRENT both OBJECTIVE FUNCTION = 32.8339666102458"
    ## [1] "BEST method = 'both' PATH MEMBER = c( 13 )"
    ## [1] "BEST both OBJECTIVE FUNCTION = 32.8339666102458"

![](man/figures/README-nns_forecasting-4.png)<!-- -->

    ## Package: healthyverse
    ## [1] "CURRNET METHOD: lin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 19 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 21.2292530426984"
    ## [1] "BEST method = 'lin' PATH MEMBER = c( 19 )"
    ## [1] "BEST lin OBJECTIVE FUNCTION = 21.2292530426984"
    ## [1] "CURRNET METHOD: nonlin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'nonlin' , seasonal.factor =  c( 19 ) ...)"
    ## [1] "CURRENT nonlin OBJECTIVE FUNCTION = 13.0025345312222"
    ## [1] "BEST method = 'nonlin' PATH MEMBER = c( 19 )"
    ## [1] "BEST nonlin OBJECTIVE FUNCTION = 13.0025345312222"
    ## [1] "CURRNET METHOD: both"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'both' , seasonal.factor =  c( 19 ) ...)"
    ## [1] "CURRENT both OBJECTIVE FUNCTION = 23.7054817368574"
    ## [1] "BEST method = 'both' PATH MEMBER = c( 19 )"
    ## [1] "BEST both OBJECTIVE FUNCTION = 23.7054817368574"

![](man/figures/README-nns_forecasting-5.png)<!-- -->

    ## Package: RandomWalker
    ## [1] "CURRNET METHOD: lin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 9 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 4.90253912788017"
    ## [1] "BEST method = 'lin' PATH MEMBER = c( 9 )"
    ## [1] "BEST lin OBJECTIVE FUNCTION = 4.90253912788017"
    ## [1] "CURRNET METHOD: nonlin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'nonlin' , seasonal.factor =  c( 9 ) ...)"
    ## [1] "CURRENT nonlin OBJECTIVE FUNCTION = 7.51584200148108"
    ## [1] "BEST method = 'nonlin' PATH MEMBER = c( 9 )"
    ## [1] "BEST nonlin OBJECTIVE FUNCTION = 7.51584200148108"
    ## [1] "CURRNET METHOD: both"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'both' , seasonal.factor =  c( 9 ) ...)"
    ## [1] "CURRENT both OBJECTIVE FUNCTION = 6.322335384845"
    ## [1] "BEST method = 'both' PATH MEMBER = c( 9 )"
    ## [1] "BEST both OBJECTIVE FUNCTION = 6.322335384845"

![](man/figures/README-nns_forecasting-6.png)<!-- -->

    ## Package: tidyAML
    ## [1] "CURRNET METHOD: lin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 23 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 13.4915698317947"
    ## [1] "BEST method = 'lin' PATH MEMBER = c( 23 )"
    ## [1] "BEST lin OBJECTIVE FUNCTION = 13.4915698317947"
    ## [1] "CURRNET METHOD: nonlin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'nonlin' , seasonal.factor =  c( 23 ) ...)"
    ## [1] "CURRENT nonlin OBJECTIVE FUNCTION = 29.5491288441051"
    ## [1] "BEST method = 'nonlin' PATH MEMBER = c( 23 )"
    ## [1] "BEST nonlin OBJECTIVE FUNCTION = 29.5491288441051"
    ## [1] "CURRNET METHOD: both"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'both' , seasonal.factor =  c( 23 ) ...)"
    ## [1] "CURRENT both OBJECTIVE FUNCTION = 20.7339612317065"
    ## [1] "BEST method = 'both' PATH MEMBER = c( 23 )"
    ## [1] "BEST both OBJECTIVE FUNCTION = 20.7339612317065"

![](man/figures/README-nns_forecasting-7.png)<!-- -->

    ## Package: TidyDensity
    ## [1] "CURRNET METHOD: lin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 19 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 25.5525236137378"
    ## [1] "BEST method = 'lin' PATH MEMBER = c( 19 )"
    ## [1] "BEST lin OBJECTIVE FUNCTION = 25.5525236137378"
    ## [1] "CURRNET METHOD: nonlin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'nonlin' , seasonal.factor =  c( 19 ) ...)"
    ## [1] "CURRENT nonlin OBJECTIVE FUNCTION = 29.0896472025037"
    ## [1] "BEST method = 'nonlin' PATH MEMBER = c( 19 )"
    ## [1] "BEST nonlin OBJECTIVE FUNCTION = 29.0896472025037"
    ## [1] "CURRNET METHOD: both"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'both' , seasonal.factor =  c( 19 ) ...)"
    ## [1] "CURRENT both OBJECTIVE FUNCTION = 30.6939215353318"
    ## [1] "BEST method = 'both' PATH MEMBER = c( 19 )"
    ## [1] "BEST both OBJECTIVE FUNCTION = 30.6939215353318"

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
    ## 1 healthyR.data <tibble [1,700 × 2]> <tibble [28 × 2]> <split [1672|28]>
    ## 2 healthyR      <tibble [1,692 × 2]> <tibble [28 × 2]> <split [1664|28]>
    ## 3 healthyR.ts   <tibble [1,637 × 2]> <tibble [28 × 2]> <split [1609|28]>
    ## 4 healthyverse  <tibble [1,608 × 2]> <tibble [28 × 2]> <split [1580|28]>
    ## 5 healthyR.ai   <tibble [1,433 × 2]> <tibble [28 × 2]> <split [1405|28]>
    ## 6 TidyDensity   <tibble [1,284 × 2]> <tibble [28 × 2]> <split [1256|28]>
    ## 7 tidyAML       <tibble [892 × 2]>   <tibble [28 × 2]> <split [864|28]> 
    ## 8 RandomWalker  <tibble [314 × 2]>   <tibble [28 × 2]> <split [286|28]>

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
| healthyR.data | 1 | ARIMA | Test | 0.6396116 | 105.21626 | 0.6252729 | 130.89423 | 0.7907318 | 0.0710344 |
| healthyR.data | 2 | LM | Test | 0.6782521 | 123.10691 | 0.6630471 | 127.87525 | 0.8107509 | 0.0573071 |
| healthyR.data | 3 | NULL | NA | NA | NA | NA | NA | NA | NA |
| healthyR.data | 4 | NNAR | Test | 0.6885256 | 98.56086 | 0.6730903 | 184.13574 | 0.8696538 | 0.0465518 |
| healthyR | 1 | ARIMA | Test | 0.5056950 | 167.10652 | 0.6760855 | 154.76593 | 0.6794776 | 0.0371405 |
| healthyR | 2 | LM | Test | 0.4447992 | 95.51083 | 0.5946713 | 140.32094 | 0.6285179 | 0.0476456 |
| healthyR | 3 | NULL | NA | NA | NA | NA | NA | NA | NA |
| healthyR | 4 | NNAR | Test | 0.4727801 | 131.59049 | 0.6320802 | 151.14873 | 0.6462797 | 0.0230293 |
| healthyR.ts | 1 | ARIMA | Test | 0.6793625 | 111.85230 | 0.7534658 | 147.60164 | 0.7868926 | 0.0225964 |
| healthyR.ts | 2 | LM | Test | 0.6715703 | 101.35986 | 0.7448236 | 122.94468 | 0.8471940 | 0.0127829 |
| healthyR.ts | 3 | NULL | NA | NA | NA | NA | NA | NA | NA |
| healthyR.ts | 4 | NNAR | Test | 0.6490658 | 97.96430 | 0.7198645 | 179.67701 | 0.7348223 | 0.0218486 |
| healthyverse | 1 | ARIMA | Test | 0.3635319 | 184.40908 | 0.8334098 | 65.73382 | 0.4747792 | 0.0155832 |
| healthyverse | 2 | LM | Test | 0.3673758 | 206.64600 | 0.8422222 | 61.35514 | 0.4790184 | 0.1176342 |
| healthyverse | 3 | NULL | NA | NA | NA | NA | NA | NA | NA |
| healthyverse | 4 | NNAR | Test | 0.4419290 | 171.73696 | 1.0131380 | 83.00259 | 0.5435778 | 0.0603724 |
| healthyR.ai | 1 | ARIMA | Test | 0.5468367 | 115.12959 | 0.7765104 | 126.73651 | 0.7358020 | 0.0070043 |
| healthyR.ai | 2 | LM | Test | 0.5509692 | 108.28148 | 0.7823785 | 128.92996 | 0.7451030 | 0.1410707 |
| healthyR.ai | 3 | NULL | NA | NA | NA | NA | NA | NA | NA |
| healthyR.ai | 4 | NNAR | Test | 0.5591203 | 105.74002 | 0.7939532 | 135.99303 | 0.7505900 | 0.0177358 |
| TidyDensity | 1 | ARIMA | Test | 0.8157080 | 108.08953 | 0.7363619 | 111.66906 | 1.0895530 | 0.0021515 |
| TidyDensity | 2 | LM | Test | 0.7493308 | 138.45223 | 0.6764414 | 90.14714 | 1.0314324 | 0.0171748 |
| TidyDensity | 3 | NULL | NA | NA | NA | NA | NA | NA | NA |
| TidyDensity | 4 | NNAR | Test | 0.8959780 | 103.16650 | 0.8088238 | 137.47559 | 1.1574833 | 0.0161949 |
| tidyAML | 1 | ARIMA | Test | 0.5213708 | 186.00751 | 1.0116680 | 88.38647 | 0.6233779 | 0.0162287 |
| tidyAML | 2 | LM | Test | 0.4911294 | 177.46267 | 0.9529877 | 83.98081 | 0.5980075 | 0.2075618 |
| tidyAML | 3 | NULL | NA | NA | NA | NA | NA | NA | NA |
| tidyAML | 4 | NNAR | Test | 0.5092094 | 187.55337 | 0.9880701 | 85.02494 | 0.6262797 | 0.0723744 |
| RandomWalker | 1 | ARIMA | Test | 1.2979005 | 153.93040 | 0.5995061 | 170.07038 | 1.4474004 | 0.0082741 |
| RandomWalker | 2 | LM | Test | 1.2813505 | 98.08641 | 0.5918616 | 181.59980 | 1.4225880 | 0.0508343 |
| RandomWalker | 3 | NULL | NA | NA | NA | NA | NA | NA | NA |
| RandomWalker | 4 | NNAR | Test | 1.3582830 | 112.07906 | 0.6273970 | 166.96663 | 1.5123965 | 0.0474228 |

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
    ## 1 healthyR.da…         1 ARIMA       Test  0.640 105.  0.625 131.  0.791 0.0710 
    ## 2 healthyR             2 LM          Test  0.445  95.5 0.595 140.  0.629 0.0476 
    ## 3 healthyR.ts          4 NNAR        Test  0.649  98.0 0.720 180.  0.735 0.0218 
    ## 4 healthyverse         1 ARIMA       Test  0.364 184.  0.833  65.7 0.475 0.0156 
    ## 5 healthyR.ai          1 ARIMA       Test  0.547 115.  0.777 127.  0.736 0.00700
    ## 6 TidyDensity          2 LM          Test  0.749 138.  0.676  90.1 1.03  0.0172 
    ## 7 tidyAML              2 LM          Test  0.491 177.  0.953  84.0 0.598 0.208  
    ## 8 RandomWalker         2 LM          Test  1.28   98.1 0.592 182.  1.42  0.0508

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
    ## 1 healthyR.data <tibble>     <tibble>     <split [1672|28]> <mdl_tm_t [1 × 5]>
    ## 2 healthyR      <tibble>     <tibble>     <split [1664|28]> <mdl_tm_t [1 × 5]>
    ## 3 healthyR.ts   <tibble>     <tibble>     <split [1609|28]> <mdl_tm_t [1 × 5]>
    ## 4 healthyverse  <tibble>     <tibble>     <split [1580|28]> <mdl_tm_t [1 × 5]>
    ## 5 healthyR.ai   <tibble>     <tibble>     <split [1405|28]> <mdl_tm_t [1 × 5]>
    ## 6 TidyDensity   <tibble>     <tibble>     <split [1256|28]> <mdl_tm_t [1 × 5]>
    ## 7 tidyAML       <tibble>     <tibble>     <split [864|28]>  <mdl_tm_t [1 × 5]>
    ## 8 RandomWalker  <tibble>     <tibble>     <split [286|28]>  <mdl_tm_t [1 × 5]>

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
