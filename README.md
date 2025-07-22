Time Series Analysis, Modeling and Forecasting of the Healthyverse
Packages
================
Steven P. Sanderson II, MPH - Date:
22 July, 2025

This analysis follows a *Nested Modeltime Workflow*.

## Get Data

``` r
glimpse(downloads_tbl)
```

    ## Rows: 146,168
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

The last day in the data set is 2025-07-20 21:52:48, the file was
birthed on: 2022-07-02 23:58:17.511888, and at report knit time is
-2.672991^{4} hours old. Happy analyzing!

Now that we have our data lets take a look at it using the `skimr`
package.

``` r
skim(downloads_tbl)
```

|                                                  |               |
|:-------------------------------------------------|:--------------|
| Name                                             | downloads_tbl |
| Number of rows                                   | 146168        |
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
| r_version     |    105678 |          0.28 |   5 |   5 |     0 |       48 |          0 |
| r_arch        |    105678 |          0.28 |   3 |   7 |     0 |        5 |          0 |
| r_os          |    105678 |          0.28 |   7 |  15 |     0 |       23 |          0 |
| package       |         0 |          1.00 |   7 |  13 |     0 |        8 |          0 |
| version       |         0 |          1.00 |   5 |  17 |     0 |       60 |          0 |
| country       |     12325 |          0.92 |   2 |   2 |     0 |      163 |          0 |

**Variable type: Date**

| skim_variable | n_missing | complete_rate | min | max | median | n_unique |
|:---|---:|---:|:---|:---|:---|---:|
| date | 0 | 1 | 2020-11-23 | 2025-07-20 | 2023-07-26 | 1701 |

**Variable type: numeric**

| skim_variable | n_missing | complete_rate | mean | sd | p0 | p25 | p50 | p75 | p100 | hist |
|:---|---:|---:|---:|---:|---:|---:|---:|---:|---:|:---|
| size | 0 | 1 | 1131917.41 | 1512097.60 | 355 | 14701 | 301749 | 2367669.00 | 5677952 | ▇▁▂▁▁ |
| ip_id | 0 | 1 | 10466.14 | 18593.82 | 1 | 288 | 3037 | 11828.25 | 209747 | ▇▁▁▁▁ |

**Variable type: POSIXct**

| skim_variable | n_missing | complete_rate | min | max | median | n_unique |
|:---|---:|---:|:---|:---|:---|---:|
| date_time | 0 | 1 | 2020-11-23 09:00:41 | 2025-07-20 21:52:48 | 2023-07-26 20:25:24 | 89947 |

**Variable type: Timespan**

| skim_variable | n_missing | complete_rate | min | max | median | n_unique |
|:--------------|----------:|--------------:|----:|----:|-------:|---------:|
| time          |         0 |             1 |   0 |  59 |     44 |       60 |

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
    ## -148.15  -36.01  -11.20   26.77  817.20 
    ## 
    ## Coefficients:
    ##                                                      Estimate Std. Error
    ## (Intercept)                                        -1.746e+02  6.495e+01
    ## date                                                1.078e-02  3.438e-03
    ## lag(value, 1)                                       1.020e-01  2.403e-02
    ## lag(value, 7)                                       9.343e-02  2.487e-02
    ## lag(value, 14)                                      8.622e-02  2.488e-02
    ## lag(value, 21)                                      6.358e-02  2.493e-02
    ## lag(value, 28)                                      6.707e-02  2.479e-02
    ## lag(value, 35)                                      6.890e-02  2.499e-02
    ## lag(value, 42)                                      5.758e-02  2.509e-02
    ## lag(value, 49)                                      6.499e-02  2.497e-02
    ## month(date, label = TRUE).L                        -9.743e+00  5.106e+00
    ## month(date, label = TRUE).Q                         3.441e+00  5.031e+00
    ## month(date, label = TRUE).C                        -1.324e+01  5.120e+00
    ## month(date, label = TRUE)^4                        -6.979e+00  5.088e+00
    ## month(date, label = TRUE)^5                        -1.136e+01  5.096e+00
    ## month(date, label = TRUE)^6                        -4.114e+00  5.142e+00
    ## month(date, label = TRUE)^7                        -7.077e+00  5.047e+00
    ## month(date, label = TRUE)^8                        -3.117e+00  5.033e+00
    ## month(date, label = TRUE)^9                         5.194e+00  5.017e+00
    ## month(date, label = TRUE)^10                        2.723e+00  5.036e+00
    ## month(date, label = TRUE)^11                       -3.632e+00  4.951e+00
    ## fourier_vec(date, type = "sin", K = 1, period = 7) -1.177e+01  2.297e+00
    ## fourier_vec(date, type = "cos", K = 1, period = 7)  8.229e+00  2.414e+00
    ##                                                    t value Pr(>|t|)    
    ## (Intercept)                                         -2.688 0.007269 ** 
    ## date                                                 3.135 0.001751 ** 
    ## lag(value, 1)                                        4.243 2.33e-05 ***
    ## lag(value, 7)                                        3.756 0.000178 ***
    ## lag(value, 14)                                       3.465 0.000543 ***
    ## lag(value, 21)                                       2.550 0.010851 *  
    ## lag(value, 28)                                       2.706 0.006883 ** 
    ## lag(value, 35)                                       2.757 0.005892 ** 
    ## lag(value, 42)                                       2.295 0.021882 *  
    ## lag(value, 49)                                       2.603 0.009338 ** 
    ## month(date, label = TRUE).L                         -1.908 0.056556 .  
    ## month(date, label = TRUE).Q                          0.684 0.494140    
    ## month(date, label = TRUE).C                         -2.585 0.009815 ** 
    ## month(date, label = TRUE)^4                         -1.372 0.170319    
    ## month(date, label = TRUE)^5                         -2.230 0.025904 *  
    ## month(date, label = TRUE)^6                         -0.800 0.423832    
    ## month(date, label = TRUE)^7                         -1.402 0.161050    
    ## month(date, label = TRUE)^8                         -0.619 0.535858    
    ## month(date, label = TRUE)^9                          1.035 0.300716    
    ## month(date, label = TRUE)^10                         0.541 0.588708    
    ## month(date, label = TRUE)^11                        -0.733 0.463365    
    ## fourier_vec(date, type = "sin", K = 1, period = 7)  -5.125 3.34e-07 ***
    ## fourier_vec(date, type = "cos", K = 1, period = 7)   3.409 0.000667 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 58.7 on 1629 degrees of freedom
    ##   (49 observations deleted due to missingness)
    ## Multiple R-squared:  0.2355, Adjusted R-squared:  0.2252 
    ## F-statistic: 22.81 on 22 and 1629 DF,  p-value: < 2.2e-16

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
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 21 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 3.57521762670413"
    ## [1] "BEST method = 'lin' PATH MEMBER = c( 21 )"
    ## [1] "BEST lin OBJECTIVE FUNCTION = 3.57521762670413"
    ## [1] "CURRNET METHOD: nonlin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'nonlin' , seasonal.factor =  c( 21 ) ...)"
    ## [1] "CURRENT nonlin OBJECTIVE FUNCTION = 3.51089407080813"
    ## [1] "BEST method = 'nonlin' PATH MEMBER = c( 21 )"
    ## [1] "BEST nonlin OBJECTIVE FUNCTION = 3.51089407080813"
    ## [1] "CURRNET METHOD: both"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'both' , seasonal.factor =  c( 21 ) ...)"
    ## [1] "CURRENT both OBJECTIVE FUNCTION = 3.40840139579107"
    ## [1] "BEST method = 'both' PATH MEMBER = c( 21 )"
    ## [1] "BEST both OBJECTIVE FUNCTION = 3.40840139579107"

![](man/figures/README-nns_forecasting-1.png)<!-- -->

    ## Package: healthyR.ai
    ## [1] "CURRNET METHOD: lin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 11 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 10.0199400661551"
    ## [1] "BEST method = 'lin' PATH MEMBER = c( 11 )"
    ## [1] "BEST lin OBJECTIVE FUNCTION = 10.0199400661551"
    ## [1] "CURRNET METHOD: nonlin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'nonlin' , seasonal.factor =  c( 11 ) ...)"
    ## [1] "CURRENT nonlin OBJECTIVE FUNCTION = 19.9921055846321"
    ## [1] "BEST method = 'nonlin' PATH MEMBER = c( 11 )"
    ## [1] "BEST nonlin OBJECTIVE FUNCTION = 19.9921055846321"
    ## [1] "CURRNET METHOD: both"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'both' , seasonal.factor =  c( 11 ) ...)"
    ## [1] "CURRENT both OBJECTIVE FUNCTION = 11.7560041786714"
    ## [1] "BEST method = 'both' PATH MEMBER = c( 11 )"
    ## [1] "BEST both OBJECTIVE FUNCTION = 11.7560041786714"

![](man/figures/README-nns_forecasting-2.png)<!-- -->

    ## Package: healthyR.data
    ## [1] "CURRNET METHOD: lin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 22 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 4.92861143357569"
    ## [1] "BEST method = 'lin' PATH MEMBER = c( 22 )"
    ## [1] "BEST lin OBJECTIVE FUNCTION = 4.92861143357569"
    ## [1] "CURRNET METHOD: nonlin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'nonlin' , seasonal.factor =  c( 22 ) ...)"
    ## [1] "CURRENT nonlin OBJECTIVE FUNCTION = 11.0059947954049"
    ## [1] "BEST method = 'nonlin' PATH MEMBER = c( 22 )"
    ## [1] "BEST nonlin OBJECTIVE FUNCTION = 11.0059947954049"
    ## [1] "CURRNET METHOD: both"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'both' , seasonal.factor =  c( 22 ) ...)"
    ## [1] "CURRENT both OBJECTIVE FUNCTION = 7.11786118654746"
    ## [1] "BEST method = 'both' PATH MEMBER = c( 22 )"
    ## [1] "BEST both OBJECTIVE FUNCTION = 7.11786118654746"

![](man/figures/README-nns_forecasting-3.png)<!-- -->

    ## Package: healthyR.ts
    ## [1] "CURRNET METHOD: lin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 21 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 3.70209810459235"
    ## [1] "BEST method = 'lin' PATH MEMBER = c( 21 )"
    ## [1] "BEST lin OBJECTIVE FUNCTION = 3.70209810459235"
    ## [1] "CURRNET METHOD: nonlin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'nonlin' , seasonal.factor =  c( 21 ) ...)"
    ## [1] "CURRENT nonlin OBJECTIVE FUNCTION = 4.52841093311945"
    ## [1] "BEST method = 'nonlin' PATH MEMBER = c( 21 )"
    ## [1] "BEST nonlin OBJECTIVE FUNCTION = 4.52841093311945"
    ## [1] "CURRNET METHOD: both"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'both' , seasonal.factor =  c( 21 ) ...)"
    ## [1] "CURRENT both OBJECTIVE FUNCTION = 4.04170112700423"
    ## [1] "BEST method = 'both' PATH MEMBER = c( 21 )"
    ## [1] "BEST both OBJECTIVE FUNCTION = 4.04170112700423"

![](man/figures/README-nns_forecasting-4.png)<!-- -->

    ## Package: healthyverse
    ## [1] "CURRNET METHOD: lin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 22 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 4.58428902485802"
    ## [1] "BEST method = 'lin' PATH MEMBER = c( 22 )"
    ## [1] "BEST lin OBJECTIVE FUNCTION = 4.58428902485802"
    ## [1] "CURRNET METHOD: nonlin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'nonlin' , seasonal.factor =  c( 22 ) ...)"
    ## [1] "CURRENT nonlin OBJECTIVE FUNCTION = 4.28488380179685"
    ## [1] "BEST method = 'nonlin' PATH MEMBER = c( 22 )"
    ## [1] "BEST nonlin OBJECTIVE FUNCTION = 4.28488380179685"
    ## [1] "CURRNET METHOD: both"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'both' , seasonal.factor =  c( 22 ) ...)"
    ## [1] "CURRENT both OBJECTIVE FUNCTION = 4.42033192893185"
    ## [1] "BEST method = 'both' PATH MEMBER = c( 22 )"
    ## [1] "BEST both OBJECTIVE FUNCTION = 4.42033192893185"

![](man/figures/README-nns_forecasting-5.png)<!-- -->

    ## Package: RandomWalker
    ## [1] "CURRNET METHOD: lin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 22 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 12.8811033845467"
    ## [1] "BEST method = 'lin' PATH MEMBER = c( 22 )"
    ## [1] "BEST lin OBJECTIVE FUNCTION = 12.8811033845467"
    ## [1] "CURRNET METHOD: nonlin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'nonlin' , seasonal.factor =  c( 22 ) ...)"
    ## [1] "CURRENT nonlin OBJECTIVE FUNCTION = 12.8605790237059"
    ## [1] "BEST method = 'nonlin' PATH MEMBER = c( 22 )"
    ## [1] "BEST nonlin OBJECTIVE FUNCTION = 12.8605790237059"
    ## [1] "CURRNET METHOD: both"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'both' , seasonal.factor =  c( 22 ) ...)"
    ## [1] "CURRENT both OBJECTIVE FUNCTION = 12.6452599232468"
    ## [1] "BEST method = 'both' PATH MEMBER = c( 22 )"
    ## [1] "BEST both OBJECTIVE FUNCTION = 12.6452599232468"

![](man/figures/README-nns_forecasting-6.png)<!-- -->

    ## Package: tidyAML
    ## [1] "CURRNET METHOD: lin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 13 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 23.2558839597903"
    ## [1] "BEST method = 'lin' PATH MEMBER = c( 13 )"
    ## [1] "BEST lin OBJECTIVE FUNCTION = 23.2558839597903"
    ## [1] "CURRNET METHOD: nonlin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'nonlin' , seasonal.factor =  c( 13 ) ...)"
    ## [1] "CURRENT nonlin OBJECTIVE FUNCTION = 58.1509368059419"
    ## [1] "BEST method = 'nonlin' PATH MEMBER = c( 13 )"
    ## [1] "BEST nonlin OBJECTIVE FUNCTION = 58.1509368059419"
    ## [1] "CURRNET METHOD: both"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'both' , seasonal.factor =  c( 13 ) ...)"
    ## [1] "CURRENT both OBJECTIVE FUNCTION = 37.5657605434268"
    ## [1] "BEST method = 'both' PATH MEMBER = c( 13 )"
    ## [1] "BEST both OBJECTIVE FUNCTION = 37.5657605434268"

![](man/figures/README-nns_forecasting-7.png)<!-- -->

    ## Package: TidyDensity
    ## [1] "CURRNET METHOD: lin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 22 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 23.9935505852127"
    ## [1] "BEST method = 'lin' PATH MEMBER = c( 22 )"
    ## [1] "BEST lin OBJECTIVE FUNCTION = 23.9935505852127"
    ## [1] "CURRNET METHOD: nonlin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'nonlin' , seasonal.factor =  c( 22 ) ...)"
    ## [1] "CURRENT nonlin OBJECTIVE FUNCTION = 16.3505883231737"
    ## [1] "BEST method = 'nonlin' PATH MEMBER = c( 22 )"
    ## [1] "BEST nonlin OBJECTIVE FUNCTION = 16.3505883231737"
    ## [1] "CURRNET METHOD: both"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'both' , seasonal.factor =  c( 22 ) ...)"
    ## [1] "CURRENT both OBJECTIVE FUNCTION = 23.1817631003812"
    ## [1] "BEST method = 'both' PATH MEMBER = c( 22 )"
    ## [1] "BEST both OBJECTIVE FUNCTION = 23.1817631003812"

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
    ## 1 healthyR.data <tibble [1,693 × 2]> <tibble [28 × 2]> <split [1665|28]>
    ## 2 healthyR      <tibble [1,687 × 2]> <tibble [28 × 2]> <split [1659|28]>
    ## 3 healthyR.ts   <tibble [1,631 × 2]> <tibble [28 × 2]> <split [1603|28]>
    ## 4 healthyverse  <tibble [1,601 × 2]> <tibble [28 × 2]> <split [1573|28]>
    ## 5 healthyR.ai   <tibble [1,426 × 2]> <tibble [28 × 2]> <split [1398|28]>
    ## 6 TidyDensity   <tibble [1,277 × 2]> <tibble [28 × 2]> <split [1249|28]>
    ## 7 tidyAML       <tibble [885 × 2]>   <tibble [28 × 2]> <split [857|28]> 
    ## 8 RandomWalker  <tibble [307 × 2]>   <tibble [28 × 2]> <split [279|28]>

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
| healthyR.data | 1 | ARIMA | Test | 0.5968022 | 116.88940 | 0.7303686 | 147.68602 | 0.6988438 | 0.0385120 |
| healthyR.data | 2 | LM | Test | 0.5772089 | 129.34001 | 0.7063903 | 122.16525 | 0.6641930 | 0.0821851 |
| healthyR.data | 3 | NULL | NA | NA | NA | NA | NA | NA | NA |
| healthyR.data | 4 | NNAR | Test | 0.6104813 | 99.10951 | 0.7471092 | 188.86032 | 0.7211810 | 0.0006160 |
| healthyR | 1 | ARIMA | Test | 0.4978181 | 139.70778 | 0.5714761 | 143.67793 | 0.7182100 | 0.0282490 |
| healthyR | 2 | LM | Test | 0.4803786 | 96.22573 | 0.5514563 | 135.52866 | 0.7284172 | 0.0004510 |
| healthyR | 3 | NULL | NA | NA | NA | NA | NA | NA | NA |
| healthyR | 4 | NNAR | Test | 0.5008108 | 126.83551 | 0.5749116 | 144.46249 | 0.8081827 | 0.0100003 |
| healthyR.ts | 1 | ARIMA | Test | 0.7159589 | 168.90648 | 0.8269212 | 133.10509 | 0.8650373 | 0.0167483 |
| healthyR.ts | 2 | LM | Test | 0.6430837 | 140.46964 | 0.7427515 | 121.92529 | 0.8223429 | 0.0251079 |
| healthyR.ts | 3 | NULL | NA | NA | NA | NA | NA | NA | NA |
| healthyR.ts | 4 | NNAR | Test | 0.6778749 | 129.31084 | 0.7829348 | 161.91231 | 0.8064312 | 0.0241465 |
| healthyverse | 1 | ARIMA | Test | 0.4354816 | 162.86645 | 0.7027758 | 75.88090 | 0.5934782 | 0.0034606 |
| healthyverse | 2 | LM | Test | 0.4510898 | 201.13789 | 0.7279641 | 72.43512 | 0.5952675 | 0.0000000 |
| healthyverse | 3 | NULL | NA | NA | NA | NA | NA | NA | NA |
| healthyverse | 4 | NNAR | Test | 0.4876834 | 130.04501 | 0.7870185 | 91.56936 | 0.6740161 | 0.0724492 |
| healthyR.ai | 1 | ARIMA | Test | 0.4667463 | 107.50839 | 0.8021993 | 124.17651 | 0.5905272 | 0.1016479 |
| healthyR.ai | 2 | LM | Test | 0.4821709 | 102.58562 | 0.8287097 | 130.15904 | 0.6145736 | 0.0067946 |
| healthyR.ai | 3 | NULL | NA | NA | NA | NA | NA | NA | NA |
| healthyR.ai | 4 | NNAR | Test | 0.4989012 | 130.92472 | 0.8574641 | 131.32500 | 0.6157632 | 0.0384050 |
| TidyDensity | 1 | ARIMA | Test | 0.5886903 | 93.44214 | 0.6506200 | 94.09037 | 0.8979840 | 0.0155890 |
| TidyDensity | 2 | LM | Test | 0.5796806 | 135.88654 | 0.6406625 | 79.56198 | 0.8892621 | 0.0113395 |
| TidyDensity | 3 | NULL | NA | NA | NA | NA | NA | NA | NA |
| TidyDensity | 4 | NNAR | Test | 0.7005847 | 99.00343 | 0.7742856 | 118.47578 | 0.9587002 | 0.0044959 |
| tidyAML | 1 | ARIMA | Test | 0.4475669 | 170.13621 | 0.8882254 | 82.42923 | 0.5171600 | 0.0443720 |
| tidyAML | 2 | LM | Test | 0.4484825 | 184.78254 | 0.8900423 | 80.72822 | 0.5288021 | 0.0027624 |
| tidyAML | 3 | NULL | NA | NA | NA | NA | NA | NA | NA |
| tidyAML | 4 | NNAR | Test | 0.4803439 | 163.49943 | 0.9532733 | 88.53526 | 0.5642274 | 0.0132285 |
| RandomWalker | 1 | ARIMA | Test | 1.2690250 | 116.93852 | 0.6301290 | 165.00117 | 1.4514228 | 0.0328649 |
| RandomWalker | 2 | LM | Test | 1.1945531 | 95.91326 | 0.5931503 | 181.26659 | 1.3568652 | 0.0208506 |
| RandomWalker | 3 | NULL | NA | NA | NA | NA | NA | NA | NA |
| RandomWalker | 4 | NNAR | Test | 1.2106991 | 113.74753 | 0.6011675 | 151.14758 | 1.3539010 | 0.0123915 |

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
    ## 1 healthyR.da…         2 LM          Test  0.577  129. 0.706 122.  0.664 0.0822 
    ## 2 healthyR             1 ARIMA       Test  0.498  140. 0.571 144.  0.718 0.0282 
    ## 3 healthyR.ts          4 NNAR        Test  0.678  129. 0.783 162.  0.806 0.0241 
    ## 4 healthyverse         1 ARIMA       Test  0.435  163. 0.703  75.9 0.593 0.00346
    ## 5 healthyR.ai          1 ARIMA       Test  0.467  108. 0.802 124.  0.591 0.102  
    ## 6 TidyDensity          2 LM          Test  0.580  136. 0.641  79.6 0.889 0.0113 
    ## 7 tidyAML              1 ARIMA       Test  0.448  170. 0.888  82.4 0.517 0.0444 
    ## 8 RandomWalker         4 NNAR        Test  1.21   114. 0.601 151.  1.35  0.0124

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
    ## 1 healthyR.data <tibble>     <tibble>     <split [1665|28]> <mdl_tm_t [1 × 5]>
    ## 2 healthyR      <tibble>     <tibble>     <split [1659|28]> <mdl_tm_t [1 × 5]>
    ## 3 healthyR.ts   <tibble>     <tibble>     <split [1603|28]> <mdl_tm_t [1 × 5]>
    ## 4 healthyverse  <tibble>     <tibble>     <split [1573|28]> <mdl_tm_t [1 × 5]>
    ## 5 healthyR.ai   <tibble>     <tibble>     <split [1398|28]> <mdl_tm_t [1 × 5]>
    ## 6 TidyDensity   <tibble>     <tibble>     <split [1249|28]> <mdl_tm_t [1 × 5]>
    ## 7 tidyAML       <tibble>     <tibble>     <split [857|28]>  <mdl_tm_t [1 × 5]>
    ## 8 RandomWalker  <tibble>     <tibble>     <split [279|28]>  <mdl_tm_t [1 × 5]>

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
