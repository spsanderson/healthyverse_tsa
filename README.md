Time Series Analysis, Modeling and Forecasting of the Healthyverse
Packages
================
Steven P. Sanderson II, MPH - Date:
25 July, 2025

This analysis follows a *Nested Modeltime Workflow*.

## Get Data

``` r
glimpse(downloads_tbl)
```

    ## Rows: 146,429
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

The last day in the data set is 2025-07-23 23:56:58, the file was
birthed on: 2024-08-07 07:35:44.428716, and at report knit time is
-8412.35 hours old. Happy analyzing!

Now that we have our data lets take a look at it using the `skimr`
package.

``` r
skim(downloads_tbl)
```

|                                                  |               |
|:-------------------------------------------------|:--------------|
| Name                                             | downloads_tbl |
| Number of rows                                   | 146429        |
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
| r_version     |    105874 |          0.28 |   5 |   5 |     0 |       48 |          0 |
| r_arch        |    105874 |          0.28 |   3 |   7 |     0 |        5 |          0 |
| r_os          |    105874 |          0.28 |   7 |  15 |     0 |       23 |          0 |
| package       |         0 |          1.00 |   7 |  13 |     0 |        8 |          0 |
| version       |         0 |          1.00 |   5 |  17 |     0 |       61 |          0 |
| country       |     12347 |          0.92 |   2 |   2 |     0 |      163 |          0 |

**Variable type: Date**

| skim_variable | n_missing | complete_rate | min | max | median | n_unique |
|:---|---:|---:|:---|:---|:---|---:|
| date | 0 | 1 | 2020-11-23 | 2025-07-23 | 2023-07-27 | 1704 |

**Variable type: numeric**

| skim_variable | n_missing | complete_rate | mean | sd | p0 | p25 | p50 | p75 | p100 | hist |
|:---|---:|---:|---:|---:|---:|---:|---:|---:|---:|:---|
| size | 0 | 1 | 1131856.4 | 1511641.35 | 355 | 14701 | 301941 | 2367665 | 5677952 | ▇▁▂▁▁ |
| ip_id | 0 | 1 | 10478.2 | 18608.56 | 1 | 286 | 3033 | 11844 | 209747 | ▇▁▁▁▁ |

**Variable type: POSIXct**

| skim_variable | n_missing | complete_rate | min | max | median | n_unique |
|:---|---:|---:|:---|:---|:---|---:|
| date_time | 0 | 1 | 2020-11-23 09:00:41 | 2025-07-23 23:56:58 | 2023-07-27 05:14:41 | 90146 |

**Variable type: Timespan**

| skim_variable | n_missing | complete_rate | min | max |    median | n_unique |
|:--------------|----------:|--------------:|----:|----:|----------:|---------:|
| time          |         0 |             1 |   0 |  59 | 12H 6M 8S |       60 |

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
    ## -148.35  -36.05  -11.25   26.77  817.00 
    ## 
    ## Coefficients:
    ##                                                      Estimate Std. Error
    ## (Intercept)                                        -171.17666   64.80714
    ## date                                                  0.01060    0.00343
    ## lag(value, 1)                                         0.10124    0.02403
    ## lag(value, 7)                                         0.09478    0.02485
    ## lag(value, 14)                                        0.08739    0.02488
    ## lag(value, 21)                                        0.06466    0.02492
    ## lag(value, 28)                                        0.06647    0.02478
    ## lag(value, 35)                                        0.06602    0.02483
    ## lag(value, 42)                                        0.05635    0.02507
    ## lag(value, 49)                                        0.06630    0.02493
    ## month(date, label = TRUE).L                          -9.73915    5.10680
    ## month(date, label = TRUE).Q                           3.65238    5.02648
    ## month(date, label = TRUE).C                         -13.12677    5.12005
    ## month(date, label = TRUE)^4                          -7.11614    5.08442
    ## month(date, label = TRUE)^5                         -11.44531    5.09517
    ## month(date, label = TRUE)^6                          -3.87663    5.13891
    ## month(date, label = TRUE)^7                          -6.91256    5.04485
    ## month(date, label = TRUE)^8                          -3.27711    5.02969
    ## month(date, label = TRUE)^9                           4.98823    5.01284
    ## month(date, label = TRUE)^10                          2.83625    5.03390
    ## month(date, label = TRUE)^11                         -3.22381    4.93603
    ## fourier_vec(date, type = "sin", K = 1, period = 7)  -11.69693    2.29461
    ## fourier_vec(date, type = "cos", K = 1, period = 7)    8.19709    2.41311
    ##                                                    t value Pr(>|t|)    
    ## (Intercept)                                         -2.641 0.008337 ** 
    ## date                                                 3.091 0.002031 ** 
    ## lag(value, 1)                                        4.213 2.66e-05 ***
    ## lag(value, 7)                                        3.814 0.000142 ***
    ## lag(value, 14)                                       3.513 0.000455 ***
    ## lag(value, 21)                                       2.595 0.009554 ** 
    ## lag(value, 28)                                       2.683 0.007381 ** 
    ## lag(value, 35)                                       2.658 0.007928 ** 
    ## lag(value, 42)                                       2.248 0.024713 *  
    ## lag(value, 49)                                       2.659 0.007916 ** 
    ## month(date, label = TRUE).L                         -1.907 0.056684 .  
    ## month(date, label = TRUE).Q                          0.727 0.467559    
    ## month(date, label = TRUE).C                         -2.564 0.010442 *  
    ## month(date, label = TRUE)^4                         -1.400 0.161824    
    ## month(date, label = TRUE)^5                         -2.246 0.024818 *  
    ## month(date, label = TRUE)^6                         -0.754 0.450737    
    ## month(date, label = TRUE)^7                         -1.370 0.170806    
    ## month(date, label = TRUE)^8                         -0.652 0.514781    
    ## month(date, label = TRUE)^9                          0.995 0.319840    
    ## month(date, label = TRUE)^10                         0.563 0.573220    
    ## month(date, label = TRUE)^11                        -0.653 0.513772    
    ## fourier_vec(date, type = "sin", K = 1, period = 7)  -5.098 3.84e-07 ***
    ## fourier_vec(date, type = "cos", K = 1, period = 7)   3.397 0.000698 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 58.71 on 1632 degrees of freedom
    ##   (49 observations deleted due to missingness)
    ## Multiple R-squared:  0.2346, Adjusted R-squared:  0.2243 
    ## F-statistic: 22.74 on 22 and 1632 DF,  p-value: < 2.2e-16

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
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 22 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 17.3557274722453"
    ## [1] "BEST method = 'lin' PATH MEMBER = c( 22 )"
    ## [1] "BEST lin OBJECTIVE FUNCTION = 17.3557274722453"
    ## [1] "CURRNET METHOD: nonlin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'nonlin' , seasonal.factor =  c( 22 ) ...)"
    ## [1] "CURRENT nonlin OBJECTIVE FUNCTION = 7.17080670333687"
    ## [1] "BEST method = 'nonlin' PATH MEMBER = c( 22 )"
    ## [1] "BEST nonlin OBJECTIVE FUNCTION = 7.17080670333687"
    ## [1] "CURRNET METHOD: both"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'both' , seasonal.factor =  c( 22 ) ...)"
    ## [1] "CURRENT both OBJECTIVE FUNCTION = 10.182404179398"
    ## [1] "BEST method = 'both' PATH MEMBER = c( 22 )"
    ## [1] "BEST both OBJECTIVE FUNCTION = 10.182404179398"

![](man/figures/README-nns_forecasting-1.png)<!-- -->

    ## Package: healthyR.ai
    ## [1] "CURRNET METHOD: lin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 22 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 16.5984632467411"
    ## [1] "BEST method = 'lin' PATH MEMBER = c( 22 )"
    ## [1] "BEST lin OBJECTIVE FUNCTION = 16.5984632467411"
    ## [1] "CURRNET METHOD: nonlin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'nonlin' , seasonal.factor =  c( 22 ) ...)"
    ## [1] "CURRENT nonlin OBJECTIVE FUNCTION = 22.6426651927209"
    ## [1] "BEST method = 'nonlin' PATH MEMBER = c( 22 )"
    ## [1] "BEST nonlin OBJECTIVE FUNCTION = 22.6426651927209"
    ## [1] "CURRNET METHOD: both"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'both' , seasonal.factor =  c( 22 ) ...)"
    ## [1] "CURRENT both OBJECTIVE FUNCTION = 21.854199663236"
    ## [1] "BEST method = 'both' PATH MEMBER = c( 22 )"
    ## [1] "BEST both OBJECTIVE FUNCTION = 21.854199663236"

![](man/figures/README-nns_forecasting-2.png)<!-- -->

    ## Package: healthyR.data
    ## [1] "CURRNET METHOD: lin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 23 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 9.98131012397514"
    ## [1] "BEST method = 'lin' PATH MEMBER = c( 23 )"
    ## [1] "BEST lin OBJECTIVE FUNCTION = 9.98131012397514"
    ## [1] "CURRNET METHOD: nonlin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'nonlin' , seasonal.factor =  c( 23 ) ...)"
    ## [1] "CURRENT nonlin OBJECTIVE FUNCTION = 21.4275252184002"
    ## [1] "BEST method = 'nonlin' PATH MEMBER = c( 23 )"
    ## [1] "BEST nonlin OBJECTIVE FUNCTION = 21.4275252184002"
    ## [1] "CURRNET METHOD: both"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'both' , seasonal.factor =  c( 23 ) ...)"
    ## [1] "CURRENT both OBJECTIVE FUNCTION = 18.1868645035776"
    ## [1] "BEST method = 'both' PATH MEMBER = c( 23 )"
    ## [1] "BEST both OBJECTIVE FUNCTION = 18.1868645035776"

![](man/figures/README-nns_forecasting-3.png)<!-- -->

    ## Package: healthyR.ts
    ## [1] "CURRNET METHOD: lin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 22 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 9.35675540788291"
    ## [1] "BEST method = 'lin' PATH MEMBER = c( 22 )"
    ## [1] "BEST lin OBJECTIVE FUNCTION = 9.35675540788291"
    ## [1] "CURRNET METHOD: nonlin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'nonlin' , seasonal.factor =  c( 22 ) ...)"
    ## [1] "CURRENT nonlin OBJECTIVE FUNCTION = 17.0386502844159"
    ## [1] "BEST method = 'nonlin' PATH MEMBER = c( 22 )"
    ## [1] "BEST nonlin OBJECTIVE FUNCTION = 17.0386502844159"
    ## [1] "CURRNET METHOD: both"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'both' , seasonal.factor =  c( 22 ) ...)"
    ## [1] "CURRENT both OBJECTIVE FUNCTION = 13.7317671668334"
    ## [1] "BEST method = 'both' PATH MEMBER = c( 22 )"
    ## [1] "BEST both OBJECTIVE FUNCTION = 13.7317671668334"

![](man/figures/README-nns_forecasting-4.png)<!-- -->

    ## Package: healthyverse
    ## [1] "CURRNET METHOD: lin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 22 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 6.46437798132487"
    ## [1] "BEST method = 'lin' PATH MEMBER = c( 22 )"
    ## [1] "BEST lin OBJECTIVE FUNCTION = 6.46437798132487"
    ## [1] "CURRNET METHOD: nonlin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'nonlin' , seasonal.factor =  c( 22 ) ...)"
    ## [1] "CURRENT nonlin OBJECTIVE FUNCTION = 10.0598700746582"
    ## [1] "BEST method = 'nonlin' PATH MEMBER = c( 22 )"
    ## [1] "BEST nonlin OBJECTIVE FUNCTION = 10.0598700746582"
    ## [1] "CURRNET METHOD: both"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'both' , seasonal.factor =  c( 22 ) ...)"
    ## [1] "CURRENT both OBJECTIVE FUNCTION = 11.8594020461328"
    ## [1] "BEST method = 'both' PATH MEMBER = c( 22 )"
    ## [1] "BEST both OBJECTIVE FUNCTION = 11.8594020461328"

![](man/figures/README-nns_forecasting-5.png)<!-- -->

    ## Package: RandomWalker
    ## [1] "CURRNET METHOD: lin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 25 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 6.77614681364979"
    ## [1] "BEST method = 'lin' PATH MEMBER = c( 25 )"
    ## [1] "BEST lin OBJECTIVE FUNCTION = 6.77614681364979"
    ## [1] "CURRNET METHOD: nonlin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'nonlin' , seasonal.factor =  c( 25 ) ...)"
    ## [1] "CURRENT nonlin OBJECTIVE FUNCTION = 8.1142858158311"
    ## [1] "BEST method = 'nonlin' PATH MEMBER = c( 25 )"
    ## [1] "BEST nonlin OBJECTIVE FUNCTION = 8.1142858158311"
    ## [1] "CURRNET METHOD: both"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'both' , seasonal.factor =  c( 25 ) ...)"
    ## [1] "CURRENT both OBJECTIVE FUNCTION = 7.03365048057496"
    ## [1] "BEST method = 'both' PATH MEMBER = c( 25 )"
    ## [1] "BEST both OBJECTIVE FUNCTION = 7.03365048057496"

![](man/figures/README-nns_forecasting-6.png)<!-- -->

    ## Package: tidyAML
    ## [1] "CURRNET METHOD: lin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 19 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 7.08277312548071"
    ## [1] "BEST method = 'lin' PATH MEMBER = c( 19 )"
    ## [1] "BEST lin OBJECTIVE FUNCTION = 7.08277312548071"
    ## [1] "CURRNET METHOD: nonlin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'nonlin' , seasonal.factor =  c( 19 ) ...)"
    ## [1] "CURRENT nonlin OBJECTIVE FUNCTION = 19.417393881012"
    ## [1] "BEST method = 'nonlin' PATH MEMBER = c( 19 )"
    ## [1] "BEST nonlin OBJECTIVE FUNCTION = 19.417393881012"
    ## [1] "CURRNET METHOD: both"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'both' , seasonal.factor =  c( 19 ) ...)"
    ## [1] "CURRENT both OBJECTIVE FUNCTION = 12.339017323781"
    ## [1] "BEST method = 'both' PATH MEMBER = c( 19 )"
    ## [1] "BEST both OBJECTIVE FUNCTION = 12.339017323781"

![](man/figures/README-nns_forecasting-7.png)<!-- -->

    ## Package: TidyDensity
    ## [1] "CURRNET METHOD: lin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 24 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 31.3423466789461"
    ## [1] "BEST method = 'lin' PATH MEMBER = c( 24 )"
    ## [1] "BEST lin OBJECTIVE FUNCTION = 31.3423466789461"
    ## [1] "CURRNET METHOD: nonlin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'nonlin' , seasonal.factor =  c( 24 ) ...)"
    ## [1] "CURRENT nonlin OBJECTIVE FUNCTION = 23.0369671092594"
    ## [1] "BEST method = 'nonlin' PATH MEMBER = c( 24 )"
    ## [1] "BEST nonlin OBJECTIVE FUNCTION = 23.0369671092594"
    ## [1] "CURRNET METHOD: both"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'both' , seasonal.factor =  c( 24 ) ...)"
    ## [1] "CURRENT both OBJECTIVE FUNCTION = 56.6333659668492"
    ## [1] "BEST method = 'both' PATH MEMBER = c( 24 )"
    ## [1] "BEST both OBJECTIVE FUNCTION = 56.6333659668492"

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
    ## 1 healthyR.data <tibble [1,696 × 2]> <tibble [28 × 2]> <split [1668|28]>
    ## 2 healthyR      <tibble [1,689 × 2]> <tibble [28 × 2]> <split [1661|28]>
    ## 3 healthyR.ts   <tibble [1,634 × 2]> <tibble [28 × 2]> <split [1606|28]>
    ## 4 healthyverse  <tibble [1,604 × 2]> <tibble [28 × 2]> <split [1576|28]>
    ## 5 healthyR.ai   <tibble [1,429 × 2]> <tibble [28 × 2]> <split [1401|28]>
    ## 6 TidyDensity   <tibble [1,280 × 2]> <tibble [28 × 2]> <split [1252|28]>
    ## 7 tidyAML       <tibble [888 × 2]>   <tibble [28 × 2]> <split [860|28]> 
    ## 8 RandomWalker  <tibble [310 × 2]>   <tibble [28 × 2]> <split [282|28]>

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
| healthyR.data | 1 | ARIMA | Test | 0.5606602 | 105.00111 | 0.6578246 | 134.93013 | 0.6734662 | 0.0197439 |
| healthyR.data | 2 | LM | Test | 0.5807593 | 129.46484 | 0.6814070 | 121.46462 | 0.6819291 | 0.0113610 |
| healthyR.data | 3 | NULL | NA | NA | NA | NA | NA | NA | NA |
| healthyR.data | 4 | NNAR | Test | 0.5905013 | 93.83196 | 0.6928372 | 175.68803 | 0.7284354 | 0.0268365 |
| healthyR | 1 | ARIMA | Test | 0.4694057 | 163.09321 | 0.6967309 | 146.30104 | 0.6506754 | 0.0098304 |
| healthyR | 2 | LM | Test | 0.4155232 | 100.08938 | 0.6167540 | 136.75227 | 0.6001159 | 0.0309000 |
| healthyR | 3 | NULL | NA | NA | NA | NA | NA | NA | NA |
| healthyR | 4 | NNAR | Test | 0.4038409 | 117.42449 | 0.5994142 | 147.76312 | 0.5939609 | 0.0288128 |
| healthyR.ts | 1 | ARIMA | Test | 0.6784978 | 116.43758 | 0.7491698 | 147.95073 | 0.8039008 | 0.0001418 |
| healthyR.ts | 2 | LM | Test | 0.6555391 | 121.55040 | 0.7238197 | 123.44852 | 0.8291918 | 0.0209879 |
| healthyR.ts | 3 | NULL | NA | NA | NA | NA | NA | NA | NA |
| healthyR.ts | 4 | NNAR | Test | 0.6222797 | 98.90751 | 0.6870960 | 179.64788 | 0.7130785 | 0.0018125 |
| healthyverse | 1 | ARIMA | Test | 0.4207865 | 169.57721 | 0.8901479 | 69.69340 | 0.5520400 | 0.0172644 |
| healthyverse | 2 | LM | Test | 0.4033817 | 189.59584 | 0.8533291 | 65.24361 | 0.5168830 | 0.0953327 |
| healthyverse | 3 | NULL | NA | NA | NA | NA | NA | NA | NA |
| healthyverse | 4 | NNAR | Test | 0.4636853 | 154.23757 | 0.9808977 | 79.29081 | 0.5940378 | 0.0251511 |
| healthyR.ai | 1 | ARIMA | Test | 0.4934345 | 87.03198 | 0.7771872 | 125.10395 | 0.6491867 | 0.1668245 |
| healthyR.ai | 2 | LM | Test | 0.5096667 | 95.90220 | 0.8027538 | 127.70648 | 0.6663631 | 0.0004711 |
| healthyR.ai | 3 | NULL | NA | NA | NA | NA | NA | NA | NA |
| healthyR.ai | 4 | NNAR | Test | 0.5019307 | 93.19812 | 0.7905692 | 131.56632 | 0.6562140 | 0.0974607 |
| TidyDensity | 1 | ARIMA | Test | 0.6790316 | 105.74124 | 0.6589927 | 105.48814 | 0.9619544 | 0.0094380 |
| TidyDensity | 2 | LM | Test | 0.6595112 | 143.23782 | 0.6400484 | 88.51867 | 0.9512298 | 0.0001175 |
| TidyDensity | 3 | NULL | NA | NA | NA | NA | NA | NA | NA |
| TidyDensity | 4 | NNAR | Test | 0.7502700 | 99.01058 | 0.7281288 | 131.59103 | 1.0086815 | 0.0008808 |
| tidyAML | 1 | ARIMA | Test | 0.4595037 | 189.20143 | 0.9527891 | 84.75716 | 0.5352854 | 0.0093080 |
| tidyAML | 2 | LM | Test | 0.4637821 | 194.70553 | 0.9616603 | 84.40490 | 0.5354999 | 0.0522109 |
| tidyAML | 3 | NULL | NA | NA | NA | NA | NA | NA | NA |
| tidyAML | 4 | NNAR | Test | 0.4610665 | 190.70401 | 0.9560295 | 83.79090 | 0.5386027 | 0.0000477 |
| RandomWalker | 1 | ARIMA | Test | 1.2317822 | 173.82516 | 0.6114962 | 158.32303 | 1.3720943 | 0.0251050 |
| RandomWalker | 2 | LM | Test | 1.2230188 | 97.96370 | 0.6071457 | 180.33258 | 1.3681564 | 0.0161943 |
| RandomWalker | 3 | NULL | NA | NA | NA | NA | NA | NA | NA |
| RandomWalker | 4 | NNAR | Test | 1.2304781 | 114.20105 | 0.6108488 | 161.38220 | 1.3890524 | 0.0062321 |

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
    ## 1 healthyR.da…         1 ARIMA       Test  0.561 105.  0.658 135.  0.673 1.97e-2
    ## 2 healthyR             4 NNAR        Test  0.404 117.  0.599 148.  0.594 2.88e-2
    ## 3 healthyR.ts          4 NNAR        Test  0.622  98.9 0.687 180.  0.713 1.81e-3
    ## 4 healthyverse         2 LM          Test  0.403 190.  0.853  65.2 0.517 9.53e-2
    ## 5 healthyR.ai          1 ARIMA       Test  0.493  87.0 0.777 125.  0.649 1.67e-1
    ## 6 TidyDensity          2 LM          Test  0.660 143.  0.640  88.5 0.951 1.17e-4
    ## 7 tidyAML              1 ARIMA       Test  0.460 189.  0.953  84.8 0.535 9.31e-3
    ## 8 RandomWalker         2 LM          Test  1.22   98.0 0.607 180.  1.37  1.62e-2

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
    ## 1 healthyR.data <tibble>     <tibble>     <split [1668|28]> <mdl_tm_t [1 × 5]>
    ## 2 healthyR      <tibble>     <tibble>     <split [1661|28]> <mdl_tm_t [1 × 5]>
    ## 3 healthyR.ts   <tibble>     <tibble>     <split [1606|28]> <mdl_tm_t [1 × 5]>
    ## 4 healthyverse  <tibble>     <tibble>     <split [1576|28]> <mdl_tm_t [1 × 5]>
    ## 5 healthyR.ai   <tibble>     <tibble>     <split [1401|28]> <mdl_tm_t [1 × 5]>
    ## 6 TidyDensity   <tibble>     <tibble>     <split [1252|28]> <mdl_tm_t [1 × 5]>
    ## 7 tidyAML       <tibble>     <tibble>     <split [860|28]>  <mdl_tm_t [1 × 5]>
    ## 8 RandomWalker  <tibble>     <tibble>     <split [282|28]>  <mdl_tm_t [1 × 5]>

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
