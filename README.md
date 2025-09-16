Time Series Analysis, Modeling and Forecasting of the Healthyverse
Packages
================
Steven P. Sanderson II, MPH - Date:
16 September, 2025

This analysis follows a *Nested Modeltime Workflow*.

## Get Data

``` r
glimpse(downloads_tbl)
```

    ## Rows: 152,092
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

The last day in the data set is 2025-09-14 23:57:21, the file was
birthed on: 2024-08-07 07:35:44.428716, and at report knit time is
-9684.36 hours old. Happy analyzing!

Now that we have our data lets take a look at it using the `skimr`
package.

``` r
skim(downloads_tbl)
```

|                                                  |               |
|:-------------------------------------------------|:--------------|
| Name                                             | downloads_tbl |
| Number of rows                                   | 152092        |
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
| r_version     |    110605 |          0.27 |   5 |   5 |     0 |       48 |          0 |
| r_arch        |    110605 |          0.27 |   3 |   7 |     0 |        5 |          0 |
| r_os          |    110605 |          0.27 |   7 |  15 |     0 |       23 |          0 |
| package       |         0 |          1.00 |   7 |  13 |     0 |        8 |          0 |
| version       |         0 |          1.00 |   5 |  17 |     0 |       62 |          0 |
| country       |     13291 |          0.91 |   2 |   2 |     0 |      165 |          0 |

**Variable type: Date**

| skim_variable | n_missing | complete_rate | min | max | median | n_unique |
|:---|---:|---:|:---|:---|:---|---:|
| date | 0 | 1 | 2020-11-23 | 2025-09-14 | 2023-08-31 | 1750 |

**Variable type: numeric**

| skim_variable | n_missing | complete_rate | mean | sd | p0 | p25 | p50 | p75 | p100 | hist |
|:---|---:|---:|---:|---:|---:|---:|---:|---:|---:|:---|
| size | 0 | 1 | 1129679.97 | 1502116.95 | 355 | 14734 | 307237 | 2365161 | 5677952 | ▇▁▂▁▁ |
| ip_id | 0 | 1 | 11257.76 | 21611.49 | 1 | 258 | 3030 | 12089 | 299146 | ▇▁▁▁▁ |

**Variable type: POSIXct**

| skim_variable | n_missing | complete_rate | min | max | median | n_unique |
|:---|---:|---:|:---|:---|:---|---:|
| date_time | 0 | 1 | 2020-11-23 09:00:41 | 2025-09-14 23:57:21 | 2023-08-31 08:16:17 | 94697 |

**Variable type: Timespan**

| skim_variable | n_missing | complete_rate | min | max | median | n_unique |
|:--------------|----------:|--------------:|----:|----:|-------:|---------:|
| time          |         0 |             1 |   0 |  59 |     29 |       60 |

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
    ## -146.75  -36.22  -10.89   26.68  817.44 
    ## 
    ## Coefficients:
    ##                                                      Estimate Std. Error
    ## (Intercept)                                        -1.907e+02  6.276e+01
    ## date                                                1.159e-02  3.324e-03
    ## lag(value, 1)                                       1.096e-01  2.370e-02
    ## lag(value, 7)                                       9.030e-02  2.453e-02
    ## lag(value, 14)                                      8.247e-02  2.457e-02
    ## lag(value, 21)                                      6.301e-02  2.459e-02
    ## lag(value, 28)                                      7.069e-02  2.456e-02
    ## lag(value, 35)                                      7.303e-02  2.462e-02
    ## lag(value, 42)                                      5.911e-02  2.477e-02
    ## lag(value, 49)                                      6.304e-02  2.467e-02
    ## month(date, label = TRUE).L                        -9.141e+00  5.106e+00
    ## month(date, label = TRUE).Q                         2.740e+00  4.993e+00
    ## month(date, label = TRUE).C                        -1.454e+01  5.067e+00
    ## month(date, label = TRUE)^4                        -7.298e+00  5.074e+00
    ## month(date, label = TRUE)^5                        -1.030e+01  5.029e+00
    ## month(date, label = TRUE)^6                        -3.082e+00  5.097e+00
    ## month(date, label = TRUE)^7                        -7.179e+00  4.996e+00
    ## month(date, label = TRUE)^8                        -4.222e+00  4.979e+00
    ## month(date, label = TRUE)^9                         4.157e+00  4.942e+00
    ## month(date, label = TRUE)^10                        2.505e+00  4.884e+00
    ## month(date, label = TRUE)^11                       -3.514e+00  4.816e+00
    ## fourier_vec(date, type = "sin", K = 1, period = 7) -1.141e+01  2.267e+00
    ## fourier_vec(date, type = "cos", K = 1, period = 7)  7.395e+00  2.375e+00
    ##                                                    t value Pr(>|t|)    
    ## (Intercept)                                         -3.039 0.002410 ** 
    ## date                                                 3.487 0.000502 ***
    ## lag(value, 1)                                        4.624 4.05e-06 ***
    ## lag(value, 7)                                        3.681 0.000239 ***
    ## lag(value, 14)                                       3.356 0.000808 ***
    ## lag(value, 21)                                       2.562 0.010492 *  
    ## lag(value, 28)                                       2.878 0.004055 ** 
    ## lag(value, 35)                                       2.967 0.003052 ** 
    ## lag(value, 42)                                       2.386 0.017142 *  
    ## lag(value, 49)                                       2.555 0.010710 *  
    ## month(date, label = TRUE).L                         -1.790 0.073573 .  
    ## month(date, label = TRUE).Q                          0.549 0.583265    
    ## month(date, label = TRUE).C                         -2.870 0.004158 ** 
    ## month(date, label = TRUE)^4                         -1.438 0.150482    
    ## month(date, label = TRUE)^5                         -2.048 0.040750 *  
    ## month(date, label = TRUE)^6                         -0.605 0.545515    
    ## month(date, label = TRUE)^7                         -1.437 0.150885    
    ## month(date, label = TRUE)^8                         -0.848 0.396590    
    ## month(date, label = TRUE)^9                          0.841 0.400372    
    ## month(date, label = TRUE)^10                         0.513 0.608036    
    ## month(date, label = TRUE)^11                        -0.730 0.465637    
    ## fourier_vec(date, type = "sin", K = 1, period = 7)  -5.034 5.31e-07 ***
    ## fourier_vec(date, type = "cos", K = 1, period = 7)   3.114 0.001878 ** 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 58.87 on 1678 degrees of freedom
    ##   (49 observations deleted due to missingness)
    ## Multiple R-squared:  0.2329, Adjusted R-squared:  0.2228 
    ## F-statistic: 23.16 on 22 and 1678 DF,  p-value: < 2.2e-16

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
                                         ) - tail(x, 28)) ^ 2)))
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
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 24 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 10.0059483517188"
    ## [1] "BEST method = 'lin' PATH MEMBER = c( 24 )"
    ## [1] "BEST lin OBJECTIVE FUNCTION = 10.0059483517188"
    ## [1] "CURRNET METHOD: nonlin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'nonlin' , seasonal.factor =  c( 24 ) ...)"
    ## [1] "CURRENT nonlin OBJECTIVE FUNCTION = 13.9667479211027"
    ## [1] "BEST method = 'nonlin' PATH MEMBER = c( 24 )"
    ## [1] "BEST nonlin OBJECTIVE FUNCTION = 13.9667479211027"
    ## [1] "CURRNET METHOD: both"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'both' , seasonal.factor =  c( 24 ) ...)"
    ## [1] "CURRENT both OBJECTIVE FUNCTION = 9.97073695933843"
    ## [1] "BEST method = 'both' PATH MEMBER = c( 24 )"
    ## [1] "BEST both OBJECTIVE FUNCTION = 9.97073695933843"

![](man/figures/README-nns_forecasting-1.png)<!-- -->

    ## Package: healthyR.ai
    ## [1] "CURRNET METHOD: lin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 21 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 12.3176498742681"
    ## [1] "BEST method = 'lin' PATH MEMBER = c( 21 )"
    ## [1] "BEST lin OBJECTIVE FUNCTION = 12.3176498742681"
    ## [1] "CURRNET METHOD: nonlin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'nonlin' , seasonal.factor =  c( 21 ) ...)"
    ## [1] "CURRENT nonlin OBJECTIVE FUNCTION = 10.8159278041926"
    ## [1] "BEST method = 'nonlin' PATH MEMBER = c( 21 )"
    ## [1] "BEST nonlin OBJECTIVE FUNCTION = 10.8159278041926"
    ## [1] "CURRNET METHOD: both"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'both' , seasonal.factor =  c( 21 ) ...)"
    ## [1] "CURRENT both OBJECTIVE FUNCTION = 14.4748936965833"
    ## [1] "BEST method = 'both' PATH MEMBER = c( 21 )"
    ## [1] "BEST both OBJECTIVE FUNCTION = 14.4748936965833"

![](man/figures/README-nns_forecasting-2.png)<!-- -->

    ## Package: healthyR.data
    ## [1] "CURRNET METHOD: lin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 13 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 8.13009787421926"
    ## [1] "BEST method = 'lin' PATH MEMBER = c( 13 )"
    ## [1] "BEST lin OBJECTIVE FUNCTION = 8.13009787421926"
    ## [1] "CURRNET METHOD: nonlin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'nonlin' , seasonal.factor =  c( 13 ) ...)"
    ## [1] "CURRENT nonlin OBJECTIVE FUNCTION = 12.1175135802178"
    ## [1] "BEST method = 'nonlin' PATH MEMBER = c( 13 )"
    ## [1] "BEST nonlin OBJECTIVE FUNCTION = 12.1175135802178"
    ## [1] "CURRNET METHOD: both"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'both' , seasonal.factor =  c( 13 ) ...)"
    ## [1] "CURRENT both OBJECTIVE FUNCTION = 11.8663677814522"
    ## [1] "BEST method = 'both' PATH MEMBER = c( 13 )"
    ## [1] "BEST both OBJECTIVE FUNCTION = 11.8663677814522"

![](man/figures/README-nns_forecasting-3.png)<!-- -->

    ## Package: healthyR.ts
    ## [1] "CURRNET METHOD: lin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 11 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 12.4785279286624"
    ## [1] "BEST method = 'lin' PATH MEMBER = c( 11 )"
    ## [1] "BEST lin OBJECTIVE FUNCTION = 12.4785279286624"
    ## [1] "CURRNET METHOD: nonlin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'nonlin' , seasonal.factor =  c( 11 ) ...)"
    ## [1] "CURRENT nonlin OBJECTIVE FUNCTION = 6.36651036388458"
    ## [1] "BEST method = 'nonlin' PATH MEMBER = c( 11 )"
    ## [1] "BEST nonlin OBJECTIVE FUNCTION = 6.36651036388458"
    ## [1] "CURRNET METHOD: both"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'both' , seasonal.factor =  c( 11 ) ...)"
    ## [1] "CURRENT both OBJECTIVE FUNCTION = 11.7665216778965"
    ## [1] "BEST method = 'both' PATH MEMBER = c( 11 )"
    ## [1] "BEST both OBJECTIVE FUNCTION = 11.7665216778965"

![](man/figures/README-nns_forecasting-4.png)<!-- -->

    ## Package: healthyverse
    ## [1] "CURRNET METHOD: lin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 7 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 13.379214573746"
    ## [1] "BEST method = 'lin' PATH MEMBER = c( 7 )"
    ## [1] "BEST lin OBJECTIVE FUNCTION = 13.379214573746"
    ## [1] "CURRNET METHOD: nonlin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'nonlin' , seasonal.factor =  c( 7 ) ...)"
    ## [1] "CURRENT nonlin OBJECTIVE FUNCTION = 4.88234438516529"
    ## [1] "BEST method = 'nonlin' PATH MEMBER = c( 7 )"
    ## [1] "BEST nonlin OBJECTIVE FUNCTION = 4.88234438516529"
    ## [1] "CURRNET METHOD: both"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'both' , seasonal.factor =  c( 7 ) ...)"
    ## [1] "CURRENT both OBJECTIVE FUNCTION = 6.90543876208729"
    ## [1] "BEST method = 'both' PATH MEMBER = c( 7 )"
    ## [1] "BEST both OBJECTIVE FUNCTION = 6.90543876208729"

![](man/figures/README-nns_forecasting-5.png)<!-- -->

    ## Package: RandomWalker
    ## [1] "CURRNET METHOD: lin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 7 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 5.83949977673726"
    ## [1] "BEST method = 'lin' PATH MEMBER = c( 7 )"
    ## [1] "BEST lin OBJECTIVE FUNCTION = 5.83949977673726"
    ## [1] "CURRNET METHOD: nonlin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'nonlin' , seasonal.factor =  c( 7 ) ...)"
    ## [1] "CURRENT nonlin OBJECTIVE FUNCTION = 4.74223159327071"
    ## [1] "BEST method = 'nonlin' PATH MEMBER = c( 7 )"
    ## [1] "BEST nonlin OBJECTIVE FUNCTION = 4.74223159327071"
    ## [1] "CURRNET METHOD: both"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'both' , seasonal.factor =  c( 7 ) ...)"
    ## [1] "CURRENT both OBJECTIVE FUNCTION = 4.94638193381694"
    ## [1] "BEST method = 'both' PATH MEMBER = c( 7 )"
    ## [1] "BEST both OBJECTIVE FUNCTION = 4.94638193381694"

![](man/figures/README-nns_forecasting-6.png)<!-- -->

    ## Package: tidyAML
    ## [1] "CURRNET METHOD: lin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 20 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 10.0622530577582"
    ## [1] "BEST method = 'lin' PATH MEMBER = c( 20 )"
    ## [1] "BEST lin OBJECTIVE FUNCTION = 10.0622530577582"
    ## [1] "CURRNET METHOD: nonlin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'nonlin' , seasonal.factor =  c( 20 ) ...)"
    ## [1] "CURRENT nonlin OBJECTIVE FUNCTION = 8.55011987547076"
    ## [1] "BEST method = 'nonlin' PATH MEMBER = c( 20 )"
    ## [1] "BEST nonlin OBJECTIVE FUNCTION = 8.55011987547076"
    ## [1] "CURRNET METHOD: both"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'both' , seasonal.factor =  c( 20 ) ...)"
    ## [1] "CURRENT both OBJECTIVE FUNCTION = 11.2802680888127"
    ## [1] "BEST method = 'both' PATH MEMBER = c( 20 )"
    ## [1] "BEST both OBJECTIVE FUNCTION = 11.2802680888127"

![](man/figures/README-nns_forecasting-7.png)<!-- -->

    ## Package: TidyDensity
    ## [1] "CURRNET METHOD: lin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 13 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 25.8561233018776"
    ## [1] "BEST method = 'lin' PATH MEMBER = c( 13 )"
    ## [1] "BEST lin OBJECTIVE FUNCTION = 25.8561233018776"
    ## [1] "CURRNET METHOD: nonlin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'nonlin' , seasonal.factor =  c( 13 ) ...)"
    ## [1] "CURRENT nonlin OBJECTIVE FUNCTION = 9.7368396683016"
    ## [1] "BEST method = 'nonlin' PATH MEMBER = c( 13 )"
    ## [1] "BEST nonlin OBJECTIVE FUNCTION = 9.7368396683016"
    ## [1] "CURRNET METHOD: both"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'both' , seasonal.factor =  c( 13 ) ...)"
    ## [1] "CURRENT both OBJECTIVE FUNCTION = 11.5089482784673"
    ## [1] "BEST method = 'both' PATH MEMBER = c( 13 )"
    ## [1] "BEST both OBJECTIVE FUNCTION = 11.5089482784673"

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

    ## # A tibble: 9 × 4
    ##   package       .actual_data         .future_data      .splits          
    ##   <fct>         <list>               <list>            <list>           
    ## 1 healthyR.data <tibble [1,742 × 2]> <tibble [28 × 2]> <split [1714|28]>
    ## 2 healthyR      <tibble [1,733 × 2]> <tibble [28 × 2]> <split [1705|28]>
    ## 3 healthyR.ts   <tibble [1,679 × 2]> <tibble [28 × 2]> <split [1651|28]>
    ## 4 healthyverse  <tibble [1,650 × 2]> <tibble [28 × 2]> <split [1622|28]>
    ## 5 healthyR.ai   <tibble [1,475 × 2]> <tibble [28 × 2]> <split [1447|28]>
    ## 6 TidyDensity   <tibble [1,326 × 2]> <tibble [28 × 2]> <split [1298|28]>
    ## 7 tidyAML       <tibble [933 × 2]>   <tibble [28 × 2]> <split [905|28]> 
    ## 8 RandomWalker  <tibble [356 × 2]>   <tibble [28 × 2]> <split [328|28]> 
    ## 9 <NA>          <tibble [7 × 2]>     <tibble [28 × 2]> <split [0|7]>

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
| healthyR.data | 1 | ARIMA | Test | 0.7999450 | 109.14248 | 0.9442627 | 182.75646 | 1.0093453 | 0.0860141 |
| healthyR.data | 2 | LM | Test | 0.8096082 | 197.08903 | 0.9556692 | 148.63912 | 0.9303029 | 0.0176677 |
| healthyR.data | 3 | NULL | NA | NA | NA | NA | NA | NA | NA |
| healthyR.data | 4 | NNAR | Test | 0.7675711 | 95.76575 | 0.9060481 | 182.32284 | 0.9939944 | 0.2296907 |
| healthyR | 1 | ARIMA | Test | 0.6626925 | 119.24076 | 0.7541754 | 180.80660 | 0.8567785 | 0.1219689 |
| healthyR | 2 | LM | Test | 0.6345916 | 119.89237 | 0.7221952 | 168.82655 | 0.7969395 | 0.0554915 |
| healthyR | 3 | NULL | NA | NA | NA | NA | NA | NA | NA |
| healthyR | 4 | NNAR | Test | 0.6400480 | 128.91708 | 0.7284049 | 164.89039 | 0.7989626 | 0.0075321 |
| healthyR.ts | 1 | ARIMA | Test | 0.8672090 | 176.33121 | 0.9096682 | 124.81689 | 1.0770539 | 0.0036609 |
| healthyR.ts | 2 | LM | Test | 0.8198718 | 140.33514 | 0.8600133 | 130.07286 | 1.0373471 | 0.0040454 |
| healthyR.ts | 3 | NULL | NA | NA | NA | NA | NA | NA | NA |
| healthyR.ts | 4 | NNAR | Test | 0.8689315 | 104.88587 | 0.9114751 | 184.82283 | 1.0859863 | 0.0105486 |
| healthyverse | 1 | ARIMA | Test | 0.8056136 | 105.98852 | 1.1698006 | 113.90710 | 0.9560631 | 0.0132858 |
| healthyverse | 2 | LM | Test | 0.6945897 | 119.11956 | 1.0085870 | 86.32281 | 0.8446955 | 0.0021313 |
| healthyverse | 3 | NULL | NA | NA | NA | NA | NA | NA | NA |
| healthyverse | 4 | NNAR | Test | 0.7992850 | 101.01083 | 1.1606109 | 114.93997 | 0.9495578 | 0.0006789 |
| healthyR.ai | 1 | ARIMA | Test | 0.7198749 | 98.92659 | 1.0271922 | 151.57735 | 0.9612333 | 0.0223451 |
| healthyR.ai | 2 | LM | Test | 0.7149949 | 105.04149 | 1.0202290 | 136.13046 | 0.9693236 | 0.0118951 |
| healthyR.ai | 3 | NULL | NA | NA | NA | NA | NA | NA | NA |
| healthyR.ai | 4 | NNAR | Test | 0.7282733 | 105.73112 | 1.0391760 | 147.52877 | 0.9713736 | 0.0002457 |
| TidyDensity | 1 | ARIMA | Test | 0.8627259 | 252.39615 | 0.9875323 | 107.34913 | 1.2004738 | 0.0046797 |
| TidyDensity | 2 | LM | Test | 0.8630191 | 254.76844 | 0.9878679 | 106.37883 | 1.1887059 | 0.3253452 |
| TidyDensity | 3 | NULL | NA | NA | NA | NA | NA | NA | NA |
| TidyDensity | 4 | NNAR | Test | 0.9349232 | 142.19272 | 1.0701740 | 136.45358 | 1.3652241 | 0.0152394 |
| tidyAML | 1 | ARIMA | Test | 0.7445653 | 94.64179 | 0.9823669 | 113.37827 | 0.8732447 | 0.0096053 |
| tidyAML | 2 | LM | Test | 0.6738604 | 106.96863 | 0.8890801 | 91.52382 | 0.8158340 | 0.0752657 |
| tidyAML | 3 | NULL | NA | NA | NA | NA | NA | NA | NA |
| tidyAML | 4 | NNAR | Test | 0.7162354 | 99.90422 | 0.9449890 | 107.77185 | 0.8325153 | 0.0355660 |
| RandomWalker | 1 | ARIMA | Test | 1.0886328 | 105.51869 | 0.6587249 | 172.22135 | 1.3270065 | 0.0516695 |
| RandomWalker | 2 | LM | Test | 1.0995712 | 119.66572 | 0.6653437 | 164.46141 | 1.3141414 | 0.0050384 |
| RandomWalker | 3 | NULL | NA | NA | NA | NA | NA | NA | NA |
| RandomWalker | 4 | NNAR | Test | 1.0808211 | 118.31837 | 0.6539981 | 170.00741 | 1.3016579 | 0.0484288 |

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
    ## 1 healthyR.da…         2 LM          Test  0.810 197.  0.956 149.  0.930 0.0177 
    ## 2 healthyR             2 LM          Test  0.635 120.  0.722 169.  0.797 0.0555 
    ## 3 healthyR.ts          2 LM          Test  0.820 140.  0.860 130.  1.04  0.00405
    ## 4 healthyverse         2 LM          Test  0.695 119.  1.01   86.3 0.845 0.00213
    ## 5 healthyR.ai          1 ARIMA       Test  0.720  98.9 1.03  152.  0.961 0.0223 
    ## 6 TidyDensity          2 LM          Test  0.863 255.  0.988 106.  1.19  0.325  
    ## 7 tidyAML              2 LM          Test  0.674 107.  0.889  91.5 0.816 0.0753 
    ## 8 RandomWalker         4 NNAR        Test  1.08  118.  0.654 170.  1.30  0.0484

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
    ## 1 healthyR.data <tibble>     <tibble>     <split [1714|28]> <mdl_tm_t [1 × 5]>
    ## 2 healthyR      <tibble>     <tibble>     <split [1705|28]> <mdl_tm_t [1 × 5]>
    ## 3 healthyR.ts   <tibble>     <tibble>     <split [1651|28]> <mdl_tm_t [1 × 5]>
    ## 4 healthyverse  <tibble>     <tibble>     <split [1622|28]> <mdl_tm_t [1 × 5]>
    ## 5 healthyR.ai   <tibble>     <tibble>     <split [1447|28]> <mdl_tm_t [1 × 5]>
    ## 6 TidyDensity   <tibble>     <tibble>     <split [1298|28]> <mdl_tm_t [1 × 5]>
    ## 7 tidyAML       <tibble>     <tibble>     <split [905|28]>  <mdl_tm_t [1 × 5]>
    ## 8 RandomWalker  <tibble>     <tibble>     <split [328|28]>  <mdl_tm_t [1 × 5]>

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
