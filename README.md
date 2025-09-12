Time Series Analysis, Modeling and Forecasting of the Healthyverse
Packages
================
Steven P. Sanderson II, MPH - Date:
12 September, 2025

This analysis follows a *Nested Modeltime Workflow*.

## Get Data

``` r
glimpse(downloads_tbl)
```

    ## Rows: 151,381
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

The last day in the data set is 2025-09-10 23:50:53, the file was
birthed on: 2024-08-07 07:35:44.428716, and at report knit time is
-9588.25 hours old. Happy analyzing!

Now that we have our data lets take a look at it using the `skimr`
package.

``` r
skim(downloads_tbl)
```

|                                                  |               |
|:-------------------------------------------------|:--------------|
| Name                                             | downloads_tbl |
| Number of rows                                   | 151381        |
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
| r_version     |    109961 |          0.27 |   5 |   5 |     0 |       48 |          0 |
| r_arch        |    109961 |          0.27 |   3 |   7 |     0 |        5 |          0 |
| r_os          |    109961 |          0.27 |   7 |  15 |     0 |       23 |          0 |
| package       |         0 |          1.00 |   7 |  13 |     0 |        8 |          0 |
| version       |         0 |          1.00 |   5 |  17 |     0 |       62 |          0 |
| country       |     13000 |          0.91 |   2 |   2 |     0 |      165 |          0 |

**Variable type: Date**

| skim_variable | n_missing | complete_rate | min | max | median | n_unique |
|:---|---:|---:|:---|:---|:---|---:|
| date | 0 | 1 | 2020-11-23 | 2025-09-10 | 2023-08-28 | 1746 |

**Variable type: numeric**

| skim_variable | n_missing | complete_rate | mean | sd | p0 | p25 | p50 | p75 | p100 | hist |
|:---|---:|---:|---:|---:|---:|---:|---:|---:|---:|:---|
| size | 0 | 1 | 1130086.62 | 1503296.35 | 355 | 14701 | 306736 | 2365490 | 5677952 | ▇▁▂▁▁ |
| ip_id | 0 | 1 | 11275.42 | 21612.23 | 1 | 280 | 3058 | 12138 | 299146 | ▇▁▁▁▁ |

**Variable type: POSIXct**

| skim_variable | n_missing | complete_rate | min | max | median | n_unique |
|:---|---:|---:|:---|:---|:---|---:|
| date_time | 0 | 1 | 2020-11-23 09:00:41 | 2025-09-10 23:50:53 | 2023-08-28 02:20:23 | 94093 |

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
    ## -147.12  -36.02  -11.03   26.72  817.96 
    ## 
    ## Coefficients:
    ##                                                      Estimate Std. Error
    ## (Intercept)                                        -1.825e+02  6.280e+01
    ## date                                                1.118e-02  3.325e-03
    ## lag(value, 1)                                       1.066e-01  2.372e-02
    ## lag(value, 7)                                       9.054e-02  2.452e-02
    ## lag(value, 14)                                      8.080e-02  2.455e-02
    ## lag(value, 21)                                      6.509e-02  2.458e-02
    ## lag(value, 28)                                      6.987e-02  2.455e-02
    ## lag(value, 35)                                      6.899e-02  2.462e-02
    ## lag(value, 42)                                      5.911e-02  2.475e-02
    ## lag(value, 49)                                      6.413e-02  2.464e-02
    ## month(date, label = TRUE).L                        -9.525e+00  5.098e+00
    ## month(date, label = TRUE).Q                         3.062e+00  4.986e+00
    ## month(date, label = TRUE).C                        -1.373e+01  5.068e+00
    ## month(date, label = TRUE)^4                        -6.967e+00  5.067e+00
    ## month(date, label = TRUE)^5                        -1.079e+01  5.023e+00
    ## month(date, label = TRUE)^6                        -3.922e+00  5.099e+00
    ## month(date, label = TRUE)^7                        -7.500e+00  4.988e+00
    ## month(date, label = TRUE)^8                        -3.712e+00  4.975e+00
    ## month(date, label = TRUE)^9                         5.197e+00  4.951e+00
    ## month(date, label = TRUE)^10                        3.421e+00  4.888e+00
    ## month(date, label = TRUE)^11                       -3.067e+00  4.811e+00
    ## fourier_vec(date, type = "sin", K = 1, period = 7) -1.176e+01  2.266e+00
    ## fourier_vec(date, type = "cos", K = 1, period = 7)  7.647e+00  2.378e+00
    ##                                                    t value Pr(>|t|)    
    ## (Intercept)                                         -2.906 0.003712 ** 
    ## date                                                 3.363 0.000789 ***
    ## lag(value, 1)                                        4.494 7.47e-06 ***
    ## lag(value, 7)                                        3.693 0.000229 ***
    ## lag(value, 14)                                       3.292 0.001017 ** 
    ## lag(value, 21)                                       2.648 0.008168 ** 
    ## lag(value, 28)                                       2.846 0.004479 ** 
    ## lag(value, 35)                                       2.802 0.005136 ** 
    ## lag(value, 42)                                       2.388 0.017057 *  
    ## lag(value, 49)                                       2.603 0.009319 ** 
    ## month(date, label = TRUE).L                         -1.868 0.061915 .  
    ## month(date, label = TRUE).Q                          0.614 0.539148    
    ## month(date, label = TRUE).C                         -2.710 0.006795 ** 
    ## month(date, label = TRUE)^4                         -1.375 0.169259    
    ## month(date, label = TRUE)^5                         -2.148 0.031879 *  
    ## month(date, label = TRUE)^6                         -0.769 0.441852    
    ## month(date, label = TRUE)^7                         -1.504 0.132840    
    ## month(date, label = TRUE)^8                         -0.746 0.455714    
    ## month(date, label = TRUE)^9                          1.050 0.293974    
    ## month(date, label = TRUE)^10                         0.700 0.484122    
    ## month(date, label = TRUE)^11                        -0.638 0.523875    
    ## fourier_vec(date, type = "sin", K = 1, period = 7)  -5.189 2.37e-07 ***
    ## fourier_vec(date, type = "cos", K = 1, period = 7)   3.216 0.001324 ** 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 58.76 on 1674 degrees of freedom
    ##   (49 observations deleted due to missingness)
    ## Multiple R-squared:  0.233,  Adjusted R-squared:  0.2229 
    ## F-statistic: 23.12 on 22 and 1674 DF,  p-value: < 2.2e-16

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
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 9.60082779975413"
    ## [1] "BEST method = 'lin' PATH MEMBER = c( 24 )"
    ## [1] "BEST lin OBJECTIVE FUNCTION = 9.60082779975413"
    ## [1] "CURRNET METHOD: nonlin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'nonlin' , seasonal.factor =  c( 24 ) ...)"
    ## [1] "CURRENT nonlin OBJECTIVE FUNCTION = 13.1545496404484"
    ## [1] "BEST method = 'nonlin' PATH MEMBER = c( 24 )"
    ## [1] "BEST nonlin OBJECTIVE FUNCTION = 13.1545496404484"
    ## [1] "CURRNET METHOD: both"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'both' , seasonal.factor =  c( 24 ) ...)"
    ## [1] "CURRENT both OBJECTIVE FUNCTION = 9.36075466311881"
    ## [1] "BEST method = 'both' PATH MEMBER = c( 24 )"
    ## [1] "BEST both OBJECTIVE FUNCTION = 9.36075466311881"

![](man/figures/README-nns_forecasting-1.png)<!-- -->

    ## Package: healthyR.ai
    ## [1] "CURRNET METHOD: lin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 13 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 12.2041867255477"
    ## [1] "BEST method = 'lin' PATH MEMBER = c( 13 )"
    ## [1] "BEST lin OBJECTIVE FUNCTION = 12.2041867255477"
    ## [1] "CURRNET METHOD: nonlin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'nonlin' , seasonal.factor =  c( 13 ) ...)"
    ## [1] "CURRENT nonlin OBJECTIVE FUNCTION = 7.88819195091096"
    ## [1] "BEST method = 'nonlin' PATH MEMBER = c( 13 )"
    ## [1] "BEST nonlin OBJECTIVE FUNCTION = 7.88819195091096"
    ## [1] "CURRNET METHOD: both"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'both' , seasonal.factor =  c( 13 ) ...)"
    ## [1] "CURRENT both OBJECTIVE FUNCTION = 13.1226641768522"
    ## [1] "BEST method = 'both' PATH MEMBER = c( 13 )"
    ## [1] "BEST both OBJECTIVE FUNCTION = 13.1226641768522"

![](man/figures/README-nns_forecasting-2.png)<!-- -->

    ## Package: healthyR.data
    ## [1] "CURRNET METHOD: lin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 13 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 7.0343246888378"
    ## [1] "BEST method = 'lin' PATH MEMBER = c( 13 )"
    ## [1] "BEST lin OBJECTIVE FUNCTION = 7.0343246888378"
    ## [1] "CURRNET METHOD: nonlin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'nonlin' , seasonal.factor =  c( 13 ) ...)"
    ## [1] "CURRENT nonlin OBJECTIVE FUNCTION = 8.85888643361537"
    ## [1] "BEST method = 'nonlin' PATH MEMBER = c( 13 )"
    ## [1] "BEST nonlin OBJECTIVE FUNCTION = 8.85888643361537"
    ## [1] "CURRNET METHOD: both"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'both' , seasonal.factor =  c( 13 ) ...)"
    ## [1] "CURRENT both OBJECTIVE FUNCTION = 8.5324941025523"
    ## [1] "BEST method = 'both' PATH MEMBER = c( 13 )"
    ## [1] "BEST both OBJECTIVE FUNCTION = 8.5324941025523"

![](man/figures/README-nns_forecasting-3.png)<!-- -->

    ## Package: healthyR.ts
    ## [1] "CURRNET METHOD: lin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 11 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 14.2222289675758"
    ## [1] "BEST method = 'lin' PATH MEMBER = c( 11 )"
    ## [1] "BEST lin OBJECTIVE FUNCTION = 14.2222289675758"
    ## [1] "CURRNET METHOD: nonlin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'nonlin' , seasonal.factor =  c( 11 ) ...)"
    ## [1] "CURRENT nonlin OBJECTIVE FUNCTION = 9.49096608777241"
    ## [1] "BEST method = 'nonlin' PATH MEMBER = c( 11 )"
    ## [1] "BEST nonlin OBJECTIVE FUNCTION = 9.49096608777241"
    ## [1] "CURRNET METHOD: both"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'both' , seasonal.factor =  c( 11 ) ...)"
    ## [1] "CURRENT both OBJECTIVE FUNCTION = 17.3670932331847"
    ## [1] "BEST method = 'both' PATH MEMBER = c( 11 )"
    ## [1] "BEST both OBJECTIVE FUNCTION = 17.3670932331847"

![](man/figures/README-nns_forecasting-4.png)<!-- -->

    ## Package: healthyverse
    ## [1] "CURRNET METHOD: lin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 25 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 7.01263567037238"
    ## [1] "BEST method = 'lin' PATH MEMBER = c( 25 )"
    ## [1] "BEST lin OBJECTIVE FUNCTION = 7.01263567037238"
    ## [1] "CURRNET METHOD: nonlin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'nonlin' , seasonal.factor =  c( 25 ) ...)"
    ## [1] "CURRENT nonlin OBJECTIVE FUNCTION = 5.69010399701212"
    ## [1] "BEST method = 'nonlin' PATH MEMBER = c( 25 )"
    ## [1] "BEST nonlin OBJECTIVE FUNCTION = 5.69010399701212"
    ## [1] "CURRNET METHOD: both"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'both' , seasonal.factor =  c( 25 ) ...)"
    ## [1] "CURRENT both OBJECTIVE FUNCTION = 5.27179171906272"
    ## [1] "BEST method = 'both' PATH MEMBER = c( 25 )"
    ## [1] "BEST both OBJECTIVE FUNCTION = 5.27179171906272"

![](man/figures/README-nns_forecasting-5.png)<!-- -->

    ## Package: RandomWalker
    ## [1] "CURRNET METHOD: lin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 7 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 6.02240005902774"
    ## [1] "BEST method = 'lin' PATH MEMBER = c( 7 )"
    ## [1] "BEST lin OBJECTIVE FUNCTION = 6.02240005902774"
    ## [1] "CURRNET METHOD: nonlin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'nonlin' , seasonal.factor =  c( 7 ) ...)"
    ## [1] "CURRENT nonlin OBJECTIVE FUNCTION = 3.38989287659741"
    ## [1] "BEST method = 'nonlin' PATH MEMBER = c( 7 )"
    ## [1] "BEST nonlin OBJECTIVE FUNCTION = 3.38989287659741"
    ## [1] "CURRNET METHOD: both"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'both' , seasonal.factor =  c( 7 ) ...)"
    ## [1] "CURRENT both OBJECTIVE FUNCTION = 3.76115527260196"
    ## [1] "BEST method = 'both' PATH MEMBER = c( 7 )"
    ## [1] "BEST both OBJECTIVE FUNCTION = 3.76115527260196"

![](man/figures/README-nns_forecasting-6.png)<!-- -->

    ## Package: tidyAML
    ## [1] "CURRNET METHOD: lin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 1 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 195.566315181537"
    ## [1] "BEST method = 'lin' PATH MEMBER = c( 1 )"
    ## [1] "BEST lin OBJECTIVE FUNCTION = 195.566315181537"
    ## [1] "CURRNET METHOD: nonlin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'nonlin' , seasonal.factor =  c( 1 ) ...)"
    ## [1] "CURRENT nonlin OBJECTIVE FUNCTION = 38.4924183145904"
    ## [1] "BEST method = 'nonlin' PATH MEMBER = c( 1 )"
    ## [1] "BEST nonlin OBJECTIVE FUNCTION = 38.4924183145904"
    ## [1] "CURRNET METHOD: both"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'both' , seasonal.factor =  c( 1 ) ...)"
    ## [1] "CURRENT both OBJECTIVE FUNCTION = 56.7587254810986"
    ## [1] "BEST method = 'both' PATH MEMBER = c( 1 )"
    ## [1] "BEST both OBJECTIVE FUNCTION = 56.7587254810986"

![](man/figures/README-nns_forecasting-7.png)<!-- -->

    ## Package: TidyDensity
    ## [1] "CURRNET METHOD: lin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 16 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 26.7402181000311"
    ## [1] "BEST method = 'lin' PATH MEMBER = c( 16 )"
    ## [1] "BEST lin OBJECTIVE FUNCTION = 26.7402181000311"
    ## [1] "CURRNET METHOD: nonlin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'nonlin' , seasonal.factor =  c( 16 ) ...)"
    ## [1] "CURRENT nonlin OBJECTIVE FUNCTION = 9.26622740506745"
    ## [1] "BEST method = 'nonlin' PATH MEMBER = c( 16 )"
    ## [1] "BEST nonlin OBJECTIVE FUNCTION = 9.26622740506745"
    ## [1] "CURRNET METHOD: both"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'both' , seasonal.factor =  c( 16 ) ...)"
    ## [1] "CURRENT both OBJECTIVE FUNCTION = 16.849395536544"
    ## [1] "BEST method = 'both' PATH MEMBER = c( 16 )"
    ## [1] "BEST both OBJECTIVE FUNCTION = 16.849395536544"

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
    ## 1 healthyR.data <tibble [1,738 × 2]> <tibble [28 × 2]> <split [1710|28]>
    ## 2 healthyR      <tibble [1,729 × 2]> <tibble [28 × 2]> <split [1701|28]>
    ## 3 healthyR.ts   <tibble [1,675 × 2]> <tibble [28 × 2]> <split [1647|28]>
    ## 4 healthyverse  <tibble [1,646 × 2]> <tibble [28 × 2]> <split [1618|28]>
    ## 5 healthyR.ai   <tibble [1,471 × 2]> <tibble [28 × 2]> <split [1443|28]>
    ## 6 TidyDensity   <tibble [1,322 × 2]> <tibble [28 × 2]> <split [1294|28]>
    ## 7 tidyAML       <tibble [929 × 2]>   <tibble [28 × 2]> <split [901|28]> 
    ## 8 RandomWalker  <tibble [352 × 2]>   <tibble [28 × 2]> <split [324|28]> 
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
| healthyR.data | 1 | ARIMA | Test | 0.8341358 | 116.34392 | 0.9798920 | 165.44722 | 0.9957766 | 0.0001524 |
| healthyR.data | 2 | LM | Test | 0.8830748 | 165.83262 | 1.0373826 | 148.21253 | 1.0090131 | 0.0884993 |
| healthyR.data | 3 | NULL | NA | NA | NA | NA | NA | NA | NA |
| healthyR.data | 4 | NNAR | Test | 0.8345364 | 98.08067 | 0.9803626 | 187.50666 | 1.0276592 | 0.0228112 |
| healthyR | 1 | ARIMA | Test | 0.6416750 | 113.02608 | 0.7682586 | 176.48508 | 0.8117606 | 0.0032473 |
| healthyR | 2 | LM | Test | 0.6497708 | 119.12370 | 0.7779515 | 175.11511 | 0.7951376 | 0.0116916 |
| healthyR | 3 | NULL | NA | NA | NA | NA | NA | NA | NA |
| healthyR | 4 | NNAR | Test | 0.6229973 | 120.17727 | 0.7458964 | 158.81185 | 0.7758076 | 0.0756113 |
| healthyR.ts | 1 | ARIMA | Test | 0.9277502 | 153.38572 | 0.9878776 | 134.68316 | 1.1110069 | 0.0022521 |
| healthyR.ts | 2 | LM | Test | 0.9384553 | 138.84663 | 0.9992765 | 139.91431 | 1.1098843 | 0.0760246 |
| healthyR.ts | 3 | NULL | NA | NA | NA | NA | NA | NA | NA |
| healthyR.ts | 4 | NNAR | Test | 0.9119210 | 95.96606 | 0.9710225 | 180.31725 | 1.1220335 | 0.0586878 |
| healthyverse | 1 | ARIMA | Test | 0.7266954 | 87.72887 | 1.1111151 | 98.39029 | 0.8852081 | 0.1728320 |
| healthyverse | 2 | LM | Test | 0.7434724 | 109.39394 | 1.1367670 | 91.12359 | 0.8917136 | 0.2251601 |
| healthyverse | 3 | NULL | NA | NA | NA | NA | NA | NA | NA |
| healthyverse | 4 | NNAR | Test | 0.7858715 | 87.67659 | 1.2015951 | 113.09742 | 0.9400505 | 0.1872461 |
| healthyR.ai | 1 | ARIMA | Test | 0.6241996 | 96.85196 | 0.9566269 | 140.99050 | 0.7837092 | 0.2928274 |
| healthyR.ai | 2 | LM | Test | 0.6645752 | 107.12235 | 1.0185053 | 137.28071 | 0.8305905 | 0.1105665 |
| healthyR.ai | 3 | NULL | NA | NA | NA | NA | NA | NA | NA |
| healthyR.ai | 4 | NNAR | Test | 0.6145555 | 92.29585 | 0.9418468 | 131.55882 | 0.7955900 | 0.1145878 |
| TidyDensity | 1 | ARIMA | Test | 0.6393067 | 246.49105 | 0.9503953 | 96.90461 | 0.8255365 | 0.0194284 |
| TidyDensity | 2 | LM | Test | 0.6581024 | 249.76441 | 0.9783369 | 98.26214 | 0.8338333 | 0.0961732 |
| TidyDensity | 3 | NULL | NA | NA | NA | NA | NA | NA | NA |
| TidyDensity | 4 | NNAR | Test | 0.6716008 | 143.50308 | 0.9984037 | 121.99473 | 0.9442306 | 0.0071439 |
| tidyAML | 1 | ARIMA | Test | 0.6642382 | 169.67756 | 1.1630275 | 114.93238 | 0.7851766 | 0.0315656 |
| tidyAML | 2 | LM | Test | 0.6607154 | 219.24055 | 1.1568594 | 104.57342 | 0.7825240 | 0.1158837 |
| tidyAML | 3 | NULL | NA | NA | NA | NA | NA | NA | NA |
| tidyAML | 4 | NNAR | Test | 0.6284488 | 143.05460 | 1.1003633 | 116.11219 | 0.7345797 | 0.2830943 |
| RandomWalker | 1 | ARIMA | Test | 1.0787326 | 109.61223 | 0.6360646 | 169.95297 | 1.3194775 | 0.0705380 |
| RandomWalker | 2 | LM | Test | 1.1212265 | 133.24448 | 0.6611207 | 178.41383 | 1.3258115 | 0.0063656 |
| RandomWalker | 3 | NULL | NA | NA | NA | NA | NA | NA | NA |
| RandomWalker | 4 | NNAR | Test | 1.2251430 | 180.76656 | 0.7223941 | 160.37911 | 1.4535639 | 0.0018786 |

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
    ## 1 healthyR.da…         1 ARIMA       Test  0.834 116.  0.980 165.  0.996 1.52e-4
    ## 2 healthyR             4 NNAR        Test  0.623 120.  0.746 159.  0.776 7.56e-2
    ## 3 healthyR.ts          2 LM          Test  0.938 139.  0.999 140.  1.11  7.60e-2
    ## 4 healthyverse         1 ARIMA       Test  0.727  87.7 1.11   98.4 0.885 1.73e-1
    ## 5 healthyR.ai          1 ARIMA       Test  0.624  96.9 0.957 141.  0.784 2.93e-1
    ## 6 TidyDensity          1 ARIMA       Test  0.639 246.  0.950  96.9 0.826 1.94e-2
    ## 7 tidyAML              4 NNAR        Test  0.628 143.  1.10  116.  0.735 2.83e-1
    ## 8 RandomWalker         1 ARIMA       Test  1.08  110.  0.636 170.  1.32  7.05e-2

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
    ## 1 healthyR.data <tibble>     <tibble>     <split [1710|28]> <mdl_tm_t [1 × 5]>
    ## 2 healthyR      <tibble>     <tibble>     <split [1701|28]> <mdl_tm_t [1 × 5]>
    ## 3 healthyR.ts   <tibble>     <tibble>     <split [1647|28]> <mdl_tm_t [1 × 5]>
    ## 4 healthyverse  <tibble>     <tibble>     <split [1618|28]> <mdl_tm_t [1 × 5]>
    ## 5 healthyR.ai   <tibble>     <tibble>     <split [1443|28]> <mdl_tm_t [1 × 5]>
    ## 6 TidyDensity   <tibble>     <tibble>     <split [1294|28]> <mdl_tm_t [1 × 5]>
    ## 7 tidyAML       <tibble>     <tibble>     <split [901|28]>  <mdl_tm_t [1 × 5]>
    ## 8 RandomWalker  <tibble>     <tibble>     <split [324|28]>  <mdl_tm_t [1 × 5]>

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
