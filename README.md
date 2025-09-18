Time Series Analysis, Modeling and Forecasting of the Healthyverse
Packages
================
Steven P. Sanderson II, MPH - Date:
18 September, 2025

This analysis follows a *Nested Modeltime Workflow*.

## Get Data

``` r
glimpse(downloads_tbl)
```

    ## Rows: 152,665
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

The last day in the data set is 2025-09-16 23:39:49, the file was
birthed on: 2024-08-07 07:35:44.428716, and at report knit time is
-9732.07 hours old. Happy analyzing!

Now that we have our data lets take a look at it using the `skimr`
package.

``` r
skim(downloads_tbl)
```

|                                                  |               |
|:-------------------------------------------------|:--------------|
| Name                                             | downloads_tbl |
| Number of rows                                   | 152665        |
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
| r_version     |    111148 |          0.27 |   5 |   5 |     0 |       48 |          0 |
| r_arch        |    111148 |          0.27 |   3 |   7 |     0 |        5 |          0 |
| r_os          |    111148 |          0.27 |   7 |  15 |     0 |       23 |          0 |
| package       |         0 |          1.00 |   7 |  13 |     0 |        8 |          0 |
| version       |         0 |          1.00 |   5 |  17 |     0 |       62 |          0 |
| country       |     13607 |          0.91 |   2 |   2 |     0 |      165 |          0 |

**Variable type: Date**

| skim_variable | n_missing | complete_rate | min | max | median | n_unique |
|:---|---:|---:|:---|:---|:---|---:|
| date | 0 | 1 | 2020-11-23 | 2025-09-16 | 2023-09-04 | 1752 |

**Variable type: numeric**

| skim_variable | n_missing | complete_rate | mean | sd | p0 | p25 | p50 | p75 | p100 | hist |
|:---|---:|---:|---:|---:|---:|---:|---:|---:|---:|:---|
| size | 0 | 1 | 1128332.56 | 1501036.35 | 355 | 16107 | 306965 | 2365156 | 5677952 | ▇▁▂▁▁ |
| ip_id | 0 | 1 | 11234.75 | 21598.84 | 1 | 244 | 3011 | 12036 | 299146 | ▇▁▁▁▁ |

**Variable type: POSIXct**

| skim_variable | n_missing | complete_rate | min | max | median | n_unique |
|:---|---:|---:|:---|:---|:---|---:|
| date_time | 0 | 1 | 2020-11-23 09:00:41 | 2025-09-16 23:39:49 | 2023-09-04 08:43:56 | 95165 |

**Variable type: Timespan**

| skim_variable | n_missing | complete_rate | min | max |     median | n_unique |
|:--------------|----------:|--------------:|----:|----:|-----------:|---------:|
| time          |         0 |             1 |   0 |  59 | 12H 6M 34S |       60 |

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
    ## -146.58  -36.12  -10.82   26.66  815.91 
    ## 
    ## Coefficients:
    ##                                                      Estimate Std. Error
    ## (Intercept)                                        -1.978e+02  6.292e+01
    ## date                                                1.192e-02  3.333e-03
    ## lag(value, 1)                                       1.157e-01  2.370e-02
    ## lag(value, 7)                                       9.163e-02  2.460e-02
    ## lag(value, 14)                                      8.233e-02  2.465e-02
    ## lag(value, 21)                                      6.104e-02  2.466e-02
    ## lag(value, 28)                                      7.584e-02  2.458e-02
    ## lag(value, 35)                                      7.274e-02  2.469e-02
    ## lag(value, 42)                                      5.788e-02  2.485e-02
    ## lag(value, 49)                                      6.252e-02  2.474e-02
    ## month(date, label = TRUE).L                        -8.711e+00  5.120e+00
    ## month(date, label = TRUE).Q                         2.413e+00  5.008e+00
    ## month(date, label = TRUE).C                        -1.528e+01  5.078e+00
    ## month(date, label = TRUE)^4                        -7.550e+00  5.089e+00
    ## month(date, label = TRUE)^5                        -9.754e+00  5.042e+00
    ## month(date, label = TRUE)^6                        -2.181e+00  5.107e+00
    ## month(date, label = TRUE)^7                        -6.830e+00  5.010e+00
    ## month(date, label = TRUE)^8                        -4.700e+00  4.993e+00
    ## month(date, label = TRUE)^9                         3.127e+00  4.948e+00
    ## month(date, label = TRUE)^10                        1.577e+00  4.892e+00
    ## month(date, label = TRUE)^11                       -3.914e+00  4.829e+00
    ## fourier_vec(date, type = "sin", K = 1, period = 7) -1.163e+01  2.273e+00
    ## fourier_vec(date, type = "cos", K = 1, period = 7)  6.943e+00  2.379e+00
    ##                                                    t value Pr(>|t|)    
    ## (Intercept)                                         -3.144 0.001694 ** 
    ## date                                                 3.578 0.000356 ***
    ## lag(value, 1)                                        4.883 1.14e-06 ***
    ## lag(value, 7)                                        3.726 0.000201 ***
    ## lag(value, 14)                                       3.340 0.000856 ***
    ## lag(value, 21)                                       2.475 0.013418 *  
    ## lag(value, 28)                                       3.086 0.002062 ** 
    ## lag(value, 35)                                       2.946 0.003264 ** 
    ## lag(value, 42)                                       2.329 0.019966 *  
    ## lag(value, 49)                                       2.527 0.011607 *  
    ## month(date, label = TRUE).L                         -1.701 0.089069 .  
    ## month(date, label = TRUE).Q                          0.482 0.629991    
    ## month(date, label = TRUE).C                         -3.008 0.002667 ** 
    ## month(date, label = TRUE)^4                         -1.484 0.138103    
    ## month(date, label = TRUE)^5                         -1.935 0.053208 .  
    ## month(date, label = TRUE)^6                         -0.427 0.669398    
    ## month(date, label = TRUE)^7                         -1.363 0.172979    
    ## month(date, label = TRUE)^8                         -0.941 0.346679    
    ## month(date, label = TRUE)^9                          0.632 0.527536    
    ## month(date, label = TRUE)^10                         0.322 0.747228    
    ## month(date, label = TRUE)^11                        -0.810 0.417842    
    ## fourier_vec(date, type = "sin", K = 1, period = 7)  -5.114 3.51e-07 ***
    ## fourier_vec(date, type = "cos", K = 1, period = 7)   2.919 0.003562 ** 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 59.05 on 1680 degrees of freedom
    ##   (49 observations deleted due to missingness)
    ## Multiple R-squared:  0.2351, Adjusted R-squared:  0.2251 
    ## F-statistic: 23.47 on 22 and 1680 DF,  p-value: < 2.2e-16

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
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 11.2388101135098"
    ## [1] "BEST method = 'lin' PATH MEMBER = c( 24 )"
    ## [1] "BEST lin OBJECTIVE FUNCTION = 11.2388101135098"
    ## [1] "CURRNET METHOD: nonlin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'nonlin' , seasonal.factor =  c( 24 ) ...)"
    ## [1] "CURRENT nonlin OBJECTIVE FUNCTION = 12.2680884577359"
    ## [1] "BEST method = 'nonlin' PATH MEMBER = c( 24 )"
    ## [1] "BEST nonlin OBJECTIVE FUNCTION = 12.2680884577359"
    ## [1] "CURRNET METHOD: both"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'both' , seasonal.factor =  c( 24 ) ...)"
    ## [1] "CURRENT both OBJECTIVE FUNCTION = 11.1917020373399"
    ## [1] "BEST method = 'both' PATH MEMBER = c( 24 )"
    ## [1] "BEST both OBJECTIVE FUNCTION = 11.1917020373399"

![](man/figures/README-nns_forecasting-1.png)<!-- -->

    ## Package: healthyR.ai
    ## [1] "CURRNET METHOD: lin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 18 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 59.7985550538537"
    ## [1] "BEST method = 'lin' PATH MEMBER = c( 18 )"
    ## [1] "BEST lin OBJECTIVE FUNCTION = 59.7985550538537"
    ## [1] "CURRNET METHOD: nonlin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'nonlin' , seasonal.factor =  c( 18 ) ...)"
    ## [1] "CURRENT nonlin OBJECTIVE FUNCTION = 67.3484849376768"
    ## [1] "BEST method = 'nonlin' PATH MEMBER = c( 18 )"
    ## [1] "BEST nonlin OBJECTIVE FUNCTION = 67.3484849376768"
    ## [1] "CURRNET METHOD: both"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'both' , seasonal.factor =  c( 18 ) ...)"
    ## [1] "CURRENT both OBJECTIVE FUNCTION = 67.4590135181652"
    ## [1] "BEST method = 'both' PATH MEMBER = c( 18 )"
    ## [1] "BEST both OBJECTIVE FUNCTION = 67.4590135181652"

![](man/figures/README-nns_forecasting-2.png)<!-- -->

    ## Package: healthyR.data
    ## [1] "CURRNET METHOD: lin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 13 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 9.33450900593532"
    ## [1] "BEST method = 'lin' PATH MEMBER = c( 13 )"
    ## [1] "BEST lin OBJECTIVE FUNCTION = 9.33450900593532"
    ## [1] "CURRNET METHOD: nonlin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'nonlin' , seasonal.factor =  c( 13 ) ...)"
    ## [1] "CURRENT nonlin OBJECTIVE FUNCTION = 14.9925464305342"
    ## [1] "BEST method = 'nonlin' PATH MEMBER = c( 13 )"
    ## [1] "BEST nonlin OBJECTIVE FUNCTION = 14.9925464305342"
    ## [1] "CURRNET METHOD: both"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'both' , seasonal.factor =  c( 13 ) ...)"
    ## [1] "CURRENT both OBJECTIVE FUNCTION = 16.3385656027696"
    ## [1] "BEST method = 'both' PATH MEMBER = c( 13 )"
    ## [1] "BEST both OBJECTIVE FUNCTION = 16.3385656027696"

![](man/figures/README-nns_forecasting-3.png)<!-- -->

    ## Package: healthyR.ts
    ## [1] "CURRNET METHOD: lin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 22 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 7.48431249561943"
    ## [1] "BEST method = 'lin' PATH MEMBER = c( 22 )"
    ## [1] "BEST lin OBJECTIVE FUNCTION = 7.48431249561943"
    ## [1] "CURRNET METHOD: nonlin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'nonlin' , seasonal.factor =  c( 22 ) ...)"
    ## [1] "CURRENT nonlin OBJECTIVE FUNCTION = 10.1841499343805"
    ## [1] "BEST method = 'nonlin' PATH MEMBER = c( 22 )"
    ## [1] "BEST nonlin OBJECTIVE FUNCTION = 10.1841499343805"
    ## [1] "CURRNET METHOD: both"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'both' , seasonal.factor =  c( 22 ) ...)"
    ## [1] "CURRENT both OBJECTIVE FUNCTION = 9.6212445215573"
    ## [1] "BEST method = 'both' PATH MEMBER = c( 22 )"
    ## [1] "BEST both OBJECTIVE FUNCTION = 9.6212445215573"

![](man/figures/README-nns_forecasting-4.png)<!-- -->

    ## Package: healthyverse
    ## [1] "CURRNET METHOD: lin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 7 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 13.3164206877718"
    ## [1] "BEST method = 'lin' PATH MEMBER = c( 7 )"
    ## [1] "BEST lin OBJECTIVE FUNCTION = 13.3164206877718"
    ## [1] "CURRNET METHOD: nonlin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'nonlin' , seasonal.factor =  c( 7 ) ...)"
    ## [1] "CURRENT nonlin OBJECTIVE FUNCTION = 4.64737631098557"
    ## [1] "BEST method = 'nonlin' PATH MEMBER = c( 7 )"
    ## [1] "BEST nonlin OBJECTIVE FUNCTION = 4.64737631098557"
    ## [1] "CURRNET METHOD: both"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'both' , seasonal.factor =  c( 7 ) ...)"
    ## [1] "CURRENT both OBJECTIVE FUNCTION = 5.40724657391607"
    ## [1] "BEST method = 'both' PATH MEMBER = c( 7 )"
    ## [1] "BEST both OBJECTIVE FUNCTION = 5.40724657391607"

![](man/figures/README-nns_forecasting-5.png)<!-- -->

    ## Package: RandomWalker
    ## [1] "CURRNET METHOD: lin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 7 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 6.03874449194464"
    ## [1] "BEST method = 'lin' PATH MEMBER = c( 7 )"
    ## [1] "BEST lin OBJECTIVE FUNCTION = 6.03874449194464"
    ## [1] "CURRNET METHOD: nonlin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'nonlin' , seasonal.factor =  c( 7 ) ...)"
    ## [1] "CURRENT nonlin OBJECTIVE FUNCTION = 9.81812450375183"
    ## [1] "BEST method = 'nonlin' PATH MEMBER = c( 7 )"
    ## [1] "BEST nonlin OBJECTIVE FUNCTION = 9.81812450375183"
    ## [1] "CURRNET METHOD: both"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'both' , seasonal.factor =  c( 7 ) ...)"
    ## [1] "CURRENT both OBJECTIVE FUNCTION = 11.1393803911909"
    ## [1] "BEST method = 'both' PATH MEMBER = c( 7 )"
    ## [1] "BEST both OBJECTIVE FUNCTION = 11.1393803911909"

![](man/figures/README-nns_forecasting-6.png)<!-- -->

    ## Package: tidyAML
    ## [1] "CURRNET METHOD: lin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 15 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 6.44012877636231"
    ## [1] "BEST method = 'lin' PATH MEMBER = c( 15 )"
    ## [1] "BEST lin OBJECTIVE FUNCTION = 6.44012877636231"
    ## [1] "CURRNET METHOD: nonlin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'nonlin' , seasonal.factor =  c( 15 ) ...)"
    ## [1] "CURRENT nonlin OBJECTIVE FUNCTION = 6.29975457727983"
    ## [1] "BEST method = 'nonlin' PATH MEMBER = c( 15 )"
    ## [1] "BEST nonlin OBJECTIVE FUNCTION = 6.29975457727983"
    ## [1] "CURRNET METHOD: both"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'both' , seasonal.factor =  c( 15 ) ...)"
    ## [1] "CURRENT both OBJECTIVE FUNCTION = 8.08803752911988"
    ## [1] "BEST method = 'both' PATH MEMBER = c( 15 )"
    ## [1] "BEST both OBJECTIVE FUNCTION = 8.08803752911988"

![](man/figures/README-nns_forecasting-7.png)<!-- -->

    ## Package: TidyDensity
    ## [1] "CURRNET METHOD: lin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 16 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 18.8340562538515"
    ## [1] "BEST method = 'lin' PATH MEMBER = c( 16 )"
    ## [1] "BEST lin OBJECTIVE FUNCTION = 18.8340562538515"
    ## [1] "CURRNET METHOD: nonlin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'nonlin' , seasonal.factor =  c( 16 ) ...)"
    ## [1] "CURRENT nonlin OBJECTIVE FUNCTION = 17.6490720974051"
    ## [1] "BEST method = 'nonlin' PATH MEMBER = c( 16 )"
    ## [1] "BEST nonlin OBJECTIVE FUNCTION = 17.6490720974051"
    ## [1] "CURRNET METHOD: both"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'both' , seasonal.factor =  c( 16 ) ...)"
    ## [1] "CURRENT both OBJECTIVE FUNCTION = 24.0759369361103"
    ## [1] "BEST method = 'both' PATH MEMBER = c( 16 )"
    ## [1] "BEST both OBJECTIVE FUNCTION = 24.0759369361103"

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
    ## 1 healthyR.data <tibble [1,744 × 2]> <tibble [28 × 2]> <split [1716|28]>
    ## 2 healthyR      <tibble [1,735 × 2]> <tibble [28 × 2]> <split [1707|28]>
    ## 3 healthyR.ts   <tibble [1,681 × 2]> <tibble [28 × 2]> <split [1653|28]>
    ## 4 healthyverse  <tibble [1,652 × 2]> <tibble [28 × 2]> <split [1624|28]>
    ## 5 healthyR.ai   <tibble [1,477 × 2]> <tibble [28 × 2]> <split [1449|28]>
    ## 6 TidyDensity   <tibble [1,328 × 2]> <tibble [28 × 2]> <split [1300|28]>
    ## 7 tidyAML       <tibble [935 × 2]>   <tibble [28 × 2]> <split [907|28]> 
    ## 8 RandomWalker  <tibble [358 × 2]>   <tibble [28 × 2]> <split [330|28]> 
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
| healthyR.data | 1 | ARIMA | Test | 0.7948628 | 120.52229 | 0.9833093 | 163.34918 | 1.0268278 | 0.1020657 |
| healthyR.data | 2 | LM | Test | 0.7792462 | 199.76677 | 0.9639904 | 148.80975 | 0.8887495 | 0.1796877 |
| healthyR.data | 3 | NULL | NA | NA | NA | NA | NA | NA | NA |
| healthyR.data | 4 | NNAR | Test | 0.7851768 | 108.00580 | 0.9713271 | 178.24479 | 1.0104260 | 0.1702559 |
| healthyR | 1 | ARIMA | Test | 0.6535391 | 116.34529 | 0.7882474 | 170.81515 | 0.8499463 | 0.0895821 |
| healthyR | 2 | LM | Test | 0.6326400 | 124.98613 | 0.7630406 | 176.06113 | 0.7811223 | 0.1890520 |
| healthyR | 3 | NULL | NA | NA | NA | NA | NA | NA | NA |
| healthyR | 4 | NNAR | Test | 0.6600020 | 136.50705 | 0.7960425 | 172.10568 | 0.8135454 | 0.0085533 |
| healthyR.ts | 1 | ARIMA | Test | 0.8573891 | 195.43664 | 0.8913092 | 122.65653 | 1.0801033 | 0.0261028 |
| healthyR.ts | 2 | LM | Test | 0.7732971 | 141.51945 | 0.8038904 | 126.16242 | 1.0008414 | 0.0157986 |
| healthyR.ts | 3 | NULL | NA | NA | NA | NA | NA | NA | NA |
| healthyR.ts | 4 | NNAR | Test | 0.8485773 | 112.87485 | 0.8821488 | 174.14135 | 1.0617352 | 0.0270348 |
| healthyverse | 1 | ARIMA | Test | 0.8070616 | 99.60369 | 1.1455920 | 122.29597 | 0.9687907 | 0.0017174 |
| healthyverse | 2 | LM | Test | 0.6370866 | 112.93522 | 0.9043192 | 79.55840 | 0.7863231 | 0.0887389 |
| healthyverse | 3 | NULL | NA | NA | NA | NA | NA | NA | NA |
| healthyverse | 4 | NNAR | Test | 0.7853638 | 95.18655 | 1.1147929 | 114.75588 | 0.9552664 | 0.0156899 |
| healthyR.ai | 1 | ARIMA | Test | 0.9423197 | 99.57412 | 1.1885014 | 159.28960 | 1.4413251 | 0.0451452 |
| healthyR.ai | 2 | LM | Test | 0.9075745 | 104.04983 | 1.1446789 | 137.48258 | 1.4376598 | 0.2490352 |
| healthyR.ai | 3 | NULL | NA | NA | NA | NA | NA | NA | NA |
| healthyR.ai | 4 | NNAR | Test | 0.9747050 | 109.51699 | 1.2293473 | 150.99346 | 1.4806213 | 0.0419383 |
| TidyDensity | 1 | ARIMA | Test | 0.9135308 | 265.42702 | 1.0048921 | 110.03257 | 1.2529212 | 0.0373423 |
| TidyDensity | 2 | LM | Test | 0.8910148 | 255.85250 | 0.9801243 | 107.29032 | 1.2211332 | 0.4555717 |
| TidyDensity | 3 | NULL | NA | NA | NA | NA | NA | NA | NA |
| TidyDensity | 4 | NNAR | Test | 1.0015988 | 176.55295 | 1.1017676 | 138.60688 | 1.4280146 | 0.0826971 |
| tidyAML | 1 | ARIMA | Test | 0.7581918 | 95.29159 | 0.9859817 | 123.75700 | 0.9088675 | 0.0389706 |
| tidyAML | 2 | LM | Test | 0.6486828 | 125.25139 | 0.8435720 | 90.62273 | 0.7874280 | 0.0179857 |
| tidyAML | 3 | NULL | NA | NA | NA | NA | NA | NA | NA |
| tidyAML | 4 | NNAR | Test | 0.7281447 | 113.05448 | 0.9469074 | 112.39629 | 0.8707671 | 0.0086195 |
| RandomWalker | 1 | ARIMA | Test | 1.1242257 | 117.73629 | 0.6840408 | 163.91350 | 1.3899450 | 0.0624627 |
| RandomWalker | 2 | LM | Test | 1.0900600 | 127.24822 | 0.6632525 | 163.37679 | 1.2932346 | 0.0366779 |
| RandomWalker | 3 | NULL | NA | NA | NA | NA | NA | NA | NA |
| RandomWalker | 4 | NNAR | Test | 1.0659379 | 147.73430 | 0.6485753 | 138.62351 | 1.2603464 | 0.0859115 |

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
    ## 1 healthyR.data         2 LM          Test  0.779  200. 0.964 149.  0.889 0.180 
    ## 2 healthyR              2 LM          Test  0.633  125. 0.763 176.  0.781 0.189 
    ## 3 healthyR.ts           2 LM          Test  0.773  142. 0.804 126.  1.00  0.0158
    ## 4 healthyverse          2 LM          Test  0.637  113. 0.904  79.6 0.786 0.0887
    ## 5 healthyR.ai           2 LM          Test  0.908  104. 1.14  137.  1.44  0.249 
    ## 6 TidyDensity           2 LM          Test  0.891  256. 0.980 107.  1.22  0.456 
    ## 7 tidyAML               2 LM          Test  0.649  125. 0.844  90.6 0.787 0.0180
    ## 8 RandomWalker          4 NNAR        Test  1.07   148. 0.649 139.  1.26  0.0859

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
    ## 1 healthyR.data <tibble>     <tibble>     <split [1716|28]> <mdl_tm_t [1 × 5]>
    ## 2 healthyR      <tibble>     <tibble>     <split [1707|28]> <mdl_tm_t [1 × 5]>
    ## 3 healthyR.ts   <tibble>     <tibble>     <split [1653|28]> <mdl_tm_t [1 × 5]>
    ## 4 healthyverse  <tibble>     <tibble>     <split [1624|28]> <mdl_tm_t [1 × 5]>
    ## 5 healthyR.ai   <tibble>     <tibble>     <split [1449|28]> <mdl_tm_t [1 × 5]>
    ## 6 TidyDensity   <tibble>     <tibble>     <split [1300|28]> <mdl_tm_t [1 × 5]>
    ## 7 tidyAML       <tibble>     <tibble>     <split [907|28]>  <mdl_tm_t [1 × 5]>
    ## 8 RandomWalker  <tibble>     <tibble>     <split [330|28]>  <mdl_tm_t [1 × 5]>

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
