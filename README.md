Time Series Analysis, Modeling and Forecasting of the Healthyverse
Packages
================
Steven P. Sanderson II, MPH - Date:
09 September, 2025

This analysis follows a *Nested Modeltime Workflow*.

## Get Data

``` r
glimpse(downloads_tbl)
```

    ## Rows: 150,918
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

The last day in the data set is 2025-09-07 23:35:42, the file was
birthed on: 2024-08-07 07:35:44.428716, and at report knit time is -9516
hours old. Happy analyzing!

Now that we have our data lets take a look at it using the `skimr`
package.

``` r
skim(downloads_tbl)
```

|                                                  |               |
|:-------------------------------------------------|:--------------|
| Name                                             | downloads_tbl |
| Number of rows                                   | 150918        |
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
| r_version     |    109603 |          0.27 |   5 |   5 |     0 |       48 |          0 |
| r_arch        |    109603 |          0.27 |   3 |   7 |     0 |        5 |          0 |
| r_os          |    109603 |          0.27 |   7 |  15 |     0 |       23 |          0 |
| package       |         0 |          1.00 |   7 |  13 |     0 |        8 |          0 |
| version       |         0 |          1.00 |   5 |  17 |     0 |       62 |          0 |
| country       |     12879 |          0.91 |   2 |   2 |     0 |      165 |          0 |

**Variable type: Date**

| skim_variable | n_missing | complete_rate | min | max | median | n_unique |
|:---|---:|---:|:---|:---|:---|---:|
| date | 0 | 1 | 2020-11-23 | 2025-09-07 | 2023-08-24 | 1743 |

**Variable type: numeric**

| skim_variable | n_missing | complete_rate | mean | sd | p0 | p25 | p50 | p75 | p100 | hist |
|:---|---:|---:|---:|---:|---:|---:|---:|---:|---:|:---|
| size | 0 | 1 | 1130156.47 | 1504236.42 | 355 | 14701 | 306707 | 2365870 | 5677952 | ▇▁▂▁▁ |
| ip_id | 0 | 1 | 11251.97 | 21570.74 | 1 | 281 | 3058 | 12119 | 299146 | ▇▁▁▁▁ |

**Variable type: POSIXct**

| skim_variable | n_missing | complete_rate | min | max | median | n_unique |
|:---|---:|---:|:---|:---|:---|---:|
| date_time | 0 | 1 | 2020-11-23 09:00:41 | 2025-09-07 23:35:42 | 2023-08-24 18:03:48 | 93767 |

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
    ## -147.24  -36.07  -11.22   26.79  817.95 
    ## 
    ## Coefficients:
    ##                                                      Estimate Std. Error
    ## (Intercept)                                        -1.781e+02  6.295e+01
    ## date                                                1.095e-02  3.333e-03
    ## lag(value, 1)                                       1.063e-01  2.374e-02
    ## lag(value, 7)                                       9.067e-02  2.453e-02
    ## lag(value, 14)                                      8.109e-02  2.457e-02
    ## lag(value, 21)                                      6.445e-02  2.465e-02
    ## lag(value, 28)                                      7.014e-02  2.456e-02
    ## lag(value, 35)                                      6.894e-02  2.466e-02
    ## lag(value, 42)                                      5.881e-02  2.479e-02
    ## lag(value, 49)                                      6.490e-02  2.466e-02
    ## month(date, label = TRUE).L                        -9.681e+00  5.102e+00
    ## month(date, label = TRUE).Q                         3.173e+00  4.988e+00
    ## month(date, label = TRUE).C                        -1.346e+01  5.076e+00
    ## month(date, label = TRUE)^4                        -6.838e+00  5.069e+00
    ## month(date, label = TRUE)^5                        -1.098e+01  5.028e+00
    ## month(date, label = TRUE)^6                        -4.245e+00  5.110e+00
    ## month(date, label = TRUE)^7                        -7.623e+00  4.990e+00
    ## month(date, label = TRUE)^8                        -3.512e+00  4.979e+00
    ## month(date, label = TRUE)^9                         5.595e+00  4.968e+00
    ## month(date, label = TRUE)^10                        3.767e+00  4.901e+00
    ## month(date, label = TRUE)^11                       -2.910e+00  4.814e+00
    ## fourier_vec(date, type = "sin", K = 1, period = 7) -1.163e+01  2.269e+00
    ## fourier_vec(date, type = "cos", K = 1, period = 7)  7.669e+00  2.380e+00
    ##                                                    t value Pr(>|t|)    
    ## (Intercept)                                         -2.829 0.004723 ** 
    ## date                                                 3.286 0.001036 ** 
    ## lag(value, 1)                                        4.479 8.01e-06 ***
    ## lag(value, 7)                                        3.696 0.000226 ***
    ## lag(value, 14)                                       3.300 0.000987 ***
    ## lag(value, 21)                                       2.614 0.009024 ** 
    ## lag(value, 28)                                       2.856 0.004339 ** 
    ## lag(value, 35)                                       2.796 0.005234 ** 
    ## lag(value, 42)                                       2.373 0.017775 *  
    ## lag(value, 49)                                       2.632 0.008571 ** 
    ## month(date, label = TRUE).L                         -1.897 0.057939 .  
    ## month(date, label = TRUE).Q                          0.636 0.524794    
    ## month(date, label = TRUE).C                         -2.651 0.008103 ** 
    ## month(date, label = TRUE)^4                         -1.349 0.177550    
    ## month(date, label = TRUE)^5                         -2.183 0.029140 *  
    ## month(date, label = TRUE)^6                         -0.831 0.406260    
    ## month(date, label = TRUE)^7                         -1.528 0.126811    
    ## month(date, label = TRUE)^8                         -0.705 0.480755    
    ## month(date, label = TRUE)^9                          1.126 0.260159    
    ## month(date, label = TRUE)^10                         0.769 0.442262    
    ## month(date, label = TRUE)^11                        -0.605 0.545593    
    ## fourier_vec(date, type = "sin", K = 1, period = 7)  -5.125 3.31e-07 ***
    ## fourier_vec(date, type = "cos", K = 1, period = 7)   3.222 0.001295 ** 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 58.77 on 1671 degrees of freedom
    ##   (49 observations deleted due to missingness)
    ## Multiple R-squared:  0.2322, Adjusted R-squared:  0.2221 
    ## F-statistic: 22.97 on 22 and 1671 DF,  p-value: < 2.2e-16

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
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 8.9552767925964"
    ## [1] "BEST method = 'lin' PATH MEMBER = c( 24 )"
    ## [1] "BEST lin OBJECTIVE FUNCTION = 8.9552767925964"
    ## [1] "CURRNET METHOD: nonlin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'nonlin' , seasonal.factor =  c( 24 ) ...)"
    ## [1] "CURRENT nonlin OBJECTIVE FUNCTION = 12.0888144741933"
    ## [1] "BEST method = 'nonlin' PATH MEMBER = c( 24 )"
    ## [1] "BEST nonlin OBJECTIVE FUNCTION = 12.0888144741933"
    ## [1] "CURRNET METHOD: both"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'both' , seasonal.factor =  c( 24 ) ...)"
    ## [1] "CURRENT both OBJECTIVE FUNCTION = 9.05915369504169"
    ## [1] "BEST method = 'both' PATH MEMBER = c( 24 )"
    ## [1] "BEST both OBJECTIVE FUNCTION = 9.05915369504169"

![](man/figures/README-nns_forecasting-1.png)<!-- -->

    ## Package: healthyR.ai
    ## [1] "CURRNET METHOD: lin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 13 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 11.6839394132731"
    ## [1] "BEST method = 'lin' PATH MEMBER = c( 13 )"
    ## [1] "BEST lin OBJECTIVE FUNCTION = 11.6839394132731"
    ## [1] "CURRNET METHOD: nonlin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'nonlin' , seasonal.factor =  c( 13 ) ...)"
    ## [1] "CURRENT nonlin OBJECTIVE FUNCTION = 6.87157795156845"
    ## [1] "BEST method = 'nonlin' PATH MEMBER = c( 13 )"
    ## [1] "BEST nonlin OBJECTIVE FUNCTION = 6.87157795156845"
    ## [1] "CURRNET METHOD: both"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'both' , seasonal.factor =  c( 13 ) ...)"
    ## [1] "CURRENT both OBJECTIVE FUNCTION = 12.5753799943969"
    ## [1] "BEST method = 'both' PATH MEMBER = c( 13 )"
    ## [1] "BEST both OBJECTIVE FUNCTION = 12.5753799943969"

![](man/figures/README-nns_forecasting-2.png)<!-- -->

    ## Package: healthyR.data
    ## [1] "CURRNET METHOD: lin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 13 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 6.08229264907597"
    ## [1] "BEST method = 'lin' PATH MEMBER = c( 13 )"
    ## [1] "BEST lin OBJECTIVE FUNCTION = 6.08229264907597"
    ## [1] "CURRNET METHOD: nonlin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'nonlin' , seasonal.factor =  c( 13 ) ...)"
    ## [1] "CURRENT nonlin OBJECTIVE FUNCTION = 7.79667313816308"
    ## [1] "BEST method = 'nonlin' PATH MEMBER = c( 13 )"
    ## [1] "BEST nonlin OBJECTIVE FUNCTION = 7.79667313816308"
    ## [1] "CURRNET METHOD: both"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'both' , seasonal.factor =  c( 13 ) ...)"
    ## [1] "CURRENT both OBJECTIVE FUNCTION = 7.71349406451732"
    ## [1] "BEST method = 'both' PATH MEMBER = c( 13 )"
    ## [1] "BEST both OBJECTIVE FUNCTION = 7.71349406451732"

![](man/figures/README-nns_forecasting-3.png)<!-- -->

    ## Package: healthyR.ts
    ## [1] "CURRNET METHOD: lin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 22 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 7.65358404405505"
    ## [1] "BEST method = 'lin' PATH MEMBER = c( 22 )"
    ## [1] "BEST lin OBJECTIVE FUNCTION = 7.65358404405505"
    ## [1] "CURRNET METHOD: nonlin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'nonlin' , seasonal.factor =  c( 22 ) ...)"
    ## [1] "CURRENT nonlin OBJECTIVE FUNCTION = 3.44156780782819"
    ## [1] "BEST method = 'nonlin' PATH MEMBER = c( 22 )"
    ## [1] "BEST nonlin OBJECTIVE FUNCTION = 3.44156780782819"
    ## [1] "CURRNET METHOD: both"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'both' , seasonal.factor =  c( 22 ) ...)"
    ## [1] "CURRENT both OBJECTIVE FUNCTION = 3.17729648809361"
    ## [1] "BEST method = 'both' PATH MEMBER = c( 22 )"
    ## [1] "BEST both OBJECTIVE FUNCTION = 3.17729648809361"

![](man/figures/README-nns_forecasting-4.png)<!-- -->

    ## Package: healthyverse
    ## [1] "CURRNET METHOD: lin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 7 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 14.9471657498516"
    ## [1] "BEST method = 'lin' PATH MEMBER = c( 7 )"
    ## [1] "BEST lin OBJECTIVE FUNCTION = 14.9471657498516"
    ## [1] "CURRNET METHOD: nonlin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'nonlin' , seasonal.factor =  c( 7 ) ...)"
    ## [1] "CURRENT nonlin OBJECTIVE FUNCTION = 7.61005519025815"
    ## [1] "BEST method = 'nonlin' PATH MEMBER = c( 7 )"
    ## [1] "BEST nonlin OBJECTIVE FUNCTION = 7.61005519025815"
    ## [1] "CURRNET METHOD: both"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'both' , seasonal.factor =  c( 7 ) ...)"
    ## [1] "CURRENT both OBJECTIVE FUNCTION = 11.3357451452868"
    ## [1] "BEST method = 'both' PATH MEMBER = c( 7 )"
    ## [1] "BEST both OBJECTIVE FUNCTION = 11.3357451452868"

![](man/figures/README-nns_forecasting-5.png)<!-- -->

    ## Package: RandomWalker
    ## [1] "CURRNET METHOD: lin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 1 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 126.544841296215"
    ## [1] "BEST method = 'lin' PATH MEMBER = c( 1 )"
    ## [1] "BEST lin OBJECTIVE FUNCTION = 126.544841296215"
    ## [1] "CURRNET METHOD: nonlin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'nonlin' , seasonal.factor =  c( 1 ) ...)"
    ## [1] "CURRENT nonlin OBJECTIVE FUNCTION = 120.726723833397"
    ## [1] "BEST method = 'nonlin' PATH MEMBER = c( 1 )"
    ## [1] "BEST nonlin OBJECTIVE FUNCTION = 120.726723833397"
    ## [1] "CURRNET METHOD: both"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'both' , seasonal.factor =  c( 1 ) ...)"
    ## [1] "CURRENT both OBJECTIVE FUNCTION = 360.534882744396"
    ## [1] "BEST method = 'both' PATH MEMBER = c( 1 )"
    ## [1] "BEST both OBJECTIVE FUNCTION = 360.534882744396"

![](man/figures/README-nns_forecasting-6.png)<!-- -->

    ## Package: tidyAML
    ## [1] "CURRNET METHOD: lin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 2 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 129.481687532028"
    ## [1] "BEST method = 'lin' PATH MEMBER = c( 2 )"
    ## [1] "BEST lin OBJECTIVE FUNCTION = 129.481687532028"
    ## [1] "CURRNET METHOD: nonlin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'nonlin' , seasonal.factor =  c( 2 ) ...)"
    ## [1] "CURRENT nonlin OBJECTIVE FUNCTION = 14.595026824739"
    ## [1] "BEST method = 'nonlin' PATH MEMBER = c( 2 )"
    ## [1] "BEST nonlin OBJECTIVE FUNCTION = 14.595026824739"
    ## [1] "CURRNET METHOD: both"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'both' , seasonal.factor =  c( 2 ) ...)"
    ## [1] "CURRENT both OBJECTIVE FUNCTION = 15.9051488697252"
    ## [1] "BEST method = 'both' PATH MEMBER = c( 2 )"
    ## [1] "BEST both OBJECTIVE FUNCTION = 15.9051488697252"

![](man/figures/README-nns_forecasting-7.png)<!-- -->

    ## Package: TidyDensity
    ## [1] "CURRNET METHOD: lin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 9 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 17.2375978709815"
    ## [1] "BEST method = 'lin' PATH MEMBER = c( 9 )"
    ## [1] "BEST lin OBJECTIVE FUNCTION = 17.2375978709815"
    ## [1] "CURRNET METHOD: nonlin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'nonlin' , seasonal.factor =  c( 9 ) ...)"
    ## [1] "CURRENT nonlin OBJECTIVE FUNCTION = 15.8104134005434"
    ## [1] "BEST method = 'nonlin' PATH MEMBER = c( 9 )"
    ## [1] "BEST nonlin OBJECTIVE FUNCTION = 15.8104134005434"
    ## [1] "CURRNET METHOD: both"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'both' , seasonal.factor =  c( 9 ) ...)"
    ## [1] "CURRENT both OBJECTIVE FUNCTION = 17.3408213989762"
    ## [1] "BEST method = 'both' PATH MEMBER = c( 9 )"
    ## [1] "BEST both OBJECTIVE FUNCTION = 17.3408213989762"

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
    ## 1 healthyR.data <tibble [1,735 × 2]> <tibble [28 × 2]> <split [1707|28]>
    ## 2 healthyR      <tibble [1,726 × 2]> <tibble [28 × 2]> <split [1698|28]>
    ## 3 healthyR.ts   <tibble [1,672 × 2]> <tibble [28 × 2]> <split [1644|28]>
    ## 4 healthyverse  <tibble [1,643 × 2]> <tibble [28 × 2]> <split [1615|28]>
    ## 5 healthyR.ai   <tibble [1,468 × 2]> <tibble [28 × 2]> <split [1440|28]>
    ## 6 TidyDensity   <tibble [1,319 × 2]> <tibble [28 × 2]> <split [1291|28]>
    ## 7 tidyAML       <tibble [926 × 2]>   <tibble [28 × 2]> <split [898|28]> 
    ## 8 RandomWalker  <tibble [349 × 2]>   <tibble [28 × 2]> <split [321|28]> 
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
| healthyR.data | 1 | ARIMA | Test | 0.8426767 | 127.52481 | 1.0287249 | 160.28953 | 1.0065555 | 0.0079960 |
| healthyR.data | 2 | LM | Test | 0.8941997 | 191.62638 | 1.0916233 | 152.51250 | 1.0182284 | 0.1611050 |
| healthyR.data | 3 | NULL | NA | NA | NA | NA | NA | NA | NA |
| healthyR.data | 4 | NNAR | Test | 0.8107353 | 100.61609 | 0.9897314 | 183.10194 | 0.9982203 | 0.0019347 |
| healthyR | 1 | ARIMA | Test | 0.6659630 | 105.46053 | 0.8348659 | 169.83244 | 0.8191942 | 0.0029507 |
| healthyR | 2 | LM | Test | 0.6821286 | 112.11783 | 0.8551315 | 174.68849 | 0.8114274 | 0.1035413 |
| healthyR | 3 | NULL | NA | NA | NA | NA | NA | NA | NA |
| healthyR | 4 | NNAR | Test | 0.6582019 | 111.70376 | 0.8251364 | 157.34955 | 0.7989028 | 0.0537168 |
| healthyR.ts | 1 | ARIMA | Test | 0.8302963 | 142.00948 | 0.9453313 | 123.48903 | 1.0494157 | 0.0077396 |
| healthyR.ts | 2 | LM | Test | 0.8655259 | 133.16167 | 0.9854417 | 132.76118 | 1.0545217 | 0.1772334 |
| healthyR.ts | 3 | NULL | NA | NA | NA | NA | NA | NA | NA |
| healthyR.ts | 4 | NNAR | Test | 0.8612081 | 87.42003 | 0.9805258 | 157.88271 | 1.0801306 | 0.0304579 |
| healthyverse | 1 | ARIMA | Test | 0.7619207 | 131.47750 | 1.1727421 | 103.05855 | 0.9002618 | 0.2417428 |
| healthyverse | 2 | LM | Test | 0.7939527 | 167.26367 | 1.2220455 | 99.30881 | 0.9227284 | 0.4003052 |
| healthyverse | 3 | NULL | NA | NA | NA | NA | NA | NA | NA |
| healthyverse | 4 | NNAR | Test | 0.7938664 | 110.22277 | 1.2219127 | 116.25526 | 0.9578655 | 0.2033114 |
| healthyR.ai | 1 | ARIMA | Test | 0.6850467 | 84.54703 | 1.0876992 | 143.14192 | 0.8265603 | 0.1947200 |
| healthyR.ai | 2 | LM | Test | 0.7354183 | 93.15843 | 1.1676779 | 144.45571 | 0.8717839 | 0.2546153 |
| healthyR.ai | 3 | NULL | NA | NA | NA | NA | NA | NA | NA |
| healthyR.ai | 4 | NNAR | Test | 0.6888056 | 86.24269 | 1.0936675 | 145.97591 | 0.8434572 | 0.0623689 |
| TidyDensity | 1 | ARIMA | Test | 0.4857572 | 235.95346 | 0.8125713 | 87.22950 | 0.6214379 | 0.1330548 |
| TidyDensity | 2 | LM | Test | 0.5364293 | 245.92336 | 0.8973352 | 92.20318 | 0.6556847 | 0.0208821 |
| TidyDensity | 3 | NULL | NA | NA | NA | NA | NA | NA | NA |
| TidyDensity | 4 | NNAR | Test | 0.5053273 | 137.72727 | 0.8453079 | 109.74415 | 0.6805769 | 0.0793760 |
| tidyAML | 1 | ARIMA | Test | 0.6450618 | 213.16429 | 1.2752408 | 114.12181 | 0.7748191 | 0.0156459 |
| tidyAML | 2 | LM | Test | 0.6525595 | 274.69780 | 1.2900633 | 107.57965 | 0.7685128 | 0.4150717 |
| tidyAML | 3 | NULL | NA | NA | NA | NA | NA | NA | NA |
| tidyAML | 4 | NNAR | Test | 0.6367590 | 218.75326 | 1.2588269 | 111.41655 | 0.7645486 | 0.0007219 |
| RandomWalker | 1 | ARIMA | Test | 1.0980332 | 122.35208 | 0.6803580 | 174.30804 | 1.3070925 | 0.0004890 |
| RandomWalker | 2 | LM | Test | 1.0706294 | 120.85628 | 0.6633782 | 175.81788 | 1.2663354 | 0.0058968 |
| RandomWalker | 3 | NULL | NA | NA | NA | NA | NA | NA | NA |
| RandomWalker | 4 | NNAR | Test | 1.2366872 | 190.82965 | 0.7662701 | 156.43693 | 1.3915555 | 0.0000854 |

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
    ## 1 healthyR.da…         4 NNAR        Test  0.811 101.  0.990 183.  0.998 1.93e-3
    ## 2 healthyR             4 NNAR        Test  0.658 112.  0.825 157.  0.799 5.37e-2
    ## 3 healthyR.ts          1 ARIMA       Test  0.830 142.  0.945 123.  1.05  7.74e-3
    ## 4 healthyverse         1 ARIMA       Test  0.762 131.  1.17  103.  0.900 2.42e-1
    ## 5 healthyR.ai          1 ARIMA       Test  0.685  84.5 1.09  143.  0.827 1.95e-1
    ## 6 TidyDensity          1 ARIMA       Test  0.486 236.  0.813  87.2 0.621 1.33e-1
    ## 7 tidyAML              4 NNAR        Test  0.637 219.  1.26  111.  0.765 7.22e-4
    ## 8 RandomWalker         2 LM          Test  1.07  121.  0.663 176.  1.27  5.90e-3

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
    ## 1 healthyR.data <tibble>     <tibble>     <split [1707|28]> <mdl_tm_t [1 × 5]>
    ## 2 healthyR      <tibble>     <tibble>     <split [1698|28]> <mdl_tm_t [1 × 5]>
    ## 3 healthyR.ts   <tibble>     <tibble>     <split [1644|28]> <mdl_tm_t [1 × 5]>
    ## 4 healthyverse  <tibble>     <tibble>     <split [1615|28]> <mdl_tm_t [1 × 5]>
    ## 5 healthyR.ai   <tibble>     <tibble>     <split [1440|28]> <mdl_tm_t [1 × 5]>
    ## 6 TidyDensity   <tibble>     <tibble>     <split [1291|28]> <mdl_tm_t [1 × 5]>
    ## 7 tidyAML       <tibble>     <tibble>     <split [898|28]>  <mdl_tm_t [1 × 5]>
    ## 8 RandomWalker  <tibble>     <tibble>     <split [321|28]>  <mdl_tm_t [1 × 5]>

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
