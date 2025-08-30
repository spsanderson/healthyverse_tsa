Time Series Analysis, Modeling and Forecasting of the Healthyverse
Packages
================
Steven P. Sanderson II, MPH - Date:
30 August, 2025

This analysis follows a *Nested Modeltime Workflow*.

## Get Data

``` r
glimpse(downloads_tbl)
```

    ## Rows: 150,468
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

The last day in the data set is 2025-08-28 23:02:52, the file was
birthed on: 2022-07-02 23:58:17.511888, and at report knit time is
-2.766708^{4} hours old. Happy analyzing!

Now that we have our data lets take a look at it using the `skimr`
package.

``` r
skim(downloads_tbl)
```

|                                                  |               |
|:-------------------------------------------------|:--------------|
| Name                                             | downloads_tbl |
| Number of rows                                   | 150468        |
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
| r_version     |    109262 |          0.27 |   5 |   5 |     0 |       48 |          0 |
| r_arch        |    109262 |          0.27 |   3 |   7 |     0 |        5 |          0 |
| r_os          |    109262 |          0.27 |   7 |  15 |     0 |       23 |          0 |
| package       |         0 |          1.00 |   7 |  13 |     0 |        8 |          0 |
| version       |         0 |          1.00 |   5 |  17 |     0 |       61 |          0 |
| country       |     12824 |          0.91 |   2 |   2 |     0 |      165 |          0 |

**Variable type: Date**

| skim_variable | n_missing | complete_rate | min | max | median | n_unique |
|:---|---:|---:|:---|:---|:---|---:|
| date | 0 | 1 | 2020-11-23 | 2025-08-28 | 2023-08-23 | 1738 |

**Variable type: numeric**

| skim_variable | n_missing | complete_rate | mean | sd | p0 | p25 | p50 | p75 | p100 | hist |
|:---|---:|---:|---:|---:|---:|---:|---:|---:|---:|:---|
| size | 0 | 1 | 1130721.46 | 1505063.01 | 355 | 14701 | 306596 | 2365957 | 5677952 | ▇▁▂▁▁ |
| ip_id | 0 | 1 | 11246.71 | 21567.96 | 1 | 286 | 3058 | 12115 | 299146 | ▇▁▁▁▁ |

**Variable type: POSIXct**

| skim_variable | n_missing | complete_rate | min | max | median | n_unique |
|:---|---:|---:|:---|:---|:---|---:|
| date_time | 0 | 1 | 2020-11-23 09:00:41 | 2025-08-28 23:02:52 | 2023-08-23 19:23:44 | 93450 |

**Variable type: Timespan**

| skim_variable | n_missing | complete_rate | min | max | median | n_unique |
|:--------------|----------:|--------------:|----:|----:|-------:|---------:|
| time          |         0 |             1 |   0 |  59 |     28 |       60 |

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
    ## -147.56  -36.08  -11.07   26.89  817.72 
    ## 
    ## Coefficients:
    ##                                                      Estimate Std. Error
    ## (Intercept)                                        -1.798e+02  6.319e+01
    ## date                                                1.104e-02  3.346e-03
    ## lag(value, 1)                                       1.078e-01  2.377e-02
    ## lag(value, 7)                                       9.044e-02  2.454e-02
    ## lag(value, 14)                                      8.025e-02  2.459e-02
    ## lag(value, 21)                                      6.648e-02  2.468e-02
    ## lag(value, 28)                                      6.943e-02  2.462e-02
    ## lag(value, 35)                                      6.722e-02  2.469e-02
    ## lag(value, 42)                                      5.879e-02  2.480e-02
    ## lag(value, 49)                                      6.676e-02  2.468e-02
    ## month(date, label = TRUE).L                        -9.583e+00  5.103e+00
    ## month(date, label = TRUE).Q                         3.106e+00  4.987e+00
    ## month(date, label = TRUE).C                        -1.364e+01  5.088e+00
    ## month(date, label = TRUE)^4                        -6.911e+00  5.069e+00
    ## month(date, label = TRUE)^5                        -1.083e+01  5.030e+00
    ## month(date, label = TRUE)^6                        -4.051e+00  5.125e+00
    ## month(date, label = TRUE)^7                        -7.520e+00  4.990e+00
    ## month(date, label = TRUE)^8                        -3.654e+00  4.984e+00
    ## month(date, label = TRUE)^9                         5.352e+00  4.991e+00
    ## month(date, label = TRUE)^10                        3.544e+00  4.918e+00
    ## month(date, label = TRUE)^11                       -3.004e+00  4.816e+00
    ## fourier_vec(date, type = "sin", K = 1, period = 7) -1.157e+01  2.272e+00
    ## fourier_vec(date, type = "cos", K = 1, period = 7)  7.685e+00  2.384e+00
    ##                                                    t value Pr(>|t|)    
    ## (Intercept)                                         -2.846 0.004478 ** 
    ## date                                                 3.299 0.000990 ***
    ## lag(value, 1)                                        4.535 6.17e-06 ***
    ## lag(value, 7)                                        3.685 0.000236 ***
    ## lag(value, 14)                                       3.263 0.001123 ** 
    ## lag(value, 21)                                       2.694 0.007131 ** 
    ## lag(value, 28)                                       2.820 0.004859 ** 
    ## lag(value, 35)                                       2.723 0.006546 ** 
    ## lag(value, 42)                                       2.371 0.017865 *  
    ## lag(value, 49)                                       2.705 0.006908 ** 
    ## month(date, label = TRUE).L                         -1.878 0.060573 .  
    ## month(date, label = TRUE).Q                          0.623 0.533578    
    ## month(date, label = TRUE).C                         -2.681 0.007419 ** 
    ## month(date, label = TRUE)^4                         -1.363 0.172979    
    ## month(date, label = TRUE)^5                         -2.154 0.031411 *  
    ## month(date, label = TRUE)^6                         -0.790 0.429424    
    ## month(date, label = TRUE)^7                         -1.507 0.131975    
    ## month(date, label = TRUE)^8                         -0.733 0.463578    
    ## month(date, label = TRUE)^9                          1.072 0.283745    
    ## month(date, label = TRUE)^10                         0.721 0.471273    
    ## month(date, label = TRUE)^11                        -0.624 0.532784    
    ## fourier_vec(date, type = "sin", K = 1, period = 7)  -5.092 3.95e-07 ***
    ## fourier_vec(date, type = "cos", K = 1, period = 7)   3.224 0.001288 ** 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 58.74 on 1666 degrees of freedom
    ##   (49 observations deleted due to missingness)
    ## Multiple R-squared:  0.2332, Adjusted R-squared:  0.2231 
    ## F-statistic: 23.03 on 22 and 1666 DF,  p-value: < 2.2e-16

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
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 5 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 14.6906492904139"
    ## [1] "BEST method = 'lin' PATH MEMBER = c( 5 )"
    ## [1] "BEST lin OBJECTIVE FUNCTION = 14.6906492904139"
    ## [1] "CURRNET METHOD: nonlin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'nonlin' , seasonal.factor =  c( 5 ) ...)"
    ## [1] "CURRENT nonlin OBJECTIVE FUNCTION = 7.5295076144313"
    ## [1] "BEST method = 'nonlin' PATH MEMBER = c( 5 )"
    ## [1] "BEST nonlin OBJECTIVE FUNCTION = 7.5295076144313"
    ## [1] "CURRNET METHOD: both"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'both' , seasonal.factor =  c( 5 ) ...)"
    ## [1] "CURRENT both OBJECTIVE FUNCTION = 10.6728223507624"
    ## [1] "BEST method = 'both' PATH MEMBER = c( 5 )"
    ## [1] "BEST both OBJECTIVE FUNCTION = 10.6728223507624"

![](man/figures/README-nns_forecasting-1.png)<!-- -->

    ## Package: healthyR.ai
    ## [1] "CURRNET METHOD: lin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 13 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 13.7076176202393"
    ## [1] "BEST method = 'lin' PATH MEMBER = c( 13 )"
    ## [1] "BEST lin OBJECTIVE FUNCTION = 13.7076176202393"
    ## [1] "CURRNET METHOD: nonlin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'nonlin' , seasonal.factor =  c( 13 ) ...)"
    ## [1] "CURRENT nonlin OBJECTIVE FUNCTION = 4.21848747422567"
    ## [1] "BEST method = 'nonlin' PATH MEMBER = c( 13 )"
    ## [1] "BEST nonlin OBJECTIVE FUNCTION = 4.21848747422567"
    ## [1] "CURRNET METHOD: both"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'both' , seasonal.factor =  c( 13 ) ...)"
    ## [1] "CURRENT both OBJECTIVE FUNCTION = 7.46243482405538"
    ## [1] "BEST method = 'both' PATH MEMBER = c( 13 )"
    ## [1] "BEST both OBJECTIVE FUNCTION = 7.46243482405538"

![](man/figures/README-nns_forecasting-2.png)<!-- -->

    ## Package: healthyR.data
    ## [1] "CURRNET METHOD: lin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 13 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 6.3527464458303"
    ## [1] "BEST method = 'lin' PATH MEMBER = c( 13 )"
    ## [1] "BEST lin OBJECTIVE FUNCTION = 6.3527464458303"
    ## [1] "CURRNET METHOD: nonlin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'nonlin' , seasonal.factor =  c( 13 ) ...)"
    ## [1] "CURRENT nonlin OBJECTIVE FUNCTION = 3.83763335971404"
    ## [1] "BEST method = 'nonlin' PATH MEMBER = c( 13 )"
    ## [1] "BEST nonlin OBJECTIVE FUNCTION = 3.83763335971404"
    ## [1] "CURRNET METHOD: both"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'both' , seasonal.factor =  c( 13 ) ...)"
    ## [1] "CURRENT both OBJECTIVE FUNCTION = 4.3294926019058"
    ## [1] "BEST method = 'both' PATH MEMBER = c( 13 )"
    ## [1] "BEST both OBJECTIVE FUNCTION = 4.3294926019058"

![](man/figures/README-nns_forecasting-3.png)<!-- -->

    ## Package: healthyR.ts
    ## [1] "CURRNET METHOD: lin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 22 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 9.99499841297919"
    ## [1] "BEST method = 'lin' PATH MEMBER = c( 22 )"
    ## [1] "BEST lin OBJECTIVE FUNCTION = 9.99499841297919"
    ## [1] "CURRNET METHOD: nonlin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'nonlin' , seasonal.factor =  c( 22 ) ...)"
    ## [1] "CURRENT nonlin OBJECTIVE FUNCTION = 9.45112542880849"
    ## [1] "BEST method = 'nonlin' PATH MEMBER = c( 22 )"
    ## [1] "BEST nonlin OBJECTIVE FUNCTION = 9.45112542880849"
    ## [1] "CURRNET METHOD: both"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'both' , seasonal.factor =  c( 22 ) ...)"
    ## [1] "CURRENT both OBJECTIVE FUNCTION = 7.5785955622578"
    ## [1] "BEST method = 'both' PATH MEMBER = c( 22 )"
    ## [1] "BEST both OBJECTIVE FUNCTION = 7.5785955622578"

![](man/figures/README-nns_forecasting-4.png)<!-- -->

    ## Package: healthyverse
    ## [1] "CURRNET METHOD: lin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 7 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 17.7207551525204"
    ## [1] "BEST method = 'lin' PATH MEMBER = c( 7 )"
    ## [1] "BEST lin OBJECTIVE FUNCTION = 17.7207551525204"
    ## [1] "CURRNET METHOD: nonlin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'nonlin' , seasonal.factor =  c( 7 ) ...)"
    ## [1] "CURRENT nonlin OBJECTIVE FUNCTION = 4.48347225001222"
    ## [1] "BEST method = 'nonlin' PATH MEMBER = c( 7 )"
    ## [1] "BEST nonlin OBJECTIVE FUNCTION = 4.48347225001222"
    ## [1] "CURRNET METHOD: both"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'both' , seasonal.factor =  c( 7 ) ...)"
    ## [1] "CURRENT both OBJECTIVE FUNCTION = 7.24065321720259"
    ## [1] "BEST method = 'both' PATH MEMBER = c( 7 )"
    ## [1] "BEST both OBJECTIVE FUNCTION = 7.24065321720259"

![](man/figures/README-nns_forecasting-5.png)<!-- -->

    ## Package: RandomWalker
    ## [1] "CURRNET METHOD: lin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 13 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 7.17858822409597"
    ## [1] "BEST method = 'lin' PATH MEMBER = c( 13 )"
    ## [1] "BEST lin OBJECTIVE FUNCTION = 7.17858822409597"
    ## [1] "CURRNET METHOD: nonlin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'nonlin' , seasonal.factor =  c( 13 ) ...)"
    ## [1] "CURRENT nonlin OBJECTIVE FUNCTION = 4.02647335854124"
    ## [1] "BEST method = 'nonlin' PATH MEMBER = c( 13 )"
    ## [1] "BEST nonlin OBJECTIVE FUNCTION = 4.02647335854124"
    ## [1] "CURRNET METHOD: both"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'both' , seasonal.factor =  c( 13 ) ...)"
    ## [1] "CURRENT both OBJECTIVE FUNCTION = 4.42209253634272"
    ## [1] "BEST method = 'both' PATH MEMBER = c( 13 )"
    ## [1] "BEST both OBJECTIVE FUNCTION = 4.42209253634272"

![](man/figures/README-nns_forecasting-6.png)<!-- -->

    ## Package: tidyAML
    ## [1] "CURRNET METHOD: lin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 1 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 160.193407083468"
    ## [1] "BEST method = 'lin' PATH MEMBER = c( 1 )"
    ## [1] "BEST lin OBJECTIVE FUNCTION = 160.193407083468"
    ## [1] "CURRNET METHOD: nonlin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'nonlin' , seasonal.factor =  c( 1 ) ...)"
    ## [1] "CURRENT nonlin OBJECTIVE FUNCTION = 32.5329700552041"
    ## [1] "BEST method = 'nonlin' PATH MEMBER = c( 1 )"
    ## [1] "BEST nonlin OBJECTIVE FUNCTION = 32.5329700552041"
    ## [1] "CURRNET METHOD: both"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'both' , seasonal.factor =  c( 1 ) ...)"
    ## [1] "CURRENT both OBJECTIVE FUNCTION = 47.6452325586432"
    ## [1] "BEST method = 'both' PATH MEMBER = c( 1 )"
    ## [1] "BEST both OBJECTIVE FUNCTION = 47.6452325586432"

![](man/figures/README-nns_forecasting-7.png)<!-- -->

    ## Package: TidyDensity
    ## [1] "CURRNET METHOD: lin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 9 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 13.1656203222038"
    ## [1] "BEST method = 'lin' PATH MEMBER = c( 9 )"
    ## [1] "BEST lin OBJECTIVE FUNCTION = 13.1656203222038"
    ## [1] "CURRNET METHOD: nonlin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'nonlin' , seasonal.factor =  c( 9 ) ...)"
    ## [1] "CURRENT nonlin OBJECTIVE FUNCTION = 8.46313099960394"
    ## [1] "BEST method = 'nonlin' PATH MEMBER = c( 9 )"
    ## [1] "BEST nonlin OBJECTIVE FUNCTION = 8.46313099960394"
    ## [1] "CURRNET METHOD: both"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'both' , seasonal.factor =  c( 9 ) ...)"
    ## [1] "CURRENT both OBJECTIVE FUNCTION = 11.018662314398"
    ## [1] "BEST method = 'both' PATH MEMBER = c( 9 )"
    ## [1] "BEST both OBJECTIVE FUNCTION = 11.018662314398"

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
    ## 1 healthyR.data <tibble [1,730 × 2]> <tibble [28 × 2]> <split [1702|28]>
    ## 2 healthyR      <tibble [1,722 × 2]> <tibble [28 × 2]> <split [1694|28]>
    ## 3 healthyR.ts   <tibble [1,667 × 2]> <tibble [28 × 2]> <split [1639|28]>
    ## 4 healthyverse  <tibble [1,638 × 2]> <tibble [28 × 2]> <split [1610|28]>
    ## 5 healthyR.ai   <tibble [1,463 × 2]> <tibble [28 × 2]> <split [1435|28]>
    ## 6 TidyDensity   <tibble [1,314 × 2]> <tibble [28 × 2]> <split [1286|28]>
    ## 7 tidyAML       <tibble [922 × 2]>   <tibble [28 × 2]> <split [894|28]> 
    ## 8 RandomWalker  <tibble [344 × 2]>   <tibble [28 × 2]> <split [316|28]> 
    ## 9 <NA>          <tibble [2 × 2]>     <tibble [28 × 2]> <split [0|2]>

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
| healthyR.data | 1 | ARIMA | Test | 0.8694128 | 198.8815 | 1.4373579 | 159.66635 | 1.0240900 | 0.0100992 |
| healthyR.data | 2 | LM | Test | 0.8669673 | 217.4957 | 1.4333149 | 160.52323 | 0.9948753 | 0.1753689 |
| healthyR.data | 3 | EARTH | Test | 0.8535676 | 210.1890 | 1.4111618 | 160.83664 | 0.9828182 | 0.1753689 |
| healthyR.data | 4 | NNAR | Test | 0.7133598 | 105.3865 | 1.1793631 | 189.85126 | 0.8901274 | 0.0185721 |
| healthyR | 1 | ARIMA | Test | 0.7551413 | 115.2715 | 1.0998782 | 161.08710 | 0.9007702 | 0.0148992 |
| healthyR | 2 | LM | Test | 0.7395666 | 109.9577 | 1.0771933 | 174.83738 | 0.8699953 | 0.3281539 |
| healthyR | 3 | EARTH | Test | 0.8199319 | 162.0569 | 1.1942469 | 150.30196 | 0.9488231 | 0.3281539 |
| healthyR | 4 | NNAR | Test | 0.7448838 | 111.7294 | 1.0849379 | 173.99709 | 0.8742965 | 0.0056131 |
| healthyR.ts | 1 | ARIMA | Test | 0.8484101 | 109.5610 | 1.1075850 | 140.19006 | 1.0354077 | 0.0010680 |
| healthyR.ts | 2 | LM | Test | 0.7971873 | 112.2235 | 1.0407146 | 123.74481 | 0.9568820 | 0.0297406 |
| healthyR.ts | 3 | EARTH | Test | 1.1535066 | 164.2049 | 1.5058833 | 158.68137 | 1.3999167 | 0.0297406 |
| healthyR.ts | 4 | NNAR | Test | 0.9191822 | 98.2197 | 1.1999768 | 188.27082 | 1.1007825 | 0.0044966 |
| healthyverse | 1 | ARIMA | Test | 0.7911526 | 210.1151 | 1.3413590 | 117.63140 | 0.9265924 | 0.0538384 |
| healthyverse | 2 | LM | Test | 0.7961053 | 214.4866 | 1.3497560 | 117.43383 | 0.9140065 | 0.3861898 |
| healthyverse | 3 | EARTH | Test | 0.8206639 | 230.5053 | 1.3913939 | 116.73843 | 0.9434606 | 0.3861898 |
| healthyverse | 4 | NNAR | Test | 0.7146481 | 147.0712 | 1.2116495 | 124.51903 | 0.8307953 | 0.0232362 |
| healthyR.ai | 1 | ARIMA | Test | 0.8077411 | 116.0248 | 1.5472223 | 151.47309 | 0.9535056 | 0.0313670 |
| healthyR.ai | 2 | LM | Test | 0.7568278 | 100.5834 | 1.4496982 | 151.76798 | 0.8936729 | 0.2558354 |
| healthyR.ai | 3 | EARTH | Test | 0.8210339 | 121.5310 | 1.5726846 | 141.55074 | 0.9793499 | 0.2558354 |
| healthyR.ai | 4 | NNAR | Test | 0.7760142 | 100.4689 | 1.4864496 | 154.64868 | 0.9342082 | 0.0603982 |
| TidyDensity | 1 | ARIMA | Test | 0.5418193 | 186.1156 | 0.9648889 | 89.18327 | 0.6860032 | 0.0895674 |
| TidyDensity | 2 | LM | Test | 0.5712010 | 223.5421 | 1.0172128 | 88.90856 | 0.7033799 | 0.5857739 |
| TidyDensity | 3 | EARTH | Test | 0.7071564 | 390.3423 | 1.2593263 | 87.57937 | 0.8433733 | 0.5857739 |
| TidyDensity | 4 | NNAR | Test | 0.5926403 | 118.0039 | 1.0553924 | 116.54610 | 0.7575877 | 0.1077386 |
| tidyAML | 1 | ARIMA | Test | 0.6794972 | 329.1549 | 1.5647134 | 123.61374 | 0.8011028 | 0.0625090 |
| tidyAML | 2 | LM | Test | 0.6766143 | 337.5249 | 1.5580748 | 123.41969 | 0.7844743 | 0.3635523 |
| tidyAML | 3 | EARTH | Test | 0.6627621 | 284.0988 | 1.5261766 | 131.90422 | 0.7499676 | 0.3635523 |
| tidyAML | 4 | NNAR | Test | 0.6794093 | 329.3277 | 1.5645110 | 125.90514 | 0.7837257 | 0.1409584 |
| RandomWalker | 1 | ARIMA | Test | 1.0201621 | 114.2403 | 0.6872638 | 176.52599 | 1.3246977 | 0.0239311 |
| RandomWalker | 2 | LM | Test | 0.9945657 | 107.3049 | 0.6700200 | 176.71562 | 1.2553158 | 0.0010715 |
| RandomWalker | 3 | EARTH | Test | 1.0006424 | 99.9695 | 0.6741137 | 179.00676 | 1.2977691 | NA |
| RandomWalker | 4 | NNAR | Test | 0.9795882 | 211.4053 | 0.6599299 | 140.24133 | 1.2108768 | 0.0387646 |

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
    ## 1 healthyR.data         4 NNAR        Test  0.713  105. 1.18  190.  0.890 0.0186
    ## 2 healthyR              2 LM          Test  0.740  110. 1.08  175.  0.870 0.328 
    ## 3 healthyR.ts           2 LM          Test  0.797  112. 1.04  124.  0.957 0.0297
    ## 4 healthyverse          4 NNAR        Test  0.715  147. 1.21  125.  0.831 0.0232
    ## 5 healthyR.ai           2 LM          Test  0.757  101. 1.45  152.  0.894 0.256 
    ## 6 TidyDensity           1 ARIMA       Test  0.542  186. 0.965  89.2 0.686 0.0896
    ## 7 tidyAML               3 EARTH       Test  0.663  284. 1.53  132.  0.750 0.364 
    ## 8 RandomWalker          4 NNAR        Test  0.980  211. 0.660 140.  1.21  0.0388

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
    ## 1 healthyR.data <tibble>     <tibble>     <split [1702|28]> <mdl_tm_t [1 × 5]>
    ## 2 healthyR      <tibble>     <tibble>     <split [1694|28]> <mdl_tm_t [1 × 5]>
    ## 3 healthyR.ts   <tibble>     <tibble>     <split [1639|28]> <mdl_tm_t [1 × 5]>
    ## 4 healthyverse  <tibble>     <tibble>     <split [1610|28]> <mdl_tm_t [1 × 5]>
    ## 5 healthyR.ai   <tibble>     <tibble>     <split [1435|28]> <mdl_tm_t [1 × 5]>
    ## 6 TidyDensity   <tibble>     <tibble>     <split [1286|28]> <mdl_tm_t [1 × 5]>
    ## 7 tidyAML       <tibble>     <tibble>     <split [894|28]>  <mdl_tm_t [1 × 5]>
    ## 8 RandomWalker  <tibble>     <tibble>     <split [316|28]>  <mdl_tm_t [1 × 5]>

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
