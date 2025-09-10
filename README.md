Time Series Analysis, Modeling and Forecasting of the Healthyverse
Packages
================
Steven P. Sanderson II, MPH - Date:
10 September, 2025

This analysis follows a *Nested Modeltime Workflow*.

## Get Data

``` r
glimpse(downloads_tbl)
```

    ## Rows: 151,025
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

The last day in the data set is 2025-09-08 22:21:49, the file was
birthed on: 2024-08-07 07:35:44.428716, and at report knit time is
-9538.77 hours old. Happy analyzing!

Now that we have our data lets take a look at it using the `skimr`
package.

``` r
skim(downloads_tbl)
```

|                                                  |               |
|:-------------------------------------------------|:--------------|
| Name                                             | downloads_tbl |
| Number of rows                                   | 151025        |
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
| r_version     |    109680 |          0.27 |   5 |   5 |     0 |       48 |          0 |
| r_arch        |    109680 |          0.27 |   3 |   7 |     0 |        5 |          0 |
| r_os          |    109680 |          0.27 |   7 |  15 |     0 |       23 |          0 |
| package       |         0 |          1.00 |   7 |  13 |     0 |        8 |          0 |
| version       |         0 |          1.00 |   5 |  17 |     0 |       62 |          0 |
| country       |     12886 |          0.91 |   2 |   2 |     0 |      165 |          0 |

**Variable type: Date**

| skim_variable | n_missing | complete_rate | min | max | median | n_unique |
|:---|---:|---:|:---|:---|:---|---:|
| date | 0 | 1 | 2020-11-23 | 2025-09-08 | 2023-08-25 | 1744 |

**Variable type: numeric**

| skim_variable | n_missing | complete_rate | mean | sd | p0 | p25 | p50 | p75 | p100 | hist |
|:---|---:|---:|---:|---:|---:|---:|---:|---:|---:|:---|
| size | 0 | 1 | 1130163.0 | 1503978.47 | 355 | 14701 | 306707 | 2365853 | 5677952 | ▇▁▂▁▁ |
| ip_id | 0 | 1 | 11268.1 | 21593.68 | 1 | 285 | 3058 | 12135 | 299146 | ▇▁▁▁▁ |

**Variable type: POSIXct**

| skim_variable | n_missing | complete_rate | min | max | median | n_unique |
|:---|---:|---:|:---|:---|:---|---:|
| date_time | 0 | 1 | 2020-11-23 09:00:41 | 2025-09-08 22:21:49 | 2023-08-25 07:23:52 | 93851 |

**Variable type: Timespan**

| skim_variable | n_missing | complete_rate | min | max |     median | n_unique |
|:--------------|----------:|--------------:|----:|----:|-----------:|---------:|
| time          |         0 |             1 |   0 |  59 | 12H 6M 23S |       60 |

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
    ## -147.24  -36.08  -11.14   26.76  817.95 
    ## 
    ## Coefficients:
    ##                                                      Estimate Std. Error
    ## (Intercept)                                        -178.28624   62.89283
    ## date                                                  0.01096    0.00333
    ## lag(value, 1)                                         0.10630    0.02373
    ## lag(value, 7)                                         0.09068    0.02452
    ## lag(value, 14)                                        0.08102    0.02455
    ## lag(value, 21)                                        0.06462    0.02458
    ## lag(value, 28)                                        0.07015    0.02455
    ## lag(value, 35)                                        0.06891    0.02465
    ## lag(value, 42)                                        0.05876    0.02477
    ## lag(value, 49)                                        0.06492    0.02465
    ## month(date, label = TRUE).L                          -9.67324    5.09983
    ## month(date, label = TRUE).Q                           3.16748    4.98620
    ## month(date, label = TRUE).C                         -13.47220    5.07184
    ## month(date, label = TRUE)^4                          -6.84326    5.06715
    ## month(date, label = TRUE)^5                         -10.96820    5.02508
    ## month(date, label = TRUE)^6                          -4.22736    5.10508
    ## month(date, label = TRUE)^7                          -7.61719    4.98844
    ## month(date, label = TRUE)^8                          -3.52152    4.97652
    ## month(date, label = TRUE)^9                           5.57516    4.96113
    ## month(date, label = TRUE)^10                          3.74808    4.89534
    ## month(date, label = TRUE)^11                         -2.91719    4.81170
    ## fourier_vec(date, type = "sin", K = 1, period = 7)  -11.62963    2.26796
    ## fourier_vec(date, type = "cos", K = 1, period = 7)    7.66187    2.37791
    ##                                                    t value Pr(>|t|)    
    ## (Intercept)                                         -2.835 0.004641 ** 
    ## date                                                 3.292 0.001015 ** 
    ## lag(value, 1)                                        4.480 7.98e-06 ***
    ## lag(value, 7)                                        3.698 0.000225 ***
    ## lag(value, 14)                                       3.300 0.000988 ***
    ## lag(value, 21)                                       2.629 0.008643 ** 
    ## lag(value, 28)                                       2.857 0.004323 ** 
    ## lag(value, 35)                                       2.796 0.005235 ** 
    ## lag(value, 42)                                       2.372 0.017806 *  
    ## lag(value, 49)                                       2.633 0.008535 ** 
    ## month(date, label = TRUE).L                         -1.897 0.058029 .  
    ## month(date, label = TRUE).Q                          0.635 0.525353    
    ## month(date, label = TRUE).C                         -2.656 0.007976 ** 
    ## month(date, label = TRUE)^4                         -1.351 0.177033    
    ## month(date, label = TRUE)^5                         -2.183 0.029197 *  
    ## month(date, label = TRUE)^6                         -0.828 0.407749    
    ## month(date, label = TRUE)^7                         -1.527 0.126958    
    ## month(date, label = TRUE)^8                         -0.708 0.479276    
    ## month(date, label = TRUE)^9                          1.124 0.261273    
    ## month(date, label = TRUE)^10                         0.766 0.443997    
    ## month(date, label = TRUE)^11                        -0.606 0.544419    
    ## fourier_vec(date, type = "sin", K = 1, period = 7)  -5.128 3.27e-07 ***
    ## fourier_vec(date, type = "cos", K = 1, period = 7)   3.222 0.001297 ** 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 58.75 on 1672 degrees of freedom
    ##   (49 observations deleted due to missingness)
    ## Multiple R-squared:  0.2322, Adjusted R-squared:  0.2221 
    ## F-statistic: 22.99 on 22 and 1672 DF,  p-value: < 2.2e-16

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
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 9.66094278701282"
    ## [1] "BEST method = 'lin' PATH MEMBER = c( 24 )"
    ## [1] "BEST lin OBJECTIVE FUNCTION = 9.66094278701282"
    ## [1] "CURRNET METHOD: nonlin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'nonlin' , seasonal.factor =  c( 24 ) ...)"
    ## [1] "CURRENT nonlin OBJECTIVE FUNCTION = 14.4522126326685"
    ## [1] "BEST method = 'nonlin' PATH MEMBER = c( 24 )"
    ## [1] "BEST nonlin OBJECTIVE FUNCTION = 14.4522126326685"
    ## [1] "CURRNET METHOD: both"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'both' , seasonal.factor =  c( 24 ) ...)"
    ## [1] "CURRENT both OBJECTIVE FUNCTION = 10.3779947792519"
    ## [1] "BEST method = 'both' PATH MEMBER = c( 24 )"
    ## [1] "BEST both OBJECTIVE FUNCTION = 10.3779947792519"

![](man/figures/README-nns_forecasting-1.png)<!-- -->

    ## Package: healthyR.ai
    ## [1] "CURRNET METHOD: lin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 13 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 11.7757274881094"
    ## [1] "BEST method = 'lin' PATH MEMBER = c( 13 )"
    ## [1] "BEST lin OBJECTIVE FUNCTION = 11.7757274881094"
    ## [1] "CURRNET METHOD: nonlin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'nonlin' , seasonal.factor =  c( 13 ) ...)"
    ## [1] "CURRENT nonlin OBJECTIVE FUNCTION = 8.20215404384118"
    ## [1] "BEST method = 'nonlin' PATH MEMBER = c( 13 )"
    ## [1] "BEST nonlin OBJECTIVE FUNCTION = 8.20215404384118"
    ## [1] "CURRNET METHOD: both"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'both' , seasonal.factor =  c( 13 ) ...)"
    ## [1] "CURRENT both OBJECTIVE FUNCTION = 15.5118503476161"
    ## [1] "BEST method = 'both' PATH MEMBER = c( 13 )"
    ## [1] "BEST both OBJECTIVE FUNCTION = 15.5118503476161"

![](man/figures/README-nns_forecasting-2.png)<!-- -->

    ## Package: healthyR.data
    ## [1] "CURRNET METHOD: lin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 13 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 6.41555233594887"
    ## [1] "BEST method = 'lin' PATH MEMBER = c( 13 )"
    ## [1] "BEST lin OBJECTIVE FUNCTION = 6.41555233594887"
    ## [1] "CURRNET METHOD: nonlin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'nonlin' , seasonal.factor =  c( 13 ) ...)"
    ## [1] "CURRENT nonlin OBJECTIVE FUNCTION = 7.95945592931911"
    ## [1] "BEST method = 'nonlin' PATH MEMBER = c( 13 )"
    ## [1] "BEST nonlin OBJECTIVE FUNCTION = 7.95945592931911"
    ## [1] "CURRNET METHOD: both"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'both' , seasonal.factor =  c( 13 ) ...)"
    ## [1] "CURRENT both OBJECTIVE FUNCTION = 8.03494329805622"
    ## [1] "BEST method = 'both' PATH MEMBER = c( 13 )"
    ## [1] "BEST both OBJECTIVE FUNCTION = 8.03494329805622"

![](man/figures/README-nns_forecasting-3.png)<!-- -->

    ## Package: healthyR.ts
    ## [1] "CURRNET METHOD: lin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 22 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 7.66618656528686"
    ## [1] "BEST method = 'lin' PATH MEMBER = c( 22 )"
    ## [1] "BEST lin OBJECTIVE FUNCTION = 7.66618656528686"
    ## [1] "CURRNET METHOD: nonlin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'nonlin' , seasonal.factor =  c( 22 ) ...)"
    ## [1] "CURRENT nonlin OBJECTIVE FUNCTION = 3.38802936044282"
    ## [1] "BEST method = 'nonlin' PATH MEMBER = c( 22 )"
    ## [1] "BEST nonlin OBJECTIVE FUNCTION = 3.38802936044282"
    ## [1] "CURRNET METHOD: both"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'both' , seasonal.factor =  c( 22 ) ...)"
    ## [1] "CURRENT both OBJECTIVE FUNCTION = 3.13775119834936"
    ## [1] "BEST method = 'both' PATH MEMBER = c( 22 )"
    ## [1] "BEST both OBJECTIVE FUNCTION = 3.13775119834936"

![](man/figures/README-nns_forecasting-4.png)<!-- -->

    ## Package: healthyverse
    ## [1] "CURRNET METHOD: lin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 25 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 6.68987176830104"
    ## [1] "BEST method = 'lin' PATH MEMBER = c( 25 )"
    ## [1] "BEST lin OBJECTIVE FUNCTION = 6.68987176830104"
    ## [1] "CURRNET METHOD: nonlin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'nonlin' , seasonal.factor =  c( 25 ) ...)"
    ## [1] "CURRENT nonlin OBJECTIVE FUNCTION = 5.22863132693699"
    ## [1] "BEST method = 'nonlin' PATH MEMBER = c( 25 )"
    ## [1] "BEST nonlin OBJECTIVE FUNCTION = 5.22863132693699"
    ## [1] "CURRNET METHOD: both"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'both' , seasonal.factor =  c( 25 ) ...)"
    ## [1] "CURRENT both OBJECTIVE FUNCTION = 4.80905556091617"
    ## [1] "BEST method = 'both' PATH MEMBER = c( 25 )"
    ## [1] "BEST both OBJECTIVE FUNCTION = 4.80905556091617"

![](man/figures/README-nns_forecasting-5.png)<!-- -->

    ## Package: RandomWalker
    ## [1] "CURRNET METHOD: lin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 1 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 139.012754778318"
    ## [1] "BEST method = 'lin' PATH MEMBER = c( 1 )"
    ## [1] "BEST lin OBJECTIVE FUNCTION = 139.012754778318"
    ## [1] "CURRNET METHOD: nonlin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'nonlin' , seasonal.factor =  c( 1 ) ...)"
    ## [1] "CURRENT nonlin OBJECTIVE FUNCTION = 85.4882576409071"
    ## [1] "BEST method = 'nonlin' PATH MEMBER = c( 1 )"
    ## [1] "BEST nonlin OBJECTIVE FUNCTION = 85.4882576409071"
    ## [1] "CURRNET METHOD: both"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'both' , seasonal.factor =  c( 1 ) ...)"
    ## [1] "CURRENT both OBJECTIVE FUNCTION = 168.774097487905"
    ## [1] "BEST method = 'both' PATH MEMBER = c( 1 )"
    ## [1] "BEST both OBJECTIVE FUNCTION = 168.774097487905"

![](man/figures/README-nns_forecasting-6.png)<!-- -->

    ## Package: tidyAML
    ## [1] "CURRNET METHOD: lin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 2 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 105.374740103791"
    ## [1] "BEST method = 'lin' PATH MEMBER = c( 2 )"
    ## [1] "BEST lin OBJECTIVE FUNCTION = 105.374740103791"
    ## [1] "CURRNET METHOD: nonlin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'nonlin' , seasonal.factor =  c( 2 ) ...)"
    ## [1] "CURRENT nonlin OBJECTIVE FUNCTION = 10.4857814520784"
    ## [1] "BEST method = 'nonlin' PATH MEMBER = c( 2 )"
    ## [1] "BEST nonlin OBJECTIVE FUNCTION = 10.4857814520784"
    ## [1] "CURRNET METHOD: both"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'both' , seasonal.factor =  c( 2 ) ...)"
    ## [1] "CURRENT both OBJECTIVE FUNCTION = 11.6970088138609"
    ## [1] "BEST method = 'both' PATH MEMBER = c( 2 )"
    ## [1] "BEST both OBJECTIVE FUNCTION = 11.6970088138609"

![](man/figures/README-nns_forecasting-7.png)<!-- -->

    ## Package: TidyDensity
    ## [1] "CURRNET METHOD: lin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 14 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 13.8274286373534"
    ## [1] "BEST method = 'lin' PATH MEMBER = c( 14 )"
    ## [1] "BEST lin OBJECTIVE FUNCTION = 13.8274286373534"
    ## [1] "CURRNET METHOD: nonlin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'nonlin' , seasonal.factor =  c( 14 ) ...)"
    ## [1] "CURRENT nonlin OBJECTIVE FUNCTION = 10.768927254619"
    ## [1] "BEST method = 'nonlin' PATH MEMBER = c( 14 )"
    ## [1] "BEST nonlin OBJECTIVE FUNCTION = 10.768927254619"
    ## [1] "CURRNET METHOD: both"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'both' , seasonal.factor =  c( 14 ) ...)"
    ## [1] "CURRENT both OBJECTIVE FUNCTION = 12.8814538000344"
    ## [1] "BEST method = 'both' PATH MEMBER = c( 14 )"
    ## [1] "BEST both OBJECTIVE FUNCTION = 12.8814538000344"

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
    ## 1 healthyR.data <tibble [1,736 × 2]> <tibble [28 × 2]> <split [1708|28]>
    ## 2 healthyR      <tibble [1,727 × 2]> <tibble [28 × 2]> <split [1699|28]>
    ## 3 healthyR.ts   <tibble [1,673 × 2]> <tibble [28 × 2]> <split [1645|28]>
    ## 4 healthyverse  <tibble [1,644 × 2]> <tibble [28 × 2]> <split [1616|28]>
    ## 5 healthyR.ai   <tibble [1,469 × 2]> <tibble [28 × 2]> <split [1441|28]>
    ## 6 TidyDensity   <tibble [1,320 × 2]> <tibble [28 × 2]> <split [1292|28]>
    ## 7 tidyAML       <tibble [927 × 2]>   <tibble [28 × 2]> <split [899|28]> 
    ## 8 RandomWalker  <tibble [350 × 2]>   <tibble [28 × 2]> <split [322|28]> 
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
| healthyR.data | 1 | ARIMA | Test | 0.8508221 | 121.22566 | 1.0072766 | 159.58302 | 1.0134742 | 0.0110113 |
| healthyR.data | 2 | LM | Test | 0.8883017 | 184.17149 | 1.0516482 | 148.13286 | 1.0149663 | 0.1730465 |
| healthyR.data | 3 | NULL | NA | NA | NA | NA | NA | NA | NA |
| healthyR.data | 4 | NNAR | Test | 0.8204635 | 100.98414 | 0.9713355 | 187.18121 | 1.0103419 | 0.0033683 |
| healthyR | 1 | ARIMA | Test | 0.6608564 | 109.63641 | 0.7809169 | 172.93633 | 0.8211890 | 0.0001284 |
| healthyR | 2 | LM | Test | 0.6721710 | 113.09842 | 0.7942870 | 174.88412 | 0.8070278 | 0.0622117 |
| healthyR | 3 | NULL | NA | NA | NA | NA | NA | NA | NA |
| healthyR | 4 | NNAR | Test | 0.6529266 | 114.56096 | 0.7715464 | 161.61925 | 0.7950318 | 0.0512015 |
| healthyR.ts | 1 | ARIMA | Test | 0.8594971 | 146.92929 | 0.9711508 | 127.89146 | 1.0704874 | 0.0073817 |
| healthyR.ts | 2 | LM | Test | 0.8866032 | 135.98957 | 1.0017781 | 136.32776 | 1.0724885 | 0.2073096 |
| healthyR.ts | 3 | NULL | NA | NA | NA | NA | NA | NA | NA |
| healthyR.ts | 4 | NNAR | Test | 0.8597699 | 93.86880 | 0.9714590 | 175.15235 | 1.0721633 | 0.0622623 |
| healthyverse | 1 | ARIMA | Test | 0.7518233 | 140.63117 | 1.1941464 | 100.95520 | 0.8974456 | 0.1029016 |
| healthyverse | 2 | LM | Test | 0.7748632 | 166.29945 | 1.2307416 | 97.60147 | 0.9143538 | 0.4534254 |
| healthyverse | 3 | NULL | NA | NA | NA | NA | NA | NA | NA |
| healthyverse | 4 | NNAR | Test | 0.7840173 | 116.44688 | 1.2452814 | 115.67721 | 0.9491935 | 0.2012498 |
| healthyR.ai | 1 | ARIMA | Test | 0.6552954 | 85.60594 | 0.9925314 | 140.70690 | 0.8013846 | 0.2621980 |
| healthyR.ai | 2 | LM | Test | 0.7045126 | 94.18967 | 1.0670774 | 140.90272 | 0.8519642 | 0.1970245 |
| healthyR.ai | 3 | NULL | NA | NA | NA | NA | NA | NA | NA |
| healthyR.ai | 4 | NNAR | Test | 0.6491923 | 82.18696 | 0.9832875 | 136.24904 | 0.8113923 | 0.1107227 |
| TidyDensity | 1 | ARIMA | Test | 0.5336545 | 241.48594 | 0.8432793 | 89.94446 | 0.6859547 | 0.0832465 |
| TidyDensity | 2 | LM | Test | 0.5779500 | 247.53685 | 0.9132748 | 94.44611 | 0.7126066 | 0.0016243 |
| TidyDensity | 3 | NULL | NA | NA | NA | NA | NA | NA | NA |
| TidyDensity | 4 | NNAR | Test | 0.5648883 | 141.15113 | 0.8926348 | 114.27379 | 0.7727605 | 0.0397584 |
| tidyAML | 1 | ARIMA | Test | 0.6265267 | 179.30255 | 1.1190018 | 113.20097 | 0.7595298 | 0.0002896 |
| tidyAML | 2 | LM | Test | 0.6404268 | 263.18799 | 1.1438281 | 104.37689 | 0.7582853 | 0.3103151 |
| tidyAML | 3 | NULL | NA | NA | NA | NA | NA | NA | NA |
| tidyAML | 4 | NNAR | Test | 0.5799509 | 178.93106 | 1.0358156 | 107.44302 | 0.6898699 | 0.4299714 |
| RandomWalker | 1 | ARIMA | Test | 1.0496279 | 117.22781 | 0.6511816 | 173.20895 | 1.2720909 | 0.0213406 |
| RandomWalker | 2 | LM | Test | 1.0668794 | 131.78979 | 0.6618843 | 178.42444 | 1.2645914 | 0.0039464 |
| RandomWalker | 3 | NULL | NA | NA | NA | NA | NA | NA | NA |
| RandomWalker | 4 | NNAR | Test | 1.2040404 | 265.29053 | 0.7469781 | 165.70393 | 1.3399257 | 0.0140280 |

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
    ## 1 healthyR.da…         4 NNAR        Test  0.820 101.  0.971 187.  1.01  0.00337
    ## 2 healthyR             4 NNAR        Test  0.653 115.  0.772 162.  0.795 0.0512 
    ## 3 healthyR.ts          1 ARIMA       Test  0.859 147.  0.971 128.  1.07  0.00738
    ## 4 healthyverse         1 ARIMA       Test  0.752 141.  1.19  101.  0.897 0.103  
    ## 5 healthyR.ai          1 ARIMA       Test  0.655  85.6 0.993 141.  0.801 0.262  
    ## 6 TidyDensity          1 ARIMA       Test  0.534 241.  0.843  89.9 0.686 0.0832 
    ## 7 tidyAML              4 NNAR        Test  0.580 179.  1.04  107.  0.690 0.430  
    ## 8 RandomWalker         2 LM          Test  1.07  132.  0.662 178.  1.26  0.00395

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
    ## 1 healthyR.data <tibble>     <tibble>     <split [1708|28]> <mdl_tm_t [1 × 5]>
    ## 2 healthyR      <tibble>     <tibble>     <split [1699|28]> <mdl_tm_t [1 × 5]>
    ## 3 healthyR.ts   <tibble>     <tibble>     <split [1645|28]> <mdl_tm_t [1 × 5]>
    ## 4 healthyverse  <tibble>     <tibble>     <split [1616|28]> <mdl_tm_t [1 × 5]>
    ## 5 healthyR.ai   <tibble>     <tibble>     <split [1441|28]> <mdl_tm_t [1 × 5]>
    ## 6 TidyDensity   <tibble>     <tibble>     <split [1292|28]> <mdl_tm_t [1 × 5]>
    ## 7 tidyAML       <tibble>     <tibble>     <split [899|28]>  <mdl_tm_t [1 × 5]>
    ## 8 RandomWalker  <tibble>     <tibble>     <split [322|28]>  <mdl_tm_t [1 × 5]>

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
