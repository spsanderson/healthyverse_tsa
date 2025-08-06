Time Series Analysis, Modeling and Forecasting of the Healthyverse
Packages
================
Steven P. Sanderson II, MPH - Date:
06 August, 2025

This analysis follows a *Nested Modeltime Workflow*.

## Get Data

``` r
glimpse(downloads_tbl)
```

    ## Rows: 147,985
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

The last day in the data set is 2025-08-04 23:47:45, the file was
birthed on: 2022-07-02 23:58:17.511888, and at report knit time is
-2.709182^{4} hours old. Happy analyzing!

Now that we have our data lets take a look at it using the `skimr`
package.

``` r
skim(downloads_tbl)
```

|                                                  |               |
|:-------------------------------------------------|:--------------|
| Name                                             | downloads_tbl |
| Number of rows                                   | 147985        |
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
| r_version     |    107128 |          0.28 |   5 |   5 |     0 |       48 |          0 |
| r_arch        |    107128 |          0.28 |   3 |   7 |     0 |        5 |          0 |
| r_os          |    107128 |          0.28 |   7 |  15 |     0 |       23 |          0 |
| package       |         0 |          1.00 |   7 |  13 |     0 |        8 |          0 |
| version       |         0 |          1.00 |   5 |  17 |     0 |       61 |          0 |
| country       |     12503 |          0.92 |   2 |   2 |     0 |      164 |          0 |

**Variable type: Date**

| skim_variable | n_missing | complete_rate | min | max | median | n_unique |
|:---|---:|---:|:---|:---|:---|---:|
| date | 0 | 1 | 2020-11-23 | 2025-08-04 | 2023-08-08 | 1716 |

**Variable type: numeric**

| skim_variable | n_missing | complete_rate | mean | sd | p0 | p25 | p50 | p75 | p100 | hist |
|:---|---:|---:|---:|---:|---:|---:|---:|---:|---:|:---|
| size | 0 | 1 | 1131674.87 | 1509062.41 | 355 | 14701 | 305418 | 2367601 | 5677952 | ▇▁▂▁▁ |
| ip_id | 0 | 1 | 10650.24 | 19123.94 | 1 | 285 | 3058 | 11952 | 230824 | ▇▁▁▁▁ |

**Variable type: POSIXct**

| skim_variable | n_missing | complete_rate | min | max | median | n_unique |
|:---|---:|---:|:---|:---|:---|---:|
| date_time | 0 | 1 | 2020-11-23 09:00:41 | 2025-08-04 23:47:45 | 2023-08-08 13:54:08 | 91426 |

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
    ## -148.23  -36.14  -11.25   26.75  817.49 
    ## 
    ## Coefficients:
    ##                                                      Estimate Std. Error
    ## (Intercept)                                        -185.02874   64.23682
    ## date                                                  0.01132    0.00340
    ## lag(value, 1)                                         0.10286    0.02392
    ## lag(value, 7)                                         0.09256    0.02479
    ## lag(value, 14)                                        0.08486    0.02481
    ## lag(value, 21)                                        0.06479    0.02486
    ## lag(value, 28)                                        0.06701    0.02476
    ## lag(value, 35)                                        0.06652    0.02481
    ## lag(value, 42)                                        0.05974    0.02490
    ## lag(value, 49)                                        0.06554    0.02489
    ## month(date, label = TRUE).L                          -9.53998    5.10774
    ## month(date, label = TRUE).Q                           2.97451    5.01139
    ## month(date, label = TRUE).C                         -13.80528    5.11461
    ## month(date, label = TRUE)^4                          -6.82239    5.07601
    ## month(date, label = TRUE)^5                         -10.63221    5.08172
    ## month(date, label = TRUE)^6                          -3.95696    5.12887
    ## month(date, label = TRUE)^7                          -7.71352    5.03106
    ## month(date, label = TRUE)^8                          -3.82226    5.01676
    ## month(date, label = TRUE)^9                           5.46279    5.00030
    ## month(date, label = TRUE)^10                          3.81308    5.00975
    ## month(date, label = TRUE)^11                         -2.71749    4.88505
    ## fourier_vec(date, type = "sin", K = 1, period = 7)  -11.59927    2.28858
    ## fourier_vec(date, type = "cos", K = 1, period = 7)    8.11575    2.40344
    ##                                                    t value Pr(>|t|)    
    ## (Intercept)                                         -2.880 0.004023 ** 
    ## date                                                 3.329 0.000890 ***
    ## lag(value, 1)                                        4.300 1.81e-05 ***
    ## lag(value, 7)                                        3.733 0.000195 ***
    ## lag(value, 14)                                       3.420 0.000641 ***
    ## lag(value, 21)                                       2.606 0.009242 ** 
    ## lag(value, 28)                                       2.707 0.006861 ** 
    ## lag(value, 35)                                       2.681 0.007409 ** 
    ## lag(value, 42)                                       2.399 0.016556 *  
    ## lag(value, 49)                                       2.633 0.008533 ** 
    ## month(date, label = TRUE).L                         -1.868 0.061975 .  
    ## month(date, label = TRUE).Q                          0.594 0.552895    
    ## month(date, label = TRUE).C                         -2.699 0.007022 ** 
    ## month(date, label = TRUE)^4                         -1.344 0.179119    
    ## month(date, label = TRUE)^5                         -2.092 0.036570 *  
    ## month(date, label = TRUE)^6                         -0.772 0.440517    
    ## month(date, label = TRUE)^7                         -1.533 0.125424    
    ## month(date, label = TRUE)^8                         -0.762 0.446231    
    ## month(date, label = TRUE)^9                          1.092 0.274776    
    ## month(date, label = TRUE)^10                         0.761 0.446688    
    ## month(date, label = TRUE)^11                        -0.556 0.578089    
    ## fourier_vec(date, type = "sin", K = 1, period = 7)  -5.068 4.47e-07 ***
    ## fourier_vec(date, type = "cos", K = 1, period = 7)   3.377 0.000751 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 58.74 on 1644 degrees of freedom
    ##   (49 observations deleted due to missingness)
    ## Multiple R-squared:  0.2347, Adjusted R-squared:  0.2245 
    ## F-statistic: 22.92 on 22 and 1644 DF,  p-value: < 2.2e-16

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
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 24 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 13.7351198331391"
    ## [1] "BEST method = 'lin' PATH MEMBER = c( 24 )"
    ## [1] "BEST lin OBJECTIVE FUNCTION = 13.7351198331391"
    ## [1] "CURRNET METHOD: nonlin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'nonlin' , seasonal.factor =  c( 24 ) ...)"
    ## [1] "CURRENT nonlin OBJECTIVE FUNCTION = 4.61572994993297"
    ## [1] "BEST method = 'nonlin' PATH MEMBER = c( 24 )"
    ## [1] "BEST nonlin OBJECTIVE FUNCTION = 4.61572994993297"
    ## [1] "CURRNET METHOD: both"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'both' , seasonal.factor =  c( 24 ) ...)"
    ## [1] "CURRENT both OBJECTIVE FUNCTION = 7.50332972576909"
    ## [1] "BEST method = 'both' PATH MEMBER = c( 24 )"
    ## [1] "BEST both OBJECTIVE FUNCTION = 7.50332972576909"

![](man/figures/README-nns_forecasting-1.png)<!-- -->

    ## Package: healthyR.ai
    ## [1] "CURRNET METHOD: lin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 24 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 7.62567906078636"
    ## [1] "BEST method = 'lin' PATH MEMBER = c( 24 )"
    ## [1] "BEST lin OBJECTIVE FUNCTION = 7.62567906078636"
    ## [1] "CURRNET METHOD: nonlin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'nonlin' , seasonal.factor =  c( 24 ) ...)"
    ## [1] "CURRENT nonlin OBJECTIVE FUNCTION = 11.8441609634614"
    ## [1] "BEST method = 'nonlin' PATH MEMBER = c( 24 )"
    ## [1] "BEST nonlin OBJECTIVE FUNCTION = 11.8441609634614"
    ## [1] "CURRNET METHOD: both"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'both' , seasonal.factor =  c( 24 ) ...)"
    ## [1] "CURRENT both OBJECTIVE FUNCTION = 10.2220058456593"
    ## [1] "BEST method = 'both' PATH MEMBER = c( 24 )"
    ## [1] "BEST both OBJECTIVE FUNCTION = 10.2220058456593"

![](man/figures/README-nns_forecasting-2.png)<!-- -->

    ## Package: healthyR.data
    ## [1] "CURRNET METHOD: lin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 20 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 7.92997930305187"
    ## [1] "BEST method = 'lin' PATH MEMBER = c( 20 )"
    ## [1] "BEST lin OBJECTIVE FUNCTION = 7.92997930305187"
    ## [1] "CURRNET METHOD: nonlin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'nonlin' , seasonal.factor =  c( 20 ) ...)"
    ## [1] "CURRENT nonlin OBJECTIVE FUNCTION = 6.09722737842582"
    ## [1] "BEST method = 'nonlin' PATH MEMBER = c( 20 )"
    ## [1] "BEST nonlin OBJECTIVE FUNCTION = 6.09722737842582"
    ## [1] "CURRNET METHOD: both"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'both' , seasonal.factor =  c( 20 ) ...)"
    ## [1] "CURRENT both OBJECTIVE FUNCTION = 7.10212132995081"
    ## [1] "BEST method = 'both' PATH MEMBER = c( 20 )"
    ## [1] "BEST both OBJECTIVE FUNCTION = 7.10212132995081"

![](man/figures/README-nns_forecasting-3.png)<!-- -->

    ## Package: healthyR.ts
    ## [1] "CURRNET METHOD: lin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 20 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 19.905798829478"
    ## [1] "BEST method = 'lin' PATH MEMBER = c( 20 )"
    ## [1] "BEST lin OBJECTIVE FUNCTION = 19.905798829478"
    ## [1] "CURRNET METHOD: nonlin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'nonlin' , seasonal.factor =  c( 20 ) ...)"
    ## [1] "CURRENT nonlin OBJECTIVE FUNCTION = 16.4652318444394"
    ## [1] "BEST method = 'nonlin' PATH MEMBER = c( 20 )"
    ## [1] "BEST nonlin OBJECTIVE FUNCTION = 16.4652318444394"
    ## [1] "CURRNET METHOD: both"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'both' , seasonal.factor =  c( 20 ) ...)"
    ## [1] "CURRENT both OBJECTIVE FUNCTION = 22.0267029997514"
    ## [1] "BEST method = 'both' PATH MEMBER = c( 20 )"
    ## [1] "BEST both OBJECTIVE FUNCTION = 22.0267029997514"

![](man/figures/README-nns_forecasting-4.png)<!-- -->

    ## Package: healthyverse
    ## [1] "CURRNET METHOD: lin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 20 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 18.3562432390449"
    ## [1] "BEST method = 'lin' PATH MEMBER = c( 20 )"
    ## [1] "BEST lin OBJECTIVE FUNCTION = 18.3562432390449"
    ## [1] "CURRNET METHOD: nonlin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'nonlin' , seasonal.factor =  c( 20 ) ...)"
    ## [1] "CURRENT nonlin OBJECTIVE FUNCTION = 11.626686457197"
    ## [1] "BEST method = 'nonlin' PATH MEMBER = c( 20 )"
    ## [1] "BEST nonlin OBJECTIVE FUNCTION = 11.626686457197"
    ## [1] "CURRNET METHOD: both"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'both' , seasonal.factor =  c( 20 ) ...)"
    ## [1] "CURRENT both OBJECTIVE FUNCTION = 14.0402492249849"
    ## [1] "BEST method = 'both' PATH MEMBER = c( 20 )"
    ## [1] "BEST both OBJECTIVE FUNCTION = 14.0402492249849"

![](man/figures/README-nns_forecasting-5.png)<!-- -->

    ## Package: RandomWalker
    ## [1] "CURRNET METHOD: lin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 20 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 7.91810341671963"
    ## [1] "BEST method = 'lin' PATH MEMBER = c( 20 )"
    ## [1] "BEST lin OBJECTIVE FUNCTION = 7.91810341671963"
    ## [1] "CURRNET METHOD: nonlin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'nonlin' , seasonal.factor =  c( 20 ) ...)"
    ## [1] "CURRENT nonlin OBJECTIVE FUNCTION = 5.77389946177687"
    ## [1] "BEST method = 'nonlin' PATH MEMBER = c( 20 )"
    ## [1] "BEST nonlin OBJECTIVE FUNCTION = 5.77389946177687"
    ## [1] "CURRNET METHOD: both"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'both' , seasonal.factor =  c( 20 ) ...)"
    ## [1] "CURRENT both OBJECTIVE FUNCTION = 5.79138694329525"
    ## [1] "BEST method = 'both' PATH MEMBER = c( 20 )"
    ## [1] "BEST both OBJECTIVE FUNCTION = 5.79138694329525"

![](man/figures/README-nns_forecasting-6.png)<!-- -->

    ## Package: tidyAML
    ## [1] "CURRNET METHOD: lin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 14 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 7.40844303967894"
    ## [1] "BEST method = 'lin' PATH MEMBER = c( 14 )"
    ## [1] "BEST lin OBJECTIVE FUNCTION = 7.40844303967894"
    ## [1] "CURRNET METHOD: nonlin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'nonlin' , seasonal.factor =  c( 14 ) ...)"
    ## [1] "CURRENT nonlin OBJECTIVE FUNCTION = 8.04659226264253"
    ## [1] "BEST method = 'nonlin' PATH MEMBER = c( 14 )"
    ## [1] "BEST nonlin OBJECTIVE FUNCTION = 8.04659226264253"
    ## [1] "CURRNET METHOD: both"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'both' , seasonal.factor =  c( 14 ) ...)"
    ## [1] "CURRENT both OBJECTIVE FUNCTION = 7.86510189382041"
    ## [1] "BEST method = 'both' PATH MEMBER = c( 14 )"
    ## [1] "BEST both OBJECTIVE FUNCTION = 7.86510189382041"

![](man/figures/README-nns_forecasting-7.png)<!-- -->

    ## Package: TidyDensity
    ## [1] "CURRNET METHOD: lin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 20 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 27.6726484859834"
    ## [1] "BEST method = 'lin' PATH MEMBER = c( 20 )"
    ## [1] "BEST lin OBJECTIVE FUNCTION = 27.6726484859834"
    ## [1] "CURRNET METHOD: nonlin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'nonlin' , seasonal.factor =  c( 20 ) ...)"
    ## [1] "CURRENT nonlin OBJECTIVE FUNCTION = 25.0390000398318"
    ## [1] "BEST method = 'nonlin' PATH MEMBER = c( 20 )"
    ## [1] "BEST nonlin OBJECTIVE FUNCTION = 25.0390000398318"
    ## [1] "CURRNET METHOD: both"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'both' , seasonal.factor =  c( 20 ) ...)"
    ## [1] "CURRENT both OBJECTIVE FUNCTION = 34.3728166123166"
    ## [1] "BEST method = 'both' PATH MEMBER = c( 20 )"
    ## [1] "BEST both OBJECTIVE FUNCTION = 34.3728166123166"

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
    ## 1 healthyR.data <tibble [1,708 × 2]> <tibble [28 × 2]> <split [1680|28]>
    ## 2 healthyR      <tibble [1,700 × 2]> <tibble [28 × 2]> <split [1672|28]>
    ## 3 healthyR.ts   <tibble [1,645 × 2]> <tibble [28 × 2]> <split [1617|28]>
    ## 4 healthyverse  <tibble [1,616 × 2]> <tibble [28 × 2]> <split [1588|28]>
    ## 5 healthyR.ai   <tibble [1,441 × 2]> <tibble [28 × 2]> <split [1413|28]>
    ## 6 TidyDensity   <tibble [1,292 × 2]> <tibble [28 × 2]> <split [1264|28]>
    ## 7 tidyAML       <tibble [900 × 2]>   <tibble [28 × 2]> <split [872|28]> 
    ## 8 RandomWalker  <tibble [322 × 2]>   <tibble [28 × 2]> <split [294|28]>

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
| healthyR.data | 1 | ARIMA | Test | 0.6638018 | 136.39936 | 0.6827723 | 142.62516 | 0.8215302 | 0.0856182 |
| healthyR.data | 2 | LM | Test | 0.7301261 | 159.35678 | 0.7509921 | 144.95469 | 0.8622682 | 0.0054714 |
| healthyR.data | 3 | NULL | NA | NA | NA | NA | NA | NA | NA |
| healthyR.data | 4 | NNAR | Test | 0.6375229 | 100.81134 | 0.6557424 | 189.75852 | 0.8445643 | 0.0021409 |
| healthyR | 1 | ARIMA | Test | 0.5801423 | 136.22607 | 0.8362407 | 172.03575 | 0.7164028 | 0.0126203 |
| healthyR | 2 | LM | Test | 0.5606873 | 109.20890 | 0.8081975 | 164.02735 | 0.7125461 | 0.1584211 |
| healthyR | 3 | NULL | NA | NA | NA | NA | NA | NA | NA |
| healthyR | 4 | NNAR | Test | 0.5701733 | 125.50083 | 0.8218709 | 163.07219 | 0.7129425 | 0.0081833 |
| healthyR.ts | 1 | ARIMA | Test | 0.6518208 | 104.53706 | 0.7434003 | 147.05849 | 0.7737710 | 0.0033852 |
| healthyR.ts | 2 | LM | Test | 0.6262021 | 106.67353 | 0.7141823 | 120.31555 | 0.7706709 | 0.1308147 |
| healthyR.ts | 3 | NULL | NA | NA | NA | NA | NA | NA | NA |
| healthyR.ts | 4 | NNAR | Test | 0.6523301 | 98.87603 | 0.7439812 | 185.59555 | 0.7692087 | 0.0283841 |
| healthyverse | 1 | ARIMA | Test | 0.5837411 | 217.47767 | 0.9829123 | 95.44461 | 0.7220755 | 0.0008151 |
| healthyverse | 2 | LM | Test | 0.5947294 | 233.88387 | 1.0014145 | 93.54148 | 0.7388008 | 0.0110243 |
| healthyverse | 3 | NULL | NA | NA | NA | NA | NA | NA | NA |
| healthyverse | 4 | NNAR | Test | 0.6077590 | 163.12553 | 1.0233540 | 112.42883 | 0.7221472 | 0.0127121 |
| healthyR.ai | 1 | ARIMA | Test | 0.7003196 | 126.76454 | 0.8744031 | 151.09158 | 0.8692887 | 0.0001009 |
| healthyR.ai | 2 | LM | Test | 0.6979468 | 122.14176 | 0.8714405 | 153.06214 | 0.8683747 | 0.0787726 |
| healthyR.ai | 3 | NULL | NA | NA | NA | NA | NA | NA | NA |
| healthyR.ai | 4 | NNAR | Test | 0.6893583 | 114.84957 | 0.8607171 | 154.66964 | 0.8643086 | 0.0103645 |
| TidyDensity | 1 | ARIMA | Test | 0.9972034 | 96.96010 | 0.8152921 | 112.24939 | 1.2382406 | 0.0116566 |
| TidyDensity | 2 | LM | Test | 0.8706631 | 110.64830 | 0.7118355 | 86.91662 | 1.1240464 | 0.0262959 |
| TidyDensity | 3 | NULL | NA | NA | NA | NA | NA | NA | NA |
| TidyDensity | 4 | NNAR | Test | 1.0942199 | 91.99628 | 0.8946107 | 137.24333 | 1.3202411 | 0.0059986 |
| tidyAML | 1 | ARIMA | Test | 0.5662800 | 222.96868 | 1.1284958 | 106.30810 | 0.6717036 | 0.1149394 |
| tidyAML | 2 | LM | Test | 0.5677466 | 240.92572 | 1.1314185 | 104.07837 | 0.6615442 | 0.0206036 |
| tidyAML | 3 | NULL | NA | NA | NA | NA | NA | NA | NA |
| tidyAML | 4 | NNAR | Test | 0.5685574 | 215.53871 | 1.1330344 | 107.09434 | 0.6705076 | 0.0417129 |
| RandomWalker | 1 | ARIMA | Test | 1.2195399 | 107.78923 | 0.5840094 | 180.75067 | 1.3993542 | 0.0159992 |
| RandomWalker | 2 | LM | Test | 1.2161552 | 96.71370 | 0.5823885 | 169.97018 | 1.4030589 | 0.0126187 |
| RandomWalker | 3 | NULL | NA | NA | NA | NA | NA | NA | NA |
| RandomWalker | 4 | NNAR | Test | 1.2878871 | 108.40516 | 0.6167393 | 148.28319 | 1.5098564 | 0.0525259 |

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
    ## 1 healthyR.da…         1 ARIMA       Test  0.664 136.  0.683 143.  0.822 8.56e-2
    ## 2 healthyR             2 LM          Test  0.561 109.  0.808 164.  0.713 1.58e-1
    ## 3 healthyR.ts          4 NNAR        Test  0.652  98.9 0.744 186.  0.769 2.84e-2
    ## 4 healthyverse         1 ARIMA       Test  0.584 217.  0.983  95.4 0.722 8.15e-4
    ## 5 healthyR.ai          4 NNAR        Test  0.689 115.  0.861 155.  0.864 1.04e-2
    ## 6 TidyDensity          2 LM          Test  0.871 111.  0.712  86.9 1.12  2.63e-2
    ## 7 tidyAML              2 LM          Test  0.568 241.  1.13  104.  0.662 2.06e-2
    ## 8 RandomWalker         1 ARIMA       Test  1.22  108.  0.584 181.  1.40  1.60e-2

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
    ## 1 healthyR.data <tibble>     <tibble>     <split [1680|28]> <mdl_tm_t [1 × 5]>
    ## 2 healthyR      <tibble>     <tibble>     <split [1672|28]> <mdl_tm_t [1 × 5]>
    ## 3 healthyR.ts   <tibble>     <tibble>     <split [1617|28]> <mdl_tm_t [1 × 5]>
    ## 4 healthyverse  <tibble>     <tibble>     <split [1588|28]> <mdl_tm_t [1 × 5]>
    ## 5 healthyR.ai   <tibble>     <tibble>     <split [1413|28]> <mdl_tm_t [1 × 5]>
    ## 6 TidyDensity   <tibble>     <tibble>     <split [1264|28]> <mdl_tm_t [1 × 5]>
    ## 7 tidyAML       <tibble>     <tibble>     <split [872|28]>  <mdl_tm_t [1 × 5]>
    ## 8 RandomWalker  <tibble>     <tibble>     <split [294|28]>  <mdl_tm_t [1 × 5]>

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
