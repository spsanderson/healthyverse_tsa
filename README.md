# Time Series Analysis, Modeling and Forecasting of the Healthyverse Packages
Steven P. Sanderson II, MPH - Date:
2026-07-15

# Introduction

This analysis follows a *Nested Modeltime Workflow* from **`modeltime`**
along with using the **`NNS`** package. I use this to monitor the
downloads of all of my packages:

- [`healthyR`](https://www.spsanderson.com/healthyR/)
- [`healthyR.data`](https://www.spsanderson.com/healthyR.data/)
- [`healthyR.ts`](https://www.spsanderson.com/healthyR.ts/)
- [`healthyR.ai`](https://www.spsanderson.com/healthyR.ai/)
- [`healthyverse`](https://www.spsanderson.com/healthyverse/)
- [`TidyDensity`](https://www.spsanderson.com/TidyDensity/)
- [`tidyAML`](https://www.spsanderson.com/tidyAML/)
- [`RandomWalker`](https://www.spsanderson.com/RandomWalker/)

## Get Data

``` r
glimpse(downloads_tbl)
```

    Rows: 184,137
    Columns: 11
    $ date      <date> 2020-11-23, 2020-11-23, 2020-11-23, 2020-11-23, 2020-11-23,…
    $ time      <Period> 15H 36M 55S, 11H 26M 39S, 23H 34M 44S, 18H 39M 32S, 9H 0M…
    $ date_time <dttm> 2020-11-23 15:36:55, 2020-11-23 11:26:39, 2020-11-23 23:34:…
    $ size      <int> 4858294, 4858294, 4858301, 4858295, 361, 4863722, 4864794, 4…
    $ r_version <chr> NA, "4.0.3", "3.5.3", "3.5.2", NA, NA, NA, NA, NA, NA, NA, N…
    $ r_arch    <chr> NA, "x86_64", "x86_64", "x86_64", NA, NA, NA, NA, NA, NA, NA…
    $ r_os      <chr> NA, "mingw32", "mingw32", "linux-gnu", NA, NA, NA, NA, NA, N…
    $ package   <chr> "healthyR.data", "healthyR.data", "healthyR.data", "healthyR…
    $ version   <chr> "1.0.0", "1.0.0", "1.0.0", "1.0.0", "1.0.0", "1.0.0", "1.0.0…
    $ country   <chr> "US", "US", "US", "GB", "US", "US", "DE", "HK", "JP", "US", …
    $ ip_id     <int> 2069, 2804, 78827, 27595, 90474, 90474, 42435, 74, 7655, 638…

The last day in the data set is 2026-07-13 23:47:43, the file was
birthed on: 2025-10-31 10:47:59.603742, and at report knit time is 6129
hours old. Happy analyzing!

Now that we have our data lets take a look at it using the `skimr`
package.

``` r
skim(downloads_tbl)
```

|                                                  |               |
|:-------------------------------------------------|:--------------|
| Name                                             | downloads_tbl |
| Number of rows                                   | 184137        |
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
| r_version     |    138057 |          0.25 |   5 |  17 |     0 |       54 |          0 |
| r_arch        |    138057 |          0.25 |   1 |   7 |     0 |        7 |          0 |
| r_os          |    138057 |          0.25 |   7 |  33 |     0 |       32 |          0 |
| package       |         0 |          1.00 |   7 |  13 |     0 |        8 |          0 |
| version       |         0 |          1.00 |   5 |  17 |     0 |       63 |          0 |
| country       |     17937 |          0.90 |   2 |   2 |     0 |      170 |          0 |

**Variable type: Date**

| skim_variable | n_missing | complete_rate | min | max | median | n_unique |
|:---|---:|---:|:---|:---|:---|---:|
| date | 0 | 1 | 2020-11-23 | 2026-07-13 | 2024-02-10 | 2052 |

**Variable type: numeric**

| skim_variable | n_missing | complete_rate | mean | sd | p0 | p25 | p50 | p75 | p100 | hist |
|:---|---:|---:|---:|---:|---:|---:|---:|---:|---:|:---|
| size | 0 | 1 | 1132574.41 | 1473998.03 | 355 | 43656 | 325586 | 2334830 | 5677952 | ▇▁▂▁▁ |
| ip_id | 0 | 1 | 12064.06 | 25473.94 | 1 | 156 | 2697 | 11921 | 429286 | ▇▁▁▁▁ |

**Variable type: POSIXct**

| skim_variable | n_missing | complete_rate | min | max | median | n_unique |
|:---|---:|---:|:---|:---|:---|---:|
| date_time | 0 | 1 | 2020-11-23 09:00:41 | 2026-07-13 23:47:43 | 2024-02-10 10:14:15 | 117880 |

**Variable type: Timespan**

| skim_variable | n_missing | complete_rate | min | max |      median | n_unique |
|:--------------|----------:|--------------:|----:|----:|------------:|---------:|
| time          |         0 |             1 |   0 |  59 | 12H 12M 14S |       60 |

We can see that the following columns are missing a lot of data and for
us are most likely not useful anyways, so we will drop them
`c(r_version, r_arch, r_os)`

## Plots

Now lets take a look at a time-series plot of the total daily downloads
by package. We will use a log scale and place a vertical line at each
version release for each package.

![](man/figures/README-initial_ts_plot-1.png)

![](man/figures/README-initial_ts_plot-2.png)

    [[1]]

![](man/figures/README-initial_ts_plot-3.png)


    [[2]]

![](man/figures/README-initial_ts_plot-4.png)


    [[3]]

![](man/figures/README-initial_ts_plot-5.png)


    [[4]]

![](man/figures/README-initial_ts_plot-6.png)


    [[5]]

![](man/figures/README-initial_ts_plot-7.png)


    [[6]]

![](man/figures/README-initial_ts_plot-8.png)


    [[7]]

![](man/figures/README-initial_ts_plot-9.png)


    [[8]]

![](man/figures/README-initial_ts_plot-10.png)

Now lets take a look at some time series decomposition graphs.

    [[1]]

![](man/figures/README-ts_decomp_plt-1.png)


    [[2]]

![](man/figures/README-ts_decomp_plt-2.png)


    [[3]]

![](man/figures/README-ts_decomp_plt-3.png)


    [[4]]

![](man/figures/README-ts_decomp_plt-4.png)


    [[5]]

![](man/figures/README-ts_decomp_plt-5.png)


    [[6]]

![](man/figures/README-ts_decomp_plt-6.png)


    [[7]]

![](man/figures/README-ts_decomp_plt-7.png)


    [[8]]

![](man/figures/README-ts_decomp_plt-8.png)

    [[1]]

![](man/figures/README-ts_decomp_plt-9.png)


    [[2]]

![](man/figures/README-ts_decomp_plt-10.png)


    [[3]]

![](man/figures/README-ts_decomp_plt-11.png)


    [[4]]

![](man/figures/README-ts_decomp_plt-12.png)


    [[5]]

![](man/figures/README-ts_decomp_plt-13.png)


    [[6]]

![](man/figures/README-ts_decomp_plt-14.png)


    [[7]]

![](man/figures/README-ts_decomp_plt-15.png)


    [[8]]

![](man/figures/README-ts_decomp_plt-16.png)

Seasonal Diagnostics:

    [[1]]

![](man/figures/README-ts_decomp_seasonal_plt-1.png)


    [[2]]

![](man/figures/README-ts_decomp_seasonal_plt-2.png)


    [[3]]

![](man/figures/README-ts_decomp_seasonal_plt-3.png)


    [[4]]

![](man/figures/README-ts_decomp_seasonal_plt-4.png)


    [[5]]

![](man/figures/README-ts_decomp_seasonal_plt-5.png)


    [[6]]

![](man/figures/README-ts_decomp_seasonal_plt-6.png)


    [[7]]

![](man/figures/README-ts_decomp_seasonal_plt-7.png)


    [[8]]

![](man/figures/README-ts_decomp_seasonal_plt-8.png)

ACF and PACF Diagnostics:

    [[1]]

![](man/figures/README-ts_decomp_acf_plt-1.png)


    [[2]]

![](man/figures/README-ts_decomp_acf_plt-2.png)


    [[3]]

![](man/figures/README-ts_decomp_acf_plt-3.png)


    [[4]]

![](man/figures/README-ts_decomp_acf_plt-4.png)


    [[5]]

![](man/figures/README-ts_decomp_acf_plt-5.png)


    [[6]]

![](man/figures/README-ts_decomp_acf_plt-6.png)


    [[7]]

![](man/figures/README-ts_decomp_acf_plt-7.png)


    [[8]]

![](man/figures/README-ts_decomp_acf_plt-8.png)

## Feature Engineering

Now that we have our basic data and a shot of what it looks like, let’s
add some features to our data which can be very helpful in modeling.
Lets start by making a `tibble` that is aggregated by the day and
package, as we are going to be interested in forecasting the next 4
weeks or 28 days for each package. First lets get our base data.


    Call:
    stats::lm(formula = .formula, data = df)

    Residuals:
        Min      1Q  Median      3Q     Max 
    -152.86  -38.42  -11.79   28.38  828.41 

    Coefficients:
                                                         Estimate Std. Error
    (Intercept)                                        -1.557e+02  5.014e+01
    date                                                9.941e-03  2.650e-03
    lag(value, 1)                                       9.028e-02  2.207e-02
    lag(value, 7)                                       6.419e-02  2.265e-02
    lag(value, 14)                                      7.010e-02  2.255e-02
    lag(value, 21)                                      8.398e-02  2.262e-02
    lag(value, 28)                                      7.828e-02  2.254e-02
    lag(value, 35)                                      3.962e-02  2.265e-02
    lag(value, 42)                                      6.482e-02  2.276e-02
    lag(value, 49)                                      7.968e-02  2.271e-02
    month(date, label = TRUE).L                        -8.443e+00  4.755e+00
    month(date, label = TRUE).Q                         1.391e-01  4.683e+00
    month(date, label = TRUE).C                        -1.593e+01  4.732e+00
    month(date, label = TRUE)^4                        -8.638e+00  4.741e+00
    month(date, label = TRUE)^5                        -4.126e+00  4.736e+00
    month(date, label = TRUE)^6                        -1.470e+00  4.750e+00
    month(date, label = TRUE)^7                        -3.878e+00  4.697e+00
    month(date, label = TRUE)^8                        -3.558e+00  4.673e+00
    month(date, label = TRUE)^9                         2.080e+00  4.682e+00
    month(date, label = TRUE)^10                        3.019e-01  4.702e+00
    month(date, label = TRUE)^11                       -2.245e+00  4.635e+00
    fourier_vec(date, type = "sin", K = 1, period = 7) -1.082e+01  2.104e+00
    fourier_vec(date, type = "cos", K = 1, period = 7)  7.780e+00  2.163e+00
                                                       t value Pr(>|t|)    
    (Intercept)                                         -3.104 0.001934 ** 
    date                                                 3.751 0.000181 ***
    lag(value, 1)                                        4.091 4.47e-05 ***
    lag(value, 7)                                        2.835 0.004635 ** 
    lag(value, 14)                                       3.109 0.001904 ** 
    lag(value, 21)                                       3.713 0.000210 ***
    lag(value, 28)                                       3.472 0.000527 ***
    lag(value, 35)                                       1.750 0.080358 .  
    lag(value, 42)                                       2.848 0.004448 ** 
    lag(value, 49)                                       3.508 0.000461 ***
    month(date, label = TRUE).L                         -1.776 0.075937 .  
    month(date, label = TRUE).Q                          0.030 0.976312    
    month(date, label = TRUE).C                         -3.366 0.000777 ***
    month(date, label = TRUE)^4                         -1.822 0.068571 .  
    month(date, label = TRUE)^5                         -0.871 0.383735    
    month(date, label = TRUE)^6                         -0.309 0.756994    
    month(date, label = TRUE)^7                         -0.826 0.409154    
    month(date, label = TRUE)^8                         -0.761 0.446499    
    month(date, label = TRUE)^9                          0.444 0.656806    
    month(date, label = TRUE)^10                         0.064 0.948815    
    month(date, label = TRUE)^11                        -0.484 0.628171    
    fourier_vec(date, type = "sin", K = 1, period = 7)  -5.140 3.03e-07 ***
    fourier_vec(date, type = "cos", K = 1, period = 7)   3.597 0.000330 ***
    ---
    Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

    Residual standard error: 60.3 on 1980 degrees of freedom
      (49 observations deleted due to missingness)
    Multiple R-squared:  0.2004,    Adjusted R-squared:  0.1915 
    F-statistic: 22.55 on 22 and 1980 DF,  p-value: < 2.2e-16

![](man/figures/README-base_data_frame-1.png)

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

    Package: healthyR
    [1] "CURRNET METHOD: lin"
    [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 16 ) ...)"
    [1] "CURRENT lin OBJECTIVE FUNCTION = 17.6792231570542"
    [1] "BEST method = 'lin' PATH MEMBER = c( 16 )"
    [1] "BEST lin OBJECTIVE FUNCTION = 17.6792231570542"
    [1] "CURRNET METHOD: nonlin"
    [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    [1] "NNS.ARMA(... method =  'nonlin' , seasonal.factor =  c( 16 ) ...)"
    [1] "CURRENT nonlin OBJECTIVE FUNCTION = 5.78331060937476"
    [1] "BEST method = 'nonlin' PATH MEMBER = c( 16 )"
    [1] "BEST nonlin OBJECTIVE FUNCTION = 5.78331060937476"
    [1] "CURRNET METHOD: both"
    [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    [1] "NNS.ARMA(... method =  'both' , seasonal.factor =  c( 16 ) ...)"
    [1] "CURRENT both OBJECTIVE FUNCTION = 11.6095719109288"
    [1] "BEST method = 'both' PATH MEMBER = c( 16 )"
    [1] "BEST both OBJECTIVE FUNCTION = 11.6095719109288"

![](man/figures/README-nns_forecasting-1.png)

    Package: healthyR.ai
    [1] "CURRNET METHOD: lin"
    [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 21 ) ...)"
    [1] "CURRENT lin OBJECTIVE FUNCTION = 5.05469496379072"
    [1] "BEST method = 'lin' PATH MEMBER = c( 21 )"
    [1] "BEST lin OBJECTIVE FUNCTION = 5.05469496379072"
    [1] "CURRNET METHOD: nonlin"
    [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    [1] "NNS.ARMA(... method =  'nonlin' , seasonal.factor =  c( 21 ) ...)"
    [1] "CURRENT nonlin OBJECTIVE FUNCTION = 7.62540294056938"
    [1] "BEST method = 'nonlin' PATH MEMBER = c( 21 )"
    [1] "BEST nonlin OBJECTIVE FUNCTION = 7.62540294056938"
    [1] "CURRNET METHOD: both"
    [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    [1] "NNS.ARMA(... method =  'both' , seasonal.factor =  c( 21 ) ...)"
    [1] "CURRENT both OBJECTIVE FUNCTION = 6.99044469129244"
    [1] "BEST method = 'both' PATH MEMBER = c( 21 )"
    [1] "BEST both OBJECTIVE FUNCTION = 6.99044469129244"

![](man/figures/README-nns_forecasting-2.png)

    Package: healthyR.data
    [1] "CURRNET METHOD: lin"
    [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 21 ) ...)"
    [1] "CURRENT lin OBJECTIVE FUNCTION = 7.53502091406156"
    [1] "BEST method = 'lin' PATH MEMBER = c( 21 )"
    [1] "BEST lin OBJECTIVE FUNCTION = 7.53502091406156"
    [1] "CURRNET METHOD: nonlin"
    [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    [1] "NNS.ARMA(... method =  'nonlin' , seasonal.factor =  c( 21 ) ...)"
    [1] "CURRENT nonlin OBJECTIVE FUNCTION = 7.65631542595025"
    [1] "BEST method = 'nonlin' PATH MEMBER = c( 21 )"
    [1] "BEST nonlin OBJECTIVE FUNCTION = 7.65631542595025"
    [1] "CURRNET METHOD: both"
    [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    [1] "NNS.ARMA(... method =  'both' , seasonal.factor =  c( 21 ) ...)"
    [1] "CURRENT both OBJECTIVE FUNCTION = 6.73289236064946"
    [1] "BEST method = 'both' PATH MEMBER = c( 21 )"
    [1] "BEST both OBJECTIVE FUNCTION = 6.73289236064946"

![](man/figures/README-nns_forecasting-3.png)

    Package: healthyR.ts
    [1] "CURRNET METHOD: lin"
    [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 10 ) ...)"
    [1] "CURRENT lin OBJECTIVE FUNCTION = 39.8271462578578"
    [1] "BEST method = 'lin' PATH MEMBER = c( 10 )"
    [1] "BEST lin OBJECTIVE FUNCTION = 39.8271462578578"
    [1] "CURRNET METHOD: nonlin"
    [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    [1] "NNS.ARMA(... method =  'nonlin' , seasonal.factor =  c( 10 ) ...)"
    [1] "CURRENT nonlin OBJECTIVE FUNCTION = 35.1882521971521"
    [1] "BEST method = 'nonlin' PATH MEMBER = c( 10 )"
    [1] "BEST nonlin OBJECTIVE FUNCTION = 35.1882521971521"
    [1] "CURRNET METHOD: both"
    [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    [1] "NNS.ARMA(... method =  'both' , seasonal.factor =  c( 10 ) ...)"
    [1] "CURRENT both OBJECTIVE FUNCTION = 49.4814608756934"
    [1] "BEST method = 'both' PATH MEMBER = c( 10 )"
    [1] "BEST both OBJECTIVE FUNCTION = 49.4814608756934"

![](man/figures/README-nns_forecasting-4.png)

    Package: healthyverse
    [1] "CURRNET METHOD: lin"
    [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 10 ) ...)"
    [1] "CURRENT lin OBJECTIVE FUNCTION = 8.46924399364743"
    [1] "BEST method = 'lin' PATH MEMBER = c( 10 )"
    [1] "BEST lin OBJECTIVE FUNCTION = 8.46924399364743"
    [1] "CURRNET METHOD: nonlin"
    [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    [1] "NNS.ARMA(... method =  'nonlin' , seasonal.factor =  c( 10 ) ...)"
    [1] "CURRENT nonlin OBJECTIVE FUNCTION = 19.1607695783333"
    [1] "BEST method = 'nonlin' PATH MEMBER = c( 10 )"
    [1] "BEST nonlin OBJECTIVE FUNCTION = 19.1607695783333"
    [1] "CURRNET METHOD: both"
    [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    [1] "NNS.ARMA(... method =  'both' , seasonal.factor =  c( 10 ) ...)"
    [1] "CURRENT both OBJECTIVE FUNCTION = 10.1225591345656"
    [1] "BEST method = 'both' PATH MEMBER = c( 10 )"
    [1] "BEST both OBJECTIVE FUNCTION = 10.1225591345656"

![](man/figures/README-nns_forecasting-5.png)

    Package: RandomWalker
    [1] "CURRNET METHOD: lin"
    [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 2 ) ...)"
    [1] "CURRENT lin OBJECTIVE FUNCTION = 41.091953677621"
    [1] "BEST method = 'lin' PATH MEMBER = c( 2 )"
    [1] "BEST lin OBJECTIVE FUNCTION = 41.091953677621"
    [1] "CURRNET METHOD: nonlin"
    [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    [1] "NNS.ARMA(... method =  'nonlin' , seasonal.factor =  c( 2 ) ...)"
    [1] "CURRENT nonlin OBJECTIVE FUNCTION = 13.4589304302983"
    [1] "BEST method = 'nonlin' PATH MEMBER = c( 2 )"
    [1] "BEST nonlin OBJECTIVE FUNCTION = 13.4589304302983"
    [1] "CURRNET METHOD: both"
    [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    [1] "NNS.ARMA(... method =  'both' , seasonal.factor =  c( 2 ) ...)"
    [1] "CURRENT both OBJECTIVE FUNCTION = 12.7778958611105"
    [1] "BEST method = 'both' PATH MEMBER = c( 2 )"
    [1] "BEST both OBJECTIVE FUNCTION = 12.7778958611105"

![](man/figures/README-nns_forecasting-6.png)

    Package: tidyAML
    [1] "CURRNET METHOD: lin"
    [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 16 ) ...)"
    [1] "CURRENT lin OBJECTIVE FUNCTION = 11.8854908199367"
    [1] "BEST method = 'lin' PATH MEMBER = c( 16 )"
    [1] "BEST lin OBJECTIVE FUNCTION = 11.8854908199367"
    [1] "CURRNET METHOD: nonlin"
    [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    [1] "NNS.ARMA(... method =  'nonlin' , seasonal.factor =  c( 16 ) ...)"
    [1] "CURRENT nonlin OBJECTIVE FUNCTION = 4.49300442365787"
    [1] "BEST method = 'nonlin' PATH MEMBER = c( 16 )"
    [1] "BEST nonlin OBJECTIVE FUNCTION = 4.49300442365787"
    [1] "CURRNET METHOD: both"
    [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    [1] "NNS.ARMA(... method =  'both' , seasonal.factor =  c( 16 ) ...)"
    [1] "CURRENT both OBJECTIVE FUNCTION = 5.642484469865"
    [1] "BEST method = 'both' PATH MEMBER = c( 16 )"
    [1] "BEST both OBJECTIVE FUNCTION = 5.642484469865"

![](man/figures/README-nns_forecasting-7.png)

    Package: TidyDensity
    [1] "CURRNET METHOD: lin"
    [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 17 ) ...)"
    [1] "CURRENT lin OBJECTIVE FUNCTION = 6.03144244036495"
    [1] "BEST method = 'lin' PATH MEMBER = c( 17 )"
    [1] "BEST lin OBJECTIVE FUNCTION = 6.03144244036495"
    [1] "CURRNET METHOD: nonlin"
    [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    [1] "NNS.ARMA(... method =  'nonlin' , seasonal.factor =  c( 17 ) ...)"
    [1] "CURRENT nonlin OBJECTIVE FUNCTION = 6.7745848941191"
    [1] "BEST method = 'nonlin' PATH MEMBER = c( 17 )"
    [1] "BEST nonlin OBJECTIVE FUNCTION = 6.7745848941191"
    [1] "CURRNET METHOD: both"
    [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    [1] "NNS.ARMA(... method =  'both' , seasonal.factor =  c( 17 ) ...)"
    [1] "CURRENT both OBJECTIVE FUNCTION = 5.93929283947732"
    [1] "BEST method = 'both' PATH MEMBER = c( 17 )"
    [1] "BEST both OBJECTIVE FUNCTION = 5.93929283947732"

![](man/figures/README-nns_forecasting-8.png)

    [[1]]
    NULL

    [[2]]
    NULL

    [[3]]
    NULL

    [[4]]
    NULL

    [[5]]
    NULL

    [[6]]
    NULL

    [[7]]
    NULL

    [[8]]
    NULL

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
  group_by(package) %>%
  # get standardization
  mutate(value_trans = standard_vec(value_trans, silent = TRUE)$standard_scaled) %>%
  tk_augment_fourier(
    .date_var = date,
    .periods  = c(7, 14, 30, 90, 180),
    .K        = 2
  ) %>%
  tk_augment_timeseries_signature(
    .date_var = date
  ) %>%
  ungroup() %>%
  select(-c(value, -year.iso))
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

    # 0. Filter out column where package is NA
    filter(!is.na(package)) %>%
    
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

    # A tibble: 8 × 4
      package       .actual_data          .future_data       .splits          
      <fct>         <list>                <list>             <list>           
    1 healthyR.data <tibble [2,040 × 50]> <tibble [28 × 50]> <split [2012|28]>
    2 healthyR      <tibble [2,034 × 50]> <tibble [28 × 50]> <split [2006|28]>
    3 healthyR.ts   <tibble [1,970 × 50]> <tibble [28 × 50]> <split [1942|28]>
    4 healthyverse  <tibble [1,876 × 50]> <tibble [28 × 50]> <split [1848|28]>
    5 healthyR.ai   <tibble [1,775 × 50]> <tibble [28 × 50]> <split [1747|28]>
    6 TidyDensity   <tibble [1,628 × 50]> <tibble [28 × 50]> <split [1600|28]>
    7 tidyAML       <tibble [1,232 × 50]> <tibble [28 × 50]> <split [1204|28]>
    8 RandomWalker  <tibble [656 × 50]>   <tibble [28 × 50]> <split [628|28]> 

Now it is time to make some recipes and models using the modeltime
workflow.

## Modeltime Workflow

### Recipe Object

``` r
recipe_base <- recipe(
  value_trans ~ .
  , data = extract_nested_test_split(nested_data_tbl)
  )

recipe_base

recipe_date <- recipe(
  value_trans ~ date
  , data = extract_nested_test_split(nested_data_tbl)
  )
```

### Models

``` r
# Models ------------------------------------------------------------------

# Auto ARIMA --------------------------------------------------------------

model_spec_arima_no_boost <- arima_reg() %>%
  set_engine(engine = "auto_arima")

wflw_auto_arima <- workflow() %>%
  add_recipe(recipe = recipe_date) %>%
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
  add_recipe(recipe = recipe_date) %>%
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
| healthyR.data | 1 | ARIMA | Test | 0.7598657 | 129.46485 | 0.9099027 | 168.99704 | 0.9252129 | 0.0069336 |
| healthyR.data | 2 | LM | Test | 0.7598623 | 178.52411 | 0.9098986 | 151.23124 | 0.9033881 | 0.0173762 |
| healthyR.data | 3 | EARTH | Test | 1.3627898 | 407.16104 | 1.6318753 | 147.19673 | 1.5762919 | 0.0085353 |
| healthyR.data | 4 | NNAR | Test | 0.7358542 | 209.99626 | 0.8811501 | 150.99760 | 0.8752331 | 0.0438819 |
| healthyR | 1 | ARIMA | Test | 0.8411334 | 164.58702 | 0.9518905 | 153.52861 | 1.1031835 | 0.0644204 |
| healthyR | 2 | LM | Test | 0.8614434 | 313.21394 | 0.9748748 | 152.15199 | 1.0533186 | 0.0544381 |
| healthyR | 3 | EARTH | Test | 2.1423860 | 1381.90589 | 2.4244869 | 162.74110 | 2.4762401 | 0.0328241 |
| healthyR | 4 | NNAR | Test | 0.8420230 | 387.60520 | 0.9528972 | 147.82799 | 1.0315088 | 0.0906652 |
| healthyR.ts | 1 | ARIMA | Test | 0.6789577 | 683.98901 | 0.8367363 | 177.88995 | 0.9414874 | 0.1097370 |
| healthyR.ts | 2 | LM | Test | 0.7017030 | 3128.88920 | 0.8647672 | 157.26248 | 0.9290658 | 0.0156046 |
| healthyR.ts | 3 | EARTH | Test | 2.2770634 | 12057.35825 | 2.8062152 | 173.56229 | 2.5543673 | 0.0261623 |
| healthyR.ts | 4 | NNAR | Test | 0.6603424 | 3351.12121 | 0.8137951 | 150.35788 | 0.8819517 | 0.0036122 |
| healthyverse | 1 | ARIMA | Test | 0.5380251 | 83.52367 | 0.8067682 | 43.76813 | 0.6616776 | 0.0568576 |
| healthyverse | 2 | LM | Test | 0.8725393 | 63.16490 | 1.3083720 | 79.23171 | 1.0166192 | 0.0048485 |
| healthyverse | 3 | EARTH | Test | 0.4993915 | 94.48420 | 0.7488371 | 39.88568 | 0.7030764 | 0.1106262 |
| healthyverse | 4 | NNAR | Test | 1.0561249 | 73.64847 | 1.5836584 | 110.37287 | 1.2203348 | 0.0034699 |
| healthyR.ai | 1 | ARIMA | Test | 0.6855381 | 91.43078 | 1.0489565 | 138.55867 | 0.8608497 | 0.0314136 |
| healthyR.ai | 2 | LM | Test | 0.6922543 | 111.98739 | 1.0592332 | 113.49825 | 0.8865219 | 0.0041343 |
| healthyR.ai | 3 | EARTH | Test | 2.6163909 | 684.22877 | 4.0033957 | 163.91194 | 2.9405596 | 0.0731172 |
| healthyR.ai | 4 | NNAR | Test | 0.7614541 | 135.15812 | 1.1651171 | 134.71058 | 0.9350524 | 0.0003008 |
| TidyDensity | 1 | ARIMA | Test | 1.0224723 | 396.79202 | 0.9459432 | 160.28352 | 1.1415517 | 0.0000091 |
| TidyDensity | 2 | LM | Test | 0.9841928 | 423.87722 | 0.9105287 | 163.04643 | 1.1094183 | 0.0122265 |
| TidyDensity | 3 | EARTH | Test | 1.0432172 | 362.69301 | 0.9651354 | 158.69853 | 1.1618898 | 0.0731101 |
| TidyDensity | 4 | NNAR | Test | 1.0219253 | 454.28958 | 0.9454371 | 167.57167 | 1.1310208 | 0.0180073 |
| tidyAML | 1 | ARIMA | Test | 0.7864464 | 109.45963 | 1.0228755 | 180.92289 | 0.9889310 | 0.0103806 |
| tidyAML | 2 | LM | Test | 0.7248264 | 154.71310 | 0.9427308 | 126.14596 | 0.9642476 | 0.1853897 |
| tidyAML | 3 | EARTH | Test | 0.8606048 | 212.09498 | 1.1193282 | 158.18068 | 1.0222019 | 0.0293999 |
| tidyAML | 4 | NNAR | Test | 0.6958678 | 128.10819 | 0.9050663 | 133.55637 | 0.9420325 | 0.0851025 |
| RandomWalker | 1 | ARIMA | Test | 0.8845239 | 212.25415 | 0.7514093 | 181.01101 | 1.0065290 | 0.0063441 |
| RandomWalker | 2 | LM | Test | 0.8554860 | 145.92786 | 0.7267414 | 173.46773 | 1.0120680 | 0.0092338 |
| RandomWalker | 3 | EARTH | Test | 0.8196422 | 113.65695 | 0.6962918 | 181.76030 | 0.9735422 | 0.0351570 |
| RandomWalker | 4 | NNAR | Test | 0.9264818 | 141.68388 | 0.7870528 | 156.31143 | 1.1019383 | 0.0301023 |

### Plot Models

``` r
nested_modeltime_tbl %>%
  extract_nested_test_forecast() %>%
  group_by(package) %>%
  filter_by_time(.date_var = .index, .start_date = max(.index) - 60) %>%
  ungroup() %>%
  plot_modeltime_forecast(
    .interactive = FALSE,
    .conf_interval_show  = FALSE,
    .facet_scales = "free"
  ) +
  theme_minimal() +
  facet_wrap(~ package, nrow = 3) +
  theme(legend.position = "bottom")
```

![](man/figures/README-model_plot-1.png)

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

    # Nested Modeltime Table
      

    # A tibble: 8 × 10
      package     .model_id .model_desc .type   mae   mape  mase smape  rmse     rsq
      <fct>           <int> <chr>       <chr> <dbl>  <dbl> <dbl> <dbl> <dbl>   <dbl>
    1 healthyR.d…         4 NNAR        Test  0.736  210.  0.881 151.  0.875 0.0439 
    2 healthyR            4 NNAR        Test  0.842  388.  0.953 148.  1.03  0.0907 
    3 healthyR.ts         4 NNAR        Test  0.660 3351.  0.814 150.  0.882 0.00361
    4 healthyver…         1 ARIMA       Test  0.538   83.5 0.807  43.8 0.662 0.0569 
    5 healthyR.ai         1 ARIMA       Test  0.686   91.4 1.05  139.  0.861 0.0314 
    6 TidyDensity         2 LM          Test  0.984  424.  0.911 163.  1.11  0.0122 
    7 tidyAML             4 NNAR        Test  0.696  128.  0.905 134.  0.942 0.0851 
    8 RandomWalk…         3 EARTH       Test  0.820  114.  0.696 182.  0.974 0.0352 

``` r
best_nested_modeltime_tbl %>%
  extract_nested_test_forecast() %>%
  #filter(!is.na(.model_id)) %>%
  group_by(package) %>%
  filter_by_time(.date_var = .index, .start_date = max(.index) - 60) %>%
  ungroup() %>%
  plot_modeltime_forecast(
    .interactive = FALSE,
    .conf_interval_alpha = 0.2,
    .facet_scales = "free"
  ) +
  facet_wrap(~ package, nrow = 3) +
  theme_minimal() +
  theme(legend.position = "bottom")
```

![](man/figures/README-best_model-1.png)

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

    # Nested Modeltime Table
      

    # A tibble: 8 × 5
      package       .actual_data .future_data .splits           .modeltime_tables 
      <fct>         <list>       <list>       <list>            <list>            
    1 healthyR.data <tibble>     <tibble>     <split [2012|28]> <mdl_tm_t [1 × 5]>
    2 healthyR      <tibble>     <tibble>     <split [2006|28]> <mdl_tm_t [1 × 5]>
    3 healthyR.ts   <tibble>     <tibble>     <split [1942|28]> <mdl_tm_t [1 × 5]>
    4 healthyverse  <tibble>     <tibble>     <split [1848|28]> <mdl_tm_t [1 × 5]>
    5 healthyR.ai   <tibble>     <tibble>     <split [1747|28]> <mdl_tm_t [1 × 5]>
    6 TidyDensity   <tibble>     <tibble>     <split [1600|28]> <mdl_tm_t [1 × 5]>
    7 tidyAML       <tibble>     <tibble>     <split [1204|28]> <mdl_tm_t [1 × 5]>
    8 RandomWalker  <tibble>     <tibble>     <split [628|28]>  <mdl_tm_t [1 × 5]>

``` r
nested_modeltime_refit_tbl %>%
  extract_nested_future_forecast() %>%
  group_by(package) %>%
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
  filter_by_time(.date_var = .index, .start_date = max(.index) - 60) %>%
  ungroup() %>%
  plot_modeltime_forecast(
    .interactive = FALSE,
    .conf_interval_alpha = 0.2,
    .facet_scales = "free"
  ) +
  facet_wrap(~ package, nrow = 3) +
  theme_minimal() +
  theme(legend.position = "bottom")
```

![](man/figures/README-refit-1.png)
