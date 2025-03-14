Time Series Analysis and Nested Modeling of the Healthyverse Packages
================
Steven P. Sanderson II, MPH - Date:
14 March, 2025

This analysis follows a *Nested Modeltime Workflow*.

## Get Data

``` r
glimpse(downloads_tbl)
```

    ## Rows: 134,006
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

The last day in the data set is 2025-03-12 23:51:21, the file was
birthed on: 2022-07-02 23:58:17.511888, and at report knit time is
-2.361188^{4} hours old. Happy analyzing!

Now that we have our data lets take a look at it using the `skimr`
package.

``` r
skim(downloads_tbl)
```

|                                                  |               |
|:-------------------------------------------------|:--------------|
| Name                                             | downloads_tbl |
| Number of rows                                   | 134006        |
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
| r_version     |     96244 |          0.28 |   5 |   5 |     0 |       46 |          0 |
| r_arch        |     96244 |          0.28 |   3 |   7 |     0 |        5 |          0 |
| r_os          |     96244 |          0.28 |   7 |  15 |     0 |       21 |          0 |
| package       |         0 |          1.00 |   7 |  13 |     0 |        8 |          0 |
| version       |         0 |          1.00 |   5 |  17 |     0 |       60 |          0 |
| country       |     11289 |          0.92 |   2 |   2 |     0 |      163 |          0 |

**Variable type: Date**

| skim_variable | n_missing | complete_rate | min | max | median | n_unique |
|:---|---:|---:|:---|:---|:---|---:|
| date | 0 | 1 | 2020-11-23 | 2025-03-12 | 2023-05-17 | 1571 |

**Variable type: numeric**

| skim_variable | n_missing | complete_rate | mean | sd | p0 | p25 | p50 | p75 | p100 | hist |
|:---|---:|---:|---:|---:|---:|---:|---:|---:|---:|:---|
| size | 0 | 1 | 1137071.22 | 1526215.60 | 355 | 14701 | 261842.5 | 2367795.00 | 5677952 | ▇▁▂▁▁ |
| ip_id | 0 | 1 | 10384.84 | 18374.92 | 1 | 298 | 3091.0 | 11837.75 | 209747 | ▇▁▁▁▁ |

**Variable type: POSIXct**

| skim_variable | n_missing | complete_rate | min | max | median | n_unique |
|:---|---:|---:|:---|:---|:---|---:|
| date_time | 0 | 1 | 2020-11-23 09:00:41 | 2025-03-12 23:51:21 | 2023-05-17 20:11:23 | 81373 |

**Variable type: Timespan**

| skim_variable | n_missing | complete_rate | min | max | median | n_unique |
|:--------------|----------:|--------------:|----:|----:|-------:|---------:|
| time          |         0 |             1 |   0 |  59 |   30.5 |       60 |

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
    ## -153.48  -35.08  -10.22   26.67  811.02 
    ## 
    ## Coefficients:
    ##                                                      Estimate Std. Error
    ## (Intercept)                                        -2.016e+02  7.516e+01
    ## date                                                1.205e-02  3.988e-03
    ## lag(value, 1)                                       1.121e-01  2.499e-02
    ## lag(value, 7)                                       9.004e-02  2.589e-02
    ## lag(value, 14)                                      9.439e-02  2.589e-02
    ## lag(value, 21)                                      6.793e-02  2.607e-02
    ## lag(value, 28)                                      6.378e-02  2.594e-02
    ## lag(value, 35)                                      6.713e-02  2.613e-02
    ## lag(value, 42)                                      5.538e-02  2.626e-02
    ## lag(value, 49)                                      8.529e-02  2.616e-02
    ## month(date, label = TRUE).L                        -1.161e+01  5.176e+00
    ## month(date, label = TRUE).Q                         2.468e+00  5.190e+00
    ## month(date, label = TRUE).C                        -1.144e+01  5.262e+00
    ## month(date, label = TRUE)^4                        -8.007e+00  5.229e+00
    ## month(date, label = TRUE)^5                        -1.204e+01  5.205e+00
    ## month(date, label = TRUE)^6                        -2.561e+00  5.286e+00
    ## month(date, label = TRUE)^7                        -7.423e+00  5.204e+00
    ## month(date, label = TRUE)^8                        -3.699e+00  5.239e+00
    ## month(date, label = TRUE)^9                         5.049e+00  5.279e+00
    ## month(date, label = TRUE)^10                        4.794e+00  5.307e+00
    ## month(date, label = TRUE)^11                       -6.130e+00  5.328e+00
    ## fourier_vec(date, type = "sin", K = 1, period = 7) -1.193e+01  2.402e+00
    ## fourier_vec(date, type = "cos", K = 1, period = 7)  7.935e+00  2.532e+00
    ##                                                    t value Pr(>|t|)    
    ## (Intercept)                                         -2.683 0.007387 ** 
    ## date                                                 3.023 0.002544 ** 
    ## lag(value, 1)                                        4.486 7.81e-06 ***
    ## lag(value, 7)                                        3.478 0.000519 ***
    ## lag(value, 14)                                       3.646 0.000275 ***
    ## lag(value, 21)                                       2.605 0.009269 ** 
    ## lag(value, 28)                                       2.458 0.014068 *  
    ## lag(value, 35)                                       2.569 0.010304 *  
    ## lag(value, 42)                                       2.109 0.035087 *  
    ## lag(value, 49)                                       3.260 0.001138 ** 
    ## month(date, label = TRUE).L                         -2.243 0.025073 *  
    ## month(date, label = TRUE).Q                          0.475 0.634570    
    ## month(date, label = TRUE).C                         -2.175 0.029803 *  
    ## month(date, label = TRUE)^4                         -1.531 0.125933    
    ## month(date, label = TRUE)^5                         -2.314 0.020819 *  
    ## month(date, label = TRUE)^6                         -0.485 0.628096    
    ## month(date, label = TRUE)^7                         -1.426 0.153943    
    ## month(date, label = TRUE)^8                         -0.706 0.480278    
    ## month(date, label = TRUE)^9                          0.956 0.339059    
    ## month(date, label = TRUE)^10                         0.903 0.366471    
    ## month(date, label = TRUE)^11                        -1.150 0.250159    
    ## fourier_vec(date, type = "sin", K = 1, period = 7)  -4.967 7.60e-07 ***
    ## fourier_vec(date, type = "cos", K = 1, period = 7)   3.134 0.001761 ** 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 58.49 on 1499 degrees of freedom
    ##   (49 observations deleted due to missingness)
    ## Multiple R-squared:  0.2586, Adjusted R-squared:  0.2477 
    ## F-statistic: 23.76 on 22 and 1499 DF,  p-value: < 2.2e-16

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
            sf <- NNS.seas(x, modulo = 7, plot = FALSE)$periods
            
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
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 77 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 1.34122202794851"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 77, 98 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 1.15915126999858"
    ## [1] "BEST method = 'lin', seasonal.factor = c( 77, 98 )"
    ## [1] "BEST lin OBJECTIVE FUNCTION = 1.15915126999858"
    ## [1] "CURRNET METHOD: nonlin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'nonlin' , seasonal.factor =  c( 77, 98 ) ...)"
    ## [1] "CURRENT nonlin OBJECTIVE FUNCTION = 1.36636061157614"
    ## [1] "BEST method = 'nonlin' PATH MEMBER = c( 77, 98 )"
    ## [1] "BEST nonlin OBJECTIVE FUNCTION = 1.36636061157614"
    ## [1] "CURRNET METHOD: both"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'both' , seasonal.factor =  c( 77, 98 ) ...)"
    ## [1] "CURRENT both OBJECTIVE FUNCTION = 1.17499077243005"
    ## [1] "BEST method = 'both' PATH MEMBER = c( 77, 98 )"
    ## [1] "BEST both OBJECTIVE FUNCTION = 1.17499077243005"

![](man/figures/README-nns_forecasting-1.png)<!-- -->

    ## Package: healthyR.ai
    ## [1] "CURRNET METHOD: lin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 21 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 1.49698178771826"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 21, 98 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 1.29457206401755"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 21, 98, 77 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 1.21697053896808"
    ## [1] "BEST method = 'lin', seasonal.factor = c( 21, 98, 77 )"
    ## [1] "BEST lin OBJECTIVE FUNCTION = 1.21697053896808"
    ## [1] "CURRNET METHOD: nonlin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'nonlin' , seasonal.factor =  c( 21, 98, 77 ) ...)"
    ## [1] "CURRENT nonlin OBJECTIVE FUNCTION = 1.25423282152751"
    ## [1] "BEST method = 'nonlin' PATH MEMBER = c( 21, 98, 77 )"
    ## [1] "BEST nonlin OBJECTIVE FUNCTION = 1.25423282152751"
    ## [1] "CURRNET METHOD: both"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'both' , seasonal.factor =  c( 21, 98, 77 ) ...)"
    ## [1] "CURRENT both OBJECTIVE FUNCTION = 1.17012341412543"
    ## [1] "BEST method = 'both' PATH MEMBER = c( 21, 98, 77 )"
    ## [1] "BEST both OBJECTIVE FUNCTION = 1.17012341412543"

![](man/figures/README-nns_forecasting-2.png)<!-- -->

    ## Package: healthyR.data
    ## [1] "CURRNET METHOD: lin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 98 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 1.42997535224085"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 98, 77 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 1.27509888893953"
    ## [1] "BEST method = 'lin', seasonal.factor = c( 98, 77 )"
    ## [1] "BEST lin OBJECTIVE FUNCTION = 1.27509888893953"
    ## [1] "CURRNET METHOD: nonlin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'nonlin' , seasonal.factor =  c( 98, 77 ) ...)"
    ## [1] "CURRENT nonlin OBJECTIVE FUNCTION = 1.73946435114426"
    ## [1] "BEST method = 'nonlin' PATH MEMBER = c( 98, 77 )"
    ## [1] "BEST nonlin OBJECTIVE FUNCTION = 1.73946435114426"
    ## [1] "CURRNET METHOD: both"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'both' , seasonal.factor =  c( 98, 77 ) ...)"
    ## [1] "CURRENT both OBJECTIVE FUNCTION = 1.4416639633172"
    ## [1] "BEST method = 'both' PATH MEMBER = c( 98, 77 )"
    ## [1] "BEST both OBJECTIVE FUNCTION = 1.4416639633172"

![](man/figures/README-nns_forecasting-3.png)<!-- -->

    ## Package: healthyR.ts
    ## [1] "CURRNET METHOD: lin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 14 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 1.37527643731309"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 14, 98 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 1.11886530073828"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 14, 98, 63 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 1.0654675586814"
    ## [1] "BEST method = 'lin', seasonal.factor = c( 14, 98, 63 )"
    ## [1] "BEST lin OBJECTIVE FUNCTION = 1.0654675586814"
    ## [1] "CURRNET METHOD: nonlin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'nonlin' , seasonal.factor =  c( 14, 98, 63 ) ...)"
    ## [1] "CURRENT nonlin OBJECTIVE FUNCTION = 1.27126335043048"
    ## [1] "BEST method = 'nonlin' PATH MEMBER = c( 14, 98, 63 )"
    ## [1] "BEST nonlin OBJECTIVE FUNCTION = 1.27126335043048"
    ## [1] "CURRNET METHOD: both"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'both' , seasonal.factor =  c( 14, 98, 63 ) ...)"
    ## [1] "CURRENT both OBJECTIVE FUNCTION = 1.11542347698346"
    ## [1] "BEST method = 'both' PATH MEMBER = c( 14, 98, 63 )"
    ## [1] "BEST both OBJECTIVE FUNCTION = 1.11542347698346"

![](man/figures/README-nns_forecasting-4.png)<!-- -->

    ## Package: healthyverse
    ## [1] "CURRNET METHOD: lin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 49 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 1.5930819436175"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 49, 77 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 1.36838340260092"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 49, 77, 70 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 1.3361906082031"
    ## [1] "BEST method = 'lin', seasonal.factor = c( 49, 77, 70 )"
    ## [1] "BEST lin OBJECTIVE FUNCTION = 1.3361906082031"
    ## [1] "CURRNET METHOD: nonlin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'nonlin' , seasonal.factor =  c( 49, 77, 70 ) ...)"
    ## [1] "CURRENT nonlin OBJECTIVE FUNCTION = 2.57205945241091"
    ## [1] "BEST method = 'nonlin' PATH MEMBER = c( 49, 77, 70 )"
    ## [1] "BEST nonlin OBJECTIVE FUNCTION = 2.57205945241091"
    ## [1] "CURRNET METHOD: both"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'both' , seasonal.factor =  c( 49, 77, 70 ) ...)"
    ## [1] "CURRENT both OBJECTIVE FUNCTION = 1.77424501238597"
    ## [1] "BEST method = 'both' PATH MEMBER = c( 49, 77, 70 )"
    ## [1] "BEST both OBJECTIVE FUNCTION = 1.77424501238597"

![](man/figures/README-nns_forecasting-5.png)<!-- -->

    ## Package: RandomWalker
    ## [1] "CURRNET METHOD: lin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 7 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 8.74649748702282"
    ## [1] "BEST method = 'lin' PATH MEMBER = c( 7 )"
    ## [1] "BEST lin OBJECTIVE FUNCTION = 8.74649748702282"
    ## [1] "CURRNET METHOD: nonlin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'nonlin' , seasonal.factor =  c( 7 ) ...)"
    ## [1] "CURRENT nonlin OBJECTIVE FUNCTION = 3.78264551623959"
    ## [1] "BEST method = 'nonlin' PATH MEMBER = c( 7 )"
    ## [1] "BEST nonlin OBJECTIVE FUNCTION = 3.78264551623959"
    ## [1] "CURRNET METHOD: both"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'both' , seasonal.factor =  c( 7 ) ...)"
    ## [1] "CURRENT both OBJECTIVE FUNCTION = 3.59892586201775"
    ## [1] "BEST method = 'both' PATH MEMBER = c( 7 )"
    ## [1] "BEST both OBJECTIVE FUNCTION = 3.59892586201775"

![](man/figures/README-nns_forecasting-6.png)<!-- -->

    ## Package: tidyAML
    ## [1] "CURRNET METHOD: lin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 91 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 6.50818094321463"
    ## [1] "BEST method = 'lin', seasonal.factor = c( 91 )"
    ## [1] "BEST lin OBJECTIVE FUNCTION = 6.50818094321463"
    ## [1] "CURRNET METHOD: nonlin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'nonlin' , seasonal.factor =  c( 91 ) ...)"
    ## [1] "CURRENT nonlin OBJECTIVE FUNCTION = 7.58947750739376"
    ## [1] "BEST method = 'nonlin' PATH MEMBER = c( 91 )"
    ## [1] "BEST nonlin OBJECTIVE FUNCTION = 7.58947750739376"
    ## [1] "CURRNET METHOD: both"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'both' , seasonal.factor =  c( 91 ) ...)"
    ## [1] "CURRENT both OBJECTIVE FUNCTION = 6.89151720831414"
    ## [1] "BEST method = 'both' PATH MEMBER = c( 91 )"
    ## [1] "BEST both OBJECTIVE FUNCTION = 6.89151720831414"

![](man/figures/README-nns_forecasting-7.png)<!-- -->

    ## Package: TidyDensity
    ## [1] "CURRNET METHOD: lin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 77 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 1.5418822028667"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 77, 63 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 1.22204181301601"
    ## [1] "BEST method = 'lin', seasonal.factor = c( 77, 63 )"
    ## [1] "BEST lin OBJECTIVE FUNCTION = 1.22204181301601"
    ## [1] "CURRNET METHOD: nonlin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'nonlin' , seasonal.factor =  c( 77, 63 ) ...)"
    ## [1] "CURRENT nonlin OBJECTIVE FUNCTION = 2.02352208132597"
    ## [1] "BEST method = 'nonlin' PATH MEMBER = c( 77, 63 )"
    ## [1] "BEST nonlin OBJECTIVE FUNCTION = 2.02352208132597"
    ## [1] "CURRNET METHOD: both"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'both' , seasonal.factor =  c( 77, 63 ) ...)"
    ## [1] "CURRENT both OBJECTIVE FUNCTION = 1.48510478309668"
    ## [1] "BEST method = 'both' PATH MEMBER = c( 77, 63 )"
    ## [1] "BEST both OBJECTIVE FUNCTION = 1.48510478309668"

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
    ## 1 healthyR.data <tibble [1,564 × 2]> <tibble [28 × 2]> <split [1536|28]>
    ## 2 healthyR      <tibble [1,557 × 2]> <tibble [28 × 2]> <split [1529|28]>
    ## 3 healthyR.ts   <tibble [1,501 × 2]> <tibble [28 × 2]> <split [1473|28]>
    ## 4 healthyverse  <tibble [1,472 × 2]> <tibble [28 × 2]> <split [1444|28]>
    ## 5 healthyR.ai   <tibble [1,296 × 2]> <tibble [28 × 2]> <split [1268|28]>
    ## 6 TidyDensity   <tibble [1,147 × 2]> <tibble [28 × 2]> <split [1119|28]>
    ## 7 tidyAML       <tibble [755 × 2]>   <tibble [28 × 2]> <split [727|28]> 
    ## 8 RandomWalker  <tibble [177 × 2]>   <tibble [28 × 2]> <split [149|28]>

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
| healthyR.data | 1 | ARIMA | Test | 0.7156189 | 123.28629 | 0.6913043 | 166.04618 | 0.8641103 | 0.0281339 |
| healthyR.data | 2 | LM | Test | 0.7763762 | 208.54705 | 0.7499972 | 146.04227 | 0.9043867 | 0.0609267 |
| healthyR.data | 3 | EARTH | Test | 0.8443915 | 220.54116 | 0.8157015 | 129.00668 | 1.0800487 | 0.0609267 |
| healthyR.data | 4 | NNAR | Test | 0.7195369 | 112.69545 | 0.6950891 | 165.36030 | 0.8992623 | 0.0024384 |
| healthyR | 1 | ARIMA | Test | 0.7149183 | 175.60650 | 0.6599902 | 159.86018 | 0.8984274 | 0.0299756 |
| healthyR | 2 | LM | Test | 0.7378798 | 110.70688 | 0.6811875 | 184.32328 | 0.9243395 | 0.0064430 |
| healthyR | 3 | EARTH | Test | 2.8313788 | 1543.99505 | 2.6138405 | 150.89715 | 3.1793910 | 0.0064430 |
| healthyR | 4 | NNAR | Test | 0.6969955 | 144.47149 | 0.6434445 | 147.25060 | 0.9129181 | 0.0288610 |
| healthyR.ts | 1 | ARIMA | Test | 0.9705271 | 119.81634 | 0.6791619 | 132.07968 | 1.1443845 | 0.1094093 |
| healthyR.ts | 2 | LM | Test | 0.9618329 | 121.90875 | 0.6730778 | 127.53105 | 1.1412384 | 0.1094093 |
| healthyR.ts | 3 | EARTH | Test | 0.9531401 | 124.03458 | 0.6669947 | 123.24714 | 1.1394571 | 0.1094093 |
| healthyR.ts | 4 | NNAR | Test | 0.9195045 | 100.23116 | 0.6434570 | 161.85118 | 1.1201782 | 0.0993151 |
| healthyverse | 1 | ARIMA | Test | 0.6175860 | 112.62594 | 0.8514118 | 114.07767 | 0.7873823 | 0.0621315 |
| healthyverse | 2 | LM | Test | 0.7164282 | 185.35584 | 0.9876769 | 107.31013 | 0.8537943 | 0.1041191 |
| healthyverse | 3 | EARTH | Test | 2.9100223 | 715.47093 | 4.0117929 | 175.19409 | 3.2937470 | 0.1041191 |
| healthyverse | 4 | NNAR | Test | 0.6167262 | 97.09712 | 0.8502264 | 106.56073 | 0.8097102 | 0.0770929 |
| healthyR.ai | 1 | ARIMA | Test | 0.7516081 | 96.30564 | 0.7234697 | 168.94452 | 0.8743847 | 0.1248549 |
| healthyR.ai | 2 | LM | Test | 0.7986187 | 95.27869 | 0.7687204 | 150.94853 | 0.9484639 | 0.0848383 |
| healthyR.ai | 3 | EARTH | Test | 0.7972245 | 95.84832 | 0.7673784 | 152.62528 | 0.9430537 | 0.0848383 |
| healthyR.ai | 4 | NNAR | Test | 0.6833051 | 81.22450 | 0.6577239 | 136.42491 | 0.8381097 | 0.1726412 |
| TidyDensity | 1 | ARIMA | Test | 0.6208369 | 133.80408 | 0.6543422 | 105.92100 | 0.7426563 | 0.1489968 |
| TidyDensity | 2 | LM | Test | 0.6674558 | 173.77016 | 0.7034771 | 104.09602 | 0.7815620 | 0.0263482 |
| TidyDensity | 3 | EARTH | Test | 0.6646502 | 131.76239 | 0.7005201 | 114.90585 | 0.7887862 | 0.0263482 |
| TidyDensity | 4 | NNAR | Test | 0.6910283 | 106.22970 | 0.7283218 | 135.41313 | 0.8735503 | 0.0004915 |
| tidyAML | 1 | ARIMA | Test | 0.6419843 | 126.62720 | 0.6498414 | 100.99625 | 0.7672265 | 0.0157762 |
| tidyAML | 2 | LM | Test | 0.6432587 | 140.64821 | 0.6511314 | 93.73853 | 0.7735652 | 0.0107135 |
| tidyAML | 3 | EARTH | Test | 0.7804920 | 99.07781 | 0.7900442 | 162.12991 | 0.9640277 | 0.0107135 |
| tidyAML | 4 | NNAR | Test | 0.6465883 | 118.06019 | 0.6545018 | 102.99494 | 0.7821396 | 0.0029359 |
| RandomWalker | 1 | ARIMA | Test | 1.0895409 | 84.70718 | 0.5142272 | 131.72361 | 1.2232858 | 0.4214556 |
| RandomWalker | 2 | LM | Test | 1.3674687 | 111.59390 | 0.6453999 | 192.40473 | 1.4782994 | 0.0167457 |
| RandomWalker | 3 | EARTH | Test | 1.2638584 | 92.26815 | 0.5964993 | 169.10322 | 1.4396787 | NA |
| RandomWalker | 4 | NNAR | Test | 1.3264013 | 115.71095 | 0.6260175 | 147.66058 | 1.5183527 | 0.0137718 |

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
    ## 1 healthyR.data         1 ARIMA       Test  0.716 123.  0.691  166. 0.864 0.0281
    ## 2 healthyR              1 ARIMA       Test  0.715 176.  0.660  160. 0.898 0.0300
    ## 3 healthyR.ts           4 NNAR        Test  0.920 100.  0.643  162. 1.12  0.0993
    ## 4 healthyverse          1 ARIMA       Test  0.618 113.  0.851  114. 0.787 0.0621
    ## 5 healthyR.ai           4 NNAR        Test  0.683  81.2 0.658  136. 0.838 0.173 
    ## 6 TidyDensity           1 ARIMA       Test  0.621 134.  0.654  106. 0.743 0.149 
    ## 7 tidyAML               1 ARIMA       Test  0.642 127.  0.650  101. 0.767 0.0158
    ## 8 RandomWalker          1 ARIMA       Test  1.09   84.7 0.514  132. 1.22  0.421

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
    ## 1 healthyR.data <tibble>     <tibble>     <split [1536|28]> <mdl_tm_t [1 × 5]>
    ## 2 healthyR      <tibble>     <tibble>     <split [1529|28]> <mdl_tm_t [1 × 5]>
    ## 3 healthyR.ts   <tibble>     <tibble>     <split [1473|28]> <mdl_tm_t [1 × 5]>
    ## 4 healthyverse  <tibble>     <tibble>     <split [1444|28]> <mdl_tm_t [1 × 5]>
    ## 5 healthyR.ai   <tibble>     <tibble>     <split [1268|28]> <mdl_tm_t [1 × 5]>
    ## 6 TidyDensity   <tibble>     <tibble>     <split [1119|28]> <mdl_tm_t [1 × 5]>
    ## 7 tidyAML       <tibble>     <tibble>     <split [727|28]>  <mdl_tm_t [1 × 5]>
    ## 8 RandomWalker  <tibble>     <tibble>     <split [149|28]>  <mdl_tm_t [1 × 5]>

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
