Time Series Analysis and Nested Modeling of the Healthyverse Packages
================
Steven P. Sanderson II, MPH - Date:
18 April, 2025

This analysis follows a *Nested Modeltime Workflow*.

## Get Data

``` r
glimpse(downloads_tbl)
```

    ## Rows: 137,359
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

The last day in the data set is 2025-04-16 23:58:30, the file was
birthed on: 2024-08-07 07:35:44, and at report knit time is -6060.38
hours old. Happy analyzing!

Now that we have our data lets take a look at it using the `skimr`
package.

``` r
skim(downloads_tbl)
```

|                                                  |               |
|:-------------------------------------------------|:--------------|
| Name                                             | downloads_tbl |
| Number of rows                                   | 137359        |
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
| r_version     |     98865 |          0.28 |   5 |   5 |     0 |       46 |          0 |
| r_arch        |     98865 |          0.28 |   3 |   7 |     0 |        5 |          0 |
| r_os          |     98865 |          0.28 |   7 |  15 |     0 |       21 |          0 |
| package       |         0 |          1.00 |   7 |  13 |     0 |        8 |          0 |
| version       |         0 |          1.00 |   5 |  17 |     0 |       60 |          0 |
| country       |     11571 |          0.92 |   2 |   2 |     0 |      163 |          0 |

**Variable type: Date**

| skim_variable | n_missing | complete_rate | min | max | median | n_unique |
|:---|---:|---:|:---|:---|:---|---:|
| date | 0 | 1 | 2020-11-23 | 2025-04-16 | 2023-06-02 | 1606 |

**Variable type: numeric**

| skim_variable | n_missing | complete_rate | mean | sd | p0 | p25 | p50 | p75 | p100 | hist |
|:---|---:|---:|---:|---:|---:|---:|---:|---:|---:|:---|
| size | 0 | 1 | 1135594.72 | 1522675.63 | 355 | 14701 | 274998 | 2367774 | 5677952 | ▇▁▂▁▁ |
| ip_id | 0 | 1 | 10375.55 | 18408.15 | 1 | 303 | 3064 | 11721 | 209747 | ▇▁▁▁▁ |

**Variable type: POSIXct**

| skim_variable | n_missing | complete_rate | min | max | median | n_unique |
|:---|---:|---:|:---|:---|:---|---:|
| date_time | 0 | 1 | 2020-11-23 09:00:41 | 2025-04-16 23:58:30 | 2023-06-02 22:32:54 | 83632 |

**Variable type: Timespan**

| skim_variable | n_missing | complete_rate | min | max |     median | n_unique |
|:--------------|----------:|--------------:|----:|----:|-----------:|---------:|
| time          |         0 |             1 |   0 |  59 | 12H 7M 29S |       60 |

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
    ## -147.98  -35.49  -10.57   26.70  813.18 
    ## 
    ## Coefficients:
    ##                                                      Estimate Std. Error
    ## (Intercept)                                        -1.895e+02  7.263e+01
    ## date                                                1.149e-02  3.853e-03
    ## lag(value, 1)                                       1.063e-01  2.473e-02
    ## lag(value, 7)                                       9.931e-02  2.568e-02
    ## lag(value, 14)                                      9.637e-02  2.571e-02
    ## lag(value, 21)                                      6.487e-02  2.586e-02
    ## lag(value, 28)                                      6.390e-02  2.575e-02
    ## lag(value, 35)                                      6.948e-02  2.581e-02
    ## lag(value, 42)                                      4.951e-02  2.591e-02
    ## lag(value, 49)                                      7.016e-02  2.575e-02
    ## month(date, label = TRUE).L                        -1.041e+01  5.140e+00
    ## month(date, label = TRUE).Q                         2.583e+00  5.201e+00
    ## month(date, label = TRUE).C                        -1.209e+01  5.214e+00
    ## month(date, label = TRUE)^4                        -6.676e+00  5.199e+00
    ## month(date, label = TRUE)^5                        -1.245e+01  5.206e+00
    ## month(date, label = TRUE)^6                        -2.807e+00  5.267e+00
    ## month(date, label = TRUE)^7                        -6.292e+00  5.169e+00
    ## month(date, label = TRUE)^8                        -4.673e+00  5.176e+00
    ## month(date, label = TRUE)^9                         5.750e+00  5.194e+00
    ## month(date, label = TRUE)^10                        4.235e+00  5.271e+00
    ## month(date, label = TRUE)^11                       -5.741e+00  5.340e+00
    ## fourier_vec(date, type = "sin", K = 1, period = 7) -1.160e+01  2.384e+00
    ## fourier_vec(date, type = "cos", K = 1, period = 7)  7.951e+00  2.512e+00
    ##                                                    t value Pr(>|t|)    
    ## (Intercept)                                         -2.609 0.009159 ** 
    ## date                                                 2.982 0.002912 ** 
    ## lag(value, 1)                                        4.298 1.83e-05 ***
    ## lag(value, 7)                                        3.868 0.000114 ***
    ## lag(value, 14)                                       3.748 0.000184 ***
    ## lag(value, 21)                                       2.508 0.012230 *  
    ## lag(value, 28)                                       2.482 0.013165 *  
    ## lag(value, 35)                                       2.692 0.007183 ** 
    ## lag(value, 42)                                       1.911 0.056199 .  
    ## lag(value, 49)                                       2.725 0.006510 ** 
    ## month(date, label = TRUE).L                         -2.025 0.043013 *  
    ## month(date, label = TRUE).Q                          0.497 0.619534    
    ## month(date, label = TRUE).C                         -2.318 0.020586 *  
    ## month(date, label = TRUE)^4                         -1.284 0.199339    
    ## month(date, label = TRUE)^5                         -2.391 0.016916 *  
    ## month(date, label = TRUE)^6                         -0.533 0.594128    
    ## month(date, label = TRUE)^7                         -1.217 0.223750    
    ## month(date, label = TRUE)^8                         -0.903 0.366772    
    ## month(date, label = TRUE)^9                          1.107 0.268387    
    ## month(date, label = TRUE)^10                         0.804 0.421789    
    ## month(date, label = TRUE)^11                        -1.075 0.282498    
    ## fourier_vec(date, type = "sin", K = 1, period = 7)  -4.865 1.26e-06 ***
    ## fourier_vec(date, type = "cos", K = 1, period = 7)   3.165 0.001583 ** 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 58.78 on 1534 degrees of freedom
    ##   (49 observations deleted due to missingness)
    ## Multiple R-squared:  0.2479, Adjusted R-squared:  0.2371 
    ## F-statistic: 22.98 on 22 and 1534 DF,  p-value: < 2.2e-16

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
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 63 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 2.96421994505375"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 63, 91 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 2.50730127755907"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 63, 91, 70 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 2.44827147523047"
    ## [1] "BEST method = 'lin', seasonal.factor = c( 63, 91, 70 )"
    ## [1] "BEST lin OBJECTIVE FUNCTION = 2.44827147523047"
    ## [1] "CURRNET METHOD: nonlin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'nonlin' , seasonal.factor =  c( 63, 91, 70 ) ...)"
    ## [1] "CURRENT nonlin OBJECTIVE FUNCTION = 4.22298939920344"
    ## [1] "BEST method = 'nonlin' PATH MEMBER = c( 63, 91, 70 )"
    ## [1] "BEST nonlin OBJECTIVE FUNCTION = 4.22298939920344"
    ## [1] "CURRNET METHOD: both"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'both' , seasonal.factor =  c( 63, 91, 70 ) ...)"
    ## [1] "CURRENT both OBJECTIVE FUNCTION = 3.14654436267833"
    ## [1] "BEST method = 'both' PATH MEMBER = c( 63, 91, 70 )"
    ## [1] "BEST both OBJECTIVE FUNCTION = 3.14654436267833"

![](man/figures/README-nns_forecasting-1.png)<!-- -->

    ## Package: healthyR.ai
    ## [1] "CURRNET METHOD: lin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 63 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 2.44321524718395"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 63, 98 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 1.89689991368037"
    ## [1] "BEST method = 'lin', seasonal.factor = c( 63, 98 )"
    ## [1] "BEST lin OBJECTIVE FUNCTION = 1.89689991368037"
    ## [1] "CURRNET METHOD: nonlin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'nonlin' , seasonal.factor =  c( 63, 98 ) ...)"
    ## [1] "CURRENT nonlin OBJECTIVE FUNCTION = 2.28585135198188"
    ## [1] "BEST method = 'nonlin' PATH MEMBER = c( 63, 98 )"
    ## [1] "BEST nonlin OBJECTIVE FUNCTION = 2.28585135198188"
    ## [1] "CURRNET METHOD: both"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'both' , seasonal.factor =  c( 63, 98 ) ...)"
    ## [1] "CURRENT both OBJECTIVE FUNCTION = 1.94739191680271"
    ## [1] "BEST method = 'both' PATH MEMBER = c( 63, 98 )"
    ## [1] "BEST both OBJECTIVE FUNCTION = 1.94739191680271"

![](man/figures/README-nns_forecasting-2.png)<!-- -->

    ## Package: healthyR.data
    ## [1] "CURRNET METHOD: lin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 98 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 2.10106552447094"
    ## [1] "BEST method = 'lin', seasonal.factor = c( 98 )"
    ## [1] "BEST lin OBJECTIVE FUNCTION = 2.10106552447094"
    ## [1] "CURRNET METHOD: nonlin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'nonlin' , seasonal.factor =  c( 98 ) ...)"
    ## [1] "CURRENT nonlin OBJECTIVE FUNCTION = 2.46760759916857"
    ## [1] "BEST method = 'nonlin' PATH MEMBER = c( 98 )"
    ## [1] "BEST nonlin OBJECTIVE FUNCTION = 2.46760759916857"
    ## [1] "CURRNET METHOD: both"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'both' , seasonal.factor =  c( 98 ) ...)"
    ## [1] "CURRENT both OBJECTIVE FUNCTION = 2.14131028554055"
    ## [1] "BEST method = 'both' PATH MEMBER = c( 98 )"
    ## [1] "BEST both OBJECTIVE FUNCTION = 2.14131028554055"

![](man/figures/README-nns_forecasting-3.png)<!-- -->

    ## Package: healthyR.ts
    ## [1] "CURRNET METHOD: lin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 63 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 2.40087402589534"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 63, 98 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 2.0101134594531"
    ## [1] "BEST method = 'lin', seasonal.factor = c( 63, 98 )"
    ## [1] "BEST lin OBJECTIVE FUNCTION = 2.0101134594531"
    ## [1] "CURRNET METHOD: nonlin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'nonlin' , seasonal.factor =  c( 63, 98 ) ...)"
    ## [1] "CURRENT nonlin OBJECTIVE FUNCTION = 3.19364655255016"
    ## [1] "BEST method = 'nonlin' PATH MEMBER = c( 63, 98 )"
    ## [1] "BEST nonlin OBJECTIVE FUNCTION = 3.19364655255016"
    ## [1] "CURRNET METHOD: both"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'both' , seasonal.factor =  c( 63, 98 ) ...)"
    ## [1] "CURRENT both OBJECTIVE FUNCTION = 2.47653323791533"
    ## [1] "BEST method = 'both' PATH MEMBER = c( 63, 98 )"
    ## [1] "BEST both OBJECTIVE FUNCTION = 2.47653323791533"

![](man/figures/README-nns_forecasting-4.png)<!-- -->

    ## Package: healthyverse
    ## [1] "CURRNET METHOD: lin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 91 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 3.80463938202456"
    ## [1] "BEST method = 'lin', seasonal.factor = c( 91 )"
    ## [1] "BEST lin OBJECTIVE FUNCTION = 3.80463938202456"
    ## [1] "CURRNET METHOD: nonlin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'nonlin' , seasonal.factor =  c( 91 ) ...)"
    ## [1] "CURRENT nonlin OBJECTIVE FUNCTION = 5.38626154815699"
    ## [1] "BEST method = 'nonlin' PATH MEMBER = c( 91 )"
    ## [1] "BEST nonlin OBJECTIVE FUNCTION = 5.38626154815699"
    ## [1] "CURRNET METHOD: both"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'both' , seasonal.factor =  c( 91 ) ...)"
    ## [1] "CURRENT both OBJECTIVE FUNCTION = 4.34040228738022"
    ## [1] "BEST method = 'both' PATH MEMBER = c( 91 )"
    ## [1] "BEST both OBJECTIVE FUNCTION = 4.34040228738022"

![](man/figures/README-nns_forecasting-5.png)<!-- -->

    ## Package: RandomWalker
    ## [1] "CURRNET METHOD: lin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 42 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 1.8832062859861"
    ## [1] "BEST method = 'lin', seasonal.factor = c( 42 )"
    ## [1] "BEST lin OBJECTIVE FUNCTION = 1.8832062859861"
    ## [1] "CURRNET METHOD: nonlin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'nonlin' , seasonal.factor =  c( 42 ) ...)"
    ## [1] "CURRENT nonlin OBJECTIVE FUNCTION = 2.23924128184997"
    ## [1] "BEST method = 'nonlin' PATH MEMBER = c( 42 )"
    ## [1] "BEST nonlin OBJECTIVE FUNCTION = 2.23924128184997"
    ## [1] "CURRNET METHOD: both"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'both' , seasonal.factor =  c( 42 ) ...)"
    ## [1] "CURRENT both OBJECTIVE FUNCTION = 1.82202040181459"
    ## [1] "BEST method = 'both' PATH MEMBER = c( 42 )"
    ## [1] "BEST both OBJECTIVE FUNCTION = 1.82202040181459"

![](man/figures/README-nns_forecasting-6.png)<!-- -->

    ## Package: tidyAML
    ## [1] "CURRNET METHOD: lin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 28 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 4.17654966954133"
    ## [1] "BEST method = 'lin', seasonal.factor = c( 28 )"
    ## [1] "BEST lin OBJECTIVE FUNCTION = 4.17654966954133"
    ## [1] "CURRNET METHOD: nonlin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'nonlin' , seasonal.factor =  c( 28 ) ...)"
    ## [1] "CURRENT nonlin OBJECTIVE FUNCTION = 2.7576357499475"
    ## [1] "BEST method = 'nonlin' PATH MEMBER = c( 28 )"
    ## [1] "BEST nonlin OBJECTIVE FUNCTION = 2.7576357499475"
    ## [1] "CURRNET METHOD: both"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'both' , seasonal.factor =  c( 28 ) ...)"
    ## [1] "CURRENT both OBJECTIVE FUNCTION = 2.6564814874381"
    ## [1] "BEST method = 'both' PATH MEMBER = c( 28 )"
    ## [1] "BEST both OBJECTIVE FUNCTION = 2.6564814874381"

![](man/figures/README-nns_forecasting-7.png)<!-- -->

    ## Package: TidyDensity
    ## [1] "CURRNET METHOD: lin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 63 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 2.91839912048994"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 63, 77 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 2.89641011559762"
    ## [1] "BEST method = 'lin', seasonal.factor = c( 63, 77 )"
    ## [1] "BEST lin OBJECTIVE FUNCTION = 2.89641011559762"
    ## [1] "CURRNET METHOD: nonlin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'nonlin' , seasonal.factor =  c( 63, 77 ) ...)"
    ## [1] "CURRENT nonlin OBJECTIVE FUNCTION = 3.72253043687994"
    ## [1] "BEST method = 'nonlin' PATH MEMBER = c( 63, 77 )"
    ## [1] "BEST nonlin OBJECTIVE FUNCTION = 3.72253043687994"
    ## [1] "CURRNET METHOD: both"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'both' , seasonal.factor =  c( 63, 77 ) ...)"
    ## [1] "CURRENT both OBJECTIVE FUNCTION = 3.13079196938131"
    ## [1] "BEST method = 'both' PATH MEMBER = c( 63, 77 )"
    ## [1] "BEST both OBJECTIVE FUNCTION = 3.13079196938131"

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
    ## 1 healthyR.data <tibble [1,599 × 2]> <tibble [28 × 2]> <split [1571|28]>
    ## 2 healthyR      <tibble [1,592 × 2]> <tibble [28 × 2]> <split [1564|28]>
    ## 3 healthyR.ts   <tibble [1,536 × 2]> <tibble [28 × 2]> <split [1508|28]>
    ## 4 healthyverse  <tibble [1,507 × 2]> <tibble [28 × 2]> <split [1479|28]>
    ## 5 healthyR.ai   <tibble [1,331 × 2]> <tibble [28 × 2]> <split [1303|28]>
    ## 6 TidyDensity   <tibble [1,182 × 2]> <tibble [28 × 2]> <split [1154|28]>
    ## 7 tidyAML       <tibble [790 × 2]>   <tibble [28 × 2]> <split [762|28]> 
    ## 8 RandomWalker  <tibble [212 × 2]>   <tibble [28 × 2]> <split [184|28]>

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
| healthyR.data | 1 | ARIMA | Test | 0.8166529 | 175.01566 | 0.6519883 | 163.7885 | 0.9436430 | 0.1099733 |
| healthyR.data | 2 | LM | Test | 0.7797320 | 169.69811 | 0.6225119 | 146.5980 | 0.9057676 | 0.0493791 |
| healthyR.data | 3 | EARTH | Test | 1.0496093 | 330.43926 | 0.8379729 | 131.9468 | 1.2606270 | 0.0493791 |
| healthyR.data | 4 | NNAR | Test | 0.7440144 | 116.48452 | 0.5939962 | 171.9089 | 0.9355045 | 0.0000001 |
| healthyR | 1 | ARIMA | Test | 0.7388047 | 104.16298 | 0.7297873 | 178.2227 | 0.9099680 | 0.0256899 |
| healthyR | 2 | LM | Test | 0.7115738 | 97.97049 | 0.7028887 | 185.6999 | 0.8742374 | 0.0715654 |
| healthyR | 3 | EARTH | Test | 0.7111615 | 97.21340 | 0.7024814 | 177.0310 | 0.8729731 | 0.0715654 |
| healthyR | 4 | NNAR | Test | 0.7476649 | 100.15380 | 0.7385394 | 158.6677 | 0.9256955 | 0.0473272 |
| healthyR.ts | 1 | ARIMA | Test | 0.9143378 | 273.40720 | 0.7268053 | 148.3628 | 1.1273960 | 0.0455431 |
| healthyR.ts | 2 | LM | Test | 0.9345655 | 326.40447 | 0.7428843 | 141.1867 | 1.1680833 | 0.0455431 |
| healthyR.ts | 3 | EARTH | Test | 0.9335866 | 322.95318 | 0.7421061 | 141.8001 | 1.1648705 | NA |
| healthyR.ts | 4 | NNAR | Test | 0.9431758 | 166.34090 | 0.7497286 | 179.3385 | 1.1791870 | 0.0337781 |
| healthyverse | 1 | ARIMA | Test | 0.6692285 | 351.45408 | 0.6858813 | 111.7667 | 0.8381570 | 0.0154100 |
| healthyverse | 2 | LM | Test | 0.6698987 | 432.19256 | 0.6865681 | 105.4821 | 0.8229659 | 0.0025692 |
| healthyverse | 3 | EARTH | Test | 0.6511266 | 263.86872 | 0.6673289 | 116.1471 | 0.8487658 | 0.0025692 |
| healthyverse | 4 | NNAR | Test | 0.6677176 | 247.36087 | 0.6843327 | 122.0862 | 0.8722359 | 0.0139842 |
| healthyR.ai | 1 | ARIMA | Test | 0.7254637 | 148.82572 | 0.7240198 | 179.4298 | 0.9424998 | 0.0405568 |
| healthyR.ai | 2 | LM | Test | 0.6664376 | 146.02753 | 0.6651112 | 146.8749 | 0.8685641 | 0.0359816 |
| healthyR.ai | 3 | EARTH | Test | 0.6717265 | 123.98120 | 0.6703895 | 153.3813 | 0.8802402 | 0.0359816 |
| healthyR.ai | 4 | NNAR | Test | 0.7896146 | 173.76226 | 0.7880430 | 161.5622 | 1.0330173 | 0.1332527 |
| TidyDensity | 1 | ARIMA | Test | 0.6205283 | 308.70846 | 0.6902220 | 111.5305 | 0.7536063 | 0.0569331 |
| TidyDensity | 2 | LM | Test | 0.6419715 | 367.50284 | 0.7140734 | 102.3069 | 0.8171988 | 0.0977793 |
| TidyDensity | 3 | EARTH | Test | 0.6171700 | 269.02775 | 0.6864865 | 109.4917 | 0.7734796 | 0.0977793 |
| TidyDensity | 4 | NNAR | Test | 0.6868838 | 253.30594 | 0.7640300 | 145.4068 | 0.8267306 | 0.0083488 |
| tidyAML | 1 | ARIMA | Test | 0.6427715 | 317.61615 | 0.8458465 | 101.6648 | 0.7999587 | 0.0377784 |
| tidyAML | 2 | LM | Test | 0.6540296 | 310.78721 | 0.8606616 | 103.8010 | 0.8058125 | 0.0343169 |
| tidyAML | 3 | EARTH | Test | 0.6803634 | 337.60786 | 0.8953151 | 104.3609 | 0.8313325 | 0.0343169 |
| tidyAML | 4 | NNAR | Test | 0.6408142 | 294.51774 | 0.8432709 | 104.3341 | 0.7879498 | 0.0024578 |
| RandomWalker | 1 | ARIMA | Test | 1.2134277 | 116.25743 | 0.5920482 | 173.2953 | 1.4373721 | 0.0021978 |
| RandomWalker | 2 | LM | Test | 1.2237908 | 98.82794 | 0.5971046 | 193.1199 | 1.4389783 | 0.0879682 |
| RandomWalker | 3 | EARTH | Test | 1.2085690 | 110.11240 | 0.5896776 | 170.2913 | 1.4376996 | NA |
| RandomWalker | 4 | NNAR | Test | 1.4349838 | 280.29409 | 0.7001486 | 156.5642 | 1.6259132 | 0.0000682 |

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
    ## 1 healthyR.da…         2 LM          Test  0.780 170.  0.623  147. 0.906 0.0494 
    ## 2 healthyR             3 EARTH       Test  0.711  97.2 0.702  177. 0.873 0.0716 
    ## 3 healthyR.ts          1 ARIMA       Test  0.914 273.  0.727  148. 1.13  0.0455 
    ## 4 healthyverse         2 LM          Test  0.670 432.  0.687  105. 0.823 0.00257
    ## 5 healthyR.ai          2 LM          Test  0.666 146.  0.665  147. 0.869 0.0360 
    ## 6 TidyDensity          1 ARIMA       Test  0.621 309.  0.690  112. 0.754 0.0569 
    ## 7 tidyAML              4 NNAR        Test  0.641 295.  0.843  104. 0.788 0.00246
    ## 8 RandomWalker         1 ARIMA       Test  1.21  116.  0.592  173. 1.44  0.00220

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
    ## 1 healthyR.data <tibble>     <tibble>     <split [1571|28]> <mdl_tm_t [1 × 5]>
    ## 2 healthyR      <tibble>     <tibble>     <split [1564|28]> <mdl_tm_t [1 × 5]>
    ## 3 healthyR.ts   <tibble>     <tibble>     <split [1508|28]> <mdl_tm_t [1 × 5]>
    ## 4 healthyverse  <tibble>     <tibble>     <split [1479|28]> <mdl_tm_t [1 × 5]>
    ## 5 healthyR.ai   <tibble>     <tibble>     <split [1303|28]> <mdl_tm_t [1 × 5]>
    ## 6 TidyDensity   <tibble>     <tibble>     <split [1154|28]> <mdl_tm_t [1 × 5]>
    ## 7 tidyAML       <tibble>     <tibble>     <split [762|28]>  <mdl_tm_t [1 × 5]>
    ## 8 RandomWalker  <tibble>     <tibble>     <split [184|28]>  <mdl_tm_t [1 × 5]>

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
