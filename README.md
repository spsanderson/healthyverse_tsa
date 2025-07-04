Time Series Analysis and Nested Modeling of the Healthyverse Packages
================
Steven P. Sanderson II, MPH - Date:
30 June, 2025

This analysis follows a *Nested Modeltime Workflow*.

## Get Data

``` r
glimpse(downloads_tbl)
```

    ## Rows: 144,071
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

The last day in the data set is 2025-06-28 23:46:11, the file was
birthed on: 2024-08-07 07:35:44.428716, and at report knit time is
-7812.17 hours old. Happy analyzing!

Now that we have our data lets take a look at it using the `skimr`
package.

``` r
skim(downloads_tbl)
```

|                                                  |               |
|:-------------------------------------------------|:--------------|
| Name                                             | downloads_tbl |
| Number of rows                                   | 144071        |
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
| r_version     |    104091 |          0.28 |   5 |   5 |     0 |       48 |          0 |
| r_arch        |    104091 |          0.28 |   3 |   7 |     0 |        5 |          0 |
| r_os          |    104091 |          0.28 |   7 |  15 |     0 |       22 |          0 |
| package       |         0 |          1.00 |   7 |  13 |     0 |        8 |          0 |
| version       |         0 |          1.00 |   5 |  17 |     0 |       60 |          0 |
| country       |     12163 |          0.92 |   2 |   2 |     0 |      163 |          0 |

**Variable type: Date**

| skim_variable | n_missing | complete_rate | min | max | median | n_unique |
|:---|---:|---:|:---|:---|:---|---:|
| date | 0 | 1 | 2020-11-23 | 2025-06-28 | 2023-07-12 | 1679 |

**Variable type: numeric**

| skim_variable | n_missing | complete_rate | mean | sd | p0 | p25 | p50 | p75 | p100 | hist |
|:---|---:|---:|---:|---:|---:|---:|---:|---:|---:|:---|
| size | 0 | 1 | 1132141.70 | 1514512.41 | 355 | 14701 | 290110 | 2367690.0 | 5677952 | ▇▁▂▁▁ |
| ip_id | 0 | 1 | 10453.04 | 18579.02 | 1 | 298 | 3049 | 11828.5 | 209747 | ▇▁▁▁▁ |

**Variable type: POSIXct**

| skim_variable | n_missing | complete_rate | min | max | median | n_unique |
|:---|---:|---:|:---|:---|:---|---:|
| date_time | 0 | 1 | 2020-11-23 09:00:41 | 2025-06-28 23:46:11 | 2023-07-12 19:41:22 | 88397 |

**Variable type: Timespan**

| skim_variable | n_missing | complete_rate | min | max |     median | n_unique |
|:--------------|----------:|--------------:|----:|----:|-----------:|---------:|
| time          |         0 |             1 |   0 |  59 | 12H 6M 25S |       60 |

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
    ## -148.29  -35.92  -11.35   26.76  816.02 
    ## 
    ## Coefficients:
    ##                                                      Estimate Std. Error
    ## (Intercept)                                        -1.749e+02  6.662e+01
    ## date                                                1.077e-02  3.527e-03
    ## lag(value, 1)                                       1.049e-01  2.418e-02
    ## lag(value, 7)                                       9.445e-02  2.501e-02
    ## lag(value, 14)                                      8.640e-02  2.514e-02
    ## lag(value, 21)                                      6.638e-02  2.521e-02
    ## lag(value, 28)                                      7.178e-02  2.509e-02
    ## lag(value, 35)                                      6.827e-02  2.517e-02
    ## lag(value, 42)                                      5.437e-02  2.527e-02
    ## lag(value, 49)                                      6.335e-02  2.515e-02
    ## month(date, label = TRUE).L                        -9.720e+00  5.117e+00
    ## month(date, label = TRUE).Q                         3.246e+00  5.083e+00
    ## month(date, label = TRUE).C                        -1.326e+01  5.135e+00
    ## month(date, label = TRUE)^4                        -6.653e+00  5.139e+00
    ## month(date, label = TRUE)^5                        -1.133e+01  5.119e+00
    ## month(date, label = TRUE)^6                        -4.103e+00  5.185e+00
    ## month(date, label = TRUE)^7                        -7.107e+00  5.084e+00
    ## month(date, label = TRUE)^8                        -2.858e+00  5.075e+00
    ## month(date, label = TRUE)^9                         5.218e+00  5.073e+00
    ## month(date, label = TRUE)^10                        2.521e+00  5.068e+00
    ## month(date, label = TRUE)^11                       -3.593e+00  5.086e+00
    ## fourier_vec(date, type = "sin", K = 1, period = 7) -1.184e+01  2.316e+00
    ## fourier_vec(date, type = "cos", K = 1, period = 7)  8.248e+00  2.438e+00
    ##                                                    t value Pr(>|t|)    
    ## (Intercept)                                         -2.625 0.008742 ** 
    ## date                                                 3.053 0.002306 ** 
    ## lag(value, 1)                                        4.337 1.53e-05 ***
    ## lag(value, 7)                                        3.776 0.000165 ***
    ## lag(value, 14)                                       3.437 0.000604 ***
    ## lag(value, 21)                                       2.634 0.008527 ** 
    ## lag(value, 28)                                       2.861 0.004284 ** 
    ## lag(value, 35)                                       2.712 0.006752 ** 
    ## lag(value, 42)                                       2.152 0.031570 *  
    ## lag(value, 49)                                       2.519 0.011862 *  
    ## month(date, label = TRUE).L                         -1.899 0.057682 .  
    ## month(date, label = TRUE).Q                          0.639 0.523179    
    ## month(date, label = TRUE).C                         -2.583 0.009891 ** 
    ## month(date, label = TRUE)^4                         -1.295 0.195585    
    ## month(date, label = TRUE)^5                         -2.213 0.027031 *  
    ## month(date, label = TRUE)^6                         -0.791 0.428933    
    ## month(date, label = TRUE)^7                         -1.398 0.162299    
    ## month(date, label = TRUE)^8                         -0.563 0.573382    
    ## month(date, label = TRUE)^9                          1.029 0.303773    
    ## month(date, label = TRUE)^10                         0.497 0.618955    
    ## month(date, label = TRUE)^11                        -0.707 0.479960    
    ## fourier_vec(date, type = "sin", K = 1, period = 7)  -5.113 3.55e-07 ***
    ## fourier_vec(date, type = "cos", K = 1, period = 7)   3.384 0.000732 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 58.82 on 1607 degrees of freedom
    ##   (49 observations deleted due to missingness)
    ## Multiple R-squared:  0.238,  Adjusted R-squared:  0.2276 
    ## F-statistic: 22.82 on 22 and 1607 DF,  p-value: < 2.2e-16

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
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 21 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 2.65898235609819"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 21, 63 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 2.4145597060677"
    ## [1] "BEST method = 'lin', seasonal.factor = c( 21, 63 )"
    ## [1] "BEST lin OBJECTIVE FUNCTION = 2.4145597060677"
    ## [1] "CURRNET METHOD: nonlin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'nonlin' , seasonal.factor =  c( 21, 63 ) ...)"
    ## [1] "CURRENT nonlin OBJECTIVE FUNCTION = 6.68931183366912"
    ## [1] "BEST method = 'nonlin' PATH MEMBER = c( 21, 63 )"
    ## [1] "BEST nonlin OBJECTIVE FUNCTION = 6.68931183366912"
    ## [1] "CURRNET METHOD: both"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'both' , seasonal.factor =  c( 21, 63 ) ...)"
    ## [1] "CURRENT both OBJECTIVE FUNCTION = 3.8441429469348"
    ## [1] "BEST method = 'both' PATH MEMBER = c( 21, 63 )"
    ## [1] "BEST both OBJECTIVE FUNCTION = 3.8441429469348"

![](man/figures/README-nns_forecasting-1.png)<!-- -->

    ## Package: healthyR.ai
    ## [1] "CURRNET METHOD: lin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 63 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 2.24756969072553"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 63, 35 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 1.79870731937483"
    ## [1] "BEST method = 'lin', seasonal.factor = c( 63, 35 )"
    ## [1] "BEST lin OBJECTIVE FUNCTION = 1.79870731937483"
    ## [1] "CURRNET METHOD: nonlin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'nonlin' , seasonal.factor =  c( 63, 35 ) ...)"
    ## [1] "CURRENT nonlin OBJECTIVE FUNCTION = 10.7885743776821"
    ## [1] "BEST method = 'nonlin' PATH MEMBER = c( 63, 35 )"
    ## [1] "BEST nonlin OBJECTIVE FUNCTION = 10.7885743776821"
    ## [1] "CURRNET METHOD: both"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'both' , seasonal.factor =  c( 63, 35 ) ...)"
    ## [1] "CURRENT both OBJECTIVE FUNCTION = 3.90430895137886"
    ## [1] "BEST method = 'both' PATH MEMBER = c( 63, 35 )"
    ## [1] "BEST both OBJECTIVE FUNCTION = 3.90430895137886"

![](man/figures/README-nns_forecasting-2.png)<!-- -->

    ## Package: healthyR.data
    ## [1] "CURRNET METHOD: lin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 63 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 2.78632566804951"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 63, 56 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 2.60958215037283"
    ## [1] "BEST method = 'lin', seasonal.factor = c( 63, 56 )"
    ## [1] "BEST lin OBJECTIVE FUNCTION = 2.60958215037283"
    ## [1] "CURRNET METHOD: nonlin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'nonlin' , seasonal.factor =  c( 63, 56 ) ...)"
    ## [1] "CURRENT nonlin OBJECTIVE FUNCTION = 3.78553795169628"
    ## [1] "BEST method = 'nonlin' PATH MEMBER = c( 63, 56 )"
    ## [1] "BEST nonlin OBJECTIVE FUNCTION = 3.78553795169628"
    ## [1] "CURRNET METHOD: both"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'both' , seasonal.factor =  c( 63, 56 ) ...)"
    ## [1] "CURRENT both OBJECTIVE FUNCTION = 2.90917702627908"
    ## [1] "BEST method = 'both' PATH MEMBER = c( 63, 56 )"
    ## [1] "BEST both OBJECTIVE FUNCTION = 2.90917702627908"

![](man/figures/README-nns_forecasting-3.png)<!-- -->

    ## Package: healthyR.ts
    ## [1] "CURRNET METHOD: lin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 63 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 1.70421066993394"
    ## [1] "BEST method = 'lin', seasonal.factor = c( 63 )"
    ## [1] "BEST lin OBJECTIVE FUNCTION = 1.70421066993394"
    ## [1] "CURRNET METHOD: nonlin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'nonlin' , seasonal.factor =  c( 63 ) ...)"
    ## [1] "CURRENT nonlin OBJECTIVE FUNCTION = 8.82271625447218"
    ## [1] "BEST method = 'nonlin' PATH MEMBER = c( 63 )"
    ## [1] "BEST nonlin OBJECTIVE FUNCTION = 8.82271625447218"
    ## [1] "CURRNET METHOD: both"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'both' , seasonal.factor =  c( 63 ) ...)"
    ## [1] "CURRENT both OBJECTIVE FUNCTION = 3.82569278440415"
    ## [1] "BEST method = 'both' PATH MEMBER = c( 63 )"
    ## [1] "BEST both OBJECTIVE FUNCTION = 3.82569278440415"

![](man/figures/README-nns_forecasting-4.png)<!-- -->

    ## Package: healthyverse
    ## [1] "CURRNET METHOD: lin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 63 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 5.48045988077408"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 63, 77 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 4.58281238843823"
    ## [1] "BEST method = 'lin', seasonal.factor = c( 63, 77 )"
    ## [1] "BEST lin OBJECTIVE FUNCTION = 4.58281238843823"
    ## [1] "CURRNET METHOD: nonlin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'nonlin' , seasonal.factor =  c( 63, 77 ) ...)"
    ## [1] "CURRENT nonlin OBJECTIVE FUNCTION = 5.30654025499201"
    ## [1] "BEST method = 'nonlin' PATH MEMBER = c( 63, 77 )"
    ## [1] "BEST nonlin OBJECTIVE FUNCTION = 5.30654025499201"
    ## [1] "CURRNET METHOD: both"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'both' , seasonal.factor =  c( 63, 77 ) ...)"
    ## [1] "CURRENT both OBJECTIVE FUNCTION = 5.13874949365589"
    ## [1] "BEST method = 'both' PATH MEMBER = c( 63, 77 )"
    ## [1] "BEST both OBJECTIVE FUNCTION = 5.13874949365589"

![](man/figures/README-nns_forecasting-5.png)<!-- -->

    ## Package: RandomWalker
    ## [1] "CURRNET METHOD: lin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 70 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 3.31762557016292"
    ## [1] "BEST method = 'lin', seasonal.factor = c( 70 )"
    ## [1] "BEST lin OBJECTIVE FUNCTION = 3.31762557016292"
    ## [1] "CURRNET METHOD: nonlin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'nonlin' , seasonal.factor =  c( 70 ) ...)"
    ## [1] "CURRENT nonlin OBJECTIVE FUNCTION = 5.51125456496636"
    ## [1] "BEST method = 'nonlin' PATH MEMBER = c( 70 )"
    ## [1] "BEST nonlin OBJECTIVE FUNCTION = 5.51125456496636"
    ## [1] "CURRNET METHOD: both"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'both' , seasonal.factor =  c( 70 ) ...)"
    ## [1] "CURRENT both OBJECTIVE FUNCTION = 4.16360635650207"
    ## [1] "BEST method = 'both' PATH MEMBER = c( 70 )"
    ## [1] "BEST both OBJECTIVE FUNCTION = 4.16360635650207"

![](man/figures/README-nns_forecasting-6.png)<!-- -->

    ## Package: tidyAML
    ## [1] "CURRNET METHOD: lin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 84 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 4.66845076633351"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 84, 91 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 4.58994565569618"
    ## [1] "BEST method = 'lin', seasonal.factor = c( 84, 91 )"
    ## [1] "BEST lin OBJECTIVE FUNCTION = 4.58994565569618"
    ## [1] "CURRNET METHOD: nonlin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'nonlin' , seasonal.factor =  c( 84, 91 ) ...)"
    ## [1] "CURRENT nonlin OBJECTIVE FUNCTION = 3.13309089959658"
    ## [1] "BEST method = 'nonlin' PATH MEMBER = c( 84, 91 )"
    ## [1] "BEST nonlin OBJECTIVE FUNCTION = 3.13309089959658"
    ## [1] "CURRNET METHOD: both"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'both' , seasonal.factor =  c( 84, 91 ) ...)"
    ## [1] "CURRENT both OBJECTIVE FUNCTION = 3.61092668304761"
    ## [1] "BEST method = 'both' PATH MEMBER = c( 84, 91 )"
    ## [1] "BEST both OBJECTIVE FUNCTION = 3.61092668304761"

![](man/figures/README-nns_forecasting-7.png)<!-- -->

    ## Package: TidyDensity
    ## [1] "CURRNET METHOD: lin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 63 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 2.90228963726196"
    ## [1] "BEST method = 'lin', seasonal.factor = c( 63 )"
    ## [1] "BEST lin OBJECTIVE FUNCTION = 2.90228963726196"
    ## [1] "CURRNET METHOD: nonlin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'nonlin' , seasonal.factor =  c( 63 ) ...)"
    ## [1] "CURRENT nonlin OBJECTIVE FUNCTION = 9.55066273613481"
    ## [1] "BEST method = 'nonlin' PATH MEMBER = c( 63 )"
    ## [1] "BEST nonlin OBJECTIVE FUNCTION = 9.55066273613481"
    ## [1] "CURRNET METHOD: both"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'both' , seasonal.factor =  c( 63 ) ...)"
    ## [1] "CURRENT both OBJECTIVE FUNCTION = 5.55151009228586"
    ## [1] "BEST method = 'both' PATH MEMBER = c( 63 )"
    ## [1] "BEST both OBJECTIVE FUNCTION = 5.55151009228586"

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
    ## 1 healthyR.data <tibble [1,671 × 2]> <tibble [28 × 2]> <split [1643|28]>
    ## 2 healthyR      <tibble [1,665 × 2]> <tibble [28 × 2]> <split [1637|28]>
    ## 3 healthyR.ts   <tibble [1,609 × 2]> <tibble [28 × 2]> <split [1581|28]>
    ## 4 healthyverse  <tibble [1,579 × 2]> <tibble [28 × 2]> <split [1551|28]>
    ## 5 healthyR.ai   <tibble [1,404 × 2]> <tibble [28 × 2]> <split [1376|28]>
    ## 6 TidyDensity   <tibble [1,255 × 2]> <tibble [28 × 2]> <split [1227|28]>
    ## 7 tidyAML       <tibble [863 × 2]>   <tibble [28 × 2]> <split [835|28]> 
    ## 8 RandomWalker  <tibble [285 × 2]>   <tibble [28 × 2]> <split [257|28]>

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
| healthyR.data | 1 | ARIMA | Test | 0.6387835 | 181.65077 | 0.7804932 | 124.76827 | 0.8193868 | 0.0810782 |
| healthyR.data | 2 | LM | Test | 0.6653190 | 194.84458 | 0.8129155 | 126.22425 | 0.8483741 | 0.0305800 |
| healthyR.data | 3 | NULL | NA | NA | NA | NA | NA | NA | NA |
| healthyR.data | 4 | NNAR | Test | 0.6594280 | 118.71066 | 0.8057176 | 175.73597 | 0.8695703 | 0.0450964 |
| healthyR | 1 | ARIMA | Test | 0.7603318 | 139.70776 | 0.8032404 | 143.26609 | 0.9329311 | 0.0243760 |
| healthyR | 2 | LM | Test | 0.7661563 | 101.62832 | 0.8093935 | 168.30400 | 0.9521429 | 0.0608532 |
| healthyR | 3 | NULL | NA | NA | NA | NA | NA | NA | NA |
| healthyR | 4 | NNAR | Test | 0.7604749 | 111.46082 | 0.8033916 | 160.29883 | 0.9431446 | 0.0080150 |
| healthyR.ts | 1 | ARIMA | Test | 0.7785931 | 136.08432 | 0.6140600 | 150.76589 | 1.0534646 | 0.0796576 |
| healthyR.ts | 2 | LM | Test | 0.8937631 | 196.54070 | 0.7048922 | 152.02761 | 1.1077812 | 0.0263526 |
| healthyR.ts | 3 | NULL | NA | NA | NA | NA | NA | NA | NA |
| healthyR.ts | 4 | NNAR | Test | 0.8085585 | 101.03637 | 0.6376931 | 183.07903 | 1.0971784 | 0.0424065 |
| healthyverse | 1 | ARIMA | Test | 0.7631414 | 197.44323 | 0.8605687 | 91.71774 | 0.9209048 | 0.0681587 |
| healthyverse | 2 | LM | Test | 0.7687495 | 188.29561 | 0.8668927 | 92.24998 | 0.9442108 | 0.0403692 |
| healthyverse | 3 | NULL | NA | NA | NA | NA | NA | NA | NA |
| healthyverse | 4 | NNAR | Test | 0.7877840 | 134.67028 | 0.8883573 | 104.81231 | 0.9795342 | 0.1075584 |
| healthyR.ai | 1 | ARIMA | Test | 0.6620945 | 109.03854 | 0.7125492 | 153.37524 | 0.7838749 | 0.2066873 |
| healthyR.ai | 2 | LM | Test | 0.6965887 | 116.13173 | 0.7496720 | 157.61518 | 0.8228740 | 0.1303510 |
| healthyR.ai | 3 | NULL | NA | NA | NA | NA | NA | NA | NA |
| healthyR.ai | 4 | NNAR | Test | 0.6881330 | 122.32054 | 0.7405720 | 148.59840 | 0.8076908 | 0.0681417 |
| TidyDensity | 1 | ARIMA | Test | 0.5469748 | 84.11488 | 0.7606257 | 118.52974 | 0.7603363 | 0.0373289 |
| TidyDensity | 2 | LM | Test | 0.5225717 | 126.98490 | 0.7266905 | 84.37358 | 0.7452488 | 0.0465412 |
| TidyDensity | 3 | NULL | NA | NA | NA | NA | NA | NA | NA |
| TidyDensity | 4 | NNAR | Test | 0.5636644 | 76.52739 | 0.7838342 | 118.98276 | 0.7936086 | 0.0000105 |
| tidyAML | 1 | ARIMA | Test | 0.6586138 | 139.48543 | 0.7525541 | 95.65193 | 0.8092775 | 0.0007003 |
| tidyAML | 2 | LM | Test | 0.6180216 | 141.13836 | 0.7061722 | 86.44069 | 0.7734708 | 0.0033140 |
| tidyAML | 3 | NULL | NA | NA | NA | NA | NA | NA | NA |
| tidyAML | 4 | NNAR | Test | 0.6267195 | 144.38204 | 0.7161106 | 88.87107 | 0.7729049 | 0.0047296 |
| RandomWalker | 1 | ARIMA | Test | 1.1653781 | 107.99281 | 0.6764491 | 176.73611 | 1.3408082 | 0.0069643 |
| RandomWalker | 2 | LM | Test | 1.1841876 | 102.04249 | 0.6873672 | 185.98460 | 1.3399498 | 0.0152445 |
| RandomWalker | 3 | NULL | NA | NA | NA | NA | NA | NA | NA |
| RandomWalker | 4 | NNAR | Test | 1.2486977 | 151.63949 | 0.7248123 | 151.83370 | 1.4027720 | 0.0160437 |

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
    ## 1 healthyR.da…         1 ARIMA       Test  0.639  182. 0.780 125.  0.819 0.0811 
    ## 2 healthyR             1 ARIMA       Test  0.760  140. 0.803 143.  0.933 0.0244 
    ## 3 healthyR.ts          1 ARIMA       Test  0.779  136. 0.614 151.  1.05  0.0797 
    ## 4 healthyverse         1 ARIMA       Test  0.763  197. 0.861  91.7 0.921 0.0682 
    ## 5 healthyR.ai          1 ARIMA       Test  0.662  109. 0.713 153.  0.784 0.207  
    ## 6 TidyDensity          2 LM          Test  0.523  127. 0.727  84.4 0.745 0.0465 
    ## 7 tidyAML              4 NNAR        Test  0.627  144. 0.716  88.9 0.773 0.00473
    ## 8 RandomWalker         2 LM          Test  1.18   102. 0.687 186.  1.34  0.0152

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
    ## 1 healthyR.data <tibble>     <tibble>     <split [1643|28]> <mdl_tm_t [1 × 5]>
    ## 2 healthyR      <tibble>     <tibble>     <split [1637|28]> <mdl_tm_t [1 × 5]>
    ## 3 healthyR.ts   <tibble>     <tibble>     <split [1581|28]> <mdl_tm_t [1 × 5]>
    ## 4 healthyverse  <tibble>     <tibble>     <split [1551|28]> <mdl_tm_t [1 × 5]>
    ## 5 healthyR.ai   <tibble>     <tibble>     <split [1376|28]> <mdl_tm_t [1 × 5]>
    ## 6 TidyDensity   <tibble>     <tibble>     <split [1227|28]> <mdl_tm_t [1 × 5]>
    ## 7 tidyAML       <tibble>     <tibble>     <split [835|28]>  <mdl_tm_t [1 × 5]>
    ## 8 RandomWalker  <tibble>     <tibble>     <split [257|28]>  <mdl_tm_t [1 × 5]>

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
