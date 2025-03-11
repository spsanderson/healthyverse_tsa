Time Series Analysis and Nested Modeling of the Healthyverse Packages
================
Steven P. Sanderson II, MPH - Date:
11 March, 2025

This analysis follows a *Nested Modeltime Workflow*.

## Get Data

``` r
glimpse(downloads_tbl)
```

    ## Rows: 133,703
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

The last day in the data set is 2025-03-09 22:47:09, the file was
birthed on: 2024-08-07 07:35:44, and at report knit time is -5147.19
hours old. Happy analyzing!

Now that we have our data lets take a look at it using the `skimr`
package.

``` r
skim(downloads_tbl)
```

|                                                  |               |
|:-------------------------------------------------|:--------------|
| Name                                             | downloads_tbl |
| Number of rows                                   | 133703        |
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
| r_version     |     96012 |          0.28 |   5 |   5 |     0 |       46 |          0 |
| r_arch        |     96012 |          0.28 |   3 |   7 |     0 |        5 |          0 |
| r_os          |     96012 |          0.28 |   7 |  15 |     0 |       21 |          0 |
| package       |         0 |          1.00 |   7 |  13 |     0 |        8 |          0 |
| version       |         0 |          1.00 |   5 |  17 |     0 |       60 |          0 |
| country       |     11262 |          0.92 |   2 |   2 |     0 |      163 |          0 |

**Variable type: Date**

| skim_variable | n_missing | complete_rate | min | max | median | n_unique |
|:---|---:|---:|:---|:---|:---|---:|
| date | 0 | 1 | 2020-11-23 | 2025-03-09 | 2023-05-16 | 1568 |

**Variable type: numeric**

| skim_variable | n_missing | complete_rate | mean | sd | p0 | p25 | p50 | p75 | p100 | hist |
|:---|---:|---:|---:|---:|---:|---:|---:|---:|---:|:---|
| size | 0 | 1 | 1136378.38 | 1526348.91 | 355 | 14701 | 260638 | 2367791.0 | 5677952 | ▇▁▂▁▁ |
| ip_id | 0 | 1 | 10388.39 | 18371.12 | 1 | 300 | 3091 | 11862.5 | 209747 | ▇▁▁▁▁ |

**Variable type: POSIXct**

| skim_variable | n_missing | complete_rate | min | max | median | n_unique |
|:---|---:|---:|:---|:---|:---|---:|
| date_time | 0 | 1 | 2020-11-23 09:00:41 | 2025-03-09 22:47:09 | 2023-05-16 15:39:27 | 81078 |

**Variable type: Timespan**

| skim_variable | n_missing | complete_rate | min | max |     median | n_unique |
|:--------------|----------:|--------------:|----:|----:|-----------:|---------:|
| time          |         0 |             1 |   0 |  59 | 12H 8M 18S |       60 |

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
    ## -154.03  -35.03  -10.16   26.81  810.66 
    ## 
    ## Coefficients:
    ##                                                      Estimate Std. Error
    ## (Intercept)                                        -2.057e+02  7.545e+01
    ## date                                                1.226e-02  4.003e-03
    ## lag(value, 1)                                       1.117e-01  2.502e-02
    ## lag(value, 7)                                       8.990e-02  2.591e-02
    ## lag(value, 14)                                      9.343e-02  2.593e-02
    ## lag(value, 21)                                      6.834e-02  2.611e-02
    ## lag(value, 28)                                      6.507e-02  2.601e-02
    ## lag(value, 35)                                      6.746e-02  2.621e-02
    ## lag(value, 42)                                      5.474e-02  2.632e-02
    ## lag(value, 49)                                      8.722e-02  2.623e-02
    ## month(date, label = TRUE).L                        -1.188e+01  5.186e+00
    ## month(date, label = TRUE).Q                         2.441e+00  5.193e+00
    ## month(date, label = TRUE).C                        -1.127e+01  5.270e+00
    ## month(date, label = TRUE)^4                        -8.297e+00  5.240e+00
    ## month(date, label = TRUE)^5                        -1.195e+01  5.208e+00
    ## month(date, label = TRUE)^6                        -2.465e+00  5.290e+00
    ## month(date, label = TRUE)^7                        -7.726e+00  5.217e+00
    ## month(date, label = TRUE)^8                        -3.395e+00  5.252e+00
    ## month(date, label = TRUE)^9                         4.821e+00  5.288e+00
    ## month(date, label = TRUE)^10                        4.942e+00  5.312e+00
    ## month(date, label = TRUE)^11                       -6.204e+00  5.331e+00
    ## fourier_vec(date, type = "sin", K = 1, period = 7) -1.194e+01  2.406e+00
    ## fourier_vec(date, type = "cos", K = 1, period = 7)  7.871e+00  2.535e+00
    ##                                                    t value Pr(>|t|)    
    ## (Intercept)                                         -2.726 0.006485 ** 
    ## date                                                 3.063 0.002233 ** 
    ## lag(value, 1)                                        4.463 8.68e-06 ***
    ## lag(value, 7)                                        3.469 0.000537 ***
    ## lag(value, 14)                                       3.603 0.000325 ***
    ## lag(value, 21)                                       2.617 0.008969 ** 
    ## lag(value, 28)                                       2.502 0.012444 *  
    ## lag(value, 35)                                       2.574 0.010160 *  
    ## lag(value, 42)                                       2.080 0.037719 *  
    ## lag(value, 49)                                       3.325 0.000907 ***
    ## month(date, label = TRUE).L                         -2.290 0.022133 *  
    ## month(date, label = TRUE).Q                          0.470 0.638415    
    ## month(date, label = TRUE).C                         -2.139 0.032631 *  
    ## month(date, label = TRUE)^4                         -1.583 0.113562    
    ## month(date, label = TRUE)^5                         -2.294 0.021932 *  
    ## month(date, label = TRUE)^6                         -0.466 0.641215    
    ## month(date, label = TRUE)^7                         -1.481 0.138817    
    ## month(date, label = TRUE)^8                         -0.646 0.518166    
    ## month(date, label = TRUE)^9                          0.912 0.362132    
    ## month(date, label = TRUE)^10                         0.930 0.352341    
    ## month(date, label = TRUE)^11                        -1.164 0.244702    
    ## fourier_vec(date, type = "sin", K = 1, period = 7)  -4.964 7.71e-07 ***
    ## fourier_vec(date, type = "cos", K = 1, period = 7)   3.106 0.001934 ** 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 58.51 on 1496 degrees of freedom
    ##   (49 observations deleted due to missingness)
    ## Multiple R-squared:  0.2588, Adjusted R-squared:  0.2479 
    ## F-statistic: 23.75 on 22 and 1496 DF,  p-value: < 2.2e-16

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
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 1.31250053485873"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 77, 98 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 1.11392112855093"
    ## [1] "BEST method = 'lin', seasonal.factor = c( 77, 98 )"
    ## [1] "BEST lin OBJECTIVE FUNCTION = 1.11392112855093"
    ## [1] "CURRNET METHOD: nonlin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'nonlin' , seasonal.factor =  c( 77, 98 ) ...)"
    ## [1] "CURRENT nonlin OBJECTIVE FUNCTION = 1.21838784971717"
    ## [1] "BEST method = 'nonlin' PATH MEMBER = c( 77, 98 )"
    ## [1] "BEST nonlin OBJECTIVE FUNCTION = 1.21838784971717"
    ## [1] "CURRNET METHOD: both"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'both' , seasonal.factor =  c( 77, 98 ) ...)"
    ## [1] "CURRENT both OBJECTIVE FUNCTION = 1.07546992275325"
    ## [1] "BEST method = 'both' PATH MEMBER = c( 77, 98 )"
    ## [1] "BEST both OBJECTIVE FUNCTION = 1.07546992275325"

![](man/figures/README-nns_forecasting-1.png)<!-- -->

    ## Package: healthyR.ai
    ## [1] "CURRNET METHOD: lin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 77 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 1.34542428431121"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 77, 63 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 1.21435195899087"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 77, 63, 98 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 1.15935853797009"
    ## [1] "BEST method = 'lin', seasonal.factor = c( 77, 63, 98 )"
    ## [1] "BEST lin OBJECTIVE FUNCTION = 1.15935853797009"
    ## [1] "CURRNET METHOD: nonlin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'nonlin' , seasonal.factor =  c( 77, 63, 98 ) ...)"
    ## [1] "CURRENT nonlin OBJECTIVE FUNCTION = 1.80135102293991"
    ## [1] "BEST method = 'nonlin' PATH MEMBER = c( 77, 63, 98 )"
    ## [1] "BEST nonlin OBJECTIVE FUNCTION = 1.80135102293991"
    ## [1] "CURRNET METHOD: both"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'both' , seasonal.factor =  c( 77, 63, 98 ) ...)"
    ## [1] "CURRENT both OBJECTIVE FUNCTION = 1.36523346084843"
    ## [1] "BEST method = 'both' PATH MEMBER = c( 77, 63, 98 )"
    ## [1] "BEST both OBJECTIVE FUNCTION = 1.36523346084843"

![](man/figures/README-nns_forecasting-2.png)<!-- -->

    ## Package: healthyR.data
    ## [1] "CURRNET METHOD: lin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 98 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 1.53387730014084"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 98, 77 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 1.46358000150871"
    ## [1] "BEST method = 'lin', seasonal.factor = c( 98, 77 )"
    ## [1] "BEST lin OBJECTIVE FUNCTION = 1.46358000150871"
    ## [1] "CURRNET METHOD: nonlin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'nonlin' , seasonal.factor =  c( 98, 77 ) ...)"
    ## [1] "CURRENT nonlin OBJECTIVE FUNCTION = 2.02788377407384"
    ## [1] "BEST method = 'nonlin' PATH MEMBER = c( 98, 77 )"
    ## [1] "BEST nonlin OBJECTIVE FUNCTION = 2.02788377407384"
    ## [1] "CURRNET METHOD: both"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'both' , seasonal.factor =  c( 98, 77 ) ...)"
    ## [1] "CURRENT both OBJECTIVE FUNCTION = 1.69803792087616"
    ## [1] "BEST method = 'both' PATH MEMBER = c( 98, 77 )"
    ## [1] "BEST both OBJECTIVE FUNCTION = 1.69803792087616"

![](man/figures/README-nns_forecasting-3.png)<!-- -->

    ## Package: healthyR.ts
    ## [1] "CURRNET METHOD: lin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 14 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 1.47550448922748"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 14, 98 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 1.23055220870715"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 14, 98, 63 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 1.16869754236404"
    ## [1] "BEST method = 'lin', seasonal.factor = c( 14, 98, 63 )"
    ## [1] "BEST lin OBJECTIVE FUNCTION = 1.16869754236404"
    ## [1] "CURRNET METHOD: nonlin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'nonlin' , seasonal.factor =  c( 14, 98, 63 ) ...)"
    ## [1] "CURRENT nonlin OBJECTIVE FUNCTION = 1.23669492545444"
    ## [1] "BEST method = 'nonlin' PATH MEMBER = c( 14, 98, 63 )"
    ## [1] "BEST nonlin OBJECTIVE FUNCTION = 1.23669492545444"
    ## [1] "CURRNET METHOD: both"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'both' , seasonal.factor =  c( 14, 98, 63 ) ...)"
    ## [1] "CURRENT both OBJECTIVE FUNCTION = 1.15487296843066"
    ## [1] "BEST method = 'both' PATH MEMBER = c( 14, 98, 63 )"
    ## [1] "BEST both OBJECTIVE FUNCTION = 1.15487296843066"

![](man/figures/README-nns_forecasting-4.png)<!-- -->

    ## Package: healthyverse
    ## [1] "CURRNET METHOD: lin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 77 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 1.45422409196813"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 77, 49 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 1.33525732720517"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 77, 49, 70 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 1.27548769959492"
    ## [1] "BEST method = 'lin', seasonal.factor = c( 77, 49, 70 )"
    ## [1] "BEST lin OBJECTIVE FUNCTION = 1.27548769959492"
    ## [1] "CURRNET METHOD: nonlin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'nonlin' , seasonal.factor =  c( 77, 49, 70 ) ...)"
    ## [1] "CURRENT nonlin OBJECTIVE FUNCTION = 2.24439265994611"
    ## [1] "BEST method = 'nonlin' PATH MEMBER = c( 77, 49, 70 )"
    ## [1] "BEST nonlin OBJECTIVE FUNCTION = 2.24439265994611"
    ## [1] "CURRNET METHOD: both"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'both' , seasonal.factor =  c( 77, 49, 70 ) ...)"
    ## [1] "CURRENT both OBJECTIVE FUNCTION = 1.61586107516272"
    ## [1] "BEST method = 'both' PATH MEMBER = c( 77, 49, 70 )"
    ## [1] "BEST both OBJECTIVE FUNCTION = 1.61586107516272"

![](man/figures/README-nns_forecasting-5.png)<!-- -->

    ## Package: RandomWalker
    ## [1] "CURRNET METHOD: lin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 14 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 7.50525924122864"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 14, 28 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 7.27582006100112"
    ## [1] "BEST method = 'lin', seasonal.factor = c( 14, 28 )"
    ## [1] "BEST lin OBJECTIVE FUNCTION = 7.27582006100112"
    ## [1] "CURRNET METHOD: nonlin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'nonlin' , seasonal.factor =  c( 14, 28 ) ...)"
    ## [1] "CURRENT nonlin OBJECTIVE FUNCTION = 4.93974836686249"
    ## [1] "BEST method = 'nonlin' PATH MEMBER = c( 14, 28 )"
    ## [1] "BEST nonlin OBJECTIVE FUNCTION = 4.93974836686249"
    ## [1] "CURRNET METHOD: both"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'both' , seasonal.factor =  c( 14, 28 ) ...)"
    ## [1] "CURRENT both OBJECTIVE FUNCTION = 5.43888375781829"
    ## [1] "BEST method = 'both' PATH MEMBER = c( 14, 28 )"
    ## [1] "BEST both OBJECTIVE FUNCTION = 5.43888375781829"

![](man/figures/README-nns_forecasting-6.png)<!-- -->

    ## Package: tidyAML
    ## [1] "CURRNET METHOD: lin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 91 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 6.24773626243234"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 91, 49 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 6.19849241208524"
    ## [1] "BEST method = 'lin', seasonal.factor = c( 91, 49 )"
    ## [1] "BEST lin OBJECTIVE FUNCTION = 6.19849241208524"
    ## [1] "CURRNET METHOD: nonlin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'nonlin' , seasonal.factor =  c( 91, 49 ) ...)"
    ## [1] "CURRENT nonlin OBJECTIVE FUNCTION = 4.33738277458779"
    ## [1] "BEST method = 'nonlin' PATH MEMBER = c( 91, 49 )"
    ## [1] "BEST nonlin OBJECTIVE FUNCTION = 4.33738277458779"
    ## [1] "CURRNET METHOD: both"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'both' , seasonal.factor =  c( 91, 49 ) ...)"
    ## [1] "CURRENT both OBJECTIVE FUNCTION = 5.31913644758854"
    ## [1] "BEST method = 'both' PATH MEMBER = c( 91, 49 )"
    ## [1] "BEST both OBJECTIVE FUNCTION = 5.31913644758854"

![](man/figures/README-nns_forecasting-7.png)<!-- -->

    ## Package: TidyDensity
    ## [1] "CURRNET METHOD: lin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 77 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 1.48052165415687"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 77, 63 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 1.21877616467334"
    ## [1] "BEST method = 'lin', seasonal.factor = c( 77, 63 )"
    ## [1] "BEST lin OBJECTIVE FUNCTION = 1.21877616467334"
    ## [1] "CURRNET METHOD: nonlin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'nonlin' , seasonal.factor =  c( 77, 63 ) ...)"
    ## [1] "CURRENT nonlin OBJECTIVE FUNCTION = 2.19672672835102"
    ## [1] "BEST method = 'nonlin' PATH MEMBER = c( 77, 63 )"
    ## [1] "BEST nonlin OBJECTIVE FUNCTION = 2.19672672835102"
    ## [1] "CURRNET METHOD: both"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'both' , seasonal.factor =  c( 77, 63 ) ...)"
    ## [1] "CURRENT both OBJECTIVE FUNCTION = 1.56736979390256"
    ## [1] "BEST method = 'both' PATH MEMBER = c( 77, 63 )"
    ## [1] "BEST both OBJECTIVE FUNCTION = 1.56736979390256"

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
    ## 1 healthyR.data <tibble [1,561 × 2]> <tibble [28 × 2]> <split [1533|28]>
    ## 2 healthyR      <tibble [1,554 × 2]> <tibble [28 × 2]> <split [1526|28]>
    ## 3 healthyR.ts   <tibble [1,498 × 2]> <tibble [28 × 2]> <split [1470|28]>
    ## 4 healthyverse  <tibble [1,469 × 2]> <tibble [28 × 2]> <split [1441|28]>
    ## 5 healthyR.ai   <tibble [1,293 × 2]> <tibble [28 × 2]> <split [1265|28]>
    ## 6 TidyDensity   <tibble [1,144 × 2]> <tibble [28 × 2]> <split [1116|28]>
    ## 7 tidyAML       <tibble [752 × 2]>   <tibble [28 × 2]> <split [724|28]> 
    ## 8 RandomWalker  <tibble [174 × 2]>   <tibble [28 × 2]> <split [146|28]>

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
| healthyR.data | 1 | ARIMA | Test | 0.6989931 | 150.85083 | 0.6918463 | 160.26348 | 0.8208890 | 0.1259578 |
| healthyR.data | 2 | LM | Test | 0.8103485 | 203.21726 | 0.8020630 | 148.57115 | 0.9389519 | 0.1184817 |
| healthyR.data | 3 | EARTH | Test | 0.7469018 | 169.11283 | 0.7392650 | 127.74117 | 0.9590414 | 0.1184817 |
| healthyR.data | 4 | NNAR | Test | 0.7286807 | 115.25537 | 0.7212302 | 174.16871 | 0.8612054 | 0.0108449 |
| healthyR | 1 | ARIMA | Test | 0.8110185 | 168.28825 | 0.7575125 | 174.13420 | 0.9874402 | 0.0219718 |
| healthyR | 2 | LM | Test | 0.8170500 | 114.02418 | 0.7631461 | 181.41722 | 1.0082942 | 0.0848987 |
| healthyR | 3 | EARTH | Test | 0.8372539 | 148.09011 | 0.7820171 | 163.64124 | 1.0346307 | 0.0848987 |
| healthyR | 4 | NNAR | Test | 0.7855472 | 149.10622 | 0.7337217 | 153.66464 | 0.9692388 | 0.0682494 |
| healthyR.ts | 1 | ARIMA | Test | 0.9987972 | 112.32764 | 0.6925566 | 143.33602 | 1.1857651 | 0.1938584 |
| healthyR.ts | 2 | LM | Test | 0.9574525 | 118.68541 | 0.6638886 | 124.98472 | 1.1468717 | 0.1938584 |
| healthyR.ts | 3 | EARTH | Test | 0.9465596 | 120.40416 | 0.6563356 | 120.75783 | 1.1396454 | 0.1938584 |
| healthyR.ts | 4 | NNAR | Test | 0.9397645 | 103.67860 | 0.6516239 | 165.65854 | 1.1353251 | 0.1310665 |
| healthyverse | 1 | ARIMA | Test | 0.6762027 | 137.35234 | 0.8931787 | 112.12856 | 0.8541778 | 0.0175376 |
| healthyverse | 2 | LM | Test | 0.7607467 | 190.31996 | 1.0048507 | 111.52739 | 0.9313119 | 0.1975449 |
| healthyverse | 3 | EARTH | Test | 0.6657449 | 137.05394 | 0.8793653 | 109.48913 | 0.8581409 | 0.1975449 |
| healthyverse | 4 | NNAR | Test | 0.6610674 | 124.99012 | 0.8731869 | 120.03042 | 0.8329469 | 0.0209926 |
| healthyR.ai | 1 | ARIMA | Test | 0.7718837 | 96.99034 | 0.7314899 | 159.42968 | 0.9177177 | 0.0957030 |
| healthyR.ai | 2 | LM | Test | 0.8147589 | 97.88845 | 0.7721213 | 150.85030 | 0.9876585 | 0.1973002 |
| healthyR.ai | 3 | EARTH | Test | 0.8124937 | 99.70532 | 0.7699747 | 141.27412 | 1.0023566 | 0.1973002 |
| healthyR.ai | 4 | NNAR | Test | 0.7173918 | 92.79028 | 0.6798496 | 138.20236 | 0.8743796 | 0.1608603 |
| TidyDensity | 1 | ARIMA | Test | 0.6882623 | 132.76601 | 0.6935791 | 112.51553 | 0.8294822 | 0.0032647 |
| TidyDensity | 2 | LM | Test | 0.6953088 | 178.90963 | 0.7006800 | 105.01159 | 0.8074918 | 0.0831372 |
| TidyDensity | 3 | EARTH | Test | 0.6961797 | 118.64132 | 0.7015576 | 117.95217 | 0.8543905 | 0.0831372 |
| TidyDensity | 4 | NNAR | Test | 0.7169325 | 100.85516 | 0.7224707 | 136.83741 | 0.9102542 | 0.0244056 |
| tidyAML | 1 | ARIMA | Test | 0.6209121 | 137.68360 | 0.6628785 | 93.01318 | 0.7443181 | 0.0368897 |
| tidyAML | 2 | LM | Test | 0.6346033 | 144.06514 | 0.6774951 | 93.48221 | 0.7592652 | 0.0649183 |
| tidyAML | 3 | EARTH | Test | 0.6763528 | 173.75130 | 0.7220663 | 93.89009 | 0.7887844 | 0.0649183 |
| tidyAML | 4 | NNAR | Test | 0.6276081 | 142.63814 | 0.6700271 | 94.98223 | 0.7509369 | 0.0179134 |
| RandomWalker | 1 | ARIMA | Test | 0.9149203 | 153.99460 | 0.4106964 | 95.07771 | 1.0713385 | 0.4758526 |
| RandomWalker | 2 | LM | Test | 1.3104760 | 110.22345 | 0.5882565 | 194.51344 | 1.4649024 | 0.0126877 |
| RandomWalker | 3 | EARTH | Test | 1.2421680 | 86.42664 | 0.5575938 | 156.68506 | 1.4597279 | NA |
| RandomWalker | 4 | NNAR | Test | 1.3478235 | 161.85407 | 0.6050213 | 150.41055 | 1.6317083 | 0.0004354 |

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
    ## 1 healthyR.data         1 ARIMA       Test  0.699 151.  0.692 160.  0.821 0.126 
    ## 2 healthyR              4 NNAR        Test  0.786 149.  0.734 154.  0.969 0.0682
    ## 3 healthyR.ts           4 NNAR        Test  0.940 104.  0.652 166.  1.14  0.131 
    ## 4 healthyverse          4 NNAR        Test  0.661 125.  0.873 120.  0.833 0.0210
    ## 5 healthyR.ai           4 NNAR        Test  0.717  92.8 0.680 138.  0.874 0.161 
    ## 6 TidyDensity           2 LM          Test  0.695 179.  0.701 105.  0.807 0.0831
    ## 7 tidyAML               1 ARIMA       Test  0.621 138.  0.663  93.0 0.744 0.0369
    ## 8 RandomWalker          1 ARIMA       Test  0.915 154.  0.411  95.1 1.07  0.476

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
    ## 1 healthyR.data <tibble>     <tibble>     <split [1533|28]> <mdl_tm_t [1 × 5]>
    ## 2 healthyR      <tibble>     <tibble>     <split [1526|28]> <mdl_tm_t [1 × 5]>
    ## 3 healthyR.ts   <tibble>     <tibble>     <split [1470|28]> <mdl_tm_t [1 × 5]>
    ## 4 healthyverse  <tibble>     <tibble>     <split [1441|28]> <mdl_tm_t [1 × 5]>
    ## 5 healthyR.ai   <tibble>     <tibble>     <split [1265|28]> <mdl_tm_t [1 × 5]>
    ## 6 TidyDensity   <tibble>     <tibble>     <split [1116|28]> <mdl_tm_t [1 × 5]>
    ## 7 tidyAML       <tibble>     <tibble>     <split [724|28]>  <mdl_tm_t [1 × 5]>
    ## 8 RandomWalker  <tibble>     <tibble>     <split [146|28]>  <mdl_tm_t [1 × 5]>

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
