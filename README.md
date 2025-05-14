Time Series Analysis and Nested Modeling of the Healthyverse Packages
================
Steven P. Sanderson II, MPH - Date:
14 May, 2025

This analysis follows a *Nested Modeltime Workflow*.

## Get Data

``` r
glimpse(downloads_tbl)
```

    ## Rows: 139,750
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

The last day in the data set is 2025-05-12 23:42:03, the file was
birthed on: 2024-08-07 07:35:44, and at report knit time is -6684.11
hours old. Happy analyzing!

Now that we have our data lets take a look at it using the `skimr`
package.

``` r
skim(downloads_tbl)
```

|                                                  |               |
|:-------------------------------------------------|:--------------|
| Name                                             | downloads_tbl |
| Number of rows                                   | 139750        |
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
| r_version     |    100727 |          0.28 |   5 |   5 |     0 |       46 |          0 |
| r_arch        |    100727 |          0.28 |   3 |   7 |     0 |        5 |          0 |
| r_os          |    100727 |          0.28 |   7 |  15 |     0 |       22 |          0 |
| package       |         0 |          1.00 |   7 |  13 |     0 |        8 |          0 |
| version       |         0 |          1.00 |   5 |  17 |     0 |       60 |          0 |
| country       |     11916 |          0.91 |   2 |   2 |     0 |      163 |          0 |

**Variable type: Date**

| skim_variable | n_missing | complete_rate | min | max | median | n_unique |
|:---|---:|---:|:---|:---|:---|---:|
| date | 0 | 1 | 2020-11-23 | 2025-05-12 | 2023-06-20 | 1632 |

**Variable type: numeric**

| skim_variable | n_missing | complete_rate | mean | sd | p0 | p25 | p50 | p75 | p100 | hist |
|:---|---:|---:|---:|---:|---:|---:|---:|---:|---:|:---|
| size | 0 | 1 | 1134694.24 | 1519774.30 | 355 | 14701 | 289680 | 2367751 | 5677952 | ▇▁▂▁▁ |
| ip_id | 0 | 1 | 10388.73 | 18489.26 | 1 | 292 | 3042 | 11639 | 209747 | ▇▁▁▁▁ |

**Variable type: POSIXct**

| skim_variable | n_missing | complete_rate | min | max | median | n_unique |
|:---|---:|---:|:---|:---|:---|---:|
| date_time | 0 | 1 | 2020-11-23 09:00:41 | 2025-05-12 23:42:03 | 2023-06-20 14:18:44 | 85377 |

**Variable type: Timespan**

| skim_variable | n_missing | complete_rate | min | max | median | n_unique |
|:--------------|----------:|--------------:|----:|----:|-------:|---------:|
| time          |         0 |             1 |   0 |  59 |     11 |       60 |

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
    ## -146.41  -35.69  -11.29   26.61  814.94 
    ## 
    ## Coefficients:
    ##                                                      Estimate Std. Error
    ## (Intercept)                                        -1.830e+02  7.040e+01
    ## date                                                1.120e-02  3.732e-03
    ## lag(value, 1)                                       1.032e-01  2.456e-02
    ## lag(value, 7)                                       9.634e-02  2.545e-02
    ## lag(value, 14)                                      8.898e-02  2.548e-02
    ## lag(value, 21)                                      6.796e-02  2.559e-02
    ## lag(value, 28)                                      6.999e-02  2.549e-02
    ## lag(value, 35)                                      6.639e-02  2.563e-02
    ## lag(value, 42)                                      4.643e-02  2.573e-02
    ## lag(value, 49)                                      6.732e-02  2.564e-02
    ## month(date, label = TRUE).L                        -9.954e+00  5.128e+00
    ## month(date, label = TRUE).Q                         3.108e+00  5.175e+00
    ## month(date, label = TRUE).C                        -1.264e+01  5.167e+00
    ## month(date, label = TRUE)^4                        -6.760e+00  5.198e+00
    ## month(date, label = TRUE)^5                        -1.194e+01  5.169e+00
    ## month(date, label = TRUE)^6                        -3.285e+00  5.241e+00
    ## month(date, label = TRUE)^7                        -6.637e+00  5.145e+00
    ## month(date, label = TRUE)^8                        -4.096e+00  5.147e+00
    ## month(date, label = TRUE)^9                         5.377e+00  5.145e+00
    ## month(date, label = TRUE)^10                        4.215e+00  5.178e+00
    ## month(date, label = TRUE)^11                       -5.557e+00  5.300e+00
    ## fourier_vec(date, type = "sin", K = 1, period = 7) -1.182e+01  2.361e+00
    ## fourier_vec(date, type = "cos", K = 1, period = 7)  8.381e+00  2.485e+00
    ##                                                    t value Pr(>|t|)    
    ## (Intercept)                                         -2.599 0.009441 ** 
    ## date                                                 3.001 0.002733 ** 
    ## lag(value, 1)                                        4.202 2.80e-05 ***
    ## lag(value, 7)                                        3.786 0.000159 ***
    ## lag(value, 14)                                       3.492 0.000492 ***
    ## lag(value, 21)                                       2.656 0.007993 ** 
    ## lag(value, 28)                                       2.746 0.006100 ** 
    ## lag(value, 35)                                       2.590 0.009686 ** 
    ## lag(value, 42)                                       1.805 0.071314 .  
    ## lag(value, 49)                                       2.626 0.008725 ** 
    ## month(date, label = TRUE).L                         -1.941 0.052447 .  
    ## month(date, label = TRUE).Q                          0.601 0.548160    
    ## month(date, label = TRUE).C                         -2.445 0.014575 *  
    ## month(date, label = TRUE)^4                         -1.301 0.193585    
    ## month(date, label = TRUE)^5                         -2.310 0.021027 *  
    ## month(date, label = TRUE)^6                         -0.627 0.530883    
    ## month(date, label = TRUE)^7                         -1.290 0.197268    
    ## month(date, label = TRUE)^8                         -0.796 0.426316    
    ## month(date, label = TRUE)^9                          1.045 0.296157    
    ## month(date, label = TRUE)^10                         0.814 0.415784    
    ## month(date, label = TRUE)^11                        -1.049 0.294520    
    ## fourier_vec(date, type = "sin", K = 1, period = 7)  -5.005 6.21e-07 ***
    ## fourier_vec(date, type = "cos", K = 1, period = 7)   3.372 0.000765 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 58.85 on 1560 degrees of freedom
    ##   (49 observations deleted due to missingness)
    ## Multiple R-squared:  0.2421, Adjusted R-squared:  0.2314 
    ## F-statistic: 22.65 on 22 and 1560 DF,  p-value: < 2.2e-16

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
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 84 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 3.70354069784446"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 84, 91 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 3.37061428615375"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 84, 91, 63 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 3.0167879026887"
    ## [1] "BEST method = 'lin', seasonal.factor = c( 84, 91, 63 )"
    ## [1] "BEST lin OBJECTIVE FUNCTION = 3.0167879026887"
    ## [1] "CURRNET METHOD: nonlin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'nonlin' , seasonal.factor =  c( 84, 91, 63 ) ...)"
    ## [1] "CURRENT nonlin OBJECTIVE FUNCTION = 3.65426750207294"
    ## [1] "BEST method = 'nonlin' PATH MEMBER = c( 84, 91, 63 )"
    ## [1] "BEST nonlin OBJECTIVE FUNCTION = 3.65426750207294"
    ## [1] "CURRNET METHOD: both"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'both' , seasonal.factor =  c( 84, 91, 63 ) ...)"
    ## [1] "CURRENT both OBJECTIVE FUNCTION = 3.26290215311353"
    ## [1] "BEST method = 'both' PATH MEMBER = c( 84, 91, 63 )"
    ## [1] "BEST both OBJECTIVE FUNCTION = 3.26290215311353"

![](man/figures/README-nns_forecasting-1.png)<!-- -->

    ## Package: healthyR.ai
    ## [1] "CURRNET METHOD: lin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 84 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 4.69886616798769"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 84, 63 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 4.64705914118706"
    ## [1] "BEST method = 'lin', seasonal.factor = c( 84, 63 )"
    ## [1] "BEST lin OBJECTIVE FUNCTION = 4.64705914118706"
    ## [1] "CURRNET METHOD: nonlin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'nonlin' , seasonal.factor =  c( 84, 63 ) ...)"
    ## [1] "CURRENT nonlin OBJECTIVE FUNCTION = 4.99971188146433"
    ## [1] "BEST method = 'nonlin' PATH MEMBER = c( 84, 63 )"
    ## [1] "BEST nonlin OBJECTIVE FUNCTION = 4.99971188146433"
    ## [1] "CURRNET METHOD: both"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'both' , seasonal.factor =  c( 84, 63 ) ...)"
    ## [1] "CURRENT both OBJECTIVE FUNCTION = 4.78278414361104"
    ## [1] "BEST method = 'both' PATH MEMBER = c( 84, 63 )"
    ## [1] "BEST both OBJECTIVE FUNCTION = 4.78278414361104"

![](man/figures/README-nns_forecasting-2.png)<!-- -->

    ## Package: healthyR.data
    ## [1] "CURRNET METHOD: lin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 21 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 2.71899258739161"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 21, 91 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 2.15512024745691"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 21, 91, 63 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 2.01835639269705"
    ## [1] "BEST method = 'lin', seasonal.factor = c( 21, 91, 63 )"
    ## [1] "BEST lin OBJECTIVE FUNCTION = 2.01835639269705"
    ## [1] "CURRNET METHOD: nonlin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'nonlin' , seasonal.factor =  c( 21, 91, 63 ) ...)"
    ## [1] "CURRENT nonlin OBJECTIVE FUNCTION = 1.74755719012091"
    ## [1] "BEST method = 'nonlin' PATH MEMBER = c( 21, 91, 63 )"
    ## [1] "BEST nonlin OBJECTIVE FUNCTION = 1.74755719012091"
    ## [1] "CURRNET METHOD: both"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'both' , seasonal.factor =  c( 21, 91, 63 ) ...)"
    ## [1] "CURRENT both OBJECTIVE FUNCTION = 1.703041219895"
    ## [1] "BEST method = 'both' PATH MEMBER = c( 21, 91, 63 )"
    ## [1] "BEST both OBJECTIVE FUNCTION = 1.703041219895"

![](man/figures/README-nns_forecasting-3.png)<!-- -->

    ## Package: healthyR.ts
    ## [1] "CURRNET METHOD: lin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 63 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 5.06600022494205"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 63, 91 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 4.36421415289236"
    ## [1] "BEST method = 'lin', seasonal.factor = c( 63, 91 )"
    ## [1] "BEST lin OBJECTIVE FUNCTION = 4.36421415289236"
    ## [1] "CURRNET METHOD: nonlin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'nonlin' , seasonal.factor =  c( 63, 91 ) ...)"
    ## [1] "CURRENT nonlin OBJECTIVE FUNCTION = 5.86112395251268"
    ## [1] "BEST method = 'nonlin' PATH MEMBER = c( 63, 91 )"
    ## [1] "BEST nonlin OBJECTIVE FUNCTION = 5.86112395251268"
    ## [1] "CURRNET METHOD: both"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'both' , seasonal.factor =  c( 63, 91 ) ...)"
    ## [1] "CURRENT both OBJECTIVE FUNCTION = 4.9717976830046"
    ## [1] "BEST method = 'both' PATH MEMBER = c( 63, 91 )"
    ## [1] "BEST both OBJECTIVE FUNCTION = 4.9717976830046"

![](man/figures/README-nns_forecasting-4.png)<!-- -->

    ## Package: healthyverse
    ## [1] "CURRNET METHOD: lin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 28 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 7.60506306857322"
    ## [1] "BEST method = 'lin', seasonal.factor = c( 28 )"
    ## [1] "BEST lin OBJECTIVE FUNCTION = 7.60506306857322"
    ## [1] "CURRNET METHOD: nonlin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'nonlin' , seasonal.factor =  c( 28 ) ...)"
    ## [1] "CURRENT nonlin OBJECTIVE FUNCTION = 4.96246064588702"
    ## [1] "BEST method = 'nonlin' PATH MEMBER = c( 28 )"
    ## [1] "BEST nonlin OBJECTIVE FUNCTION = 4.96246064588702"
    ## [1] "CURRNET METHOD: both"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'both' , seasonal.factor =  c( 28 ) ...)"
    ## [1] "CURRENT both OBJECTIVE FUNCTION = 4.82903367657672"
    ## [1] "BEST method = 'both' PATH MEMBER = c( 28 )"
    ## [1] "BEST both OBJECTIVE FUNCTION = 4.82903367657672"

![](man/figures/README-nns_forecasting-5.png)<!-- -->

    ## Package: RandomWalker
    ## [1] "CURRNET METHOD: lin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 35 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 3.24852792551691"
    ## [1] "BEST method = 'lin', seasonal.factor = c( 35 )"
    ## [1] "BEST lin OBJECTIVE FUNCTION = 3.24852792551691"
    ## [1] "CURRNET METHOD: nonlin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'nonlin' , seasonal.factor =  c( 35 ) ...)"
    ## [1] "CURRENT nonlin OBJECTIVE FUNCTION = 3.09273175672822"
    ## [1] "BEST method = 'nonlin' PATH MEMBER = c( 35 )"
    ## [1] "BEST nonlin OBJECTIVE FUNCTION = 3.09273175672822"
    ## [1] "CURRNET METHOD: both"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'both' , seasonal.factor =  c( 35 ) ...)"
    ## [1] "CURRENT both OBJECTIVE FUNCTION = 2.96462008598628"
    ## [1] "BEST method = 'both' PATH MEMBER = c( 35 )"
    ## [1] "BEST both OBJECTIVE FUNCTION = 2.96462008598628"

![](man/figures/README-nns_forecasting-6.png)<!-- -->

    ## Package: tidyAML
    ## [1] "CURRNET METHOD: lin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 28 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 2.63371794818969"
    ## [1] "BEST method = 'lin', seasonal.factor = c( 28 )"
    ## [1] "BEST lin OBJECTIVE FUNCTION = 2.63371794818969"
    ## [1] "CURRNET METHOD: nonlin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'nonlin' , seasonal.factor =  c( 28 ) ...)"
    ## [1] "CURRENT nonlin OBJECTIVE FUNCTION = 3.31929620032507"
    ## [1] "BEST method = 'nonlin' PATH MEMBER = c( 28 )"
    ## [1] "BEST nonlin OBJECTIVE FUNCTION = 3.31929620032507"
    ## [1] "CURRNET METHOD: both"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'both' , seasonal.factor =  c( 28 ) ...)"
    ## [1] "CURRENT both OBJECTIVE FUNCTION = 2.65890127286409"
    ## [1] "BEST method = 'both' PATH MEMBER = c( 28 )"
    ## [1] "BEST both OBJECTIVE FUNCTION = 2.65890127286409"

![](man/figures/README-nns_forecasting-7.png)<!-- -->

    ## Package: TidyDensity
    ## [1] "CURRNET METHOD: lin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 84 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 3.13880048228809"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 84, 28 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 2.8241828777476"
    ## [1] "BEST method = 'lin', seasonal.factor = c( 84, 28 )"
    ## [1] "BEST lin OBJECTIVE FUNCTION = 2.8241828777476"
    ## [1] "CURRNET METHOD: nonlin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'nonlin' , seasonal.factor =  c( 84, 28 ) ...)"
    ## [1] "CURRENT nonlin OBJECTIVE FUNCTION = 4.51267603176198"
    ## [1] "BEST method = 'nonlin' PATH MEMBER = c( 84, 28 )"
    ## [1] "BEST nonlin OBJECTIVE FUNCTION = 4.51267603176198"
    ## [1] "CURRNET METHOD: both"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'both' , seasonal.factor =  c( 84, 28 ) ...)"
    ## [1] "CURRENT both OBJECTIVE FUNCTION = 3.1871026246212"
    ## [1] "BEST method = 'both' PATH MEMBER = c( 84, 28 )"
    ## [1] "BEST both OBJECTIVE FUNCTION = 3.1871026246212"

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
    ## 1 healthyR.data <tibble [1,625 × 2]> <tibble [28 × 2]> <split [1597|28]>
    ## 2 healthyR      <tibble [1,618 × 2]> <tibble [28 × 2]> <split [1590|28]>
    ## 3 healthyR.ts   <tibble [1,562 × 2]> <tibble [28 × 2]> <split [1534|28]>
    ## 4 healthyverse  <tibble [1,532 × 2]> <tibble [28 × 2]> <split [1504|28]>
    ## 5 healthyR.ai   <tibble [1,357 × 2]> <tibble [28 × 2]> <split [1329|28]>
    ## 6 TidyDensity   <tibble [1,208 × 2]> <tibble [28 × 2]> <split [1180|28]>
    ## 7 tidyAML       <tibble [816 × 2]>   <tibble [28 × 2]> <split [788|28]> 
    ## 8 RandomWalker  <tibble [238 × 2]>   <tibble [28 × 2]> <split [210|28]>

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
| healthyR.data | 1 | ARIMA | Test | 0.6705702 | 136.20200 | 0.6652016 | 122.01554 | 0.8816281 | 0.0829962 |
| healthyR.data | 2 | LM | Test | 0.7028434 | 186.15519 | 0.6972164 | 117.06801 | 0.9166902 | 0.0119946 |
| healthyR.data | 3 | EARTH | Test | 0.7056258 | 229.28195 | 0.6999764 | 109.81371 | 0.9356272 | 0.0119946 |
| healthyR.data | 4 | NNAR | Test | 0.7636554 | 93.05739 | 0.7575415 | 169.25545 | 0.9619152 | 0.0000524 |
| healthyR | 1 | ARIMA | Test | 0.7176757 | 118.13965 | 0.8568407 | 188.15884 | 0.8667737 | 0.0161103 |
| healthyR | 2 | LM | Test | 0.6655294 | 92.29338 | 0.7945826 | 168.55485 | 0.8234029 | 0.0996339 |
| healthyR | 3 | EARTH | Test | 0.6248254 | 87.88612 | 0.7459857 | 133.20240 | 0.7846517 | 0.0996339 |
| healthyR | 4 | NNAR | Test | 0.6900002 | 91.73197 | 0.8237985 | 145.79778 | 0.8731823 | 0.0706889 |
| healthyR.ts | 1 | ARIMA | Test | 0.7726693 | 101.74665 | 0.7373489 | 176.45044 | 0.9661794 | 0.0459446 |
| healthyR.ts | 2 | LM | Test | 0.9451061 | 247.98520 | 0.9019032 | 156.75751 | 1.1368168 | 0.0459446 |
| healthyR.ts | 3 | EARTH | Test | 0.7485064 | 290.41090 | 0.7142905 | 120.54642 | 0.9603352 | 0.0459446 |
| healthyR.ts | 4 | NNAR | Test | 0.7748835 | 95.30984 | 0.7394619 | 172.29547 | 0.9957261 | 0.1021691 |
| healthyverse | 1 | ARIMA | Test | 0.6329398 | 340.60732 | 1.0120786 | 102.16254 | 0.7763191 | 0.1497952 |
| healthyverse | 2 | LM | Test | 0.6171287 | 387.27678 | 0.9867965 | 97.44235 | 0.7287623 | 0.0962422 |
| healthyverse | 3 | EARTH | Test | 0.6933218 | 520.97596 | 1.1086302 | 98.77056 | 0.7699511 | 0.0962422 |
| healthyverse | 4 | NNAR | Test | 0.6688446 | 272.07305 | 1.0694909 | 112.58839 | 0.8488980 | 0.1890407 |
| healthyR.ai | 1 | ARIMA | Test | 0.7939504 | 125.52335 | 0.9800884 | 173.52104 | 1.0444786 | 0.0041403 |
| healthyR.ai | 2 | LM | Test | 0.7749596 | 134.36508 | 0.9566453 | 156.62132 | 1.0433403 | 0.0665794 |
| healthyR.ai | 3 | EARTH | Test | 0.7766672 | 146.54400 | 0.9587532 | 146.70995 | 1.0589536 | 0.0665794 |
| healthyR.ai | 4 | NNAR | Test | 0.7950603 | 149.15590 | 0.9814585 | 156.35559 | 1.0519714 | 0.0000685 |
| TidyDensity | 1 | ARIMA | Test | 0.5423189 | 310.07007 | 1.0084045 | 118.40787 | 0.6595005 | 0.1077803 |
| TidyDensity | 2 | LM | Test | 0.5938989 | 425.10751 | 1.1043139 | 114.96730 | 0.7163960 | 0.0745517 |
| TidyDensity | 3 | EARTH | Test | 0.4961884 | 276.92886 | 0.9226280 | 115.32761 | 0.6013233 | 0.0745517 |
| TidyDensity | 4 | NNAR | Test | 0.5282818 | 162.36573 | 0.9823034 | 130.19426 | 0.6850164 | 0.0713911 |
| tidyAML | 1 | ARIMA | Test | 0.7016864 | 183.07163 | 0.8909627 | 109.38923 | 0.8296999 | 0.0022905 |
| tidyAML | 2 | LM | Test | 0.6681722 | 191.71904 | 0.8484082 | 96.19987 | 0.7998208 | 0.0522605 |
| tidyAML | 3 | EARTH | Test | 0.7183230 | 122.22881 | 0.9120868 | 121.09752 | 0.8764663 | 0.0522605 |
| tidyAML | 4 | NNAR | Test | 0.7200327 | 180.33456 | 0.9142578 | 111.59695 | 0.8625698 | 0.0305760 |
| RandomWalker | 1 | ARIMA | Test | 1.1444879 | 118.19900 | 0.5461870 | 139.51894 | 1.3944979 | 0.1454271 |
| RandomWalker | 2 | LM | Test | 1.2608683 | 105.70859 | 0.6017276 | 181.43958 | 1.4808933 | 0.0002791 |
| RandomWalker | 3 | EARTH | Test | 1.2580609 | 108.33769 | 0.6003878 | 172.38762 | 1.4897039 | NA |
| RandomWalker | 4 | NNAR | Test | 1.2393246 | 271.34486 | 0.5914462 | 139.00193 | 1.3738583 | 0.1510245 |

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
    ## 1 healthyR.data         1 ARIMA       Test  0.671 136.  0.665 122.  0.882 0.0830
    ## 2 healthyR              3 EARTH       Test  0.625  87.9 0.746 133.  0.785 0.0996
    ## 3 healthyR.ts           3 EARTH       Test  0.749 290.  0.714 121.  0.960 0.0459
    ## 4 healthyverse          2 LM          Test  0.617 387.  0.987  97.4 0.729 0.0962
    ## 5 healthyR.ai           2 LM          Test  0.775 134.  0.957 157.  1.04  0.0666
    ## 6 TidyDensity           3 EARTH       Test  0.496 277.  0.923 115.  0.601 0.0746
    ## 7 tidyAML               2 LM          Test  0.668 192.  0.848  96.2 0.800 0.0523
    ## 8 RandomWalker          4 NNAR        Test  1.24  271.  0.591 139.  1.37  0.151

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
    ## 1 healthyR.data <tibble>     <tibble>     <split [1597|28]> <mdl_tm_t [1 × 5]>
    ## 2 healthyR      <tibble>     <tibble>     <split [1590|28]> <mdl_tm_t [1 × 5]>
    ## 3 healthyR.ts   <tibble>     <tibble>     <split [1534|28]> <mdl_tm_t [1 × 5]>
    ## 4 healthyverse  <tibble>     <tibble>     <split [1504|28]> <mdl_tm_t [1 × 5]>
    ## 5 healthyR.ai   <tibble>     <tibble>     <split [1329|28]> <mdl_tm_t [1 × 5]>
    ## 6 TidyDensity   <tibble>     <tibble>     <split [1180|28]> <mdl_tm_t [1 × 5]>
    ## 7 tidyAML       <tibble>     <tibble>     <split [788|28]>  <mdl_tm_t [1 × 5]>
    ## 8 RandomWalker  <tibble>     <tibble>     <split [210|28]>  <mdl_tm_t [1 × 5]>

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
