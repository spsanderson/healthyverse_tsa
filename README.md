Time Series Analysis and Nested Modeling of the Healthyverse Packages
================
Steven P. Sanderson II, MPH - Date:
17 June, 2025

This analysis follows a *Nested Modeltime Workflow*.

## Get Data

``` r
glimpse(downloads_tbl)
```

    ## Rows: 142,521
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

The last day in the data set is 2025-06-15 22:20:21, the file was
birthed on: 2024-08-07 07:35:44.428716, and at report knit time is
-7498.74 hours old. Happy analyzing!

Now that we have our data lets take a look at it using the `skimr`
package.

``` r
skim(downloads_tbl)
```

|                                                  |               |
|:-------------------------------------------------|:--------------|
| Name                                             | downloads_tbl |
| Number of rows                                   | 142521        |
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
| r_version     |    102847 |          0.28 |   5 |   5 |     0 |       47 |          0 |
| r_arch        |    102847 |          0.28 |   3 |   7 |     0 |        5 |          0 |
| r_os          |    102847 |          0.28 |   7 |  15 |     0 |       22 |          0 |
| package       |         0 |          1.00 |   7 |  13 |     0 |        8 |          0 |
| version       |         0 |          1.00 |   5 |  17 |     0 |       60 |          0 |
| country       |     12085 |          0.92 |   2 |   2 |     0 |      163 |          0 |

**Variable type: Date**

| skim_variable | n_missing | complete_rate | min | max | median | n_unique |
|:---|---:|---:|:---|:---|:---|---:|
| date | 0 | 1 | 2020-11-23 | 2025-06-15 | 2023-07-05 | 1666 |

**Variable type: numeric**

| skim_variable | n_missing | complete_rate | mean | sd | p0 | p25 | p50 | p75 | p100 | hist |
|:---|---:|---:|---:|---:|---:|---:|---:|---:|---:|:---|
| size | 0 | 1 | 1134346.81 | 1516556.82 | 355 | 14701 | 289918 | 2367728 | 5677952 | ▇▁▂▁▁ |
| ip_id | 0 | 1 | 10431.98 | 18577.53 | 1 | 287 | 3032 | 11700 | 209747 | ▇▁▁▁▁ |

**Variable type: POSIXct**

| skim_variable | n_missing | complete_rate | min | max | median | n_unique |
|:---|---:|---:|:---|:---|:---|---:|
| date_time | 0 | 1 | 2020-11-23 09:00:41 | 2025-06-15 22:20:21 | 2023-07-05 07:21:17 | 87409 |

**Variable type: Timespan**

| skim_variable | n_missing | complete_rate | min | max |     median | n_unique |
|:--------------|----------:|--------------:|----:|----:|-----------:|---------:|
| time          |         0 |             1 |   0 |  59 | 12H 6M 22S |       60 |

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
    ## -146.81  -35.64  -11.12   26.91  815.18 
    ## 
    ## Coefficients:
    ##                                                      Estimate Std. Error
    ## (Intercept)                                        -1.580e+02  6.731e+01
    ## date                                                9.871e-03  3.565e-03
    ## lag(value, 1)                                       1.027e-01  2.427e-02
    ## lag(value, 7)                                       9.639e-02  2.516e-02
    ## lag(value, 14)                                      8.952e-02  2.514e-02
    ## lag(value, 21)                                      6.803e-02  2.522e-02
    ## lag(value, 28)                                      7.029e-02  2.511e-02
    ## lag(value, 35)                                      6.871e-02  2.516e-02
    ## lag(value, 42)                                      4.929e-02  2.530e-02
    ## lag(value, 49)                                      6.913e-02  2.518e-02
    ## month(date, label = TRUE).L                        -9.595e+00  5.102e+00
    ## month(date, label = TRUE).Q                         4.076e+00  5.093e+00
    ## month(date, label = TRUE).C                        -1.349e+01  5.123e+00
    ## month(date, label = TRUE)^4                        -7.362e+00  5.145e+00
    ## month(date, label = TRUE)^5                        -1.099e+01  5.107e+00
    ## month(date, label = TRUE)^6                        -3.303e+00  5.189e+00
    ## month(date, label = TRUE)^7                        -7.634e+00  5.080e+00
    ## month(date, label = TRUE)^8                        -3.528e+00  5.078e+00
    ## month(date, label = TRUE)^9                         6.018e+00  5.082e+00
    ## month(date, label = TRUE)^10                        3.043e+00  5.065e+00
    ## month(date, label = TRUE)^11                       -4.847e+00  5.139e+00
    ## fourier_vec(date, type = "sin", K = 1, period = 7) -1.146e+01  2.320e+00
    ## fourier_vec(date, type = "cos", K = 1, period = 7)  8.034e+00  2.439e+00
    ##                                                    t value Pr(>|t|)    
    ## (Intercept)                                         -2.348 0.019019 *  
    ## date                                                 2.769 0.005689 ** 
    ## lag(value, 1)                                        4.231 2.46e-05 ***
    ## lag(value, 7)                                        3.831 0.000133 ***
    ## lag(value, 14)                                       3.561 0.000381 ***
    ## lag(value, 21)                                       2.698 0.007058 ** 
    ## lag(value, 28)                                       2.800 0.005173 ** 
    ## lag(value, 35)                                       2.731 0.006392 ** 
    ## lag(value, 42)                                       1.948 0.051571 .  
    ## lag(value, 49)                                       2.745 0.006110 ** 
    ## month(date, label = TRUE).L                         -1.880 0.060223 .  
    ## month(date, label = TRUE).Q                          0.800 0.423666    
    ## month(date, label = TRUE).C                         -2.633 0.008553 ** 
    ## month(date, label = TRUE)^4                         -1.431 0.152674    
    ## month(date, label = TRUE)^5                         -2.153 0.031476 *  
    ## month(date, label = TRUE)^6                         -0.637 0.524505    
    ## month(date, label = TRUE)^7                         -1.503 0.133090    
    ## month(date, label = TRUE)^8                         -0.695 0.487274    
    ## month(date, label = TRUE)^9                          1.184 0.236560    
    ## month(date, label = TRUE)^10                         0.601 0.548067    
    ## month(date, label = TRUE)^11                        -0.943 0.345762    
    ## fourier_vec(date, type = "sin", K = 1, period = 7)  -4.941 8.60e-07 ***
    ## fourier_vec(date, type = "cos", K = 1, period = 7)   3.294 0.001008 ** 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 58.65 on 1594 degrees of freedom
    ##   (49 observations deleted due to missingness)
    ## Multiple R-squared:  0.2383, Adjusted R-squared:  0.2278 
    ## F-statistic: 22.66 on 22 and 1594 DF,  p-value: < 2.2e-16

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
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 56 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 3.78083293164034"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 56, 63 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 3.09862816223659"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 56, 63, 21 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 3.02317779930191"
    ## [1] "BEST method = 'lin', seasonal.factor = c( 56, 63, 21 )"
    ## [1] "BEST lin OBJECTIVE FUNCTION = 3.02317779930191"
    ## [1] "CURRNET METHOD: nonlin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'nonlin' , seasonal.factor =  c( 56, 63, 21 ) ...)"
    ## [1] "CURRENT nonlin OBJECTIVE FUNCTION = 4.93875862627921"
    ## [1] "BEST method = 'nonlin' PATH MEMBER = c( 56, 63, 21 )"
    ## [1] "BEST nonlin OBJECTIVE FUNCTION = 4.93875862627921"
    ## [1] "CURRNET METHOD: both"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'both' , seasonal.factor =  c( 56, 63, 21 ) ...)"
    ## [1] "CURRENT both OBJECTIVE FUNCTION = 3.52654707244722"
    ## [1] "BEST method = 'both' PATH MEMBER = c( 56, 63, 21 )"
    ## [1] "BEST both OBJECTIVE FUNCTION = 3.52654707244722"

![](man/figures/README-nns_forecasting-1.png)<!-- -->

    ## Package: healthyR.ai
    ## [1] "CURRNET METHOD: lin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 63 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 4.22663053600812"
    ## [1] "BEST method = 'lin', seasonal.factor = c( 63 )"
    ## [1] "BEST lin OBJECTIVE FUNCTION = 4.22663053600812"
    ## [1] "CURRNET METHOD: nonlin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'nonlin' , seasonal.factor =  c( 63 ) ...)"
    ## [1] "CURRENT nonlin OBJECTIVE FUNCTION = 4.37325863949502"
    ## [1] "BEST method = 'nonlin' PATH MEMBER = c( 63 )"
    ## [1] "BEST nonlin OBJECTIVE FUNCTION = 4.37325863949502"
    ## [1] "CURRNET METHOD: both"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'both' , seasonal.factor =  c( 63 ) ...)"
    ## [1] "CURRENT both OBJECTIVE FUNCTION = 4.11436707034228"
    ## [1] "BEST method = 'both' PATH MEMBER = c( 63 )"
    ## [1] "BEST both OBJECTIVE FUNCTION = 4.11436707034228"

![](man/figures/README-nns_forecasting-2.png)<!-- -->

    ## Package: healthyR.data
    ## [1] "CURRNET METHOD: lin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 84 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 3.7057319939123"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 84, 63 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 3.06300423921851"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 84, 63, 56 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 2.87448274289272"
    ## [1] "BEST method = 'lin', seasonal.factor = c( 84, 63, 56 )"
    ## [1] "BEST lin OBJECTIVE FUNCTION = 2.87448274289272"
    ## [1] "CURRNET METHOD: nonlin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'nonlin' , seasonal.factor =  c( 84, 63, 56 ) ...)"
    ## [1] "CURRENT nonlin OBJECTIVE FUNCTION = 4.92408879191941"
    ## [1] "BEST method = 'nonlin' PATH MEMBER = c( 84, 63, 56 )"
    ## [1] "BEST nonlin OBJECTIVE FUNCTION = 4.92408879191941"
    ## [1] "CURRNET METHOD: both"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'both' , seasonal.factor =  c( 84, 63, 56 ) ...)"
    ## [1] "CURRENT both OBJECTIVE FUNCTION = 3.52881168692518"
    ## [1] "BEST method = 'both' PATH MEMBER = c( 84, 63, 56 )"
    ## [1] "BEST both OBJECTIVE FUNCTION = 3.52881168692518"

![](man/figures/README-nns_forecasting-3.png)<!-- -->

    ## Package: healthyR.ts
    ## [1] "CURRNET METHOD: lin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 63 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 2.69327999126206"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 63, 42 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 2.32182480760956"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 63, 42, 56 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 2.28243357396167"
    ## [1] "BEST method = 'lin', seasonal.factor = c( 63, 42, 56 )"
    ## [1] "BEST lin OBJECTIVE FUNCTION = 2.28243357396167"
    ## [1] "CURRNET METHOD: nonlin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'nonlin' , seasonal.factor =  c( 63, 42, 56 ) ...)"
    ## [1] "CURRENT nonlin OBJECTIVE FUNCTION = 12.3974517078415"
    ## [1] "BEST method = 'nonlin' PATH MEMBER = c( 63, 42, 56 )"
    ## [1] "BEST nonlin OBJECTIVE FUNCTION = 12.3974517078415"
    ## [1] "CURRNET METHOD: both"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'both' , seasonal.factor =  c( 63, 42, 56 ) ...)"
    ## [1] "CURRENT both OBJECTIVE FUNCTION = 4.48083243929011"
    ## [1] "BEST method = 'both' PATH MEMBER = c( 63, 42, 56 )"
    ## [1] "BEST both OBJECTIVE FUNCTION = 4.48083243929011"

![](man/figures/README-nns_forecasting-4.png)<!-- -->

    ## Package: healthyverse
    ## [1] "CURRNET METHOD: lin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 35 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 3.62295455021836"
    ## [1] "BEST method = 'lin', seasonal.factor = c( 35 )"
    ## [1] "BEST lin OBJECTIVE FUNCTION = 3.62295455021836"
    ## [1] "CURRNET METHOD: nonlin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'nonlin' , seasonal.factor =  c( 35 ) ...)"
    ## [1] "CURRENT nonlin OBJECTIVE FUNCTION = 4.03731632785323"
    ## [1] "BEST method = 'nonlin' PATH MEMBER = c( 35 )"
    ## [1] "BEST nonlin OBJECTIVE FUNCTION = 4.03731632785323"
    ## [1] "CURRNET METHOD: both"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'both' , seasonal.factor =  c( 35 ) ...)"
    ## [1] "CURRENT both OBJECTIVE FUNCTION = 2.89982731207304"
    ## [1] "BEST method = 'both' PATH MEMBER = c( 35 )"
    ## [1] "BEST both OBJECTIVE FUNCTION = 2.89982731207304"

![](man/figures/README-nns_forecasting-5.png)<!-- -->

    ## Package: RandomWalker
    ## [1] "CURRNET METHOD: lin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 70 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 3.84799106593199"
    ## [1] "BEST method = 'lin', seasonal.factor = c( 70 )"
    ## [1] "BEST lin OBJECTIVE FUNCTION = 3.84799106593199"
    ## [1] "CURRNET METHOD: nonlin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'nonlin' , seasonal.factor =  c( 70 ) ...)"
    ## [1] "CURRENT nonlin OBJECTIVE FUNCTION = 4.69095417721736"
    ## [1] "BEST method = 'nonlin' PATH MEMBER = c( 70 )"
    ## [1] "BEST nonlin OBJECTIVE FUNCTION = 4.69095417721736"
    ## [1] "CURRNET METHOD: both"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'both' , seasonal.factor =  c( 70 ) ...)"
    ## [1] "CURRENT both OBJECTIVE FUNCTION = 4.11226775030595"
    ## [1] "BEST method = 'both' PATH MEMBER = c( 70 )"
    ## [1] "BEST both OBJECTIVE FUNCTION = 4.11226775030595"

![](man/figures/README-nns_forecasting-6.png)<!-- -->

    ## Package: tidyAML
    ## [1] "CURRNET METHOD: lin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 84 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 5.62576685609099"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 84, 91 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 5.4966434856712"
    ## [1] "BEST method = 'lin', seasonal.factor = c( 84, 91 )"
    ## [1] "BEST lin OBJECTIVE FUNCTION = 5.4966434856712"
    ## [1] "CURRNET METHOD: nonlin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'nonlin' , seasonal.factor =  c( 84, 91 ) ...)"
    ## [1] "CURRENT nonlin OBJECTIVE FUNCTION = 3.55568257057522"
    ## [1] "BEST method = 'nonlin' PATH MEMBER = c( 84, 91 )"
    ## [1] "BEST nonlin OBJECTIVE FUNCTION = 3.55568257057522"
    ## [1] "CURRNET METHOD: both"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'both' , seasonal.factor =  c( 84, 91 ) ...)"
    ## [1] "CURRENT both OBJECTIVE FUNCTION = 4.26479450273704"
    ## [1] "BEST method = 'both' PATH MEMBER = c( 84, 91 )"
    ## [1] "BEST both OBJECTIVE FUNCTION = 4.26479450273704"

![](man/figures/README-nns_forecasting-7.png)<!-- -->

    ## Package: TidyDensity
    ## [1] "CURRNET METHOD: lin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 35 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 5.14569528940359"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 35, 63 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 3.84739427535103"
    ## [1] "BEST method = 'lin', seasonal.factor = c( 35, 63 )"
    ## [1] "BEST lin OBJECTIVE FUNCTION = 3.84739427535103"
    ## [1] "CURRNET METHOD: nonlin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'nonlin' , seasonal.factor =  c( 35, 63 ) ...)"
    ## [1] "CURRENT nonlin OBJECTIVE FUNCTION = 6.71205771933361"
    ## [1] "BEST method = 'nonlin' PATH MEMBER = c( 35, 63 )"
    ## [1] "BEST nonlin OBJECTIVE FUNCTION = 6.71205771933361"
    ## [1] "CURRNET METHOD: both"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'both' , seasonal.factor =  c( 35, 63 ) ...)"
    ## [1] "CURRENT both OBJECTIVE FUNCTION = 4.80656390322323"
    ## [1] "BEST method = 'both' PATH MEMBER = c( 35, 63 )"
    ## [1] "BEST both OBJECTIVE FUNCTION = 4.80656390322323"

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
    ## 1 healthyR.data <tibble [1,658 × 2]> <tibble [28 × 2]> <split [1630|28]>
    ## 2 healthyR      <tibble [1,652 × 2]> <tibble [28 × 2]> <split [1624|28]>
    ## 3 healthyR.ts   <tibble [1,596 × 2]> <tibble [28 × 2]> <split [1568|28]>
    ## 4 healthyverse  <tibble [1,566 × 2]> <tibble [28 × 2]> <split [1538|28]>
    ## 5 healthyR.ai   <tibble [1,391 × 2]> <tibble [28 × 2]> <split [1363|28]>
    ## 6 TidyDensity   <tibble [1,242 × 2]> <tibble [28 × 2]> <split [1214|28]>
    ## 7 tidyAML       <tibble [850 × 2]>   <tibble [28 × 2]> <split [822|28]> 
    ## 8 RandomWalker  <tibble [272 × 2]>   <tibble [28 × 2]> <split [244|28]>

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
| healthyR.data | 1 | ARIMA | Test | 0.5769847 | 124.36143 | 0.6396925 | 123.96472 | 0.7642366 | 0.0832545 |
| healthyR.data | 2 | LM | Test | 0.6061236 | 163.32109 | 0.6719984 | 115.23968 | 0.7695681 | 0.0001498 |
| healthyR.data | 3 | NULL | NA | NA | NA | NA | NA | NA | NA |
| healthyR.data | 4 | NNAR | Test | 0.6714347 | 98.32367 | 0.7444076 | 184.90127 | 0.9036788 | 0.0096728 |
| healthyR | 1 | ARIMA | Test | 0.7573535 | 108.88179 | 0.8635908 | 174.23504 | 0.8794500 | 0.0358217 |
| healthyR | 2 | LM | Test | 0.7598488 | 97.47604 | 0.8664361 | 176.40347 | 0.9034257 | 0.0020769 |
| healthyR | 3 | NULL | NA | NA | NA | NA | NA | NA | NA |
| healthyR | 4 | NNAR | Test | 0.7296528 | 97.02221 | 0.8320044 | 158.46589 | 0.8645295 | 0.1008504 |
| healthyR.ts | 1 | ARIMA | Test | 0.8112921 | 101.62354 | 0.7162523 | 185.76745 | 1.0666797 | 0.0023942 |
| healthyR.ts | 2 | LM | Test | 0.9509663 | 160.14107 | 0.8395642 | 151.79828 | 1.2116293 | 0.0479050 |
| healthyR.ts | 3 | NULL | NA | NA | NA | NA | NA | NA | NA |
| healthyR.ts | 4 | NNAR | Test | 0.8162980 | 100.41061 | 0.7206717 | 182.60687 | 1.0723619 | 0.0002309 |
| healthyverse | 1 | ARIMA | Test | 0.6272108 | 156.19947 | 1.1864329 | 80.80956 | 0.7710406 | 0.0323739 |
| healthyverse | 2 | LM | Test | 0.5966446 | 168.36353 | 1.1286139 | 76.28560 | 0.7130334 | 0.0098079 |
| healthyverse | 3 | NULL | NA | NA | NA | NA | NA | NA | NA |
| healthyverse | 4 | NNAR | Test | 0.6790585 | 111.71195 | 1.2845081 | 93.42693 | 0.8710909 | 0.0212466 |
| healthyR.ai | 1 | ARIMA | Test | 0.6525775 | 97.29772 | 0.7357182 | 158.20166 | 0.7903203 | 0.0175863 |
| healthyR.ai | 2 | LM | Test | 0.6362037 | 101.95698 | 0.7172583 | 142.65486 | 0.7692139 | 0.0005527 |
| healthyR.ai | 3 | NULL | NA | NA | NA | NA | NA | NA | NA |
| healthyR.ai | 4 | NNAR | Test | 0.6075822 | 105.72005 | 0.6849902 | 127.88472 | 0.7380746 | 0.0295722 |
| TidyDensity | 1 | ARIMA | Test | 0.4820946 | 104.22188 | 0.9710372 | 104.89697 | 0.6358710 | 0.0005904 |
| TidyDensity | 2 | LM | Test | 0.6082978 | 161.05586 | 1.2252364 | 107.63689 | 0.7942561 | 0.0492374 |
| TidyDensity | 3 | NULL | NA | NA | NA | NA | NA | NA | NA |
| TidyDensity | 4 | NNAR | Test | 0.4658467 | 83.79059 | 0.9383106 | 120.14523 | 0.5818016 | 0.0220583 |
| tidyAML | 1 | ARIMA | Test | 0.7283439 | 93.16584 | 0.7577400 | 127.21022 | 0.9315918 | 0.0000458 |
| tidyAML | 2 | LM | Test | 0.5870505 | 114.43073 | 0.6107439 | 79.80988 | 0.7796806 | 0.1245822 |
| tidyAML | 3 | NULL | NA | NA | NA | NA | NA | NA | NA |
| tidyAML | 4 | NNAR | Test | 0.6425159 | 102.56781 | 0.6684479 | 106.88044 | 0.8031043 | 0.1680441 |
| RandomWalker | 1 | ARIMA | Test | 1.3667432 | 173.64741 | 0.7362712 | 148.87312 | 1.5993081 | 0.0207203 |
| RandomWalker | 2 | LM | Test | 1.2898234 | 117.12425 | 0.6948342 | 189.76518 | 1.4220087 | 0.0088204 |
| RandomWalker | 3 | NULL | NA | NA | NA | NA | NA | NA | NA |
| RandomWalker | 4 | NNAR | Test | 1.4106495 | 158.83986 | 0.7599238 | 167.23582 | 1.5882704 | 0.0000310 |

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
    ## 1 healthyR.da…         1 ARIMA       Test  0.577 124.  0.640 124.  0.764 0.0833 
    ## 2 healthyR             4 NNAR        Test  0.730  97.0 0.832 158.  0.865 0.101  
    ## 3 healthyR.ts          1 ARIMA       Test  0.811 102.  0.716 186.  1.07  0.00239
    ## 4 healthyverse         2 LM          Test  0.597 168.  1.13   76.3 0.713 0.00981
    ## 5 healthyR.ai          4 NNAR        Test  0.608 106.  0.685 128.  0.738 0.0296 
    ## 6 TidyDensity          4 NNAR        Test  0.466  83.8 0.938 120.  0.582 0.0221 
    ## 7 tidyAML              2 LM          Test  0.587 114.  0.611  79.8 0.780 0.125  
    ## 8 RandomWalker         2 LM          Test  1.29  117.  0.695 190.  1.42  0.00882

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
    ## 1 healthyR.data <tibble>     <tibble>     <split [1630|28]> <mdl_tm_t [1 × 5]>
    ## 2 healthyR      <tibble>     <tibble>     <split [1624|28]> <mdl_tm_t [1 × 5]>
    ## 3 healthyR.ts   <tibble>     <tibble>     <split [1568|28]> <mdl_tm_t [1 × 5]>
    ## 4 healthyverse  <tibble>     <tibble>     <split [1538|28]> <mdl_tm_t [1 × 5]>
    ## 5 healthyR.ai   <tibble>     <tibble>     <split [1363|28]> <mdl_tm_t [1 × 5]>
    ## 6 TidyDensity   <tibble>     <tibble>     <split [1214|28]> <mdl_tm_t [1 × 5]>
    ## 7 tidyAML       <tibble>     <tibble>     <split [822|28]>  <mdl_tm_t [1 × 5]>
    ## 8 RandomWalker  <tibble>     <tibble>     <split [244|28]>  <mdl_tm_t [1 × 5]>

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
