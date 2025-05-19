Time Series Analysis and Nested Modeling of the Healthyverse Packages
================
Steven P. Sanderson II, MPH - Date:
19 May, 2025

This analysis follows a *Nested Modeltime Workflow*.

## Get Data

``` r
glimpse(downloads_tbl)
```

    ## Rows: 140,300
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

The last day in the data set is 2025-05-17 22:13:41, the file was
birthed on: 2022-07-02 23:58:17.511888, and at report knit time is
-2.519426^{4} hours old. Happy analyzing!

Now that we have our data lets take a look at it using the `skimr`
package.

``` r
skim(downloads_tbl)
```

|                                                  |               |
|:-------------------------------------------------|:--------------|
| Name                                             | downloads_tbl |
| Number of rows                                   | 140300        |
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
| r_version     |    101115 |          0.28 |   5 |   5 |     0 |       46 |          0 |
| r_arch        |    101115 |          0.28 |   3 |   7 |     0 |        5 |          0 |
| r_os          |    101115 |          0.28 |   7 |  15 |     0 |       22 |          0 |
| package       |         0 |          1.00 |   7 |  13 |     0 |        8 |          0 |
| version       |         0 |          1.00 |   5 |  17 |     0 |       60 |          0 |
| country       |     11967 |          0.91 |   2 |   2 |     0 |      163 |          0 |

**Variable type: Date**

| skim_variable | n_missing | complete_rate | min | max | median | n_unique |
|:---|---:|---:|:---|:---|:---|---:|
| date | 0 | 1 | 2020-11-23 | 2025-05-17 | 2023-06-23 | 1637 |

**Variable type: numeric**

| skim_variable | n_missing | complete_rate | mean | sd | p0 | p25 | p50 | p75 | p100 | hist |
|:---|---:|---:|---:|---:|---:|---:|---:|---:|---:|:---|
| size | 0 | 1 | 1133609.77 | 1518737.62 | 355 | 14701.00 | 289680 | 2367737 | 5677952 | ▇▁▂▁▁ |
| ip_id | 0 | 1 | 10427.81 | 18565.46 | 1 | 288.75 | 3058 | 11706 | 209747 | ▇▁▁▁▁ |

**Variable type: POSIXct**

| skim_variable | n_missing | complete_rate | min | max | median | n_unique |
|:---|---:|---:|:---|:---|:---|---:|
| date_time | 0 | 1 | 2020-11-23 09:00:41 | 2025-05-17 22:13:41 | 2023-06-23 01:10:13 | 85763 |

**Variable type: Timespan**

| skim_variable | n_missing | complete_rate | min | max | median | n_unique |
|:--------------|----------:|--------------:|----:|----:|-------:|---------:|
| time          |         0 |             1 |   0 |  59 |     59 |       60 |

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
    ## -147.32  -35.65  -11.25   26.66  814.92 
    ## 
    ## Coefficients:
    ##                                                      Estimate Std. Error
    ## (Intercept)                                        -1.819e+02  6.991e+01
    ## date                                                1.114e-02  3.706e-03
    ## lag(value, 1)                                       1.024e-01  2.453e-02
    ## lag(value, 7)                                       9.474e-02  2.538e-02
    ## lag(value, 14)                                      8.902e-02  2.540e-02
    ## lag(value, 21)                                      6.686e-02  2.554e-02
    ## lag(value, 28)                                      7.184e-02  2.543e-02
    ## lag(value, 35)                                      6.525e-02  2.551e-02
    ## lag(value, 42)                                      4.806e-02  2.569e-02
    ## lag(value, 49)                                      6.925e-02  2.552e-02
    ## month(date, label = TRUE).L                        -9.988e+00  5.124e+00
    ## month(date, label = TRUE).Q                         3.134e+00  5.164e+00
    ## month(date, label = TRUE).C                        -1.271e+01  5.157e+00
    ## month(date, label = TRUE)^4                        -6.837e+00  5.194e+00
    ## month(date, label = TRUE)^5                        -1.195e+01  5.155e+00
    ## month(date, label = TRUE)^6                        -3.364e+00  5.238e+00
    ## month(date, label = TRUE)^7                        -6.661e+00  5.133e+00
    ## month(date, label = TRUE)^8                        -4.144e+00  5.137e+00
    ## month(date, label = TRUE)^9                         5.414e+00  5.140e+00
    ## month(date, label = TRUE)^10                        4.237e+00  5.152e+00
    ## month(date, label = TRUE)^11                       -5.602e+00  5.283e+00
    ## fourier_vec(date, type = "sin", K = 1, period = 7) -1.180e+01  2.353e+00
    ## fourier_vec(date, type = "cos", K = 1, period = 7)  8.278e+00  2.478e+00
    ##                                                    t value Pr(>|t|)    
    ## (Intercept)                                         -2.602 0.009343 ** 
    ## date                                                 3.007 0.002677 ** 
    ## lag(value, 1)                                        4.177 3.12e-05 ***
    ## lag(value, 7)                                        3.733 0.000196 ***
    ## lag(value, 14)                                       3.504 0.000471 ***
    ## lag(value, 21)                                       2.618 0.008933 ** 
    ## lag(value, 28)                                       2.825 0.004786 ** 
    ## lag(value, 35)                                       2.558 0.010620 *  
    ## lag(value, 42)                                       1.871 0.061509 .  
    ## lag(value, 49)                                       2.714 0.006727 ** 
    ## month(date, label = TRUE).L                         -1.949 0.051447 .  
    ## month(date, label = TRUE).Q                          0.607 0.543963    
    ## month(date, label = TRUE).C                         -2.465 0.013806 *  
    ## month(date, label = TRUE)^4                         -1.317 0.188194    
    ## month(date, label = TRUE)^5                         -2.317 0.020617 *  
    ## month(date, label = TRUE)^6                         -0.642 0.520837    
    ## month(date, label = TRUE)^7                         -1.298 0.194578    
    ## month(date, label = TRUE)^8                         -0.807 0.420018    
    ## month(date, label = TRUE)^9                          1.053 0.292337    
    ## month(date, label = TRUE)^10                         0.822 0.411008    
    ## month(date, label = TRUE)^11                        -1.060 0.289167    
    ## fourier_vec(date, type = "sin", K = 1, period = 7)  -5.017 5.85e-07 ***
    ## fourier_vec(date, type = "cos", K = 1, period = 7)   3.341 0.000854 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 58.83 on 1565 degrees of freedom
    ##   (49 observations deleted due to missingness)
    ## Multiple R-squared:  0.2419, Adjusted R-squared:  0.2312 
    ## F-statistic:  22.7 on 22 and 1565 DF,  p-value: < 2.2e-16

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
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 4.23094037473475"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 63, 84 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 3.55858375747988"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 63, 84, 91 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 3.16555939906146"
    ## [1] "BEST method = 'lin', seasonal.factor = c( 63, 84, 91 )"
    ## [1] "BEST lin OBJECTIVE FUNCTION = 3.16555939906146"
    ## [1] "CURRNET METHOD: nonlin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'nonlin' , seasonal.factor =  c( 63, 84, 91 ) ...)"
    ## [1] "CURRENT nonlin OBJECTIVE FUNCTION = 4.21451725382292"
    ## [1] "BEST method = 'nonlin' PATH MEMBER = c( 63, 84, 91 )"
    ## [1] "BEST nonlin OBJECTIVE FUNCTION = 4.21451725382292"
    ## [1] "CURRNET METHOD: both"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'both' , seasonal.factor =  c( 63, 84, 91 ) ...)"
    ## [1] "CURRENT both OBJECTIVE FUNCTION = 3.60256673446334"
    ## [1] "BEST method = 'both' PATH MEMBER = c( 63, 84, 91 )"
    ## [1] "BEST both OBJECTIVE FUNCTION = 3.60256673446334"

![](man/figures/README-nns_forecasting-1.png)<!-- -->

    ## Package: healthyR.ai
    ## [1] "CURRNET METHOD: lin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 63 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 5.44469050318853"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 63, 84 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 5.34406890561643"
    ## [1] "BEST method = 'lin', seasonal.factor = c( 63, 84 )"
    ## [1] "BEST lin OBJECTIVE FUNCTION = 5.34406890561643"
    ## [1] "CURRNET METHOD: nonlin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'nonlin' , seasonal.factor =  c( 63, 84 ) ...)"
    ## [1] "CURRENT nonlin OBJECTIVE FUNCTION = 5.84210651203364"
    ## [1] "BEST method = 'nonlin' PATH MEMBER = c( 63, 84 )"
    ## [1] "BEST nonlin OBJECTIVE FUNCTION = 5.84210651203364"
    ## [1] "CURRNET METHOD: both"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'both' , seasonal.factor =  c( 63, 84 ) ...)"
    ## [1] "CURRENT both OBJECTIVE FUNCTION = 5.53865159048944"
    ## [1] "BEST method = 'both' PATH MEMBER = c( 63, 84 )"
    ## [1] "BEST both OBJECTIVE FUNCTION = 5.53865159048944"

![](man/figures/README-nns_forecasting-2.png)<!-- -->

    ## Package: healthyR.data
    ## [1] "CURRNET METHOD: lin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 63 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 2.29183308940514"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 63, 91 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 1.96022028667211"
    ## [1] "BEST method = 'lin', seasonal.factor = c( 63, 91 )"
    ## [1] "BEST lin OBJECTIVE FUNCTION = 1.96022028667211"
    ## [1] "CURRNET METHOD: nonlin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'nonlin' , seasonal.factor =  c( 63, 91 ) ...)"
    ## [1] "CURRENT nonlin OBJECTIVE FUNCTION = 2.04473571833831"
    ## [1] "BEST method = 'nonlin' PATH MEMBER = c( 63, 91 )"
    ## [1] "BEST nonlin OBJECTIVE FUNCTION = 2.04473571833831"
    ## [1] "CURRNET METHOD: both"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'both' , seasonal.factor =  c( 63, 91 ) ...)"
    ## [1] "CURRENT both OBJECTIVE FUNCTION = 1.84451368283702"
    ## [1] "BEST method = 'both' PATH MEMBER = c( 63, 91 )"
    ## [1] "BEST both OBJECTIVE FUNCTION = 1.84451368283702"

![](man/figures/README-nns_forecasting-3.png)<!-- -->

    ## Package: healthyR.ts
    ## [1] "CURRNET METHOD: lin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 63 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 5.40130186796845"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 63, 28 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 4.92130547270032"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 63, 28, 91 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 4.84214827494901"
    ## [1] "BEST method = 'lin', seasonal.factor = c( 63, 28, 91 )"
    ## [1] "BEST lin OBJECTIVE FUNCTION = 4.84214827494901"
    ## [1] "CURRNET METHOD: nonlin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'nonlin' , seasonal.factor =  c( 63, 28, 91 ) ...)"
    ## [1] "CURRENT nonlin OBJECTIVE FUNCTION = 5.47999711310792"
    ## [1] "BEST method = 'nonlin' PATH MEMBER = c( 63, 28, 91 )"
    ## [1] "BEST nonlin OBJECTIVE FUNCTION = 5.47999711310792"
    ## [1] "CURRNET METHOD: both"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'both' , seasonal.factor =  c( 63, 28, 91 ) ...)"
    ## [1] "CURRENT both OBJECTIVE FUNCTION = 4.96051301203465"
    ## [1] "BEST method = 'both' PATH MEMBER = c( 63, 28, 91 )"
    ## [1] "BEST both OBJECTIVE FUNCTION = 4.96051301203465"

![](man/figures/README-nns_forecasting-4.png)<!-- -->

    ## Package: healthyverse
    ## [1] "CURRNET METHOD: lin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 28 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 8.90120431932801"
    ## [1] "BEST method = 'lin', seasonal.factor = c( 28 )"
    ## [1] "BEST lin OBJECTIVE FUNCTION = 8.90120431932801"
    ## [1] "CURRNET METHOD: nonlin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'nonlin' , seasonal.factor =  c( 28 ) ...)"
    ## [1] "CURRENT nonlin OBJECTIVE FUNCTION = 5.10973736731316"
    ## [1] "BEST method = 'nonlin' PATH MEMBER = c( 28 )"
    ## [1] "BEST nonlin OBJECTIVE FUNCTION = 5.10973736731316"
    ## [1] "CURRNET METHOD: both"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'both' , seasonal.factor =  c( 28 ) ...)"
    ## [1] "CURRENT both OBJECTIVE FUNCTION = 5.66646766246067"
    ## [1] "BEST method = 'both' PATH MEMBER = c( 28 )"
    ## [1] "BEST both OBJECTIVE FUNCTION = 5.66646766246067"

![](man/figures/README-nns_forecasting-5.png)<!-- -->

    ## Package: RandomWalker
    ## [1] "CURRNET METHOD: lin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 35 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 2.87000997624193"
    ## [1] "BEST method = 'lin', seasonal.factor = c( 35 )"
    ## [1] "BEST lin OBJECTIVE FUNCTION = 2.87000997624193"
    ## [1] "CURRNET METHOD: nonlin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'nonlin' , seasonal.factor =  c( 35 ) ...)"
    ## [1] "CURRENT nonlin OBJECTIVE FUNCTION = 2.73684283848413"
    ## [1] "BEST method = 'nonlin' PATH MEMBER = c( 35 )"
    ## [1] "BEST nonlin OBJECTIVE FUNCTION = 2.73684283848413"
    ## [1] "CURRNET METHOD: both"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'both' , seasonal.factor =  c( 35 ) ...)"
    ## [1] "CURRENT both OBJECTIVE FUNCTION = 2.62567985730991"
    ## [1] "BEST method = 'both' PATH MEMBER = c( 35 )"
    ## [1] "BEST both OBJECTIVE FUNCTION = 2.62567985730991"

![](man/figures/README-nns_forecasting-6.png)<!-- -->

    ## Package: tidyAML
    ## [1] "CURRNET METHOD: lin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 84 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 5.75384839363019"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 84, 28 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 5.45822123658716"
    ## [1] "BEST method = 'lin', seasonal.factor = c( 84, 28 )"
    ## [1] "BEST lin OBJECTIVE FUNCTION = 5.45822123658716"
    ## [1] "CURRNET METHOD: nonlin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'nonlin' , seasonal.factor =  c( 84, 28 ) ...)"
    ## [1] "CURRENT nonlin OBJECTIVE FUNCTION = 3.61661095471111"
    ## [1] "BEST method = 'nonlin' PATH MEMBER = c( 84, 28 )"
    ## [1] "BEST nonlin OBJECTIVE FUNCTION = 3.61661095471111"
    ## [1] "CURRNET METHOD: both"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'both' , seasonal.factor =  c( 84, 28 ) ...)"
    ## [1] "CURRENT both OBJECTIVE FUNCTION = 4.30674617826057"
    ## [1] "BEST method = 'both' PATH MEMBER = c( 84, 28 )"
    ## [1] "BEST both OBJECTIVE FUNCTION = 4.30674617826057"

![](man/figures/README-nns_forecasting-7.png)<!-- -->

    ## Package: TidyDensity
    ## [1] "CURRNET METHOD: lin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 28 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 3.57651444382421"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 28, 84 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 3.10531650295114"
    ## [1] "BEST method = 'lin', seasonal.factor = c( 28, 84 )"
    ## [1] "BEST lin OBJECTIVE FUNCTION = 3.10531650295114"
    ## [1] "CURRNET METHOD: nonlin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'nonlin' , seasonal.factor =  c( 28, 84 ) ...)"
    ## [1] "CURRENT nonlin OBJECTIVE FUNCTION = 4.98032145029067"
    ## [1] "BEST method = 'nonlin' PATH MEMBER = c( 28, 84 )"
    ## [1] "BEST nonlin OBJECTIVE FUNCTION = 4.98032145029067"
    ## [1] "CURRNET METHOD: both"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'both' , seasonal.factor =  c( 28, 84 ) ...)"
    ## [1] "CURRENT both OBJECTIVE FUNCTION = 3.60546530785343"
    ## [1] "BEST method = 'both' PATH MEMBER = c( 28, 84 )"
    ## [1] "BEST both OBJECTIVE FUNCTION = 3.60546530785343"

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
    ## 1 healthyR.data <tibble [1,630 × 2]> <tibble [28 × 2]> <split [1602|28]>
    ## 2 healthyR      <tibble [1,623 × 2]> <tibble [28 × 2]> <split [1595|28]>
    ## 3 healthyR.ts   <tibble [1,567 × 2]> <tibble [28 × 2]> <split [1539|28]>
    ## 4 healthyverse  <tibble [1,537 × 2]> <tibble [28 × 2]> <split [1509|28]>
    ## 5 healthyR.ai   <tibble [1,362 × 2]> <tibble [28 × 2]> <split [1334|28]>
    ## 6 TidyDensity   <tibble [1,213 × 2]> <tibble [28 × 2]> <split [1185|28]>
    ## 7 tidyAML       <tibble [821 × 2]>   <tibble [28 × 2]> <split [793|28]> 
    ## 8 RandomWalker  <tibble [243 × 2]>   <tibble [28 × 2]> <split [215|28]>

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
| healthyR.data | 1 | ARIMA | Test | 0.6340314 | 134.59521 | 0.6496046 | 132.18465 | 0.8642638 | 0.0327796 |
| healthyR.data | 2 | LM | Test | 0.6282305 | 192.85971 | 0.6436612 | 113.27807 | 0.8388694 | 0.0052640 |
| healthyR.data | 3 | EARTH | Test | 0.6282305 | 194.64329 | 0.6436612 | 112.52781 | 0.8416182 | 0.0052640 |
| healthyR.data | 4 | NNAR | Test | 0.7313647 | 124.45561 | 0.7493286 | 174.51517 | 0.9213326 | 0.1301733 |
| healthyR | 1 | ARIMA | Test | 0.6052669 | 105.30191 | 0.7563962 | 173.85407 | 0.7930849 | 0.0466983 |
| healthyR | 2 | LM | Test | 0.5909299 | 93.40924 | 0.7384794 | 161.28807 | 0.7829248 | 0.0008111 |
| healthyR | 3 | EARTH | Test | 0.5505042 | 98.53652 | 0.6879597 | 123.98812 | 0.7424426 | 0.0008111 |
| healthyR | 4 | NNAR | Test | 0.5712207 | 98.67865 | 0.7138490 | 135.31506 | 0.7575105 | 0.0008034 |
| healthyR.ts | 1 | ARIMA | Test | 0.6799033 | 114.60683 | 0.6970360 | 173.86167 | 0.8406118 | 0.0001940 |
| healthyR.ts | 2 | LM | Test | 0.8954394 | 255.55668 | 0.9180033 | 162.63931 | 1.0502628 | 0.0001940 |
| healthyR.ts | 3 | EARTH | Test | 0.5686463 | 219.77094 | 0.5829755 | 110.61875 | 0.7563542 | 0.0001940 |
| healthyR.ts | 4 | NNAR | Test | 0.6744726 | 110.80561 | 0.6914684 | 173.47616 | 0.8381326 | 0.0009278 |
| healthyverse | 1 | ARIMA | Test | 0.5826901 | 183.50063 | 0.9960136 | 85.17429 | 0.7789816 | 0.0632806 |
| healthyverse | 2 | LM | Test | 0.5283602 | 281.92369 | 0.9031456 | 73.43329 | 0.6486655 | 0.0000210 |
| healthyverse | 3 | EARTH | Test | 3.7042204 | 1183.60922 | 6.3317602 | 199.39686 | 3.9857482 | 0.0000210 |
| healthyverse | 4 | NNAR | Test | 0.6244594 | 125.11591 | 1.0674114 | 98.72711 | 0.8234098 | 0.0021866 |
| healthyR.ai | 1 | ARIMA | Test | 0.6992486 | 115.25077 | 0.8816372 | 160.92322 | 0.9920124 | 0.0002647 |
| healthyR.ai | 2 | LM | Test | 0.6883218 | 121.66804 | 0.8678604 | 137.84595 | 1.0026918 | 0.1067655 |
| healthyR.ai | 3 | EARTH | Test | 0.6851122 | 143.53505 | 0.8638136 | 125.41931 | 1.0174284 | 0.1067655 |
| healthyR.ai | 4 | NNAR | Test | 0.6760024 | 126.76545 | 0.8523277 | 144.61159 | 0.9676285 | 0.0790201 |
| TidyDensity | 1 | ARIMA | Test | 0.4560537 | 357.91752 | 0.9262348 | 110.99788 | 0.5638525 | 0.0004778 |
| TidyDensity | 2 | LM | Test | 0.5615477 | 495.47630 | 1.1404907 | 116.13330 | 0.6821803 | 0.0489056 |
| TidyDensity | 3 | EARTH | Test | 0.4425530 | 333.27243 | 0.8988153 | 111.52290 | 0.5435445 | 0.0489056 |
| TidyDensity | 4 | NNAR | Test | 0.4011163 | 158.36549 | 0.8146583 | 121.87851 | 0.5218000 | 0.0451335 |
| tidyAML | 1 | ARIMA | Test | 0.7950718 | 132.08297 | 0.9570167 | 111.97641 | 1.0567936 | 0.0022778 |
| tidyAML | 2 | LM | Test | 0.8181392 | 191.11877 | 0.9847826 | 105.51393 | 1.0753155 | 0.1804690 |
| tidyAML | 3 | EARTH | Test | 0.8185114 | 122.94223 | 0.9852307 | 130.51162 | 1.0343842 | 0.1804690 |
| tidyAML | 4 | NNAR | Test | 0.8238769 | 126.81727 | 0.9916890 | 118.21868 | 1.0785684 | 0.0084463 |
| RandomWalker | 1 | ARIMA | Test | 1.1815068 | 105.75487 | 0.5564881 | 127.05009 | 1.4928734 | 0.0331650 |
| RandomWalker | 2 | LM | Test | 1.2524642 | 99.98539 | 0.5899090 | 198.60087 | 1.4463553 | 0.0310741 |
| RandomWalker | 3 | EARTH | Test | 1.2398126 | 95.35399 | 0.5839501 | 171.98312 | 1.4643204 | NA |
| RandomWalker | 4 | NNAR | Test | 1.3742255 | 131.95072 | 0.6472583 | 154.33110 | 1.5473748 | 0.0056518 |

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
    ## 1 healthyR.da…         2 LM          Test  0.628 193.  0.644 113.  0.839 5.26e-3
    ## 2 healthyR             3 EARTH       Test  0.551  98.5 0.688 124.  0.742 8.11e-4
    ## 3 healthyR.ts          3 EARTH       Test  0.569 220.  0.583 111.  0.756 1.94e-4
    ## 4 healthyverse         2 LM          Test  0.528 282.  0.903  73.4 0.649 2.10e-5
    ## 5 healthyR.ai          4 NNAR        Test  0.676 127.  0.852 145.  0.968 7.90e-2
    ## 6 TidyDensity          4 NNAR        Test  0.401 158.  0.815 122.  0.522 4.51e-2
    ## 7 tidyAML              3 EARTH       Test  0.819 123.  0.985 131.  1.03  1.80e-1
    ## 8 RandomWalker         2 LM          Test  1.25  100.  0.590 199.  1.45  3.11e-2

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
    ## 1 healthyR.data <tibble>     <tibble>     <split [1602|28]> <mdl_tm_t [1 × 5]>
    ## 2 healthyR      <tibble>     <tibble>     <split [1595|28]> <mdl_tm_t [1 × 5]>
    ## 3 healthyR.ts   <tibble>     <tibble>     <split [1539|28]> <mdl_tm_t [1 × 5]>
    ## 4 healthyverse  <tibble>     <tibble>     <split [1509|28]> <mdl_tm_t [1 × 5]>
    ## 5 healthyR.ai   <tibble>     <tibble>     <split [1334|28]> <mdl_tm_t [1 × 5]>
    ## 6 TidyDensity   <tibble>     <tibble>     <split [1185|28]> <mdl_tm_t [1 × 5]>
    ## 7 tidyAML       <tibble>     <tibble>     <split [793|28]>  <mdl_tm_t [1 × 5]>
    ## 8 RandomWalker  <tibble>     <tibble>     <split [215|28]>  <mdl_tm_t [1 × 5]>

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
