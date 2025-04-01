Time Series Analysis and Nested Modeling of the Healthyverse Packages
================
Steven P. Sanderson II, MPH - Date:
01 April, 2025

This analysis follows a *Nested Modeltime Workflow*.

## Get Data

``` r
glimpse(downloads_tbl)
```

    ## Rows: 135,596
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

The last day in the data set is 2025-03-30 19:35:10, the file was
birthed on: 2024-08-07 07:35:44, and at report knit time is -5647.99
hours old. Happy analyzing!

Now that we have our data lets take a look at it using the `skimr`
package.

``` r
skim(downloads_tbl)
```

|                                                  |               |
|:-------------------------------------------------|:--------------|
| Name                                             | downloads_tbl |
| Number of rows                                   | 135596        |
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
| r_version     |     97492 |          0.28 |   5 |   5 |     0 |       46 |          0 |
| r_arch        |     97492 |          0.28 |   3 |   7 |     0 |        5 |          0 |
| r_os          |     97492 |          0.28 |   7 |  15 |     0 |       21 |          0 |
| package       |         0 |          1.00 |   7 |  13 |     0 |        8 |          0 |
| version       |         0 |          1.00 |   5 |  17 |     0 |       60 |          0 |
| country       |     11391 |          0.92 |   2 |   2 |     0 |      163 |          0 |

**Variable type: Date**

| skim_variable | n_missing | complete_rate | min | max | median | n_unique |
|:---|---:|---:|:---|:---|:---|---:|
| date | 0 | 1 | 2020-11-23 | 2025-03-30 | 2023-05-24 | 1589 |

**Variable type: numeric**

| skim_variable | n_missing | complete_rate | mean | sd | p0 | p25 | p50 | p75 | p100 | hist |
|:---|---:|---:|---:|---:|---:|---:|---:|---:|---:|:---|
| size | 0 | 1 | 1134154.05 | 1523984.67 | 355 | 14701.00 | 260937.5 | 2367764 | 5677952 | ▇▁▂▁▁ |
| ip_id | 0 | 1 | 10353.96 | 18345.36 | 1 | 309.75 | 3077.0 | 11729 | 209747 | ▇▁▁▁▁ |

**Variable type: POSIXct**

| skim_variable | n_missing | complete_rate | min | max | median | n_unique |
|:---|---:|---:|:---|:---|:---|---:|
| date_time | 0 | 1 | 2020-11-23 09:00:41 | 2025-03-30 19:35:10 | 2023-05-24 21:00:12 | 82376 |

**Variable type: Timespan**

| skim_variable | n_missing | complete_rate | min | max | median | n_unique |
|:--------------|----------:|--------------:|----:|----:|-------:|---------:|
| time          |         0 |             1 |   0 |  59 |     35 |       60 |

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
    ## -150.07  -35.34  -10.40   26.75  812.76 
    ## 
    ## Coefficients:
    ##                                                      Estimate Std. Error
    ## (Intercept)                                        -1.931e+02  7.401e+01
    ## date                                                1.167e-02  3.926e-03
    ## lag(value, 1)                                       1.071e-01  2.487e-02
    ## lag(value, 7)                                       9.606e-02  2.584e-02
    ## lag(value, 14)                                      9.429e-02  2.584e-02
    ## lag(value, 21)                                      6.359e-02  2.593e-02
    ## lag(value, 28)                                      5.915e-02  2.582e-02
    ## lag(value, 35)                                      6.896e-02  2.588e-02
    ## lag(value, 42)                                      5.205e-02  2.610e-02
    ## lag(value, 49)                                      7.885e-02  2.596e-02
    ## month(date, label = TRUE).L                        -1.065e+01  5.156e+00
    ## month(date, label = TRUE).Q                         2.537e+00  5.207e+00
    ## month(date, label = TRUE).C                        -1.194e+01  5.253e+00
    ## month(date, label = TRUE)^4                        -7.052e+00  5.203e+00
    ## month(date, label = TRUE)^5                        -1.244e+01  5.219e+00
    ## month(date, label = TRUE)^6                        -2.914e+00  5.297e+00
    ## month(date, label = TRUE)^7                        -6.402e+00  5.169e+00
    ## month(date, label = TRUE)^8                        -4.768e+00  5.199e+00
    ## month(date, label = TRUE)^9                         5.799e+00  5.263e+00
    ## month(date, label = TRUE)^10                        4.322e+00  5.313e+00
    ## month(date, label = TRUE)^11                       -5.859e+00  5.344e+00
    ## fourier_vec(date, type = "sin", K = 1, period = 7) -1.186e+01  2.397e+00
    ## fourier_vec(date, type = "cos", K = 1, period = 7)  8.245e+00  2.523e+00
    ##                                                    t value Pr(>|t|)    
    ## (Intercept)                                         -2.609 0.009165 ** 
    ## date                                                 2.974 0.002989 ** 
    ## lag(value, 1)                                        4.308 1.75e-05 ***
    ## lag(value, 7)                                        3.717 0.000209 ***
    ## lag(value, 14)                                       3.649 0.000273 ***
    ## lag(value, 21)                                       2.452 0.014308 *  
    ## lag(value, 28)                                       2.291 0.022115 *  
    ## lag(value, 35)                                       2.664 0.007799 ** 
    ## lag(value, 42)                                       1.995 0.046268 *  
    ## lag(value, 49)                                       3.037 0.002430 ** 
    ## month(date, label = TRUE).L                         -2.066 0.038951 *  
    ## month(date, label = TRUE).Q                          0.487 0.626208    
    ## month(date, label = TRUE).C                         -2.273 0.023161 *  
    ## month(date, label = TRUE)^4                         -1.355 0.175482    
    ## month(date, label = TRUE)^5                         -2.384 0.017250 *  
    ## month(date, label = TRUE)^6                         -0.550 0.582366    
    ## month(date, label = TRUE)^7                         -1.239 0.215689    
    ## month(date, label = TRUE)^8                         -0.917 0.359166    
    ## month(date, label = TRUE)^9                          1.102 0.270699    
    ## month(date, label = TRUE)^10                         0.813 0.416110    
    ## month(date, label = TRUE)^11                        -1.096 0.273072    
    ## fourier_vec(date, type = "sin", K = 1, period = 7)  -4.948 8.32e-07 ***
    ## fourier_vec(date, type = "cos", K = 1, period = 7)   3.268 0.001109 ** 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 58.69 on 1517 degrees of freedom
    ##   (49 observations deleted due to missingness)
    ## Multiple R-squared:  0.2514, Adjusted R-squared:  0.2405 
    ## F-statistic: 23.16 on 22 and 1517 DF,  p-value: < 2.2e-16

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
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 98 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 2.47772918597048"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 98, 35 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 2.0689146523586"
    ## [1] "BEST method = 'lin', seasonal.factor = c( 98, 35 )"
    ## [1] "BEST lin OBJECTIVE FUNCTION = 2.0689146523586"
    ## [1] "CURRNET METHOD: nonlin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'nonlin' , seasonal.factor =  c( 98, 35 ) ...)"
    ## [1] "CURRENT nonlin OBJECTIVE FUNCTION = 2.67168560833192"
    ## [1] "BEST method = 'nonlin' PATH MEMBER = c( 98, 35 )"
    ## [1] "BEST nonlin OBJECTIVE FUNCTION = 2.67168560833192"
    ## [1] "CURRNET METHOD: both"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'both' , seasonal.factor =  c( 98, 35 ) ...)"
    ## [1] "CURRENT both OBJECTIVE FUNCTION = 2.26789668791785"
    ## [1] "BEST method = 'both' PATH MEMBER = c( 98, 35 )"
    ## [1] "BEST both OBJECTIVE FUNCTION = 2.26789668791785"

![](man/figures/README-nns_forecasting-1.png)<!-- -->

    ## Package: healthyR.ai
    ## [1] "CURRNET METHOD: lin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 49 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 1.84577470441707"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 49, 70 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 1.57488652452041"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 49, 70, 98 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 1.48974408738542"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 49, 70, 98, 77 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 1.45471265375594"
    ## [1] "BEST method = 'lin', seasonal.factor = c( 49, 70, 98, 77 )"
    ## [1] "BEST lin OBJECTIVE FUNCTION = 1.45471265375594"
    ## [1] "CURRNET METHOD: nonlin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'nonlin' , seasonal.factor =  c( 49, 70, 98, 77 ) ...)"
    ## [1] "CURRENT nonlin OBJECTIVE FUNCTION = 2.46550154362649"
    ## [1] "BEST method = 'nonlin' PATH MEMBER = c( 49, 70, 98, 77 )"
    ## [1] "BEST nonlin OBJECTIVE FUNCTION = 2.46550154362649"
    ## [1] "CURRNET METHOD: both"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'both' , seasonal.factor =  c( 49, 70, 98, 77 ) ...)"
    ## [1] "CURRENT both OBJECTIVE FUNCTION = 1.76667042122473"
    ## [1] "BEST method = 'both' PATH MEMBER = c( 49, 70, 98, 77 )"
    ## [1] "BEST both OBJECTIVE FUNCTION = 1.76667042122473"

![](man/figures/README-nns_forecasting-2.png)<!-- -->

    ## Package: healthyR.data
    ## [1] "CURRNET METHOD: lin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 98 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 1.4654971449864"
    ## [1] "BEST method = 'lin', seasonal.factor = c( 98 )"
    ## [1] "BEST lin OBJECTIVE FUNCTION = 1.4654971449864"
    ## [1] "CURRNET METHOD: nonlin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'nonlin' , seasonal.factor =  c( 98 ) ...)"
    ## [1] "CURRENT nonlin OBJECTIVE FUNCTION = 1.82638367521215"
    ## [1] "BEST method = 'nonlin' PATH MEMBER = c( 98 )"
    ## [1] "BEST nonlin OBJECTIVE FUNCTION = 1.82638367521215"
    ## [1] "CURRNET METHOD: both"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'both' , seasonal.factor =  c( 98 ) ...)"
    ## [1] "CURRENT both OBJECTIVE FUNCTION = 1.47466080280591"
    ## [1] "BEST method = 'both' PATH MEMBER = c( 98 )"
    ## [1] "BEST both OBJECTIVE FUNCTION = 1.47466080280591"

![](man/figures/README-nns_forecasting-3.png)<!-- -->

    ## Package: healthyR.ts
    ## [1] "CURRNET METHOD: lin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 98 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 1.86994556907445"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 98, 63 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 1.43556239119177"
    ## [1] "BEST method = 'lin', seasonal.factor = c( 98, 63 )"
    ## [1] "BEST lin OBJECTIVE FUNCTION = 1.43556239119177"
    ## [1] "CURRNET METHOD: nonlin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'nonlin' , seasonal.factor =  c( 98, 63 ) ...)"
    ## [1] "CURRENT nonlin OBJECTIVE FUNCTION = 1.9938913302377"
    ## [1] "BEST method = 'nonlin' PATH MEMBER = c( 98, 63 )"
    ## [1] "BEST nonlin OBJECTIVE FUNCTION = 1.9938913302377"
    ## [1] "CURRNET METHOD: both"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'both' , seasonal.factor =  c( 98, 63 ) ...)"
    ## [1] "CURRENT both OBJECTIVE FUNCTION = 1.60027707958542"
    ## [1] "BEST method = 'both' PATH MEMBER = c( 98, 63 )"
    ## [1] "BEST both OBJECTIVE FUNCTION = 1.60027707958542"

![](man/figures/README-nns_forecasting-4.png)<!-- -->

    ## Package: healthyverse
    ## [1] "CURRNET METHOD: lin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 49 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 2.14778763380631"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 49, 77 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 1.865684471327"
    ## [1] "BEST method = 'lin', seasonal.factor = c( 49, 77 )"
    ## [1] "BEST lin OBJECTIVE FUNCTION = 1.865684471327"
    ## [1] "CURRNET METHOD: nonlin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'nonlin' , seasonal.factor =  c( 49, 77 ) ...)"
    ## [1] "CURRENT nonlin OBJECTIVE FUNCTION = 3.70064725999573"
    ## [1] "BEST method = 'nonlin' PATH MEMBER = c( 49, 77 )"
    ## [1] "BEST nonlin OBJECTIVE FUNCTION = 3.70064725999573"
    ## [1] "CURRNET METHOD: both"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'both' , seasonal.factor =  c( 49, 77 ) ...)"
    ## [1] "CURRENT both OBJECTIVE FUNCTION = 2.54803082642209"
    ## [1] "BEST method = 'both' PATH MEMBER = c( 49, 77 )"
    ## [1] "BEST both OBJECTIVE FUNCTION = 2.54803082642209"

![](man/figures/README-nns_forecasting-5.png)<!-- -->

    ## Package: RandomWalker
    ## [1] "CURRNET METHOD: lin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 21 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 5.90987017896246"
    ## [1] "BEST method = 'lin', seasonal.factor = c( 21 )"
    ## [1] "BEST lin OBJECTIVE FUNCTION = 5.90987017896246"
    ## [1] "CURRNET METHOD: nonlin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'nonlin' , seasonal.factor =  c( 21 ) ...)"
    ## [1] "CURRENT nonlin OBJECTIVE FUNCTION = 4.10496205741666"
    ## [1] "BEST method = 'nonlin' PATH MEMBER = c( 21 )"
    ## [1] "BEST nonlin OBJECTIVE FUNCTION = 4.10496205741666"
    ## [1] "CURRNET METHOD: both"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'both' , seasonal.factor =  c( 21 ) ...)"
    ## [1] "CURRENT both OBJECTIVE FUNCTION = 3.40902711321649"
    ## [1] "BEST method = 'both' PATH MEMBER = c( 21 )"
    ## [1] "BEST both OBJECTIVE FUNCTION = 3.40902711321649"

![](man/figures/README-nns_forecasting-6.png)<!-- -->

    ## Package: tidyAML
    ## [1] "CURRNET METHOD: lin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 28 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 4.95928030140643"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 28, 56 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 4.76431307309333"
    ## [1] "BEST method = 'lin', seasonal.factor = c( 28, 56 )"
    ## [1] "BEST lin OBJECTIVE FUNCTION = 4.76431307309333"
    ## [1] "CURRNET METHOD: nonlin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'nonlin' , seasonal.factor =  c( 28, 56 ) ...)"
    ## [1] "CURRENT nonlin OBJECTIVE FUNCTION = 4.33842474925428"
    ## [1] "BEST method = 'nonlin' PATH MEMBER = c( 28, 56 )"
    ## [1] "BEST nonlin OBJECTIVE FUNCTION = 4.33842474925428"
    ## [1] "CURRNET METHOD: both"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'both' , seasonal.factor =  c( 28, 56 ) ...)"
    ## [1] "CURRENT both OBJECTIVE FUNCTION = 4.12860729750076"
    ## [1] "BEST method = 'both' PATH MEMBER = c( 28, 56 )"
    ## [1] "BEST both OBJECTIVE FUNCTION = 4.12860729750076"

![](man/figures/README-nns_forecasting-7.png)<!-- -->

    ## Package: TidyDensity
    ## [1] "CURRNET METHOD: lin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 77 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 1.83190576998759"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 77, 63 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 1.60901389883974"
    ## [1] "BEST method = 'lin', seasonal.factor = c( 77, 63 )"
    ## [1] "BEST lin OBJECTIVE FUNCTION = 1.60901389883974"
    ## [1] "CURRNET METHOD: nonlin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'nonlin' , seasonal.factor =  c( 77, 63 ) ...)"
    ## [1] "CURRENT nonlin OBJECTIVE FUNCTION = 2.48651011240348"
    ## [1] "BEST method = 'nonlin' PATH MEMBER = c( 77, 63 )"
    ## [1] "BEST nonlin OBJECTIVE FUNCTION = 2.48651011240348"
    ## [1] "CURRNET METHOD: both"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'both' , seasonal.factor =  c( 77, 63 ) ...)"
    ## [1] "CURRENT both OBJECTIVE FUNCTION = 1.84621647826196"
    ## [1] "BEST method = 'both' PATH MEMBER = c( 77, 63 )"
    ## [1] "BEST both OBJECTIVE FUNCTION = 1.84621647826196"

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
    ## 1 healthyR.data <tibble [1,582 × 2]> <tibble [28 × 2]> <split [1554|28]>
    ## 2 healthyR      <tibble [1,575 × 2]> <tibble [28 × 2]> <split [1547|28]>
    ## 3 healthyR.ts   <tibble [1,519 × 2]> <tibble [28 × 2]> <split [1491|28]>
    ## 4 healthyverse  <tibble [1,490 × 2]> <tibble [28 × 2]> <split [1462|28]>
    ## 5 healthyR.ai   <tibble [1,314 × 2]> <tibble [28 × 2]> <split [1286|28]>
    ## 6 TidyDensity   <tibble [1,165 × 2]> <tibble [28 × 2]> <split [1137|28]>
    ## 7 tidyAML       <tibble [773 × 2]>   <tibble [28 × 2]> <split [745|28]> 
    ## 8 RandomWalker  <tibble [195 × 2]>   <tibble [28 × 2]> <split [167|28]>

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
| healthyR.data | 1 | ARIMA | Test | 0.7291732 | 177.71817 | 0.7421259 | 158.68400 | 0.9358722 | 0.0016698 |
| healthyR.data | 2 | LM | Test | 0.7169325 | 207.63264 | 0.7296678 | 132.50718 | 0.8866835 | 0.0287929 |
| healthyR.data | 3 | EARTH | Test | 0.7057543 | 175.58788 | 0.7182911 | 136.57946 | 0.8800343 | 0.0287929 |
| healthyR.data | 4 | NNAR | Test | 0.7410815 | 112.22385 | 0.7542458 | 176.64044 | 0.9842829 | 0.0132774 |
| healthyR | 1 | ARIMA | Test | 0.7854918 | 114.02159 | 0.8391758 | 179.07348 | 0.9294431 | 0.0000010 |
| healthyR | 2 | LM | Test | 0.7406285 | 98.41016 | 0.7912463 | 185.98737 | 0.9022747 | 0.0625362 |
| healthyR | 3 | EARTH | Test | 0.7390660 | 98.46254 | 0.7895770 | 187.25212 | 0.9001561 | 0.0625362 |
| healthyR | 4 | NNAR | Test | 0.7522459 | 107.36678 | 0.8036577 | 153.11736 | 0.9193824 | 0.0005394 |
| healthyR.ts | 1 | ARIMA | Test | 1.0748304 | 334.14581 | 0.8730513 | 146.88585 | 1.2638181 | 0.0435876 |
| healthyR.ts | 2 | LM | Test | 1.0508311 | 306.71424 | 0.8535574 | 147.54202 | 1.2427488 | 0.0435876 |
| healthyR.ts | 3 | EARTH | Test | 1.0557201 | 312.69392 | 0.8575286 | 147.32419 | 1.2472597 | 0.0435876 |
| healthyR.ts | 4 | NNAR | Test | 1.0020564 | 126.58775 | 0.8139392 | 178.68669 | 1.2378132 | 0.0156681 |
| healthyverse | 1 | ARIMA | Test | 0.6401655 | 206.22402 | 0.8930623 | 107.72862 | 0.7839800 | 0.1397411 |
| healthyverse | 2 | LM | Test | 0.6215817 | 274.39530 | 0.8671369 | 96.66032 | 0.7441731 | 0.0005868 |
| healthyverse | 3 | EARTH | Test | 0.6274360 | 169.93716 | 0.8753040 | 112.33299 | 0.7861154 | 0.0005868 |
| healthyverse | 4 | NNAR | Test | 0.6605276 | 157.93566 | 0.9214684 | 122.10712 | 0.8260021 | 0.0936319 |
| healthyR.ai | 1 | ARIMA | Test | 0.7631866 | 115.24001 | 0.7403142 | 169.91543 | 0.9465812 | 0.0159111 |
| healthyR.ai | 2 | LM | Test | 0.7170964 | 121.37611 | 0.6956054 | 139.46117 | 0.9292334 | 0.0186132 |
| healthyR.ai | 3 | EARTH | Test | 0.8405016 | 171.21902 | 0.8153121 | 180.92088 | 1.0085704 | 0.0186132 |
| healthyR.ai | 4 | NNAR | Test | 0.7456841 | 164.46929 | 0.7233363 | 142.80714 | 0.9274940 | 0.0193818 |
| TidyDensity | 1 | ARIMA | Test | 0.6706048 | 229.97473 | 0.7088212 | 117.43808 | 0.8207948 | 0.0152363 |
| TidyDensity | 2 | LM | Test | 0.7278453 | 285.88983 | 0.7693236 | 115.53069 | 0.8924333 | 0.0173509 |
| TidyDensity | 3 | EARTH | Test | 0.6766205 | 224.53689 | 0.7151797 | 118.39991 | 0.8279365 | 0.0173509 |
| TidyDensity | 4 | NNAR | Test | 0.6457620 | 146.15862 | 0.6825626 | 137.19048 | 0.7946459 | 0.0108139 |
| tidyAML | 1 | ARIMA | Test | 0.7283239 | 358.28170 | 0.7688989 | 109.16121 | 0.8795623 | 0.0539736 |
| tidyAML | 2 | LM | Test | 0.7100293 | 349.22351 | 0.7495851 | 107.77980 | 0.8359596 | 0.0002596 |
| tidyAML | 3 | EARTH | Test | 0.7294208 | 390.96470 | 0.7700568 | 106.66986 | 0.8514254 | 0.0002596 |
| tidyAML | 4 | NNAR | Test | 0.7013216 | 323.07120 | 0.7403922 | 107.91466 | 0.8409826 | 0.0152758 |
| RandomWalker | 1 | ARIMA | Test | 1.2098139 | 131.87914 | 0.7292841 | 107.03710 | 1.5986905 | 0.0126037 |
| RandomWalker | 2 | LM | Test | 1.2252588 | 110.80373 | 0.7385944 | 194.07072 | 1.3328061 | 0.0041633 |
| RandomWalker | 3 | EARTH | Test | 1.1335250 | 90.12725 | 0.6832966 | 165.65461 | 1.2801089 | NA |
| RandomWalker | 4 | NNAR | Test | 1.1225825 | 90.11539 | 0.6767004 | 165.07801 | 1.2635218 | 0.0681893 |

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
    ## 1 healthyR.da…         3 EARTH       Test  0.706 176.  0.718 137.  0.880 2.88e-2
    ## 2 healthyR             3 EARTH       Test  0.739  98.5 0.790 187.  0.900 6.25e-2
    ## 3 healthyR.ts          4 NNAR        Test  1.00  127.  0.814 179.  1.24  1.57e-2
    ## 4 healthyverse         2 LM          Test  0.622 274.  0.867  96.7 0.744 5.87e-4
    ## 5 healthyR.ai          4 NNAR        Test  0.746 164.  0.723 143.  0.927 1.94e-2
    ## 6 TidyDensity          4 NNAR        Test  0.646 146.  0.683 137.  0.795 1.08e-2
    ## 7 tidyAML              2 LM          Test  0.710 349.  0.750 108.  0.836 2.60e-4
    ## 8 RandomWalker         4 NNAR        Test  1.12   90.1 0.677 165.  1.26  6.82e-2

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
    ## 1 healthyR.data <tibble>     <tibble>     <split [1554|28]> <mdl_tm_t [1 × 5]>
    ## 2 healthyR      <tibble>     <tibble>     <split [1547|28]> <mdl_tm_t [1 × 5]>
    ## 3 healthyR.ts   <tibble>     <tibble>     <split [1491|28]> <mdl_tm_t [1 × 5]>
    ## 4 healthyverse  <tibble>     <tibble>     <split [1462|28]> <mdl_tm_t [1 × 5]>
    ## 5 healthyR.ai   <tibble>     <tibble>     <split [1286|28]> <mdl_tm_t [1 × 5]>
    ## 6 TidyDensity   <tibble>     <tibble>     <split [1137|28]> <mdl_tm_t [1 × 5]>
    ## 7 tidyAML       <tibble>     <tibble>     <split [745|28]>  <mdl_tm_t [1 × 5]>
    ## 8 RandomWalker  <tibble>     <tibble>     <split [167|28]>  <mdl_tm_t [1 × 5]>

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
