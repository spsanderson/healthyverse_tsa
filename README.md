Time Series Analysis and Nested Modeling of the Healthyverse Packages
================
Steven P. Sanderson II, MPH - Date:
09 June, 2025

This analysis follows a *Nested Modeltime Workflow*.

## Get Data

``` r
glimpse(downloads_tbl)
```

    ## Rows: 141,817
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

The last day in the data set is 2025-06-07 23:26:08, the file was
birthed on: 2024-08-07 07:35:44, and at report knit time is -7307.84
hours old. Happy analyzing!

Now that we have our data lets take a look at it using the `skimr`
package.

``` r
skim(downloads_tbl)
```

|                                                  |               |
|:-------------------------------------------------|:--------------|
| Name                                             | downloads_tbl |
| Number of rows                                   | 141817        |
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
| r_version     |    102284 |          0.28 |   5 |   5 |     0 |       47 |          0 |
| r_arch        |    102284 |          0.28 |   3 |   7 |     0 |        5 |          0 |
| r_os          |    102284 |          0.28 |   7 |  15 |     0 |       22 |          0 |
| package       |         0 |          1.00 |   7 |  13 |     0 |        8 |          0 |
| version       |         0 |          1.00 |   5 |  17 |     0 |       60 |          0 |
| country       |     12052 |          0.92 |   2 |   2 |     0 |      163 |          0 |

**Variable type: Date**

| skim_variable | n_missing | complete_rate | min | max | median | n_unique |
|:---|---:|---:|:---|:---|:---|---:|
| date | 0 | 1 | 2020-11-23 | 2025-06-07 | 2023-06-30 | 1658 |

**Variable type: numeric**

| skim_variable | n_missing | complete_rate | mean | sd | p0 | p25 | p50 | p75 | p100 | hist |
|:---|---:|---:|---:|---:|---:|---:|---:|---:|---:|:---|
| size | 0 | 1 | 1133393.91 | 1517075.59 | 355 | 14701 | 289703 | 2367728 | 5677952 | ▇▁▂▁▁ |
| ip_id | 0 | 1 | 10432.09 | 18579.77 | 1 | 284 | 3039 | 11684 | 209747 | ▇▁▁▁▁ |

**Variable type: POSIXct**

| skim_variable | n_missing | complete_rate | min | max | median | n_unique |
|:---|---:|---:|:---|:---|:---|---:|
| date_time | 0 | 1 | 2020-11-23 09:00:41 | 2025-06-07 23:26:08 | 2023-06-30 19:38:29 | 86870 |

**Variable type: Timespan**

| skim_variable | n_missing | complete_rate | min | max |     median | n_unique |
|:--------------|----------:|--------------:|----:|----:|-----------:|---------:|
| time          |         0 |             1 |   0 |  59 | 12H 6M 47S |       60 |

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
    ## -146.72  -35.81  -11.05   26.84  814.94 
    ## 
    ## Coefficients:
    ##                                                      Estimate Std. Error
    ## (Intercept)                                        -1.538e+02  6.801e+01
    ## date                                                9.637e-03  3.603e-03
    ## lag(value, 1)                                       1.050e-01  2.433e-02
    ## lag(value, 7)                                       9.664e-02  2.521e-02
    ## lag(value, 14)                                      8.917e-02  2.520e-02
    ## lag(value, 21)                                      6.880e-02  2.527e-02
    ## lag(value, 28)                                      7.085e-02  2.517e-02
    ## lag(value, 35)                                      6.726e-02  2.526e-02
    ## lag(value, 42)                                      5.100e-02  2.540e-02
    ## lag(value, 49)                                      6.913e-02  2.526e-02
    ## month(date, label = TRUE).L                        -9.569e+00  5.106e+00
    ## month(date, label = TRUE).Q                         4.197e+00  5.114e+00
    ## month(date, label = TRUE).C                        -1.353e+01  5.128e+00
    ## month(date, label = TRUE)^4                        -7.450e+00  5.163e+00
    ## month(date, label = TRUE)^5                        -1.091e+01  5.113e+00
    ## month(date, label = TRUE)^6                        -3.169e+00  5.208e+00
    ## month(date, label = TRUE)^7                        -7.680e+00  5.090e+00
    ## month(date, label = TRUE)^8                        -3.635e+00  5.094e+00
    ## month(date, label = TRUE)^9                         6.147e+00  5.103e+00
    ## month(date, label = TRUE)^10                        3.124e+00  5.077e+00
    ## month(date, label = TRUE)^11                       -5.042e+00  5.189e+00
    ## fourier_vec(date, type = "sin", K = 1, period = 7) -1.148e+01  2.329e+00
    ## fourier_vec(date, type = "cos", K = 1, period = 7)  7.972e+00  2.449e+00
    ##                                                    t value Pr(>|t|)    
    ## (Intercept)                                         -2.262 0.023856 *  
    ## date                                                 2.675 0.007555 ** 
    ## lag(value, 1)                                        4.314 1.70e-05 ***
    ## lag(value, 7)                                        3.834 0.000131 ***
    ## lag(value, 14)                                       3.539 0.000413 ***
    ## lag(value, 21)                                       2.722 0.006551 ** 
    ## lag(value, 28)                                       2.815 0.004937 ** 
    ## lag(value, 35)                                       2.663 0.007826 ** 
    ## lag(value, 42)                                       2.008 0.044792 *  
    ## lag(value, 49)                                       2.736 0.006290 ** 
    ## month(date, label = TRUE).L                         -1.874 0.061091 .  
    ## month(date, label = TRUE).Q                          0.821 0.411978    
    ## month(date, label = TRUE).C                         -2.638 0.008420 ** 
    ## month(date, label = TRUE)^4                         -1.443 0.149221    
    ## month(date, label = TRUE)^5                         -2.134 0.032977 *  
    ## month(date, label = TRUE)^6                         -0.608 0.542959    
    ## month(date, label = TRUE)^7                         -1.509 0.131561    
    ## month(date, label = TRUE)^8                         -0.714 0.475617    
    ## month(date, label = TRUE)^9                          1.204 0.228581    
    ## month(date, label = TRUE)^10                         0.615 0.538348    
    ## month(date, label = TRUE)^11                        -0.972 0.331391    
    ## fourier_vec(date, type = "sin", K = 1, period = 7)  -4.928 9.17e-07 ***
    ## fourier_vec(date, type = "cos", K = 1, period = 7)   3.256 0.001154 ** 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 58.68 on 1586 degrees of freedom
    ##   (49 observations deleted due to missingness)
    ## Multiple R-squared:  0.2394, Adjusted R-squared:  0.2288 
    ## F-statistic: 22.69 on 22 and 1586 DF,  p-value: < 2.2e-16

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
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 2.7430823524398"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 21, 63 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 2.46052619688832"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 21, 63, 91 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 2.36262847890346"
    ## [1] "BEST method = 'lin', seasonal.factor = c( 21, 63, 91 )"
    ## [1] "BEST lin OBJECTIVE FUNCTION = 2.36262847890346"
    ## [1] "CURRNET METHOD: nonlin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'nonlin' , seasonal.factor =  c( 21, 63, 91 ) ...)"
    ## [1] "CURRENT nonlin OBJECTIVE FUNCTION = 4.44580036577609"
    ## [1] "BEST method = 'nonlin' PATH MEMBER = c( 21, 63, 91 )"
    ## [1] "BEST nonlin OBJECTIVE FUNCTION = 4.44580036577609"
    ## [1] "CURRNET METHOD: both"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'both' , seasonal.factor =  c( 21, 63, 91 ) ...)"
    ## [1] "CURRENT both OBJECTIVE FUNCTION = 3.00473680136983"
    ## [1] "BEST method = 'both' PATH MEMBER = c( 21, 63, 91 )"
    ## [1] "BEST both OBJECTIVE FUNCTION = 3.00473680136983"

![](man/figures/README-nns_forecasting-1.png)<!-- -->

    ## Package: healthyR.ai
    ## [1] "CURRNET METHOD: lin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 63 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 3.68227576034834"
    ## [1] "BEST method = 'lin', seasonal.factor = c( 63 )"
    ## [1] "BEST lin OBJECTIVE FUNCTION = 3.68227576034834"
    ## [1] "CURRNET METHOD: nonlin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'nonlin' , seasonal.factor =  c( 63 ) ...)"
    ## [1] "CURRENT nonlin OBJECTIVE FUNCTION = 4.05378278722203"
    ## [1] "BEST method = 'nonlin' PATH MEMBER = c( 63 )"
    ## [1] "BEST nonlin OBJECTIVE FUNCTION = 4.05378278722203"
    ## [1] "CURRNET METHOD: both"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'both' , seasonal.factor =  c( 63 ) ...)"
    ## [1] "CURRENT both OBJECTIVE FUNCTION = 3.74243003055983"
    ## [1] "BEST method = 'both' PATH MEMBER = c( 63 )"
    ## [1] "BEST both OBJECTIVE FUNCTION = 3.74243003055983"

![](man/figures/README-nns_forecasting-2.png)<!-- -->

    ## Package: healthyR.data
    ## [1] "CURRNET METHOD: lin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 63 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 2.55411104733254"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 63, 84 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 2.32252538886903"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 63, 84, 91 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 2.22588949750934"
    ## [1] "BEST method = 'lin', seasonal.factor = c( 63, 84, 91 )"
    ## [1] "BEST lin OBJECTIVE FUNCTION = 2.22588949750934"
    ## [1] "CURRNET METHOD: nonlin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'nonlin' , seasonal.factor =  c( 63, 84, 91 ) ...)"
    ## [1] "CURRENT nonlin OBJECTIVE FUNCTION = 2.31469558509605"
    ## [1] "BEST method = 'nonlin' PATH MEMBER = c( 63, 84, 91 )"
    ## [1] "BEST nonlin OBJECTIVE FUNCTION = 2.31469558509605"
    ## [1] "CURRNET METHOD: both"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'both' , seasonal.factor =  c( 63, 84, 91 ) ...)"
    ## [1] "CURRENT both OBJECTIVE FUNCTION = 2.12231407694237"
    ## [1] "BEST method = 'both' PATH MEMBER = c( 63, 84, 91 )"
    ## [1] "BEST both OBJECTIVE FUNCTION = 2.12231407694237"

![](man/figures/README-nns_forecasting-3.png)<!-- -->

    ## Package: healthyR.ts
    ## [1] "CURRNET METHOD: lin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 21 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 3.16511889505517"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 21, 56 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 2.8181038950464"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 21, 56, 63 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 2.69743722439823"
    ## [1] "BEST method = 'lin', seasonal.factor = c( 21, 56, 63 )"
    ## [1] "BEST lin OBJECTIVE FUNCTION = 2.69743722439823"
    ## [1] "CURRNET METHOD: nonlin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'nonlin' , seasonal.factor =  c( 21, 56, 63 ) ...)"
    ## [1] "CURRENT nonlin OBJECTIVE FUNCTION = 5.17111413785284"
    ## [1] "BEST method = 'nonlin' PATH MEMBER = c( 21, 56, 63 )"
    ## [1] "BEST nonlin OBJECTIVE FUNCTION = 5.17111413785284"
    ## [1] "CURRNET METHOD: both"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'both' , seasonal.factor =  c( 21, 56, 63 ) ...)"
    ## [1] "CURRENT both OBJECTIVE FUNCTION = 3.16246033198328"
    ## [1] "BEST method = 'both' PATH MEMBER = c( 21, 56, 63 )"
    ## [1] "BEST both OBJECTIVE FUNCTION = 3.16246033198328"

![](man/figures/README-nns_forecasting-4.png)<!-- -->

    ## Package: healthyverse
    ## [1] "CURRNET METHOD: lin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 28 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 4.19172370298605"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 28, 56 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 4.01373480488995"
    ## [1] "BEST method = 'lin', seasonal.factor = c( 28, 56 )"
    ## [1] "BEST lin OBJECTIVE FUNCTION = 4.01373480488995"
    ## [1] "CURRNET METHOD: nonlin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'nonlin' , seasonal.factor =  c( 28, 56 ) ...)"
    ## [1] "CURRENT nonlin OBJECTIVE FUNCTION = 4.83478312995496"
    ## [1] "BEST method = 'nonlin' PATH MEMBER = c( 28, 56 )"
    ## [1] "BEST nonlin OBJECTIVE FUNCTION = 4.83478312995496"
    ## [1] "CURRNET METHOD: both"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'both' , seasonal.factor =  c( 28, 56 ) ...)"
    ## [1] "CURRENT both OBJECTIVE FUNCTION = 4.0085728844063"
    ## [1] "BEST method = 'both' PATH MEMBER = c( 28, 56 )"
    ## [1] "BEST both OBJECTIVE FUNCTION = 4.0085728844063"

![](man/figures/README-nns_forecasting-5.png)<!-- -->

    ## Package: RandomWalker
    ## [1] "CURRNET METHOD: lin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 63 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 3.2257661577086"
    ## [1] "BEST method = 'lin', seasonal.factor = c( 63 )"
    ## [1] "BEST lin OBJECTIVE FUNCTION = 3.2257661577086"
    ## [1] "CURRNET METHOD: nonlin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'nonlin' , seasonal.factor =  c( 63 ) ...)"
    ## [1] "CURRENT nonlin OBJECTIVE FUNCTION = 3.25568514794957"
    ## [1] "BEST method = 'nonlin' PATH MEMBER = c( 63 )"
    ## [1] "BEST nonlin OBJECTIVE FUNCTION = 3.25568514794957"
    ## [1] "CURRNET METHOD: both"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'both' , seasonal.factor =  c( 63 ) ...)"
    ## [1] "CURRENT both OBJECTIVE FUNCTION = 3.06234849163741"
    ## [1] "BEST method = 'both' PATH MEMBER = c( 63 )"
    ## [1] "BEST both OBJECTIVE FUNCTION = 3.06234849163741"

![](man/figures/README-nns_forecasting-6.png)<!-- -->

    ## Package: tidyAML
    ## [1] "CURRNET METHOD: lin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 84 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 6.43697401810396"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 84, 91 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 6.34962386146168"
    ## [1] "BEST method = 'lin', seasonal.factor = c( 84, 91 )"
    ## [1] "BEST lin OBJECTIVE FUNCTION = 6.34962386146168"
    ## [1] "CURRNET METHOD: nonlin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'nonlin' , seasonal.factor =  c( 84, 91 ) ...)"
    ## [1] "CURRENT nonlin OBJECTIVE FUNCTION = 4.25366230586835"
    ## [1] "BEST method = 'nonlin' PATH MEMBER = c( 84, 91 )"
    ## [1] "BEST nonlin OBJECTIVE FUNCTION = 4.25366230586835"
    ## [1] "CURRNET METHOD: both"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'both' , seasonal.factor =  c( 84, 91 ) ...)"
    ## [1] "CURRENT both OBJECTIVE FUNCTION = 4.99287515264327"
    ## [1] "BEST method = 'both' PATH MEMBER = c( 84, 91 )"
    ## [1] "BEST both OBJECTIVE FUNCTION = 4.99287515264327"

![](man/figures/README-nns_forecasting-7.png)<!-- -->

    ## Package: TidyDensity
    ## [1] "CURRNET METHOD: lin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 63 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 3.74524832904839"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 63, 56 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 3.47356558778286"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 63, 56, 35 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 3.38286731954275"
    ## [1] "BEST method = 'lin', seasonal.factor = c( 63, 56, 35 )"
    ## [1] "BEST lin OBJECTIVE FUNCTION = 3.38286731954275"
    ## [1] "CURRNET METHOD: nonlin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'nonlin' , seasonal.factor =  c( 63, 56, 35 ) ...)"
    ## [1] "CURRENT nonlin OBJECTIVE FUNCTION = 4.78422409893306"
    ## [1] "BEST method = 'nonlin' PATH MEMBER = c( 63, 56, 35 )"
    ## [1] "BEST nonlin OBJECTIVE FUNCTION = 4.78422409893306"
    ## [1] "CURRNET METHOD: both"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'both' , seasonal.factor =  c( 63, 56, 35 ) ...)"
    ## [1] "CURRENT both OBJECTIVE FUNCTION = 3.66678773858143"
    ## [1] "BEST method = 'both' PATH MEMBER = c( 63, 56, 35 )"
    ## [1] "BEST both OBJECTIVE FUNCTION = 3.66678773858143"

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
    ## 1 healthyR.data <tibble [1,650 × 2]> <tibble [28 × 2]> <split [1622|28]>
    ## 2 healthyR      <tibble [1,644 × 2]> <tibble [28 × 2]> <split [1616|28]>
    ## 3 healthyR.ts   <tibble [1,588 × 2]> <tibble [28 × 2]> <split [1560|28]>
    ## 4 healthyverse  <tibble [1,558 × 2]> <tibble [28 × 2]> <split [1530|28]>
    ## 5 healthyR.ai   <tibble [1,383 × 2]> <tibble [28 × 2]> <split [1355|28]>
    ## 6 TidyDensity   <tibble [1,234 × 2]> <tibble [28 × 2]> <split [1206|28]>
    ## 7 tidyAML       <tibble [842 × 2]>   <tibble [28 × 2]> <split [814|28]> 
    ## 8 RandomWalker  <tibble [264 × 2]>   <tibble [28 × 2]> <split [236|28]>

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
| healthyR.data | 1 | ARIMA | Test | 0.5855370 | 102.30733 | 0.6266358 | 128.70690 | 0.8020992 | 0.2055497 |
| healthyR.data | 2 | LM | Test | 0.6377634 | 206.75981 | 0.6825279 | 123.39294 | 0.7939722 | 0.0044239 |
| healthyR.data | 3 | EARTH | Test | 0.6576065 | 138.72253 | 0.7037639 | 146.84330 | 0.8625510 | 0.0044239 |
| healthyR.data | 4 | NNAR | Test | 0.7156268 | 95.18227 | 0.7658566 | 168.71399 | 0.9708544 | 0.0169405 |
| healthyR | 1 | ARIMA | Test | 0.7148223 | 123.00235 | 0.8703759 | 164.91372 | 0.8893949 | 0.0557124 |
| healthyR | 2 | LM | Test | 0.6793562 | 97.70083 | 0.8271919 | 167.53139 | 0.8648480 | 0.0502272 |
| healthyR | 3 | EARTH | Test | 0.6345182 | 128.86998 | 0.7725966 | 134.83236 | 0.7845044 | 0.0502272 |
| healthyR | 4 | NNAR | Test | 0.7000701 | 149.57695 | 0.8524134 | 166.65682 | 0.8571585 | 0.0245626 |
| healthyR.ts | 1 | ARIMA | Test | 0.6372057 | 102.92148 | 0.8125609 | 154.61933 | 0.7857825 | 0.0276328 |
| healthyR.ts | 2 | LM | Test | 0.9049160 | 161.96352 | 1.1539436 | 154.63168 | 1.1203716 | 0.0276328 |
| healthyR.ts | 3 | EARTH | Test | 0.5489115 | 157.42151 | 0.6999687 | 93.41615 | 0.6972032 | 0.0276328 |
| healthyR.ts | 4 | NNAR | Test | 0.7674924 | 109.87067 | 0.9787018 | 175.14898 | 0.9695342 | 0.0003261 |
| healthyverse | 1 | ARIMA | Test | 0.5973861 | 142.79604 | 1.0699211 | 78.62738 | 0.7592503 | 0.0141880 |
| healthyverse | 2 | LM | Test | 0.5832757 | 147.96670 | 1.0446493 | 75.66541 | 0.7129084 | 0.0305761 |
| healthyverse | 3 | EARTH | Test | 0.5890906 | 119.70138 | 1.0550638 | 78.17111 | 0.7650496 | NA |
| healthyverse | 4 | NNAR | Test | 0.6661535 | 100.58549 | 1.1930838 | 92.55448 | 0.8604255 | 0.0246351 |
| healthyR.ai | 1 | ARIMA | Test | 0.6395329 | 110.72057 | 0.9745449 | 172.58049 | 0.7969351 | 0.0234194 |
| healthyR.ai | 2 | LM | Test | 0.5678213 | 90.64147 | 0.8652681 | 124.13721 | 0.7337760 | 0.0463945 |
| healthyR.ai | 3 | EARTH | Test | 1.5170939 | 772.28622 | 2.3118063 | 117.30769 | 1.7342538 | 0.0463945 |
| healthyR.ai | 4 | NNAR | Test | 0.5892327 | 130.05376 | 0.8978955 | 125.50746 | 0.7586005 | 0.1139031 |
| TidyDensity | 1 | ARIMA | Test | 0.4935984 | 172.03605 | 1.0726726 | 112.53752 | 0.6396590 | 0.0239366 |
| TidyDensity | 2 | LM | Test | 0.6380096 | 267.23984 | 1.3865022 | 119.11747 | 0.8023462 | 0.0025582 |
| TidyDensity | 3 | EARTH | Test | 0.4761721 | 161.41729 | 1.0348023 | 111.38477 | 0.6208872 | 0.0025582 |
| TidyDensity | 4 | NNAR | Test | 0.4241940 | 110.95388 | 0.9218449 | 121.39702 | 0.5371477 | 0.0072082 |
| tidyAML | 1 | ARIMA | Test | 0.8063876 | 130.51856 | 0.8517556 | 101.19456 | 1.1166030 | 0.0011514 |
| tidyAML | 2 | LM | Test | 0.7847263 | 124.73835 | 0.8288757 | 100.31402 | 1.0904809 | 0.2796813 |
| tidyAML | 3 | EARTH | Test | 1.2695063 | 274.06927 | 1.3409298 | 120.40715 | 1.4888505 | 0.2796813 |
| tidyAML | 4 | NNAR | Test | 0.7546819 | 114.02068 | 0.7971410 | 100.72009 | 1.0658945 | 0.0064640 |
| RandomWalker | 1 | ARIMA | Test | 1.1015081 | 120.86909 | 0.6021124 | 123.70022 | 1.4039810 | 0.0172449 |
| RandomWalker | 2 | LM | Test | 1.2002443 | 118.06846 | 0.6560841 | 191.16879 | 1.3687606 | 0.0123125 |
| RandomWalker | 3 | EARTH | Test | 4.6809575 | 1181.24558 | 2.5587307 | 165.75031 | 5.2047281 | 0.0123125 |
| RandomWalker | 4 | NNAR | Test | 1.1558392 | 181.07405 | 0.6318112 | 156.68751 | 1.3411679 | 0.0401079 |

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
    ## 1 healthyR.da…         2 LM          Test  0.638 207.  0.683 123.  0.794 0.00442
    ## 2 healthyR             3 EARTH       Test  0.635 129.  0.773 135.  0.785 0.0502 
    ## 3 healthyR.ts          3 EARTH       Test  0.549 157.  0.700  93.4 0.697 0.0276 
    ## 4 healthyverse         2 LM          Test  0.583 148.  1.04   75.7 0.713 0.0306 
    ## 5 healthyR.ai          2 LM          Test  0.568  90.6 0.865 124.  0.734 0.0464 
    ## 6 TidyDensity          4 NNAR        Test  0.424 111.  0.922 121.  0.537 0.00721
    ## 7 tidyAML              4 NNAR        Test  0.755 114.  0.797 101.  1.07  0.00646
    ## 8 RandomWalker         4 NNAR        Test  1.16  181.  0.632 157.  1.34  0.0401

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
    ## 1 healthyR.data <tibble>     <tibble>     <split [1622|28]> <mdl_tm_t [1 × 5]>
    ## 2 healthyR      <tibble>     <tibble>     <split [1616|28]> <mdl_tm_t [1 × 5]>
    ## 3 healthyR.ts   <tibble>     <tibble>     <split [1560|28]> <mdl_tm_t [1 × 5]>
    ## 4 healthyverse  <tibble>     <tibble>     <split [1530|28]> <mdl_tm_t [1 × 5]>
    ## 5 healthyR.ai   <tibble>     <tibble>     <split [1355|28]> <mdl_tm_t [1 × 5]>
    ## 6 TidyDensity   <tibble>     <tibble>     <split [1206|28]> <mdl_tm_t [1 × 5]>
    ## 7 tidyAML       <tibble>     <tibble>     <split [814|28]>  <mdl_tm_t [1 × 5]>
    ## 8 RandomWalker  <tibble>     <tibble>     <split [236|28]>  <mdl_tm_t [1 × 5]>

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
