Time Series Analysis and Nested Modeling of the Healthyverse Packages
================
Steven P. Sanderson II, MPH - Date:
12 June, 2025

This analysis follows a *Nested Modeltime Workflow*.

## Get Data

``` r
glimpse(downloads_tbl)
```

    ## Rows: 142,067
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

The last day in the data set is 2025-06-10 22:27:16, the file was
birthed on: 2024-08-07 07:35:44, and at report knit time is -7378.86
hours old. Happy analyzing!

Now that we have our data lets take a look at it using the `skimr`
package.

``` r
skim(downloads_tbl)
```

|                                                  |               |
|:-------------------------------------------------|:--------------|
| Name                                             | downloads_tbl |
| Number of rows                                   | 142067        |
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
| r_version     |    102490 |          0.28 |   5 |   5 |     0 |       47 |          0 |
| r_arch        |    102490 |          0.28 |   3 |   7 |     0 |        5 |          0 |
| r_os          |    102490 |          0.28 |   7 |  15 |     0 |       22 |          0 |
| package       |         0 |          1.00 |   7 |  13 |     0 |        8 |          0 |
| version       |         0 |          1.00 |   5 |  17 |     0 |       60 |          0 |
| country       |     12071 |          0.92 |   2 |   2 |     0 |      163 |          0 |

**Variable type: Date**

| skim_variable | n_missing | complete_rate | min | max | median | n_unique |
|:---|---:|---:|:---|:---|:---|---:|
| date | 0 | 1 | 2020-11-23 | 2025-06-10 | 2023-07-02 | 1661 |

**Variable type: numeric**

| skim_variable | n_missing | complete_rate | mean | sd | p0 | p25 | p50 | p75 | p100 | hist |
|:---|---:|---:|---:|---:|---:|---:|---:|---:|---:|:---|
| size | 0 | 1 | 1133402.50 | 1516824.53 | 355 | 14701 | 289710 | 2367727 | 5677952 | ▇▁▂▁▁ |
| ip_id | 0 | 1 | 10435.42 | 18588.14 | 1 | 280 | 3030 | 11668 | 209747 | ▇▁▁▁▁ |

**Variable type: POSIXct**

| skim_variable | n_missing | complete_rate | min | max | median | n_unique |
|:---|---:|---:|:---|:---|:---|---:|
| date_time | 0 | 1 | 2020-11-23 09:00:41 | 2025-06-10 22:27:16 | 2023-07-02 08:01:01 | 87036 |

**Variable type: Timespan**

| skim_variable | n_missing | complete_rate | min | max |     median | n_unique |
|:--------------|----------:|--------------:|----:|----:|-----------:|---------:|
| time          |         0 |             1 |   0 |  59 | 12H 6M 39S |       60 |

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
    ## -146.97  -35.81  -11.04   26.82  814.95 
    ## 
    ## Coefficients:
    ##                                                      Estimate Std. Error
    ## (Intercept)                                        -1.554e+02  6.779e+01
    ## date                                                9.724e-03  3.591e-03
    ## lag(value, 1)                                       1.039e-01  2.432e-02
    ## lag(value, 7)                                       9.707e-02  2.520e-02
    ## lag(value, 14)                                      8.952e-02  2.519e-02
    ## lag(value, 21)                                      6.770e-02  2.527e-02
    ## lag(value, 28)                                      6.973e-02  2.515e-02
    ## lag(value, 35)                                      6.710e-02  2.526e-02
    ## lag(value, 42)                                      5.067e-02  2.537e-02
    ## lag(value, 49)                                      7.053e-02  2.525e-02
    ## month(date, label = TRUE).L                        -9.581e+00  5.107e+00
    ## month(date, label = TRUE).Q                         4.159e+00  5.108e+00
    ## month(date, label = TRUE).C                        -1.351e+01  5.129e+00
    ## month(date, label = TRUE)^4                        -7.441e+00  5.158e+00
    ## month(date, label = TRUE)^5                        -1.093e+01  5.113e+00
    ## month(date, label = TRUE)^6                        -3.243e+00  5.204e+00
    ## month(date, label = TRUE)^7                        -7.646e+00  5.089e+00
    ## month(date, label = TRUE)^8                        -3.625e+00  5.090e+00
    ## month(date, label = TRUE)^9                         6.106e+00  5.098e+00
    ## month(date, label = TRUE)^10                        3.108e+00  5.075e+00
    ## month(date, label = TRUE)^11                       -4.975e+00  5.172e+00
    ## fourier_vec(date, type = "sin", K = 1, period = 7) -1.146e+01  2.326e+00
    ## fourier_vec(date, type = "cos", K = 1, period = 7)  7.942e+00  2.449e+00
    ##                                                    t value Pr(>|t|)    
    ## (Intercept)                                         -2.292 0.022039 *  
    ## date                                                 2.708 0.006840 ** 
    ## lag(value, 1)                                        4.273 2.04e-05 ***
    ## lag(value, 7)                                        3.852 0.000122 ***
    ## lag(value, 14)                                       3.554 0.000391 ***
    ## lag(value, 21)                                       2.679 0.007468 ** 
    ## lag(value, 28)                                       2.772 0.005628 ** 
    ## lag(value, 35)                                       2.656 0.007984 ** 
    ## lag(value, 42)                                       1.997 0.045947 *  
    ## lag(value, 49)                                       2.794 0.005276 ** 
    ## month(date, label = TRUE).L                         -1.876 0.060848 .  
    ## month(date, label = TRUE).Q                          0.814 0.415645    
    ## month(date, label = TRUE).C                         -2.633 0.008535 ** 
    ## month(date, label = TRUE)^4                         -1.442 0.149380    
    ## month(date, label = TRUE)^5                         -2.137 0.032771 *  
    ## month(date, label = TRUE)^6                         -0.623 0.533277    
    ## month(date, label = TRUE)^7                         -1.503 0.133142    
    ## month(date, label = TRUE)^8                         -0.712 0.476533    
    ## month(date, label = TRUE)^9                          1.198 0.231188    
    ## month(date, label = TRUE)^10                         0.612 0.540312    
    ## month(date, label = TRUE)^11                        -0.962 0.336215    
    ## fourier_vec(date, type = "sin", K = 1, period = 7)  -4.927 9.22e-07 ***
    ## fourier_vec(date, type = "cos", K = 1, period = 7)   3.243 0.001205 ** 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 58.7 on 1589 degrees of freedom
    ##   (49 observations deleted due to missingness)
    ## Multiple R-squared:  0.2385, Adjusted R-squared:  0.228 
    ## F-statistic: 22.62 on 22 and 1589 DF,  p-value: < 2.2e-16

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
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 2.91521481636558"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 21, 63 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 2.59262035382948"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 21, 63, 56 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 2.47628061557143"
    ## [1] "BEST method = 'lin', seasonal.factor = c( 21, 63, 56 )"
    ## [1] "BEST lin OBJECTIVE FUNCTION = 2.47628061557143"
    ## [1] "CURRNET METHOD: nonlin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'nonlin' , seasonal.factor =  c( 21, 63, 56 ) ...)"
    ## [1] "CURRENT nonlin OBJECTIVE FUNCTION = 5.33564791029902"
    ## [1] "BEST method = 'nonlin' PATH MEMBER = c( 21, 63, 56 )"
    ## [1] "BEST nonlin OBJECTIVE FUNCTION = 5.33564791029902"
    ## [1] "CURRNET METHOD: both"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'both' , seasonal.factor =  c( 21, 63, 56 ) ...)"
    ## [1] "CURRENT both OBJECTIVE FUNCTION = 3.18460403107977"
    ## [1] "BEST method = 'both' PATH MEMBER = c( 21, 63, 56 )"
    ## [1] "BEST both OBJECTIVE FUNCTION = 3.18460403107977"

![](man/figures/README-nns_forecasting-1.png)<!-- -->

    ## Package: healthyR.ai
    ## [1] "CURRNET METHOD: lin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 63 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 3.71829718717409"
    ## [1] "BEST method = 'lin', seasonal.factor = c( 63 )"
    ## [1] "BEST lin OBJECTIVE FUNCTION = 3.71829718717409"
    ## [1] "CURRNET METHOD: nonlin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'nonlin' , seasonal.factor =  c( 63 ) ...)"
    ## [1] "CURRENT nonlin OBJECTIVE FUNCTION = 4.07014241779248"
    ## [1] "BEST method = 'nonlin' PATH MEMBER = c( 63 )"
    ## [1] "BEST nonlin OBJECTIVE FUNCTION = 4.07014241779248"
    ## [1] "CURRNET METHOD: both"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'both' , seasonal.factor =  c( 63 ) ...)"
    ## [1] "CURRENT both OBJECTIVE FUNCTION = 3.75261357458102"
    ## [1] "BEST method = 'both' PATH MEMBER = c( 63 )"
    ## [1] "BEST both OBJECTIVE FUNCTION = 3.75261357458102"

![](man/figures/README-nns_forecasting-2.png)<!-- -->

    ## Package: healthyR.data
    ## [1] "CURRNET METHOD: lin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 63 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 2.87617132100771"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 63, 91 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 2.20307653361872"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 63, 91, 84 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 2.01347918569906"
    ## [1] "BEST method = 'lin', seasonal.factor = c( 63, 91, 84 )"
    ## [1] "BEST lin OBJECTIVE FUNCTION = 2.01347918569906"
    ## [1] "CURRNET METHOD: nonlin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'nonlin' , seasonal.factor =  c( 63, 91, 84 ) ...)"
    ## [1] "CURRENT nonlin OBJECTIVE FUNCTION = 2.21472142474345"
    ## [1] "BEST method = 'nonlin' PATH MEMBER = c( 63, 91, 84 )"
    ## [1] "BEST nonlin OBJECTIVE FUNCTION = 2.21472142474345"
    ## [1] "CURRNET METHOD: both"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'both' , seasonal.factor =  c( 63, 91, 84 ) ...)"
    ## [1] "CURRENT both OBJECTIVE FUNCTION = 1.96830403557603"
    ## [1] "BEST method = 'both' PATH MEMBER = c( 63, 91, 84 )"
    ## [1] "BEST both OBJECTIVE FUNCTION = 1.96830403557603"

![](man/figures/README-nns_forecasting-3.png)<!-- -->

    ## Package: healthyR.ts
    ## [1] "CURRNET METHOD: lin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 21 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 3.18455684219583"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 21, 56 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 2.74709596683155"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 21, 56, 63 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 2.62909162781826"
    ## [1] "BEST method = 'lin', seasonal.factor = c( 21, 56, 63 )"
    ## [1] "BEST lin OBJECTIVE FUNCTION = 2.62909162781826"
    ## [1] "CURRNET METHOD: nonlin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'nonlin' , seasonal.factor =  c( 21, 56, 63 ) ...)"
    ## [1] "CURRENT nonlin OBJECTIVE FUNCTION = 5.26453853721937"
    ## [1] "BEST method = 'nonlin' PATH MEMBER = c( 21, 56, 63 )"
    ## [1] "BEST nonlin OBJECTIVE FUNCTION = 5.26453853721937"
    ## [1] "CURRNET METHOD: both"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'both' , seasonal.factor =  c( 21, 56, 63 ) ...)"
    ## [1] "CURRENT both OBJECTIVE FUNCTION = 3.10621300871698"
    ## [1] "BEST method = 'both' PATH MEMBER = c( 21, 56, 63 )"
    ## [1] "BEST both OBJECTIVE FUNCTION = 3.10621300871698"

![](man/figures/README-nns_forecasting-4.png)<!-- -->

    ## Package: healthyverse
    ## [1] "CURRNET METHOD: lin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 56 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 4.6192397379952"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 56, 35 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 4.1068116631718"
    ## [1] "BEST method = 'lin', seasonal.factor = c( 56, 35 )"
    ## [1] "BEST lin OBJECTIVE FUNCTION = 4.1068116631718"
    ## [1] "CURRNET METHOD: nonlin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'nonlin' , seasonal.factor =  c( 56, 35 ) ...)"
    ## [1] "CURRENT nonlin OBJECTIVE FUNCTION = 4.29210102849482"
    ## [1] "BEST method = 'nonlin' PATH MEMBER = c( 56, 35 )"
    ## [1] "BEST nonlin OBJECTIVE FUNCTION = 4.29210102849482"
    ## [1] "CURRNET METHOD: both"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'both' , seasonal.factor =  c( 56, 35 ) ...)"
    ## [1] "CURRENT both OBJECTIVE FUNCTION = 3.87985849539128"
    ## [1] "BEST method = 'both' PATH MEMBER = c( 56, 35 )"
    ## [1] "BEST both OBJECTIVE FUNCTION = 3.87985849539128"

![](man/figures/README-nns_forecasting-5.png)<!-- -->

    ## Package: RandomWalker
    ## [1] "CURRNET METHOD: lin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 63 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 3.73056935931231"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 63, 70 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 3.33949654064453"
    ## [1] "BEST method = 'lin', seasonal.factor = c( 63, 70 )"
    ## [1] "BEST lin OBJECTIVE FUNCTION = 3.33949654064453"
    ## [1] "CURRNET METHOD: nonlin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'nonlin' , seasonal.factor =  c( 63, 70 ) ...)"
    ## [1] "CURRENT nonlin OBJECTIVE FUNCTION = 3.15621333578588"
    ## [1] "BEST method = 'nonlin' PATH MEMBER = c( 63, 70 )"
    ## [1] "BEST nonlin OBJECTIVE FUNCTION = 3.15621333578588"
    ## [1] "CURRNET METHOD: both"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'both' , seasonal.factor =  c( 63, 70 ) ...)"
    ## [1] "CURRENT both OBJECTIVE FUNCTION = 3.10947206863831"
    ## [1] "BEST method = 'both' PATH MEMBER = c( 63, 70 )"
    ## [1] "BEST both OBJECTIVE FUNCTION = 3.10947206863831"

![](man/figures/README-nns_forecasting-6.png)<!-- -->

    ## Package: tidyAML
    ## [1] "CURRNET METHOD: lin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 84 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 4.52100990495556"
    ## [1] "BEST method = 'lin', seasonal.factor = c( 84 )"
    ## [1] "BEST lin OBJECTIVE FUNCTION = 4.52100990495556"
    ## [1] "CURRNET METHOD: nonlin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'nonlin' , seasonal.factor =  c( 84 ) ...)"
    ## [1] "CURRENT nonlin OBJECTIVE FUNCTION = 3.04976326283408"
    ## [1] "BEST method = 'nonlin' PATH MEMBER = c( 84 )"
    ## [1] "BEST nonlin OBJECTIVE FUNCTION = 3.04976326283408"
    ## [1] "CURRNET METHOD: both"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'both' , seasonal.factor =  c( 84 ) ...)"
    ## [1] "CURRENT both OBJECTIVE FUNCTION = 3.4621098854035"
    ## [1] "BEST method = 'both' PATH MEMBER = c( 84 )"
    ## [1] "BEST both OBJECTIVE FUNCTION = 3.4621098854035"

![](man/figures/README-nns_forecasting-7.png)<!-- -->

    ## Package: TidyDensity
    ## [1] "CURRNET METHOD: lin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 21 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 4.31921284952496"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 21, 63 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 3.61767602178896"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 21, 63, 35 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 3.41869581823957"
    ## [1] "BEST method = 'lin', seasonal.factor = c( 21, 63, 35 )"
    ## [1] "BEST lin OBJECTIVE FUNCTION = 3.41869581823957"
    ## [1] "CURRNET METHOD: nonlin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'nonlin' , seasonal.factor =  c( 21, 63, 35 ) ...)"
    ## [1] "CURRENT nonlin OBJECTIVE FUNCTION = 6.05772225516313"
    ## [1] "BEST method = 'nonlin' PATH MEMBER = c( 21, 63, 35 )"
    ## [1] "BEST nonlin OBJECTIVE FUNCTION = 6.05772225516313"
    ## [1] "CURRNET METHOD: both"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'both' , seasonal.factor =  c( 21, 63, 35 ) ...)"
    ## [1] "CURRENT both OBJECTIVE FUNCTION = 4.12759817039559"
    ## [1] "BEST method = 'both' PATH MEMBER = c( 21, 63, 35 )"
    ## [1] "BEST both OBJECTIVE FUNCTION = 4.12759817039559"

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
    ## 1 healthyR.data <tibble [1,653 × 2]> <tibble [28 × 2]> <split [1625|28]>
    ## 2 healthyR      <tibble [1,647 × 2]> <tibble [28 × 2]> <split [1619|28]>
    ## 3 healthyR.ts   <tibble [1,591 × 2]> <tibble [28 × 2]> <split [1563|28]>
    ## 4 healthyverse  <tibble [1,561 × 2]> <tibble [28 × 2]> <split [1533|28]>
    ## 5 healthyR.ai   <tibble [1,386 × 2]> <tibble [28 × 2]> <split [1358|28]>
    ## 6 TidyDensity   <tibble [1,237 × 2]> <tibble [28 × 2]> <split [1209|28]>
    ## 7 tidyAML       <tibble [845 × 2]>   <tibble [28 × 2]> <split [817|28]> 
    ## 8 RandomWalker  <tibble [267 × 2]>   <tibble [28 × 2]> <split [239|28]>

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
| healthyR.data | 1 | ARIMA | Test | 0.5543083 | 111.24333 | 0.6351046 | 120.61688 | 0.7503719 | 0.1241937 |
| healthyR.data | 2 | LM | Test | 0.6147470 | 177.34337 | 0.7043529 | 121.46999 | 0.7627377 | 0.0044136 |
| healthyR.data | 3 | EARTH | Test | 0.6475400 | 242.07980 | 0.7419259 | 116.95894 | 0.7574455 | 0.0044136 |
| healthyR.data | 4 | NNAR | Test | 0.6751413 | 100.42853 | 0.7735503 | 173.70854 | 0.9030172 | 0.0476296 |
| healthyR | 1 | ARIMA | Test | 0.6857557 | 130.08945 | 0.7961886 | 162.81769 | 0.8323099 | 0.0343220 |
| healthyR | 2 | LM | Test | 0.6693557 | 94.27716 | 0.7771476 | 166.42734 | 0.8458884 | 0.0421303 |
| healthyR | 3 | EARTH | Test | 0.6370625 | 121.34807 | 0.7396539 | 135.48235 | 0.7724328 | 0.0421303 |
| healthyR | 4 | NNAR | Test | 0.6890466 | 144.56953 | 0.8000095 | 165.03304 | 0.8350429 | 0.0249229 |
| healthyR.ts | 1 | ARIMA | Test | 0.6476690 | 102.65601 | 0.7618016 | 141.97119 | 0.7996073 | 0.0252008 |
| healthyR.ts | 2 | LM | Test | 0.9252308 | 161.01368 | 1.0882756 | 156.75163 | 1.1336878 | 0.0252008 |
| healthyR.ts | 3 | EARTH | Test | 0.5723099 | 155.18720 | 0.6731627 | 90.88548 | 0.7455209 | 0.0252008 |
| healthyR.ts | 4 | NNAR | Test | 0.7836890 | 106.74174 | 0.9217912 | 176.85029 | 0.9747867 | 0.0000809 |
| healthyverse | 1 | ARIMA | Test | 0.5854896 | 111.86504 | 1.0926454 | 74.78468 | 0.7468628 | 0.0195729 |
| healthyverse | 2 | LM | Test | 0.5614285 | 119.89964 | 1.0477423 | 70.82453 | 0.6893643 | 0.0381806 |
| healthyverse | 3 | EARTH | Test | 0.5783040 | 99.13400 | 1.0792355 | 74.56459 | 0.7481917 | NA |
| healthyverse | 4 | NNAR | Test | 0.6692243 | 94.20967 | 1.2489116 | 90.03727 | 0.8515259 | 0.0765028 |
| healthyR.ai | 1 | ARIMA | Test | 0.6056434 | 99.55927 | 0.8154885 | 148.12547 | 0.7885833 | 0.0048254 |
| healthyR.ai | 2 | LM | Test | 0.5834001 | 95.06540 | 0.7855382 | 130.40671 | 0.7453043 | 0.0209051 |
| healthyR.ai | 3 | EARTH | Test | 0.5751171 | 96.62768 | 0.7743853 | 125.63172 | 0.7348582 | 0.0209051 |
| healthyR.ai | 4 | NNAR | Test | 0.5954958 | 133.51948 | 0.8018249 | 129.00230 | 0.7621383 | 0.0755486 |
| TidyDensity | 1 | ARIMA | Test | 0.4858558 | 127.67082 | 0.9152248 | 110.96957 | 0.6329225 | 0.0084049 |
| TidyDensity | 2 | LM | Test | 0.6373356 | 217.03768 | 1.2005730 | 115.53710 | 0.8076625 | 0.0068576 |
| TidyDensity | 3 | EARTH | Test | 0.4905843 | 131.07553 | 0.9241321 | 109.97058 | 0.6364827 | 0.0068576 |
| TidyDensity | 4 | NNAR | Test | 0.4451806 | 86.54496 | 0.8386034 | 119.67584 | 0.5700472 | 0.0081048 |
| tidyAML | 1 | ARIMA | Test | 0.7311084 | 116.89162 | 0.7975945 | 119.46598 | 0.9102169 | 0.0153036 |
| tidyAML | 2 | LM | Test | 0.7117264 | 133.37720 | 0.7764499 | 98.21445 | 0.9153674 | 0.3691960 |
| tidyAML | 3 | EARTH | Test | 4.4193446 | 1027.86462 | 4.8212343 | 172.13582 | 5.0525856 | 0.3691960 |
| tidyAML | 4 | NNAR | Test | 0.6564599 | 113.60609 | 0.7161575 | 102.90776 | 0.8334769 | 0.1527565 |
| RandomWalker | 1 | ARIMA | Test | 1.2443471 | 140.58649 | 0.6562067 | 130.05018 | 1.5259131 | 0.0021469 |
| RandomWalker | 2 | LM | Test | 1.2766830 | 118.64374 | 0.6732590 | 190.76278 | 1.4314072 | 0.0096811 |
| RandomWalker | 3 | EARTH | Test | 1.1950295 | 97.74841 | 0.6301990 | 175.74078 | 1.4083714 | NA |
| RandomWalker | 4 | NNAR | Test | 1.3590855 | 171.17595 | 0.7167140 | 153.46952 | 1.5944659 | 0.0001815 |

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
    ##   package     .model_id .model_desc .type   mae  mape  mase smape  rmse      rsq
    ##   <fct>           <int> <chr>       <chr> <dbl> <dbl> <dbl> <dbl> <dbl>    <dbl>
    ## 1 healthyR.d…         1 ARIMA       Test  0.554 111.  0.635 121.  0.750  0.124  
    ## 2 healthyR            3 EARTH       Test  0.637 121.  0.740 135.  0.772  0.0421 
    ## 3 healthyR.ts         3 EARTH       Test  0.572 155.  0.673  90.9 0.746  0.0252 
    ## 4 healthyver…         2 LM          Test  0.561 120.  1.05   70.8 0.689  0.0382 
    ## 5 healthyR.ai         3 EARTH       Test  0.575  96.6 0.774 126.  0.735  0.0209 
    ## 6 TidyDensity         4 NNAR        Test  0.445  86.5 0.839 120.  0.570  0.00810
    ## 7 tidyAML             4 NNAR        Test  0.656 114.  0.716 103.  0.833  0.153  
    ## 8 RandomWalk…         3 EARTH       Test  1.20   97.7 0.630 176.  1.41  NA

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
    ## 1 healthyR.data <tibble>     <tibble>     <split [1625|28]> <mdl_tm_t [1 × 5]>
    ## 2 healthyR      <tibble>     <tibble>     <split [1619|28]> <mdl_tm_t [1 × 5]>
    ## 3 healthyR.ts   <tibble>     <tibble>     <split [1563|28]> <mdl_tm_t [1 × 5]>
    ## 4 healthyverse  <tibble>     <tibble>     <split [1533|28]> <mdl_tm_t [1 × 5]>
    ## 5 healthyR.ai   <tibble>     <tibble>     <split [1358|28]> <mdl_tm_t [1 × 5]>
    ## 6 TidyDensity   <tibble>     <tibble>     <split [1209|28]> <mdl_tm_t [1 × 5]>
    ## 7 tidyAML       <tibble>     <tibble>     <split [817|28]>  <mdl_tm_t [1 × 5]>
    ## 8 RandomWalker  <tibble>     <tibble>     <split [239|28]>  <mdl_tm_t [1 × 5]>

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
