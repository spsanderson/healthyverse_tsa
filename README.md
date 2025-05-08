Time Series Analysis and Nested Modeling of the Healthyverse Packages
================
Steven P. Sanderson II, MPH - Date:
08 May, 2025

This analysis follows a *Nested Modeltime Workflow*.

## Get Data

``` r
glimpse(downloads_tbl)
```

    ## Rows: 139,140
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

The last day in the data set is 2025-05-06 23:59:40, the file was
birthed on: 2024-08-07 07:35:44, and at report knit time is -6540.4
hours old. Happy analyzing!

Now that we have our data lets take a look at it using the `skimr`
package.

``` r
skim(downloads_tbl)
```

|                                                  |               |
|:-------------------------------------------------|:--------------|
| Name                                             | downloads_tbl |
| Number of rows                                   | 139140        |
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
| r_version     |    100236 |          0.28 |   5 |   5 |     0 |       46 |          0 |
| r_arch        |    100236 |          0.28 |   3 |   7 |     0 |        5 |          0 |
| r_os          |    100236 |          0.28 |   7 |  15 |     0 |       22 |          0 |
| package       |         0 |          1.00 |   7 |  13 |     0 |        8 |          0 |
| version       |         0 |          1.00 |   5 |  17 |     0 |       60 |          0 |
| country       |     11823 |          0.92 |   2 |   2 |     0 |      163 |          0 |

**Variable type: Date**

| skim_variable | n_missing | complete_rate | min | max | median | n_unique |
|:---|---:|---:|:---|:---|:---|---:|
| date | 0 | 1 | 2020-11-23 | 2025-05-06 | 2023-06-15 | 1626 |

**Variable type: numeric**

| skim_variable | n_missing | complete_rate | mean | sd | p0 | p25 | p50 | p75 | p100 | hist |
|:---|---:|---:|---:|---:|---:|---:|---:|---:|---:|:---|
| size | 0 | 1 | 1134343.17 | 1520175.54 | 355 | 14701 | 288968.5 | 2367754 | 5677952 | ▇▁▂▁▁ |
| ip_id | 0 | 1 | 10395.59 | 18489.62 | 1 | 292 | 3058.0 | 11666 | 209747 | ▇▁▁▁▁ |

**Variable type: POSIXct**

| skim_variable | n_missing | complete_rate | min | max | median | n_unique |
|:---|---:|---:|:---|:---|:---|---:|
| date_time | 0 | 1 | 2020-11-23 09:00:41 | 2025-05-06 23:59:40 | 2023-06-15 07:14:58 | 84908 |

**Variable type: Timespan**

| skim_variable | n_missing | complete_rate | min | max | median | n_unique |
|:--------------|----------:|--------------:|----:|----:|-------:|---------:|
| time          |         0 |             1 |   0 |  59 |     20 |       60 |

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
    ## -146.71  -35.87  -11.01   26.64  814.63 
    ## 
    ## Coefficients:
    ##                                                      Estimate Std. Error
    ## (Intercept)                                        -1.796e+02  7.086e+01
    ## date                                                1.100e-02  3.757e-03
    ## lag(value, 1)                                       1.048e-01  2.459e-02
    ## lag(value, 7)                                       9.797e-02  2.546e-02
    ## lag(value, 14)                                      9.204e-02  2.551e-02
    ## lag(value, 21)                                      6.738e-02  2.560e-02
    ## lag(value, 28)                                      6.730e-02  2.557e-02
    ## lag(value, 35)                                      6.719e-02  2.563e-02
    ## lag(value, 42)                                      4.884e-02  2.580e-02
    ## lag(value, 49)                                      6.724e-02  2.562e-02
    ## month(date, label = TRUE).L                        -9.968e+00  5.126e+00
    ## month(date, label = TRUE).Q                         3.106e+00  5.180e+00
    ## month(date, label = TRUE).C                        -1.267e+01  5.170e+00
    ## month(date, label = TRUE)^4                        -6.711e+00  5.194e+00
    ## month(date, label = TRUE)^5                        -1.189e+01  5.178e+00
    ## month(date, label = TRUE)^6                        -3.242e+00  5.235e+00
    ## month(date, label = TRUE)^7                        -6.579e+00  5.152e+00
    ## month(date, label = TRUE)^8                        -4.093e+00  5.151e+00
    ## month(date, label = TRUE)^9                         5.393e+00  5.143e+00
    ## month(date, label = TRUE)^10                        4.182e+00  5.201e+00
    ## month(date, label = TRUE)^11                       -5.533e+00  5.312e+00
    ## fourier_vec(date, type = "sin", K = 1, period = 7) -1.153e+01  2.364e+00
    ## fourier_vec(date, type = "cos", K = 1, period = 7)  8.118e+00  2.488e+00
    ##                                                    t value Pr(>|t|)    
    ## (Intercept)                                         -2.535 0.011335 *  
    ## date                                                 2.929 0.003455 ** 
    ## lag(value, 1)                                        4.262 2.15e-05 ***
    ## lag(value, 7)                                        3.847 0.000124 ***
    ## lag(value, 14)                                       3.608 0.000318 ***
    ## lag(value, 21)                                       2.632 0.008563 ** 
    ## lag(value, 28)                                       2.632 0.008569 ** 
    ## lag(value, 35)                                       2.621 0.008841 ** 
    ## lag(value, 42)                                       1.893 0.058582 .  
    ## lag(value, 49)                                       2.625 0.008753 ** 
    ## month(date, label = TRUE).L                         -1.945 0.051985 .  
    ## month(date, label = TRUE).Q                          0.600 0.548852    
    ## month(date, label = TRUE).C                         -2.450 0.014385 *  
    ## month(date, label = TRUE)^4                         -1.292 0.196538    
    ## month(date, label = TRUE)^5                         -2.297 0.021738 *  
    ## month(date, label = TRUE)^6                         -0.619 0.535838    
    ## month(date, label = TRUE)^7                         -1.277 0.201831    
    ## month(date, label = TRUE)^8                         -0.795 0.427004    
    ## month(date, label = TRUE)^9                          1.049 0.294530    
    ## month(date, label = TRUE)^10                         0.804 0.421483    
    ## month(date, label = TRUE)^11                        -1.042 0.297701    
    ## fourier_vec(date, type = "sin", K = 1, period = 7)  -4.877 1.18e-06 ***
    ## fourier_vec(date, type = "cos", K = 1, period = 7)   3.262 0.001129 ** 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 58.79 on 1554 degrees of freedom
    ##   (49 observations deleted due to missingness)
    ## Multiple R-squared:  0.2429, Adjusted R-squared:  0.2321 
    ## F-statistic: 22.66 on 22 and 1554 DF,  p-value: < 2.2e-16

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
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 5.29377538931263"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 84, 91 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 5.01250682275546"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 84, 91, 63 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 4.83019594195247"
    ## [1] "BEST method = 'lin', seasonal.factor = c( 84, 91, 63 )"
    ## [1] "BEST lin OBJECTIVE FUNCTION = 4.83019594195247"
    ## [1] "CURRNET METHOD: nonlin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'nonlin' , seasonal.factor =  c( 84, 91, 63 ) ...)"
    ## [1] "CURRENT nonlin OBJECTIVE FUNCTION = 5.46800535706179"
    ## [1] "BEST method = 'nonlin' PATH MEMBER = c( 84, 91, 63 )"
    ## [1] "BEST nonlin OBJECTIVE FUNCTION = 5.46800535706179"
    ## [1] "CURRNET METHOD: both"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'both' , seasonal.factor =  c( 84, 91, 63 ) ...)"
    ## [1] "CURRENT both OBJECTIVE FUNCTION = 4.98485619525449"
    ## [1] "BEST method = 'both' PATH MEMBER = c( 84, 91, 63 )"
    ## [1] "BEST both OBJECTIVE FUNCTION = 4.98485619525449"

![](man/figures/README-nns_forecasting-1.png)<!-- -->

    ## Package: healthyR.ai
    ## [1] "CURRNET METHOD: lin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 84 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 6.10929919950795"
    ## [1] "BEST method = 'lin', seasonal.factor = c( 84 )"
    ## [1] "BEST lin OBJECTIVE FUNCTION = 6.10929919950795"
    ## [1] "CURRNET METHOD: nonlin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'nonlin' , seasonal.factor =  c( 84 ) ...)"
    ## [1] "CURRENT nonlin OBJECTIVE FUNCTION = 7.06585250016572"
    ## [1] "BEST method = 'nonlin' PATH MEMBER = c( 84 )"
    ## [1] "BEST nonlin OBJECTIVE FUNCTION = 7.06585250016572"
    ## [1] "CURRNET METHOD: both"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'both' , seasonal.factor =  c( 84 ) ...)"
    ## [1] "CURRENT both OBJECTIVE FUNCTION = 6.48482809286444"
    ## [1] "BEST method = 'both' PATH MEMBER = c( 84 )"
    ## [1] "BEST both OBJECTIVE FUNCTION = 6.48482809286444"

![](man/figures/README-nns_forecasting-2.png)<!-- -->

    ## Package: healthyR.data
    ## [1] "CURRNET METHOD: lin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 91 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 3.3267226365494"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 91, 63 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 2.51084589227281"
    ## [1] "BEST method = 'lin', seasonal.factor = c( 91, 63 )"
    ## [1] "BEST lin OBJECTIVE FUNCTION = 2.51084589227281"
    ## [1] "CURRNET METHOD: nonlin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'nonlin' , seasonal.factor =  c( 91, 63 ) ...)"
    ## [1] "CURRENT nonlin OBJECTIVE FUNCTION = 3.10888799330597"
    ## [1] "BEST method = 'nonlin' PATH MEMBER = c( 91, 63 )"
    ## [1] "BEST nonlin OBJECTIVE FUNCTION = 3.10888799330597"
    ## [1] "CURRNET METHOD: both"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'both' , seasonal.factor =  c( 91, 63 ) ...)"
    ## [1] "CURRENT both OBJECTIVE FUNCTION = 2.56539636649346"
    ## [1] "BEST method = 'both' PATH MEMBER = c( 91, 63 )"
    ## [1] "BEST both OBJECTIVE FUNCTION = 2.56539636649346"

![](man/figures/README-nns_forecasting-3.png)<!-- -->

    ## Package: healthyR.ts
    ## [1] "CURRNET METHOD: lin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 91 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 7.22572165094473"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 91, 35 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 6.40399340867358"
    ## [1] "BEST method = 'lin', seasonal.factor = c( 91, 35 )"
    ## [1] "BEST lin OBJECTIVE FUNCTION = 6.40399340867358"
    ## [1] "CURRNET METHOD: nonlin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'nonlin' , seasonal.factor =  c( 91, 35 ) ...)"
    ## [1] "CURRENT nonlin OBJECTIVE FUNCTION = 7.90707995752373"
    ## [1] "BEST method = 'nonlin' PATH MEMBER = c( 91, 35 )"
    ## [1] "BEST nonlin OBJECTIVE FUNCTION = 7.90707995752373"
    ## [1] "CURRNET METHOD: both"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'both' , seasonal.factor =  c( 91, 35 ) ...)"
    ## [1] "CURRENT both OBJECTIVE FUNCTION = 7.01897105472015"
    ## [1] "BEST method = 'both' PATH MEMBER = c( 91, 35 )"
    ## [1] "BEST both OBJECTIVE FUNCTION = 7.01897105472015"

![](man/figures/README-nns_forecasting-4.png)<!-- -->

    ## Package: healthyverse
    ## [1] "CURRNET METHOD: lin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 91 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 7.8224376075907"
    ## [1] "BEST method = 'lin', seasonal.factor = c( 91 )"
    ## [1] "BEST lin OBJECTIVE FUNCTION = 7.8224376075907"
    ## [1] "CURRNET METHOD: nonlin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'nonlin' , seasonal.factor =  c( 91 ) ...)"
    ## [1] "CURRENT nonlin OBJECTIVE FUNCTION = 9.61733002378018"
    ## [1] "BEST method = 'nonlin' PATH MEMBER = c( 91 )"
    ## [1] "BEST nonlin OBJECTIVE FUNCTION = 9.61733002378018"
    ## [1] "CURRNET METHOD: both"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'both' , seasonal.factor =  c( 91 ) ...)"
    ## [1] "CURRENT both OBJECTIVE FUNCTION = 8.73818445749781"
    ## [1] "BEST method = 'both' PATH MEMBER = c( 91 )"
    ## [1] "BEST both OBJECTIVE FUNCTION = 8.73818445749781"

![](man/figures/README-nns_forecasting-5.png)<!-- -->

    ## Package: RandomWalker
    ## [1] "CURRNET METHOD: lin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 21 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 3.5170384003746"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 21, 42 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 3.50776624009175"
    ## [1] "BEST method = 'lin', seasonal.factor = c( 21, 42 )"
    ## [1] "BEST lin OBJECTIVE FUNCTION = 3.50776624009175"
    ## [1] "CURRNET METHOD: nonlin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'nonlin' , seasonal.factor =  c( 21, 42 ) ...)"
    ## [1] "CURRENT nonlin OBJECTIVE FUNCTION = 3.68664747382787"
    ## [1] "BEST method = 'nonlin' PATH MEMBER = c( 21, 42 )"
    ## [1] "BEST nonlin OBJECTIVE FUNCTION = 3.68664747382787"
    ## [1] "CURRNET METHOD: both"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'both' , seasonal.factor =  c( 21, 42 ) ...)"
    ## [1] "CURRENT both OBJECTIVE FUNCTION = 3.38433105654449"
    ## [1] "BEST method = 'both' PATH MEMBER = c( 21, 42 )"
    ## [1] "BEST both OBJECTIVE FUNCTION = 3.38433105654449"

![](man/figures/README-nns_forecasting-6.png)<!-- -->

    ## Package: tidyAML
    ## [1] "CURRNET METHOD: lin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 28 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 2.93640493307151"
    ## [1] "BEST method = 'lin', seasonal.factor = c( 28 )"
    ## [1] "BEST lin OBJECTIVE FUNCTION = 2.93640493307151"
    ## [1] "CURRNET METHOD: nonlin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'nonlin' , seasonal.factor =  c( 28 ) ...)"
    ## [1] "CURRENT nonlin OBJECTIVE FUNCTION = 3.83490040164569"
    ## [1] "BEST method = 'nonlin' PATH MEMBER = c( 28 )"
    ## [1] "BEST nonlin OBJECTIVE FUNCTION = 3.83490040164569"
    ## [1] "CURRNET METHOD: both"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'both' , seasonal.factor =  c( 28 ) ...)"
    ## [1] "CURRENT both OBJECTIVE FUNCTION = 3.02560348327886"
    ## [1] "BEST method = 'both' PATH MEMBER = c( 28 )"
    ## [1] "BEST both OBJECTIVE FUNCTION = 3.02560348327886"

![](man/figures/README-nns_forecasting-7.png)<!-- -->

    ## Package: TidyDensity
    ## [1] "CURRNET METHOD: lin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 28 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 4.14103672401238"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 28, 84 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 3.49130793784197"
    ## [1] "BEST method = 'lin', seasonal.factor = c( 28, 84 )"
    ## [1] "BEST lin OBJECTIVE FUNCTION = 3.49130793784197"
    ## [1] "CURRNET METHOD: nonlin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'nonlin' , seasonal.factor =  c( 28, 84 ) ...)"
    ## [1] "CURRENT nonlin OBJECTIVE FUNCTION = 7.56378379610048"
    ## [1] "BEST method = 'nonlin' PATH MEMBER = c( 28, 84 )"
    ## [1] "BEST nonlin OBJECTIVE FUNCTION = 7.56378379610048"
    ## [1] "CURRNET METHOD: both"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'both' , seasonal.factor =  c( 28, 84 ) ...)"
    ## [1] "CURRENT both OBJECTIVE FUNCTION = 4.85090146090857"
    ## [1] "BEST method = 'both' PATH MEMBER = c( 28, 84 )"
    ## [1] "BEST both OBJECTIVE FUNCTION = 4.85090146090857"

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
    ## 1 healthyR.data <tibble [1,619 × 2]> <tibble [28 × 2]> <split [1591|28]>
    ## 2 healthyR      <tibble [1,612 × 2]> <tibble [28 × 2]> <split [1584|28]>
    ## 3 healthyR.ts   <tibble [1,556 × 2]> <tibble [28 × 2]> <split [1528|28]>
    ## 4 healthyverse  <tibble [1,526 × 2]> <tibble [28 × 2]> <split [1498|28]>
    ## 5 healthyR.ai   <tibble [1,351 × 2]> <tibble [28 × 2]> <split [1323|28]>
    ## 6 TidyDensity   <tibble [1,202 × 2]> <tibble [28 × 2]> <split [1174|28]>
    ## 7 tidyAML       <tibble [810 × 2]>   <tibble [28 × 2]> <split [782|28]> 
    ## 8 RandomWalker  <tibble [232 × 2]>   <tibble [28 × 2]> <split [204|28]>

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
| healthyR.data | 1 | ARIMA | Test | 0.7233637 | 123.66819 | 0.6472111 | 139.45717 | 0.9105574 | 0.0021694 |
| healthyR.data | 2 | LM | Test | 0.7293871 | 168.85649 | 0.6526004 | 127.49897 | 0.8972501 | 0.0223956 |
| healthyR.data | 3 | EARTH | Test | 0.9101035 | 327.67583 | 0.8142918 | 121.49071 | 1.0933788 | 0.0223956 |
| healthyR.data | 4 | NNAR | Test | 0.7592823 | 97.43918 | 0.6793483 | 175.20691 | 0.9340427 | 0.0413470 |
| healthyR | 1 | ARIMA | Test | 0.6464431 | 101.69493 | 0.7494943 | 181.74673 | 0.7977386 | 0.0063979 |
| healthyR | 2 | LM | Test | 0.6425171 | 95.11779 | 0.7449424 | 173.37926 | 0.7924279 | 0.1349577 |
| healthyR | 3 | EARTH | Test | 0.6179731 | 95.19730 | 0.7164858 | 141.10367 | 0.7641430 | 0.1349577 |
| healthyR | 4 | NNAR | Test | 0.6290908 | 90.82951 | 0.7293758 | 149.79824 | 0.7773714 | 0.0020832 |
| healthyR.ts | 1 | ARIMA | Test | 0.7918077 | 101.82633 | 0.6877309 | 174.33067 | 0.9770596 | 0.0880914 |
| healthyR.ts | 2 | LM | Test | 0.9105749 | 236.58346 | 0.7908871 | 144.46372 | 1.1393134 | 0.0880914 |
| healthyR.ts | 3 | EARTH | Test | 0.8661184 | 346.19049 | 0.7522740 | 129.06588 | 1.0311397 | 0.0880914 |
| healthyR.ts | 4 | NNAR | Test | 0.7970905 | 126.57206 | 0.6923192 | 179.10877 | 0.9770716 | 0.0002707 |
| healthyverse | 1 | ARIMA | Test | 0.6088585 | 336.85430 | 0.8340701 | 98.25235 | 0.7479835 | 0.0092045 |
| healthyverse | 2 | LM | Test | 0.6206692 | 372.06784 | 0.8502495 | 97.50149 | 0.7570800 | 0.0079822 |
| healthyverse | 3 | EARTH | Test | 0.7601515 | 546.77310 | 1.0413251 | 102.01560 | 0.8743799 | 0.0079822 |
| healthyverse | 4 | NNAR | Test | 0.5826534 | 210.61895 | 0.7981720 | 106.40809 | 0.7418712 | 0.1437978 |
| healthyR.ai | 1 | ARIMA | Test | 0.7794213 | 123.70642 | 0.8339429 | 167.09387 | 1.0264576 | 0.0362029 |
| healthyR.ai | 2 | LM | Test | 0.7800485 | 126.88961 | 0.8346140 | 161.31428 | 1.0409226 | 0.0037932 |
| healthyR.ai | 3 | EARTH | Test | 0.7993601 | 146.29602 | 0.8552764 | 150.05007 | 1.0742213 | 0.0037932 |
| healthyR.ai | 4 | NNAR | Test | 0.7727572 | 138.40777 | 0.8268127 | 153.43702 | 1.0373736 | 0.0512329 |
| TidyDensity | 1 | ARIMA | Test | 0.5569285 | 232.80961 | 0.8089991 | 118.72825 | 0.6819992 | 0.0028407 |
| TidyDensity | 2 | LM | Test | 0.6086033 | 351.92616 | 0.8840623 | 108.16170 | 0.7556820 | 0.1246836 |
| TidyDensity | 3 | EARTH | Test | 0.5562176 | 234.21776 | 0.8079664 | 115.25892 | 0.6839964 | 0.1246836 |
| TidyDensity | 4 | NNAR | Test | 0.5385790 | 127.59121 | 0.7823444 | 134.42719 | 0.6856795 | 0.1247126 |
| tidyAML | 1 | ARIMA | Test | 0.6540689 | 175.67296 | 0.9289369 | 95.64856 | 0.8013879 | 0.0001159 |
| tidyAML | 2 | LM | Test | 0.6610650 | 181.79919 | 0.9388731 | 96.36910 | 0.7982537 | 0.1802817 |
| tidyAML | 3 | EARTH | Test | 0.6700037 | 131.46054 | 0.9515683 | 108.90843 | 0.8186271 | 0.1802817 |
| tidyAML | 4 | NNAR | Test | 0.6510156 | 171.78501 | 0.9246005 | 96.86904 | 0.7892289 | 0.0184459 |
| RandomWalker | 1 | ARIMA | Test | 1.3879265 | 156.68567 | 0.6536210 | 156.82156 | 1.6865737 | 0.1920304 |
| RandomWalker | 2 | LM | Test | 1.2237148 | 116.26229 | 0.5762882 | 163.98826 | 1.4597096 | 0.0399288 |
| RandomWalker | 3 | EARTH | Test | 1.2263948 | 113.44795 | 0.5775503 | 169.69856 | 1.4525992 | NA |
| RandomWalker | 4 | NNAR | Test | 1.3362037 | 171.07348 | 0.6292630 | 164.59907 | 1.5528903 | 0.0088337 |

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
    ## 1 healthyR.d…         2 LM          Test  0.729 169.  0.653 127.  0.897  0.0224 
    ## 2 healthyR            3 EARTH       Test  0.618  95.2 0.716 141.  0.764  0.135  
    ## 3 healthyR.ts         1 ARIMA       Test  0.792 102.  0.688 174.  0.977  0.0881 
    ## 4 healthyver…         4 NNAR        Test  0.583 211.  0.798 106.  0.742  0.144  
    ## 5 healthyR.ai         1 ARIMA       Test  0.779 124.  0.834 167.  1.03   0.0362 
    ## 6 TidyDensity         1 ARIMA       Test  0.557 233.  0.809 119.  0.682  0.00284
    ## 7 tidyAML             4 NNAR        Test  0.651 172.  0.925  96.9 0.789  0.0184 
    ## 8 RandomWalk…         3 EARTH       Test  1.23  113.  0.578 170.  1.45  NA

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
    ## 1 healthyR.data <tibble>     <tibble>     <split [1591|28]> <mdl_tm_t [1 × 5]>
    ## 2 healthyR      <tibble>     <tibble>     <split [1584|28]> <mdl_tm_t [1 × 5]>
    ## 3 healthyR.ts   <tibble>     <tibble>     <split [1528|28]> <mdl_tm_t [1 × 5]>
    ## 4 healthyverse  <tibble>     <tibble>     <split [1498|28]> <mdl_tm_t [1 × 5]>
    ## 5 healthyR.ai   <tibble>     <tibble>     <split [1323|28]> <mdl_tm_t [1 × 5]>
    ## 6 TidyDensity   <tibble>     <tibble>     <split [1174|28]> <mdl_tm_t [1 × 5]>
    ## 7 tidyAML       <tibble>     <tibble>     <split [782|28]>  <mdl_tm_t [1 × 5]>
    ## 8 RandomWalker  <tibble>     <tibble>     <split [204|28]>  <mdl_tm_t [1 × 5]>

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
