Time Series Analysis and Nested Modeling of the Healthyverse Packages
================
Steven P. Sanderson II, MPH - Date:
03 June, 2025

This analysis follows a *Nested Modeltime Workflow*.

## Get Data

``` r
glimpse(downloads_tbl)
```

    ## Rows: 141,427
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

The last day in the data set is 2025-06-01 21:31:25, the file was
birthed on: 2024-08-07 07:35:44, and at report knit time is -7161.93
hours old. Happy analyzing!

Now that we have our data lets take a look at it using the `skimr`
package.

``` r
skim(downloads_tbl)
```

|                                                  |               |
|:-------------------------------------------------|:--------------|
| Name                                             | downloads_tbl |
| Number of rows                                   | 141427        |
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
| r_version     |    102015 |          0.28 |   5 |   5 |     0 |       47 |          0 |
| r_arch        |    102015 |          0.28 |   3 |   7 |     0 |        5 |          0 |
| r_os          |    102015 |          0.28 |   7 |  15 |     0 |       22 |          0 |
| package       |         0 |          1.00 |   7 |  13 |     0 |        8 |          0 |
| version       |         0 |          1.00 |   5 |  17 |     0 |       60 |          0 |
| country       |     12012 |          0.92 |   2 |   2 |     0 |      163 |          0 |

**Variable type: Date**

| skim_variable | n_missing | complete_rate | min | max | median | n_unique |
|:---|---:|---:|:---|:---|:---|---:|
| date | 0 | 1 | 2020-11-23 | 2025-06-01 | 2023-06-28 | 1652 |

**Variable type: numeric**

| skim_variable | n_missing | complete_rate | mean | sd | p0 | p25 | p50 | p75 | p100 | hist |
|:---|---:|---:|---:|---:|---:|---:|---:|---:|---:|:---|
| size | 0 | 1 | 1133079.98 | 1517361.46 | 355 | 14701 | 289681 | 2367727 | 5677952 | ▇▁▂▁▁ |
| ip_id | 0 | 1 | 10425.41 | 18571.57 | 1 | 279 | 3039 | 11662 | 209747 | ▇▁▁▁▁ |

**Variable type: POSIXct**

| skim_variable | n_missing | complete_rate | min | max | median | n_unique |
|:---|---:|---:|:---|:---|:---|---:|
| date_time | 0 | 1 | 2020-11-23 09:00:41 | 2025-06-01 21:31:25 | 2023-06-28 19:54:18 | 86563 |

**Variable type: Timespan**

| skim_variable | n_missing | complete_rate | min | max |     median | n_unique |
|:--------------|----------:|--------------:|----:|----:|-----------:|---------:|
| time          |         0 |             1 |   0 |  59 | 12H 6M 49S |       60 |

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
    ## -148.12  -35.79  -10.84   26.87  814.51 
    ## 
    ## Coefficients:
    ##                                                      Estimate Std. Error
    ## (Intercept)                                        -1.617e+02  6.864e+01
    ## date                                                1.006e-02  3.637e-03
    ## lag(value, 1)                                       1.046e-01  2.438e-02
    ## lag(value, 7)                                       9.595e-02  2.526e-02
    ## lag(value, 14)                                      8.980e-02  2.525e-02
    ## lag(value, 21)                                      6.966e-02  2.533e-02
    ## lag(value, 28)                                      7.228e-02  2.525e-02
    ## lag(value, 35)                                      6.494e-02  2.534e-02
    ## lag(value, 42)                                      4.860e-02  2.546e-02
    ## lag(value, 49)                                      7.029e-02  2.532e-02
    ## month(date, label = TRUE).L                        -9.623e+00  5.110e+00
    ## month(date, label = TRUE).Q                         3.813e+00  5.133e+00
    ## month(date, label = TRUE).C                        -1.341e+01  5.134e+00
    ## month(date, label = TRUE)^4                        -7.049e+00  5.179e+00
    ## month(date, label = TRUE)^5                        -1.107e+01  5.120e+00
    ## month(date, label = TRUE)^6                        -3.493e+00  5.227e+00
    ## month(date, label = TRUE)^7                        -7.443e+00  5.100e+00
    ## month(date, label = TRUE)^8                        -3.294e+00  5.108e+00
    ## month(date, label = TRUE)^9                         5.770e+00  5.122e+00
    ## month(date, label = TRUE)^10                        2.832e+00  5.088e+00
    ## month(date, label = TRUE)^11                       -4.370e+00  5.234e+00
    ## fourier_vec(date, type = "sin", K = 1, period = 7) -1.163e+01  2.336e+00
    ## fourier_vec(date, type = "cos", K = 1, period = 7)  7.940e+00  2.458e+00
    ##                                                    t value Pr(>|t|)    
    ## (Intercept)                                         -2.356 0.018576 *  
    ## date                                                 2.765 0.005755 ** 
    ## lag(value, 1)                                        4.290 1.90e-05 ***
    ## lag(value, 7)                                        3.799 0.000151 ***
    ## lag(value, 14)                                       3.557 0.000387 ***
    ## lag(value, 21)                                       2.751 0.006017 ** 
    ## lag(value, 28)                                       2.863 0.004255 ** 
    ## lag(value, 35)                                       2.563 0.010466 *  
    ## lag(value, 42)                                       1.909 0.056439 .  
    ## lag(value, 49)                                       2.776 0.005572 ** 
    ## month(date, label = TRUE).L                         -1.883 0.059870 .  
    ## month(date, label = TRUE).Q                          0.743 0.457705    
    ## month(date, label = TRUE).C                         -2.612 0.009082 ** 
    ## month(date, label = TRUE)^4                         -1.361 0.173706    
    ## month(date, label = TRUE)^5                         -2.162 0.030745 *  
    ## month(date, label = TRUE)^6                         -0.668 0.504009    
    ## month(date, label = TRUE)^7                         -1.459 0.144682    
    ## month(date, label = TRUE)^8                         -0.645 0.519070    
    ## month(date, label = TRUE)^9                          1.127 0.260109    
    ## month(date, label = TRUE)^10                         0.557 0.577912    
    ## month(date, label = TRUE)^11                        -0.835 0.403860    
    ## fourier_vec(date, type = "sin", K = 1, period = 7)  -4.980 7.06e-07 ***
    ## fourier_vec(date, type = "cos", K = 1, period = 7)   3.231 0.001259 ** 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 58.73 on 1580 degrees of freedom
    ##   (49 observations deleted due to missingness)
    ## Multiple R-squared:   0.24,  Adjusted R-squared:  0.2295 
    ## F-statistic: 22.68 on 22 and 1580 DF,  p-value: < 2.2e-16

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
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 3.00776832930125"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 63, 28 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 2.47323172665043"
    ## [1] "BEST method = 'lin', seasonal.factor = c( 63, 28 )"
    ## [1] "BEST lin OBJECTIVE FUNCTION = 2.47323172665043"
    ## [1] "CURRNET METHOD: nonlin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'nonlin' , seasonal.factor =  c( 63, 28 ) ...)"
    ## [1] "CURRENT nonlin OBJECTIVE FUNCTION = 3.4754583304744"
    ## [1] "BEST method = 'nonlin' PATH MEMBER = c( 63, 28 )"
    ## [1] "BEST nonlin OBJECTIVE FUNCTION = 3.4754583304744"
    ## [1] "CURRNET METHOD: both"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'both' , seasonal.factor =  c( 63, 28 ) ...)"
    ## [1] "CURRENT both OBJECTIVE FUNCTION = 2.63491463104735"
    ## [1] "BEST method = 'both' PATH MEMBER = c( 63, 28 )"
    ## [1] "BEST both OBJECTIVE FUNCTION = 2.63491463104735"

![](man/figures/README-nns_forecasting-1.png)<!-- -->

    ## Package: healthyR.ai
    ## [1] "CURRNET METHOD: lin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 63 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 4.02417889041498"
    ## [1] "BEST method = 'lin', seasonal.factor = c( 63 )"
    ## [1] "BEST lin OBJECTIVE FUNCTION = 4.02417889041498"
    ## [1] "CURRNET METHOD: nonlin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'nonlin' , seasonal.factor =  c( 63 ) ...)"
    ## [1] "CURRENT nonlin OBJECTIVE FUNCTION = 4.17841782533112"
    ## [1] "BEST method = 'nonlin' PATH MEMBER = c( 63 )"
    ## [1] "BEST nonlin OBJECTIVE FUNCTION = 4.17841782533112"
    ## [1] "CURRNET METHOD: both"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'both' , seasonal.factor =  c( 63 ) ...)"
    ## [1] "CURRENT both OBJECTIVE FUNCTION = 3.90635911943102"
    ## [1] "BEST method = 'both' PATH MEMBER = c( 63 )"
    ## [1] "BEST both OBJECTIVE FUNCTION = 3.90635911943102"

![](man/figures/README-nns_forecasting-2.png)<!-- -->

    ## Package: healthyR.data
    ## [1] "CURRNET METHOD: lin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 63 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 2.5225773457626"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 63, 91 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 2.08869183610393"
    ## [1] "BEST method = 'lin', seasonal.factor = c( 63, 91 )"
    ## [1] "BEST lin OBJECTIVE FUNCTION = 2.08869183610393"
    ## [1] "CURRNET METHOD: nonlin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'nonlin' , seasonal.factor =  c( 63, 91 ) ...)"
    ## [1] "CURRENT nonlin OBJECTIVE FUNCTION = 2.19062540399925"
    ## [1] "BEST method = 'nonlin' PATH MEMBER = c( 63, 91 )"
    ## [1] "BEST nonlin OBJECTIVE FUNCTION = 2.19062540399925"
    ## [1] "CURRNET METHOD: both"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'both' , seasonal.factor =  c( 63, 91 ) ...)"
    ## [1] "CURRENT both OBJECTIVE FUNCTION = 1.99530519404211"
    ## [1] "BEST method = 'both' PATH MEMBER = c( 63, 91 )"
    ## [1] "BEST both OBJECTIVE FUNCTION = 1.99530519404211"

![](man/figures/README-nns_forecasting-3.png)<!-- -->

    ## Package: healthyR.ts
    ## [1] "CURRNET METHOD: lin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 21 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 3.45147362635842"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 21, 63 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 2.98254441388528"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 21, 63, 28 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 2.86116800426052"
    ## [1] "BEST method = 'lin', seasonal.factor = c( 21, 63, 28 )"
    ## [1] "BEST lin OBJECTIVE FUNCTION = 2.86116800426052"
    ## [1] "CURRNET METHOD: nonlin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'nonlin' , seasonal.factor =  c( 21, 63, 28 ) ...)"
    ## [1] "CURRENT nonlin OBJECTIVE FUNCTION = 2.54705174873153"
    ## [1] "BEST method = 'nonlin' PATH MEMBER = c( 21, 63, 28 )"
    ## [1] "BEST nonlin OBJECTIVE FUNCTION = 2.54705174873153"
    ## [1] "CURRNET METHOD: both"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'both' , seasonal.factor =  c( 21, 63, 28 ) ...)"
    ## [1] "CURRENT both OBJECTIVE FUNCTION = 2.24286889604247"
    ## [1] "BEST method = 'both' PATH MEMBER = c( 21, 63, 28 )"
    ## [1] "BEST both OBJECTIVE FUNCTION = 2.24286889604247"

![](man/figures/README-nns_forecasting-4.png)<!-- -->

    ## Package: healthyverse
    ## [1] "CURRNET METHOD: lin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 56 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 6.03595141944272"
    ## [1] "BEST method = 'lin', seasonal.factor = c( 56 )"
    ## [1] "BEST lin OBJECTIVE FUNCTION = 6.03595141944272"
    ## [1] "CURRNET METHOD: nonlin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'nonlin' , seasonal.factor =  c( 56 ) ...)"
    ## [1] "CURRENT nonlin OBJECTIVE FUNCTION = 9.249826931616"
    ## [1] "BEST method = 'nonlin' PATH MEMBER = c( 56 )"
    ## [1] "BEST nonlin OBJECTIVE FUNCTION = 9.249826931616"
    ## [1] "CURRNET METHOD: both"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'both' , seasonal.factor =  c( 56 ) ...)"
    ## [1] "CURRENT both OBJECTIVE FUNCTION = 7.47200369542842"
    ## [1] "BEST method = 'both' PATH MEMBER = c( 56 )"
    ## [1] "BEST both OBJECTIVE FUNCTION = 7.47200369542842"

![](man/figures/README-nns_forecasting-5.png)<!-- -->

    ## Package: RandomWalker
    ## [1] "CURRNET METHOD: lin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 63 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 2.31237175867331"
    ## [1] "BEST method = 'lin', seasonal.factor = c( 63 )"
    ## [1] "BEST lin OBJECTIVE FUNCTION = 2.31237175867331"
    ## [1] "CURRNET METHOD: nonlin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'nonlin' , seasonal.factor =  c( 63 ) ...)"
    ## [1] "CURRENT nonlin OBJECTIVE FUNCTION = 2.50597521722252"
    ## [1] "BEST method = 'nonlin' PATH MEMBER = c( 63 )"
    ## [1] "BEST nonlin OBJECTIVE FUNCTION = 2.50597521722252"
    ## [1] "CURRNET METHOD: both"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'both' , seasonal.factor =  c( 63 ) ...)"
    ## [1] "CURRENT both OBJECTIVE FUNCTION = 2.26187260519319"
    ## [1] "BEST method = 'both' PATH MEMBER = c( 63 )"
    ## [1] "BEST both OBJECTIVE FUNCTION = 2.26187260519319"

![](man/figures/README-nns_forecasting-6.png)<!-- -->

    ## Package: tidyAML
    ## [1] "CURRNET METHOD: lin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 28 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 6.46459631849741"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 28, 84 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 6.12246228804219"
    ## [1] "BEST method = 'lin', seasonal.factor = c( 28, 84 )"
    ## [1] "BEST lin OBJECTIVE FUNCTION = 6.12246228804219"
    ## [1] "CURRNET METHOD: nonlin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'nonlin' , seasonal.factor =  c( 28, 84 ) ...)"
    ## [1] "CURRENT nonlin OBJECTIVE FUNCTION = 3.58201709380893"
    ## [1] "BEST method = 'nonlin' PATH MEMBER = c( 28, 84 )"
    ## [1] "BEST nonlin OBJECTIVE FUNCTION = 3.58201709380893"
    ## [1] "CURRNET METHOD: both"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'both' , seasonal.factor =  c( 28, 84 ) ...)"
    ## [1] "CURRENT both OBJECTIVE FUNCTION = 4.4987140773321"
    ## [1] "BEST method = 'both' PATH MEMBER = c( 28, 84 )"
    ## [1] "BEST both OBJECTIVE FUNCTION = 4.4987140773321"

![](man/figures/README-nns_forecasting-7.png)<!-- -->

    ## Package: TidyDensity
    ## [1] "CURRNET METHOD: lin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 56 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 5.82497272286287"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 56, 84 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 5.52255790893374"
    ## [1] "BEST method = 'lin', seasonal.factor = c( 56, 84 )"
    ## [1] "BEST lin OBJECTIVE FUNCTION = 5.52255790893374"
    ## [1] "CURRNET METHOD: nonlin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'nonlin' , seasonal.factor =  c( 56, 84 ) ...)"
    ## [1] "CURRENT nonlin OBJECTIVE FUNCTION = 5.58518699637577"
    ## [1] "BEST method = 'nonlin' PATH MEMBER = c( 56, 84 )"
    ## [1] "BEST nonlin OBJECTIVE FUNCTION = 5.58518699637577"
    ## [1] "CURRNET METHOD: both"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'both' , seasonal.factor =  c( 56, 84 ) ...)"
    ## [1] "CURRENT both OBJECTIVE FUNCTION = 4.91716228632952"
    ## [1] "BEST method = 'both' PATH MEMBER = c( 56, 84 )"
    ## [1] "BEST both OBJECTIVE FUNCTION = 4.91716228632952"

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
    ## 1 healthyR.data <tibble [1,644 × 2]> <tibble [28 × 2]> <split [1616|28]>
    ## 2 healthyR      <tibble [1,638 × 2]> <tibble [28 × 2]> <split [1610|28]>
    ## 3 healthyR.ts   <tibble [1,582 × 2]> <tibble [28 × 2]> <split [1554|28]>
    ## 4 healthyverse  <tibble [1,552 × 2]> <tibble [28 × 2]> <split [1524|28]>
    ## 5 healthyR.ai   <tibble [1,377 × 2]> <tibble [28 × 2]> <split [1349|28]>
    ## 6 TidyDensity   <tibble [1,228 × 2]> <tibble [28 × 2]> <split [1200|28]>
    ## 7 tidyAML       <tibble [836 × 2]>   <tibble [28 × 2]> <split [808|28]> 
    ## 8 RandomWalker  <tibble [258 × 2]>   <tibble [28 × 2]> <split [230|28]>

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
| healthyR.data | 1 | ARIMA | Test | 0.6721072 | 182.17415 | 0.7113398 | 121.20390 | 0.8802311 | 0.0865582 |
| healthyR.data | 2 | LM | Test | 0.6591613 | 181.10386 | 0.6976382 | 120.96489 | 0.8487976 | 0.0984508 |
| healthyR.data | 3 | EARTH | Test | 0.7049609 | 285.08110 | 0.7461112 | 108.04466 | 0.9131905 | 0.0984508 |
| healthyR.data | 4 | NNAR | Test | 0.7242812 | 95.81943 | 0.7665593 | 174.31931 | 0.9327900 | 0.1226836 |
| healthyR | 1 | ARIMA | Test | 0.7172500 | 122.36598 | 0.7542271 | 164.03595 | 0.9122983 | 0.1840336 |
| healthyR | 2 | LM | Test | 0.6878809 | 96.26482 | 0.7233440 | 165.09432 | 0.8785675 | 0.0572334 |
| healthyR | 3 | EARTH | Test | 0.6547551 | 129.11351 | 0.6885104 | 136.96293 | 0.8187892 | 0.0572334 |
| healthyR | 4 | NNAR | Test | 0.6740320 | 99.50510 | 0.7087812 | 148.40338 | 0.8623016 | 0.1669709 |
| healthyR.ts | 1 | ARIMA | Test | 0.6379246 | 117.83419 | 0.6868660 | 141.43557 | 0.8474747 | 0.0687558 |
| healthyR.ts | 2 | LM | Test | 0.9060414 | 216.68076 | 0.9755527 | 158.08561 | 1.1230721 | 0.0687558 |
| healthyR.ts | 3 | EARTH | Test | 0.6574477 | 258.85790 | 0.7078869 | 104.94940 | 0.8486275 | 0.0687558 |
| healthyR.ts | 4 | NNAR | Test | 0.7041295 | 103.10439 | 0.7581502 | 180.46273 | 0.9118932 | 0.0102275 |
| healthyverse | 1 | ARIMA | Test | 0.6075783 | 156.99623 | 0.9738217 | 82.14050 | 0.7753288 | 0.0250532 |
| healthyverse | 2 | LM | Test | 0.5857884 | 170.50054 | 0.9388971 | 78.63027 | 0.7142747 | 0.0615170 |
| healthyverse | 3 | EARTH | Test | 0.5804103 | 196.21266 | 0.9302772 | 75.97333 | 0.6885832 | 0.0615170 |
| healthyverse | 4 | NNAR | Test | 0.6552494 | 117.41624 | 1.0502286 | 94.97371 | 0.8377555 | 0.0004020 |
| healthyR.ai | 1 | ARIMA | Test | 0.5756222 | 88.29908 | 0.8523944 | 161.25310 | 0.7461530 | 0.0112221 |
| healthyR.ai | 2 | LM | Test | 0.5193802 | 89.72174 | 0.7691099 | 119.81051 | 0.6927557 | 0.0908705 |
| healthyR.ai | 3 | EARTH | Test | 1.8017707 | 607.43862 | 2.6681027 | 188.69533 | 1.9270845 | 0.0908705 |
| healthyR.ai | 4 | NNAR | Test | 0.5090407 | 81.77597 | 0.7537989 | 112.77205 | 0.6860425 | 0.0068174 |
| TidyDensity | 1 | ARIMA | Test | 0.4541616 | 165.57040 | 0.9299013 | 112.88952 | 0.5921961 | 0.1506104 |
| TidyDensity | 2 | LM | Test | 0.6033214 | 290.44879 | 1.2353077 | 116.15047 | 0.7583579 | 0.1791627 |
| TidyDensity | 3 | EARTH | Test | 0.4421572 | 174.54462 | 0.9053221 | 108.41011 | 0.5811199 | 0.1791627 |
| TidyDensity | 4 | NNAR | Test | 0.4149217 | 114.63453 | 0.8495571 | 128.89111 | 0.5156546 | 0.0629123 |
| tidyAML | 1 | ARIMA | Test | 0.8636182 | 150.70215 | 0.8768978 | 108.77728 | 1.1488902 | 0.0423294 |
| tidyAML | 2 | LM | Test | 0.8324724 | 136.08912 | 0.8452732 | 108.87311 | 1.1156440 | 0.0335527 |
| tidyAML | 3 | EARTH | Test | 3.2474788 | 650.11493 | 3.2974146 | 153.97864 | 3.5068963 | 0.0335527 |
| tidyAML | 4 | NNAR | Test | 0.8321049 | 145.45295 | 0.8449000 | 109.12871 | 1.1097062 | 0.0062963 |
| RandomWalker | 1 | ARIMA | Test | 1.1079743 | 96.15797 | 0.5910559 | 156.67534 | 1.3441923 | 0.1474939 |
| RandomWalker | 2 | LM | Test | 1.1893423 | 99.85156 | 0.6344622 | 197.75854 | 1.4179012 | 0.0434187 |
| RandomWalker | 3 | EARTH | Test | 1.1749571 | 103.48727 | 0.6267883 | 173.00329 | 1.4310575 | NA |
| RandomWalker | 4 | NNAR | Test | 1.3263170 | 135.85099 | 0.7075322 | 158.77814 | 1.4960477 | 0.0051623 |

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
    ## 1 healthyR.da…         2 LM          Test  0.659 181.  0.698 121.  0.849 0.0985 
    ## 2 healthyR             3 EARTH       Test  0.655 129.  0.689 137.  0.819 0.0572 
    ## 3 healthyR.ts          1 ARIMA       Test  0.638 118.  0.687 141.  0.847 0.0688 
    ## 4 healthyverse         3 EARTH       Test  0.580 196.  0.930  76.0 0.689 0.0615 
    ## 5 healthyR.ai          4 NNAR        Test  0.509  81.8 0.754 113.  0.686 0.00682
    ## 6 TidyDensity          4 NNAR        Test  0.415 115.  0.850 129.  0.516 0.0629 
    ## 7 tidyAML              4 NNAR        Test  0.832 145.  0.845 109.  1.11  0.00630
    ## 8 RandomWalker         1 ARIMA       Test  1.11   96.2 0.591 157.  1.34  0.147

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
    ## 1 healthyR.data <tibble>     <tibble>     <split [1616|28]> <mdl_tm_t [1 × 5]>
    ## 2 healthyR      <tibble>     <tibble>     <split [1610|28]> <mdl_tm_t [1 × 5]>
    ## 3 healthyR.ts   <tibble>     <tibble>     <split [1554|28]> <mdl_tm_t [1 × 5]>
    ## 4 healthyverse  <tibble>     <tibble>     <split [1524|28]> <mdl_tm_t [1 × 5]>
    ## 5 healthyR.ai   <tibble>     <tibble>     <split [1349|28]> <mdl_tm_t [1 × 5]>
    ## 6 TidyDensity   <tibble>     <tibble>     <split [1200|28]> <mdl_tm_t [1 × 5]>
    ## 7 tidyAML       <tibble>     <tibble>     <split [808|28]>  <mdl_tm_t [1 × 5]>
    ## 8 RandomWalker  <tibble>     <tibble>     <split [230|28]>  <mdl_tm_t [1 × 5]>

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
