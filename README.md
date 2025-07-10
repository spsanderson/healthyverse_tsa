Time Series Analysis and Nested Modeling of the Healthyverse Packages
================
Steven P. Sanderson II, MPH - Date:
10 July, 2025

This analysis follows a *Nested Modeltime Workflow*.

## Get Data

``` r
glimpse(downloads_tbl)
```

    ## Rows: 145,015
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

The last day in the data set is 2025-07-08 23:29:38, the file was
birthed on: 2024-08-07 07:35:44.428716, and at report knit time is
-8051.9 hours old. Happy analyzing!

Now that we have our data lets take a look at it using the `skimr`
package.

``` r
skim(downloads_tbl)
```

|                                                  |               |
|:-------------------------------------------------|:--------------|
| Name                                             | downloads_tbl |
| Number of rows                                   | 145015        |
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
| r_version     |    104851 |          0.28 |   5 |   5 |     0 |       48 |          0 |
| r_arch        |    104851 |          0.28 |   3 |   7 |     0 |        5 |          0 |
| r_os          |    104851 |          0.28 |   7 |  15 |     0 |       23 |          0 |
| package       |         0 |          1.00 |   7 |  13 |     0 |        8 |          0 |
| version       |         0 |          1.00 |   5 |  17 |     0 |       60 |          0 |
| country       |     12246 |          0.92 |   2 |   2 |     0 |      163 |          0 |

**Variable type: Date**

| skim_variable | n_missing | complete_rate | min | max | median | n_unique |
|:---|---:|---:|:---|:---|:---|---:|
| date | 0 | 1 | 2020-11-23 | 2025-07-08 | 2023-07-19 | 1689 |

**Variable type: numeric**

| skim_variable | n_missing | complete_rate | mean | sd | p0 | p25 | p50 | p75 | p100 | hist |
|:---|---:|---:|---:|---:|---:|---:|---:|---:|---:|:---|
| size | 0 | 1 | 1131353.95 | 1513041.56 | 355 | 14701 | 293136 | 2367671 | 5677952 | ▇▁▂▁▁ |
| ip_id | 0 | 1 | 10462.61 | 18598.26 | 1 | 287 | 3036 | 11830 | 209747 | ▇▁▁▁▁ |

**Variable type: POSIXct**

| skim_variable | n_missing | complete_rate | min | max | median | n_unique |
|:---|---:|---:|:---|:---|:---|---:|
| date_time | 0 | 1 | 2020-11-23 09:00:41 | 2025-07-08 23:29:38 | 2023-07-19 19:29:22 | 89067 |

**Variable type: Timespan**

| skim_variable | n_missing | complete_rate | min | max |     median | n_unique |
|:--------------|----------:|--------------:|----:|----:|-----------:|---------:|
| time          |         0 |             1 |   0 |  59 | 12H 6M 12S |       60 |

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
    ## -148.68  -36.00  -11.21   26.82  816.05 
    ## 
    ## Coefficients:
    ##                                                      Estimate Std. Error
    ## (Intercept)                                        -1.738e+02  6.575e+01
    ## date                                                1.071e-02  3.481e-03
    ## lag(value, 1)                                       1.043e-01  2.410e-02
    ## lag(value, 7)                                       9.481e-02  2.493e-02
    ## lag(value, 14)                                      8.621e-02  2.493e-02
    ## lag(value, 21)                                      6.636e-02  2.508e-02
    ## lag(value, 28)                                      7.012e-02  2.497e-02
    ## lag(value, 35)                                      6.733e-02  2.507e-02
    ## lag(value, 42)                                      5.599e-02  2.517e-02
    ## lag(value, 49)                                      6.555e-02  2.506e-02
    ## month(date, label = TRUE).L                        -9.725e+00  5.108e+00
    ## month(date, label = TRUE).Q                         3.283e+00  5.054e+00
    ## month(date, label = TRUE).C                        -1.332e+01  5.124e+00
    ## month(date, label = TRUE)^4                        -6.723e+00  5.110e+00
    ## month(date, label = TRUE)^5                        -1.125e+01  5.103e+00
    ## month(date, label = TRUE)^6                        -4.136e+00  5.160e+00
    ## month(date, label = TRUE)^7                        -7.165e+00  5.063e+00
    ## month(date, label = TRUE)^8                        -2.933e+00  5.051e+00
    ## month(date, label = TRUE)^9                         5.360e+00  5.041e+00
    ## month(date, label = TRUE)^10                        2.561e+00  5.048e+00
    ## month(date, label = TRUE)^11                       -3.825e+00  5.017e+00
    ## fourier_vec(date, type = "sin", K = 1, period = 7) -1.186e+01  2.305e+00
    ## fourier_vec(date, type = "cos", K = 1, period = 7)  8.122e+00  2.426e+00
    ##                                                    t value Pr(>|t|)    
    ## (Intercept)                                         -2.644 0.008279 ** 
    ## date                                                 3.077 0.002128 ** 
    ## lag(value, 1)                                        4.326 1.61e-05 ***
    ## lag(value, 7)                                        3.802 0.000149 ***
    ## lag(value, 14)                                       3.459 0.000557 ***
    ## lag(value, 21)                                       2.646 0.008213 ** 
    ## lag(value, 28)                                       2.808 0.005039 ** 
    ## lag(value, 35)                                       2.686 0.007298 ** 
    ## lag(value, 42)                                       2.224 0.026284 *  
    ## lag(value, 49)                                       2.616 0.008975 ** 
    ## month(date, label = TRUE).L                         -1.904 0.057102 .  
    ## month(date, label = TRUE).Q                          0.650 0.516024    
    ## month(date, label = TRUE).C                         -2.600 0.009407 ** 
    ## month(date, label = TRUE)^4                         -1.316 0.188423    
    ## month(date, label = TRUE)^5                         -2.205 0.027588 *  
    ## month(date, label = TRUE)^6                         -0.802 0.422941    
    ## month(date, label = TRUE)^7                         -1.415 0.157237    
    ## month(date, label = TRUE)^8                         -0.581 0.561490    
    ## month(date, label = TRUE)^9                          1.063 0.287819    
    ## month(date, label = TRUE)^10                         0.507 0.611999    
    ## month(date, label = TRUE)^11                        -0.762 0.445881    
    ## fourier_vec(date, type = "sin", K = 1, period = 7)  -5.146 2.99e-07 ***
    ## fourier_vec(date, type = "cos", K = 1, period = 7)   3.347 0.000835 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 58.72 on 1617 degrees of freedom
    ##   (49 observations deleted due to missingness)
    ## Multiple R-squared:  0.2382, Adjusted R-squared:  0.2278 
    ## F-statistic: 22.98 on 22 and 1617 DF,  p-value: < 2.2e-16

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
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 4.00256606002294"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 21, 63 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 3.91993594447702"
    ## [1] "BEST method = 'lin', seasonal.factor = c( 21, 63 )"
    ## [1] "BEST lin OBJECTIVE FUNCTION = 3.91993594447702"
    ## [1] "CURRNET METHOD: nonlin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'nonlin' , seasonal.factor =  c( 21, 63 ) ...)"
    ## [1] "CURRENT nonlin OBJECTIVE FUNCTION = 5.54189320162322"
    ## [1] "BEST method = 'nonlin' PATH MEMBER = c( 21, 63 )"
    ## [1] "BEST nonlin OBJECTIVE FUNCTION = 5.54189320162322"
    ## [1] "CURRNET METHOD: both"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'both' , seasonal.factor =  c( 21, 63 ) ...)"
    ## [1] "CURRENT both OBJECTIVE FUNCTION = 4.87142617836505"
    ## [1] "BEST method = 'both' PATH MEMBER = c( 21, 63 )"
    ## [1] "BEST both OBJECTIVE FUNCTION = 4.87142617836505"

![](man/figures/README-nns_forecasting-1.png)<!-- -->

    ## Package: healthyR.ai
    ## [1] "CURRNET METHOD: lin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 42 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 4.36361562954491"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 42, 49 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 3.53347109355114"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 42, 49, 63 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 2.8026603667615"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 42, 49, 63, 98 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 2.65992876526431"
    ## [1] "BEST method = 'lin', seasonal.factor = c( 42, 49, 63, 98 )"
    ## [1] "BEST lin OBJECTIVE FUNCTION = 2.65992876526431"
    ## [1] "CURRNET METHOD: nonlin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'nonlin' , seasonal.factor =  c( 42, 49, 63, 98 ) ...)"
    ## [1] "CURRENT nonlin OBJECTIVE FUNCTION = 9.63029600489083"
    ## [1] "BEST method = 'nonlin' PATH MEMBER = c( 42, 49, 63, 98 )"
    ## [1] "BEST nonlin OBJECTIVE FUNCTION = 9.63029600489083"
    ## [1] "CURRNET METHOD: both"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'both' , seasonal.factor =  c( 42, 49, 63, 98 ) ...)"
    ## [1] "CURRENT both OBJECTIVE FUNCTION = 4.87995147319659"
    ## [1] "BEST method = 'both' PATH MEMBER = c( 42, 49, 63, 98 )"
    ## [1] "BEST both OBJECTIVE FUNCTION = 4.87995147319659"

![](man/figures/README-nns_forecasting-2.png)<!-- -->

    ## Package: healthyR.data
    ## [1] "CURRNET METHOD: lin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 77 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 3.1206656608795"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 77, 42 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 2.68731087135495"
    ## [1] "BEST method = 'lin', seasonal.factor = c( 77, 42 )"
    ## [1] "BEST lin OBJECTIVE FUNCTION = 2.68731087135495"
    ## [1] "CURRNET METHOD: nonlin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'nonlin' , seasonal.factor =  c( 77, 42 ) ...)"
    ## [1] "CURRENT nonlin OBJECTIVE FUNCTION = 2.50840908327481"
    ## [1] "BEST method = 'nonlin' PATH MEMBER = c( 77, 42 )"
    ## [1] "BEST nonlin OBJECTIVE FUNCTION = 2.50840908327481"
    ## [1] "CURRNET METHOD: both"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'both' , seasonal.factor =  c( 77, 42 ) ...)"
    ## [1] "CURRENT both OBJECTIVE FUNCTION = 2.37754717780983"
    ## [1] "BEST method = 'both' PATH MEMBER = c( 77, 42 )"
    ## [1] "BEST both OBJECTIVE FUNCTION = 2.37754717780983"

![](man/figures/README-nns_forecasting-3.png)<!-- -->

    ## Package: healthyR.ts
    ## [1] "CURRNET METHOD: lin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 63 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 2.18233302441767"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 63, 49 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 2.1375986752421"
    ## [1] "BEST method = 'lin', seasonal.factor = c( 63, 49 )"
    ## [1] "BEST lin OBJECTIVE FUNCTION = 2.1375986752421"
    ## [1] "CURRNET METHOD: nonlin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'nonlin' , seasonal.factor =  c( 63, 49 ) ...)"
    ## [1] "CURRENT nonlin OBJECTIVE FUNCTION = 14.2339301991804"
    ## [1] "BEST method = 'nonlin' PATH MEMBER = c( 63, 49 )"
    ## [1] "BEST nonlin OBJECTIVE FUNCTION = 14.2339301991804"
    ## [1] "CURRNET METHOD: both"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'both' , seasonal.factor =  c( 63, 49 ) ...)"
    ## [1] "CURRENT both OBJECTIVE FUNCTION = 5.62717133960672"
    ## [1] "BEST method = 'both' PATH MEMBER = c( 63, 49 )"
    ## [1] "BEST both OBJECTIVE FUNCTION = 5.62717133960672"

![](man/figures/README-nns_forecasting-4.png)<!-- -->

    ## Package: healthyverse
    ## [1] "CURRNET METHOD: lin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 77 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 4.40572305802886"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 77, 63 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 3.49823853587716"
    ## [1] "BEST method = 'lin', seasonal.factor = c( 77, 63 )"
    ## [1] "BEST lin OBJECTIVE FUNCTION = 3.49823853587716"
    ## [1] "CURRNET METHOD: nonlin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'nonlin' , seasonal.factor =  c( 77, 63 ) ...)"
    ## [1] "CURRENT nonlin OBJECTIVE FUNCTION = 4.76546542852529"
    ## [1] "BEST method = 'nonlin' PATH MEMBER = c( 77, 63 )"
    ## [1] "BEST nonlin OBJECTIVE FUNCTION = 4.76546542852529"
    ## [1] "CURRNET METHOD: both"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'both' , seasonal.factor =  c( 77, 63 ) ...)"
    ## [1] "CURRENT both OBJECTIVE FUNCTION = 4.10509623106111"
    ## [1] "BEST method = 'both' PATH MEMBER = c( 77, 63 )"
    ## [1] "BEST both OBJECTIVE FUNCTION = 4.10509623106111"

![](man/figures/README-nns_forecasting-5.png)<!-- -->

    ## Package: RandomWalker
    ## [1] "CURRNET METHOD: lin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 77 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 1.99464580380045"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 77, 49 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 1.43084527479292"
    ## [1] "BEST method = 'lin', seasonal.factor = c( 77, 49 )"
    ## [1] "BEST lin OBJECTIVE FUNCTION = 1.43084527479292"
    ## [1] "CURRNET METHOD: nonlin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'nonlin' , seasonal.factor =  c( 77, 49 ) ...)"
    ## [1] "CURRENT nonlin OBJECTIVE FUNCTION = 1.68749056774625"
    ## [1] "BEST method = 'nonlin' PATH MEMBER = c( 77, 49 )"
    ## [1] "BEST nonlin OBJECTIVE FUNCTION = 1.68749056774625"
    ## [1] "CURRNET METHOD: both"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'both' , seasonal.factor =  c( 77, 49 ) ...)"
    ## [1] "CURRENT both OBJECTIVE FUNCTION = 1.37849506714939"
    ## [1] "BEST method = 'both' PATH MEMBER = c( 77, 49 )"
    ## [1] "BEST both OBJECTIVE FUNCTION = 1.37849506714939"

![](man/figures/README-nns_forecasting-6.png)<!-- -->

    ## Package: tidyAML
    ## [1] "CURRNET METHOD: lin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 91 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 4.27929383426843"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 91, 84 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 3.85936549571152"
    ## [1] "BEST method = 'lin', seasonal.factor = c( 91, 84 )"
    ## [1] "BEST lin OBJECTIVE FUNCTION = 3.85936549571152"
    ## [1] "CURRNET METHOD: nonlin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'nonlin' , seasonal.factor =  c( 91, 84 ) ...)"
    ## [1] "CURRENT nonlin OBJECTIVE FUNCTION = 4.46659246583171"
    ## [1] "BEST method = 'nonlin' PATH MEMBER = c( 91, 84 )"
    ## [1] "BEST nonlin OBJECTIVE FUNCTION = 4.46659246583171"
    ## [1] "CURRNET METHOD: both"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'both' , seasonal.factor =  c( 91, 84 ) ...)"
    ## [1] "CURRENT both OBJECTIVE FUNCTION = 3.87957872245893"
    ## [1] "BEST method = 'both' PATH MEMBER = c( 91, 84 )"
    ## [1] "BEST both OBJECTIVE FUNCTION = 3.87957872245893"

![](man/figures/README-nns_forecasting-7.png)<!-- -->

    ## Package: TidyDensity
    ## [1] "CURRNET METHOD: lin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 63 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 3.1443736526152"
    ## [1] "BEST method = 'lin', seasonal.factor = c( 63 )"
    ## [1] "BEST lin OBJECTIVE FUNCTION = 3.1443736526152"
    ## [1] "CURRNET METHOD: nonlin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'nonlin' , seasonal.factor =  c( 63 ) ...)"
    ## [1] "CURRENT nonlin OBJECTIVE FUNCTION = 9.94896669265865"
    ## [1] "BEST method = 'nonlin' PATH MEMBER = c( 63 )"
    ## [1] "BEST nonlin OBJECTIVE FUNCTION = 9.94896669265865"
    ## [1] "CURRNET METHOD: both"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'both' , seasonal.factor =  c( 63 ) ...)"
    ## [1] "CURRENT both OBJECTIVE FUNCTION = 5.82559522204018"
    ## [1] "BEST method = 'both' PATH MEMBER = c( 63 )"
    ## [1] "BEST both OBJECTIVE FUNCTION = 5.82559522204018"

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
    ## 1 healthyR.data <tibble [1,681 × 2]> <tibble [28 × 2]> <split [1653|28]>
    ## 2 healthyR      <tibble [1,675 × 2]> <tibble [28 × 2]> <split [1647|28]>
    ## 3 healthyR.ts   <tibble [1,619 × 2]> <tibble [28 × 2]> <split [1591|28]>
    ## 4 healthyverse  <tibble [1,589 × 2]> <tibble [28 × 2]> <split [1561|28]>
    ## 5 healthyR.ai   <tibble [1,414 × 2]> <tibble [28 × 2]> <split [1386|28]>
    ## 6 TidyDensity   <tibble [1,265 × 2]> <tibble [28 × 2]> <split [1237|28]>
    ## 7 tidyAML       <tibble [873 × 2]>   <tibble [28 × 2]> <split [845|28]> 
    ## 8 RandomWalker  <tibble [295 × 2]>   <tibble [28 × 2]> <split [267|28]>

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
| healthyR.data | 1 | ARIMA | Test | 0.6663636 | 140.32175 | 0.8258058 | 130.29827 | 0.7976642 | 0.0033531 |
| healthyR.data | 2 | LM | Test | 0.6517483 | 132.78020 | 0.8076934 | 123.31663 | 0.7905078 | 0.0192217 |
| healthyR.data | 3 | NULL | NA | NA | NA | NA | NA | NA | NA |
| healthyR.data | 4 | NNAR | Test | 0.6738404 | 99.17450 | 0.8350715 | 192.76704 | 0.7992604 | 0.0013503 |
| healthyR | 1 | ARIMA | Test | 0.7083892 | 166.30392 | 0.7374237 | 150.62191 | 0.9332923 | 0.0319346 |
| healthyR | 2 | LM | Test | 0.6797732 | 103.15299 | 0.7076347 | 152.82885 | 0.9310783 | 0.0136723 |
| healthyR | 3 | NULL | NA | NA | NA | NA | NA | NA | NA |
| healthyR | 4 | NNAR | Test | 0.6788238 | 111.45800 | 0.7066464 | 150.01463 | 0.9273957 | 0.1265238 |
| healthyR.ts | 1 | ARIMA | Test | 0.8547373 | 134.32066 | 0.7769936 | 160.19805 | 1.1386280 | 0.0416422 |
| healthyR.ts | 2 | LM | Test | 0.8069316 | 152.38809 | 0.7335361 | 133.91779 | 1.0663878 | 0.1339794 |
| healthyR.ts | 3 | NULL | NA | NA | NA | NA | NA | NA | NA |
| healthyR.ts | 4 | NNAR | Test | 0.7910601 | 95.26942 | 0.7191082 | 175.95926 | 1.0748579 | 0.0045260 |
| healthyverse | 1 | ARIMA | Test | 0.6619540 | 232.68929 | 0.7591564 | 89.94722 | 0.8493288 | 0.0201394 |
| healthyverse | 2 | LM | Test | 0.6527656 | 232.69188 | 0.7486188 | 89.46370 | 0.8397523 | 0.0018701 |
| healthyverse | 3 | NULL | NA | NA | NA | NA | NA | NA | NA |
| healthyverse | 4 | NNAR | Test | 0.6043159 | 139.55616 | 0.6930547 | 94.41700 | 0.8215506 | 0.0787131 |
| healthyR.ai | 1 | ARIMA | Test | 0.6309596 | 106.84864 | 0.7958499 | 140.16216 | 0.7991502 | 0.1534304 |
| healthyR.ai | 2 | LM | Test | 0.6023868 | 103.86477 | 0.7598101 | 144.35267 | 0.7479329 | 0.0094979 |
| healthyR.ai | 3 | NULL | NA | NA | NA | NA | NA | NA | NA |
| healthyR.ai | 4 | NNAR | Test | 0.6309058 | 107.35200 | 0.7957821 | 147.41739 | 0.7843530 | 0.1788745 |
| TidyDensity | 1 | ARIMA | Test | 0.5447367 | 87.91464 | 0.7666524 | 112.14147 | 0.7576506 | 0.0086633 |
| TidyDensity | 2 | LM | Test | 0.4984514 | 117.63110 | 0.7015114 | 81.26098 | 0.7071430 | 0.0021239 |
| TidyDensity | 3 | NULL | NA | NA | NA | NA | NA | NA | NA |
| TidyDensity | 4 | NNAR | Test | 0.5880329 | 89.73179 | 0.8275866 | 127.64473 | 0.7982765 | 0.0044462 |
| tidyAML | 1 | ARIMA | Test | 0.5385445 | 191.36907 | 0.7675223 | 86.89710 | 0.6786102 | 0.0253850 |
| tidyAML | 2 | LM | Test | 0.5560969 | 191.37855 | 0.7925376 | 90.96764 | 0.6805962 | 0.0274963 |
| tidyAML | 3 | NULL | NA | NA | NA | NA | NA | NA | NA |
| tidyAML | 4 | NNAR | Test | 0.5481718 | 196.02654 | 0.7812428 | 88.35872 | 0.6801018 | 0.0144996 |
| RandomWalker | 1 | ARIMA | Test | 1.1198818 | 115.48964 | 0.6571570 | 159.15780 | 1.3151643 | 0.0007739 |
| RandomWalker | 2 | LM | Test | 1.0947096 | 97.49897 | 0.6423857 | 182.54073 | 1.2537848 | 0.0442079 |
| RandomWalker | 3 | NULL | NA | NA | NA | NA | NA | NA | NA |
| RandomWalker | 4 | NNAR | Test | 1.2243782 | 154.67138 | 0.7184765 | 154.22251 | 1.4362384 | 0.0098464 |

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
    ## 1 healthyR.da…         2 LM          Test  0.652 133.  0.808 123.  0.791 0.0192 
    ## 2 healthyR             4 NNAR        Test  0.679 111.  0.707 150.  0.927 0.127  
    ## 3 healthyR.ts          2 LM          Test  0.807 152.  0.734 134.  1.07  0.134  
    ## 4 healthyverse         4 NNAR        Test  0.604 140.  0.693  94.4 0.822 0.0787 
    ## 5 healthyR.ai          2 LM          Test  0.602 104.  0.760 144.  0.748 0.00950
    ## 6 TidyDensity          2 LM          Test  0.498 118.  0.702  81.3 0.707 0.00212
    ## 7 tidyAML              1 ARIMA       Test  0.539 191.  0.768  86.9 0.679 0.0254 
    ## 8 RandomWalker         2 LM          Test  1.09   97.5 0.642 183.  1.25  0.0442

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
    ## 1 healthyR.data <tibble>     <tibble>     <split [1653|28]> <mdl_tm_t [1 × 5]>
    ## 2 healthyR      <tibble>     <tibble>     <split [1647|28]> <mdl_tm_t [1 × 5]>
    ## 3 healthyR.ts   <tibble>     <tibble>     <split [1591|28]> <mdl_tm_t [1 × 5]>
    ## 4 healthyverse  <tibble>     <tibble>     <split [1561|28]> <mdl_tm_t [1 × 5]>
    ## 5 healthyR.ai   <tibble>     <tibble>     <split [1386|28]> <mdl_tm_t [1 × 5]>
    ## 6 TidyDensity   <tibble>     <tibble>     <split [1237|28]> <mdl_tm_t [1 × 5]>
    ## 7 tidyAML       <tibble>     <tibble>     <split [845|28]>  <mdl_tm_t [1 × 5]>
    ## 8 RandomWalker  <tibble>     <tibble>     <split [267|28]>  <mdl_tm_t [1 × 5]>

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
