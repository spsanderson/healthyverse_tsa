Time Series Analysis and Nested Modeling of the Healthyverse Packages
================
Steven P. Sanderson II, MPH - Date:
19 March, 2025

This analysis follows a *Nested Modeltime Workflow*.

## Get Data

``` r
glimpse(downloads_tbl)
```

    ## Rows: 134,442
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

The last day in the data set is 2025-03-17 23:56:59, the file was
birthed on: 2024-08-07 07:35:44, and at report knit time is -5340.35
hours old. Happy analyzing!

Now that we have our data lets take a look at it using the `skimr`
package.

``` r
skim(downloads_tbl)
```

|                                                  |               |
|:-------------------------------------------------|:--------------|
| Name                                             | downloads_tbl |
| Number of rows                                   | 134442        |
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
| r_version     |     96602 |          0.28 |   5 |   5 |     0 |       46 |          0 |
| r_arch        |     96602 |          0.28 |   3 |   7 |     0 |        5 |          0 |
| r_os          |     96602 |          0.28 |   7 |  15 |     0 |       21 |          0 |
| package       |         0 |          1.00 |   7 |  13 |     0 |        8 |          0 |
| version       |         0 |          1.00 |   5 |  17 |     0 |       60 |          0 |
| country       |     11313 |          0.92 |   2 |   2 |     0 |      163 |          0 |

**Variable type: Date**

| skim_variable | n_missing | complete_rate | min | max | median | n_unique |
|:---|---:|---:|:---|:---|:---|---:|
| date | 0 | 1 | 2020-11-23 | 2025-03-17 | 2023-05-19 | 1576 |

**Variable type: numeric**

| skim_variable | n_missing | complete_rate | mean | sd | p0 | p25 | p50 | p75 | p100 | hist |
|:---|---:|---:|---:|---:|---:|---:|---:|---:|---:|:---|
| size | 0 | 1 | 1135638.77 | 1525427.60 | 355 | 14701.00 | 260657 | 2367781 | 5677952 | ▇▁▂▁▁ |
| ip_id | 0 | 1 | 10373.94 | 18358.17 | 1 | 300.25 | 3091 | 11798 | 209747 | ▇▁▁▁▁ |

**Variable type: POSIXct**

| skim_variable | n_missing | complete_rate | min | max | median | n_unique |
|:---|---:|---:|:---|:---|:---|---:|
| date_time | 0 | 1 | 2020-11-23 09:00:41 | 2025-03-17 23:56:59 | 2023-05-19 19:28:49 | 81579 |

**Variable type: Timespan**

| skim_variable | n_missing | complete_rate | min | max | median | n_unique |
|:--------------|----------:|--------------:|----:|----:|-------:|---------:|
| time          |         0 |             1 |   0 |  59 |      6 |       60 |

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
    ## -153.01  -35.12  -10.16   26.98  811.00 
    ## 
    ## Coefficients:
    ##                                                      Estimate Std. Error
    ## (Intercept)                                        -1.981e+02  7.481e+01
    ## date                                                1.188e-02  3.969e-03
    ## lag(value, 1)                                       1.097e-01  2.495e-02
    ## lag(value, 7)                                       9.257e-02  2.584e-02
    ## lag(value, 14)                                      9.569e-02  2.588e-02
    ## lag(value, 21)                                      6.498e-02  2.594e-02
    ## lag(value, 28)                                      6.275e-02  2.592e-02
    ## lag(value, 35)                                      7.092e-02  2.607e-02
    ## lag(value, 42)                                      5.390e-02  2.619e-02
    ## lag(value, 49)                                      8.454e-02  2.612e-02
    ## month(date, label = TRUE).L                        -1.142e+01  5.167e+00
    ## month(date, label = TRUE).Q                         2.488e+00  5.193e+00
    ## month(date, label = TRUE).C                        -1.162e+01  5.256e+00
    ## month(date, label = TRUE)^4                        -7.818e+00  5.220e+00
    ## month(date, label = TRUE)^5                        -1.215e+01  5.206e+00
    ## month(date, label = TRUE)^6                        -2.650e+00  5.286e+00
    ## month(date, label = TRUE)^7                        -7.207e+00  5.189e+00
    ## month(date, label = TRUE)^8                        -3.929e+00  5.222e+00
    ## month(date, label = TRUE)^9                         5.228e+00  5.271e+00
    ## month(date, label = TRUE)^10                        4.694e+00  5.306e+00
    ## month(date, label = TRUE)^11                       -6.103e+00  5.330e+00
    ## fourier_vec(date, type = "sin", K = 1, period = 7) -1.181e+01  2.401e+00
    ## fourier_vec(date, type = "cos", K = 1, period = 7)  7.992e+00  2.529e+00
    ##                                                    t value Pr(>|t|)    
    ## (Intercept)                                         -2.649 0.008166 ** 
    ## date                                                 2.993 0.002811 ** 
    ## lag(value, 1)                                        4.395 1.19e-05 ***
    ## lag(value, 7)                                        3.582 0.000352 ***
    ## lag(value, 14)                                       3.697 0.000226 ***
    ## lag(value, 21)                                       2.505 0.012358 *  
    ## lag(value, 28)                                       2.421 0.015613 *  
    ## lag(value, 35)                                       2.720 0.006594 ** 
    ## lag(value, 42)                                       2.058 0.039765 *  
    ## lag(value, 49)                                       3.237 0.001236 ** 
    ## month(date, label = TRUE).L                         -2.211 0.027203 *  
    ## month(date, label = TRUE).Q                          0.479 0.631868    
    ## month(date, label = TRUE).C                         -2.210 0.027238 *  
    ## month(date, label = TRUE)^4                         -1.498 0.134425    
    ## month(date, label = TRUE)^5                         -2.333 0.019755 *  
    ## month(date, label = TRUE)^6                         -0.501 0.616208    
    ## month(date, label = TRUE)^7                         -1.389 0.165091    
    ## month(date, label = TRUE)^8                         -0.752 0.451886    
    ## month(date, label = TRUE)^9                          0.992 0.321467    
    ## month(date, label = TRUE)^10                         0.885 0.376514    
    ## month(date, label = TRUE)^11                        -1.145 0.252368    
    ## fourier_vec(date, type = "sin", K = 1, period = 7)  -4.919 9.65e-07 ***
    ## fourier_vec(date, type = "cos", K = 1, period = 7)   3.160 0.001607 ** 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 58.51 on 1504 degrees of freedom
    ##   (49 observations deleted due to missingness)
    ## Multiple R-squared:  0.2575, Adjusted R-squared:  0.2466 
    ## F-statistic:  23.7 on 22 and 1504 DF,  p-value: < 2.2e-16

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
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 1.40367576187729"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 77, 98 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 1.24474629195897"
    ## [1] "BEST method = 'lin', seasonal.factor = c( 77, 98 )"
    ## [1] "BEST lin OBJECTIVE FUNCTION = 1.24474629195897"
    ## [1] "CURRNET METHOD: nonlin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'nonlin' , seasonal.factor =  c( 77, 98 ) ...)"
    ## [1] "CURRENT nonlin OBJECTIVE FUNCTION = 1.51704249548099"
    ## [1] "BEST method = 'nonlin' PATH MEMBER = c( 77, 98 )"
    ## [1] "BEST nonlin OBJECTIVE FUNCTION = 1.51704249548099"
    ## [1] "CURRNET METHOD: both"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'both' , seasonal.factor =  c( 77, 98 ) ...)"
    ## [1] "CURRENT both OBJECTIVE FUNCTION = 1.31376711483938"
    ## [1] "BEST method = 'both' PATH MEMBER = c( 77, 98 )"
    ## [1] "BEST both OBJECTIVE FUNCTION = 1.31376711483938"

![](man/figures/README-nns_forecasting-1.png)<!-- -->

    ## Package: healthyR.ai
    ## [1] "CURRNET METHOD: lin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 21 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 1.42767779579281"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 21, 77 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 1.25710861919969"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 21, 77, 98 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 1.21395221021773"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 21, 77, 98, 63 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 1.1670061128218"
    ## [1] "BEST method = 'lin', seasonal.factor = c( 21, 77, 98, 63 )"
    ## [1] "BEST lin OBJECTIVE FUNCTION = 1.1670061128218"
    ## [1] "CURRNET METHOD: nonlin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'nonlin' , seasonal.factor =  c( 21, 77, 98, 63 ) ...)"
    ## [1] "CURRENT nonlin OBJECTIVE FUNCTION = 1.49109874723879"
    ## [1] "BEST method = 'nonlin' PATH MEMBER = c( 21, 77, 98, 63 )"
    ## [1] "BEST nonlin OBJECTIVE FUNCTION = 1.49109874723879"
    ## [1] "CURRNET METHOD: both"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'both' , seasonal.factor =  c( 21, 77, 98, 63 ) ...)"
    ## [1] "CURRENT both OBJECTIVE FUNCTION = 1.27261168463065"
    ## [1] "BEST method = 'both' PATH MEMBER = c( 21, 77, 98, 63 )"
    ## [1] "BEST both OBJECTIVE FUNCTION = 1.27261168463065"

![](man/figures/README-nns_forecasting-2.png)<!-- -->

    ## Package: healthyR.data
    ## [1] "CURRNET METHOD: lin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 98 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 1.44867102563993"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 98, 77 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 0.985750087018749"
    ## [1] "BEST method = 'lin', seasonal.factor = c( 98, 77 )"
    ## [1] "BEST lin OBJECTIVE FUNCTION = 0.985750087018749"
    ## [1] "CURRNET METHOD: nonlin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'nonlin' , seasonal.factor =  c( 98, 77 ) ...)"
    ## [1] "CURRENT nonlin OBJECTIVE FUNCTION = 1.29715450054209"
    ## [1] "BEST method = 'nonlin' PATH MEMBER = c( 98, 77 )"
    ## [1] "BEST nonlin OBJECTIVE FUNCTION = 1.29715450054209"
    ## [1] "CURRNET METHOD: both"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'both' , seasonal.factor =  c( 98, 77 ) ...)"
    ## [1] "CURRENT both OBJECTIVE FUNCTION = 1.04348213533717"
    ## [1] "BEST method = 'both' PATH MEMBER = c( 98, 77 )"
    ## [1] "BEST both OBJECTIVE FUNCTION = 1.04348213533717"

![](man/figures/README-nns_forecasting-3.png)<!-- -->

    ## Package: healthyR.ts
    ## [1] "CURRNET METHOD: lin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 98 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 1.38527105490477"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 98, 21 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 1.21912881200152"
    ## [1] "BEST method = 'lin', seasonal.factor = c( 98, 21 )"
    ## [1] "BEST lin OBJECTIVE FUNCTION = 1.21912881200152"
    ## [1] "CURRNET METHOD: nonlin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'nonlin' , seasonal.factor =  c( 98, 21 ) ...)"
    ## [1] "CURRENT nonlin OBJECTIVE FUNCTION = 1.15060388362189"
    ## [1] "BEST method = 'nonlin' PATH MEMBER = c( 98, 21 )"
    ## [1] "BEST nonlin OBJECTIVE FUNCTION = 1.15060388362189"
    ## [1] "CURRNET METHOD: both"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'both' , seasonal.factor =  c( 98, 21 ) ...)"
    ## [1] "CURRENT both OBJECTIVE FUNCTION = 1.09025398443123"
    ## [1] "BEST method = 'both' PATH MEMBER = c( 98, 21 )"
    ## [1] "BEST both OBJECTIVE FUNCTION = 1.09025398443123"

![](man/figures/README-nns_forecasting-4.png)<!-- -->

    ## Package: healthyverse
    ## [1] "CURRNET METHOD: lin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 49 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 1.54253234915181"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 49, 77 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 1.35735488484884"
    ## [1] "BEST method = 'lin', seasonal.factor = c( 49, 77 )"
    ## [1] "BEST lin OBJECTIVE FUNCTION = 1.35735488484884"
    ## [1] "CURRNET METHOD: nonlin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'nonlin' , seasonal.factor =  c( 49, 77 ) ...)"
    ## [1] "CURRENT nonlin OBJECTIVE FUNCTION = 2.70003664356016"
    ## [1] "BEST method = 'nonlin' PATH MEMBER = c( 49, 77 )"
    ## [1] "BEST nonlin OBJECTIVE FUNCTION = 2.70003664356016"
    ## [1] "CURRNET METHOD: both"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'both' , seasonal.factor =  c( 49, 77 ) ...)"
    ## [1] "CURRENT both OBJECTIVE FUNCTION = 1.85981118922689"
    ## [1] "BEST method = 'both' PATH MEMBER = c( 49, 77 )"
    ## [1] "BEST both OBJECTIVE FUNCTION = 1.85981118922689"

![](man/figures/README-nns_forecasting-5.png)<!-- -->

    ## Package: RandomWalker
    ## [1] "CURRNET METHOD: lin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 35 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 3.15668674138195"
    ## [1] "BEST method = 'lin', seasonal.factor = c( 35 )"
    ## [1] "BEST lin OBJECTIVE FUNCTION = 3.15668674138195"
    ## [1] "CURRNET METHOD: nonlin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'nonlin' , seasonal.factor =  c( 35 ) ...)"
    ## [1] "CURRENT nonlin OBJECTIVE FUNCTION = 4.76197808738959"
    ## [1] "BEST method = 'nonlin' PATH MEMBER = c( 35 )"
    ## [1] "BEST nonlin OBJECTIVE FUNCTION = 4.76197808738959"
    ## [1] "CURRNET METHOD: both"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'both' , seasonal.factor =  c( 35 ) ...)"
    ## [1] "CURRENT both OBJECTIVE FUNCTION = 3.91271981045145"
    ## [1] "BEST method = 'both' PATH MEMBER = c( 35 )"
    ## [1] "BEST both OBJECTIVE FUNCTION = 3.91271981045145"

![](man/figures/README-nns_forecasting-6.png)<!-- -->

    ## Package: tidyAML
    ## [1] "CURRNET METHOD: lin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 56 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 6.52395524268372"
    ## [1] "BEST method = 'lin', seasonal.factor = c( 56 )"
    ## [1] "BEST lin OBJECTIVE FUNCTION = 6.52395524268372"
    ## [1] "CURRNET METHOD: nonlin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'nonlin' , seasonal.factor =  c( 56 ) ...)"
    ## [1] "CURRENT nonlin OBJECTIVE FUNCTION = 6.17651776553656"
    ## [1] "BEST method = 'nonlin' PATH MEMBER = c( 56 )"
    ## [1] "BEST nonlin OBJECTIVE FUNCTION = 6.17651776553656"
    ## [1] "CURRNET METHOD: both"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'both' , seasonal.factor =  c( 56 ) ...)"
    ## [1] "CURRENT both OBJECTIVE FUNCTION = 6.05586893421094"
    ## [1] "BEST method = 'both' PATH MEMBER = c( 56 )"
    ## [1] "BEST both OBJECTIVE FUNCTION = 6.05586893421094"

![](man/figures/README-nns_forecasting-7.png)<!-- -->

    ## Package: TidyDensity
    ## [1] "CURRNET METHOD: lin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 77 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 1.52987919612293"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 77, 63 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 1.2065156878939"
    ## [1] "BEST method = 'lin', seasonal.factor = c( 77, 63 )"
    ## [1] "BEST lin OBJECTIVE FUNCTION = 1.2065156878939"
    ## [1] "CURRNET METHOD: nonlin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'nonlin' , seasonal.factor =  c( 77, 63 ) ...)"
    ## [1] "CURRENT nonlin OBJECTIVE FUNCTION = 1.89023595299183"
    ## [1] "BEST method = 'nonlin' PATH MEMBER = c( 77, 63 )"
    ## [1] "BEST nonlin OBJECTIVE FUNCTION = 1.89023595299183"
    ## [1] "CURRNET METHOD: both"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'both' , seasonal.factor =  c( 77, 63 ) ...)"
    ## [1] "CURRENT both OBJECTIVE FUNCTION = 1.42851688448049"
    ## [1] "BEST method = 'both' PATH MEMBER = c( 77, 63 )"
    ## [1] "BEST both OBJECTIVE FUNCTION = 1.42851688448049"

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
    ## 1 healthyR.data <tibble [1,569 × 2]> <tibble [28 × 2]> <split [1541|28]>
    ## 2 healthyR      <tibble [1,562 × 2]> <tibble [28 × 2]> <split [1534|28]>
    ## 3 healthyR.ts   <tibble [1,506 × 2]> <tibble [28 × 2]> <split [1478|28]>
    ## 4 healthyverse  <tibble [1,477 × 2]> <tibble [28 × 2]> <split [1449|28]>
    ## 5 healthyR.ai   <tibble [1,301 × 2]> <tibble [28 × 2]> <split [1273|28]>
    ## 6 TidyDensity   <tibble [1,152 × 2]> <tibble [28 × 2]> <split [1124|28]>
    ## 7 tidyAML       <tibble [760 × 2]>   <tibble [28 × 2]> <split [732|28]> 
    ## 8 RandomWalker  <tibble [182 × 2]>   <tibble [28 × 2]> <split [154|28]>

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
| healthyR.data | 1 | ARIMA | Test | 0.7356091 | 92.30520 | 0.6197290 | 151.89358 | 0.9265850 | 0.2510495 |
| healthyR.data | 2 | LM | Test | 0.8086072 | 190.71095 | 0.6812278 | 143.65671 | 0.9372262 | 0.0467657 |
| healthyR.data | 3 | EARTH | Test | 0.9767531 | 205.99704 | 0.8228857 | 133.26956 | 1.2477219 | 0.0467657 |
| healthyR.data | 4 | NNAR | Test | 0.8063818 | 97.94804 | 0.6793529 | 169.80418 | 1.0078902 | 0.0254542 |
| healthyR | 1 | ARIMA | Test | 0.7268484 | 133.33868 | 0.7819332 | 153.75286 | 0.8876008 | 0.0193356 |
| healthyR | 2 | LM | Test | 0.7320283 | 105.32173 | 0.7875056 | 184.50841 | 0.8984657 | 0.0605511 |
| healthyR | 3 | EARTH | Test | 0.7325631 | 109.88834 | 0.7880810 | 174.18732 | 0.8998890 | 0.0605511 |
| healthyR | 4 | NNAR | Test | 0.7352350 | 135.64356 | 0.7909553 | 159.13767 | 0.9012206 | 0.0131377 |
| healthyR.ts | 1 | ARIMA | Test | 1.0381514 | 126.90948 | 0.6983880 | 131.28504 | 1.2286020 | 0.0916395 |
| healthyR.ts | 2 | LM | Test | 1.0362537 | 123.66733 | 0.6971113 | 134.08698 | 1.2231052 | 0.0916395 |
| healthyR.ts | 3 | EARTH | Test | 1.0389135 | 127.18793 | 0.6989007 | 131.20399 | 1.2295633 | 0.0916395 |
| healthyR.ts | 4 | NNAR | Test | 0.9872292 | 105.27885 | 0.6641315 | 169.19016 | 1.1399084 | 0.1215783 |
| healthyverse | 1 | ARIMA | Test | 0.6696277 | 116.23127 | 0.9131153 | 115.41567 | 0.8105673 | 0.0617207 |
| healthyverse | 2 | LM | Test | 0.6786551 | 153.30525 | 0.9254252 | 97.14818 | 0.8200536 | 0.0952512 |
| healthyverse | 3 | EARTH | Test | 0.6703006 | 106.43468 | 0.9140329 | 109.62125 | 0.8311577 | 0.0952512 |
| healthyverse | 4 | NNAR | Test | 0.6718512 | 98.82305 | 0.9161473 | 109.70951 | 0.8551053 | 0.0040392 |
| healthyR.ai | 1 | ARIMA | Test | 0.8214533 | 105.50161 | 0.8158309 | 177.00038 | 0.9426630 | 0.1480357 |
| healthyR.ai | 2 | LM | Test | 0.8195914 | 92.54615 | 0.8139818 | 149.09263 | 0.9829596 | 0.0894355 |
| healthyR.ai | 3 | EARTH | Test | 1.9053388 | 387.68887 | 1.8922979 | 148.87840 | 2.1998982 | 0.0894355 |
| healthyR.ai | 4 | NNAR | Test | 0.7810256 | 98.43292 | 0.7756799 | 145.86107 | 0.9199669 | 0.1267760 |
| TidyDensity | 1 | ARIMA | Test | 0.7128905 | 175.62667 | 0.6879286 | 119.31216 | 0.8533497 | 0.0106798 |
| TidyDensity | 2 | LM | Test | 0.7574606 | 216.01913 | 0.7309381 | 118.27525 | 0.8765981 | 0.1135574 |
| TidyDensity | 3 | EARTH | Test | 0.7165675 | 159.86337 | 0.6914769 | 124.65291 | 0.8582259 | 0.1135574 |
| TidyDensity | 4 | NNAR | Test | 0.6993600 | 119.54622 | 0.6748719 | 137.83803 | 0.8887619 | 0.0196975 |
| tidyAML | 1 | ARIMA | Test | 0.6602126 | 255.23550 | 0.7046029 | 93.90089 | 0.7886478 | 0.0032232 |
| tidyAML | 2 | LM | Test | 0.6411054 | 245.74696 | 0.6842110 | 94.47214 | 0.7616570 | 0.0601808 |
| tidyAML | 3 | EARTH | Test | 0.6565921 | 284.48585 | 0.7007390 | 92.65001 | 0.7781090 | 0.0601808 |
| tidyAML | 4 | NNAR | Test | 0.6222063 | 228.33209 | 0.6640412 | 95.02963 | 0.7380811 | 0.0726519 |
| RandomWalker | 1 | ARIMA | Test | 0.9106761 | 74.88663 | 0.4499896 | 89.15488 | 1.1948733 | 0.3163595 |
| RandomWalker | 2 | LM | Test | 1.3816200 | 110.40841 | 0.6826958 | 189.60615 | 1.4677488 | 0.0063401 |
| RandomWalker | 3 | EARTH | Test | 1.2750301 | 94.66320 | 0.6300268 | 175.91149 | 1.4088118 | NA |
| RandomWalker | 4 | NNAR | Test | 1.4427263 | 116.18799 | 0.7128900 | 168.75200 | 1.5936892 | 0.0153496 |

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
    ## 1 healthyR.data         1 ARIMA       Test  0.736  92.3 0.620 152.  0.927 0.251 
    ## 2 healthyR              1 ARIMA       Test  0.727 133.  0.782 154.  0.888 0.0193
    ## 3 healthyR.ts           4 NNAR        Test  0.987 105.  0.664 169.  1.14  0.122 
    ## 4 healthyverse          1 ARIMA       Test  0.670 116.  0.913 115.  0.811 0.0617
    ## 5 healthyR.ai           4 NNAR        Test  0.781  98.4 0.776 146.  0.920 0.127 
    ## 6 TidyDensity           1 ARIMA       Test  0.713 176.  0.688 119.  0.853 0.0107
    ## 7 tidyAML               4 NNAR        Test  0.622 228.  0.664  95.0 0.738 0.0727
    ## 8 RandomWalker          1 ARIMA       Test  0.911  74.9 0.450  89.2 1.19  0.316

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
    ## 1 healthyR.data <tibble>     <tibble>     <split [1541|28]> <mdl_tm_t [1 × 5]>
    ## 2 healthyR      <tibble>     <tibble>     <split [1534|28]> <mdl_tm_t [1 × 5]>
    ## 3 healthyR.ts   <tibble>     <tibble>     <split [1478|28]> <mdl_tm_t [1 × 5]>
    ## 4 healthyverse  <tibble>     <tibble>     <split [1449|28]> <mdl_tm_t [1 × 5]>
    ## 5 healthyR.ai   <tibble>     <tibble>     <split [1273|28]> <mdl_tm_t [1 × 5]>
    ## 6 TidyDensity   <tibble>     <tibble>     <split [1124|28]> <mdl_tm_t [1 × 5]>
    ## 7 tidyAML       <tibble>     <tibble>     <split [732|28]>  <mdl_tm_t [1 × 5]>
    ## 8 RandomWalker  <tibble>     <tibble>     <split [154|28]>  <mdl_tm_t [1 × 5]>

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
