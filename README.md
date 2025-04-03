Time Series Analysis and Nested Modeling of the Healthyverse Packages
================
Steven P. Sanderson II, MPH - Date:
03 April, 2025

This analysis follows a *Nested Modeltime Workflow*.

## Get Data

``` r
glimpse(downloads_tbl)
```

    ## Rows: 135,785
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

The last day in the data set is 2025-04-01 22:29:37, the file was
birthed on: 2022-07-02 23:58:17.511888, and at report knit time is
-2.409052^{4} hours old. Happy analyzing!

Now that we have our data lets take a look at it using the `skimr`
package.

``` r
skim(downloads_tbl)
```

|                                                  |               |
|:-------------------------------------------------|:--------------|
| Name                                             | downloads_tbl |
| Number of rows                                   | 135785        |
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
| r_version     |     97642 |          0.28 |   5 |   5 |     0 |       46 |          0 |
| r_arch        |     97642 |          0.28 |   3 |   7 |     0 |        5 |          0 |
| r_os          |     97642 |          0.28 |   7 |  15 |     0 |       21 |          0 |
| package       |         0 |          1.00 |   7 |  13 |     0 |        8 |          0 |
| version       |         0 |          1.00 |   5 |  17 |     0 |       60 |          0 |
| country       |     11403 |          0.92 |   2 |   2 |     0 |      163 |          0 |

**Variable type: Date**

| skim_variable | n_missing | complete_rate | min | max | median | n_unique |
|:---|---:|---:|:---|:---|:---|---:|
| date | 0 | 1 | 2020-11-23 | 2025-04-01 | 2023-05-25 | 1591 |

**Variable type: numeric**

| skim_variable | n_missing | complete_rate | mean | sd | p0 | p25 | p50 | p75 | p100 | hist |
|:---|---:|---:|---:|---:|---:|---:|---:|---:|---:|:---|
| size | 0 | 1 | 1133826.80 | 1523668.5 | 355 | 14701 | 262144 | 2367761 | 5677952 | ▇▁▂▁▁ |
| ip_id | 0 | 1 | 10355.67 | 18344.7 | 1 | 305 | 3076 | 11729 | 209747 | ▇▁▁▁▁ |

**Variable type: POSIXct**

| skim_variable | n_missing | complete_rate | min | max | median | n_unique |
|:---|---:|---:|:---|:---|:---|---:|
| date_time | 0 | 1 | 2020-11-23 09:00:41 | 2025-04-01 22:29:37 | 2023-05-25 05:50:34 | 82501 |

**Variable type: Timespan**

| skim_variable | n_missing | complete_rate | min | max |     median | n_unique |
|:--------------|----------:|--------------:|----:|----:|-----------:|---------:|
| time          |         0 |             1 |   0 |  59 | 12H 7M 24S |       60 |

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
    ## -150.07  -35.35  -10.40   26.99  812.64 
    ## 
    ## Coefficients:
    ##                                                      Estimate Std. Error
    ## (Intercept)                                        -1.925e+02  7.379e+01
    ## date                                                1.165e-02  3.915e-03
    ## lag(value, 1)                                       1.073e-01  2.485e-02
    ## lag(value, 7)                                       9.537e-02  2.583e-02
    ## lag(value, 14)                                      9.478e-02  2.583e-02
    ## lag(value, 21)                                      6.426e-02  2.591e-02
    ## lag(value, 28)                                      5.965e-02  2.581e-02
    ## lag(value, 35)                                      6.886e-02  2.585e-02
    ## lag(value, 42)                                      5.088e-02  2.607e-02
    ## lag(value, 49)                                      7.885e-02  2.595e-02
    ## month(date, label = TRUE).L                        -1.060e+01  5.151e+00
    ## month(date, label = TRUE).Q                         2.499e+00  5.205e+00
    ## month(date, label = TRUE).C                        -1.195e+01  5.247e+00
    ## month(date, label = TRUE)^4                        -6.942e+00  5.198e+00
    ## month(date, label = TRUE)^5                        -1.257e+01  5.215e+00
    ## month(date, label = TRUE)^6                        -2.845e+00  5.293e+00
    ## month(date, label = TRUE)^7                        -6.311e+00  5.165e+00
    ## month(date, label = TRUE)^8                        -4.981e+00  5.193e+00
    ## month(date, label = TRUE)^9                         6.051e+00  5.254e+00
    ## month(date, label = TRUE)^10                        4.130e+00  5.307e+00
    ## month(date, label = TRUE)^11                       -5.771e+00  5.341e+00
    ## fourier_vec(date, type = "sin", K = 1, period = 7) -1.188e+01  2.394e+00
    ## fourier_vec(date, type = "cos", K = 1, period = 7)  8.301e+00  2.522e+00
    ##                                                    t value Pr(>|t|)    
    ## (Intercept)                                         -2.609 0.009178 ** 
    ## date                                                 2.975 0.002979 ** 
    ## lag(value, 1)                                        4.318 1.68e-05 ***
    ## lag(value, 7)                                        3.692 0.000230 ***
    ## lag(value, 14)                                       3.669 0.000251 ***
    ## lag(value, 21)                                       2.480 0.013246 *  
    ## lag(value, 28)                                       2.311 0.020949 *  
    ## lag(value, 35)                                       2.663 0.007818 ** 
    ## lag(value, 42)                                       1.952 0.051127 .  
    ## lag(value, 49)                                       3.039 0.002416 ** 
    ## month(date, label = TRUE).L                         -2.057 0.039830 *  
    ## month(date, label = TRUE).Q                          0.480 0.631213    
    ## month(date, label = TRUE).C                         -2.277 0.022939 *  
    ## month(date, label = TRUE)^4                         -1.335 0.181931    
    ## month(date, label = TRUE)^5                         -2.409 0.016094 *  
    ## month(date, label = TRUE)^6                         -0.538 0.591001    
    ## month(date, label = TRUE)^7                         -1.222 0.221951    
    ## month(date, label = TRUE)^8                         -0.959 0.337627    
    ## month(date, label = TRUE)^9                          1.152 0.249649    
    ## month(date, label = TRUE)^10                         0.778 0.436582    
    ## month(date, label = TRUE)^11                        -1.080 0.280133    
    ## fourier_vec(date, type = "sin", K = 1, period = 7)  -4.963 7.74e-07 ***
    ## fourier_vec(date, type = "cos", K = 1, period = 7)   3.291 0.001020 ** 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 58.67 on 1519 degrees of freedom
    ##   (49 observations deleted due to missingness)
    ## Multiple R-squared:  0.2514, Adjusted R-squared:  0.2406 
    ## F-statistic: 23.19 on 22 and 1519 DF,  p-value: < 2.2e-16

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
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 2.55352328464236"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 98, 35 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 2.2199164275791"
    ## [1] "BEST method = 'lin', seasonal.factor = c( 98, 35 )"
    ## [1] "BEST lin OBJECTIVE FUNCTION = 2.2199164275791"
    ## [1] "CURRNET METHOD: nonlin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'nonlin' , seasonal.factor =  c( 98, 35 ) ...)"
    ## [1] "CURRENT nonlin OBJECTIVE FUNCTION = 2.88090731026894"
    ## [1] "BEST method = 'nonlin' PATH MEMBER = c( 98, 35 )"
    ## [1] "BEST nonlin OBJECTIVE FUNCTION = 2.88090731026894"
    ## [1] "CURRNET METHOD: both"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'both' , seasonal.factor =  c( 98, 35 ) ...)"
    ## [1] "CURRENT both OBJECTIVE FUNCTION = 2.45608619260262"
    ## [1] "BEST method = 'both' PATH MEMBER = c( 98, 35 )"
    ## [1] "BEST both OBJECTIVE FUNCTION = 2.45608619260262"

![](man/figures/README-nns_forecasting-1.png)<!-- -->

    ## Package: healthyR.ai
    ## [1] "CURRNET METHOD: lin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 49 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 1.83094808618091"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 49, 70 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 1.59433474052953"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 49, 70, 98 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 1.46959647071117"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 49, 70, 98, 77 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 1.46021350239903"
    ## [1] "BEST method = 'lin', seasonal.factor = c( 49, 70, 98, 77 )"
    ## [1] "BEST lin OBJECTIVE FUNCTION = 1.46021350239903"
    ## [1] "CURRNET METHOD: nonlin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'nonlin' , seasonal.factor =  c( 49, 70, 98, 77 ) ...)"
    ## [1] "CURRENT nonlin OBJECTIVE FUNCTION = 2.83897994876425"
    ## [1] "BEST method = 'nonlin' PATH MEMBER = c( 49, 70, 98, 77 )"
    ## [1] "BEST nonlin OBJECTIVE FUNCTION = 2.83897994876425"
    ## [1] "CURRNET METHOD: both"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'both' , seasonal.factor =  c( 49, 70, 98, 77 ) ...)"
    ## [1] "CURRENT both OBJECTIVE FUNCTION = 1.93311911530972"
    ## [1] "BEST method = 'both' PATH MEMBER = c( 49, 70, 98, 77 )"
    ## [1] "BEST both OBJECTIVE FUNCTION = 1.93311911530972"

![](man/figures/README-nns_forecasting-2.png)<!-- -->

    ## Package: healthyR.data
    ## [1] "CURRNET METHOD: lin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 98 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 1.66594600575183"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 98, 77 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 1.41638274164315"
    ## [1] "BEST method = 'lin', seasonal.factor = c( 98, 77 )"
    ## [1] "BEST lin OBJECTIVE FUNCTION = 1.41638274164315"
    ## [1] "CURRNET METHOD: nonlin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'nonlin' , seasonal.factor =  c( 98, 77 ) ...)"
    ## [1] "CURRENT nonlin OBJECTIVE FUNCTION = 1.98335106742759"
    ## [1] "BEST method = 'nonlin' PATH MEMBER = c( 98, 77 )"
    ## [1] "BEST nonlin OBJECTIVE FUNCTION = 1.98335106742759"
    ## [1] "CURRNET METHOD: both"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'both' , seasonal.factor =  c( 98, 77 ) ...)"
    ## [1] "CURRENT both OBJECTIVE FUNCTION = 1.58171377907799"
    ## [1] "BEST method = 'both' PATH MEMBER = c( 98, 77 )"
    ## [1] "BEST both OBJECTIVE FUNCTION = 1.58171377907799"

![](man/figures/README-nns_forecasting-3.png)<!-- -->

    ## Package: healthyR.ts
    ## [1] "CURRNET METHOD: lin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 98 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 1.82723444717978"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 98, 63 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 1.44650159678023"
    ## [1] "BEST method = 'lin', seasonal.factor = c( 98, 63 )"
    ## [1] "BEST lin OBJECTIVE FUNCTION = 1.44650159678023"
    ## [1] "CURRNET METHOD: nonlin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'nonlin' , seasonal.factor =  c( 98, 63 ) ...)"
    ## [1] "CURRENT nonlin OBJECTIVE FUNCTION = 1.99909216219021"
    ## [1] "BEST method = 'nonlin' PATH MEMBER = c( 98, 63 )"
    ## [1] "BEST nonlin OBJECTIVE FUNCTION = 1.99909216219021"
    ## [1] "CURRNET METHOD: both"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'both' , seasonal.factor =  c( 98, 63 ) ...)"
    ## [1] "CURRENT both OBJECTIVE FUNCTION = 1.60575515021221"
    ## [1] "BEST method = 'both' PATH MEMBER = c( 98, 63 )"
    ## [1] "BEST both OBJECTIVE FUNCTION = 1.60575515021221"

![](man/figures/README-nns_forecasting-4.png)<!-- -->

    ## Package: healthyverse
    ## [1] "CURRNET METHOD: lin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 49 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 2.0975507124322"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 49, 77 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 1.91297344216404"
    ## [1] "BEST method = 'lin', seasonal.factor = c( 49, 77 )"
    ## [1] "BEST lin OBJECTIVE FUNCTION = 1.91297344216404"
    ## [1] "CURRNET METHOD: nonlin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'nonlin' , seasonal.factor =  c( 49, 77 ) ...)"
    ## [1] "CURRENT nonlin OBJECTIVE FUNCTION = 3.86253399666788"
    ## [1] "BEST method = 'nonlin' PATH MEMBER = c( 49, 77 )"
    ## [1] "BEST nonlin OBJECTIVE FUNCTION = 3.86253399666788"
    ## [1] "CURRNET METHOD: both"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'both' , seasonal.factor =  c( 49, 77 ) ...)"
    ## [1] "CURRENT both OBJECTIVE FUNCTION = 2.63446017284118"
    ## [1] "BEST method = 'both' PATH MEMBER = c( 49, 77 )"
    ## [1] "BEST both OBJECTIVE FUNCTION = 2.63446017284118"

![](man/figures/README-nns_forecasting-5.png)<!-- -->

    ## Package: RandomWalker
    ## [1] "CURRNET METHOD: lin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 21 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 5.15900074114673"
    ## [1] "BEST method = 'lin', seasonal.factor = c( 21 )"
    ## [1] "BEST lin OBJECTIVE FUNCTION = 5.15900074114673"
    ## [1] "CURRNET METHOD: nonlin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'nonlin' , seasonal.factor =  c( 21 ) ...)"
    ## [1] "CURRENT nonlin OBJECTIVE FUNCTION = 3.8272241130769"
    ## [1] "BEST method = 'nonlin' PATH MEMBER = c( 21 )"
    ## [1] "BEST nonlin OBJECTIVE FUNCTION = 3.8272241130769"
    ## [1] "CURRNET METHOD: both"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'both' , seasonal.factor =  c( 21 ) ...)"
    ## [1] "CURRENT both OBJECTIVE FUNCTION = 3.07806961642506"
    ## [1] "BEST method = 'both' PATH MEMBER = c( 21 )"
    ## [1] "BEST both OBJECTIVE FUNCTION = 3.07806961642506"

![](man/figures/README-nns_forecasting-6.png)<!-- -->

    ## Package: tidyAML
    ## [1] "CURRNET METHOD: lin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 28 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 5.45107057048562"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 28, 56 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 5.32550638661827"
    ## [1] "BEST method = 'lin', seasonal.factor = c( 28, 56 )"
    ## [1] "BEST lin OBJECTIVE FUNCTION = 5.32550638661827"
    ## [1] "CURRNET METHOD: nonlin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'nonlin' , seasonal.factor =  c( 28, 56 ) ...)"
    ## [1] "CURRENT nonlin OBJECTIVE FUNCTION = 4.48954485299226"
    ## [1] "BEST method = 'nonlin' PATH MEMBER = c( 28, 56 )"
    ## [1] "BEST nonlin OBJECTIVE FUNCTION = 4.48954485299226"
    ## [1] "CURRNET METHOD: both"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'both' , seasonal.factor =  c( 28, 56 ) ...)"
    ## [1] "CURRENT both OBJECTIVE FUNCTION = 4.42645552277851"
    ## [1] "BEST method = 'both' PATH MEMBER = c( 28, 56 )"
    ## [1] "BEST both OBJECTIVE FUNCTION = 4.42645552277851"

![](man/figures/README-nns_forecasting-7.png)<!-- -->

    ## Package: TidyDensity
    ## [1] "CURRNET METHOD: lin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 77 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 1.86404859225712"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 77, 63 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 1.68080394784309"
    ## [1] "BEST method = 'lin', seasonal.factor = c( 77, 63 )"
    ## [1] "BEST lin OBJECTIVE FUNCTION = 1.68080394784309"
    ## [1] "CURRNET METHOD: nonlin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'nonlin' , seasonal.factor =  c( 77, 63 ) ...)"
    ## [1] "CURRENT nonlin OBJECTIVE FUNCTION = 2.53216862180007"
    ## [1] "BEST method = 'nonlin' PATH MEMBER = c( 77, 63 )"
    ## [1] "BEST nonlin OBJECTIVE FUNCTION = 2.53216862180007"
    ## [1] "CURRNET METHOD: both"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'both' , seasonal.factor =  c( 77, 63 ) ...)"
    ## [1] "CURRENT both OBJECTIVE FUNCTION = 1.91479212331376"
    ## [1] "BEST method = 'both' PATH MEMBER = c( 77, 63 )"
    ## [1] "BEST both OBJECTIVE FUNCTION = 1.91479212331376"

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
    ## 1 healthyR.data <tibble [1,584 × 2]> <tibble [28 × 2]> <split [1556|28]>
    ## 2 healthyR      <tibble [1,577 × 2]> <tibble [28 × 2]> <split [1549|28]>
    ## 3 healthyR.ts   <tibble [1,521 × 2]> <tibble [28 × 2]> <split [1493|28]>
    ## 4 healthyverse  <tibble [1,492 × 2]> <tibble [28 × 2]> <split [1464|28]>
    ## 5 healthyR.ai   <tibble [1,316 × 2]> <tibble [28 × 2]> <split [1288|28]>
    ## 6 TidyDensity   <tibble [1,167 × 2]> <tibble [28 × 2]> <split [1139|28]>
    ## 7 tidyAML       <tibble [775 × 2]>   <tibble [28 × 2]> <split [747|28]> 
    ## 8 RandomWalker  <tibble [197 × 2]>   <tibble [28 × 2]> <split [169|28]>

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
| healthyR.data | 1 | ARIMA | Test | 0.7948141 | 116.69821 | 0.7309106 | 165.48197 | 1.0193759 | 0.0120620 |
| healthyR.data | 2 | LM | Test | 0.7330911 | 155.11737 | 0.6741502 | 130.30264 | 0.8956291 | 0.0020069 |
| healthyR.data | 3 | EARTH | Test | 0.9060869 | 153.51084 | 0.8332370 | 153.26521 | 1.1692703 | 0.0020069 |
| healthyR.data | 4 | NNAR | Test | 0.7873852 | 102.57108 | 0.7240790 | 164.91517 | 1.0379906 | 0.0004209 |
| healthyR | 1 | ARIMA | Test | 0.7984134 | 107.97971 | 0.8207410 | 167.74095 | 0.9686451 | 0.0216883 |
| healthyR | 2 | LM | Test | 0.7792738 | 98.52147 | 0.8010661 | 186.60552 | 0.9528867 | 0.0685084 |
| healthyR | 3 | EARTH | Test | 0.7772084 | 98.53362 | 0.7989430 | 186.56284 | 0.9498699 | 0.0685084 |
| healthyR | 4 | NNAR | Test | 0.7872014 | 106.99301 | 0.8092155 | 156.36306 | 0.9601315 | 0.0012130 |
| healthyR.ts | 1 | ARIMA | Test | 1.1639646 | 348.22405 | 0.8737134 | 150.37203 | 1.3812097 | 0.0226647 |
| healthyR.ts | 2 | LM | Test | 1.1181000 | 304.57190 | 0.8392858 | 150.70526 | 1.3409111 | 0.0226647 |
| healthyR.ts | 3 | EARTH | Test | 1.1438220 | 329.53854 | 0.8585936 | 150.43153 | 1.3636639 | 0.0226647 |
| healthyR.ts | 4 | NNAR | Test | 1.0456744 | 115.56606 | 0.7849205 | 171.64740 | 1.3089456 | 0.0216237 |
| healthyverse | 1 | ARIMA | Test | 0.6678868 | 185.69584 | 0.8458266 | 108.29469 | 0.8415623 | 0.0138637 |
| healthyverse | 2 | LM | Test | 0.6383240 | 263.11263 | 0.8083876 | 93.46126 | 0.7750344 | 0.0001165 |
| healthyverse | 3 | EARTH | Test | 0.6690477 | 162.77705 | 0.8472969 | 110.86874 | 0.8569319 | 0.0001165 |
| healthyverse | 4 | NNAR | Test | 0.6914884 | 153.49083 | 0.8757161 | 119.10276 | 0.8819174 | 0.0007487 |
| healthyR.ai | 1 | ARIMA | Test | 0.7410800 | 110.52956 | 0.7149311 | 159.90352 | 0.9404966 | 0.1028311 |
| healthyR.ai | 2 | LM | Test | 0.7183481 | 138.15899 | 0.6930013 | 140.11750 | 0.9416503 | 0.0355733 |
| healthyR.ai | 3 | EARTH | Test | 0.8509638 | 187.49617 | 0.8209377 | 182.71448 | 1.0301431 | 0.0355733 |
| healthyR.ai | 4 | NNAR | Test | 0.7711235 | 190.84228 | 0.7439145 | 148.78775 | 0.9675237 | 0.0094597 |
| TidyDensity | 1 | ARIMA | Test | 0.6763497 | 229.66377 | 0.6840555 | 116.51568 | 0.8249333 | 0.0269385 |
| TidyDensity | 2 | LM | Test | 0.7227193 | 277.36770 | 0.7309534 | 113.26456 | 0.8903231 | 0.0000524 |
| TidyDensity | 3 | EARTH | Test | 0.6819106 | 220.51413 | 0.6896797 | 117.46627 | 0.8318600 | 0.0000524 |
| TidyDensity | 4 | NNAR | Test | 0.6728109 | 148.74552 | 0.6804764 | 139.99776 | 0.8159817 | 0.0026043 |
| tidyAML | 1 | ARIMA | Test | 0.7057384 | 315.86145 | 0.7142012 | 107.39190 | 0.8525044 | 0.0413379 |
| tidyAML | 2 | LM | Test | 0.6935474 | 334.25983 | 0.7018640 | 104.40164 | 0.8307504 | 0.0014874 |
| tidyAML | 3 | EARTH | Test | 0.7035403 | 163.06145 | 0.7119767 | 132.82014 | 0.8966498 | 0.0014874 |
| tidyAML | 4 | NNAR | Test | 0.6777071 | 312.38908 | 0.6858338 | 104.81629 | 0.8218308 | 0.0247618 |
| RandomWalker | 1 | ARIMA | Test | 1.3931933 | 142.83678 | 0.8354721 | 116.50718 | 1.7822097 | 0.0271726 |
| RandomWalker | 2 | LM | Test | 1.2816303 | 112.56622 | 0.7685699 | 192.84082 | 1.4035054 | 0.0012684 |
| RandomWalker | 3 | EARTH | Test | 1.1848616 | 90.88542 | 0.7105395 | 167.98118 | 1.3443561 | NA |
| RandomWalker | 4 | NNAR | Test | 1.3904271 | 129.16647 | 0.8338133 | 185.71742 | 1.5481487 | 0.0981492 |

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
    ## 1 healthyR.d…         2 LM          Test  0.733 155.  0.674 130.  0.896  2.01e-3
    ## 2 healthyR            3 EARTH       Test  0.777  98.5 0.799 187.  0.950  6.85e-2
    ## 3 healthyR.ts         4 NNAR        Test  1.05  116.  0.785 172.  1.31   2.16e-2
    ## 4 healthyver…         2 LM          Test  0.638 263.  0.808  93.5 0.775  1.16e-4
    ## 5 healthyR.ai         1 ARIMA       Test  0.741 111.  0.715 160.  0.940  1.03e-1
    ## 6 TidyDensity         4 NNAR        Test  0.673 149.  0.680 140.  0.816  2.60e-3
    ## 7 tidyAML             4 NNAR        Test  0.678 312.  0.686 105.  0.822  2.48e-2
    ## 8 RandomWalk…         3 EARTH       Test  1.18   90.9 0.711 168.  1.34  NA

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
    ## 1 healthyR.data <tibble>     <tibble>     <split [1556|28]> <mdl_tm_t [1 × 5]>
    ## 2 healthyR      <tibble>     <tibble>     <split [1549|28]> <mdl_tm_t [1 × 5]>
    ## 3 healthyR.ts   <tibble>     <tibble>     <split [1493|28]> <mdl_tm_t [1 × 5]>
    ## 4 healthyverse  <tibble>     <tibble>     <split [1464|28]> <mdl_tm_t [1 × 5]>
    ## 5 healthyR.ai   <tibble>     <tibble>     <split [1288|28]> <mdl_tm_t [1 × 5]>
    ## 6 TidyDensity   <tibble>     <tibble>     <split [1139|28]> <mdl_tm_t [1 × 5]>
    ## 7 tidyAML       <tibble>     <tibble>     <split [747|28]>  <mdl_tm_t [1 × 5]>
    ## 8 RandomWalker  <tibble>     <tibble>     <split [169|28]>  <mdl_tm_t [1 × 5]>

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
