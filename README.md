Time Series Analysis and Nested Modeling of the Healthyverse Packages
================
Steven P. Sanderson II, MPH - Date:
26 March, 2025

This analysis follows a *Nested Modeltime Workflow*.

## Get Data

``` r
glimpse(downloads_tbl)
```

    ## Rows: 135,096
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

The last day in the data set is 2025-03-24 23:49:15, the file was
birthed on: 2024-08-07 07:35:44, and at report knit time is -5508.23
hours old. Happy analyzing!

Now that we have our data lets take a look at it using the `skimr`
package.

``` r
skim(downloads_tbl)
```

|                                                  |               |
|:-------------------------------------------------|:--------------|
| Name                                             | downloads_tbl |
| Number of rows                                   | 135096        |
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
| r_version     |     97074 |          0.28 |   5 |   5 |     0 |       46 |          0 |
| r_arch        |     97074 |          0.28 |   3 |   7 |     0 |        5 |          0 |
| r_os          |     97074 |          0.28 |   7 |  15 |     0 |       21 |          0 |
| package       |         0 |          1.00 |   7 |  13 |     0 |        8 |          0 |
| version       |         0 |          1.00 |   5 |  17 |     0 |       60 |          0 |
| country       |     11359 |          0.92 |   2 |   2 |     0 |      163 |          0 |

**Variable type: Date**

| skim_variable | n_missing | complete_rate | min | max | median | n_unique |
|:---|---:|---:|:---|:---|:---|---:|
| date | 0 | 1 | 2020-11-23 | 2025-03-24 | 2023-05-23 | 1583 |

**Variable type: numeric**

| skim_variable | n_missing | complete_rate | mean | sd | p0 | p25 | p50 | p75 | p100 | hist |
|:---|---:|---:|---:|---:|---:|---:|---:|---:|---:|:---|
| size | 0 | 1 | 1135365.70 | 1524813.20 | 355 | 14701 | 261770 | 2367775.50 | 5677952 | ▇▁▂▁▁ |
| ip_id | 0 | 1 | 10361.25 | 18349.11 | 1 | 305 | 3075 | 11764.25 | 209747 | ▇▁▁▁▁ |

**Variable type: POSIXct**

| skim_variable | n_missing | complete_rate | min | max | median | n_unique |
|:---|---:|---:|:---|:---|:---|---:|
| date_time | 0 | 1 | 2020-11-23 09:00:41 | 2025-03-24 23:49:15 | 2023-05-23 21:08:48 | 82091 |

**Variable type: Timespan**

| skim_variable | n_missing | complete_rate | min | max | median | n_unique |
|:--------------|----------:|--------------:|----:|----:|-------:|---------:|
| time          |         0 |             1 |   0 |  59 |   22.5 |       60 |

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
    ## -152.33  -35.14  -10.20   26.75  811.32 
    ## 
    ## Coefficients:
    ##                                                      Estimate Std. Error
    ## (Intercept)                                        -1.942e+02  7.412e+01
    ## date                                                1.168e-02  3.933e-03
    ## lag(value, 1)                                       1.104e-01  2.489e-02
    ## lag(value, 7)                                       9.242e-02  2.576e-02
    ## lag(value, 14)                                      9.614e-02  2.578e-02
    ## lag(value, 21)                                      6.487e-02  2.587e-02
    ## lag(value, 28)                                      6.063e-02  2.574e-02
    ## lag(value, 35)                                      7.062e-02  2.594e-02
    ## lag(value, 42)                                      5.289e-02  2.603e-02
    ## lag(value, 49)                                      8.410e-02  2.597e-02
    ## month(date, label = TRUE).L                        -1.111e+01  5.145e+00
    ## month(date, label = TRUE).Q                         2.518e+00  5.185e+00
    ## month(date, label = TRUE).C                        -1.178e+01  5.238e+00
    ## month(date, label = TRUE)^4                        -7.510e+00  5.195e+00
    ## month(date, label = TRUE)^5                        -1.225e+01  5.197e+00
    ## month(date, label = TRUE)^6                        -2.769e+00  5.276e+00
    ## month(date, label = TRUE)^7                        -6.875e+00  5.161e+00
    ## month(date, label = TRUE)^8                        -4.279e+00  5.192e+00
    ## month(date, label = TRUE)^9                         5.474e+00  5.250e+00
    ## month(date, label = TRUE)^10                        4.528e+00  5.294e+00
    ## month(date, label = TRUE)^11                       -6.014e+00  5.322e+00
    ## fourier_vec(date, type = "sin", K = 1, period = 7) -1.191e+01  2.391e+00
    ## fourier_vec(date, type = "cos", K = 1, period = 7)  7.913e+00  2.521e+00
    ##                                                    t value Pr(>|t|)    
    ## (Intercept)                                         -2.620 0.008893 ** 
    ## date                                                 2.971 0.003018 ** 
    ## lag(value, 1)                                        4.435 9.85e-06 ***
    ## lag(value, 7)                                        3.588 0.000344 ***
    ## lag(value, 14)                                       3.729 0.000199 ***
    ## lag(value, 21)                                       2.507 0.012267 *  
    ## lag(value, 28)                                       2.355 0.018638 *  
    ## lag(value, 35)                                       2.723 0.006545 ** 
    ## lag(value, 42)                                       2.032 0.042360 *  
    ## lag(value, 49)                                       3.239 0.001227 ** 
    ## month(date, label = TRUE).L                         -2.160 0.030924 *  
    ## month(date, label = TRUE).Q                          0.486 0.627289    
    ## month(date, label = TRUE).C                         -2.249 0.024672 *  
    ## month(date, label = TRUE)^4                         -1.446 0.148495    
    ## month(date, label = TRUE)^5                         -2.356 0.018583 *  
    ## month(date, label = TRUE)^6                         -0.525 0.599817    
    ## month(date, label = TRUE)^7                         -1.332 0.183054    
    ## month(date, label = TRUE)^8                         -0.824 0.410005    
    ## month(date, label = TRUE)^9                          1.043 0.297271    
    ## month(date, label = TRUE)^10                         0.855 0.392534    
    ## month(date, label = TRUE)^11                        -1.130 0.258656    
    ## fourier_vec(date, type = "sin", K = 1, period = 7)  -4.980 7.08e-07 ***
    ## fourier_vec(date, type = "cos", K = 1, period = 7)   3.139 0.001727 ** 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 58.43 on 1511 degrees of freedom
    ##   (49 observations deleted due to missingness)
    ## Multiple R-squared:  0.2565, Adjusted R-squared:  0.2456 
    ## F-statistic: 23.69 on 22 and 1511 DF,  p-value: < 2.2e-16

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
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 1.68889844137941"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 63, 98 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 1.32036925714005"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 63, 98, 77 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 1.30040939071171"
    ## [1] "BEST method = 'lin', seasonal.factor = c( 63, 98, 77 )"
    ## [1] "BEST lin OBJECTIVE FUNCTION = 1.30040939071171"
    ## [1] "CURRNET METHOD: nonlin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'nonlin' , seasonal.factor =  c( 63, 98, 77 ) ...)"
    ## [1] "CURRENT nonlin OBJECTIVE FUNCTION = 2.04358235522229"
    ## [1] "BEST method = 'nonlin' PATH MEMBER = c( 63, 98, 77 )"
    ## [1] "BEST nonlin OBJECTIVE FUNCTION = 2.04358235522229"
    ## [1] "CURRNET METHOD: both"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'both' , seasonal.factor =  c( 63, 98, 77 ) ...)"
    ## [1] "CURRENT both OBJECTIVE FUNCTION = 1.58559881551767"
    ## [1] "BEST method = 'both' PATH MEMBER = c( 63, 98, 77 )"
    ## [1] "BEST both OBJECTIVE FUNCTION = 1.58559881551767"

![](man/figures/README-nns_forecasting-1.png)<!-- -->

    ## Package: healthyR.ai
    ## [1] "CURRNET METHOD: lin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 21 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 1.31564009537737"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 21, 77 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 1.17260813175365"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 21, 77, 98 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 1.12698804807411"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 21, 77, 98, 63 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 1.04956362022118"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 21, 77, 98, 63, 70 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 1.03247516268329"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 21, 77, 98, 63, 70, 49 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 1.01845654662563"
    ## [1] "BEST method = 'lin', seasonal.factor = c( 21, 77, 98, 63, 70, 49 )"
    ## [1] "BEST lin OBJECTIVE FUNCTION = 1.01845654662563"
    ## [1] "CURRNET METHOD: nonlin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'nonlin' , seasonal.factor =  c( 21, 77, 98, 63, 70, 49 ) ...)"
    ## [1] "CURRENT nonlin OBJECTIVE FUNCTION = 1.32370111414889"
    ## [1] "BEST method = 'nonlin' PATH MEMBER = c( 21, 77, 98, 63, 70, 49 )"
    ## [1] "BEST nonlin OBJECTIVE FUNCTION = 1.32370111414889"
    ## [1] "CURRNET METHOD: both"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'both' , seasonal.factor =  c( 21, 77, 98, 63, 70, 49 ) ...)"
    ## [1] "CURRENT both OBJECTIVE FUNCTION = 1.09704684229944"
    ## [1] "BEST method = 'both' PATH MEMBER = c( 21, 77, 98, 63, 70, 49 )"
    ## [1] "BEST both OBJECTIVE FUNCTION = 1.09704684229944"

![](man/figures/README-nns_forecasting-2.png)<!-- -->

    ## Package: healthyR.data
    ## [1] "CURRNET METHOD: lin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 98 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 1.23687622141499"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 98, 77 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 1.0617815047255"
    ## [1] "BEST method = 'lin', seasonal.factor = c( 98, 77 )"
    ## [1] "BEST lin OBJECTIVE FUNCTION = 1.0617815047255"
    ## [1] "CURRNET METHOD: nonlin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'nonlin' , seasonal.factor =  c( 98, 77 ) ...)"
    ## [1] "CURRENT nonlin OBJECTIVE FUNCTION = 1.65524862801219"
    ## [1] "BEST method = 'nonlin' PATH MEMBER = c( 98, 77 )"
    ## [1] "BEST nonlin OBJECTIVE FUNCTION = 1.65524862801219"
    ## [1] "CURRNET METHOD: both"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'both' , seasonal.factor =  c( 98, 77 ) ...)"
    ## [1] "CURRENT both OBJECTIVE FUNCTION = 1.28101735351495"
    ## [1] "BEST method = 'both' PATH MEMBER = c( 98, 77 )"
    ## [1] "BEST both OBJECTIVE FUNCTION = 1.28101735351495"

![](man/figures/README-nns_forecasting-3.png)<!-- -->

    ## Package: healthyR.ts
    ## [1] "CURRNET METHOD: lin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 63 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 1.46685345194823"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 63, 98 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 0.995160780992406"
    ## [1] "BEST method = 'lin', seasonal.factor = c( 63, 98 )"
    ## [1] "BEST lin OBJECTIVE FUNCTION = 0.995160780992406"
    ## [1] "CURRNET METHOD: nonlin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'nonlin' , seasonal.factor =  c( 63, 98 ) ...)"
    ## [1] "CURRENT nonlin OBJECTIVE FUNCTION = 1.49380528516317"
    ## [1] "BEST method = 'nonlin' PATH MEMBER = c( 63, 98 )"
    ## [1] "BEST nonlin OBJECTIVE FUNCTION = 1.49380528516317"
    ## [1] "CURRNET METHOD: both"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'both' , seasonal.factor =  c( 63, 98 ) ...)"
    ## [1] "CURRENT both OBJECTIVE FUNCTION = 1.13652321119476"
    ## [1] "BEST method = 'both' PATH MEMBER = c( 63, 98 )"
    ## [1] "BEST both OBJECTIVE FUNCTION = 1.13652321119476"

![](man/figures/README-nns_forecasting-4.png)<!-- -->

    ## Package: healthyverse
    ## [1] "CURRNET METHOD: lin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 49 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 1.61178507280778"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 49, 77 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 1.46971162209198"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 49, 77, 70 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 1.41102778646951"
    ## [1] "BEST method = 'lin', seasonal.factor = c( 49, 77, 70 )"
    ## [1] "BEST lin OBJECTIVE FUNCTION = 1.41102778646951"
    ## [1] "CURRNET METHOD: nonlin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'nonlin' , seasonal.factor =  c( 49, 77, 70 ) ...)"
    ## [1] "CURRENT nonlin OBJECTIVE FUNCTION = 2.86694039303903"
    ## [1] "BEST method = 'nonlin' PATH MEMBER = c( 49, 77, 70 )"
    ## [1] "BEST nonlin OBJECTIVE FUNCTION = 2.86694039303903"
    ## [1] "CURRNET METHOD: both"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'both' , seasonal.factor =  c( 49, 77, 70 ) ...)"
    ## [1] "CURRENT both OBJECTIVE FUNCTION = 1.94412480398002"
    ## [1] "BEST method = 'both' PATH MEMBER = c( 49, 77, 70 )"
    ## [1] "BEST both OBJECTIVE FUNCTION = 1.94412480398002"

![](man/figures/README-nns_forecasting-5.png)<!-- -->

    ## Package: RandomWalker
    ## [1] "CURRNET METHOD: lin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 35 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 4.19120275995174"
    ## [1] "BEST method = 'lin', seasonal.factor = c( 35 )"
    ## [1] "BEST lin OBJECTIVE FUNCTION = 4.19120275995174"
    ## [1] "CURRNET METHOD: nonlin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'nonlin' , seasonal.factor =  c( 35 ) ...)"
    ## [1] "CURRENT nonlin OBJECTIVE FUNCTION = 7.44236896927737"
    ## [1] "BEST method = 'nonlin' PATH MEMBER = c( 35 )"
    ## [1] "BEST nonlin OBJECTIVE FUNCTION = 7.44236896927737"
    ## [1] "CURRNET METHOD: both"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'both' , seasonal.factor =  c( 35 ) ...)"
    ## [1] "CURRENT both OBJECTIVE FUNCTION = 5.90277666889115"
    ## [1] "BEST method = 'both' PATH MEMBER = c( 35 )"
    ## [1] "BEST both OBJECTIVE FUNCTION = 5.90277666889115"

![](man/figures/README-nns_forecasting-6.png)<!-- -->

    ## Package: tidyAML
    ## [1] "CURRNET METHOD: lin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 35 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 4.28272648576617"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 35, 56 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 4.08606581024449"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 35, 56, 70 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 4.08543998852225"
    ## [1] "BEST method = 'lin', seasonal.factor = c( 35, 56, 70 )"
    ## [1] "BEST lin OBJECTIVE FUNCTION = 4.08543998852225"
    ## [1] "CURRNET METHOD: nonlin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'nonlin' , seasonal.factor =  c( 35, 56, 70 ) ...)"
    ## [1] "CURRENT nonlin OBJECTIVE FUNCTION = 3.62940398646561"
    ## [1] "BEST method = 'nonlin' PATH MEMBER = c( 35, 56, 70 )"
    ## [1] "BEST nonlin OBJECTIVE FUNCTION = 3.62940398646561"
    ## [1] "CURRNET METHOD: both"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'both' , seasonal.factor =  c( 35, 56, 70 ) ...)"
    ## [1] "CURRENT both OBJECTIVE FUNCTION = 3.5456848004141"
    ## [1] "BEST method = 'both' PATH MEMBER = c( 35, 56, 70 )"
    ## [1] "BEST both OBJECTIVE FUNCTION = 3.5456848004141"

![](man/figures/README-nns_forecasting-7.png)<!-- -->

    ## Package: TidyDensity
    ## [1] "CURRNET METHOD: lin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 63 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 1.65820323898045"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 63, 77 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 1.38899598409811"
    ## [1] "BEST method = 'lin', seasonal.factor = c( 63, 77 )"
    ## [1] "BEST lin OBJECTIVE FUNCTION = 1.38899598409811"
    ## [1] "CURRNET METHOD: nonlin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'nonlin' , seasonal.factor =  c( 63, 77 ) ...)"
    ## [1] "CURRENT nonlin OBJECTIVE FUNCTION = 2.28312301099895"
    ## [1] "BEST method = 'nonlin' PATH MEMBER = c( 63, 77 )"
    ## [1] "BEST nonlin OBJECTIVE FUNCTION = 2.28312301099895"
    ## [1] "CURRNET METHOD: both"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'both' , seasonal.factor =  c( 63, 77 ) ...)"
    ## [1] "CURRENT both OBJECTIVE FUNCTION = 1.65588705279474"
    ## [1] "BEST method = 'both' PATH MEMBER = c( 63, 77 )"
    ## [1] "BEST both OBJECTIVE FUNCTION = 1.65588705279474"

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
    ## 1 healthyR.data <tibble [1,576 × 2]> <tibble [28 × 2]> <split [1548|28]>
    ## 2 healthyR      <tibble [1,569 × 2]> <tibble [28 × 2]> <split [1541|28]>
    ## 3 healthyR.ts   <tibble [1,513 × 2]> <tibble [28 × 2]> <split [1485|28]>
    ## 4 healthyverse  <tibble [1,484 × 2]> <tibble [28 × 2]> <split [1456|28]>
    ## 5 healthyR.ai   <tibble [1,308 × 2]> <tibble [28 × 2]> <split [1280|28]>
    ## 6 TidyDensity   <tibble [1,159 × 2]> <tibble [28 × 2]> <split [1131|28]>
    ## 7 tidyAML       <tibble [767 × 2]>   <tibble [28 × 2]> <split [739|28]> 
    ## 8 RandomWalker  <tibble [189 × 2]>   <tibble [28 × 2]> <split [161|28]>

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
| healthyR.data | 1 | ARIMA | Test | 0.6268727 | 124.22331 | 0.7011221 | 157.69174 | 0.8338631 | 0.1086716 |
| healthyR.data | 2 | LM | Test | 0.6860680 | 219.24744 | 0.7673287 | 138.97286 | 0.8209246 | 0.0013940 |
| healthyR.data | 3 | EARTH | Test | 0.8607489 | 230.20984 | 0.9626995 | 145.99402 | 1.0916999 | 0.0013940 |
| healthyR.data | 4 | NNAR | Test | 0.6452375 | 92.50307 | 0.7216621 | 162.06623 | 0.8719438 | 0.0927158 |
| healthyR | 1 | ARIMA | Test | 0.6776285 | 154.98464 | 0.7804714 | 159.75800 | 0.8052958 | 0.0532843 |
| healthyR | 2 | LM | Test | 0.6462776 | 102.51447 | 0.7443625 | 186.23896 | 0.7934613 | 0.0041157 |
| healthyR | 3 | EARTH | Test | 0.6481512 | 98.69295 | 0.7465204 | 192.08814 | 0.7941681 | 0.0041157 |
| healthyR | 4 | NNAR | Test | 0.6494161 | 218.16065 | 0.7479773 | 161.32057 | 0.7691404 | 0.0636557 |
| healthyR.ts | 1 | ARIMA | Test | 0.9401506 | 392.19319 | 0.7745116 | 133.06993 | 1.1383711 | 0.0180569 |
| healthyR.ts | 2 | LM | Test | 0.8939899 | 324.89331 | 0.7364837 | 134.58965 | 1.0976707 | 0.0180569 |
| healthyR.ts | 3 | EARTH | Test | 0.8909511 | 318.77516 | 0.7339803 | 134.80869 | 1.0947889 | NA |
| healthyR.ts | 4 | NNAR | Test | 0.8256410 | 98.06607 | 0.6801768 | 158.37740 | 1.0688907 | 0.0199553 |
| healthyverse | 1 | ARIMA | Test | 0.6367929 | 175.00413 | 0.9353299 | 116.63146 | 0.7809480 | 0.0190922 |
| healthyverse | 2 | LM | Test | 0.6584076 | 286.64918 | 0.9670779 | 102.93735 | 0.7610450 | 0.0197925 |
| healthyverse | 3 | EARTH | Test | 0.6776638 | 98.61092 | 0.9953616 | 142.09223 | 0.8798493 | 0.0197925 |
| healthyverse | 4 | NNAR | Test | 0.6165700 | 157.96269 | 0.9056263 | 113.06949 | 0.7737662 | 0.0703987 |
| healthyR.ai | 1 | ARIMA | Test | 0.7377143 | 121.21189 | 0.8323931 | 180.01559 | 0.8582381 | 0.1641930 |
| healthyR.ai | 2 | LM | Test | 0.6965523 | 105.97747 | 0.7859484 | 140.47659 | 0.8574633 | 0.0002140 |
| healthyR.ai | 3 | EARTH | Test | 1.5597729 | 671.05556 | 1.7599554 | 155.47628 | 1.7548600 | 0.0002140 |
| healthyR.ai | 4 | NNAR | Test | 0.6479033 | 117.21917 | 0.7310557 | 127.63029 | 0.7809508 | 0.2014723 |
| TidyDensity | 1 | ARIMA | Test | 0.6188433 | 227.05290 | 0.7106360 | 104.67877 | 0.7894697 | 0.0006148 |
| TidyDensity | 2 | LM | Test | 0.6584327 | 283.26560 | 0.7560976 | 105.43700 | 0.8201689 | 0.0510397 |
| TidyDensity | 3 | EARTH | Test | 0.6080497 | 212.25500 | 0.6982413 | 107.38241 | 0.7676752 | 0.0510397 |
| TidyDensity | 4 | NNAR | Test | 0.6078886 | 150.61067 | 0.6980564 | 127.57116 | 0.7712866 | 0.0350675 |
| tidyAML | 1 | ARIMA | Test | 0.6511500 | 271.55774 | 0.7416256 | 100.45259 | 0.7749993 | 0.0258965 |
| tidyAML | 2 | LM | Test | 0.6282474 | 274.29777 | 0.7155408 | 97.46748 | 0.7458586 | 0.0025204 |
| tidyAML | 3 | EARTH | Test | 0.6043370 | 149.34361 | 0.6883081 | 107.79693 | 0.7871801 | 0.0025204 |
| tidyAML | 4 | NNAR | Test | 0.6375403 | 256.01858 | 0.7261249 | 98.85859 | 0.7675518 | 0.0061645 |
| RandomWalker | 1 | ARIMA | Test | 0.9909993 | 110.29083 | 0.5793300 | 90.38137 | 1.4291631 | 0.0686336 |
| RandomWalker | 2 | LM | Test | 1.2188307 | 113.46863 | 0.7125183 | 191.70910 | 1.3323144 | 0.0001358 |
| RandomWalker | 3 | EARTH | Test | 1.1307333 | 90.35845 | 0.6610173 | 165.67952 | 1.2929749 | NA |
| RandomWalker | 4 | NNAR | Test | 1.2396694 | 116.93292 | 0.7247005 | 175.00574 | 1.3756446 | 0.0233811 |

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
    ## 1 healthyR.d…         2 LM          Test  0.686 219.  0.767 139.  0.821  0.00139
    ## 2 healthyR            4 NNAR        Test  0.649 218.  0.748 161.  0.769  0.0637 
    ## 3 healthyR.ts         4 NNAR        Test  0.826  98.1 0.680 158.  1.07   0.0200 
    ## 4 healthyver…         2 LM          Test  0.658 287.  0.967 103.  0.761  0.0198 
    ## 5 healthyR.ai         4 NNAR        Test  0.648 117.  0.731 128.  0.781  0.201  
    ## 6 TidyDensity         3 EARTH       Test  0.608 212.  0.698 107.  0.768  0.0510 
    ## 7 tidyAML             2 LM          Test  0.628 274.  0.716  97.5 0.746  0.00252
    ## 8 RandomWalk…         3 EARTH       Test  1.13   90.4 0.661 166.  1.29  NA

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
    ## 1 healthyR.data <tibble>     <tibble>     <split [1548|28]> <mdl_tm_t [1 × 5]>
    ## 2 healthyR      <tibble>     <tibble>     <split [1541|28]> <mdl_tm_t [1 × 5]>
    ## 3 healthyR.ts   <tibble>     <tibble>     <split [1485|28]> <mdl_tm_t [1 × 5]>
    ## 4 healthyverse  <tibble>     <tibble>     <split [1456|28]> <mdl_tm_t [1 × 5]>
    ## 5 healthyR.ai   <tibble>     <tibble>     <split [1280|28]> <mdl_tm_t [1 × 5]>
    ## 6 TidyDensity   <tibble>     <tibble>     <split [1131|28]> <mdl_tm_t [1 × 5]>
    ## 7 tidyAML       <tibble>     <tibble>     <split [739|28]>  <mdl_tm_t [1 × 5]>
    ## 8 RandomWalker  <tibble>     <tibble>     <split [161|28]>  <mdl_tm_t [1 × 5]>

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
