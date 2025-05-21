Time Series Analysis and Nested Modeling of the Healthyverse Packages
================
Steven P. Sanderson II, MPH - Date:
20 May, 2025

This analysis follows a *Nested Modeltime Workflow*.

## Get Data

``` r
glimpse(downloads_tbl)
```

    ## Rows: 140,380
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

The last day in the data set is 2025-05-18 21:30:32, the file was
birthed on: 2022-07-02 23:58:17.511888, and at report knit time is
-2.521754^{4} hours old. Happy analyzing!

Now that we have our data lets take a look at it using the `skimr`
package.

``` r
skim(downloads_tbl)
```

|                                                  |               |
|:-------------------------------------------------|:--------------|
| Name                                             | downloads_tbl |
| Number of rows                                   | 140380        |
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
| r_version     |    101191 |          0.28 |   5 |   5 |     0 |       46 |          0 |
| r_arch        |    101191 |          0.28 |   3 |   7 |     0 |        5 |          0 |
| r_os          |    101191 |          0.28 |   7 |  15 |     0 |       22 |          0 |
| package       |         0 |          1.00 |   7 |  13 |     0 |        8 |          0 |
| version       |         0 |          1.00 |   5 |  17 |     0 |       60 |          0 |
| country       |     11975 |          0.91 |   2 |   2 |     0 |      163 |          0 |

**Variable type: Date**

| skim_variable | n_missing | complete_rate | min | max | median | n_unique |
|:---|---:|---:|:---|:---|:---|---:|
| date | 0 | 1 | 2020-11-23 | 2025-05-18 | 2023-06-23 | 1638 |

**Variable type: numeric**

| skim_variable | n_missing | complete_rate | mean | sd | p0 | p25 | p50 | p75 | p100 | hist |
|:---|---:|---:|---:|---:|---:|---:|---:|---:|---:|:---|
| size | 0 | 1 | 1133806.40 | 1518702.64 | 355 | 14701 | 289680 | 2367742.0 | 5677952 | ▇▁▂▁▁ |
| ip_id | 0 | 1 | 10424.09 | 18562.37 | 1 | 289 | 3057 | 11681.5 | 209747 | ▇▁▁▁▁ |

**Variable type: POSIXct**

| skim_variable | n_missing | complete_rate | min | max | median | n_unique |
|:---|---:|---:|:---|:---|:---|---:|
| date_time | 0 | 1 | 2020-11-23 09:00:41 | 2025-05-18 21:30:32 | 2023-06-23 23:30:44 | 85836 |

**Variable type: Timespan**

| skim_variable | n_missing | complete_rate | min | max | median | n_unique |
|:--------------|----------:|--------------:|----:|----:|-------:|---------:|
| time          |         0 |             1 |   0 |  59 |     57 |       60 |

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
    ## -147.32  -35.65  -11.24   26.63  814.92 
    ## 
    ## Coefficients:
    ##                                                      Estimate Std. Error
    ## (Intercept)                                        -1.820e+02  6.982e+01
    ## date                                                1.115e-02  3.701e-03
    ## lag(value, 1)                                       1.024e-01  2.452e-02
    ## lag(value, 7)                                       9.472e-02  2.536e-02
    ## lag(value, 14)                                      8.903e-02  2.539e-02
    ## lag(value, 21)                                      6.689e-02  2.550e-02
    ## lag(value, 28)                                      7.182e-02  2.541e-02
    ## lag(value, 35)                                      6.525e-02  2.550e-02
    ## lag(value, 42)                                      4.806e-02  2.568e-02
    ## lag(value, 49)                                      6.925e-02  2.551e-02
    ## month(date, label = TRUE).L                        -9.989e+00  5.122e+00
    ## month(date, label = TRUE).Q                         3.131e+00  5.161e+00
    ## month(date, label = TRUE).C                        -1.271e+01  5.154e+00
    ## month(date, label = TRUE)^4                        -6.836e+00  5.191e+00
    ## month(date, label = TRUE)^5                        -1.195e+01  5.152e+00
    ## month(date, label = TRUE)^6                        -3.363e+00  5.236e+00
    ## month(date, label = TRUE)^7                        -6.658e+00  5.130e+00
    ## month(date, label = TRUE)^8                        -4.146e+00  5.135e+00
    ## month(date, label = TRUE)^9                         5.412e+00  5.137e+00
    ## month(date, label = TRUE)^10                        4.241e+00  5.147e+00
    ## month(date, label = TRUE)^11                       -5.605e+00  5.279e+00
    ## fourier_vec(date, type = "sin", K = 1, period = 7) -1.180e+01  2.352e+00
    ## fourier_vec(date, type = "cos", K = 1, period = 7)  8.277e+00  2.476e+00
    ##                                                    t value Pr(>|t|)    
    ## (Intercept)                                         -2.607 0.009228 ** 
    ## date                                                 3.012 0.002636 ** 
    ## lag(value, 1)                                        4.178  3.1e-05 ***
    ## lag(value, 7)                                        3.735 0.000195 ***
    ## lag(value, 14)                                       3.506 0.000468 ***
    ## lag(value, 21)                                       2.623 0.008806 ** 
    ## lag(value, 28)                                       2.826 0.004768 ** 
    ## lag(value, 35)                                       2.559 0.010598 *  
    ## lag(value, 42)                                       1.872 0.061432 .  
    ## lag(value, 49)                                       2.714 0.006711 ** 
    ## month(date, label = TRUE).L                         -1.950 0.051325 .  
    ## month(date, label = TRUE).Q                          0.607 0.544091    
    ## month(date, label = TRUE).C                         -2.466 0.013773 *  
    ## month(date, label = TRUE)^4                         -1.317 0.188121    
    ## month(date, label = TRUE)^5                         -2.320 0.020494 *  
    ## month(date, label = TRUE)^6                         -0.642 0.520785    
    ## month(date, label = TRUE)^7                         -1.298 0.194492    
    ## month(date, label = TRUE)^8                         -0.808 0.419482    
    ## month(date, label = TRUE)^9                          1.053 0.292288    
    ## month(date, label = TRUE)^10                         0.824 0.410047    
    ## month(date, label = TRUE)^11                        -1.062 0.288531    
    ## fourier_vec(date, type = "sin", K = 1, period = 7)  -5.018  5.8e-07 ***
    ## fourier_vec(date, type = "cos", K = 1, period = 7)   3.342 0.000850 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 58.81 on 1566 degrees of freedom
    ##   (49 observations deleted due to missingness)
    ## Multiple R-squared:  0.2419, Adjusted R-squared:  0.2312 
    ## F-statistic: 22.71 on 22 and 1566 DF,  p-value: < 2.2e-16

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
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 28 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 3.7233873476822"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 28, 63 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 3.3448840233692"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 28, 63, 84 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 3.24321608937299"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 28, 63, 84, 91 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 3.20213889588583"
    ## [1] "BEST method = 'lin', seasonal.factor = c( 28, 63, 84, 91 )"
    ## [1] "BEST lin OBJECTIVE FUNCTION = 3.20213889588583"
    ## [1] "CURRNET METHOD: nonlin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'nonlin' , seasonal.factor =  c( 28, 63, 84, 91 ) ...)"
    ## [1] "CURRENT nonlin OBJECTIVE FUNCTION = 3.54496603694326"
    ## [1] "BEST method = 'nonlin' PATH MEMBER = c( 28, 63, 84, 91 )"
    ## [1] "BEST nonlin OBJECTIVE FUNCTION = 3.54496603694326"
    ## [1] "CURRNET METHOD: both"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'both' , seasonal.factor =  c( 28, 63, 84, 91 ) ...)"
    ## [1] "CURRENT both OBJECTIVE FUNCTION = 3.26086926742917"
    ## [1] "BEST method = 'both' PATH MEMBER = c( 28, 63, 84, 91 )"
    ## [1] "BEST both OBJECTIVE FUNCTION = 3.26086926742917"

![](man/figures/README-nns_forecasting-1.png)<!-- -->

    ## Package: healthyR.ai
    ## [1] "CURRNET METHOD: lin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 63 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 5.41978321182018"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 63, 84 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 5.31176102065558"
    ## [1] "BEST method = 'lin', seasonal.factor = c( 63, 84 )"
    ## [1] "BEST lin OBJECTIVE FUNCTION = 5.31176102065558"
    ## [1] "CURRNET METHOD: nonlin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'nonlin' , seasonal.factor =  c( 63, 84 ) ...)"
    ## [1] "CURRENT nonlin OBJECTIVE FUNCTION = 6.04285351319947"
    ## [1] "BEST method = 'nonlin' PATH MEMBER = c( 63, 84 )"
    ## [1] "BEST nonlin OBJECTIVE FUNCTION = 6.04285351319947"
    ## [1] "CURRNET METHOD: both"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'both' , seasonal.factor =  c( 63, 84 ) ...)"
    ## [1] "CURRENT both OBJECTIVE FUNCTION = 5.58877714796107"
    ## [1] "BEST method = 'both' PATH MEMBER = c( 63, 84 )"
    ## [1] "BEST both OBJECTIVE FUNCTION = 5.58877714796107"

![](man/figures/README-nns_forecasting-2.png)<!-- -->

    ## Package: healthyR.data
    ## [1] "CURRNET METHOD: lin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 63 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 2.14704551678285"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 63, 91 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 1.96009076001007"
    ## [1] "BEST method = 'lin', seasonal.factor = c( 63, 91 )"
    ## [1] "BEST lin OBJECTIVE FUNCTION = 1.96009076001007"
    ## [1] "CURRNET METHOD: nonlin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'nonlin' , seasonal.factor =  c( 63, 91 ) ...)"
    ## [1] "CURRENT nonlin OBJECTIVE FUNCTION = 2.06634413521084"
    ## [1] "BEST method = 'nonlin' PATH MEMBER = c( 63, 91 )"
    ## [1] "BEST nonlin OBJECTIVE FUNCTION = 2.06634413521084"
    ## [1] "CURRNET METHOD: both"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'both' , seasonal.factor =  c( 63, 91 ) ...)"
    ## [1] "CURRENT both OBJECTIVE FUNCTION = 1.86482183965626"
    ## [1] "BEST method = 'both' PATH MEMBER = c( 63, 91 )"
    ## [1] "BEST both OBJECTIVE FUNCTION = 1.86482183965626"

![](man/figures/README-nns_forecasting-3.png)<!-- -->

    ## Package: healthyR.ts
    ## [1] "CURRNET METHOD: lin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 63 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 5.40540120331844"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 63, 28 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 4.91682094497051"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 63, 28, 91 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 4.84538284016149"
    ## [1] "BEST method = 'lin', seasonal.factor = c( 63, 28, 91 )"
    ## [1] "BEST lin OBJECTIVE FUNCTION = 4.84538284016149"
    ## [1] "CURRNET METHOD: nonlin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'nonlin' , seasonal.factor =  c( 63, 28, 91 ) ...)"
    ## [1] "CURRENT nonlin OBJECTIVE FUNCTION = 5.34311570928013"
    ## [1] "BEST method = 'nonlin' PATH MEMBER = c( 63, 28, 91 )"
    ## [1] "BEST nonlin OBJECTIVE FUNCTION = 5.34311570928013"
    ## [1] "CURRNET METHOD: both"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'both' , seasonal.factor =  c( 63, 28, 91 ) ...)"
    ## [1] "CURRENT both OBJECTIVE FUNCTION = 4.89086125173687"
    ## [1] "BEST method = 'both' PATH MEMBER = c( 63, 28, 91 )"
    ## [1] "BEST both OBJECTIVE FUNCTION = 4.89086125173687"

![](man/figures/README-nns_forecasting-4.png)<!-- -->

    ## Package: healthyverse
    ## [1] "CURRNET METHOD: lin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 63 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 8.83387182367987"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 63, 98 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 8.05460881608005"
    ## [1] "BEST method = 'lin', seasonal.factor = c( 63, 98 )"
    ## [1] "BEST lin OBJECTIVE FUNCTION = 8.05460881608005"
    ## [1] "CURRNET METHOD: nonlin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'nonlin' , seasonal.factor =  c( 63, 98 ) ...)"
    ## [1] "CURRENT nonlin OBJECTIVE FUNCTION = 4.19461618703076"
    ## [1] "BEST method = 'nonlin' PATH MEMBER = c( 63, 98 )"
    ## [1] "BEST nonlin OBJECTIVE FUNCTION = 4.19461618703076"
    ## [1] "CURRNET METHOD: both"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'both' , seasonal.factor =  c( 63, 98 ) ...)"
    ## [1] "CURRENT both OBJECTIVE FUNCTION = 5.38405473327736"
    ## [1] "BEST method = 'both' PATH MEMBER = c( 63, 98 )"
    ## [1] "BEST both OBJECTIVE FUNCTION = 5.38405473327736"

![](man/figures/README-nns_forecasting-5.png)<!-- -->

    ## Package: RandomWalker
    ## [1] "CURRNET METHOD: lin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 35 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 2.90968642284249"
    ## [1] "BEST method = 'lin', seasonal.factor = c( 35 )"
    ## [1] "BEST lin OBJECTIVE FUNCTION = 2.90968642284249"
    ## [1] "CURRNET METHOD: nonlin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'nonlin' , seasonal.factor =  c( 35 ) ...)"
    ## [1] "CURRENT nonlin OBJECTIVE FUNCTION = 2.75978866191848"
    ## [1] "BEST method = 'nonlin' PATH MEMBER = c( 35 )"
    ## [1] "BEST nonlin OBJECTIVE FUNCTION = 2.75978866191848"
    ## [1] "CURRNET METHOD: both"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'both' , seasonal.factor =  c( 35 ) ...)"
    ## [1] "CURRENT both OBJECTIVE FUNCTION = 2.65411809877855"
    ## [1] "BEST method = 'both' PATH MEMBER = c( 35 )"
    ## [1] "BEST both OBJECTIVE FUNCTION = 2.65411809877855"

![](man/figures/README-nns_forecasting-6.png)<!-- -->

    ## Package: tidyAML
    ## [1] "CURRNET METHOD: lin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 84 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 5.77762301953343"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 84, 28 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 5.43701893724094"
    ## [1] "BEST method = 'lin', seasonal.factor = c( 84, 28 )"
    ## [1] "BEST lin OBJECTIVE FUNCTION = 5.43701893724094"
    ## [1] "CURRNET METHOD: nonlin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'nonlin' , seasonal.factor =  c( 84, 28 ) ...)"
    ## [1] "CURRENT nonlin OBJECTIVE FUNCTION = 3.59557951061313"
    ## [1] "BEST method = 'nonlin' PATH MEMBER = c( 84, 28 )"
    ## [1] "BEST nonlin OBJECTIVE FUNCTION = 3.59557951061313"
    ## [1] "CURRNET METHOD: both"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'both' , seasonal.factor =  c( 84, 28 ) ...)"
    ## [1] "CURRENT both OBJECTIVE FUNCTION = 4.28803005920649"
    ## [1] "BEST method = 'both' PATH MEMBER = c( 84, 28 )"
    ## [1] "BEST both OBJECTIVE FUNCTION = 4.28803005920649"

![](man/figures/README-nns_forecasting-7.png)<!-- -->

    ## Package: TidyDensity
    ## [1] "CURRNET METHOD: lin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 28 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 3.91275305953441"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 28, 84 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 3.3912477484182"
    ## [1] "BEST method = 'lin', seasonal.factor = c( 28, 84 )"
    ## [1] "BEST lin OBJECTIVE FUNCTION = 3.3912477484182"
    ## [1] "CURRNET METHOD: nonlin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'nonlin' , seasonal.factor =  c( 28, 84 ) ...)"
    ## [1] "CURRENT nonlin OBJECTIVE FUNCTION = 4.4287522086345"
    ## [1] "BEST method = 'nonlin' PATH MEMBER = c( 28, 84 )"
    ## [1] "BEST nonlin OBJECTIVE FUNCTION = 4.4287522086345"
    ## [1] "CURRNET METHOD: both"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'both' , seasonal.factor =  c( 28, 84 ) ...)"
    ## [1] "CURRENT both OBJECTIVE FUNCTION = 3.55984995900128"
    ## [1] "BEST method = 'both' PATH MEMBER = c( 28, 84 )"
    ## [1] "BEST both OBJECTIVE FUNCTION = 3.55984995900128"

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
    ## 1 healthyR.data <tibble [1,631 × 2]> <tibble [28 × 2]> <split [1603|28]>
    ## 2 healthyR      <tibble [1,624 × 2]> <tibble [28 × 2]> <split [1596|28]>
    ## 3 healthyR.ts   <tibble [1,568 × 2]> <tibble [28 × 2]> <split [1540|28]>
    ## 4 healthyverse  <tibble [1,538 × 2]> <tibble [28 × 2]> <split [1510|28]>
    ## 5 healthyR.ai   <tibble [1,363 × 2]> <tibble [28 × 2]> <split [1335|28]>
    ## 6 TidyDensity   <tibble [1,214 × 2]> <tibble [28 × 2]> <split [1186|28]>
    ## 7 tidyAML       <tibble [822 × 2]>   <tibble [28 × 2]> <split [794|28]> 
    ## 8 RandomWalker  <tibble [244 × 2]>   <tibble [28 × 2]> <split [216|28]>

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
| healthyR.data | 1 | ARIMA | Test | 0.5449227 | 110.89551 | 0.5971410 | 110.58777 | 0.7880002 | 0.0582904 |
| healthyR.data | 2 | LM | Test | 0.5886832 | 192.49885 | 0.6450949 | 110.39343 | 0.7973061 | 0.0034735 |
| healthyR.data | 3 | EARTH | Test | 0.5841409 | 215.42476 | 0.6401174 | 104.62372 | 0.8030351 | 0.0034735 |
| healthyR.data | 4 | NNAR | Test | 0.6998354 | 115.64113 | 0.7668986 | 167.65016 | 0.8927004 | 0.0002318 |
| healthyR | 1 | ARIMA | Test | 0.6002982 | 111.01699 | 0.7446259 | 172.51475 | 0.7879542 | 0.0215304 |
| healthyR | 2 | LM | Test | 0.5697203 | 92.99437 | 0.7066962 | 158.72319 | 0.7538193 | 0.0017156 |
| healthyR | 3 | EARTH | Test | 0.5352709 | 105.44835 | 0.6639642 | 123.32893 | 0.7177821 | 0.0017156 |
| healthyR | 4 | NNAR | Test | 0.5550760 | 91.54497 | 0.6885311 | 131.08783 | 0.7426670 | 0.0169852 |
| healthyR.ts | 1 | ARIMA | Test | 0.6530953 | 104.63827 | 0.6867385 | 176.32951 | 0.8187006 | 0.0004463 |
| healthyR.ts | 2 | LM | Test | 0.8817657 | 256.11488 | 0.9271885 | 162.58741 | 1.0379498 | 0.0004463 |
| healthyR.ts | 3 | EARTH | Test | 0.5683801 | 234.65993 | 0.5976593 | 107.96338 | 0.7627328 | 0.0004463 |
| healthyR.ts | 4 | NNAR | Test | 0.6554067 | 108.84073 | 0.6891690 | 172.16890 | 0.8303308 | 0.0011171 |
| healthyverse | 1 | ARIMA | Test | 0.5444886 | 215.45336 | 0.9087932 | 86.01116 | 0.7013165 | 0.0014316 |
| healthyverse | 2 | LM | Test | 0.5240813 | 294.52214 | 0.8747319 | 77.46229 | 0.6408356 | 0.0001763 |
| healthyverse | 3 | EARTH | Test | 0.5748224 | 190.88080 | 0.9594228 | 92.90357 | 0.7384525 | 0.0001763 |
| healthyverse | 4 | NNAR | Test | 0.5471374 | 128.55449 | 0.9132144 | 92.86603 | 0.7165042 | 0.1617429 |
| healthyR.ai | 1 | ARIMA | Test | 0.6824872 | 119.05286 | 0.9113116 | 156.72012 | 1.0106757 | 0.1428746 |
| healthyR.ai | 2 | LM | Test | 0.6501409 | 121.17827 | 0.8681202 | 134.03991 | 0.9758033 | 0.2132951 |
| healthyR.ai | 3 | EARTH | Test | 0.6441680 | 131.49567 | 0.8601447 | 124.85751 | 0.9841148 | 0.2132951 |
| healthyR.ai | 4 | NNAR | Test | 0.6644242 | 124.07846 | 0.8871924 | 147.42261 | 0.9838652 | 0.0226303 |
| TidyDensity | 1 | ARIMA | Test | 0.4110177 | 314.49055 | 0.8332988 | 107.74406 | 0.5145236 | 0.0014186 |
| TidyDensity | 2 | LM | Test | 0.5423632 | 498.44166 | 1.0995892 | 115.98930 | 0.6517200 | 0.0034402 |
| TidyDensity | 3 | EARTH | Test | 0.4176275 | 325.29204 | 0.8466996 | 111.10810 | 0.5067391 | 0.0034402 |
| TidyDensity | 4 | NNAR | Test | 0.3754920 | 159.63285 | 0.7612739 | 123.45965 | 0.4890528 | 0.0183201 |
| tidyAML | 1 | ARIMA | Test | 0.7967831 | 155.15825 | 0.9324489 | 112.69707 | 1.0521567 | 0.0047567 |
| tidyAML | 2 | LM | Test | 0.8210691 | 193.30739 | 0.9608700 | 106.55321 | 1.0766199 | 0.1588154 |
| tidyAML | 3 | EARTH | Test | 0.8066132 | 120.08368 | 0.9439527 | 131.56916 | 1.0304009 | 0.1588154 |
| tidyAML | 4 | NNAR | Test | 0.8318367 | 127.49807 | 0.9734709 | 120.13441 | 1.0876391 | 0.0286919 |
| RandomWalker | 1 | ARIMA | Test | 1.1935006 | 118.69410 | 0.5993420 | 128.25262 | 1.4910240 | 0.0318823 |
| RandomWalker | 2 | LM | Test | 1.2117275 | 98.49178 | 0.6084951 | 190.16469 | 1.4294223 | 0.0066608 |
| RandomWalker | 3 | EARTH | Test | 1.2014501 | 94.28697 | 0.6033340 | 168.27803 | 1.4457685 | NA |
| RandomWalker | 4 | NNAR | Test | 1.3076731 | 133.59345 | 0.6566762 | 160.95517 | 1.4293161 | 0.0028410 |

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
    ## 1 healthyR.da…         1 ARIMA       Test  0.545  111. 0.597 111.  0.788 5.83e-2
    ## 2 healthyR             3 EARTH       Test  0.535  105. 0.664 123.  0.718 1.72e-3
    ## 3 healthyR.ts          3 EARTH       Test  0.568  235. 0.598 108.  0.763 4.46e-4
    ## 4 healthyverse         2 LM          Test  0.524  295. 0.875  77.5 0.641 1.76e-4
    ## 5 healthyR.ai          2 LM          Test  0.650  121. 0.868 134.  0.976 2.13e-1
    ## 6 TidyDensity          4 NNAR        Test  0.375  160. 0.761 123.  0.489 1.83e-2
    ## 7 tidyAML              3 EARTH       Test  0.807  120. 0.944 132.  1.03  1.59e-1
    ## 8 RandomWalker         4 NNAR        Test  1.31   134. 0.657 161.  1.43  2.84e-3

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
    ## 1 healthyR.data <tibble>     <tibble>     <split [1603|28]> <mdl_tm_t [1 × 5]>
    ## 2 healthyR      <tibble>     <tibble>     <split [1596|28]> <mdl_tm_t [1 × 5]>
    ## 3 healthyR.ts   <tibble>     <tibble>     <split [1540|28]> <mdl_tm_t [1 × 5]>
    ## 4 healthyverse  <tibble>     <tibble>     <split [1510|28]> <mdl_tm_t [1 × 5]>
    ## 5 healthyR.ai   <tibble>     <tibble>     <split [1335|28]> <mdl_tm_t [1 × 5]>
    ## 6 TidyDensity   <tibble>     <tibble>     <split [1186|28]> <mdl_tm_t [1 × 5]>
    ## 7 tidyAML       <tibble>     <tibble>     <split [794|28]>  <mdl_tm_t [1 × 5]>
    ## 8 RandomWalker  <tibble>     <tibble>     <split [216|28]>  <mdl_tm_t [1 × 5]>

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
