Time Series Analysis and Nested Modeling of the Healthyverse Packages
================
Steven P. Sanderson II, MPH - Date:
26 May, 2025

This analysis follows a *Nested Modeltime Workflow*.

## Get Data

``` r
glimpse(downloads_tbl)
```

    ## Rows: 140,935
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

The last day in the data set is 2025-05-24 23:27:07, the file was
birthed on: 2022-07-02 23:58:17.511888, and at report knit time is
-2.536348^{4} hours old. Happy analyzing!

Now that we have our data lets take a look at it using the `skimr`
package.

``` r
skim(downloads_tbl)
```

|                                                  |               |
|:-------------------------------------------------|:--------------|
| Name                                             | downloads_tbl |
| Number of rows                                   | 140935        |
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
| r_version     |    101623 |          0.28 |   5 |   5 |     0 |       47 |          0 |
| r_arch        |    101623 |          0.28 |   3 |   7 |     0 |        5 |          0 |
| r_os          |    101623 |          0.28 |   7 |  15 |     0 |       22 |          0 |
| package       |         0 |          1.00 |   7 |  13 |     0 |        8 |          0 |
| version       |         0 |          1.00 |   5 |  17 |     0 |       60 |          0 |
| country       |     12000 |          0.91 |   2 |   2 |     0 |      163 |          0 |

**Variable type: Date**

| skim_variable | n_missing | complete_rate | min | max | median | n_unique |
|:---|---:|---:|:---|:---|:---|---:|
| date | 0 | 1 | 2020-11-23 | 2025-05-24 | 2023-06-27 | 1644 |

**Variable type: numeric**

| skim_variable | n_missing | complete_rate | mean | sd | p0 | p25 | p50 | p75 | p100 | hist |
|:---|---:|---:|---:|---:|---:|---:|---:|---:|---:|:---|
| size | 0 | 1 | 1133594.00 | 1518176.07 | 355 | 14701 | 289681 | 2367737 | 5677952 | ▇▁▂▁▁ |
| ip_id | 0 | 1 | 10430.67 | 18581.78 | 1 | 279 | 3053 | 11668 | 209747 | ▇▁▁▁▁ |

**Variable type: POSIXct**

| skim_variable | n_missing | complete_rate | min | max | median | n_unique |
|:---|---:|---:|:---|:---|:---|---:|
| date_time | 0 | 1 | 2020-11-23 09:00:41 | 2025-05-24 23:27:07 | 2023-06-27 10:52:08 | 86208 |

**Variable type: Timespan**

| skim_variable | n_missing | complete_rate | min | max |     median | n_unique |
|:--------------|----------:|--------------:|----:|----:|-----------:|---------:|
| time          |         0 |             1 |   0 |  59 | 12H 6M 52S |       60 |

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
    ## -148.11  -35.76  -11.07   26.65  814.85 
    ## 
    ## Coefficients:
    ##                                                      Estimate Std. Error
    ## (Intercept)                                        -1.756e+02  6.926e+01
    ## date                                                1.080e-02  3.671e-03
    ## lag(value, 1)                                       1.028e-01  2.446e-02
    ## lag(value, 7)                                       9.557e-02  2.531e-02
    ## lag(value, 14)                                      8.951e-02  2.531e-02
    ## lag(value, 21)                                      6.802e-02  2.541e-02
    ## lag(value, 28)                                      7.028e-02  2.535e-02
    ## lag(value, 35)                                      6.372e-02  2.542e-02
    ## lag(value, 42)                                      5.065e-02  2.554e-02
    ## lag(value, 49)                                      7.119e-02  2.545e-02
    ## month(date, label = TRUE).L                        -9.929e+00  5.115e+00
    ## month(date, label = TRUE).Q                         3.321e+00  5.148e+00
    ## month(date, label = TRUE).C                        -1.291e+01  5.143e+00
    ## month(date, label = TRUE)^4                        -6.930e+00  5.186e+00
    ## month(date, label = TRUE)^5                        -1.172e+01  5.135e+00
    ## month(date, label = TRUE)^6                        -3.402e+00  5.232e+00
    ## month(date, label = TRUE)^7                        -6.809e+00  5.115e+00
    ## month(date, label = TRUE)^8                        -4.025e+00  5.122e+00
    ## month(date, label = TRUE)^9                         5.539e+00  5.130e+00
    ## month(date, label = TRUE)^10                        3.958e+00  5.117e+00
    ## month(date, label = TRUE)^11                       -5.380e+00  5.259e+00
    ## fourier_vec(date, type = "sin", K = 1, period = 7) -1.165e+01  2.343e+00
    ## fourier_vec(date, type = "cos", K = 1, period = 7)  8.087e+00  2.467e+00
    ##                                                    t value Pr(>|t|)    
    ## (Intercept)                                         -2.536 0.011317 *  
    ## date                                                 2.942 0.003310 ** 
    ## lag(value, 1)                                        4.203 2.78e-05 ***
    ## lag(value, 7)                                        3.777 0.000165 ***
    ## lag(value, 14)                                       3.537 0.000416 ***
    ## lag(value, 21)                                       2.677 0.007504 ** 
    ## lag(value, 28)                                       2.772 0.005634 ** 
    ## lag(value, 35)                                       2.506 0.012300 *  
    ## lag(value, 42)                                       1.983 0.047514 *  
    ## lag(value, 49)                                       2.797 0.005224 ** 
    ## month(date, label = TRUE).L                         -1.941 0.052442 .  
    ## month(date, label = TRUE).Q                          0.645 0.518948    
    ## month(date, label = TRUE).C                         -2.510 0.012173 *  
    ## month(date, label = TRUE)^4                         -1.336 0.181616    
    ## month(date, label = TRUE)^5                         -2.282 0.022609 *  
    ## month(date, label = TRUE)^6                         -0.650 0.515589    
    ## month(date, label = TRUE)^7                         -1.331 0.183280    
    ## month(date, label = TRUE)^8                         -0.786 0.432099    
    ## month(date, label = TRUE)^9                          1.080 0.280418    
    ## month(date, label = TRUE)^10                         0.773 0.439402    
    ## month(date, label = TRUE)^11                        -1.023 0.306473    
    ## fourier_vec(date, type = "sin", K = 1, period = 7)  -4.970 7.44e-07 ***
    ## fourier_vec(date, type = "cos", K = 1, period = 7)   3.278 0.001070 ** 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 58.76 on 1572 degrees of freedom
    ##   (49 observations deleted due to missingness)
    ## Multiple R-squared:  0.2414, Adjusted R-squared:  0.2308 
    ## F-statistic: 22.74 on 22 and 1572 DF,  p-value: < 2.2e-16

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
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 3.11857711457296"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 63, 21 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 2.70110141623351"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 63, 21, 91 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 2.68661026087555"
    ## [1] "BEST method = 'lin', seasonal.factor = c( 63, 21, 91 )"
    ## [1] "BEST lin OBJECTIVE FUNCTION = 2.68661026087555"
    ## [1] "CURRNET METHOD: nonlin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'nonlin' , seasonal.factor =  c( 63, 21, 91 ) ...)"
    ## [1] "CURRENT nonlin OBJECTIVE FUNCTION = 3.83144492381053"
    ## [1] "BEST method = 'nonlin' PATH MEMBER = c( 63, 21, 91 )"
    ## [1] "BEST nonlin OBJECTIVE FUNCTION = 3.83144492381053"
    ## [1] "CURRNET METHOD: both"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'both' , seasonal.factor =  c( 63, 21, 91 ) ...)"
    ## [1] "CURRENT both OBJECTIVE FUNCTION = 2.96784191070993"
    ## [1] "BEST method = 'both' PATH MEMBER = c( 63, 21, 91 )"
    ## [1] "BEST both OBJECTIVE FUNCTION = 2.96784191070993"

![](man/figures/README-nns_forecasting-1.png)<!-- -->

    ## Package: healthyR.ai
    ## [1] "CURRNET METHOD: lin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 63 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 4.32107237654539"
    ## [1] "BEST method = 'lin', seasonal.factor = c( 63 )"
    ## [1] "BEST lin OBJECTIVE FUNCTION = 4.32107237654539"
    ## [1] "CURRNET METHOD: nonlin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'nonlin' , seasonal.factor =  c( 63 ) ...)"
    ## [1] "CURRENT nonlin OBJECTIVE FUNCTION = 4.04108858724247"
    ## [1] "BEST method = 'nonlin' PATH MEMBER = c( 63 )"
    ## [1] "BEST nonlin OBJECTIVE FUNCTION = 4.04108858724247"
    ## [1] "CURRNET METHOD: both"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'both' , seasonal.factor =  c( 63 ) ...)"
    ## [1] "CURRENT both OBJECTIVE FUNCTION = 3.97270955436567"
    ## [1] "BEST method = 'both' PATH MEMBER = c( 63 )"
    ## [1] "BEST both OBJECTIVE FUNCTION = 3.97270955436567"

![](man/figures/README-nns_forecasting-2.png)<!-- -->

    ## Package: healthyR.data
    ## [1] "CURRNET METHOD: lin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 63 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 2.40002150623241"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 63, 91 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 2.27793077088968"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 63, 91, 21 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 2.19255054813582"
    ## [1] "BEST method = 'lin', seasonal.factor = c( 63, 91, 21 )"
    ## [1] "BEST lin OBJECTIVE FUNCTION = 2.19255054813582"
    ## [1] "CURRNET METHOD: nonlin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'nonlin' , seasonal.factor =  c( 63, 91, 21 ) ...)"
    ## [1] "CURRENT nonlin OBJECTIVE FUNCTION = 2.07143277449232"
    ## [1] "BEST method = 'nonlin' PATH MEMBER = c( 63, 91, 21 )"
    ## [1] "BEST nonlin OBJECTIVE FUNCTION = 2.07143277449232"
    ## [1] "CURRNET METHOD: both"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'both' , seasonal.factor =  c( 63, 91, 21 ) ...)"
    ## [1] "CURRENT both OBJECTIVE FUNCTION = 1.94647584469269"
    ## [1] "BEST method = 'both' PATH MEMBER = c( 63, 91, 21 )"
    ## [1] "BEST both OBJECTIVE FUNCTION = 1.94647584469269"

![](man/figures/README-nns_forecasting-3.png)<!-- -->

    ## Package: healthyR.ts
    ## [1] "CURRNET METHOD: lin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 63 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 4.53558497927055"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 63, 28 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 3.83350040296141"
    ## [1] "BEST method = 'lin', seasonal.factor = c( 63, 28 )"
    ## [1] "BEST lin OBJECTIVE FUNCTION = 3.83350040296141"
    ## [1] "CURRNET METHOD: nonlin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'nonlin' , seasonal.factor =  c( 63, 28 ) ...)"
    ## [1] "CURRENT nonlin OBJECTIVE FUNCTION = 3.65936975855898"
    ## [1] "BEST method = 'nonlin' PATH MEMBER = c( 63, 28 )"
    ## [1] "BEST nonlin OBJECTIVE FUNCTION = 3.65936975855898"
    ## [1] "CURRNET METHOD: both"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'both' , seasonal.factor =  c( 63, 28 ) ...)"
    ## [1] "CURRENT both OBJECTIVE FUNCTION = 3.40872624934808"
    ## [1] "BEST method = 'both' PATH MEMBER = c( 63, 28 )"
    ## [1] "BEST both OBJECTIVE FUNCTION = 3.40872624934808"

![](man/figures/README-nns_forecasting-4.png)<!-- -->

    ## Package: healthyverse
    ## [1] "CURRNET METHOD: lin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 63 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 6.26020223231895"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 63, 98 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 6.17459074470273"
    ## [1] "BEST method = 'lin', seasonal.factor = c( 63, 98 )"
    ## [1] "BEST lin OBJECTIVE FUNCTION = 6.17459074470273"
    ## [1] "CURRNET METHOD: nonlin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'nonlin' , seasonal.factor =  c( 63, 98 ) ...)"
    ## [1] "CURRENT nonlin OBJECTIVE FUNCTION = 3.85269417621126"
    ## [1] "BEST method = 'nonlin' PATH MEMBER = c( 63, 98 )"
    ## [1] "BEST nonlin OBJECTIVE FUNCTION = 3.85269417621126"
    ## [1] "CURRNET METHOD: both"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'both' , seasonal.factor =  c( 63, 98 ) ...)"
    ## [1] "CURRENT both OBJECTIVE FUNCTION = 4.53241322268977"
    ## [1] "BEST method = 'both' PATH MEMBER = c( 63, 98 )"
    ## [1] "BEST both OBJECTIVE FUNCTION = 4.53241322268977"

![](man/figures/README-nns_forecasting-5.png)<!-- -->

    ## Package: RandomWalker
    ## [1] "CURRNET METHOD: lin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 63 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 2.05065706426661"
    ## [1] "BEST method = 'lin', seasonal.factor = c( 63 )"
    ## [1] "BEST lin OBJECTIVE FUNCTION = 2.05065706426661"
    ## [1] "CURRNET METHOD: nonlin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'nonlin' , seasonal.factor =  c( 63 ) ...)"
    ## [1] "CURRENT nonlin OBJECTIVE FUNCTION = 2.44578838866411"
    ## [1] "BEST method = 'nonlin' PATH MEMBER = c( 63 )"
    ## [1] "BEST nonlin OBJECTIVE FUNCTION = 2.44578838866411"
    ## [1] "CURRNET METHOD: both"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'both' , seasonal.factor =  c( 63 ) ...)"
    ## [1] "CURRENT both OBJECTIVE FUNCTION = 2.11352016665116"
    ## [1] "BEST method = 'both' PATH MEMBER = c( 63 )"
    ## [1] "BEST both OBJECTIVE FUNCTION = 2.11352016665116"

![](man/figures/README-nns_forecasting-6.png)<!-- -->

    ## Package: tidyAML
    ## [1] "CURRNET METHOD: lin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 84 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 6.10513533791857"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 84, 28 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 5.90732398568793"
    ## [1] "BEST method = 'lin', seasonal.factor = c( 84, 28 )"
    ## [1] "BEST lin OBJECTIVE FUNCTION = 5.90732398568793"
    ## [1] "CURRNET METHOD: nonlin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'nonlin' , seasonal.factor =  c( 84, 28 ) ...)"
    ## [1] "CURRENT nonlin OBJECTIVE FUNCTION = 3.80727781079411"
    ## [1] "BEST method = 'nonlin' PATH MEMBER = c( 84, 28 )"
    ## [1] "BEST nonlin OBJECTIVE FUNCTION = 3.80727781079411"
    ## [1] "CURRNET METHOD: both"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'both' , seasonal.factor =  c( 84, 28 ) ...)"
    ## [1] "CURRENT both OBJECTIVE FUNCTION = 4.6138826292463"
    ## [1] "BEST method = 'both' PATH MEMBER = c( 84, 28 )"
    ## [1] "BEST both OBJECTIVE FUNCTION = 4.6138826292463"

![](man/figures/README-nns_forecasting-7.png)<!-- -->

    ## Package: TidyDensity
    ## [1] "CURRNET METHOD: lin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 28 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 3.72384888688765"
    ## [1] "BEST method = 'lin', seasonal.factor = c( 28 )"
    ## [1] "BEST lin OBJECTIVE FUNCTION = 3.72384888688765"
    ## [1] "CURRNET METHOD: nonlin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'nonlin' , seasonal.factor =  c( 28 ) ...)"
    ## [1] "CURRENT nonlin OBJECTIVE FUNCTION = 4.04805064007856"
    ## [1] "BEST method = 'nonlin' PATH MEMBER = c( 28 )"
    ## [1] "BEST nonlin OBJECTIVE FUNCTION = 4.04805064007856"
    ## [1] "CURRNET METHOD: both"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'both' , seasonal.factor =  c( 28 ) ...)"
    ## [1] "CURRENT both OBJECTIVE FUNCTION = 3.16048133244997"
    ## [1] "BEST method = 'both' PATH MEMBER = c( 28 )"
    ## [1] "BEST both OBJECTIVE FUNCTION = 3.16048133244997"

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
    ## 1 healthyR.data <tibble [1,637 × 2]> <tibble [28 × 2]> <split [1609|28]>
    ## 2 healthyR      <tibble [1,630 × 2]> <tibble [28 × 2]> <split [1602|28]>
    ## 3 healthyR.ts   <tibble [1,574 × 2]> <tibble [28 × 2]> <split [1546|28]>
    ## 4 healthyverse  <tibble [1,544 × 2]> <tibble [28 × 2]> <split [1516|28]>
    ## 5 healthyR.ai   <tibble [1,369 × 2]> <tibble [28 × 2]> <split [1341|28]>
    ## 6 TidyDensity   <tibble [1,220 × 2]> <tibble [28 × 2]> <split [1192|28]>
    ## 7 tidyAML       <tibble [828 × 2]>   <tibble [28 × 2]> <split [800|28]> 
    ## 8 RandomWalker  <tibble [250 × 2]>   <tibble [28 × 2]> <split [222|28]>

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
| healthyR.data | 1 | ARIMA | Test | 0.5805760 | 106.11225 | 0.6757449 | 125.80088 | 0.8035990 | 0.0144014 |
| healthyR.data | 2 | LM | Test | 0.6006451 | 151.72904 | 0.6991038 | 110.85186 | 0.8054117 | 0.0273133 |
| healthyR.data | 3 | EARTH | Test | 0.6307124 | 199.20050 | 0.7340997 | 103.76798 | 0.8514229 | 0.0273133 |
| healthyR.data | 4 | NNAR | Test | 0.6904587 | 105.13509 | 0.8036398 | 173.01824 | 0.8772874 | 0.0009904 |
| healthyR | 1 | ARIMA | Test | 0.5904633 | 93.41919 | 0.6746919 | 158.95207 | 0.7758951 | 0.0075687 |
| healthyR | 2 | LM | Test | 0.5873382 | 93.70729 | 0.6711209 | 161.73349 | 0.7600020 | 0.0754633 |
| healthyR | 3 | EARTH | Test | 0.5511977 | 106.55506 | 0.6298250 | 126.73341 | 0.7184416 | 0.0754633 |
| healthyR | 4 | NNAR | Test | 0.5835343 | 91.49791 | 0.6667744 | 140.11670 | 0.7598566 | 0.0005837 |
| healthyR.ts | 1 | ARIMA | Test | 0.5802511 | 99.49405 | 0.7056579 | 195.62952 | 0.7200507 | 0.0428517 |
| healthyR.ts | 2 | LM | Test | 0.8227060 | 235.75753 | 1.0005134 | 159.68959 | 0.9601644 | 0.0428517 |
| healthyR.ts | 3 | EARTH | Test | 0.5314744 | 206.58848 | 0.6463394 | 103.38544 | 0.7195743 | 0.0428517 |
| healthyR.ts | 4 | NNAR | Test | 0.6004219 | 105.01316 | 0.7301882 | 174.42084 | 0.7406810 | 0.0283169 |
| healthyverse | 1 | ARIMA | Test | 0.5081798 | 128.76640 | 0.8322424 | 76.35602 | 0.6798535 | 0.0471616 |
| healthyverse | 2 | LM | Test | 0.4871447 | 157.43440 | 0.7977933 | 70.87131 | 0.6129965 | 0.0996152 |
| healthyverse | 3 | EARTH | Test | 0.5205801 | 105.25816 | 0.8525503 | 82.16984 | 0.6977840 | 0.0996152 |
| healthyverse | 4 | NNAR | Test | 0.5338901 | 104.91795 | 0.8743479 | 86.04936 | 0.7036595 | 0.0257528 |
| healthyR.ai | 1 | ARIMA | Test | 0.6622957 | 114.44599 | 0.8676646 | 172.24651 | 0.9802716 | 0.2084494 |
| healthyR.ai | 2 | LM | Test | 0.5787424 | 99.77943 | 0.7582026 | 127.64082 | 0.8892381 | 0.1632661 |
| healthyR.ai | 3 | EARTH | Test | 3.5144901 | 1484.05965 | 4.6042863 | 178.76942 | 3.8103981 | 0.1632661 |
| healthyR.ai | 4 | NNAR | Test | 0.6380859 | 117.33793 | 0.8359477 | 134.43857 | 0.9391789 | 0.0002686 |
| TidyDensity | 1 | ARIMA | Test | 0.4003175 | 290.14003 | 0.8622970 | 112.82358 | 0.5076620 | 0.0117259 |
| TidyDensity | 2 | LM | Test | 0.5523224 | 497.72366 | 1.1897207 | 119.86997 | 0.6664464 | 0.0022589 |
| TidyDensity | 3 | EARTH | Test | 0.4078076 | 318.22748 | 0.8784310 | 113.50948 | 0.5034661 | 0.0022589 |
| TidyDensity | 4 | NNAR | Test | 0.3490255 | 163.29278 | 0.7518124 | 127.14674 | 0.4321854 | 0.0341156 |
| tidyAML | 1 | ARIMA | Test | 0.8380754 | 100.46225 | 0.9769852 | 115.36844 | 1.0927923 | 0.0235716 |
| tidyAML | 2 | LM | Test | 0.8427622 | 120.10530 | 0.9824488 | 104.51931 | 1.1148316 | 0.2314505 |
| tidyAML | 3 | EARTH | Test | 0.8404473 | 91.20384 | 0.9797502 | 131.99368 | 1.0500728 | 0.2314505 |
| tidyAML | 4 | NNAR | Test | 0.8810904 | 121.91739 | 1.0271299 | 111.52150 | 1.1396550 | 0.1709947 |
| RandomWalker | 1 | ARIMA | Test | 1.3451386 | 209.97875 | 0.7280822 | 137.16833 | 1.6660488 | 0.0011416 |
| RandomWalker | 2 | LM | Test | 1.1829178 | 100.27139 | 0.6402771 | 195.38543 | 1.4063438 | 0.0019737 |
| RandomWalker | 3 | EARTH | Test | 1.1770542 | 105.80832 | 0.6371033 | 172.14313 | 1.4263900 | NA |
| RandomWalker | 4 | NNAR | Test | 1.2036898 | 139.64354 | 0.6515203 | 162.49669 | 1.3709838 | 0.0244399 |

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
    ## 1 healthyR.data         1 ARIMA       Test  0.581 106.  0.676 126.  0.804 0.0144
    ## 2 healthyR              3 EARTH       Test  0.551 107.  0.630 127.  0.718 0.0755
    ## 3 healthyR.ts           3 EARTH       Test  0.531 207.  0.646 103.  0.720 0.0429
    ## 4 healthyverse          2 LM          Test  0.487 157.  0.798  70.9 0.613 0.0996
    ## 5 healthyR.ai           2 LM          Test  0.579  99.8 0.758 128.  0.889 0.163 
    ## 6 TidyDensity           4 NNAR        Test  0.349 163.  0.752 127.  0.432 0.0341
    ## 7 tidyAML               3 EARTH       Test  0.840  91.2 0.980 132.  1.05  0.231 
    ## 8 RandomWalker          4 NNAR        Test  1.20  140.  0.652 162.  1.37  0.0244

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
    ## 1 healthyR.data <tibble>     <tibble>     <split [1609|28]> <mdl_tm_t [1 × 5]>
    ## 2 healthyR      <tibble>     <tibble>     <split [1602|28]> <mdl_tm_t [1 × 5]>
    ## 3 healthyR.ts   <tibble>     <tibble>     <split [1546|28]> <mdl_tm_t [1 × 5]>
    ## 4 healthyverse  <tibble>     <tibble>     <split [1516|28]> <mdl_tm_t [1 × 5]>
    ## 5 healthyR.ai   <tibble>     <tibble>     <split [1341|28]> <mdl_tm_t [1 × 5]>
    ## 6 TidyDensity   <tibble>     <tibble>     <split [1192|28]> <mdl_tm_t [1 × 5]>
    ## 7 tidyAML       <tibble>     <tibble>     <split [800|28]>  <mdl_tm_t [1 × 5]>
    ## 8 RandomWalker  <tibble>     <tibble>     <split [222|28]>  <mdl_tm_t [1 × 5]>

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
