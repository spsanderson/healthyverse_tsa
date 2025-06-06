Time Series Analysis and Nested Modeling of the Healthyverse Packages
================
Steven P. Sanderson II, MPH - Date:
06 June, 2025

This analysis follows a *Nested Modeltime Workflow*.

## Get Data

``` r
glimpse(downloads_tbl)
```

    ## Rows: 141,559
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

The last day in the data set is 2025-06-04 23:03:28, the file was
birthed on: 2024-08-07 07:35:44, and at report knit time is -7235.46
hours old. Happy analyzing!

Now that we have our data lets take a look at it using the `skimr`
package.

``` r
skim(downloads_tbl)
```

|                                                  |               |
|:-------------------------------------------------|:--------------|
| Name                                             | downloads_tbl |
| Number of rows                                   | 141559        |
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
| r_version     |    102096 |          0.28 |   5 |   5 |     0 |       47 |          0 |
| r_arch        |    102096 |          0.28 |   3 |   7 |     0 |        5 |          0 |
| r_os          |    102096 |          0.28 |   7 |  15 |     0 |       22 |          0 |
| package       |         0 |          1.00 |   7 |  13 |     0 |        8 |          0 |
| version       |         0 |          1.00 |   5 |  17 |     0 |       60 |          0 |
| country       |     12033 |          0.91 |   2 |   2 |     0 |      163 |          0 |

**Variable type: Date**

| skim_variable | n_missing | complete_rate | min | max | median | n_unique |
|:---|---:|---:|:---|:---|:---|---:|
| date | 0 | 1 | 2020-11-23 | 2025-06-04 | 2023-06-29 | 1655 |

**Variable type: numeric**

| skim_variable | n_missing | complete_rate | mean | sd | p0 | p25 | p50 | p75 | p100 | hist |
|:---|---:|---:|---:|---:|---:|---:|---:|---:|---:|:---|
| size | 0 | 1 | 1133313.90 | 1517214.45 | 355 | 14701 | 289681 | 2367730 | 5677952 | ▇▁▂▁▁ |
| ip_id | 0 | 1 | 10433.91 | 18583.44 | 1 | 279 | 3042 | 11700 | 209747 | ▇▁▁▁▁ |

**Variable type: POSIXct**

| skim_variable | n_missing | complete_rate | min | max | median | n_unique |
|:---|---:|---:|:---|:---|:---|---:|
| date_time | 0 | 1 | 2020-11-23 09:00:41 | 2025-06-04 23:03:28 | 2023-06-29 08:32:50 | 86688 |

**Variable type: Timespan**

| skim_variable | n_missing | complete_rate | min | max |     median | n_unique |
|:--------------|----------:|--------------:|----:|----:|-----------:|---------:|
| time          |         0 |             1 |   0 |  59 | 12H 6M 43S |       60 |

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
    ## -146.59  -35.76  -10.98   26.79  814.71 
    ## 
    ## Coefficients:
    ##                                                      Estimate Std. Error
    ## (Intercept)                                        -1.517e+02  6.834e+01
    ## date                                                9.517e-03  3.621e-03
    ## lag(value, 1)                                       1.056e-01  2.436e-02
    ## lag(value, 7)                                       9.672e-02  2.524e-02
    ## lag(value, 14)                                      8.996e-02  2.524e-02
    ## lag(value, 21)                                      6.938e-02  2.530e-02
    ## lag(value, 28)                                      7.130e-02  2.519e-02
    ## lag(value, 35)                                      6.724e-02  2.529e-02
    ## lag(value, 42)                                      5.027e-02  2.543e-02
    ## lag(value, 49)                                      6.930e-02  2.531e-02
    ## month(date, label = TRUE).L                        -9.555e+00  5.110e+00
    ## month(date, label = TRUE).Q                         4.258e+00  5.125e+00
    ## month(date, label = TRUE).C                        -1.355e+01  5.132e+00
    ## month(date, label = TRUE)^4                        -7.472e+00  5.172e+00
    ## month(date, label = TRUE)^5                        -1.088e+01  5.118e+00
    ## month(date, label = TRUE)^6                        -3.074e+00  5.219e+00
    ## month(date, label = TRUE)^7                        -7.719e+00  5.096e+00
    ## month(date, label = TRUE)^8                        -3.669e+00  5.102e+00
    ## month(date, label = TRUE)^9                         6.209e+00  5.114e+00
    ## month(date, label = TRUE)^10                        3.147e+00  5.083e+00
    ## month(date, label = TRUE)^11                       -5.124e+00  5.212e+00
    ## fourier_vec(date, type = "sin", K = 1, period = 7) -1.142e+01  2.332e+00
    ## fourier_vec(date, type = "cos", K = 1, period = 7)  7.859e+00  2.455e+00
    ##                                                    t value Pr(>|t|)    
    ## (Intercept)                                         -2.219 0.026599 *  
    ## date                                                 2.628 0.008660 ** 
    ## lag(value, 1)                                        4.337 1.54e-05 ***
    ## lag(value, 7)                                        3.832 0.000132 ***
    ## lag(value, 14)                                       3.564 0.000376 ***
    ## lag(value, 21)                                       2.742 0.006171 ** 
    ## lag(value, 28)                                       2.830 0.004717 ** 
    ## lag(value, 35)                                       2.659 0.007924 ** 
    ## lag(value, 42)                                       1.977 0.048211 *  
    ## lag(value, 49)                                       2.738 0.006245 ** 
    ## month(date, label = TRUE).L                         -1.870 0.061667 .  
    ## month(date, label = TRUE).Q                          0.831 0.406171    
    ## month(date, label = TRUE).C                         -2.640 0.008370 ** 
    ## month(date, label = TRUE)^4                         -1.445 0.148745    
    ## month(date, label = TRUE)^5                         -2.126 0.033658 *  
    ## month(date, label = TRUE)^6                         -0.589 0.555969    
    ## month(date, label = TRUE)^7                         -1.515 0.130062    
    ## month(date, label = TRUE)^8                         -0.719 0.472204    
    ## month(date, label = TRUE)^9                          1.214 0.224897    
    ## month(date, label = TRUE)^10                         0.619 0.535904    
    ## month(date, label = TRUE)^11                        -0.983 0.325715    
    ## fourier_vec(date, type = "sin", K = 1, period = 7)  -4.896 1.08e-06 ***
    ## fourier_vec(date, type = "cos", K = 1, period = 7)   3.201 0.001394 ** 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 58.72 on 1583 degrees of freedom
    ##   (49 observations deleted due to missingness)
    ## Multiple R-squared:  0.2393, Adjusted R-squared:  0.2288 
    ## F-statistic: 22.64 on 22 and 1583 DF,  p-value: < 2.2e-16

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
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 3.26651149084827"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 21, 28 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 3.12106798932822"
    ## [1] "BEST method = 'lin', seasonal.factor = c( 21, 28 )"
    ## [1] "BEST lin OBJECTIVE FUNCTION = 3.12106798932822"
    ## [1] "CURRNET METHOD: nonlin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'nonlin' , seasonal.factor =  c( 21, 28 ) ...)"
    ## [1] "CURRENT nonlin OBJECTIVE FUNCTION = 6.43136315752562"
    ## [1] "BEST method = 'nonlin' PATH MEMBER = c( 21, 28 )"
    ## [1] "BEST nonlin OBJECTIVE FUNCTION = 6.43136315752562"
    ## [1] "CURRNET METHOD: both"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'both' , seasonal.factor =  c( 21, 28 ) ...)"
    ## [1] "CURRENT both OBJECTIVE FUNCTION = 4.07651239302096"
    ## [1] "BEST method = 'both' PATH MEMBER = c( 21, 28 )"
    ## [1] "BEST both OBJECTIVE FUNCTION = 4.07651239302096"

![](man/figures/README-nns_forecasting-1.png)<!-- -->

    ## Package: healthyR.ai
    ## [1] "CURRNET METHOD: lin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 63 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 3.48766537868191"
    ## [1] "BEST method = 'lin', seasonal.factor = c( 63 )"
    ## [1] "BEST lin OBJECTIVE FUNCTION = 3.48766537868191"
    ## [1] "CURRNET METHOD: nonlin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'nonlin' , seasonal.factor =  c( 63 ) ...)"
    ## [1] "CURRENT nonlin OBJECTIVE FUNCTION = 3.85057766244335"
    ## [1] "BEST method = 'nonlin' PATH MEMBER = c( 63 )"
    ## [1] "BEST nonlin OBJECTIVE FUNCTION = 3.85057766244335"
    ## [1] "CURRNET METHOD: both"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'both' , seasonal.factor =  c( 63 ) ...)"
    ## [1] "CURRENT both OBJECTIVE FUNCTION = 3.52809458926814"
    ## [1] "BEST method = 'both' PATH MEMBER = c( 63 )"
    ## [1] "BEST both OBJECTIVE FUNCTION = 3.52809458926814"

![](man/figures/README-nns_forecasting-2.png)<!-- -->

    ## Package: healthyR.data
    ## [1] "CURRNET METHOD: lin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 63 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 2.52391700750457"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 63, 91 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 2.33718834914628"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 63, 91, 84 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 2.22212458171834"
    ## [1] "BEST method = 'lin', seasonal.factor = c( 63, 91, 84 )"
    ## [1] "BEST lin OBJECTIVE FUNCTION = 2.22212458171834"
    ## [1] "CURRNET METHOD: nonlin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'nonlin' , seasonal.factor =  c( 63, 91, 84 ) ...)"
    ## [1] "CURRENT nonlin OBJECTIVE FUNCTION = 2.4529037031055"
    ## [1] "BEST method = 'nonlin' PATH MEMBER = c( 63, 91, 84 )"
    ## [1] "BEST nonlin OBJECTIVE FUNCTION = 2.4529037031055"
    ## [1] "CURRNET METHOD: both"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'both' , seasonal.factor =  c( 63, 91, 84 ) ...)"
    ## [1] "CURRENT both OBJECTIVE FUNCTION = 2.18997866575732"
    ## [1] "BEST method = 'both' PATH MEMBER = c( 63, 91, 84 )"
    ## [1] "BEST both OBJECTIVE FUNCTION = 2.18997866575732"

![](man/figures/README-nns_forecasting-3.png)<!-- -->

    ## Package: healthyR.ts
    ## [1] "CURRNET METHOD: lin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 21 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 3.34567366130984"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 21, 63 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 2.734198821719"
    ## [1] "BEST method = 'lin', seasonal.factor = c( 21, 63 )"
    ## [1] "BEST lin OBJECTIVE FUNCTION = 2.734198821719"
    ## [1] "CURRNET METHOD: nonlin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'nonlin' , seasonal.factor =  c( 21, 63 ) ...)"
    ## [1] "CURRENT nonlin OBJECTIVE FUNCTION = 3.83233020560009"
    ## [1] "BEST method = 'nonlin' PATH MEMBER = c( 21, 63 )"
    ## [1] "BEST nonlin OBJECTIVE FUNCTION = 3.83233020560009"
    ## [1] "CURRNET METHOD: both"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'both' , seasonal.factor =  c( 21, 63 ) ...)"
    ## [1] "CURRENT both OBJECTIVE FUNCTION = 2.80378733202437"
    ## [1] "BEST method = 'both' PATH MEMBER = c( 21, 63 )"
    ## [1] "BEST both OBJECTIVE FUNCTION = 2.80378733202437"

![](man/figures/README-nns_forecasting-4.png)<!-- -->

    ## Package: healthyverse
    ## [1] "CURRNET METHOD: lin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 28 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 5.14152914579533"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 28, 63 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 5.05607889312891"
    ## [1] "BEST method = 'lin', seasonal.factor = c( 28, 63 )"
    ## [1] "BEST lin OBJECTIVE FUNCTION = 5.05607889312891"
    ## [1] "CURRNET METHOD: nonlin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'nonlin' , seasonal.factor =  c( 28, 63 ) ...)"
    ## [1] "CURRENT nonlin OBJECTIVE FUNCTION = 3.24968688407641"
    ## [1] "BEST method = 'nonlin' PATH MEMBER = c( 28, 63 )"
    ## [1] "BEST nonlin OBJECTIVE FUNCTION = 3.24968688407641"
    ## [1] "CURRNET METHOD: both"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'both' , seasonal.factor =  c( 28, 63 ) ...)"
    ## [1] "CURRENT both OBJECTIVE FUNCTION = 3.69998741591692"
    ## [1] "BEST method = 'both' PATH MEMBER = c( 28, 63 )"
    ## [1] "BEST both OBJECTIVE FUNCTION = 3.69998741591692"

![](man/figures/README-nns_forecasting-5.png)<!-- -->

    ## Package: RandomWalker
    ## [1] "CURRNET METHOD: lin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 63 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 2.67661232550974"
    ## [1] "BEST method = 'lin', seasonal.factor = c( 63 )"
    ## [1] "BEST lin OBJECTIVE FUNCTION = 2.67661232550974"
    ## [1] "CURRNET METHOD: nonlin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'nonlin' , seasonal.factor =  c( 63 ) ...)"
    ## [1] "CURRENT nonlin OBJECTIVE FUNCTION = 2.75689463606239"
    ## [1] "BEST method = 'nonlin' PATH MEMBER = c( 63 )"
    ## [1] "BEST nonlin OBJECTIVE FUNCTION = 2.75689463606239"
    ## [1] "CURRNET METHOD: both"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'both' , seasonal.factor =  c( 63 ) ...)"
    ## [1] "CURRENT both OBJECTIVE FUNCTION = 2.54928861031143"
    ## [1] "BEST method = 'both' PATH MEMBER = c( 63 )"
    ## [1] "BEST both OBJECTIVE FUNCTION = 2.54928861031143"

![](man/figures/README-nns_forecasting-6.png)<!-- -->

    ## Package: tidyAML
    ## [1] "CURRNET METHOD: lin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 56 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 6.45086612152691"
    ## [1] "BEST method = 'lin', seasonal.factor = c( 56 )"
    ## [1] "BEST lin OBJECTIVE FUNCTION = 6.45086612152691"
    ## [1] "CURRNET METHOD: nonlin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'nonlin' , seasonal.factor =  c( 56 ) ...)"
    ## [1] "CURRENT nonlin OBJECTIVE FUNCTION = 5.63441031960448"
    ## [1] "BEST method = 'nonlin' PATH MEMBER = c( 56 )"
    ## [1] "BEST nonlin OBJECTIVE FUNCTION = 5.63441031960448"
    ## [1] "CURRNET METHOD: both"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'both' , seasonal.factor =  c( 56 ) ...)"
    ## [1] "CURRENT both OBJECTIVE FUNCTION = 6.05162837657097"
    ## [1] "BEST method = 'both' PATH MEMBER = c( 56 )"
    ## [1] "BEST both OBJECTIVE FUNCTION = 6.05162837657097"

![](man/figures/README-nns_forecasting-7.png)<!-- -->

    ## Package: TidyDensity
    ## [1] "CURRNET METHOD: lin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 63 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 3.78357154452549"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 63, 28 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 3.4664675875333"
    ## [1] "BEST method = 'lin', seasonal.factor = c( 63, 28 )"
    ## [1] "BEST lin OBJECTIVE FUNCTION = 3.4664675875333"
    ## [1] "CURRNET METHOD: nonlin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'nonlin' , seasonal.factor =  c( 63, 28 ) ...)"
    ## [1] "CURRENT nonlin OBJECTIVE FUNCTION = 3.40040022431512"
    ## [1] "BEST method = 'nonlin' PATH MEMBER = c( 63, 28 )"
    ## [1] "BEST nonlin OBJECTIVE FUNCTION = 3.40040022431512"
    ## [1] "CURRNET METHOD: both"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'both' , seasonal.factor =  c( 63, 28 ) ...)"
    ## [1] "CURRENT both OBJECTIVE FUNCTION = 3.00596395500974"
    ## [1] "BEST method = 'both' PATH MEMBER = c( 63, 28 )"
    ## [1] "BEST both OBJECTIVE FUNCTION = 3.00596395500974"

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
    ## 1 healthyR.data <tibble [1,647 × 2]> <tibble [28 × 2]> <split [1619|28]>
    ## 2 healthyR      <tibble [1,641 × 2]> <tibble [28 × 2]> <split [1613|28]>
    ## 3 healthyR.ts   <tibble [1,585 × 2]> <tibble [28 × 2]> <split [1557|28]>
    ## 4 healthyverse  <tibble [1,555 × 2]> <tibble [28 × 2]> <split [1527|28]>
    ## 5 healthyR.ai   <tibble [1,380 × 2]> <tibble [28 × 2]> <split [1352|28]>
    ## 6 TidyDensity   <tibble [1,231 × 2]> <tibble [28 × 2]> <split [1203|28]>
    ## 7 tidyAML       <tibble [839 × 2]>   <tibble [28 × 2]> <split [811|28]> 
    ## 8 RandomWalker  <tibble [261 × 2]>   <tibble [28 × 2]> <split [233|28]>

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
| healthyR.data | 1 | ARIMA | Test | 0.6876580 | 123.64152 | 0.6985476 | 136.94431 | 0.9016236 | 0.0214849 |
| healthyR.data | 2 | LM | Test | 0.6804147 | 168.58491 | 0.6911896 | 116.96040 | 0.8765938 | 0.1265891 |
| healthyR.data | 3 | EARTH | Test | 0.6830540 | 230.28027 | 0.6938706 | 107.08374 | 0.8520287 | 0.1265891 |
| healthyR.data | 4 | NNAR | Test | 0.7810477 | 94.53407 | 0.7934162 | 167.76097 | 1.0281858 | 0.0025603 |
| healthyR | 1 | ARIMA | Test | 0.7283173 | 108.12155 | 0.8669563 | 155.64909 | 0.9094321 | 0.0219803 |
| healthyR | 2 | LM | Test | 0.7117817 | 96.05977 | 0.8472731 | 167.06893 | 0.8941113 | 0.0641533 |
| healthyR | 3 | EARTH | Test | 0.6539969 | 137.56282 | 0.7784885 | 132.75964 | 0.7918300 | 0.0641533 |
| healthyR | 4 | NNAR | Test | 0.7342424 | 149.13245 | 0.8740093 | 165.85687 | 0.8853119 | 0.0230107 |
| healthyR.ts | 1 | ARIMA | Test | 0.6120638 | 108.74026 | 0.8107762 | 160.33142 | 0.7729317 | 0.0586710 |
| healthyR.ts | 2 | LM | Test | 0.8997572 | 200.04903 | 1.1918721 | 160.33677 | 1.1044338 | 0.0586710 |
| healthyR.ts | 3 | EARTH | Test | 0.5187812 | 195.98142 | 0.6872085 | 93.05956 | 0.6675505 | 0.0586710 |
| healthyR.ts | 4 | NNAR | Test | 0.7352131 | 110.99413 | 0.9739071 | 172.84811 | 0.9467858 | 0.0000884 |
| healthyverse | 1 | ARIMA | Test | 0.6729384 | 110.63857 | 1.1198229 | 81.57692 | 0.8622756 | 0.0243709 |
| healthyverse | 2 | LM | Test | 0.6229826 | 118.55997 | 1.0366926 | 73.63384 | 0.7683853 | 0.0398155 |
| healthyverse | 3 | EARTH | Test | 0.6352131 | 110.82782 | 1.0570450 | 75.88639 | 0.7928916 | 0.0398155 |
| healthyverse | 4 | NNAR | Test | 0.7377053 | 86.32649 | 1.2276002 | 95.36800 | 0.9333277 | 0.0737567 |
| healthyR.ai | 1 | ARIMA | Test | 0.6191620 | 112.53962 | 0.9955084 | 169.19563 | 0.7822174 | 0.0254431 |
| healthyR.ai | 2 | LM | Test | 0.5478618 | 89.06592 | 0.8808698 | 122.19640 | 0.7168418 | 0.1246246 |
| healthyR.ai | 3 | EARTH | Test | 1.7425111 | 513.08153 | 2.8016651 | 192.61431 | 1.8825961 | 0.1246246 |
| healthyR.ai | 4 | NNAR | Test | 0.5718637 | 126.13373 | 0.9194607 | 125.35798 | 0.7439900 | 0.1632823 |
| TidyDensity | 1 | ARIMA | Test | 0.4363755 | 172.69474 | 1.0662880 | 105.76602 | 0.5864990 | 0.0341837 |
| TidyDensity | 2 | LM | Test | 0.6130572 | 279.63332 | 1.4980115 | 119.10570 | 0.7586089 | 0.0514886 |
| TidyDensity | 3 | EARTH | Test | 0.4180020 | 162.20679 | 1.0213921 | 103.71747 | 0.5687155 | 0.0514886 |
| TidyDensity | 4 | NNAR | Test | 0.3540349 | 100.47605 | 0.8650878 | 110.89135 | 0.4704917 | 0.0252136 |
| tidyAML | 1 | ARIMA | Test | 0.8077666 | 116.02296 | 0.8393816 | 103.77598 | 1.0944428 | 0.0000343 |
| tidyAML | 2 | LM | Test | 0.8033581 | 121.69785 | 0.8348005 | 100.57740 | 1.1043749 | 0.1206481 |
| tidyAML | 3 | EARTH | Test | 0.7886286 | 98.37132 | 0.8194946 | 112.36126 | 1.0490528 | 0.1206481 |
| tidyAML | 4 | NNAR | Test | 0.7876275 | 128.44723 | 0.8184542 | 100.79429 | 1.0859891 | 0.0359473 |
| RandomWalker | 1 | ARIMA | Test | 1.0606023 | 129.09776 | 0.5972227 | 119.61158 | 1.3632078 | 0.0477785 |
| RandomWalker | 2 | LM | Test | 1.1374284 | 104.31418 | 0.6404833 | 188.71455 | 1.3440342 | 0.0646275 |
| RandomWalker | 3 | EARTH | Test | 1.0977295 | 98.42683 | 0.6181290 | 168.99458 | 1.3443662 | NA |
| RandomWalker | 4 | NNAR | Test | 1.4509470 | 224.37026 | 0.8170250 | 187.31342 | 1.6077470 | 0.2043212 |

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
    ## 1 healthyR.data         3 EARTH       Test  0.683 230.  0.694 107.  0.852 0.127 
    ## 2 healthyR              3 EARTH       Test  0.654 138.  0.778 133.  0.792 0.0642
    ## 3 healthyR.ts           3 EARTH       Test  0.519 196.  0.687  93.1 0.668 0.0587
    ## 4 healthyverse          2 LM          Test  0.623 119.  1.04   73.6 0.768 0.0398
    ## 5 healthyR.ai           2 LM          Test  0.548  89.1 0.881 122.  0.717 0.125 
    ## 6 TidyDensity           4 NNAR        Test  0.354 100.  0.865 111.  0.470 0.0252
    ## 7 tidyAML               3 EARTH       Test  0.789  98.4 0.819 112.  1.05  0.121 
    ## 8 RandomWalker          2 LM          Test  1.14  104.  0.640 189.  1.34  0.0646

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
    ## 1 healthyR.data <tibble>     <tibble>     <split [1619|28]> <mdl_tm_t [1 × 5]>
    ## 2 healthyR      <tibble>     <tibble>     <split [1613|28]> <mdl_tm_t [1 × 5]>
    ## 3 healthyR.ts   <tibble>     <tibble>     <split [1557|28]> <mdl_tm_t [1 × 5]>
    ## 4 healthyverse  <tibble>     <tibble>     <split [1527|28]> <mdl_tm_t [1 × 5]>
    ## 5 healthyR.ai   <tibble>     <tibble>     <split [1352|28]> <mdl_tm_t [1 × 5]>
    ## 6 TidyDensity   <tibble>     <tibble>     <split [1203|28]> <mdl_tm_t [1 × 5]>
    ## 7 tidyAML       <tibble>     <tibble>     <split [811|28]>  <mdl_tm_t [1 × 5]>
    ## 8 RandomWalker  <tibble>     <tibble>     <split [233|28]>  <mdl_tm_t [1 × 5]>

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
