Time Series Analysis and Nested Modeling of the Healthyverse Packages
================
Steven P. Sanderson II, MPH - Date:
16 April, 2025

This analysis follows a *Nested Modeltime Workflow*.

## Get Data

``` r
glimpse(downloads_tbl)
```

    ## Rows: 137,021
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

The last day in the data set is 2025-04-14 23:52:45, the file was
birthed on: 2022-07-02 23:58:17.511888, and at report knit time is
-2.440391^{4} hours old. Happy analyzing!

Now that we have our data lets take a look at it using the `skimr`
package.

``` r
skim(downloads_tbl)
```

|                                                  |               |
|:-------------------------------------------------|:--------------|
| Name                                             | downloads_tbl |
| Number of rows                                   | 137021        |
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
| r_version     |     98586 |          0.28 |   5 |   5 |     0 |       46 |          0 |
| r_arch        |     98586 |          0.28 |   3 |   7 |     0 |        5 |          0 |
| r_os          |     98586 |          0.28 |   7 |  15 |     0 |       21 |          0 |
| package       |         0 |          1.00 |   7 |  13 |     0 |        8 |          0 |
| version       |         0 |          1.00 |   5 |  17 |     0 |       60 |          0 |
| country       |     11558 |          0.92 |   2 |   2 |     0 |      163 |          0 |

**Variable type: Date**

| skim_variable | n_missing | complete_rate | min | max | median | n_unique |
|:---|---:|---:|:---|:---|:---|---:|
| date | 0 | 1 | 2020-11-23 | 2025-04-14 | 2023-06-01 | 1604 |

**Variable type: numeric**

| skim_variable | n_missing | complete_rate | mean | sd | p0 | p25 | p50 | p75 | p100 | hist |
|:---|---:|---:|---:|---:|---:|---:|---:|---:|---:|:---|
| size | 0 | 1 | 1135304.81 | 1522787.69 | 355 | 14701 | 274853 | 2367774 | 5677952 | ▇▁▂▁▁ |
| ip_id | 0 | 1 | 10362.81 | 18391.36 | 1 | 299 | 3064 | 11655 | 209747 | ▇▁▁▁▁ |

**Variable type: POSIXct**

| skim_variable | n_missing | complete_rate | min | max | median | n_unique |
|:---|---:|---:|:---|:---|:---|---:|
| date_time | 0 | 1 | 2020-11-23 09:00:41 | 2025-04-14 23:52:45 | 2023-06-01 11:48:40 | 83387 |

**Variable type: Timespan**

| skim_variable | n_missing | complete_rate | min | max |     median | n_unique |
|:--------------|----------:|--------------:|----:|----:|-----------:|---------:|
| time          |         0 |             1 |   0 |  59 | 12H 7M 25S |       60 |

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
    ## -148.07  -35.52  -10.61   26.71  813.09 
    ## 
    ## Coefficients:
    ##                                                      Estimate Std. Error
    ## (Intercept)                                        -1.845e+02  7.280e+01
    ## date                                                1.122e-02  3.862e-03
    ## lag(value, 1)                                       1.058e-01  2.474e-02
    ## lag(value, 7)                                       1.004e-01  2.569e-02
    ## lag(value, 14)                                      9.565e-02  2.573e-02
    ## lag(value, 21)                                      6.558e-02  2.588e-02
    ## lag(value, 28)                                      6.413e-02  2.575e-02
    ## lag(value, 35)                                      6.950e-02  2.581e-02
    ## lag(value, 42)                                      4.872e-02  2.593e-02
    ## lag(value, 49)                                      7.142e-02  2.577e-02
    ## month(date, label = TRUE).L                        -1.026e+01  5.142e+00
    ## month(date, label = TRUE).Q                         2.719e+00  5.203e+00
    ## month(date, label = TRUE).C                        -1.233e+01  5.220e+00
    ## month(date, label = TRUE)^4                        -6.599e+00  5.200e+00
    ## month(date, label = TRUE)^5                        -1.230e+01  5.208e+00
    ## month(date, label = TRUE)^6                        -3.031e+00  5.271e+00
    ## month(date, label = TRUE)^7                        -6.214e+00  5.170e+00
    ## month(date, label = TRUE)^8                        -4.514e+00  5.179e+00
    ## month(date, label = TRUE)^9                         5.464e+00  5.202e+00
    ## month(date, label = TRUE)^10                        4.489e+00  5.276e+00
    ## month(date, label = TRUE)^11                       -5.854e+00  5.342e+00
    ## fourier_vec(date, type = "sin", K = 1, period = 7) -1.145e+01  2.389e+00
    ## fourier_vec(date, type = "cos", K = 1, period = 7)  7.945e+00  2.513e+00
    ##                                                    t value Pr(>|t|)    
    ## (Intercept)                                         -2.534 0.011375 *  
    ## date                                                 2.905 0.003721 ** 
    ## lag(value, 1)                                        4.277 2.01e-05 ***
    ## lag(value, 7)                                        3.907 9.74e-05 ***
    ## lag(value, 14)                                       3.718 0.000208 ***
    ## lag(value, 21)                                       2.533 0.011394 *  
    ## lag(value, 28)                                       2.491 0.012859 *  
    ## lag(value, 35)                                       2.692 0.007171 ** 
    ## lag(value, 42)                                       1.879 0.060412 .  
    ## lag(value, 49)                                       2.772 0.005645 ** 
    ## month(date, label = TRUE).L                         -1.996 0.046111 *  
    ## month(date, label = TRUE).Q                          0.523 0.601373    
    ## month(date, label = TRUE).C                         -2.362 0.018319 *  
    ## month(date, label = TRUE)^4                         -1.269 0.204649    
    ## month(date, label = TRUE)^5                         -2.361 0.018350 *  
    ## month(date, label = TRUE)^6                         -0.575 0.565419    
    ## month(date, label = TRUE)^7                         -1.202 0.229564    
    ## month(date, label = TRUE)^8                         -0.872 0.383590    
    ## month(date, label = TRUE)^9                          1.050 0.293662    
    ## month(date, label = TRUE)^10                         0.851 0.395064    
    ## month(date, label = TRUE)^11                        -1.096 0.273257    
    ## fourier_vec(date, type = "sin", K = 1, period = 7)  -4.794 1.79e-06 ***
    ## fourier_vec(date, type = "cos", K = 1, period = 7)   3.161 0.001600 ** 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 58.78 on 1532 degrees of freedom
    ##   (49 observations deleted due to missingness)
    ## Multiple R-squared:  0.2472, Adjusted R-squared:  0.2364 
    ## F-statistic: 22.86 on 22 and 1532 DF,  p-value: < 2.2e-16

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
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 2.86190783838351"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 63, 91 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 2.37702159360054"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 63, 91, 70 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 2.24414339014365"
    ## [1] "BEST method = 'lin', seasonal.factor = c( 63, 91, 70 )"
    ## [1] "BEST lin OBJECTIVE FUNCTION = 2.24414339014365"
    ## [1] "CURRNET METHOD: nonlin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'nonlin' , seasonal.factor =  c( 63, 91, 70 ) ...)"
    ## [1] "CURRENT nonlin OBJECTIVE FUNCTION = 3.83229928459153"
    ## [1] "BEST method = 'nonlin' PATH MEMBER = c( 63, 91, 70 )"
    ## [1] "BEST nonlin OBJECTIVE FUNCTION = 3.83229928459153"
    ## [1] "CURRNET METHOD: both"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'both' , seasonal.factor =  c( 63, 91, 70 ) ...)"
    ## [1] "CURRENT both OBJECTIVE FUNCTION = 2.85061987914165"
    ## [1] "BEST method = 'both' PATH MEMBER = c( 63, 91, 70 )"
    ## [1] "BEST both OBJECTIVE FUNCTION = 2.85061987914165"

![](man/figures/README-nns_forecasting-1.png)<!-- -->

    ## Package: healthyR.ai
    ## [1] "CURRNET METHOD: lin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 63 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 2.72209420593936"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 63, 98 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 2.11269990206027"
    ## [1] "BEST method = 'lin', seasonal.factor = c( 63, 98 )"
    ## [1] "BEST lin OBJECTIVE FUNCTION = 2.11269990206027"
    ## [1] "CURRNET METHOD: nonlin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'nonlin' , seasonal.factor =  c( 63, 98 ) ...)"
    ## [1] "CURRENT nonlin OBJECTIVE FUNCTION = 2.66281459050267"
    ## [1] "BEST method = 'nonlin' PATH MEMBER = c( 63, 98 )"
    ## [1] "BEST nonlin OBJECTIVE FUNCTION = 2.66281459050267"
    ## [1] "CURRNET METHOD: both"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'both' , seasonal.factor =  c( 63, 98 ) ...)"
    ## [1] "CURRENT both OBJECTIVE FUNCTION = 2.21061375410477"
    ## [1] "BEST method = 'both' PATH MEMBER = c( 63, 98 )"
    ## [1] "BEST both OBJECTIVE FUNCTION = 2.21061375410477"

![](man/figures/README-nns_forecasting-2.png)<!-- -->

    ## Package: healthyR.data
    ## [1] "CURRNET METHOD: lin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 98 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 2.08639104211741"
    ## [1] "BEST method = 'lin', seasonal.factor = c( 98 )"
    ## [1] "BEST lin OBJECTIVE FUNCTION = 2.08639104211741"
    ## [1] "CURRNET METHOD: nonlin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'nonlin' , seasonal.factor =  c( 98 ) ...)"
    ## [1] "CURRENT nonlin OBJECTIVE FUNCTION = 2.54809051776451"
    ## [1] "BEST method = 'nonlin' PATH MEMBER = c( 98 )"
    ## [1] "BEST nonlin OBJECTIVE FUNCTION = 2.54809051776451"
    ## [1] "CURRNET METHOD: both"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'both' , seasonal.factor =  c( 98 ) ...)"
    ## [1] "CURRENT both OBJECTIVE FUNCTION = 2.16095499774"
    ## [1] "BEST method = 'both' PATH MEMBER = c( 98 )"
    ## [1] "BEST both OBJECTIVE FUNCTION = 2.16095499774"

![](man/figures/README-nns_forecasting-3.png)<!-- -->

    ## Package: healthyR.ts
    ## [1] "CURRNET METHOD: lin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 63 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 2.33170583824845"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 63, 98 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 2.00030705863157"
    ## [1] "BEST method = 'lin', seasonal.factor = c( 63, 98 )"
    ## [1] "BEST lin OBJECTIVE FUNCTION = 2.00030705863157"
    ## [1] "CURRNET METHOD: nonlin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'nonlin' , seasonal.factor =  c( 63, 98 ) ...)"
    ## [1] "CURRENT nonlin OBJECTIVE FUNCTION = 3.29932442526"
    ## [1] "BEST method = 'nonlin' PATH MEMBER = c( 63, 98 )"
    ## [1] "BEST nonlin OBJECTIVE FUNCTION = 3.29932442526"
    ## [1] "CURRNET METHOD: both"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'both' , seasonal.factor =  c( 63, 98 ) ...)"
    ## [1] "CURRENT both OBJECTIVE FUNCTION = 2.48295398593788"
    ## [1] "BEST method = 'both' PATH MEMBER = c( 63, 98 )"
    ## [1] "BEST both OBJECTIVE FUNCTION = 2.48295398593788"

![](man/figures/README-nns_forecasting-4.png)<!-- -->

    ## Package: healthyverse
    ## [1] "CURRNET METHOD: lin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 91 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 3.56085500017981"
    ## [1] "BEST method = 'lin', seasonal.factor = c( 91 )"
    ## [1] "BEST lin OBJECTIVE FUNCTION = 3.56085500017981"
    ## [1] "CURRNET METHOD: nonlin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'nonlin' , seasonal.factor =  c( 91 ) ...)"
    ## [1] "CURRENT nonlin OBJECTIVE FUNCTION = 4.92448673891973"
    ## [1] "BEST method = 'nonlin' PATH MEMBER = c( 91 )"
    ## [1] "BEST nonlin OBJECTIVE FUNCTION = 4.92448673891973"
    ## [1] "CURRNET METHOD: both"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'both' , seasonal.factor =  c( 91 ) ...)"
    ## [1] "CURRENT both OBJECTIVE FUNCTION = 4.00169597099699"
    ## [1] "BEST method = 'both' PATH MEMBER = c( 91 )"
    ## [1] "BEST both OBJECTIVE FUNCTION = 4.00169597099699"

![](man/figures/README-nns_forecasting-5.png)<!-- -->

    ## Package: RandomWalker
    ## [1] "CURRNET METHOD: lin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 42 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 2.67112885449223"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 42, 49 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 2.53478078521824"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 42, 49, 21 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 2.46777769875464"
    ## [1] "BEST method = 'lin', seasonal.factor = c( 42, 49, 21 )"
    ## [1] "BEST lin OBJECTIVE FUNCTION = 2.46777769875464"
    ## [1] "CURRNET METHOD: nonlin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'nonlin' , seasonal.factor =  c( 42, 49, 21 ) ...)"
    ## [1] "CURRENT nonlin OBJECTIVE FUNCTION = 2.34147810432159"
    ## [1] "BEST method = 'nonlin' PATH MEMBER = c( 42, 49, 21 )"
    ## [1] "BEST nonlin OBJECTIVE FUNCTION = 2.34147810432159"
    ## [1] "CURRNET METHOD: both"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'both' , seasonal.factor =  c( 42, 49, 21 ) ...)"
    ## [1] "CURRENT both OBJECTIVE FUNCTION = 2.20605165338015"
    ## [1] "BEST method = 'both' PATH MEMBER = c( 42, 49, 21 )"
    ## [1] "BEST both OBJECTIVE FUNCTION = 2.20605165338015"

![](man/figures/README-nns_forecasting-6.png)<!-- -->

    ## Package: tidyAML
    ## [1] "CURRNET METHOD: lin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 28 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 4.98126538159127"
    ## [1] "BEST method = 'lin', seasonal.factor = c( 28 )"
    ## [1] "BEST lin OBJECTIVE FUNCTION = 4.98126538159127"
    ## [1] "CURRNET METHOD: nonlin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'nonlin' , seasonal.factor =  c( 28 ) ...)"
    ## [1] "CURRENT nonlin OBJECTIVE FUNCTION = 4.38898046399529"
    ## [1] "BEST method = 'nonlin' PATH MEMBER = c( 28 )"
    ## [1] "BEST nonlin OBJECTIVE FUNCTION = 4.38898046399529"
    ## [1] "CURRNET METHOD: both"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'both' , seasonal.factor =  c( 28 ) ...)"
    ## [1] "CURRENT both OBJECTIVE FUNCTION = 3.72732169732953"
    ## [1] "BEST method = 'both' PATH MEMBER = c( 28 )"
    ## [1] "BEST both OBJECTIVE FUNCTION = 3.72732169732953"

![](man/figures/README-nns_forecasting-7.png)<!-- -->

    ## Package: TidyDensity
    ## [1] "CURRNET METHOD: lin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 63 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 3.04533373127678"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 63, 77 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 2.87200277250341"
    ## [1] "BEST method = 'lin', seasonal.factor = c( 63, 77 )"
    ## [1] "BEST lin OBJECTIVE FUNCTION = 2.87200277250341"
    ## [1] "CURRNET METHOD: nonlin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'nonlin' , seasonal.factor =  c( 63, 77 ) ...)"
    ## [1] "CURRENT nonlin OBJECTIVE FUNCTION = 4.16939669585224"
    ## [1] "BEST method = 'nonlin' PATH MEMBER = c( 63, 77 )"
    ## [1] "BEST nonlin OBJECTIVE FUNCTION = 4.16939669585224"
    ## [1] "CURRNET METHOD: both"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'both' , seasonal.factor =  c( 63, 77 ) ...)"
    ## [1] "CURRENT both OBJECTIVE FUNCTION = 3.27841214015646"
    ## [1] "BEST method = 'both' PATH MEMBER = c( 63, 77 )"
    ## [1] "BEST both OBJECTIVE FUNCTION = 3.27841214015646"

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
    ## 1 healthyR.data <tibble [1,597 × 2]> <tibble [28 × 2]> <split [1569|28]>
    ## 2 healthyR      <tibble [1,590 × 2]> <tibble [28 × 2]> <split [1562|28]>
    ## 3 healthyR.ts   <tibble [1,534 × 2]> <tibble [28 × 2]> <split [1506|28]>
    ## 4 healthyverse  <tibble [1,505 × 2]> <tibble [28 × 2]> <split [1477|28]>
    ## 5 healthyR.ai   <tibble [1,329 × 2]> <tibble [28 × 2]> <split [1301|28]>
    ## 6 TidyDensity   <tibble [1,180 × 2]> <tibble [28 × 2]> <split [1152|28]>
    ## 7 tidyAML       <tibble [788 × 2]>   <tibble [28 × 2]> <split [760|28]> 
    ## 8 RandomWalker  <tibble [210 × 2]>   <tibble [28 × 2]> <split [182|28]>

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
| healthyR.data | 1 | ARIMA | Test | 0.7694488 | 167.16112 | 0.6246783 | 164.12815 | 0.9096094 | 0.1190479 |
| healthyR.data | 2 | LM | Test | 0.6996843 | 160.28380 | 0.5680399 | 133.77179 | 0.8465869 | 0.0104166 |
| healthyR.data | 3 | EARTH | Test | 0.9962772 | 356.32692 | 0.8088294 | 122.38892 | 1.2128183 | 0.0104166 |
| healthyR.data | 4 | NNAR | Test | 0.7238117 | 115.18089 | 0.5876278 | 172.11869 | 0.9233428 | 0.0000002 |
| healthyR | 1 | ARIMA | Test | 0.7619279 | 107.03797 | 0.7182533 | 181.95451 | 0.9192615 | 0.0273059 |
| healthyR | 2 | LM | Test | 0.7209117 | 97.53663 | 0.6795882 | 185.33345 | 0.8806633 | 0.0156866 |
| healthyR | 3 | EARTH | Test | 0.7171646 | 95.90878 | 0.6760558 | 175.30827 | 0.8774933 | 0.0156866 |
| healthyR | 4 | NNAR | Test | 0.7553806 | 101.30072 | 0.7120813 | 160.16219 | 0.9201985 | 0.0135662 |
| healthyR.ts | 1 | ARIMA | Test | 0.8651770 | 261.27284 | 0.6899582 | 141.91765 | 1.1100914 | 0.0002750 |
| healthyR.ts | 2 | LM | Test | 0.8959552 | 314.11627 | 0.7145031 | 136.80725 | 1.1562605 | 0.0002750 |
| healthyR.ts | 3 | EARTH | Test | 0.8941417 | 311.04792 | 0.7130569 | 137.18184 | 1.1529219 | NA |
| healthyR.ts | 4 | NNAR | Test | 0.8961776 | 162.23436 | 0.7146805 | 167.81104 | 1.1819113 | 0.0325700 |
| healthyverse | 1 | ARIMA | Test | 0.6585996 | 320.07065 | 0.6551371 | 104.71938 | 0.8308451 | 0.0095991 |
| healthyverse | 2 | LM | Test | 0.6444304 | 379.31852 | 0.6410424 | 97.67696 | 0.8092843 | 0.0013238 |
| healthyverse | 3 | EARTH | Test | 0.6489335 | 231.62090 | 0.6455219 | 111.07205 | 0.8533378 | 0.0013238 |
| healthyverse | 4 | NNAR | Test | 0.6780392 | 230.21562 | 0.6744745 | 118.91328 | 0.8783868 | 0.0212465 |
| healthyR.ai | 1 | ARIMA | Test | 0.6980726 | 139.09767 | 0.6576516 | 176.09874 | 0.9175567 | 0.0001009 |
| healthyR.ai | 2 | LM | Test | 0.6426509 | 142.46388 | 0.6054390 | 142.82369 | 0.8532215 | 0.0070437 |
| healthyR.ai | 3 | EARTH | Test | 0.6459544 | 128.63993 | 0.6085512 | 146.50162 | 0.8594700 | 0.0070437 |
| healthyR.ai | 4 | NNAR | Test | 0.7252413 | 146.78494 | 0.6832471 | 155.86448 | 0.9777809 | 0.0486335 |
| TidyDensity | 1 | ARIMA | Test | 0.5914672 | 286.60994 | 0.6666056 | 112.02662 | 0.7184392 | 0.0735229 |
| TidyDensity | 2 | LM | Test | 0.6130943 | 353.42122 | 0.6909802 | 99.82639 | 0.7984297 | 0.0071682 |
| TidyDensity | 3 | EARTH | Test | 0.5863485 | 251.88771 | 0.6608367 | 107.85519 | 0.7438565 | 0.0071682 |
| TidyDensity | 4 | NNAR | Test | 0.6208237 | 235.13728 | 0.6996915 | 134.92997 | 0.7648230 | 0.0026885 |
| tidyAML | 1 | ARIMA | Test | 0.6117495 | 326.64044 | 0.7509236 | 95.97410 | 0.7777056 | 0.0236708 |
| tidyAML | 2 | LM | Test | 0.6138113 | 311.65184 | 0.7534544 | 98.91837 | 0.7647261 | 0.0000640 |
| tidyAML | 3 | EARTH | Test | 0.6478030 | 346.15092 | 0.7951794 | 100.05824 | 0.7955317 | 0.0000640 |
| tidyAML | 4 | NNAR | Test | 0.6020418 | 300.87942 | 0.7390074 | 97.88006 | 0.7585309 | 0.0000763 |
| RandomWalker | 1 | ARIMA | Test | 1.2387635 | 131.48365 | 0.6292596 | 166.98166 | 1.4444300 | 0.0240225 |
| RandomWalker | 2 | LM | Test | 1.1817884 | 97.04718 | 0.6003177 | 184.73942 | 1.3897048 | 0.0229837 |
| RandomWalker | 3 | EARTH | Test | 1.1635296 | 94.57553 | 0.5910428 | 164.85044 | 1.3857008 | NA |
| RandomWalker | 4 | NNAR | Test | 1.3034071 | 155.13720 | 0.6620969 | 147.70118 | 1.5357123 | 0.0076315 |

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
    ## 1 healthyR.d…         2 LM          Test  0.700 160.  0.568 134.  0.847  1.04e-2
    ## 2 healthyR            3 EARTH       Test  0.717  95.9 0.676 175.  0.877  1.57e-2
    ## 3 healthyR.ts         1 ARIMA       Test  0.865 261.  0.690 142.  1.11   2.75e-4
    ## 4 healthyver…         2 LM          Test  0.644 379.  0.641  97.7 0.809  1.32e-3
    ## 5 healthyR.ai         2 LM          Test  0.643 142.  0.605 143.  0.853  7.04e-3
    ## 6 TidyDensity         1 ARIMA       Test  0.591 287.  0.667 112.  0.718  7.35e-2
    ## 7 tidyAML             4 NNAR        Test  0.602 301.  0.739  97.9 0.759  7.63e-5
    ## 8 RandomWalk…         3 EARTH       Test  1.16   94.6 0.591 165.  1.39  NA

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
    ## 1 healthyR.data <tibble>     <tibble>     <split [1569|28]> <mdl_tm_t [1 × 5]>
    ## 2 healthyR      <tibble>     <tibble>     <split [1562|28]> <mdl_tm_t [1 × 5]>
    ## 3 healthyR.ts   <tibble>     <tibble>     <split [1506|28]> <mdl_tm_t [1 × 5]>
    ## 4 healthyverse  <tibble>     <tibble>     <split [1477|28]> <mdl_tm_t [1 × 5]>
    ## 5 healthyR.ai   <tibble>     <tibble>     <split [1301|28]> <mdl_tm_t [1 × 5]>
    ## 6 TidyDensity   <tibble>     <tibble>     <split [1152|28]> <mdl_tm_t [1 × 5]>
    ## 7 tidyAML       <tibble>     <tibble>     <split [760|28]>  <mdl_tm_t [1 × 5]>
    ## 8 RandomWalker  <tibble>     <tibble>     <split [182|28]>  <mdl_tm_t [1 × 5]>

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
