Time Series Analysis and Nested Modeling of the Healthyverse Packages
================
Steven P. Sanderson II, MPH - Date:
12 March, 2025

This analysis follows a *Nested Modeltime Workflow*.

## Get Data

``` r
glimpse(downloads_tbl)
```

    ## Rows: 133,749
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

The last day in the data set is 2025-03-10 19:53:56, the file was
birthed on: 2024-08-07 07:35:44, and at report knit time is -5168.3
hours old. Happy analyzing!

Now that we have our data lets take a look at it using the `skimr`
package.

``` r
skim(downloads_tbl)
```

|                                                  |               |
|:-------------------------------------------------|:--------------|
| Name                                             | downloads_tbl |
| Number of rows                                   | 133749        |
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
| r_version     |     96043 |          0.28 |   5 |   5 |     0 |       46 |          0 |
| r_arch        |     96043 |          0.28 |   3 |   7 |     0 |        5 |          0 |
| r_os          |     96043 |          0.28 |   7 |  15 |     0 |       21 |          0 |
| package       |         0 |          1.00 |   7 |  13 |     0 |        8 |          0 |
| version       |         0 |          1.00 |   5 |  17 |     0 |       60 |          0 |
| country       |     11270 |          0.92 |   2 |   2 |     0 |      163 |          0 |

**Variable type: Date**

| skim_variable | n_missing | complete_rate | min | max | median | n_unique |
|:---|---:|---:|:---|:---|:---|---:|
| date | 0 | 1 | 2020-11-23 | 2025-03-10 | 2023-05-16 | 1569 |

**Variable type: numeric**

| skim_variable | n_missing | complete_rate | mean | sd | p0 | p25 | p50 | p75 | p100 | hist |
|:---|---:|---:|---:|---:|---:|---:|---:|---:|---:|:---|
| size | 0 | 1 | 1136348.84 | 1526261.63 | 355 | 14701 | 260643 | 2367791 | 5677952 | ▇▁▂▁▁ |
| ip_id | 0 | 1 | 10388.94 | 18373.12 | 1 | 299 | 3091 | 11862 | 209747 | ▇▁▁▁▁ |

**Variable type: POSIXct**

| skim_variable | n_missing | complete_rate | min | max | median | n_unique |
|:---|---:|---:|:---|:---|:---|---:|
| date_time | 0 | 1 | 2020-11-23 09:00:41 | 2025-03-10 19:53:56 | 2023-05-16 19:23:07 | 81124 |

**Variable type: Timespan**

| skim_variable | n_missing | complete_rate | min | max |     median | n_unique |
|:--------------|----------:|--------------:|----:|----:|-----------:|---------:|
| time          |         0 |             1 |   0 |  59 | 12H 8M 19S |       60 |

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
    ## -153.82  -35.14  -10.16   26.78  810.64 
    ## 
    ## Coefficients:
    ##                                                      Estimate Std. Error
    ## (Intercept)                                        -2.023e+02  7.538e+01
    ## date                                                1.208e-02  3.999e-03
    ## lag(value, 1)                                       1.121e-01  2.502e-02
    ## lag(value, 7)                                       9.020e-02  2.591e-02
    ## lag(value, 14)                                      9.414e-02  2.592e-02
    ## lag(value, 21)                                      6.742e-02  2.610e-02
    ## lag(value, 28)                                      6.464e-02  2.600e-02
    ## lag(value, 35)                                      6.836e-02  2.619e-02
    ## lag(value, 42)                                      5.464e-02  2.632e-02
    ## lag(value, 49)                                      8.657e-02  2.622e-02
    ## month(date, label = TRUE).L                        -1.171e+01  5.183e+00
    ## month(date, label = TRUE).Q                         2.460e+00  5.193e+00
    ## month(date, label = TRUE).C                        -1.140e+01  5.268e+00
    ## month(date, label = TRUE)^4                        -8.123e+00  5.237e+00
    ## month(date, label = TRUE)^5                        -1.201e+01  5.208e+00
    ## month(date, label = TRUE)^6                        -2.532e+00  5.289e+00
    ## month(date, label = TRUE)^7                        -7.538e+00  5.213e+00
    ## month(date, label = TRUE)^8                        -3.584e+00  5.249e+00
    ## month(date, label = TRUE)^9                         4.969e+00  5.286e+00
    ## month(date, label = TRUE)^10                        4.853e+00  5.311e+00
    ## month(date, label = TRUE)^11                       -6.172e+00  5.331e+00
    ## fourier_vec(date, type = "sin", K = 1, period = 7) -1.191e+01  2.405e+00
    ## fourier_vec(date, type = "cos", K = 1, period = 7)  7.932e+00  2.534e+00
    ##                                                    t value Pr(>|t|)    
    ## (Intercept)                                         -2.684 0.007362 ** 
    ## date                                                 3.021 0.002561 ** 
    ## lag(value, 1)                                        4.479 8.06e-06 ***
    ## lag(value, 7)                                        3.481 0.000513 ***
    ## lag(value, 14)                                       3.631 0.000291 ***
    ## lag(value, 21)                                       2.583 0.009881 ** 
    ## lag(value, 28)                                       2.486 0.013029 *  
    ## lag(value, 35)                                       2.610 0.009149 ** 
    ## lag(value, 42)                                       2.076 0.038061 *  
    ## lag(value, 49)                                       3.301 0.000987 ***
    ## month(date, label = TRUE).L                         -2.260 0.023968 *  
    ## month(date, label = TRUE).Q                          0.474 0.635811    
    ## month(date, label = TRUE).C                         -2.164 0.030587 *  
    ## month(date, label = TRUE)^4                         -1.551 0.121106    
    ## month(date, label = TRUE)^5                         -2.306 0.021242 *  
    ## month(date, label = TRUE)^6                         -0.479 0.632164    
    ## month(date, label = TRUE)^7                         -1.446 0.148397    
    ## month(date, label = TRUE)^8                         -0.683 0.494775    
    ## month(date, label = TRUE)^9                          0.940 0.347313    
    ## month(date, label = TRUE)^10                         0.914 0.360956    
    ## month(date, label = TRUE)^11                        -1.158 0.247148    
    ## fourier_vec(date, type = "sin", K = 1, period = 7)  -4.952 8.16e-07 ***
    ## fourier_vec(date, type = "cos", K = 1, period = 7)   3.130 0.001781 ** 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 58.51 on 1497 degrees of freedom
    ##   (49 observations deleted due to missingness)
    ## Multiple R-squared:  0.2585, Adjusted R-squared:  0.2476 
    ## F-statistic: 23.72 on 22 and 1497 DF,  p-value: < 2.2e-16

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
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 1.31648998960556"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 77, 98 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 1.14584608728794"
    ## [1] "BEST method = 'lin', seasonal.factor = c( 77, 98 )"
    ## [1] "BEST lin OBJECTIVE FUNCTION = 1.14584608728794"
    ## [1] "CURRNET METHOD: nonlin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'nonlin' , seasonal.factor =  c( 77, 98 ) ...)"
    ## [1] "CURRENT nonlin OBJECTIVE FUNCTION = 1.27419330872363"
    ## [1] "BEST method = 'nonlin' PATH MEMBER = c( 77, 98 )"
    ## [1] "BEST nonlin OBJECTIVE FUNCTION = 1.27419330872363"
    ## [1] "CURRNET METHOD: both"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'both' , seasonal.factor =  c( 77, 98 ) ...)"
    ## [1] "CURRENT both OBJECTIVE FUNCTION = 1.1216604237443"
    ## [1] "BEST method = 'both' PATH MEMBER = c( 77, 98 )"
    ## [1] "BEST both OBJECTIVE FUNCTION = 1.1216604237443"

![](man/figures/README-nns_forecasting-1.png)<!-- -->

    ## Package: healthyR.ai
    ## [1] "CURRNET METHOD: lin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 77 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 1.37076905815832"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 77, 63 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 1.18538366010485"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 77, 63, 98 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 1.16795266765261"
    ## [1] "BEST method = 'lin', seasonal.factor = c( 77, 63, 98 )"
    ## [1] "BEST lin OBJECTIVE FUNCTION = 1.16795266765261"
    ## [1] "CURRNET METHOD: nonlin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'nonlin' , seasonal.factor =  c( 77, 63, 98 ) ...)"
    ## [1] "CURRENT nonlin OBJECTIVE FUNCTION = 1.65983792891159"
    ## [1] "BEST method = 'nonlin' PATH MEMBER = c( 77, 63, 98 )"
    ## [1] "BEST nonlin OBJECTIVE FUNCTION = 1.65983792891159"
    ## [1] "CURRNET METHOD: both"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'both' , seasonal.factor =  c( 77, 63, 98 ) ...)"
    ## [1] "CURRENT both OBJECTIVE FUNCTION = 1.30890937426101"
    ## [1] "BEST method = 'both' PATH MEMBER = c( 77, 63, 98 )"
    ## [1] "BEST both OBJECTIVE FUNCTION = 1.30890937426101"

![](man/figures/README-nns_forecasting-2.png)<!-- -->

    ## Package: healthyR.data
    ## [1] "CURRNET METHOD: lin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 98 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 1.45740136599443"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 98, 77 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 1.29566592822881"
    ## [1] "BEST method = 'lin', seasonal.factor = c( 98, 77 )"
    ## [1] "BEST lin OBJECTIVE FUNCTION = 1.29566592822881"
    ## [1] "CURRNET METHOD: nonlin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'nonlin' , seasonal.factor =  c( 98, 77 ) ...)"
    ## [1] "CURRENT nonlin OBJECTIVE FUNCTION = 1.86531259308076"
    ## [1] "BEST method = 'nonlin' PATH MEMBER = c( 98, 77 )"
    ## [1] "BEST nonlin OBJECTIVE FUNCTION = 1.86531259308076"
    ## [1] "CURRNET METHOD: both"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'both' , seasonal.factor =  c( 98, 77 ) ...)"
    ## [1] "CURRENT both OBJECTIVE FUNCTION = 1.52796363419904"
    ## [1] "BEST method = 'both' PATH MEMBER = c( 98, 77 )"
    ## [1] "BEST both OBJECTIVE FUNCTION = 1.52796363419904"

![](man/figures/README-nns_forecasting-3.png)<!-- -->

    ## Package: healthyR.ts
    ## [1] "CURRNET METHOD: lin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 14 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 1.47838742406469"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 14, 98 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 1.2216704077171"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 14, 98, 63 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 1.16008355143036"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 14, 98, 63, 77 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 1.1492816022447"
    ## [1] "BEST method = 'lin', seasonal.factor = c( 14, 98, 63, 77 )"
    ## [1] "BEST lin OBJECTIVE FUNCTION = 1.1492816022447"
    ## [1] "CURRNET METHOD: nonlin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'nonlin' , seasonal.factor =  c( 14, 98, 63, 77 ) ...)"
    ## [1] "CURRENT nonlin OBJECTIVE FUNCTION = 1.3292850036867"
    ## [1] "BEST method = 'nonlin' PATH MEMBER = c( 14, 98, 63, 77 )"
    ## [1] "BEST nonlin OBJECTIVE FUNCTION = 1.3292850036867"
    ## [1] "CURRNET METHOD: both"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'both' , seasonal.factor =  c( 14, 98, 63, 77 ) ...)"
    ## [1] "CURRENT both OBJECTIVE FUNCTION = 1.19433738825132"
    ## [1] "BEST method = 'both' PATH MEMBER = c( 14, 98, 63, 77 )"
    ## [1] "BEST both OBJECTIVE FUNCTION = 1.19433738825132"

![](man/figures/README-nns_forecasting-4.png)<!-- -->

    ## Package: healthyverse
    ## [1] "CURRNET METHOD: lin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 77 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 1.43923556086188"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 77, 35 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 1.37016085332059"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 77, 35, 49 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 1.30727007257978"
    ## [1] "BEST method = 'lin', seasonal.factor = c( 77, 35, 49 )"
    ## [1] "BEST lin OBJECTIVE FUNCTION = 1.30727007257978"
    ## [1] "CURRNET METHOD: nonlin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'nonlin' , seasonal.factor =  c( 77, 35, 49 ) ...)"
    ## [1] "CURRENT nonlin OBJECTIVE FUNCTION = 2.27351809478776"
    ## [1] "BEST method = 'nonlin' PATH MEMBER = c( 77, 35, 49 )"
    ## [1] "BEST nonlin OBJECTIVE FUNCTION = 2.27351809478776"
    ## [1] "CURRNET METHOD: both"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'both' , seasonal.factor =  c( 77, 35, 49 ) ...)"
    ## [1] "CURRENT both OBJECTIVE FUNCTION = 1.66685144707675"
    ## [1] "BEST method = 'both' PATH MEMBER = c( 77, 35, 49 )"
    ## [1] "BEST both OBJECTIVE FUNCTION = 1.66685144707675"

![](man/figures/README-nns_forecasting-5.png)<!-- -->

    ## Package: RandomWalker
    ## [1] "CURRNET METHOD: lin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 14 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 7.20563069235874"
    ## [1] "BEST method = 'lin', seasonal.factor = c( 14 )"
    ## [1] "BEST lin OBJECTIVE FUNCTION = 7.20563069235874"
    ## [1] "CURRNET METHOD: nonlin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'nonlin' , seasonal.factor =  c( 14 ) ...)"
    ## [1] "CURRENT nonlin OBJECTIVE FUNCTION = 5.17943319711263"
    ## [1] "BEST method = 'nonlin' PATH MEMBER = c( 14 )"
    ## [1] "BEST nonlin OBJECTIVE FUNCTION = 5.17943319711263"
    ## [1] "CURRNET METHOD: both"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'both' , seasonal.factor =  c( 14 ) ...)"
    ## [1] "CURRENT both OBJECTIVE FUNCTION = 4.88901553892347"
    ## [1] "BEST method = 'both' PATH MEMBER = c( 14 )"
    ## [1] "BEST both OBJECTIVE FUNCTION = 4.88901553892347"

![](man/figures/README-nns_forecasting-6.png)<!-- -->

    ## Package: tidyAML
    ## [1] "CURRNET METHOD: lin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 91 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 6.36833774699309"
    ## [1] "BEST method = 'lin', seasonal.factor = c( 91 )"
    ## [1] "BEST lin OBJECTIVE FUNCTION = 6.36833774699309"
    ## [1] "CURRNET METHOD: nonlin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'nonlin' , seasonal.factor =  c( 91 ) ...)"
    ## [1] "CURRENT nonlin OBJECTIVE FUNCTION = 7.07671734643449"
    ## [1] "BEST method = 'nonlin' PATH MEMBER = c( 91 )"
    ## [1] "BEST nonlin OBJECTIVE FUNCTION = 7.07671734643449"
    ## [1] "CURRNET METHOD: both"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'both' , seasonal.factor =  c( 91 ) ...)"
    ## [1] "CURRENT both OBJECTIVE FUNCTION = 6.62296530468908"
    ## [1] "BEST method = 'both' PATH MEMBER = c( 91 )"
    ## [1] "BEST both OBJECTIVE FUNCTION = 6.62296530468908"

![](man/figures/README-nns_forecasting-7.png)<!-- -->

    ## Package: TidyDensity
    ## [1] "CURRNET METHOD: lin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 77 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 1.40944458830511"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 77, 63 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 1.19576378758535"
    ## [1] "BEST method = 'lin', seasonal.factor = c( 77, 63 )"
    ## [1] "BEST lin OBJECTIVE FUNCTION = 1.19576378758535"
    ## [1] "CURRNET METHOD: nonlin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'nonlin' , seasonal.factor =  c( 77, 63 ) ...)"
    ## [1] "CURRENT nonlin OBJECTIVE FUNCTION = 2.1895447694162"
    ## [1] "BEST method = 'nonlin' PATH MEMBER = c( 77, 63 )"
    ## [1] "BEST nonlin OBJECTIVE FUNCTION = 2.1895447694162"
    ## [1] "CURRNET METHOD: both"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'both' , seasonal.factor =  c( 77, 63 ) ...)"
    ## [1] "CURRENT both OBJECTIVE FUNCTION = 1.53802124545796"
    ## [1] "BEST method = 'both' PATH MEMBER = c( 77, 63 )"
    ## [1] "BEST both OBJECTIVE FUNCTION = 1.53802124545796"

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
    ## 1 healthyR.data <tibble [1,562 × 2]> <tibble [28 × 2]> <split [1534|28]>
    ## 2 healthyR      <tibble [1,555 × 2]> <tibble [28 × 2]> <split [1527|28]>
    ## 3 healthyR.ts   <tibble [1,499 × 2]> <tibble [28 × 2]> <split [1471|28]>
    ## 4 healthyverse  <tibble [1,470 × 2]> <tibble [28 × 2]> <split [1442|28]>
    ## 5 healthyR.ai   <tibble [1,294 × 2]> <tibble [28 × 2]> <split [1266|28]>
    ## 6 TidyDensity   <tibble [1,145 × 2]> <tibble [28 × 2]> <split [1117|28]>
    ## 7 tidyAML       <tibble [753 × 2]>   <tibble [28 × 2]> <split [725|28]> 
    ## 8 RandomWalker  <tibble [175 × 2]>   <tibble [28 × 2]> <split [147|28]>

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
| healthyR.data | 1 | ARIMA | Test | 0.7044624 | 126.15258 | 0.6995730 | 153.56662 | 0.8307840 | 0.2395531 |
| healthyR.data | 2 | LM | Test | 0.8182907 | 195.32445 | 0.8126113 | 145.67851 | 0.9463415 | 0.1557244 |
| healthyR.data | 3 | EARTH | Test | 0.8302585 | 190.28021 | 0.8244960 | 127.96941 | 1.0627212 | 0.1557244 |
| healthyR.data | 4 | NNAR | Test | 0.7685663 | 117.64964 | 0.7632320 | 170.74192 | 0.9076322 | 0.0053390 |
| healthyR | 1 | ARIMA | Test | 0.7894857 | 172.77341 | 0.7306340 | 172.32937 | 0.9593779 | 0.0436659 |
| healthyR | 2 | LM | Test | 0.8010789 | 111.86498 | 0.7413629 | 181.68740 | 0.9941673 | 0.0874728 |
| healthyR | 3 | EARTH | Test | 0.8091861 | 135.29628 | 0.7488658 | 162.98106 | 1.0100419 | 0.0874728 |
| healthyR | 4 | NNAR | Test | 0.7370218 | 136.83487 | 0.6820809 | 152.49018 | 0.9298028 | 0.1154341 |
| healthyR.ts | 1 | ARIMA | Test | 1.0174477 | 115.71760 | 0.7140616 | 140.18110 | 1.2163288 | 0.2367332 |
| healthyR.ts | 2 | LM | Test | 0.9946773 | 120.99799 | 0.6980810 | 127.66479 | 1.2017288 | 0.2367332 |
| healthyR.ts | 3 | EARTH | Test | 0.9863727 | 122.95816 | 0.6922527 | 123.55789 | 1.1989437 | 0.2367332 |
| healthyR.ts | 4 | NNAR | Test | 0.9122826 | 98.84638 | 0.6402550 | 157.06469 | 1.1250507 | 0.1825813 |
| healthyverse | 1 | ARIMA | Test | 0.6918639 | 121.52336 | 0.9301034 | 107.84240 | 0.8730905 | 0.0284459 |
| healthyverse | 2 | LM | Test | 0.7669823 | 168.54578 | 1.0310884 | 107.41610 | 0.9378501 | 0.2420201 |
| healthyverse | 3 | EARTH | Test | 0.6897286 | 121.89810 | 0.9272328 | 106.66595 | 0.8845650 | 0.2420201 |
| healthyverse | 4 | NNAR | Test | 0.6747619 | 100.59612 | 0.9071124 | 113.66898 | 0.8604934 | 0.1037739 |
| healthyR.ai | 1 | ARIMA | Test | 0.7704363 | 99.65274 | 0.7466714 | 166.24929 | 0.9036044 | 0.1565888 |
| healthyR.ai | 2 | LM | Test | 0.8128568 | 96.55990 | 0.7877833 | 149.71704 | 0.9847790 | 0.2231015 |
| healthyR.ai | 3 | EARTH | Test | 0.8069268 | 97.44276 | 0.7820363 | 143.05398 | 0.9883070 | 0.2231015 |
| healthyR.ai | 4 | NNAR | Test | 0.6856938 | 84.85848 | 0.6645429 | 135.69888 | 0.8394997 | 0.2458690 |
| TidyDensity | 1 | ARIMA | Test | 0.7124591 | 135.39717 | 0.7421069 | 117.50419 | 0.8447095 | 0.0002468 |
| TidyDensity | 2 | LM | Test | 0.7130269 | 174.28219 | 0.7426984 | 107.34731 | 0.8243918 | 0.1559958 |
| TidyDensity | 3 | EARTH | Test | 0.7129207 | 119.52307 | 0.7425877 | 121.59879 | 0.8631311 | 0.1559958 |
| TidyDensity | 4 | NNAR | Test | 0.7273413 | 100.62510 | 0.7576084 | 141.98316 | 0.9118168 | 0.0399733 |
| tidyAML | 1 | ARIMA | Test | 0.6578100 | 137.92190 | 0.6577111 | 98.52491 | 0.7820979 | 0.0249315 |
| tidyAML | 2 | LM | Test | 0.6658573 | 143.90852 | 0.6657573 | 97.14950 | 0.7952764 | 0.0138359 |
| tidyAML | 3 | EARTH | Test | 0.7166003 | 100.12932 | 0.7164926 | 129.92220 | 0.8805564 | 0.0138359 |
| tidyAML | 4 | NNAR | Test | 0.6559118 | 137.56769 | 0.6558132 | 100.13765 | 0.7877103 | 0.0097161 |
| RandomWalker | 1 | ARIMA | Test | 1.0545499 | 103.32670 | 0.4877640 | 128.19550 | 1.1861310 | 0.4117655 |
| RandomWalker | 2 | LM | Test | 1.3580148 | 108.05745 | 0.6281264 | 194.75413 | 1.4923811 | 0.0287988 |
| RandomWalker | 3 | EARTH | Test | 1.2884859 | 89.36005 | 0.5959670 | 162.61180 | 1.4804051 | NA |
| RandomWalker | 4 | NNAR | Test | 1.5001125 | 178.54945 | 0.6938513 | 152.18089 | 1.7939789 | 0.0007498 |

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
    ## 1 healthyR.data         1 ARIMA       Test  0.704 126.  0.700 154.  0.831 0.240 
    ## 2 healthyR              4 NNAR        Test  0.737 137.  0.682 152.  0.930 0.115 
    ## 3 healthyR.ts           4 NNAR        Test  0.912  98.8 0.640 157.  1.13  0.183 
    ## 4 healthyverse          4 NNAR        Test  0.675 101.  0.907 114.  0.860 0.104 
    ## 5 healthyR.ai           4 NNAR        Test  0.686  84.9 0.665 136.  0.839 0.246 
    ## 6 TidyDensity           2 LM          Test  0.713 174.  0.743 107.  0.824 0.156 
    ## 7 tidyAML               1 ARIMA       Test  0.658 138.  0.658  98.5 0.782 0.0249
    ## 8 RandomWalker          1 ARIMA       Test  1.05  103.  0.488 128.  1.19  0.412

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
    ## 1 healthyR.data <tibble>     <tibble>     <split [1534|28]> <mdl_tm_t [1 × 5]>
    ## 2 healthyR      <tibble>     <tibble>     <split [1527|28]> <mdl_tm_t [1 × 5]>
    ## 3 healthyR.ts   <tibble>     <tibble>     <split [1471|28]> <mdl_tm_t [1 × 5]>
    ## 4 healthyverse  <tibble>     <tibble>     <split [1442|28]> <mdl_tm_t [1 × 5]>
    ## 5 healthyR.ai   <tibble>     <tibble>     <split [1266|28]> <mdl_tm_t [1 × 5]>
    ## 6 TidyDensity   <tibble>     <tibble>     <split [1117|28]> <mdl_tm_t [1 × 5]>
    ## 7 tidyAML       <tibble>     <tibble>     <split [725|28]>  <mdl_tm_t [1 × 5]>
    ## 8 RandomWalker  <tibble>     <tibble>     <split [147|28]>  <mdl_tm_t [1 × 5]>

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
