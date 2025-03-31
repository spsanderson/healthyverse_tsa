Time Series Analysis and Nested Modeling of the Healthyverse Packages
================
Steven P. Sanderson II, MPH - Date:
31 March, 2025

This analysis follows a *Nested Modeltime Workflow*.

## Get Data

``` r
glimpse(downloads_tbl)
```

    ## Rows: 135,553
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

The last day in the data set is 2025-03-29 23:31:25, the file was
birthed on: 2024-08-07 07:35:44, and at report knit time is -5627.93
hours old. Happy analyzing!

Now that we have our data lets take a look at it using the `skimr`
package.

``` r
skim(downloads_tbl)
```

|                                                  |               |
|:-------------------------------------------------|:--------------|
| Name                                             | downloads_tbl |
| Number of rows                                   | 135553        |
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
| r_version     |     97467 |          0.28 |   5 |   5 |     0 |       46 |          0 |
| r_arch        |     97467 |          0.28 |   3 |   7 |     0 |        5 |          0 |
| r_os          |     97467 |          0.28 |   7 |  15 |     0 |       21 |          0 |
| package       |         0 |          1.00 |   7 |  13 |     0 |        8 |          0 |
| version       |         0 |          1.00 |   5 |  17 |     0 |       60 |          0 |
| country       |     11382 |          0.92 |   2 |   2 |     0 |      163 |          0 |

**Variable type: Date**

| skim_variable | n_missing | complete_rate | min | max | median | n_unique |
|:---|---:|---:|:---|:---|:---|---:|
| date | 0 | 1 | 2020-11-23 | 2025-03-29 | 2023-05-24 | 1588 |

**Variable type: numeric**

| skim_variable | n_missing | complete_rate | mean | sd | p0 | p25 | p50 | p75 | p100 | hist |
|:---|---:|---:|---:|---:|---:|---:|---:|---:|---:|:---|
| size | 0 | 1 | 1133926.3 | 1523906.59 | 355 | 14701 | 260657 | 2367763 | 5677952 | ▇▁▂▁▁ |
| ip_id | 0 | 1 | 10356.6 | 18347.55 | 1 | 310 | 3080 | 11729 | 209747 | ▇▁▁▁▁ |

**Variable type: POSIXct**

| skim_variable | n_missing | complete_rate | min | max | median | n_unique |
|:---|---:|---:|:---|:---|:---|---:|
| date_time | 0 | 1 | 2020-11-23 09:00:41 | 2025-03-29 23:31:25 | 2023-05-24 20:34:21 | 82335 |

**Variable type: Timespan**

| skim_variable | n_missing | complete_rate | min | max |     median | n_unique |
|:--------------|----------:|--------------:|----:|----:|-----------:|---------:|
| time          |         0 |             1 |   0 |  59 | 12H 7M 37S |       60 |

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
    ## -149.98  -35.38  -10.40   26.89  812.86 
    ## 
    ## Coefficients:
    ##                                                      Estimate Std. Error
    ## (Intercept)                                        -1.947e+02  7.411e+01
    ## date                                                1.176e-02  3.932e-03
    ## lag(value, 1)                                       1.069e-01  2.489e-02
    ## lag(value, 7)                                       9.603e-02  2.585e-02
    ## lag(value, 14)                                      9.398e-02  2.586e-02
    ## lag(value, 21)                                      6.334e-02  2.594e-02
    ## lag(value, 28)                                      5.906e-02  2.583e-02
    ## lag(value, 35)                                      6.945e-02  2.592e-02
    ## lag(value, 42)                                      5.201e-02  2.610e-02
    ## lag(value, 49)                                      7.882e-02  2.597e-02
    ## month(date, label = TRUE).L                        -1.072e+01  5.159e+00
    ## month(date, label = TRUE).Q                         2.526e+00  5.209e+00
    ## month(date, label = TRUE).C                        -1.189e+01  5.256e+00
    ## month(date, label = TRUE)^4                        -7.126e+00  5.207e+00
    ## month(date, label = TRUE)^5                        -1.242e+01  5.220e+00
    ## month(date, label = TRUE)^6                        -2.899e+00  5.299e+00
    ## month(date, label = TRUE)^7                        -6.473e+00  5.173e+00
    ## month(date, label = TRUE)^8                        -4.699e+00  5.202e+00
    ## month(date, label = TRUE)^9                         5.738e+00  5.266e+00
    ## month(date, label = TRUE)^10                        4.361e+00  5.315e+00
    ## month(date, label = TRUE)^11                       -5.879e+00  5.345e+00
    ## fourier_vec(date, type = "sin", K = 1, period = 7) -1.185e+01  2.398e+00
    ## fourier_vec(date, type = "cos", K = 1, period = 7)  8.231e+00  2.524e+00
    ##                                                    t value Pr(>|t|)    
    ## (Intercept)                                         -2.627 0.008709 ** 
    ## date                                                 2.991 0.002827 ** 
    ## lag(value, 1)                                        4.294 1.86e-05 ***
    ## lag(value, 7)                                        3.715 0.000211 ***
    ## lag(value, 14)                                       3.634 0.000288 ***
    ## lag(value, 21)                                       2.442 0.014730 *  
    ## lag(value, 28)                                       2.286 0.022373 *  
    ## lag(value, 35)                                       2.680 0.007445 ** 
    ## lag(value, 42)                                       1.992 0.046509 *  
    ## lag(value, 49)                                       3.035 0.002447 ** 
    ## month(date, label = TRUE).L                         -2.077 0.037961 *  
    ## month(date, label = TRUE).Q                          0.485 0.627819    
    ## month(date, label = TRUE).C                         -2.262 0.023853 *  
    ## month(date, label = TRUE)^4                         -1.369 0.171348    
    ## month(date, label = TRUE)^5                         -2.380 0.017436 *  
    ## month(date, label = TRUE)^6                         -0.547 0.584460    
    ## month(date, label = TRUE)^7                         -1.251 0.210956    
    ## month(date, label = TRUE)^8                         -0.903 0.366583    
    ## month(date, label = TRUE)^9                          1.090 0.276049    
    ## month(date, label = TRUE)^10                         0.821 0.412040    
    ## month(date, label = TRUE)^11                        -1.100 0.271537    
    ## fourier_vec(date, type = "sin", K = 1, period = 7)  -4.943 8.54e-07 ***
    ## fourier_vec(date, type = "cos", K = 1, period = 7)   3.261 0.001136 ** 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 58.7 on 1516 degrees of freedom
    ##   (49 observations deleted due to missingness)
    ## Multiple R-squared:  0.2513, Adjusted R-squared:  0.2404 
    ## F-statistic: 23.13 on 22 and 1516 DF,  p-value: < 2.2e-16

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
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 2.4602785702299"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 98, 35 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 2.08232431601966"
    ## [1] "BEST method = 'lin', seasonal.factor = c( 98, 35 )"
    ## [1] "BEST lin OBJECTIVE FUNCTION = 2.08232431601966"
    ## [1] "CURRNET METHOD: nonlin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'nonlin' , seasonal.factor =  c( 98, 35 ) ...)"
    ## [1] "CURRENT nonlin OBJECTIVE FUNCTION = 2.70508158780813"
    ## [1] "BEST method = 'nonlin' PATH MEMBER = c( 98, 35 )"
    ## [1] "BEST nonlin OBJECTIVE FUNCTION = 2.70508158780813"
    ## [1] "CURRNET METHOD: both"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'both' , seasonal.factor =  c( 98, 35 ) ...)"
    ## [1] "CURRENT both OBJECTIVE FUNCTION = 2.29020697779693"
    ## [1] "BEST method = 'both' PATH MEMBER = c( 98, 35 )"
    ## [1] "BEST both OBJECTIVE FUNCTION = 2.29020697779693"

![](man/figures/README-nns_forecasting-1.png)<!-- -->

    ## Package: healthyR.ai
    ## [1] "CURRNET METHOD: lin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 77 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 1.90436318267719"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 77, 49 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 1.69268648126657"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 77, 49, 70 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 1.51791383905586"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 77, 49, 70, 98 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 1.51615743163648"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 77, 49, 70, 98, 63 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 1.49079171583277"
    ## [1] "BEST method = 'lin', seasonal.factor = c( 77, 49, 70, 98, 63 )"
    ## [1] "BEST lin OBJECTIVE FUNCTION = 1.49079171583277"
    ## [1] "CURRNET METHOD: nonlin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'nonlin' , seasonal.factor =  c( 77, 49, 70, 98, 63 ) ...)"
    ## [1] "CURRENT nonlin OBJECTIVE FUNCTION = 2.6366398924313"
    ## [1] "BEST method = 'nonlin' PATH MEMBER = c( 77, 49, 70, 98, 63 )"
    ## [1] "BEST nonlin OBJECTIVE FUNCTION = 2.6366398924313"
    ## [1] "CURRNET METHOD: both"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'both' , seasonal.factor =  c( 77, 49, 70, 98, 63 ) ...)"
    ## [1] "CURRENT both OBJECTIVE FUNCTION = 1.89084778201906"
    ## [1] "BEST method = 'both' PATH MEMBER = c( 77, 49, 70, 98, 63 )"
    ## [1] "BEST both OBJECTIVE FUNCTION = 1.89084778201906"

![](man/figures/README-nns_forecasting-2.png)<!-- -->

    ## Package: healthyR.data
    ## [1] "CURRNET METHOD: lin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 98 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 1.46178018807334"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 98, 77 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 1.31753518889317"
    ## [1] "BEST method = 'lin', seasonal.factor = c( 98, 77 )"
    ## [1] "BEST lin OBJECTIVE FUNCTION = 1.31753518889317"
    ## [1] "CURRNET METHOD: nonlin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'nonlin' , seasonal.factor =  c( 98, 77 ) ...)"
    ## [1] "CURRENT nonlin OBJECTIVE FUNCTION = 1.75523465426352"
    ## [1] "BEST method = 'nonlin' PATH MEMBER = c( 98, 77 )"
    ## [1] "BEST nonlin OBJECTIVE FUNCTION = 1.75523465426352"
    ## [1] "CURRNET METHOD: both"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'both' , seasonal.factor =  c( 98, 77 ) ...)"
    ## [1] "CURRENT both OBJECTIVE FUNCTION = 1.46225067373895"
    ## [1] "BEST method = 'both' PATH MEMBER = c( 98, 77 )"
    ## [1] "BEST both OBJECTIVE FUNCTION = 1.46225067373895"

![](man/figures/README-nns_forecasting-3.png)<!-- -->

    ## Package: healthyR.ts
    ## [1] "CURRNET METHOD: lin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 98 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 1.8780066262382"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 98, 63 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 1.45522710658822"
    ## [1] "BEST method = 'lin', seasonal.factor = c( 98, 63 )"
    ## [1] "BEST lin OBJECTIVE FUNCTION = 1.45522710658822"
    ## [1] "CURRNET METHOD: nonlin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'nonlin' , seasonal.factor =  c( 98, 63 ) ...)"
    ## [1] "CURRENT nonlin OBJECTIVE FUNCTION = 1.99154342633946"
    ## [1] "BEST method = 'nonlin' PATH MEMBER = c( 98, 63 )"
    ## [1] "BEST nonlin OBJECTIVE FUNCTION = 1.99154342633946"
    ## [1] "CURRNET METHOD: both"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'both' , seasonal.factor =  c( 98, 63 ) ...)"
    ## [1] "CURRENT both OBJECTIVE FUNCTION = 1.61002792658176"
    ## [1] "BEST method = 'both' PATH MEMBER = c( 98, 63 )"
    ## [1] "BEST both OBJECTIVE FUNCTION = 1.61002792658176"

![](man/figures/README-nns_forecasting-4.png)<!-- -->

    ## Package: healthyverse
    ## [1] "CURRNET METHOD: lin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 49 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 1.98167689021591"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 49, 77 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 1.77374612557973"
    ## [1] "BEST method = 'lin', seasonal.factor = c( 49, 77 )"
    ## [1] "BEST lin OBJECTIVE FUNCTION = 1.77374612557973"
    ## [1] "CURRNET METHOD: nonlin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'nonlin' , seasonal.factor =  c( 49, 77 ) ...)"
    ## [1] "CURRENT nonlin OBJECTIVE FUNCTION = 3.68223574587041"
    ## [1] "BEST method = 'nonlin' PATH MEMBER = c( 49, 77 )"
    ## [1] "BEST nonlin OBJECTIVE FUNCTION = 3.68223574587041"
    ## [1] "CURRNET METHOD: both"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'both' , seasonal.factor =  c( 49, 77 ) ...)"
    ## [1] "CURRENT both OBJECTIVE FUNCTION = 2.48120593532791"
    ## [1] "BEST method = 'both' PATH MEMBER = c( 49, 77 )"
    ## [1] "BEST both OBJECTIVE FUNCTION = 2.48120593532791"

![](man/figures/README-nns_forecasting-5.png)<!-- -->

    ## Package: RandomWalker
    ## [1] "CURRNET METHOD: lin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 35 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 4.93526958145889"
    ## [1] "BEST method = 'lin', seasonal.factor = c( 35 )"
    ## [1] "BEST lin OBJECTIVE FUNCTION = 4.93526958145889"
    ## [1] "CURRNET METHOD: nonlin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'nonlin' , seasonal.factor =  c( 35 ) ...)"
    ## [1] "CURRENT nonlin OBJECTIVE FUNCTION = 8.8101941198923"
    ## [1] "BEST method = 'nonlin' PATH MEMBER = c( 35 )"
    ## [1] "BEST nonlin OBJECTIVE FUNCTION = 8.8101941198923"
    ## [1] "CURRNET METHOD: both"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'both' , seasonal.factor =  c( 35 ) ...)"
    ## [1] "CURRENT both OBJECTIVE FUNCTION = 6.92766654916766"
    ## [1] "BEST method = 'both' PATH MEMBER = c( 35 )"
    ## [1] "BEST both OBJECTIVE FUNCTION = 6.92766654916766"

![](man/figures/README-nns_forecasting-6.png)<!-- -->

    ## Package: tidyAML
    ## [1] "CURRNET METHOD: lin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 56 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 5.06398766919011"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 56, 70 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 4.81932568021168"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 56, 70, 28 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 4.81220507370244"
    ## [1] "BEST method = 'lin', seasonal.factor = c( 56, 70, 28 )"
    ## [1] "BEST lin OBJECTIVE FUNCTION = 4.81220507370244"
    ## [1] "CURRNET METHOD: nonlin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'nonlin' , seasonal.factor =  c( 56, 70, 28 ) ...)"
    ## [1] "CURRENT nonlin OBJECTIVE FUNCTION = 4.32329960383159"
    ## [1] "BEST method = 'nonlin' PATH MEMBER = c( 56, 70, 28 )"
    ## [1] "BEST nonlin OBJECTIVE FUNCTION = 4.32329960383159"
    ## [1] "CURRNET METHOD: both"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'both' , seasonal.factor =  c( 56, 70, 28 ) ...)"
    ## [1] "CURRENT both OBJECTIVE FUNCTION = 4.29642756281642"
    ## [1] "BEST method = 'both' PATH MEMBER = c( 56, 70, 28 )"
    ## [1] "BEST both OBJECTIVE FUNCTION = 4.29642756281642"

![](man/figures/README-nns_forecasting-7.png)<!-- -->

    ## Package: TidyDensity
    ## [1] "CURRNET METHOD: lin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 77 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 1.898214596524"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 77, 63 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 1.64381139324578"
    ## [1] "BEST method = 'lin', seasonal.factor = c( 77, 63 )"
    ## [1] "BEST lin OBJECTIVE FUNCTION = 1.64381139324578"
    ## [1] "CURRNET METHOD: nonlin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'nonlin' , seasonal.factor =  c( 77, 63 ) ...)"
    ## [1] "CURRENT nonlin OBJECTIVE FUNCTION = 2.54809066566642"
    ## [1] "BEST method = 'nonlin' PATH MEMBER = c( 77, 63 )"
    ## [1] "BEST nonlin OBJECTIVE FUNCTION = 2.54809066566642"
    ## [1] "CURRNET METHOD: both"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'both' , seasonal.factor =  c( 77, 63 ) ...)"
    ## [1] "CURRENT both OBJECTIVE FUNCTION = 1.89296506899808"
    ## [1] "BEST method = 'both' PATH MEMBER = c( 77, 63 )"
    ## [1] "BEST both OBJECTIVE FUNCTION = 1.89296506899808"

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
    ## 1 healthyR.data <tibble [1,581 × 2]> <tibble [28 × 2]> <split [1553|28]>
    ## 2 healthyR      <tibble [1,574 × 2]> <tibble [28 × 2]> <split [1546|28]>
    ## 3 healthyR.ts   <tibble [1,518 × 2]> <tibble [28 × 2]> <split [1490|28]>
    ## 4 healthyverse  <tibble [1,489 × 2]> <tibble [28 × 2]> <split [1461|28]>
    ## 5 healthyR.ai   <tibble [1,313 × 2]> <tibble [28 × 2]> <split [1285|28]>
    ## 6 TidyDensity   <tibble [1,164 × 2]> <tibble [28 × 2]> <split [1136|28]>
    ## 7 tidyAML       <tibble [772 × 2]>   <tibble [28 × 2]> <split [744|28]> 
    ## 8 RandomWalker  <tibble [194 × 2]>   <tibble [28 × 2]> <split [166|28]>

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
| healthyR.data | 1 | ARIMA | Test | 0.7670570 | 188.02226 | 0.8208986 | 175.94672 | 0.9560903 | 0.0003553 |
| healthyR.data | 2 | LM | Test | 0.7256695 | 200.17845 | 0.7766059 | 131.17456 | 0.8908989 | 0.0237856 |
| healthyR.data | 3 | EARTH | Test | 0.8978129 | 169.60062 | 0.9608325 | 155.72093 | 1.1390864 | 0.0237856 |
| healthyR.data | 4 | NNAR | Test | 0.7564987 | 95.77047 | 0.8095991 | 158.76263 | 1.0009172 | 0.0018228 |
| healthyR | 1 | ARIMA | Test | 0.7844219 | 117.28857 | 0.8379124 | 181.66647 | 0.9229140 | 0.0018831 |
| healthyR | 2 | LM | Test | 0.7256711 | 98.46541 | 0.7751553 | 186.42073 | 0.8872675 | 0.0221954 |
| healthyR | 3 | EARTH | Test | 0.8078815 | 124.03702 | 0.8629717 | 163.33368 | 0.9560818 | 0.0221954 |
| healthyR | 4 | NNAR | Test | 0.7371471 | 108.75168 | 0.7874138 | 153.35354 | 0.9058860 | 0.0003472 |
| healthyR.ts | 1 | ARIMA | Test | 1.0799045 | 362.93501 | 0.8546969 | 146.71276 | 1.2667067 | 0.0180445 |
| healthyR.ts | 2 | LM | Test | 1.0384154 | 315.49578 | 0.8218601 | 147.57679 | 1.2309049 | 0.0180445 |
| healthyR.ts | 3 | EARTH | Test | 1.0338663 | 309.65274 | 0.8182597 | 147.72433 | 1.2269235 | NA |
| healthyR.ts | 4 | NNAR | Test | 1.0056825 | 118.72810 | 0.7959535 | 172.36959 | 1.2463012 | 0.0327646 |
| healthyverse | 1 | ARIMA | Test | 0.6186160 | 215.70437 | 0.8983728 | 106.64579 | 0.7609372 | 0.1016277 |
| healthyverse | 2 | LM | Test | 0.6138006 | 284.16085 | 0.8913797 | 97.95073 | 0.7378643 | 0.0001122 |
| healthyverse | 3 | EARTH | Test | 0.5954072 | 172.20541 | 0.8646682 | 109.82832 | 0.7605786 | 0.0001122 |
| healthyverse | 4 | NNAR | Test | 0.6210844 | 167.14957 | 0.9019575 | 117.60678 | 0.7852515 | 0.0400772 |
| healthyR.ai | 1 | ARIMA | Test | 0.8031078 | 115.41060 | 0.8422598 | 172.97451 | 0.9788862 | 0.0000232 |
| healthyR.ai | 2 | LM | Test | 0.7417961 | 123.65755 | 0.7779591 | 143.33077 | 0.9406246 | 0.0086882 |
| healthyR.ai | 3 | EARTH | Test | 0.8375485 | 144.42801 | 0.8783795 | 183.84619 | 1.0047861 | 0.0086882 |
| healthyR.ai | 4 | NNAR | Test | 0.7822942 | 153.26987 | 0.8204315 | 155.50186 | 0.9496514 | 0.0134188 |
| TidyDensity | 1 | ARIMA | Test | 0.6228763 | 225.43302 | 0.6631879 | 111.00364 | 0.7759489 | 0.0173418 |
| TidyDensity | 2 | LM | Test | 0.6751107 | 282.92417 | 0.7188027 | 109.00947 | 0.8414493 | 0.0039524 |
| TidyDensity | 3 | EARTH | Test | 0.6279063 | 219.43521 | 0.6685434 | 111.93586 | 0.7830540 | 0.0039524 |
| TidyDensity | 4 | NNAR | Test | 0.6134152 | 138.16943 | 0.6531144 | 132.78529 | 0.7699743 | 0.0130358 |
| tidyAML | 1 | ARIMA | Test | 0.6710631 | 361.23663 | 0.7525370 | 105.69097 | 0.8224173 | 0.0608297 |
| tidyAML | 2 | LM | Test | 0.6577651 | 354.70461 | 0.7376244 | 104.76861 | 0.7782496 | 0.0153427 |
| tidyAML | 3 | EARTH | Test | 0.5936326 | 200.30535 | 0.6657056 | 109.32606 | 0.7692343 | 0.0153427 |
| tidyAML | 4 | NNAR | Test | 0.6510864 | 334.50167 | 0.7301349 | 104.75135 | 0.7813096 | 0.0303170 |
| RandomWalker | 1 | ARIMA | Test | 1.2255336 | 132.73516 | 0.7531633 | 107.08332 | 1.6075754 | 0.0122903 |
| RandomWalker | 2 | LM | Test | 1.2438821 | 113.24236 | 0.7644396 | 192.57516 | 1.3460046 | 0.0005219 |
| RandomWalker | 3 | EARTH | Test | 1.1433178 | 90.71916 | 0.7026368 | 167.30047 | 1.2853293 | NA |
| RandomWalker | 4 | NNAR | Test | 1.4745268 | 161.76954 | 0.9061844 | 176.29781 | 1.6425273 | 0.0428538 |

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
    ## 1 healthyR.d…         2 LM          Test  0.726 200.  0.777 131.  0.891  2.38e-2
    ## 2 healthyR            2 LM          Test  0.726  98.5 0.775 186.  0.887  2.22e-2
    ## 3 healthyR.ts         3 EARTH       Test  1.03  310.  0.818 148.  1.23  NA      
    ## 4 healthyver…         2 LM          Test  0.614 284.  0.891  98.0 0.738  1.12e-4
    ## 5 healthyR.ai         2 LM          Test  0.742 124.  0.778 143.  0.941  8.69e-3
    ## 6 TidyDensity         4 NNAR        Test  0.613 138.  0.653 133.  0.770  1.30e-2
    ## 7 tidyAML             3 EARTH       Test  0.594 200.  0.666 109.  0.769  1.53e-2
    ## 8 RandomWalk…         3 EARTH       Test  1.14   90.7 0.703 167.  1.29  NA

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
    ## 1 healthyR.data <tibble>     <tibble>     <split [1553|28]> <mdl_tm_t [1 × 5]>
    ## 2 healthyR      <tibble>     <tibble>     <split [1546|28]> <mdl_tm_t [1 × 5]>
    ## 3 healthyR.ts   <tibble>     <tibble>     <split [1490|28]> <mdl_tm_t [1 × 5]>
    ## 4 healthyverse  <tibble>     <tibble>     <split [1461|28]> <mdl_tm_t [1 × 5]>
    ## 5 healthyR.ai   <tibble>     <tibble>     <split [1285|28]> <mdl_tm_t [1 × 5]>
    ## 6 TidyDensity   <tibble>     <tibble>     <split [1136|28]> <mdl_tm_t [1 × 5]>
    ## 7 tidyAML       <tibble>     <tibble>     <split [744|28]>  <mdl_tm_t [1 × 5]>
    ## 8 RandomWalker  <tibble>     <tibble>     <split [166|28]>  <mdl_tm_t [1 × 5]>

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
