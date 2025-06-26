Time Series Analysis and Nested Modeling of the Healthyverse Packages
================
Steven P. Sanderson II, MPH - Date:
26 June, 2025

This analysis follows a *Nested Modeltime Workflow*.

## Get Data

``` r
glimpse(downloads_tbl)
```

    ## Rows: 143,667
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

The last day in the data set is 2025-06-24 22:50:58, the file was
birthed on: 2022-07-02 23:58:17.511888, and at report knit time is
-2.610688^{4} hours old. Happy analyzing!

Now that we have our data lets take a look at it using the `skimr`
package.

``` r
skim(downloads_tbl)
```

|                                                  |               |
|:-------------------------------------------------|:--------------|
| Name                                             | downloads_tbl |
| Number of rows                                   | 143667        |
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
| r_version     |    103772 |          0.28 |   5 |   5 |     0 |       48 |          0 |
| r_arch        |    103772 |          0.28 |   3 |   7 |     0 |        5 |          0 |
| r_os          |    103772 |          0.28 |   7 |  15 |     0 |       22 |          0 |
| package       |         0 |          1.00 |   7 |  13 |     0 |        8 |          0 |
| version       |         0 |          1.00 |   5 |  17 |     0 |       60 |          0 |
| country       |     12144 |          0.92 |   2 |   2 |     0 |      163 |          0 |

**Variable type: Date**

| skim_variable | n_missing | complete_rate | min | max | median | n_unique |
|:---|---:|---:|:---|:---|:---|---:|
| date | 0 | 1 | 2020-11-23 | 2025-06-24 | 2023-07-12 | 1675 |

**Variable type: numeric**

| skim_variable | n_missing | complete_rate | mean | sd | p0 | p25 | p50 | p75 | p100 | hist |
|:---|---:|---:|---:|---:|---:|---:|---:|---:|---:|:---|
| size | 0 | 1 | 1132700.76 | 1515264.73 | 355 | 14701 | 289911 | 2367697 | 5677952 | ▇▁▂▁▁ |
| ip_id | 0 | 1 | 10447.63 | 18566.01 | 1 | 298 | 3054 | 11830 | 209747 | ▇▁▁▁▁ |

**Variable type: POSIXct**

| skim_variable | n_missing | complete_rate | min | max | median | n_unique |
|:---|---:|---:|:---|:---|:---|---:|
| date_time | 0 | 1 | 2020-11-23 09:00:41 | 2025-06-24 22:50:58 | 2023-07-12 00:34:10 | 88127 |

**Variable type: Timespan**

| skim_variable | n_missing | complete_rate | min | max |     median | n_unique |
|:--------------|----------:|--------------:|----:|----:|-----------:|---------:|
| time          |         0 |             1 |   0 |  59 | 12H 6M 24S |       60 |

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
    ## -148.19  -35.94  -11.40   26.71  816.10 
    ## 
    ## Coefficients:
    ##                                                      Estimate Std. Error
    ## (Intercept)                                        -1.752e+02  6.695e+01
    ## date                                                1.078e-02  3.545e-03
    ## lag(value, 1)                                       1.051e-01  2.423e-02
    ## lag(value, 7)                                       9.399e-02  2.518e-02
    ## lag(value, 14)                                      8.627e-02  2.518e-02
    ## lag(value, 21)                                      6.683e-02  2.527e-02
    ## lag(value, 28)                                      7.185e-02  2.516e-02
    ## lag(value, 35)                                      6.775e-02  2.523e-02
    ## lag(value, 42)                                      5.439e-02  2.533e-02
    ## lag(value, 49)                                      6.298e-02  2.522e-02
    ## month(date, label = TRUE).L                        -9.711e+00  5.123e+00
    ## month(date, label = TRUE).Q                         3.254e+00  5.096e+00
    ## month(date, label = TRUE).C                        -1.325e+01  5.142e+00
    ## month(date, label = TRUE)^4                        -6.654e+00  5.151e+00
    ## month(date, label = TRUE)^5                        -1.132e+01  5.126e+00
    ## month(date, label = TRUE)^6                        -4.090e+00  5.196e+00
    ## month(date, label = TRUE)^7                        -7.115e+00  5.093e+00
    ## month(date, label = TRUE)^8                        -2.860e+00  5.086e+00
    ## month(date, label = TRUE)^9                         5.222e+00  5.085e+00
    ## month(date, label = TRUE)^10                        2.524e+00  5.078e+00
    ## month(date, label = TRUE)^11                       -3.600e+00  5.112e+00
    ## fourier_vec(date, type = "sin", K = 1, period = 7) -1.188e+01  2.321e+00
    ## fourier_vec(date, type = "cos", K = 1, period = 7)  8.295e+00  2.445e+00
    ##                                                    t value Pr(>|t|)    
    ## (Intercept)                                         -2.616 0.008973 ** 
    ## date                                                 3.042 0.002388 ** 
    ## lag(value, 1)                                        4.337 1.53e-05 ***
    ## lag(value, 7)                                        3.733 0.000196 ***
    ## lag(value, 14)                                       3.426 0.000627 ***
    ## lag(value, 21)                                       2.645 0.008252 ** 
    ## lag(value, 28)                                       2.856 0.004345 ** 
    ## lag(value, 35)                                       2.685 0.007317 ** 
    ## lag(value, 42)                                       2.147 0.031910 *  
    ## lag(value, 49)                                       2.498 0.012604 *  
    ## month(date, label = TRUE).L                         -1.896 0.058202 .  
    ## month(date, label = TRUE).Q                          0.639 0.523236    
    ## month(date, label = TRUE).C                         -2.577 0.010050 *  
    ## month(date, label = TRUE)^4                         -1.292 0.196631    
    ## month(date, label = TRUE)^5                         -2.209 0.027285 *  
    ## month(date, label = TRUE)^6                         -0.787 0.431326    
    ## month(date, label = TRUE)^7                         -1.397 0.162573    
    ## month(date, label = TRUE)^8                         -0.562 0.573968    
    ## month(date, label = TRUE)^9                          1.027 0.304594    
    ## month(date, label = TRUE)^10                         0.497 0.619184    
    ## month(date, label = TRUE)^11                        -0.704 0.481346    
    ## fourier_vec(date, type = "sin", K = 1, period = 7)  -5.120 3.43e-07 ***
    ## fourier_vec(date, type = "cos", K = 1, period = 7)   3.393 0.000709 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 58.89 on 1603 degrees of freedom
    ##   (49 observations deleted due to missingness)
    ## Multiple R-squared:  0.2378, Adjusted R-squared:  0.2273 
    ## F-statistic: 22.73 on 22 and 1603 DF,  p-value: < 2.2e-16

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
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 2.60578756642671"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 21, 63 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 2.32675481598513"
    ## [1] "BEST method = 'lin', seasonal.factor = c( 21, 63 )"
    ## [1] "BEST lin OBJECTIVE FUNCTION = 2.32675481598513"
    ## [1] "CURRNET METHOD: nonlin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'nonlin' , seasonal.factor =  c( 21, 63 ) ...)"
    ## [1] "CURRENT nonlin OBJECTIVE FUNCTION = 7.71405145313876"
    ## [1] "BEST method = 'nonlin' PATH MEMBER = c( 21, 63 )"
    ## [1] "BEST nonlin OBJECTIVE FUNCTION = 7.71405145313876"
    ## [1] "CURRNET METHOD: both"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'both' , seasonal.factor =  c( 21, 63 ) ...)"
    ## [1] "CURRENT both OBJECTIVE FUNCTION = 4.02524098504629"
    ## [1] "BEST method = 'both' PATH MEMBER = c( 21, 63 )"
    ## [1] "BEST both OBJECTIVE FUNCTION = 4.02524098504629"

![](man/figures/README-nns_forecasting-1.png)<!-- -->

    ## Package: healthyR.ai
    ## [1] "CURRNET METHOD: lin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 63 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 1.82538536339128"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 63, 35 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 1.66397886720222"
    ## [1] "BEST method = 'lin', seasonal.factor = c( 63, 35 )"
    ## [1] "BEST lin OBJECTIVE FUNCTION = 1.66397886720222"
    ## [1] "CURRNET METHOD: nonlin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'nonlin' , seasonal.factor =  c( 63, 35 ) ...)"
    ## [1] "CURRENT nonlin OBJECTIVE FUNCTION = 10.5282390370238"
    ## [1] "BEST method = 'nonlin' PATH MEMBER = c( 63, 35 )"
    ## [1] "BEST nonlin OBJECTIVE FUNCTION = 10.5282390370238"
    ## [1] "CURRNET METHOD: both"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'both' , seasonal.factor =  c( 63, 35 ) ...)"
    ## [1] "CURRENT both OBJECTIVE FUNCTION = 3.79439665351825"
    ## [1] "BEST method = 'both' PATH MEMBER = c( 63, 35 )"
    ## [1] "BEST both OBJECTIVE FUNCTION = 3.79439665351825"

![](man/figures/README-nns_forecasting-2.png)<!-- -->

    ## Package: healthyR.data
    ## [1] "CURRNET METHOD: lin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 63 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 2.87846785225718"
    ## [1] "BEST method = 'lin', seasonal.factor = c( 63 )"
    ## [1] "BEST lin OBJECTIVE FUNCTION = 2.87846785225718"
    ## [1] "CURRNET METHOD: nonlin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'nonlin' , seasonal.factor =  c( 63 ) ...)"
    ## [1] "CURRENT nonlin OBJECTIVE FUNCTION = 2.36520379317235"
    ## [1] "BEST method = 'nonlin' PATH MEMBER = c( 63 )"
    ## [1] "BEST nonlin OBJECTIVE FUNCTION = 2.36520379317235"
    ## [1] "CURRNET METHOD: both"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'both' , seasonal.factor =  c( 63 ) ...)"
    ## [1] "CURRENT both OBJECTIVE FUNCTION = 2.24945423695419"
    ## [1] "BEST method = 'both' PATH MEMBER = c( 63 )"
    ## [1] "BEST both OBJECTIVE FUNCTION = 2.24945423695419"

![](man/figures/README-nns_forecasting-3.png)<!-- -->

    ## Package: healthyR.ts
    ## [1] "CURRNET METHOD: lin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 63 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 1.70445415165312"
    ## [1] "BEST method = 'lin', seasonal.factor = c( 63 )"
    ## [1] "BEST lin OBJECTIVE FUNCTION = 1.70445415165312"
    ## [1] "CURRNET METHOD: nonlin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'nonlin' , seasonal.factor =  c( 63 ) ...)"
    ## [1] "CURRENT nonlin OBJECTIVE FUNCTION = 9.64305617040239"
    ## [1] "BEST method = 'nonlin' PATH MEMBER = c( 63 )"
    ## [1] "BEST nonlin OBJECTIVE FUNCTION = 9.64305617040239"
    ## [1] "CURRNET METHOD: both"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'both' , seasonal.factor =  c( 63 ) ...)"
    ## [1] "CURRENT both OBJECTIVE FUNCTION = 3.91418845534687"
    ## [1] "BEST method = 'both' PATH MEMBER = c( 63 )"
    ## [1] "BEST both OBJECTIVE FUNCTION = 3.91418845534687"

![](man/figures/README-nns_forecasting-4.png)<!-- -->

    ## Package: healthyverse
    ## [1] "CURRNET METHOD: lin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 63 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 5.23402441004197"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 63, 77 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 5.18076647679232"
    ## [1] "BEST method = 'lin', seasonal.factor = c( 63, 77 )"
    ## [1] "BEST lin OBJECTIVE FUNCTION = 5.18076647679232"
    ## [1] "CURRNET METHOD: nonlin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'nonlin' , seasonal.factor =  c( 63, 77 ) ...)"
    ## [1] "CURRENT nonlin OBJECTIVE FUNCTION = 5.49559289844254"
    ## [1] "BEST method = 'nonlin' PATH MEMBER = c( 63, 77 )"
    ## [1] "BEST nonlin OBJECTIVE FUNCTION = 5.49559289844254"
    ## [1] "CURRNET METHOD: both"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'both' , seasonal.factor =  c( 63, 77 ) ...)"
    ## [1] "CURRENT both OBJECTIVE FUNCTION = 5.53542736453053"
    ## [1] "BEST method = 'both' PATH MEMBER = c( 63, 77 )"
    ## [1] "BEST both OBJECTIVE FUNCTION = 5.53542736453053"

![](man/figures/README-nns_forecasting-5.png)<!-- -->

    ## Package: RandomWalker
    ## [1] "CURRNET METHOD: lin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 70 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 2.79072483271236"
    ## [1] "BEST method = 'lin', seasonal.factor = c( 70 )"
    ## [1] "BEST lin OBJECTIVE FUNCTION = 2.79072483271236"
    ## [1] "CURRNET METHOD: nonlin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'nonlin' , seasonal.factor =  c( 70 ) ...)"
    ## [1] "CURRENT nonlin OBJECTIVE FUNCTION = 4.26867050736547"
    ## [1] "BEST method = 'nonlin' PATH MEMBER = c( 70 )"
    ## [1] "BEST nonlin OBJECTIVE FUNCTION = 4.26867050736547"
    ## [1] "CURRNET METHOD: both"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'both' , seasonal.factor =  c( 70 ) ...)"
    ## [1] "CURRENT both OBJECTIVE FUNCTION = 3.33873530683139"
    ## [1] "BEST method = 'both' PATH MEMBER = c( 70 )"
    ## [1] "BEST both OBJECTIVE FUNCTION = 3.33873530683139"

![](man/figures/README-nns_forecasting-6.png)<!-- -->

    ## Package: tidyAML
    ## [1] "CURRNET METHOD: lin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 84 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 5.32798470698016"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 84, 91 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 5.01355456431355"
    ## [1] "BEST method = 'lin', seasonal.factor = c( 84, 91 )"
    ## [1] "BEST lin OBJECTIVE FUNCTION = 5.01355456431355"
    ## [1] "CURRNET METHOD: nonlin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'nonlin' , seasonal.factor =  c( 84, 91 ) ...)"
    ## [1] "CURRENT nonlin OBJECTIVE FUNCTION = 3.31531642394959"
    ## [1] "BEST method = 'nonlin' PATH MEMBER = c( 84, 91 )"
    ## [1] "BEST nonlin OBJECTIVE FUNCTION = 3.31531642394959"
    ## [1] "CURRNET METHOD: both"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'both' , seasonal.factor =  c( 84, 91 ) ...)"
    ## [1] "CURRENT both OBJECTIVE FUNCTION = 3.88459158575858"
    ## [1] "BEST method = 'both' PATH MEMBER = c( 84, 91 )"
    ## [1] "BEST both OBJECTIVE FUNCTION = 3.88459158575858"

![](man/figures/README-nns_forecasting-7.png)<!-- -->

    ## Package: TidyDensity
    ## [1] "CURRNET METHOD: lin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 63 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 2.91135815475917"
    ## [1] "BEST method = 'lin', seasonal.factor = c( 63 )"
    ## [1] "BEST lin OBJECTIVE FUNCTION = 2.91135815475917"
    ## [1] "CURRNET METHOD: nonlin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'nonlin' , seasonal.factor =  c( 63 ) ...)"
    ## [1] "CURRENT nonlin OBJECTIVE FUNCTION = 10.2425328322332"
    ## [1] "BEST method = 'nonlin' PATH MEMBER = c( 63 )"
    ## [1] "BEST nonlin OBJECTIVE FUNCTION = 10.2425328322332"
    ## [1] "CURRNET METHOD: both"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'both' , seasonal.factor =  c( 63 ) ...)"
    ## [1] "CURRENT both OBJECTIVE FUNCTION = 5.91792990014751"
    ## [1] "BEST method = 'both' PATH MEMBER = c( 63 )"
    ## [1] "BEST both OBJECTIVE FUNCTION = 5.91792990014751"

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
    ## 1 healthyR.data <tibble [1,667 × 2]> <tibble [28 × 2]> <split [1639|28]>
    ## 2 healthyR      <tibble [1,661 × 2]> <tibble [28 × 2]> <split [1633|28]>
    ## 3 healthyR.ts   <tibble [1,605 × 2]> <tibble [28 × 2]> <split [1577|28]>
    ## 4 healthyverse  <tibble [1,575 × 2]> <tibble [28 × 2]> <split [1547|28]>
    ## 5 healthyR.ai   <tibble [1,400 × 2]> <tibble [28 × 2]> <split [1372|28]>
    ## 6 TidyDensity   <tibble [1,251 × 2]> <tibble [28 × 2]> <split [1223|28]>
    ## 7 tidyAML       <tibble [859 × 2]>   <tibble [28 × 2]> <split [831|28]> 
    ## 8 RandomWalker  <tibble [281 × 2]>   <tibble [28 × 2]> <split [253|28]>

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
| healthyR.data | 1 | ARIMA | Test | 0.7194124 | 155.77448 | 0.7847448 | 132.92038 | 0.9201500 | 0.0001058 |
| healthyR.data | 2 | LM | Test | 0.7288280 | 183.02383 | 0.7950155 | 128.18073 | 0.9140401 | 0.0984795 |
| healthyR.data | 3 | NULL | NA | NA | NA | NA | NA | NA | NA |
| healthyR.data | 4 | NNAR | Test | 0.7462524 | 98.26739 | 0.8140223 | 187.32415 | 0.9770643 | 0.0393116 |
| healthyR | 1 | ARIMA | Test | 0.8774973 | 128.08729 | 0.9170340 | 163.75966 | 1.0464099 | 0.0002753 |
| healthyR | 2 | LM | Test | 0.8666961 | 102.49103 | 0.9057462 | 181.77725 | 1.0449518 | 0.0658573 |
| healthyR | 3 | NULL | NA | NA | NA | NA | NA | NA | NA |
| healthyR | 4 | NNAR | Test | 0.8600279 | 103.12819 | 0.8987776 | 173.34697 | 1.0311025 | 0.0112570 |
| healthyR.ts | 1 | ARIMA | Test | 0.9172886 | 113.84474 | 0.6907010 | 163.64831 | 1.1913565 | 0.0178591 |
| healthyR.ts | 2 | LM | Test | 1.0175440 | 178.93789 | 0.7661914 | 155.89631 | 1.2537834 | 0.0999336 |
| healthyR.ts | 3 | NULL | NA | NA | NA | NA | NA | NA | NA |
| healthyR.ts | 4 | NNAR | Test | 0.9181966 | 100.66364 | 0.6913847 | 182.43413 | 1.2012206 | 0.0125531 |
| healthyverse | 1 | ARIMA | Test | 0.8042469 | 152.45579 | 0.9435257 | 95.05643 | 0.9764359 | 0.0002709 |
| healthyverse | 2 | LM | Test | 0.8054248 | 166.67258 | 0.9449076 | 93.55450 | 0.9645988 | 0.0772005 |
| healthyverse | 3 | NULL | NA | NA | NA | NA | NA | NA | NA |
| healthyverse | 4 | NNAR | Test | 0.8722176 | 112.21878 | 1.0232675 | 112.43137 | 1.0671191 | 0.0003617 |
| healthyR.ai | 1 | ARIMA | Test | 0.7077907 | 113.07816 | 0.7553633 | 157.12730 | 0.8295046 | 0.0845684 |
| healthyR.ai | 2 | LM | Test | 0.7168014 | 111.35903 | 0.7649796 | 154.54554 | 0.8507647 | 0.1603432 |
| healthyR.ai | 3 | NULL | NA | NA | NA | NA | NA | NA | NA |
| healthyR.ai | 4 | NNAR | Test | 0.6948615 | 102.51249 | 0.7415651 | 145.06583 | 0.8302159 | 0.0776162 |
| TidyDensity | 1 | ARIMA | Test | 0.5812330 | 92.44299 | 0.7992708 | 107.50142 | 0.8024102 | 0.0653739 |
| TidyDensity | 2 | LM | Test | 0.6566874 | 145.48457 | 0.9030304 | 102.49879 | 0.8794060 | 0.1549750 |
| TidyDensity | 3 | NULL | NA | NA | NA | NA | NA | NA | NA |
| TidyDensity | 4 | NNAR | Test | 0.5892195 | 76.57585 | 0.8102533 | 119.53701 | 0.8215388 | 0.0090243 |
| tidyAML | 1 | ARIMA | Test | 0.7762918 | 105.11741 | 0.7695001 | 113.56684 | 0.9728675 | 0.0323459 |
| tidyAML | 2 | LM | Test | 0.6916208 | 122.49921 | 0.6855700 | 90.76577 | 0.8560066 | 0.0000597 |
| tidyAML | 3 | NULL | NA | NA | NA | NA | NA | NA | NA |
| tidyAML | 4 | NNAR | Test | 0.7029205 | 120.53779 | 0.6967708 | 93.41463 | 0.8974642 | 0.0231520 |
| RandomWalker | 1 | ARIMA | Test | 1.2211494 | 108.15625 | 0.6928882 | 176.39008 | 1.3868840 | 0.0119513 |
| RandomWalker | 2 | LM | Test | 1.2344693 | 106.60491 | 0.7004460 | 187.97665 | 1.3746716 | 0.0002692 |
| RandomWalker | 3 | NULL | NA | NA | NA | NA | NA | NA | NA |
| RandomWalker | 4 | NNAR | Test | 1.2809428 | 153.32241 | 0.7268154 | 155.73102 | 1.4580073 | 0.0283856 |

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
    ## 1 healthyR.da…         2 LM          Test  0.729 183.  0.795 128.  0.914 9.85e-2
    ## 2 healthyR             4 NNAR        Test  0.860 103.  0.899 173.  1.03  1.13e-2
    ## 3 healthyR.ts          1 ARIMA       Test  0.917 114.  0.691 164.  1.19  1.79e-2
    ## 4 healthyverse         2 LM          Test  0.805 167.  0.945  93.6 0.965 7.72e-2
    ## 5 healthyR.ai          1 ARIMA       Test  0.708 113.  0.755 157.  0.830 8.46e-2
    ## 6 TidyDensity          1 ARIMA       Test  0.581  92.4 0.799 108.  0.802 6.54e-2
    ## 7 tidyAML              2 LM          Test  0.692 122.  0.686  90.8 0.856 5.97e-5
    ## 8 RandomWalker         2 LM          Test  1.23  107.  0.700 188.  1.37  2.69e-4

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
    ## 1 healthyR.data <tibble>     <tibble>     <split [1639|28]> <mdl_tm_t [1 × 5]>
    ## 2 healthyR      <tibble>     <tibble>     <split [1633|28]> <mdl_tm_t [1 × 5]>
    ## 3 healthyR.ts   <tibble>     <tibble>     <split [1577|28]> <mdl_tm_t [1 × 5]>
    ## 4 healthyverse  <tibble>     <tibble>     <split [1547|28]> <mdl_tm_t [1 × 5]>
    ## 5 healthyR.ai   <tibble>     <tibble>     <split [1372|28]> <mdl_tm_t [1 × 5]>
    ## 6 TidyDensity   <tibble>     <tibble>     <split [1223|28]> <mdl_tm_t [1 × 5]>
    ## 7 tidyAML       <tibble>     <tibble>     <split [831|28]>  <mdl_tm_t [1 × 5]>
    ## 8 RandomWalker  <tibble>     <tibble>     <split [253|28]>  <mdl_tm_t [1 × 5]>

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
