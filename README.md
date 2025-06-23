Time Series Analysis and Nested Modeling of the Healthyverse Packages
================
Steven P. Sanderson II, MPH - Date:
23 June, 2025

This analysis follows a *Nested Modeltime Workflow*.

## Get Data

``` r
glimpse(downloads_tbl)
```

    ## Rows: 143,486
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

The last day in the data set is 2025-06-21 23:37:45, the file was
birthed on: 2024-08-07 07:35:44.428716, and at report knit time is
-7644.03 hours old. Happy analyzing!

Now that we have our data lets take a look at it using the `skimr`
package.

``` r
skim(downloads_tbl)
```

|                                                  |               |
|:-------------------------------------------------|:--------------|
| Name                                             | downloads_tbl |
| Number of rows                                   | 143486        |
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
| r_version     |    103653 |          0.28 |   5 |   5 |     0 |       48 |          0 |
| r_arch        |    103653 |          0.28 |   3 |   7 |     0 |        5 |          0 |
| r_os          |    103653 |          0.28 |   7 |  15 |     0 |       22 |          0 |
| package       |         0 |          1.00 |   7 |  13 |     0 |        8 |          0 |
| version       |         0 |          1.00 |   5 |  17 |     0 |       60 |          0 |
| country       |     12133 |          0.92 |   2 |   2 |     0 |      163 |          0 |

**Variable type: Date**

| skim_variable | n_missing | complete_rate | min | max | median | n_unique |
|:---|---:|---:|:---|:---|:---|---:|
| date | 0 | 1 | 2020-11-23 | 2025-06-21 | 2023-07-10 | 1672 |

**Variable type: numeric**

| skim_variable | n_missing | complete_rate | mean | sd | p0 | p25 | p50 | p75 | p100 | hist |
|:---|---:|---:|---:|---:|---:|---:|---:|---:|---:|:---|
| size | 0 | 1 | 1132750.59 | 1515587.12 | 355 | 14701 | 289858 | 2367703.00 | 5677952 | ▇▁▂▁▁ |
| ip_id | 0 | 1 | 10440.56 | 18556.59 | 1 | 295 | 3048 | 11827.75 | 209747 | ▇▁▁▁▁ |

**Variable type: POSIXct**

| skim_variable | n_missing | complete_rate | min | max | median | n_unique |
|:---|---:|---:|:---|:---|:---|---:|
| date_time | 0 | 1 | 2020-11-23 09:00:41 | 2025-06-21 23:37:45 | 2023-07-10 12:16:47 | 87960 |

**Variable type: Timespan**

| skim_variable | n_missing | complete_rate | min | max | median | n_unique |
|:--------------|----------:|--------------:|----:|----:|-------:|---------:|
| time          |         0 |             1 |   0 |  59 |     32 |       60 |

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
    ## -148.44  -35.90  -11.51   26.89  816.11 
    ## 
    ## Coefficients:
    ##                                                      Estimate Std. Error
    ## (Intercept)                                        -1.784e+02  6.718e+01
    ## date                                                1.096e-02  3.557e-03
    ## lag(value, 1)                                       1.055e-01  2.426e-02
    ## lag(value, 7)                                       9.526e-02  2.523e-02
    ## lag(value, 14)                                      8.502e-02  2.523e-02
    ## lag(value, 21)                                      6.652e-02  2.530e-02
    ## lag(value, 28)                                      7.096e-02  2.519e-02
    ## lag(value, 35)                                      6.807e-02  2.524e-02
    ## lag(value, 42)                                      5.464e-02  2.536e-02
    ## lag(value, 49)                                      6.302e-02  2.524e-02
    ## month(date, label = TRUE).L                        -9.734e+00  5.126e+00
    ## month(date, label = TRUE).Q                         3.072e+00  5.104e+00
    ## month(date, label = TRUE).C                        -1.319e+01  5.145e+00
    ## month(date, label = TRUE)^4                        -6.497e+00  5.158e+00
    ## month(date, label = TRUE)^5                        -1.138e+01  5.129e+00
    ## month(date, label = TRUE)^6                        -4.263e+00  5.203e+00
    ## month(date, label = TRUE)^7                        -6.979e+00  5.098e+00
    ## month(date, label = TRUE)^8                        -2.719e+00  5.092e+00
    ## month(date, label = TRUE)^9                         5.043e+00  5.093e+00
    ## month(date, label = TRUE)^10                        2.407e+00  5.083e+00
    ## month(date, label = TRUE)^11                       -3.309e+00  5.130e+00
    ## fourier_vec(date, type = "sin", K = 1, period = 7) -1.193e+01  2.325e+00
    ## fourier_vec(date, type = "cos", K = 1, period = 7)  8.253e+00  2.448e+00
    ##                                                    t value Pr(>|t|)    
    ## (Intercept)                                         -2.656 0.007979 ** 
    ## date                                                 3.080 0.002106 ** 
    ## lag(value, 1)                                        4.351 1.44e-05 ***
    ## lag(value, 7)                                        3.776 0.000165 ***
    ## lag(value, 14)                                       3.370 0.000769 ***
    ## lag(value, 21)                                       2.629 0.008647 ** 
    ## lag(value, 28)                                       2.817 0.004905 ** 
    ## lag(value, 35)                                       2.696 0.007081 ** 
    ## lag(value, 42)                                       2.155 0.031346 *  
    ## lag(value, 49)                                       2.497 0.012615 *  
    ## month(date, label = TRUE).L                         -1.899 0.057728 .  
    ## month(date, label = TRUE).Q                          0.602 0.547356    
    ## month(date, label = TRUE).C                         -2.564 0.010448 *  
    ## month(date, label = TRUE)^4                         -1.260 0.208025    
    ## month(date, label = TRUE)^5                         -2.219 0.026656 *  
    ## month(date, label = TRUE)^6                         -0.819 0.412690    
    ## month(date, label = TRUE)^7                         -1.369 0.171213    
    ## month(date, label = TRUE)^8                         -0.534 0.593498    
    ## month(date, label = TRUE)^9                          0.990 0.322266    
    ## month(date, label = TRUE)^10                         0.474 0.635886    
    ## month(date, label = TRUE)^11                        -0.645 0.518962    
    ## fourier_vec(date, type = "sin", K = 1, period = 7)  -5.131 3.23e-07 ***
    ## fourier_vec(date, type = "cos", K = 1, period = 7)   3.372 0.000764 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 58.92 on 1600 degrees of freedom
    ##   (49 observations deleted due to missingness)
    ## Multiple R-squared:  0.2381, Adjusted R-squared:  0.2276 
    ## F-statistic: 22.72 on 22 and 1600 DF,  p-value: < 2.2e-16

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
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 2.75090310186933"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 21, 63 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 2.48895995902585"
    ## [1] "BEST method = 'lin', seasonal.factor = c( 21, 63 )"
    ## [1] "BEST lin OBJECTIVE FUNCTION = 2.48895995902585"
    ## [1] "CURRNET METHOD: nonlin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'nonlin' , seasonal.factor =  c( 21, 63 ) ...)"
    ## [1] "CURRENT nonlin OBJECTIVE FUNCTION = 8.07016559822027"
    ## [1] "BEST method = 'nonlin' PATH MEMBER = c( 21, 63 )"
    ## [1] "BEST nonlin OBJECTIVE FUNCTION = 8.07016559822027"
    ## [1] "CURRNET METHOD: both"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'both' , seasonal.factor =  c( 21, 63 ) ...)"
    ## [1] "CURRENT both OBJECTIVE FUNCTION = 4.27167575139159"
    ## [1] "BEST method = 'both' PATH MEMBER = c( 21, 63 )"
    ## [1] "BEST both OBJECTIVE FUNCTION = 4.27167575139159"

![](man/figures/README-nns_forecasting-1.png)<!-- -->

    ## Package: healthyR.ai
    ## [1] "CURRNET METHOD: lin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 63 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 4.70168744498508"
    ## [1] "BEST method = 'lin', seasonal.factor = c( 63 )"
    ## [1] "BEST lin OBJECTIVE FUNCTION = 4.70168744498508"
    ## [1] "CURRNET METHOD: nonlin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'nonlin' , seasonal.factor =  c( 63 ) ...)"
    ## [1] "CURRENT nonlin OBJECTIVE FUNCTION = 6.04278505517274"
    ## [1] "BEST method = 'nonlin' PATH MEMBER = c( 63 )"
    ## [1] "BEST nonlin OBJECTIVE FUNCTION = 6.04278505517274"
    ## [1] "CURRNET METHOD: both"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'both' , seasonal.factor =  c( 63 ) ...)"
    ## [1] "CURRENT both OBJECTIVE FUNCTION = 5.17955650297711"
    ## [1] "BEST method = 'both' PATH MEMBER = c( 63 )"
    ## [1] "BEST both OBJECTIVE FUNCTION = 5.17955650297711"

![](man/figures/README-nns_forecasting-2.png)<!-- -->

    ## Package: healthyR.data
    ## [1] "CURRNET METHOD: lin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 63 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 2.93072650145752"
    ## [1] "BEST method = 'lin', seasonal.factor = c( 63 )"
    ## [1] "BEST lin OBJECTIVE FUNCTION = 2.93072650145752"
    ## [1] "CURRNET METHOD: nonlin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'nonlin' , seasonal.factor =  c( 63 ) ...)"
    ## [1] "CURRENT nonlin OBJECTIVE FUNCTION = 2.55706034225036"
    ## [1] "BEST method = 'nonlin' PATH MEMBER = c( 63 )"
    ## [1] "BEST nonlin OBJECTIVE FUNCTION = 2.55706034225036"
    ## [1] "CURRNET METHOD: both"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'both' , seasonal.factor =  c( 63 ) ...)"
    ## [1] "CURRENT both OBJECTIVE FUNCTION = 2.37773096391104"
    ## [1] "BEST method = 'both' PATH MEMBER = c( 63 )"
    ## [1] "BEST both OBJECTIVE FUNCTION = 2.37773096391104"

![](man/figures/README-nns_forecasting-3.png)<!-- -->

    ## Package: healthyR.ts
    ## [1] "CURRNET METHOD: lin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 63 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 1.78213793151712"
    ## [1] "BEST method = 'lin', seasonal.factor = c( 63 )"
    ## [1] "BEST lin OBJECTIVE FUNCTION = 1.78213793151712"
    ## [1] "CURRNET METHOD: nonlin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'nonlin' , seasonal.factor =  c( 63 ) ...)"
    ## [1] "CURRENT nonlin OBJECTIVE FUNCTION = 10.4825250852073"
    ## [1] "BEST method = 'nonlin' PATH MEMBER = c( 63 )"
    ## [1] "BEST nonlin OBJECTIVE FUNCTION = 10.4825250852073"
    ## [1] "CURRNET METHOD: both"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'both' , seasonal.factor =  c( 63 ) ...)"
    ## [1] "CURRENT both OBJECTIVE FUNCTION = 4.23050142525695"
    ## [1] "BEST method = 'both' PATH MEMBER = c( 63 )"
    ## [1] "BEST both OBJECTIVE FUNCTION = 4.23050142525695"

![](man/figures/README-nns_forecasting-4.png)<!-- -->

    ## Package: healthyverse
    ## [1] "CURRNET METHOD: lin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 49 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 5.04884426468129"
    ## [1] "BEST method = 'lin', seasonal.factor = c( 49 )"
    ## [1] "BEST lin OBJECTIVE FUNCTION = 5.04884426468129"
    ## [1] "CURRNET METHOD: nonlin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'nonlin' , seasonal.factor =  c( 49 ) ...)"
    ## [1] "CURRENT nonlin OBJECTIVE FUNCTION = 6.5464960266448"
    ## [1] "BEST method = 'nonlin' PATH MEMBER = c( 49 )"
    ## [1] "BEST nonlin OBJECTIVE FUNCTION = 6.5464960266448"
    ## [1] "CURRNET METHOD: both"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'both' , seasonal.factor =  c( 49 ) ...)"
    ## [1] "CURRENT both OBJECTIVE FUNCTION = 5.39544548685626"
    ## [1] "BEST method = 'both' PATH MEMBER = c( 49 )"
    ## [1] "BEST both OBJECTIVE FUNCTION = 5.39544548685626"

![](man/figures/README-nns_forecasting-5.png)<!-- -->

    ## Package: RandomWalker
    ## [1] "CURRNET METHOD: lin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 70 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 3.04633872562847"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 70, 49 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 2.81148269938491"
    ## [1] "BEST method = 'lin', seasonal.factor = c( 70, 49 )"
    ## [1] "BEST lin OBJECTIVE FUNCTION = 2.81148269938491"
    ## [1] "CURRNET METHOD: nonlin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'nonlin' , seasonal.factor =  c( 70, 49 ) ...)"
    ## [1] "CURRENT nonlin OBJECTIVE FUNCTION = 2.8587992070325"
    ## [1] "BEST method = 'nonlin' PATH MEMBER = c( 70, 49 )"
    ## [1] "BEST nonlin OBJECTIVE FUNCTION = 2.8587992070325"
    ## [1] "CURRNET METHOD: both"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'both' , seasonal.factor =  c( 70, 49 ) ...)"
    ## [1] "CURRENT both OBJECTIVE FUNCTION = 2.68946912278006"
    ## [1] "BEST method = 'both' PATH MEMBER = c( 70, 49 )"
    ## [1] "BEST both OBJECTIVE FUNCTION = 2.68946912278006"

![](man/figures/README-nns_forecasting-6.png)<!-- -->

    ## Package: tidyAML
    ## [1] "CURRNET METHOD: lin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 84 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 5.34480420298277"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 84, 91 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 5.02934710401352"
    ## [1] "BEST method = 'lin', seasonal.factor = c( 84, 91 )"
    ## [1] "BEST lin OBJECTIVE FUNCTION = 5.02934710401352"
    ## [1] "CURRNET METHOD: nonlin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'nonlin' , seasonal.factor =  c( 84, 91 ) ...)"
    ## [1] "CURRENT nonlin OBJECTIVE FUNCTION = 3.11796994574159"
    ## [1] "BEST method = 'nonlin' PATH MEMBER = c( 84, 91 )"
    ## [1] "BEST nonlin OBJECTIVE FUNCTION = 3.11796994574159"
    ## [1] "CURRNET METHOD: both"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'both' , seasonal.factor =  c( 84, 91 ) ...)"
    ## [1] "CURRENT both OBJECTIVE FUNCTION = 3.77493782194444"
    ## [1] "BEST method = 'both' PATH MEMBER = c( 84, 91 )"
    ## [1] "BEST both OBJECTIVE FUNCTION = 3.77493782194444"

![](man/figures/README-nns_forecasting-7.png)<!-- -->

    ## Package: TidyDensity
    ## [1] "CURRNET METHOD: lin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 63 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 2.88840607606434"
    ## [1] "BEST method = 'lin', seasonal.factor = c( 63 )"
    ## [1] "BEST lin OBJECTIVE FUNCTION = 2.88840607606434"
    ## [1] "CURRNET METHOD: nonlin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'nonlin' , seasonal.factor =  c( 63 ) ...)"
    ## [1] "CURRENT nonlin OBJECTIVE FUNCTION = 10.3116134237973"
    ## [1] "BEST method = 'nonlin' PATH MEMBER = c( 63 )"
    ## [1] "BEST nonlin OBJECTIVE FUNCTION = 10.3116134237973"
    ## [1] "CURRNET METHOD: both"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'both' , seasonal.factor =  c( 63 ) ...)"
    ## [1] "CURRENT both OBJECTIVE FUNCTION = 5.98627262440551"
    ## [1] "BEST method = 'both' PATH MEMBER = c( 63 )"
    ## [1] "BEST both OBJECTIVE FUNCTION = 5.98627262440551"

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
    ## 1 healthyR.data <tibble [1,664 × 2]> <tibble [28 × 2]> <split [1636|28]>
    ## 2 healthyR      <tibble [1,658 × 2]> <tibble [28 × 2]> <split [1630|28]>
    ## 3 healthyR.ts   <tibble [1,602 × 2]> <tibble [28 × 2]> <split [1574|28]>
    ## 4 healthyverse  <tibble [1,572 × 2]> <tibble [28 × 2]> <split [1544|28]>
    ## 5 healthyR.ai   <tibble [1,397 × 2]> <tibble [28 × 2]> <split [1369|28]>
    ## 6 TidyDensity   <tibble [1,248 × 2]> <tibble [28 × 2]> <split [1220|28]>
    ## 7 tidyAML       <tibble [856 × 2]>   <tibble [28 × 2]> <split [828|28]> 
    ## 8 RandomWalker  <tibble [278 × 2]>   <tibble [28 × 2]> <split [250|28]>

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
| healthyR.data | 1 | ARIMA | Test | 0.6860952 | 153.00227 | 0.7336403 | 132.55507 | 0.8909278 | 0.0713350 |
| healthyR.data | 2 | LM | Test | 0.7321297 | 189.76972 | 0.7828649 | 132.11290 | 0.9130454 | 0.2341258 |
| healthyR.data | 3 | NULL | NA | NA | NA | NA | NA | NA | NA |
| healthyR.data | 4 | NNAR | Test | 0.7268663 | 97.80670 | 0.7772368 | 183.13851 | 0.9662129 | 0.0462799 |
| healthyR | 1 | ARIMA | Test | 0.8699398 | 104.89840 | 0.9402832 | 171.74557 | 1.0183108 | 0.0000024 |
| healthyR | 2 | LM | Test | 0.8762346 | 101.27847 | 0.9470870 | 183.03406 | 1.0309422 | 0.2343843 |
| healthyR | 3 | NULL | NA | NA | NA | NA | NA | NA | NA |
| healthyR | 4 | NNAR | Test | 0.8666557 | 103.22396 | 0.9367336 | 169.89367 | 1.0132623 | 0.0014864 |
| healthyR.ts | 1 | ARIMA | Test | 0.9829473 | 106.38204 | 0.7343483 | 156.07858 | 1.2426837 | 0.0703193 |
| healthyR.ts | 2 | LM | Test | 1.0895182 | 158.48925 | 0.8139662 | 156.87186 | 1.3258682 | 0.2701551 |
| healthyR.ts | 3 | NULL | NA | NA | NA | NA | NA | NA | NA |
| healthyR.ts | 4 | NNAR | Test | 0.9915546 | 101.38928 | 0.7407788 | 186.29038 | 1.2595987 | 0.0614239 |
| healthyverse | 1 | ARIMA | Test | 0.7261743 | 133.44648 | 1.1549155 | 88.48747 | 0.9012778 | 0.0239640 |
| healthyverse | 2 | LM | Test | 0.7062548 | 151.62057 | 1.1232353 | 83.72515 | 0.8657635 | 0.2435268 |
| healthyverse | 3 | NULL | NA | NA | NA | NA | NA | NA | NA |
| healthyverse | 4 | NNAR | Test | 0.8231137 | 105.12098 | 1.3090890 | 111.74799 | 0.9938886 | 0.0289306 |
| healthyR.ai | 1 | ARIMA | Test | 0.7381100 | 114.61364 | 0.7926955 | 166.97861 | 0.8794198 | 0.0059412 |
| healthyR.ai | 2 | LM | Test | 0.7159854 | 107.99503 | 0.7689347 | 152.35821 | 0.8556938 | 0.2010573 |
| healthyR.ai | 3 | NULL | NA | NA | NA | NA | NA | NA | NA |
| healthyR.ai | 4 | NNAR | Test | 0.7053124 | 97.78457 | 0.7574724 | 147.34240 | 0.8556843 | 0.0123173 |
| TidyDensity | 1 | ARIMA | Test | 0.5565436 | 92.07575 | 0.8560563 | 108.77939 | 0.7895655 | 0.0353067 |
| TidyDensity | 2 | LM | Test | 0.6594879 | 150.51895 | 1.0144016 | 104.33571 | 0.8807221 | 0.1626870 |
| TidyDensity | 3 | NULL | NA | NA | NA | NA | NA | NA | NA |
| TidyDensity | 4 | NNAR | Test | 0.5580320 | 73.86903 | 0.8583456 | 117.54019 | 0.8049529 | 0.0009107 |
| tidyAML | 1 | ARIMA | Test | 0.7756021 | 101.02475 | 0.8310780 | 135.64774 | 0.9784061 | 0.0218157 |
| tidyAML | 2 | LM | Test | 0.6262841 | 125.22787 | 0.6710799 | 88.21327 | 0.7867191 | 0.0094440 |
| tidyAML | 3 | NULL | NA | NA | NA | NA | NA | NA | NA |
| tidyAML | 4 | NNAR | Test | 0.6988432 | 125.16496 | 0.7488289 | 110.36243 | 0.8699073 | 0.0000146 |
| RandomWalker | 1 | ARIMA | Test | 1.2434026 | 132.04807 | 0.7424997 | 156.99650 | 1.3929372 | 0.0068671 |
| RandomWalker | 2 | LM | Test | 1.2757326 | 114.02920 | 0.7618056 | 187.43755 | 1.3918035 | 0.0568304 |
| RandomWalker | 3 | NULL | NA | NA | NA | NA | NA | NA | NA |
| RandomWalker | 4 | NNAR | Test | 1.2147529 | 140.77272 | 0.7253914 | 140.66390 | 1.4085540 | 0.0561597 |

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
    ## 1 healthyR.da…         1 ARIMA       Test  0.686 153.  0.734 133.  0.891 0.0713 
    ## 2 healthyR             4 NNAR        Test  0.867 103.  0.937 170.  1.01  0.00149
    ## 3 healthyR.ts          1 ARIMA       Test  0.983 106.  0.734 156.  1.24  0.0703 
    ## 4 healthyverse         2 LM          Test  0.706 152.  1.12   83.7 0.866 0.244  
    ## 5 healthyR.ai          4 NNAR        Test  0.705  97.8 0.757 147.  0.856 0.0123 
    ## 6 TidyDensity          1 ARIMA       Test  0.557  92.1 0.856 109.  0.790 0.0353 
    ## 7 tidyAML              2 LM          Test  0.626 125.  0.671  88.2 0.787 0.00944
    ## 8 RandomWalker         2 LM          Test  1.28  114.  0.762 187.  1.39  0.0568

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
    ## 1 healthyR.data <tibble>     <tibble>     <split [1636|28]> <mdl_tm_t [1 × 5]>
    ## 2 healthyR      <tibble>     <tibble>     <split [1630|28]> <mdl_tm_t [1 × 5]>
    ## 3 healthyR.ts   <tibble>     <tibble>     <split [1574|28]> <mdl_tm_t [1 × 5]>
    ## 4 healthyverse  <tibble>     <tibble>     <split [1544|28]> <mdl_tm_t [1 × 5]>
    ## 5 healthyR.ai   <tibble>     <tibble>     <split [1369|28]> <mdl_tm_t [1 × 5]>
    ## 6 TidyDensity   <tibble>     <tibble>     <split [1220|28]> <mdl_tm_t [1 × 5]>
    ## 7 tidyAML       <tibble>     <tibble>     <split [828|28]>  <mdl_tm_t [1 × 5]>
    ## 8 RandomWalker  <tibble>     <tibble>     <split [250|28]>  <mdl_tm_t [1 × 5]>

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
