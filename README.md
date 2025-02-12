Time Series Analysis and Nested Modeling of the Healthyverse Packages
================
Steven P. Sanderson II, MPH - Date:
12 February, 2025

This analysis follows a *Nested Modeltime Workflow*.

## Get Data

``` r
glimpse(downloads_tbl)
```

    ## Rows: 130,319
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

The last day in the data set is 2025-02-10 23:30:41, the file was
birthed on: 2024-08-07 07:35:44, and at report knit time is -4499.92
hours old. Happy analyzing!

Now that we have our data lets take a look at it using the `skimr`
package.

``` r
skim(downloads_tbl)
```

|                                                  |               |
|:-------------------------------------------------|:--------------|
| Name                                             | downloads_tbl |
| Number of rows                                   | 130319        |
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
| r_version     |     93144 |          0.29 |   5 |   5 |     0 |       45 |          0 |
| r_arch        |     93144 |          0.29 |   3 |   7 |     0 |        5 |          0 |
| r_os          |     93144 |          0.29 |   7 |  15 |     0 |       21 |          0 |
| package       |         0 |          1.00 |   7 |  13 |     0 |        8 |          0 |
| version       |         0 |          1.00 |   5 |  17 |     0 |       60 |          0 |
| country       |     10969 |          0.92 |   2 |   2 |     0 |      160 |          0 |

**Variable type: Date**

| skim_variable | n_missing | complete_rate | min | max | median | n_unique |
|:---|---:|---:|:---|:---|:---|---:|
| date | 0 | 1 | 2020-11-23 | 2025-02-10 | 2023-05-01 | 1541 |

**Variable type: numeric**

| skim_variable | n_missing | complete_rate | mean | sd | p0 | p25 | p50 | p75 | p100 | hist |
|:---|---:|---:|---:|---:|---:|---:|---:|---:|---:|:---|
| size | 0 | 1 | 1140630.60 | 1530420.17 | 355 | 14701 | 260384 | 2367847 | 5677952 | ▇▁▂▁▁ |
| ip_id | 0 | 1 | 10463.11 | 18459.34 | 1 | 328 | 3098 | 11961 | 209747 | ▇▁▁▁▁ |

**Variable type: POSIXct**

| skim_variable | n_missing | complete_rate | min | max | median | n_unique |
|:---|---:|---:|:---|:---|:---|---:|
| date_time | 0 | 1 | 2020-11-23 09:00:41 | 2025-02-10 23:30:41 | 2023-05-01 10:23:12 | 78916 |

**Variable type: Timespan**

| skim_variable | n_missing | complete_rate | min | max |     median | n_unique |
|:--------------|----------:|--------------:|----:|----:|-----------:|---------:|
| time          |         0 |             1 |   0 |  59 | 12H 6M 48S |       60 |

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
    ## -155.38  -34.96   -9.78   27.29  808.57 
    ## 
    ## Coefficients:
    ##                                                      Estimate Std. Error
    ## (Intercept)                                        -1.921e+02  7.705e+01
    ## date                                                1.151e-02  4.085e-03
    ## lag(value, 1)                                       1.132e-01  2.517e-02
    ## lag(value, 7)                                       8.857e-02  2.617e-02
    ## lag(value, 14)                                      1.099e-01  2.629e-02
    ## lag(value, 21)                                      5.133e-02  2.643e-02
    ## lag(value, 28)                                      6.250e-02  2.628e-02
    ## lag(value, 35)                                      7.938e-02  2.637e-02
    ## lag(value, 42)                                      5.490e-02  2.653e-02
    ## lag(value, 49)                                      8.872e-02  2.637e-02
    ## month(date, label = TRUE).L                        -1.136e+01  5.261e+00
    ## month(date, label = TRUE).Q                         2.010e+00  5.185e+00
    ## month(date, label = TRUE).C                        -1.122e+01  5.273e+00
    ## month(date, label = TRUE)^4                        -8.074e+00  5.280e+00
    ## month(date, label = TRUE)^5                        -1.273e+01  5.267e+00
    ## month(date, label = TRUE)^6                        -1.331e+00  5.350e+00
    ## month(date, label = TRUE)^7                        -8.980e+00  5.279e+00
    ## month(date, label = TRUE)^8                        -2.348e+00  5.294e+00
    ## month(date, label = TRUE)^9                         4.220e+00  5.297e+00
    ## month(date, label = TRUE)^10                        5.215e+00  5.299e+00
    ## month(date, label = TRUE)^11                       -6.408e+00  5.313e+00
    ## fourier_vec(date, type = "sin", K = 1, period = 7) -1.160e+01  2.419e+00
    ## fourier_vec(date, type = "cos", K = 1, period = 7)  7.479e+00  2.542e+00
    ##                                                    t value Pr(>|t|)    
    ## (Intercept)                                         -2.493 0.012760 *  
    ## date                                                 2.817 0.004910 ** 
    ## lag(value, 1)                                        4.499 7.37e-06 ***
    ## lag(value, 7)                                        3.385 0.000732 ***
    ## lag(value, 14)                                       4.179 3.10e-05 ***
    ## lag(value, 21)                                       1.942 0.052336 .  
    ## lag(value, 28)                                       2.378 0.017532 *  
    ## lag(value, 35)                                       3.010 0.002657 ** 
    ## lag(value, 42)                                       2.069 0.038715 *  
    ## lag(value, 49)                                       3.364 0.000787 ***
    ## month(date, label = TRUE).L                         -2.160 0.030960 *  
    ## month(date, label = TRUE).Q                          0.388 0.698288    
    ## month(date, label = TRUE).C                         -2.127 0.033554 *  
    ## month(date, label = TRUE)^4                         -1.529 0.126445    
    ## month(date, label = TRUE)^5                         -2.417 0.015758 *  
    ## month(date, label = TRUE)^6                         -0.249 0.803628    
    ## month(date, label = TRUE)^7                         -1.701 0.089141 .  
    ## month(date, label = TRUE)^8                         -0.444 0.657422    
    ## month(date, label = TRUE)^9                          0.797 0.425768    
    ## month(date, label = TRUE)^10                         0.984 0.325213    
    ## month(date, label = TRUE)^11                        -1.206 0.227961    
    ## fourier_vec(date, type = "sin", K = 1, period = 7)  -4.796 1.78e-06 ***
    ## fourier_vec(date, type = "cos", K = 1, period = 7)   2.942 0.003308 ** 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 58.3 on 1469 degrees of freedom
    ##   (49 observations deleted due to missingness)
    ## Multiple R-squared:  0.2578, Adjusted R-squared:  0.2467 
    ## F-statistic:  23.2 on 22 and 1469 DF,  p-value: < 2.2e-16

![](man/figures/README-base_data_frame-1.png)<!-- -->

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

    ## # A tibble: 9 × 4
    ##   package       .actual_data         .future_data      .splits          
    ##   <fct>         <list>               <list>            <list>           
    ## 1 healthyR.data <tibble [1,506 × 2]> <tibble [28 × 2]> <split [1478|28]>
    ## 2 healthyR      <tibble [1,499 × 2]> <tibble [28 × 2]> <split [1471|28]>
    ## 3 <NA>          <tibble [28 × 2]>    <tibble [28 × 2]> <split [0|28]>   
    ## 4 healthyR.ts   <tibble [1,445 × 2]> <tibble [28 × 2]> <split [1417|28]>
    ## 5 healthyverse  <tibble [1,416 × 2]> <tibble [28 × 2]> <split [1388|28]>
    ## 6 healthyR.ai   <tibble [1,242 × 2]> <tibble [28 × 2]> <split [1214|28]>
    ## 7 TidyDensity   <tibble [1,096 × 2]> <tibble [28 × 2]> <split [1068|28]>
    ## 8 tidyAML       <tibble [711 × 2]>   <tibble [28 × 2]> <split [683|28]> 
    ## 9 RandomWalker  <tibble [145 × 2]>   <tibble [28 × 2]> <split [117|28]>

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
| healthyR.data | 1 | ARIMA | Test | 0.8279298 | 158.90824 | 0.6669422 | 144.52388 | 1.0085650 | 0.0207365 |
| healthyR.data | 2 | LM | Test | 0.8680835 | 162.29259 | 0.6992882 | 136.76984 | 1.0566334 | 0.1187469 |
| healthyR.data | 3 | EARTH | Test | 0.9276077 | 197.69031 | 0.7472381 | 136.25388 | 1.1223989 | 0.1187469 |
| healthyR.data | 4 | NNAR | Test | 0.7602174 | 113.02168 | 0.6123962 | 176.93566 | 0.8923487 | 0.0617197 |
| healthyR | 1 | ARIMA | Test | 0.8235029 | 93.33719 | 0.6289088 | 164.57689 | 0.9562345 | 0.1354345 |
| healthyR | 2 | LM | Test | 0.8577501 | 100.49810 | 0.6550634 | 190.33477 | 1.0003074 | 0.0169978 |
| healthyR | 3 | EARTH | Test | 0.8513143 | 101.73873 | 0.6501483 | 171.18832 | 1.0037895 | 0.0169978 |
| healthyR | 4 | NNAR | Test | 0.8181956 | 100.30023 | 0.6248556 | 160.19584 | 0.9609520 | 0.1280564 |
| healthyR.ts | 1 | ARIMA | Test | 0.8287879 | 155.15977 | 0.5677986 | 133.63999 | 1.0818215 | 0.0009198 |
| healthyR.ts | 2 | LM | Test | 0.8712464 | 203.97373 | 0.5968867 | 132.84754 | 1.0847769 | 0.0009198 |
| healthyR.ts | 3 | EARTH | Test | 0.8650847 | 197.92337 | 0.5926653 | 132.87060 | 1.0833724 | NA |
| healthyR.ts | 4 | NNAR | Test | 0.7457988 | 92.86756 | 0.5109432 | 172.04975 | 1.0272900 | 0.1794202 |
| healthyverse | 1 | ARIMA | Test | 0.5885811 | 173.87256 | 0.6417503 | 102.35796 | 0.7007475 | 0.0975863 |
| healthyverse | 2 | LM | Test | 0.6158064 | 221.63895 | 0.6714350 | 94.63975 | 0.7614180 | 0.0117324 |
| healthyverse | 3 | EARTH | Test | 0.6202917 | 156.40071 | 0.6763255 | 111.48317 | 0.7273858 | 0.0117324 |
| healthyverse | 4 | NNAR | Test | 0.6019751 | 141.80534 | 0.6563543 | 114.64260 | 0.7007700 | 0.1914758 |
| healthyR.ai | 1 | ARIMA | Test | 0.7444600 | 95.58458 | 0.6303509 | 174.68291 | 0.9125529 | 0.1817970 |
| healthyR.ai | 2 | LM | Test | 0.7922245 | 122.99635 | 0.6707942 | 160.09961 | 0.9760820 | 0.0212878 |
| healthyR.ai | 3 | EARTH | Test | 0.8012896 | 155.47593 | 0.6784698 | 136.29707 | 1.0246231 | 0.0212878 |
| healthyR.ai | 4 | NNAR | Test | 0.7280952 | 108.70352 | 0.6164945 | 161.20445 | 0.8777540 | 0.2200719 |
| TidyDensity | 1 | ARIMA | Test | 0.5419200 | 132.67415 | 0.6218007 | 105.71483 | 0.6693319 | 0.0739919 |
| TidyDensity | 2 | LM | Test | 0.6139335 | 223.01916 | 0.7044291 | 100.49638 | 0.7429912 | 0.0188955 |
| TidyDensity | 3 | EARTH | Test | 0.5442367 | 132.58437 | 0.6244588 | 104.55138 | 0.6959948 | 0.0188955 |
| TidyDensity | 4 | NNAR | Test | 0.5584017 | 96.93219 | 0.6407118 | 131.13493 | 0.6915537 | 0.1477910 |
| tidyAML | 1 | ARIMA | Test | 0.7153621 | 164.73752 | 0.7815063 | 91.12426 | 0.9065199 | 0.1668064 |
| tidyAML | 2 | LM | Test | 0.6909158 | 126.18321 | 0.7547996 | 94.41891 | 0.8698155 | 0.0011831 |
| tidyAML | 3 | EARTH | Test | 0.9330403 | 237.17987 | 1.0193115 | 96.36722 | 1.1806838 | 0.0011831 |
| tidyAML | 4 | NNAR | Test | 0.6952559 | 139.63216 | 0.7595410 | 91.94705 | 0.8904114 | 0.0036388 |
| RandomWalker | 1 | ARIMA | Test | 1.6584276 | 228.00803 | 0.8029858 | 174.19637 | 1.9375452 | 0.2684605 |
| RandomWalker | 2 | LM | Test | 1.1787474 | 90.54046 | 0.5707319 | 153.96674 | 1.3987690 | 0.0176271 |
| RandomWalker | 3 | EARTH | Test | 1.1783671 | 91.04613 | 0.5705477 | 151.48166 | 1.4028091 | NA |
| RandomWalker | 4 | NNAR | Test | 1.2559094 | 169.61054 | 0.6080926 | 154.74226 | 1.4350333 | 0.0236320 |

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
    ## 1 healthyR.da…         4 NNAR        Test  0.760 113.  0.612 177.  0.892 0.0617 
    ## 2 healthyR             1 ARIMA       Test  0.824  93.3 0.629 165.  0.956 0.135  
    ## 3 healthyR.ts          4 NNAR        Test  0.746  92.9 0.511 172.  1.03  0.179  
    ## 4 healthyverse         1 ARIMA       Test  0.589 174.  0.642 102.  0.701 0.0976 
    ## 5 healthyR.ai          4 NNAR        Test  0.728 109.  0.616 161.  0.878 0.220  
    ## 6 TidyDensity          1 ARIMA       Test  0.542 133.  0.622 106.  0.669 0.0740 
    ## 7 tidyAML              2 LM          Test  0.691 126.  0.755  94.4 0.870 0.00118
    ## 8 RandomWalker         2 LM          Test  1.18   90.5 0.571 154.  1.40  0.0176

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
    ## 1 healthyR.data <tibble>     <tibble>     <split [1478|28]> <mdl_tm_t [1 × 5]>
    ## 2 healthyR      <tibble>     <tibble>     <split [1471|28]> <mdl_tm_t [1 × 5]>
    ## 3 healthyR.ts   <tibble>     <tibble>     <split [1417|28]> <mdl_tm_t [1 × 5]>
    ## 4 healthyverse  <tibble>     <tibble>     <split [1388|28]> <mdl_tm_t [1 × 5]>
    ## 5 healthyR.ai   <tibble>     <tibble>     <split [1214|28]> <mdl_tm_t [1 × 5]>
    ## 6 TidyDensity   <tibble>     <tibble>     <split [1068|28]> <mdl_tm_t [1 × 5]>
    ## 7 tidyAML       <tibble>     <tibble>     <split [683|28]>  <mdl_tm_t [1 × 5]>
    ## 8 RandomWalker  <tibble>     <tibble>     <split [117|28]>  <mdl_tm_t [1 × 5]>

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
