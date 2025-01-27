Time Series Analysis and Nested Modeling of the Healthyverse Packages
================
Steven P. Sanderson II, MPH - Date:
27 January, 2025

This analysis follows a *Nested Modeltime Workflow*.

## Get Data

``` r
glimpse(downloads_tbl)
```

    ## Rows: 128,642
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

The last day in the data set is 2025-01-25 23:33:51, the file was
birthed on: 2024-08-07 07:35:44, and at report knit time is -4115.97
hours old. Happy analyzing!

Now that we have our data lets take a look at it using the `skimr`
package.

``` r
skim(downloads_tbl)
```

|                                                  |               |
|:-------------------------------------------------|:--------------|
| Name                                             | downloads_tbl |
| Number of rows                                   | 128642        |
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
| r_version     |     91737 |          0.29 |   5 |   5 |     0 |       45 |          0 |
| r_arch        |     91737 |          0.29 |   3 |   7 |     0 |        5 |          0 |
| r_os          |     91737 |          0.29 |   7 |  15 |     0 |       21 |          0 |
| package       |         0 |          1.00 |   7 |  13 |     0 |        8 |          0 |
| version       |         0 |          1.00 |   5 |  17 |     0 |       60 |          0 |
| country       |     10888 |          0.92 |   2 |   2 |     0 |      160 |          0 |

**Variable type: Date**

| skim_variable | n_missing | complete_rate | min | max | median | n_unique |
|:---|---:|---:|:---|:---|:---|---:|
| date | 0 | 1 | 2020-11-23 | 2025-01-25 | 2023-04-24 | 1525 |

**Variable type: numeric**

| skim_variable | n_missing | complete_rate | mean | sd | p0 | p25 | p50 | p75 | p100 | hist |
|:---|---:|---:|---:|---:|---:|---:|---:|---:|---:|:---|
| size | 0 | 1 | 1142745.16 | 1532140.24 | 355 | 14701 | 260378 | 2367895 | 5677952 | ▇▁▂▁▁ |
| ip_id | 0 | 1 | 10464.38 | 18484.29 | 1 | 328 | 3091 | 11949 | 209747 | ▇▁▁▁▁ |

**Variable type: POSIXct**

| skim_variable | n_missing | complete_rate | min | max | median | n_unique |
|:---|---:|---:|:---|:---|:---|---:|
| date_time | 0 | 1 | 2020-11-23 09:00:41 | 2025-01-25 23:33:51 | 2023-04-24 15:51:04 | 77926 |

**Variable type: Timespan**

| skim_variable | n_missing | complete_rate | min | max | median | n_unique |
|:--------------|----------:|--------------:|----:|----:|-------:|---------:|
| time          |         0 |             1 |   0 |  59 |     29 |       60 |

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
    ## -155.89  -35.21   -9.62   27.28  804.39 
    ## 
    ## Coefficients:
    ##                                                      Estimate Std. Error
    ## (Intercept)                                        -1.917e+02  7.796e+01
    ## date                                                1.145e-02  4.133e-03
    ## lag(value, 1)                                       1.192e-01  2.524e-02
    ## lag(value, 7)                                       9.674e-02  2.624e-02
    ## lag(value, 14)                                      1.120e-01  2.627e-02
    ## lag(value, 21)                                      5.101e-02  2.637e-02
    ## lag(value, 28)                                      6.662e-02  2.631e-02
    ## lag(value, 35)                                      6.841e-02  2.642e-02
    ## lag(value, 42)                                      4.965e-02  2.651e-02
    ## lag(value, 49)                                      9.292e-02  2.635e-02
    ## month(date, label = TRUE).L                        -1.159e+01  5.316e+00
    ## month(date, label = TRUE).Q                         2.473e+00  5.186e+00
    ## month(date, label = TRUE).C                        -1.164e+01  5.265e+00
    ## month(date, label = TRUE)^4                        -7.169e+00  5.293e+00
    ## month(date, label = TRUE)^5                        -1.308e+01  5.295e+00
    ## month(date, label = TRUE)^6                        -6.128e-01  5.379e+00
    ## month(date, label = TRUE)^7                        -9.149e+00  5.284e+00
    ## month(date, label = TRUE)^8                        -2.105e+00  5.281e+00
    ## month(date, label = TRUE)^9                         4.191e+00  5.273e+00
    ## month(date, label = TRUE)^10                        5.055e+00  5.272e+00
    ## month(date, label = TRUE)^11                       -6.112e+00  5.286e+00
    ## fourier_vec(date, type = "sin", K = 1, period = 7) -1.172e+01  2.418e+00
    ## fourier_vec(date, type = "cos", K = 1, period = 7)  7.225e+00  2.539e+00
    ##                                                    t value Pr(>|t|)    
    ## (Intercept)                                         -2.459 0.014050 *  
    ## date                                                 2.772 0.005650 ** 
    ## lag(value, 1)                                        4.723 2.55e-06 ***
    ## lag(value, 7)                                        3.687 0.000235 ***
    ## lag(value, 14)                                       4.265 2.13e-05 ***
    ## lag(value, 21)                                       1.934 0.053272 .  
    ## lag(value, 28)                                       2.532 0.011447 *  
    ## lag(value, 35)                                       2.589 0.009710 ** 
    ## lag(value, 42)                                       1.873 0.061260 .  
    ## lag(value, 49)                                       3.526 0.000435 ***
    ## month(date, label = TRUE).L                         -2.180 0.029443 *  
    ## month(date, label = TRUE).Q                          0.477 0.633581    
    ## month(date, label = TRUE).C                         -2.211 0.027177 *  
    ## month(date, label = TRUE)^4                         -1.354 0.175836    
    ## month(date, label = TRUE)^5                         -2.471 0.013591 *  
    ## month(date, label = TRUE)^6                         -0.114 0.909312    
    ## month(date, label = TRUE)^7                         -1.732 0.083573 .  
    ## month(date, label = TRUE)^8                         -0.399 0.690289    
    ## month(date, label = TRUE)^9                          0.795 0.426866    
    ## month(date, label = TRUE)^10                         0.959 0.337860    
    ## month(date, label = TRUE)^11                        -1.156 0.247736    
    ## fourier_vec(date, type = "sin", K = 1, period = 7)  -4.850 1.37e-06 ***
    ## fourier_vec(date, type = "cos", K = 1, period = 7)   2.845 0.004501 ** 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 58 on 1453 degrees of freedom
    ##   (49 observations deleted due to missingness)
    ## Multiple R-squared:  0.2618, Adjusted R-squared:  0.2506 
    ## F-statistic: 23.43 on 22 and 1453 DF,  p-value: < 2.2e-16

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
    ## 1 healthyR.data <tibble [1,490 × 2]> <tibble [28 × 2]> <split [1462|28]>
    ## 2 healthyR      <tibble [1,483 × 2]> <tibble [28 × 2]> <split [1455|28]>
    ## 3 <NA>          <tibble [28 × 2]>    <tibble [28 × 2]> <split [0|28]>   
    ## 4 healthyR.ts   <tibble [1,429 × 2]> <tibble [28 × 2]> <split [1401|28]>
    ## 5 healthyverse  <tibble [1,400 × 2]> <tibble [28 × 2]> <split [1372|28]>
    ## 6 healthyR.ai   <tibble [1,226 × 2]> <tibble [28 × 2]> <split [1198|28]>
    ## 7 TidyDensity   <tibble [1,080 × 2]> <tibble [28 × 2]> <split [1052|28]>
    ## 8 tidyAML       <tibble [696 × 2]>   <tibble [28 × 2]> <split [668|28]> 
    ## 9 RandomWalker  <tibble [130 × 2]>   <tibble [28 × 2]> <split [102|28]>

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
| healthyR.data | 1 | ARIMA | Test | 1.0282506 | 205.37098 | 0.7425254 | 162.8241 | 1.2020598 | 0.0230914 |
| healthyR.data | 2 | LM | Test | 1.0693617 | 239.44193 | 0.7722127 | 161.3861 | 1.2284955 | 0.0706771 |
| healthyR.data | 3 | EARTH | Test | 1.2188516 | 338.96012 | 0.8801631 | 162.4122 | 1.3551609 | 0.0706771 |
| healthyR.data | 4 | NNAR | Test | 0.9006851 | 103.49794 | 0.6504071 | 173.0294 | 1.1052860 | 0.0167396 |
| healthyR | 1 | ARIMA | Test | 0.7812029 | 106.16096 | 0.6995501 | 163.4230 | 0.9702118 | 0.0256584 |
| healthyR | 2 | LM | Test | 0.7822876 | 102.70348 | 0.7005215 | 188.4015 | 0.9641112 | 0.0134398 |
| healthyR | 3 | EARTH | Test | 0.7880538 | 107.60369 | 0.7056850 | 173.3182 | 0.9753774 | 0.0134398 |
| healthyR | 4 | NNAR | Test | 0.7708118 | 125.92072 | 0.6902452 | 167.3134 | 0.9675020 | 0.0103574 |
| healthyR.ts | 1 | ARIMA | Test | 0.7779449 | 262.42071 | 0.6044571 | 128.5774 | 1.0296737 | 0.0048018 |
| healthyR.ts | 2 | LM | Test | 0.8020113 | 304.10097 | 0.6231565 | 128.3465 | 1.0359872 | 0.0066236 |
| healthyR.ts | 3 | EARTH | Test | 0.8066355 | 311.76312 | 0.6267495 | 128.3134 | 1.0376683 | 0.0066236 |
| healthyR.ts | 4 | NNAR | Test | 0.8046280 | 112.93019 | 0.6251897 | 181.0140 | 1.1073254 | 0.0001657 |
| healthyverse | 1 | ARIMA | Test | 0.7187648 | 226.08040 | 0.6726478 | 110.4429 | 0.8683026 | 0.0027857 |
| healthyverse | 2 | LM | Test | 0.7557580 | 296.43672 | 0.7072675 | 105.7689 | 0.8981852 | 0.0000076 |
| healthyverse | 3 | EARTH | Test | 0.7997978 | 337.68952 | 0.7484816 | 106.3142 | 0.9386200 | 0.0000076 |
| healthyverse | 4 | NNAR | Test | 0.7304655 | 175.92373 | 0.6835978 | 123.7005 | 0.8828904 | 0.0003536 |
| healthyR.ai | 1 | ARIMA | Test | 0.7255785 | 108.16999 | 0.6785050 | 179.0807 | 0.9215653 | 0.0005323 |
| healthyR.ai | 2 | LM | Test | 0.7445464 | 144.82405 | 0.6962423 | 162.3781 | 0.9326298 | 0.0210745 |
| healthyR.ai | 3 | EARTH | Test | 0.7630066 | 186.45398 | 0.7135049 | 145.0084 | 0.9729754 | 0.0210745 |
| healthyR.ai | 4 | NNAR | Test | 0.7444619 | 121.68906 | 0.6961633 | 169.0453 | 0.9463371 | 0.0012887 |
| TidyDensity | 1 | ARIMA | Test | 0.7052434 | 111.38673 | 0.6967067 | 112.7804 | 0.8947211 | 0.0256181 |
| TidyDensity | 2 | LM | Test | 0.7429211 | 160.86799 | 0.7339284 | 105.3885 | 0.9202598 | 0.0041688 |
| TidyDensity | 3 | EARTH | Test | 0.6825435 | 121.48458 | 0.6742816 | 107.0712 | 0.8667051 | 0.0041688 |
| TidyDensity | 4 | NNAR | Test | 0.7685117 | 104.53263 | 0.7592092 | 149.7797 | 0.9455513 | 0.0611465 |
| tidyAML | 1 | ARIMA | Test | 0.8908528 | 289.09836 | 0.7920885 | 100.6181 | 1.0778746 | 0.0492513 |
| tidyAML | 2 | LM | Test | 0.9230088 | 212.44550 | 0.8206795 | 114.0074 | 1.0975132 | 0.0699574 |
| tidyAML | 3 | EARTH | Test | 0.9540916 | 319.36187 | 0.8483163 | 102.3357 | 1.1432907 | 0.0699574 |
| tidyAML | 4 | NNAR | Test | 0.9281383 | 224.91578 | 0.8252403 | 108.9675 | 1.1134113 | 0.0144224 |
| RandomWalker | 1 | ARIMA | Test | 1.3756711 | 161.59626 | 0.6401827 | 143.0262 | 1.6068161 | 0.0021818 |
| RandomWalker | 2 | LM | Test | 1.2709732 | 99.43268 | 0.5914605 | 167.3332 | 1.4654069 | 0.0107028 |
| RandomWalker | 3 | EARTH | Test | 1.2736959 | 101.20745 | 0.5927275 | 165.7513 | 1.4690998 | NA |
| RandomWalker | 4 | NNAR | Test | 1.3994247 | 152.77808 | 0.6512367 | 159.8397 | 1.6689093 | 0.0081076 |

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
    ## 1 healthyR.da…         4 NNAR        Test  0.901 103.  0.650  173. 1.11  1.67e-2
    ## 2 healthyR             2 LM          Test  0.782 103.  0.701  188. 0.964 1.34e-2
    ## 3 healthyR.ts          1 ARIMA       Test  0.778 262.  0.604  129. 1.03  4.80e-3
    ## 4 healthyverse         1 ARIMA       Test  0.719 226.  0.673  110. 0.868 2.79e-3
    ## 5 healthyR.ai          1 ARIMA       Test  0.726 108.  0.679  179. 0.922 5.32e-4
    ## 6 TidyDensity          3 EARTH       Test  0.683 121.  0.674  107. 0.867 4.17e-3
    ## 7 tidyAML              1 ARIMA       Test  0.891 289.  0.792  101. 1.08  4.93e-2
    ## 8 RandomWalker         2 LM          Test  1.27   99.4 0.591  167. 1.47  1.07e-2

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
    ## 1 healthyR.data <tibble>     <tibble>     <split [1462|28]> <mdl_tm_t [1 × 5]>
    ## 2 healthyR      <tibble>     <tibble>     <split [1455|28]> <mdl_tm_t [1 × 5]>
    ## 3 healthyR.ts   <tibble>     <tibble>     <split [1401|28]> <mdl_tm_t [1 × 5]>
    ## 4 healthyverse  <tibble>     <tibble>     <split [1372|28]> <mdl_tm_t [1 × 5]>
    ## 5 healthyR.ai   <tibble>     <tibble>     <split [1198|28]> <mdl_tm_t [1 × 5]>
    ## 6 TidyDensity   <tibble>     <tibble>     <split [1052|28]> <mdl_tm_t [1 × 5]>
    ## 7 tidyAML       <tibble>     <tibble>     <split [668|28]>  <mdl_tm_t [1 × 5]>
    ## 8 RandomWalker  <tibble>     <tibble>     <split [102|28]>  <mdl_tm_t [1 × 5]>

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
