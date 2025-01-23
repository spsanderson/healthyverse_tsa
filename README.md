Time Series Analysis and Nested Modeling of the Healthyverse Packages
================
Steven P. Sanderson II, MPH - Date:
23 January, 2025

This analysis follows a *Nested Modeltime Workflow*.

## Get Data

``` r
glimpse(downloads_tbl)
```

    ## Rows: 128,076
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

The last day in the data set is 2025-01-21 23:12:05, the file was
birthed on: 2024-08-07 07:35:44, and at report knit time is -4019.61
hours old. Happy analyzing!

Now that we have our data lets take a look at it using the `skimr`
package.

``` r
skim(downloads_tbl)
```

|                                                  |               |
|:-------------------------------------------------|:--------------|
| Name                                             | downloads_tbl |
| Number of rows                                   | 128076        |
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
| r_version     |     91231 |          0.29 |   5 |   5 |     0 |       45 |          0 |
| r_arch        |     91231 |          0.29 |   3 |   7 |     0 |        5 |          0 |
| r_os          |     91231 |          0.29 |   7 |  15 |     0 |       21 |          0 |
| package       |         0 |          1.00 |   7 |  13 |     0 |        8 |          0 |
| version       |         0 |          1.00 |   5 |  17 |     0 |       60 |          0 |
| country       |     10865 |          0.92 |   2 |   2 |     0 |      160 |          0 |

**Variable type: Date**

| skim_variable | n_missing | complete_rate | min | max | median | n_unique |
|:---|---:|---:|:---|:---|:---|---:|
| date | 0 | 1 | 2020-11-23 | 2025-01-21 | 2023-04-22 | 1521 |

**Variable type: numeric**

| skim_variable | n_missing | complete_rate | mean | sd | p0 | p25 | p50 | p75 | p100 | hist |
|:---|---:|---:|---:|---:|---:|---:|---:|---:|---:|:---|
| size | 0 | 1 | 1144057.21 | 1533080.87 | 355 | 14701 | 260384 | 2367915 | 5677952 | ▇▁▂▁▁ |
| ip_id | 0 | 1 | 10418.89 | 18259.34 | 1 | 336 | 3098 | 11952 | 209747 | ▇▁▁▁▁ |

**Variable type: POSIXct**

| skim_variable | n_missing | complete_rate | min | max | median | n_unique |
|:---|---:|---:|:---|:---|:---|---:|
| date_time | 0 | 1 | 2020-11-23 09:00:41 | 2025-01-21 23:12:05 | 2023-04-22 01:14:43 | 77595 |

**Variable type: Timespan**

| skim_variable | n_missing | complete_rate | min | max | median | n_unique |
|:--------------|----------:|--------------:|----:|----:|-------:|---------:|
| time          |         0 |             1 |   0 |  59 |   11.5 |       60 |

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
    ## -154.95  -35.07   -9.59   27.23  805.10 
    ## 
    ## Coefficients:
    ##                                                      Estimate Std. Error
    ## (Intercept)                                        -1.919e+02  7.799e+01
    ## date                                                1.147e-02  4.133e-03
    ## lag(value, 1)                                       1.232e-01  2.529e-02
    ## lag(value, 7)                                       9.315e-02  2.624e-02
    ## lag(value, 14)                                      1.090e-01  2.625e-02
    ## lag(value, 21)                                      4.889e-02  2.645e-02
    ## lag(value, 28)                                      6.459e-02  2.629e-02
    ## lag(value, 35)                                      7.237e-02  2.643e-02
    ## lag(value, 42)                                      4.902e-02  2.648e-02
    ## lag(value, 49)                                      9.441e-02  2.635e-02
    ## month(date, label = TRUE).L                        -1.150e+01  5.328e+00
    ## month(date, label = TRUE).Q                         2.381e+00  5.192e+00
    ## month(date, label = TRUE).C                        -1.153e+01  5.271e+00
    ## month(date, label = TRUE)^4                        -7.348e+00  5.294e+00
    ## month(date, label = TRUE)^5                        -1.304e+01  5.289e+00
    ## month(date, label = TRUE)^6                        -7.932e-01  5.372e+00
    ## month(date, label = TRUE)^7                        -9.127e+00  5.275e+00
    ## month(date, label = TRUE)^8                        -2.174e+00  5.272e+00
    ## month(date, label = TRUE)^9                         4.155e+00  5.264e+00
    ## month(date, label = TRUE)^10                        5.136e+00  5.263e+00
    ## month(date, label = TRUE)^11                       -6.202e+00  5.277e+00
    ## fourier_vec(date, type = "sin", K = 1, period = 7) -1.170e+01  2.416e+00
    ## fourier_vec(date, type = "cos", K = 1, period = 7)  7.202e+00  2.536e+00
    ##                                                    t value Pr(>|t|)    
    ## (Intercept)                                         -2.461 0.013982 *  
    ## date                                                 2.776 0.005581 ** 
    ## lag(value, 1)                                        4.872 1.23e-06 ***
    ## lag(value, 7)                                        3.550 0.000397 ***
    ## lag(value, 14)                                       4.151 3.50e-05 ***
    ## lag(value, 21)                                       1.848 0.064775 .  
    ## lag(value, 28)                                       2.457 0.014123 *  
    ## lag(value, 35)                                       2.738 0.006250 ** 
    ## lag(value, 42)                                       1.851 0.064385 .  
    ## lag(value, 49)                                       3.584 0.000350 ***
    ## month(date, label = TRUE).L                         -2.158 0.031085 *  
    ## month(date, label = TRUE).Q                          0.458 0.646670    
    ## month(date, label = TRUE).C                         -2.187 0.028896 *  
    ## month(date, label = TRUE)^4                         -1.388 0.165362    
    ## month(date, label = TRUE)^5                         -2.466 0.013771 *  
    ## month(date, label = TRUE)^6                         -0.148 0.882640    
    ## month(date, label = TRUE)^7                         -1.730 0.083799 .  
    ## month(date, label = TRUE)^8                         -0.412 0.680109    
    ## month(date, label = TRUE)^9                          0.789 0.429999    
    ## month(date, label = TRUE)^10                         0.976 0.329341    
    ## month(date, label = TRUE)^11                        -1.175 0.240027    
    ## fourier_vec(date, type = "sin", K = 1, period = 7)  -4.844 1.41e-06 ***
    ## fourier_vec(date, type = "cos", K = 1, period = 7)   2.839 0.004582 ** 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 57.89 on 1449 degrees of freedom
    ##   (49 observations deleted due to missingness)
    ## Multiple R-squared:  0.2598, Adjusted R-squared:  0.2486 
    ## F-statistic: 23.12 on 22 and 1449 DF,  p-value: < 2.2e-16

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
    ## 1 healthyR.data <tibble [1,486 × 2]> <tibble [28 × 2]> <split [1458|28]>
    ## 2 healthyR      <tibble [1,479 × 2]> <tibble [28 × 2]> <split [1451|28]>
    ## 3 <NA>          <tibble [28 × 2]>    <tibble [28 × 2]> <split [0|28]>   
    ## 4 healthyR.ts   <tibble [1,425 × 2]> <tibble [28 × 2]> <split [1397|28]>
    ## 5 healthyverse  <tibble [1,396 × 2]> <tibble [28 × 2]> <split [1368|28]>
    ## 6 healthyR.ai   <tibble [1,222 × 2]> <tibble [28 × 2]> <split [1194|28]>
    ## 7 TidyDensity   <tibble [1,076 × 2]> <tibble [28 × 2]> <split [1048|28]>
    ## 8 tidyAML       <tibble [692 × 2]>   <tibble [28 × 2]> <split [664|28]> 
    ## 9 RandomWalker  <tibble [126 × 2]>   <tibble [28 × 2]> <split [98|28]>

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
| healthyR.data | 1 | ARIMA | Test | 0.9998221 | 209.7228 | 0.7395330 | 163.3305 | 1.1784560 | 0.0075944 |
| healthyR.data | 2 | LM | Test | 1.0554359 | 244.9347 | 0.7806686 | 163.4745 | 1.2095562 | 0.1225406 |
| healthyR.data | 3 | EARTH | Test | 1.1862290 | 348.0188 | 0.8774116 | 160.9273 | 1.3271532 | 0.1225406 |
| healthyR.data | 4 | NNAR | Test | 0.8832203 | 106.3746 | 0.6532868 | 160.8668 | 1.1100405 | 0.0073984 |
| healthyR | 1 | ARIMA | Test | 0.7484664 | 119.4111 | 0.6742318 | 160.6154 | 0.9215706 | 0.0064235 |
| healthyR | 2 | LM | Test | 0.7527555 | 100.8770 | 0.6780954 | 184.3140 | 0.9307766 | 0.0169834 |
| healthyR | 3 | EARTH | Test | 0.7580914 | 105.5665 | 0.6829021 | 170.7619 | 0.9389463 | 0.0169834 |
| healthyR | 4 | NNAR | Test | 0.7843894 | 136.5760 | 0.7065919 | 160.9268 | 0.9663552 | 0.0049884 |
| healthyR.ts | 1 | ARIMA | Test | 0.8238020 | 265.7932 | 0.6302789 | 130.0318 | 1.0714964 | 0.0000047 |
| healthyR.ts | 2 | LM | Test | 0.8468026 | 296.7081 | 0.6478764 | 130.5905 | 1.0801162 | 0.0001045 |
| healthyR.ts | 3 | EARTH | Test | 0.8523639 | 304.1584 | 0.6521312 | 130.6809 | 1.0825961 | 0.0001045 |
| healthyR.ts | 4 | NNAR | Test | 0.8127557 | 105.1942 | 0.6218276 | 173.0993 | 1.1064412 | 0.0003654 |
| healthyverse | 1 | ARIMA | Test | 0.7009781 | 226.0185 | 0.6581096 | 112.8827 | 0.8369806 | 0.0156648 |
| healthyverse | 2 | LM | Test | 0.7459698 | 297.7596 | 0.7003498 | 107.2080 | 0.8756680 | 0.0011811 |
| healthyverse | 3 | EARTH | Test | 0.8019103 | 362.8251 | 0.7528692 | 106.6519 | 0.9368779 | 0.0011811 |
| healthyverse | 4 | NNAR | Test | 0.6846217 | 174.6825 | 0.6427535 | 118.8378 | 0.8399001 | 0.0601456 |
| healthyR.ai | 1 | ARIMA | Test | 0.6934546 | 110.6865 | 0.6903161 | 186.6996 | 0.8210692 | 0.0089604 |
| healthyR.ai | 2 | LM | Test | 0.6925953 | 138.0644 | 0.6894607 | 156.8864 | 0.8419403 | 0.0268803 |
| healthyR.ai | 3 | EARTH | Test | 0.7133896 | 190.5809 | 0.7101609 | 134.5724 | 0.8944417 | 0.0268803 |
| healthyR.ai | 4 | NNAR | Test | 0.7034547 | 129.7318 | 0.7002710 | 168.4946 | 0.8489097 | 0.0005680 |
| TidyDensity | 1 | ARIMA | Test | 0.7307874 | 147.7952 | 0.6982607 | 114.0995 | 0.8942686 | 0.0247398 |
| TidyDensity | 2 | LM | Test | 0.7832927 | 170.9055 | 0.7484290 | 110.7805 | 0.9580302 | 0.0089960 |
| TidyDensity | 3 | EARTH | Test | 0.7230152 | 132.1932 | 0.6908344 | 113.5573 | 0.8936941 | 0.0089960 |
| TidyDensity | 4 | NNAR | Test | 0.7577503 | 117.8949 | 0.7240235 | 147.9198 | 0.9076880 | 0.0001353 |
| tidyAML | 1 | ARIMA | Test | 0.9238060 | 240.2848 | 0.7387282 | 109.2182 | 1.0640733 | 0.1080331 |
| tidyAML | 2 | LM | Test | 0.9852459 | 214.0157 | 0.7878590 | 120.9275 | 1.1291172 | 0.0766786 |
| tidyAML | 3 | EARTH | Test | 0.9770844 | 310.9099 | 0.7813326 | 106.6200 | 1.1180625 | 0.0766786 |
| tidyAML | 4 | NNAR | Test | 0.9677981 | 244.3986 | 0.7739068 | 114.4355 | 1.1143241 | 0.0000195 |
| RandomWalker | 1 | ARIMA | Test | 1.2124395 | 228.5967 | 0.6028539 | 136.6542 | 1.4288989 | 0.1005877 |
| RandomWalker | 2 | LM | Test | 1.1821308 | 100.0115 | 0.5877837 | 159.5696 | 1.4029840 | 0.0163673 |
| RandomWalker | 3 | EARTH | Test | 1.1825761 | 100.3855 | 0.5880051 | 159.3760 | 1.4035200 | NA |
| RandomWalker | 4 | NNAR | Test | 1.4394080 | 228.7249 | 0.7157080 | 161.4481 | 1.6377284 | 0.0082629 |

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
    ## 1 healthyR.da…         4 NNAR        Test  0.883  106. 0.653  161. 1.11  7.40e-3
    ## 2 healthyR             1 ARIMA       Test  0.748  119. 0.674  161. 0.922 6.42e-3
    ## 3 healthyR.ts          1 ARIMA       Test  0.824  266. 0.630  130. 1.07  4.68e-6
    ## 4 healthyverse         1 ARIMA       Test  0.701  226. 0.658  113. 0.837 1.57e-2
    ## 5 healthyR.ai          1 ARIMA       Test  0.693  111. 0.690  187. 0.821 8.96e-3
    ## 6 TidyDensity          3 EARTH       Test  0.723  132. 0.691  114. 0.894 9.00e-3
    ## 7 tidyAML              1 ARIMA       Test  0.924  240. 0.739  109. 1.06  1.08e-1
    ## 8 RandomWalker         2 LM          Test  1.18   100. 0.588  160. 1.40  1.64e-2

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
    ## 1 healthyR.data <tibble>     <tibble>     <split [1458|28]> <mdl_tm_t [1 × 5]>
    ## 2 healthyR      <tibble>     <tibble>     <split [1451|28]> <mdl_tm_t [1 × 5]>
    ## 3 healthyR.ts   <tibble>     <tibble>     <split [1397|28]> <mdl_tm_t [1 × 5]>
    ## 4 healthyverse  <tibble>     <tibble>     <split [1368|28]> <mdl_tm_t [1 × 5]>
    ## 5 healthyR.ai   <tibble>     <tibble>     <split [1194|28]> <mdl_tm_t [1 × 5]>
    ## 6 TidyDensity   <tibble>     <tibble>     <split [1048|28]> <mdl_tm_t [1 × 5]>
    ## 7 tidyAML       <tibble>     <tibble>     <split [664|28]>  <mdl_tm_t [1 × 5]>
    ## 8 RandomWalker  <tibble>     <tibble>     <split [98|28]>   <mdl_tm_t [1 × 5]>

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
