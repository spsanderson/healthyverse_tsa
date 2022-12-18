Time Series Analysis and Nested Modeling of the Healthyverse Packages
================
Steven P. Sanderson II, MPH - Data Scientist/IT Manager
17 December, 2022

This analysis follows a *Nested Modeltime Workflow*.

## Get Data

``` r
glimpse(downloads_tbl)
```

    ## Rows: 53,146
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

The last day in the data set is 2022-12-15 22:23:14, the file was
birthed on: 2022-07-02 23:58:17, and at report knit time is -3978.42
hours old. Happy analyzing!

Now that we have our data lets take a look at it using the `skimr`
package.

``` r
skim(downloads_tbl)
```

|                                                  |               |
|:-------------------------------------------------|:--------------|
| Name                                             | downloads_tbl |
| Number of rows                                   | 53146         |
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
| r_version     |     36495 |          0.31 |   5 |   5 |     0 |       35 |          0 |
| r_arch        |     36495 |          0.31 |   3 |   7 |     0 |        5 |          0 |
| r_os          |     36495 |          0.31 |   7 |  15 |     0 |       11 |          0 |
| package       |         0 |          1.00 |   8 |  13 |     0 |        6 |          0 |
| version       |         0 |          1.00 |   5 |   6 |     0 |       35 |          0 |
| country       |      4244 |          0.92 |   2 |   2 |     0 |      122 |          0 |

**Variable type: Date**

| skim_variable | n_missing | complete_rate | min        | max        | median     | n_unique |
|:--------------|----------:|--------------:|:-----------|:-----------|:-----------|---------:|
| date          |         0 |             1 | 2020-11-23 | 2022-12-15 | 2022-01-11 |      753 |

**Variable type: numeric**

| skim_variable | n_missing | complete_rate |       mean |         sd |  p0 |   p25 |    p50 |     p75 |    p100 | hist  |
|:--------------|----------:|--------------:|-----------:|-----------:|----:|------:|-------:|--------:|--------:|:------|
| size          |         0 |             1 | 1357827.39 | 1713473.92 | 357 | 24837 | 312990 | 2731398 | 5677952 | ▇▁▂▂▁ |
| ip_id         |         0 |             1 |    8977.48 |   17139.85 |   1 |   122 |   2374 |    9384 |  143633 | ▇▁▁▁▁ |

**Variable type: POSIXct**

| skim_variable | n_missing | complete_rate | min                 | max                 | median              | n_unique |
|:--------------|----------:|--------------:|:--------------------|:--------------------|:--------------------|---------:|
| date_time     |         0 |             1 | 2020-11-23 09:00:41 | 2022-12-15 22:23:14 | 2022-01-11 17:27:23 |    31416 |

**Variable type: Timespan**

| skim_variable | n_missing | complete_rate | min | max | median | n_unique |
|:--------------|----------:|--------------:|----:|----:|-------:|---------:|
| time          |         0 |             1 |   0 |  59 |     31 |       60 |

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

    ## # A tibble: 6 × 4
    ##   package       .actual_data       .future_data      .splits         
    ##   <fct>         <list>             <list>            <list>          
    ## 1 healthyR      <tibble [548 × 2]> <tibble [28 × 2]> <split [520|28]>
    ## 2 healthyR.data <tibble [543 × 2]> <tibble [28 × 2]> <split [515|28]>
    ## 3 healthyR.ts   <tibble [546 × 2]> <tibble [28 × 2]> <split [518|28]>
    ## 4 healthyverse  <tibble [548 × 2]> <tibble [28 × 2]> <split [520|28]>
    ## 5 healthyR.ai   <tibble [481 × 2]> <tibble [28 × 2]> <split [453|28]>
    ## 6 TidyDensity   <tibble [329 × 2]> <tibble [28 × 2]> <split [301|28]>

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
```

    ## Recipe
    ## 
    ## Inputs:
    ## 
    ##       role #variables
    ##    outcome          1
    ##  predictor          1

``` r
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

model_spec_glm <- linear_reg(
  penalty = 1,
  mixture = 0.5
) %>%
  set_engine("glmnet")

wflw_glm <- workflow() %>%
  add_recipe(recipe = recipe_date) %>%
  add_model(model_spec_glm)

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
  wflw_glm,
  wflw_lm,
  wflw_mars,
  wflw_nnetar
)
```

``` r
nested_modeltime_tbl
```

    ## # Nested Modeltime Table
    ##   # A tibble: 6 × 5
    ##   package       .actual_data       .future_data      .splits          .modelti…¹
    ##   <fct>         <list>             <list>            <list>           <list>    
    ## 1 healthyR      <tibble [548 × 2]> <tibble [28 × 2]> <split [520|28]> <mdl_tm_t>
    ## 2 healthyR.data <tibble [543 × 2]> <tibble [28 × 2]> <split [515|28]> <mdl_tm_t>
    ## 3 healthyR.ts   <tibble [546 × 2]> <tibble [28 × 2]> <split [518|28]> <mdl_tm_t>
    ## 4 healthyverse  <tibble [548 × 2]> <tibble [28 × 2]> <split [520|28]> <mdl_tm_t>
    ## 5 healthyR.ai   <tibble [481 × 2]> <tibble [28 × 2]> <split [453|28]> <mdl_tm_t>
    ## 6 TidyDensity   <tibble [329 × 2]> <tibble [28 × 2]> <split [301|28]> <mdl_tm_t>
    ## # … with abbreviated variable name ¹​.modeltime_tables

### Model Accuracy

``` r
nested_modeltime_tbl %>%
  extract_nested_test_accuracy() %>%
  knitr::kable()
```

| package       | .model_id | .model_desc | .type |       mae |      mape |      mase |     smape |      rmse |       rsq |
|:--------------|----------:|:------------|:------|----------:|----------:|----------:|----------:|----------:|----------:|
| healthyR      |         1 | ARIMA       | Test  | 0.7809769 |  75.06242 | 1.0291759 | 108.07306 | 1.0356801 | 0.5552649 |
| healthyR      |         2 | NULL        | NA    |        NA |        NA |        NA |        NA |        NA |        NA |
| healthyR      |         3 | LM          | Test  | 0.8616835 |  83.31639 | 1.1355315 | 141.86952 | 1.0686854 | 0.0243105 |
| healthyR      |         4 | EARTH       | Test  | 0.7575529 |  72.71648 | 0.9983076 | 106.00434 | 0.9921834 | 0.0243105 |
| healthyR      |         5 | NNAR        | Test  | 0.7699215 |  81.48590 | 1.0146070 | 104.75652 | 1.0580654 | 0.0357557 |
| healthyR.data |         1 | ARIMA       | Test  | 0.7266489 | 112.39114 | 0.8796261 |  97.68773 | 0.9083941 | 0.0179691 |
| healthyR.data |         2 | NULL        | NA    |        NA |        NA |        NA |        NA |        NA |        NA |
| healthyR.data |         3 | LM          | Test  | 0.6287951 | 144.50726 | 0.7611717 |  78.30974 | 0.7870927 | 0.0239860 |
| healthyR.data |         4 | EARTH       | Test  | 0.6443840 | 136.57791 | 0.7800424 |  81.27152 | 0.8096828 | 0.0239860 |
| healthyR.data |         5 | NNAR        | Test  | 1.0191354 | 146.82186 | 1.2336882 | 151.37603 | 1.2366047 | 0.1216187 |
| healthyR.ts   |         1 | ARIMA       | Test  | 0.8072260 | 211.70705 | 0.9064157 | 164.94906 | 1.0673042 | 0.0012909 |
| healthyR.ts   |         2 | NULL        | NA    |        NA |        NA |        NA |        NA |        NA |        NA |
| healthyR.ts   |         3 | LM          | Test  | 0.7644295 | 163.43684 | 0.8583605 | 167.35844 | 1.0380121 | 0.1623928 |
| healthyR.ts   |         4 | EARTH       | Test  | 0.8208870 | 226.91029 | 0.9217553 | 156.84072 | 1.0733201 | 0.1623928 |
| healthyR.ts   |         5 | NNAR        | Test  | 0.7513843 | 138.29047 | 0.8437123 | 170.62273 | 1.0447976 | 0.0077154 |
| healthyverse  |         1 | ARIMA       | Test  | 0.5721574 | 101.80854 | 1.0009235 |  68.69201 | 0.7621719 | 0.1923115 |
| healthyverse  |         2 | NULL        | NA    |        NA |        NA |        NA |        NA |        NA |        NA |
| healthyverse  |         3 | LM          | Test  | 0.5513196 | 102.25446 | 0.9644702 |  66.82659 | 0.7336088 | 0.0061435 |
| healthyverse  |         4 | EARTH       | Test  | 0.5162925 | 120.21888 | 0.9031943 |  61.11113 | 0.6759354 | 0.0061435 |
| healthyverse  |         5 | NNAR        | Test  | 0.6953915 | 130.38500 | 1.2165073 | 121.47632 | 0.8679309 | 0.0407260 |
| healthyR.ai   |         1 | ARIMA       | Test  | 0.6315590 | 133.69101 | 0.8977552 | 173.58887 | 0.8825500 | 0.0181298 |
| healthyR.ai   |         2 | NULL        | NA    |        NA |        NA |        NA |        NA |        NA |        NA |
| healthyR.ai   |         3 | LM          | Test  | 0.5786664 |  95.14954 | 0.8225687 | 153.04128 | 0.8455458 | 0.2630640 |
| healthyR.ai   |         4 | EARTH       | Test  | 0.5883579 |  91.97553 | 0.8363451 | 169.52447 | 0.8480277 | 0.2630640 |
| healthyR.ai   |         5 | NNAR        | Test  | 0.5365321 | 121.10691 | 0.7626753 | 135.81342 | 0.7447130 | 0.3083613 |
| TidyDensity   |         1 | ARIMA       | Test  | 0.6505742 | 109.63643 | 0.7899338 | 143.24705 | 0.9075304 | 0.0002656 |
| TidyDensity   |         2 | NULL        | NA    |        NA |        NA |        NA |        NA |        NA |        NA |
| TidyDensity   |         3 | LM          | Test  | 0.6217659 |  95.00930 | 0.7549545 | 143.43954 | 0.8963257 | 0.2141025 |
| TidyDensity   |         4 | EARTH       | Test  | 0.6184298 |  94.39641 | 0.7509038 | 166.57923 | 0.8959840 | 0.2141025 |
| TidyDensity   |         5 | NNAR        | Test  | 0.7238925 | 144.96267 | 0.8789576 | 165.68133 | 1.0434564 | 0.1514620 |

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
    ##   # A tibble: 6 × 10
    ##   package       .model_id .model_d…¹ .type   mae  mape  mase smape  rmse     rsq
    ##   <fct>             <int> <chr>      <chr> <dbl> <dbl> <dbl> <dbl> <dbl>   <dbl>
    ## 1 healthyR              4 EARTH      Test  0.758  72.7 0.998 106.  0.992 0.0243 
    ## 2 healthyR.data         3 LM         Test  0.629 145.  0.761  78.3 0.787 0.0240 
    ## 3 healthyR.ts           3 LM         Test  0.764 163.  0.858 167.  1.04  0.162  
    ## 4 healthyverse          4 EARTH      Test  0.516 120.  0.903  61.1 0.676 0.00614
    ## 5 healthyR.ai           5 NNAR       Test  0.537 121.  0.763 136.  0.745 0.308  
    ## 6 TidyDensity           4 EARTH      Test  0.618  94.4 0.751 167.  0.896 0.214  
    ## # … with abbreviated variable name ¹​.model_desc

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
    ##   # A tibble: 6 × 5
    ##   package       .actual_data       .future_data      .splits          .modelti…¹
    ##   <fct>         <list>             <list>            <list>           <list>    
    ## 1 healthyR      <tibble [548 × 2]> <tibble [28 × 2]> <split [520|28]> <mdl_tm_t>
    ## 2 healthyR.data <tibble [543 × 2]> <tibble [28 × 2]> <split [515|28]> <mdl_tm_t>
    ## 3 healthyR.ts   <tibble [546 × 2]> <tibble [28 × 2]> <split [518|28]> <mdl_tm_t>
    ## 4 healthyverse  <tibble [548 × 2]> <tibble [28 × 2]> <split [520|28]> <mdl_tm_t>
    ## 5 healthyR.ai   <tibble [481 × 2]> <tibble [28 × 2]> <split [453|28]> <mdl_tm_t>
    ## 6 TidyDensity   <tibble [329 × 2]> <tibble [28 × 2]> <split [301|28]> <mdl_tm_t>
    ## # … with abbreviated variable name ¹​.modeltime_tables

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
