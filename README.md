Time Series Analysis and Nested Modeling of the Healthyverse Packages
================
Steven P. Sanderson II, MPH - Data Scientist/IT Manager
15 November, 2022

This analysis follows a *Nested Modeltime Workflow*.

## Get Data

``` r
glimpse(downloads_tbl)
```

    ## Rows: 51,051
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

The last day in the data set is 2022-11-13 23:52:03, the file was
birthed on: 2022-07-02 23:58:17, and at report knit time is -3211.9
hours old. Happy analyzing!

Now that we have our data lets take a look at it using the `skimr`
package.

``` r
skim(downloads_tbl)
```

|                                                  |               |
|:-------------------------------------------------|:--------------|
| Name                                             | downloads_tbl |
| Number of rows                                   | 51051         |
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
| r_version     |     34963 |          0.32 |   5 |   5 |     0 |       35 |          0 |
| r_arch        |     34963 |          0.32 |   3 |   7 |     0 |        5 |          0 |
| r_os          |     34963 |          0.32 |   7 |  15 |     0 |       11 |          0 |
| package       |         0 |          1.00 |   8 |  13 |     0 |        6 |          0 |
| version       |         0 |          1.00 |   5 |   5 |     0 |       32 |          0 |
| country       |      4034 |          0.92 |   2 |   2 |     0 |      121 |          0 |

**Variable type: Date**

| skim_variable | n_missing | complete_rate | min        | max        | median     | n_unique |
|:--------------|----------:|--------------:|:-----------|:-----------|:-----------|---------:|
| date          |         0 |             1 | 2020-11-23 | 2022-11-13 | 2021-12-21 |      721 |

**Variable type: numeric**

| skim_variable | n_missing | complete_rate |       mean |         sd |  p0 |   p25 |    p50 |     p75 |    p100 | hist  |
|:--------------|----------:|--------------:|-----------:|-----------:|----:|------:|-------:|--------:|--------:|:------|
| size          |         0 |             1 | 1369119.53 | 1726382.45 | 357 | 20493 | 308030 | 2736839 | 5677952 | ▇▁▂▂▁ |
| ip_id         |         0 |             1 |    8822.39 |   16823.39 |   1 |   142 |   2376 |    9219 |  143633 | ▇▁▁▁▁ |

**Variable type: POSIXct**

| skim_variable | n_missing | complete_rate | min                 | max                 | median              | n_unique |
|:--------------|----------:|--------------:|:--------------------|:--------------------|:--------------------|---------:|
| date_time     |         0 |             1 | 2020-11-23 09:00:41 | 2022-11-13 23:52:03 | 2021-12-21 08:59:55 |    30104 |

**Variable type: Timespan**

| skim_variable | n_missing | complete_rate | min | max |      median | n_unique |
|:--------------|----------:|--------------:|----:|----:|------------:|---------:|
| time          |         0 |             1 |   0 |  59 | 11H 39M 43S |       60 |

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
    ## 1 healthyR      <tibble [549 × 2]> <tibble [28 × 2]> <split [521|28]>
    ## 2 healthyR.data <tibble [544 × 2]> <tibble [28 × 2]> <split [516|28]>
    ## 3 healthyR.ts   <tibble [547 × 2]> <tibble [28 × 2]> <split [519|28]>
    ## 4 healthyverse  <tibble [549 × 2]> <tibble [28 × 2]> <split [521|28]>
    ## 5 healthyR.ai   <tibble [449 × 2]> <tibble [28 × 2]> <split [421|28]>
    ## 6 TidyDensity   <tibble [297 × 2]> <tibble [28 × 2]> <split [269|28]>

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
    ## 1 healthyR      <tibble [549 × 2]> <tibble [28 × 2]> <split [521|28]> <mdl_tm_t>
    ## 2 healthyR.data <tibble [544 × 2]> <tibble [28 × 2]> <split [516|28]> <mdl_tm_t>
    ## 3 healthyR.ts   <tibble [547 × 2]> <tibble [28 × 2]> <split [519|28]> <mdl_tm_t>
    ## 4 healthyverse  <tibble [549 × 2]> <tibble [28 × 2]> <split [521|28]> <mdl_tm_t>
    ## 5 healthyR.ai   <tibble [449 × 2]> <tibble [28 × 2]> <split [421|28]> <mdl_tm_t>
    ## 6 TidyDensity   <tibble [297 × 2]> <tibble [28 × 2]> <split [269|28]> <mdl_tm_t>
    ## # … with abbreviated variable name ¹​.modeltime_tables

### Model Accuracy

``` r
nested_modeltime_tbl %>%
  extract_nested_test_accuracy() %>%
  knitr::kable()
```

| package       | .model_id | .model_desc | .type |       mae |      mape |      mase |    smape |      rmse |       rsq |
|:--------------|----------:|:------------|:------|----------:|----------:|----------:|---------:|----------:|----------:|
| healthyR      |         1 | ARIMA       | Test  | 0.7377514 | 122.17512 | 0.6935887 | 170.3597 | 0.9187693 | 0.0000021 |
| healthyR      |         2 | NULL        | NA    |        NA |        NA |        NA |       NA |        NA |        NA |
| healthyR      |         3 | LM          | Test  | 0.7102984 | 100.98341 | 0.6677791 | 172.2557 | 0.9026775 | 0.0193093 |
| healthyR      |         4 | EARTH       | Test  | 0.7191815 | 131.73539 | 0.6761304 | 159.2848 | 0.8822578 | 0.0193093 |
| healthyR      |         5 | NNAR        | Test  | 0.7287419 | 178.08734 | 0.6851185 | 127.4447 | 0.9425069 | 0.0257201 |
| healthyR.data |         1 | ARIMA       | Test  | 0.9707361 | 140.01956 | 0.6589638 | 137.6179 | 1.0956784 | 0.1233756 |
| healthyR.data |         2 | NULL        | NA    |        NA |        NA |        NA |       NA |        NA |        NA |
| healthyR.data |         3 | LM          | Test  | 0.8823534 | 133.27962 | 0.5989670 | 117.6073 | 1.0207339 | 0.0003685 |
| healthyR.data |         4 | EARTH       | Test  | 0.8810072 | 135.68820 | 0.5980532 | 116.0313 | 1.0226063 | 0.0003685 |
| healthyR.data |         5 | NNAR        | Test  | 1.0806654 | 145.13821 | 0.7335869 | 144.7720 | 1.2498015 | 0.0144850 |
| healthyR.ts   |         1 | ARIMA       | Test  | 0.8017977 | 133.70592 | 0.6519073 | 157.3265 | 0.9346362 | 0.0089507 |
| healthyR.ts   |         2 | NULL        | NA    |        NA |        NA |        NA |       NA |        NA |        NA |
| healthyR.ts   |         3 | LM          | Test  | 0.7967820 | 114.93214 | 0.6478293 | 152.0729 | 0.9365189 | 0.0012841 |
| healthyR.ts   |         4 | EARTH       | Test  | 0.8019739 | 129.93256 | 0.6520506 | 144.1523 | 0.9468903 | 0.0012841 |
| healthyR.ts   |         5 | NNAR        | Test  | 0.8267369 | 137.00785 | 0.6721843 | 180.1234 | 0.9651322 | 0.0063020 |
| healthyverse  |         1 | ARIMA       | Test  | 0.9101631 | 192.03819 | 0.7404242 | 126.9626 | 1.0486227 | 0.0015756 |
| healthyverse  |         2 | NULL        | NA    |        NA |        NA |        NA |       NA |        NA |        NA |
| healthyverse  |         3 | LM          | Test  | 0.8841743 | 167.84666 | 0.7192821 | 130.5699 | 1.0310093 | 0.0001691 |
| healthyverse  |         4 | EARTH       | Test  | 0.9346361 | 207.97784 | 0.7603332 | 123.9385 | 1.0822878 | 0.0001691 |
| healthyverse  |         5 | NNAR        | Test  | 0.8523079 | 136.62009 | 0.6933586 | 155.9305 | 0.9913965 | 0.0643667 |
| healthyR.ai   |         1 | ARIMA       | Test  | 0.7930595 | 101.75887 | 0.6688009 | 154.3263 | 0.9198120 | 0.0005838 |
| healthyR.ai   |         2 | NULL        | NA    |        NA |        NA |        NA |       NA |        NA |        NA |
| healthyR.ai   |         3 | LM          | Test  | 0.7840440 | 108.30824 | 0.6611980 | 168.0367 | 0.8983592 | 0.0158487 |
| healthyR.ai   |         4 | EARTH       | Test  | 0.7737523 |  99.94385 | 0.6525188 | 184.4497 | 0.8869160 | 0.0158487 |
| healthyR.ai   |         5 | NNAR        | Test  | 0.8130425 | 123.12197 | 0.6856529 | 164.6357 | 0.9305230 | 0.0181523 |
| TidyDensity   |         1 | ARIMA       | Test  | 0.8274725 | 114.97160 | 0.6673048 | 191.0489 | 0.9313802 | 0.0046799 |
| TidyDensity   |         2 | NULL        | NA    |        NA |        NA |        NA |       NA |        NA |        NA |
| TidyDensity   |         3 | LM          | Test  | 0.7920662 |  98.81653 | 0.6387518 | 156.7753 | 0.9274879 | 0.0242125 |
| TidyDensity   |         4 | EARTH       | Test  | 0.7920980 |  94.38272 | 0.6387774 | 164.2412 | 0.9223736 | 0.0242125 |
| TidyDensity   |         5 | NNAR        | Test  | 0.8367588 | 136.14370 | 0.6747936 | 148.1410 | 0.9544997 | 0.0107290 |

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
    ## 1 healthyR              4 EARTH      Test  0.719 132.  0.676  159. 0.882 1.93e-2
    ## 2 healthyR.data         3 LM         Test  0.882 133.  0.599  118. 1.02  3.69e-4
    ## 3 healthyR.ts           1 ARIMA      Test  0.802 134.  0.652  157. 0.935 8.95e-3
    ## 4 healthyverse          5 NNAR       Test  0.852 137.  0.693  156. 0.991 6.44e-2
    ## 5 healthyR.ai           4 EARTH      Test  0.774  99.9 0.653  184. 0.887 1.58e-2
    ## 6 TidyDensity           4 EARTH      Test  0.792  94.4 0.639  164. 0.922 2.42e-2
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
    ## 1 healthyR      <tibble [549 × 2]> <tibble [28 × 2]> <split [521|28]> <mdl_tm_t>
    ## 2 healthyR.data <tibble [544 × 2]> <tibble [28 × 2]> <split [516|28]> <mdl_tm_t>
    ## 3 healthyR.ts   <tibble [547 × 2]> <tibble [28 × 2]> <split [519|28]> <mdl_tm_t>
    ## 4 healthyverse  <tibble [549 × 2]> <tibble [28 × 2]> <split [521|28]> <mdl_tm_t>
    ## 5 healthyR.ai   <tibble [449 × 2]> <tibble [28 × 2]> <split [421|28]> <mdl_tm_t>
    ## 6 TidyDensity   <tibble [297 × 2]> <tibble [28 × 2]> <split [269|28]> <mdl_tm_t>
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
