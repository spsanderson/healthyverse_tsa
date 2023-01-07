Time Series Analysis and Nested Modeling of the Healthyverse Packages
================
Steven P. Sanderson II, MPH - Data Scientist/IT Manager
07 January, 2023

This analysis follows a *Nested Modeltime Workflow*.

## Get Data

``` r
glimpse(downloads_tbl)
```

    ## Rows: 54,137
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

The last day in the data set is 2023-01-05 23:17:02, the file was
birthed on: 2022-07-02 23:58:17, and at report knit time is -4483.31
hours old. Happy analyzing!

Now that we have our data lets take a look at it using the `skimr`
package.

``` r
skim(downloads_tbl)
```

|                                                  |               |
|:-------------------------------------------------|:--------------|
| Name                                             | downloads_tbl |
| Number of rows                                   | 54137         |
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
| r_version     |     37255 |          0.31 |   5 |   5 |     0 |       35 |          0 |
| r_arch        |     37255 |          0.31 |   3 |   7 |     0 |        5 |          0 |
| r_os          |     37255 |          0.31 |   7 |  15 |     0 |       11 |          0 |
| package       |         0 |          1.00 |   8 |  13 |     0 |        6 |          0 |
| version       |         0 |          1.00 |   5 |   6 |     0 |       35 |          0 |
| country       |      4326 |          0.92 |   2 |   2 |     0 |      124 |          0 |

**Variable type: Date**

| skim_variable | n_missing | complete_rate | min        | max        | median     | n_unique |
|:--------------|----------:|--------------:|:-----------|:-----------|:-----------|---------:|
| date          |         0 |             1 | 2020-11-23 | 2023-01-05 | 2022-01-19 |      774 |

**Variable type: numeric**

| skim_variable | n_missing | complete_rate |       mean |         sd |  p0 |   p25 |    p50 |     p75 |    p100 | hist  |
|:--------------|----------:|--------------:|-----------:|-----------:|----:|------:|-------:|--------:|--------:|:------|
| size          |         0 |             1 | 1352926.06 | 1707372.60 | 357 | 24837 | 313210 | 2731273 | 5677952 | ▇▁▂▂▁ |
| ip_id         |         0 |             1 |    8971.62 |   17082.73 |   1 |   121 |   2348 |    9434 |  143633 | ▇▁▁▁▁ |

**Variable type: POSIXct**

| skim_variable | n_missing | complete_rate | min                 | max                 | median              | n_unique |
|:--------------|----------:|--------------:|:--------------------|:--------------------|:--------------------|---------:|
| date_time     |         0 |             1 | 2020-11-23 09:00:41 | 2023-01-05 23:17:02 | 2022-01-19 12:32:06 |    32038 |

**Variable type: Timespan**

| skim_variable | n_missing | complete_rate | min | max |      median | n_unique |
|:--------------|----------:|--------------:|----:|----:|------------:|---------:|
| time          |         0 |             1 |   0 |  59 | 11H 38M 15S |       60 |

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
    ## 2 healthyR.data <tibble [544 × 2]> <tibble [28 × 2]> <split [516|28]>
    ## 3 healthyR.ts   <tibble [547 × 2]> <tibble [28 × 2]> <split [519|28]>
    ## 4 healthyverse  <tibble [549 × 2]> <tibble [28 × 2]> <split [521|28]>
    ## 5 healthyR.ai   <tibble [502 × 2]> <tibble [28 × 2]> <split [474|28]>
    ## 6 TidyDensity   <tibble [350 × 2]> <tibble [28 × 2]> <split [322|28]>

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
    ## 2 healthyR.data <tibble [544 × 2]> <tibble [28 × 2]> <split [516|28]> <mdl_tm_t>
    ## 3 healthyR.ts   <tibble [547 × 2]> <tibble [28 × 2]> <split [519|28]> <mdl_tm_t>
    ## 4 healthyverse  <tibble [549 × 2]> <tibble [28 × 2]> <split [521|28]> <mdl_tm_t>
    ## 5 healthyR.ai   <tibble [502 × 2]> <tibble [28 × 2]> <split [474|28]> <mdl_tm_t>
    ## 6 TidyDensity   <tibble [350 × 2]> <tibble [28 × 2]> <split [322|28]> <mdl_tm_t>
    ## # … with abbreviated variable name ¹​.modeltime_tables

### Model Accuracy

``` r
nested_modeltime_tbl %>%
  extract_nested_test_accuracy() %>%
  knitr::kable()
```

| package       | .model_id | .model_desc | .type |       mae |       mape |      mase |     smape |      rmse |       rsq |
|:--------------|----------:|:------------|:------|----------:|-----------:|----------:|----------:|----------:|----------:|
| healthyR      |         1 | ARIMA       | Test  | 0.7404428 |  579.84654 | 0.8550788 | 120.51027 | 0.8681084 | 0.0003551 |
| healthyR      |         2 | NULL        | NA    |        NA |         NA |        NA |        NA |        NA |        NA |
| healthyR      |         3 | LM          | Test  | 0.8207929 |  511.27515 | 0.9478687 | 146.06158 | 0.9298498 | 0.0001690 |
| healthyR      |         4 | EARTH       | Test  | 0.6286131 | 1865.62176 | 0.7259356 |  82.18150 | 0.7700216 | 0.0001690 |
| healthyR      |         5 | NNAR        | Test  | 0.8538202 |  712.49863 | 0.9860094 | 129.84584 | 1.0529258 | 0.0088521 |
| healthyR.data |         1 | ARIMA       | Test  | 0.6772178 |  115.20627 | 0.7455914 |  71.34641 | 0.8388603 | 0.0222336 |
| healthyR.data |         2 | NULL        | NA    |        NA |         NA |        NA |        NA |        NA |        NA |
| healthyR.data |         3 | LM          | Test  | 0.6881971 |  108.68422 | 0.7576791 |  72.92854 | 0.8428197 | 0.0002773 |
| healthyR.data |         4 | EARTH       | Test  | 0.7118089 |  102.62409 | 0.7836748 |  76.27798 | 0.8770096 | 0.0002773 |
| healthyR.data |         5 | NNAR        | Test  | 1.0800769 |  111.25930 | 1.1891241 | 140.27485 | 1.2865016 | 0.0418063 |
| healthyR.ts   |         1 | ARIMA       | Test  | 0.8846394 | 1498.29693 | 0.9485160 | 175.76762 | 1.0285026 | 0.1031698 |
| healthyR.ts   |         2 | NULL        | NA    |        NA |         NA |        NA |        NA |        NA |        NA |
| healthyR.ts   |         3 | LM          | Test  | 0.9503449 | 1004.45199 | 1.0189660 | 185.77176 | 1.0872771 | 0.0389448 |
| healthyR.ts   |         4 | EARTH       | Test  | 0.9424699 |  954.24403 | 1.0105224 | 186.38290 | 1.0804333 | 0.0389448 |
| healthyR.ts   |         5 | NNAR        | Test  | 0.8335264 | 1327.20079 | 0.8937124 | 165.38125 | 0.9929680 | 0.0097015 |
| healthyverse  |         1 | ARIMA       | Test  | 0.5659322 |   84.07631 | 0.6451183 |  59.16011 | 0.6723082 | 0.0035659 |
| healthyverse  |         2 | NULL        | NA    |        NA |         NA |        NA |        NA |        NA |        NA |
| healthyverse  |         3 | LM          | Test  | 0.6289820 |   68.63529 | 0.7169903 |  68.33011 | 0.7862897 | 0.0588609 |
| healthyverse  |         4 | EARTH       | Test  | 0.5799042 |   74.33114 | 0.6610454 |  61.43656 | 0.7140713 | 0.0588609 |
| healthyverse  |         5 | NNAR        | Test  | 0.8327800 |   74.04174 | 0.9493039 | 128.54422 | 1.0134251 | 0.0855009 |
| healthyR.ai   |         1 | ARIMA       | Test  | 0.8262337 |  104.02972 | 1.0703371 | 176.24552 | 0.9537140 | 0.0053360 |
| healthyR.ai   |         2 | NULL        | NA    |        NA |         NA |        NA |        NA |        NA |        NA |
| healthyR.ai   |         3 | LM          | Test  | 0.7909721 |   97.71727 | 1.0246577 | 189.22938 | 0.9177568 | 0.0260368 |
| healthyR.ai   |         4 | EARTH       | Test  | 0.8165621 |  102.80700 | 1.0578081 | 196.05505 | 0.9407146 | 0.0260368 |
| healthyR.ai   |         5 | NNAR        | Test  | 0.7793880 |  115.59675 | 1.0096512 | 133.11840 | 0.9751860 | 0.0009665 |
| TidyDensity   |         1 | ARIMA       | Test  | 0.6472597 |  119.65084 | 0.9317736 | 164.35479 | 0.7534716 | 0.0006007 |
| TidyDensity   |         2 | NULL        | NA    |        NA |         NA |        NA |        NA |        NA |        NA |
| TidyDensity   |         3 | LM          | Test  | 0.6512972 |  121.59696 | 0.9375859 | 162.97429 | 0.7607937 | 0.0239045 |
| TidyDensity   |         4 | EARTH       | Test  | 0.8413248 |  248.95698 | 1.2111432 | 113.54223 | 1.0402015 | 0.0239045 |
| TidyDensity   |         5 | NNAR        | Test  | 0.6504221 |  145.28503 | 0.9363260 | 160.33899 | 0.7531016 | 0.0043108 |

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
    ##   package       .model_id .model_…¹ .type   mae   mape  mase smape  rmse     rsq
    ##   <fct>             <int> <chr>     <chr> <dbl>  <dbl> <dbl> <dbl> <dbl>   <dbl>
    ## 1 healthyR              4 EARTH     Test  0.629 1866.  0.726  82.2 0.770 1.69e-4
    ## 2 healthyR.data         1 ARIMA     Test  0.677  115.  0.746  71.3 0.839 2.22e-2
    ## 3 healthyR.ts           5 NNAR      Test  0.834 1327.  0.894 165.  0.993 9.70e-3
    ## 4 healthyverse          1 ARIMA     Test  0.566   84.1 0.645  59.2 0.672 3.57e-3
    ## 5 healthyR.ai           3 LM        Test  0.791   97.7 1.02  189.  0.918 2.60e-2
    ## 6 TidyDensity           5 NNAR      Test  0.650  145.  0.936 160.  0.753 4.31e-3
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
    ## 2 healthyR.data <tibble [544 × 2]> <tibble [28 × 2]> <split [516|28]> <mdl_tm_t>
    ## 3 healthyR.ts   <tibble [547 × 2]> <tibble [28 × 2]> <split [519|28]> <mdl_tm_t>
    ## 4 healthyverse  <tibble [549 × 2]> <tibble [28 × 2]> <split [521|28]> <mdl_tm_t>
    ## 5 healthyR.ai   <tibble [502 × 2]> <tibble [28 × 2]> <split [474|28]> <mdl_tm_t>
    ## 6 TidyDensity   <tibble [350 × 2]> <tibble [28 × 2]> <split [322|28]> <mdl_tm_t>
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
