Time Series Analysis and Nested Modeling of the Healthyverse Packages
================
Steven P. Sanderson II, MPH - Data Scientist/IT Manager
27 December, 2022

This analysis follows a *Nested Modeltime Workflow*.

## Get Data

``` r
glimpse(downloads_tbl)
```

    ## Rows: 53,797
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

The last day in the data set is 2022-12-25 22:07:22, the file was
birthed on: 2022-07-02 23:58:17, and at report knit time is -4218.15
hours old. Happy analyzing!

Now that we have our data lets take a look at it using the `skimr`
package.

``` r
skim(downloads_tbl)
```

|                                                  |               |
|:-------------------------------------------------|:--------------|
| Name                                             | downloads_tbl |
| Number of rows                                   | 53797         |
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
| r_version     |     37001 |          0.31 |   5 |   5 |     0 |       35 |          0 |
| r_arch        |     37001 |          0.31 |   3 |   7 |     0 |        5 |          0 |
| r_os          |     37001 |          0.31 |   7 |  15 |     0 |       11 |          0 |
| package       |         0 |          1.00 |   8 |  13 |     0 |        6 |          0 |
| version       |         0 |          1.00 |   5 |   6 |     0 |       35 |          0 |
| country       |      4289 |          0.92 |   2 |   2 |     0 |      124 |          0 |

**Variable type: Date**

| skim_variable | n_missing | complete_rate | min        | max        | median     | n_unique |
|:--------------|----------:|--------------:|:-----------|:-----------|:-----------|---------:|
| date          |         0 |             1 | 2020-11-23 | 2022-12-25 | 2022-01-15 |      763 |

**Variable type: numeric**

| skim_variable | n_missing | complete_rate |      mean |         sd |  p0 |   p25 |    p50 |     p75 |    p100 | hist  |
|:--------------|----------:|--------------:|----------:|-----------:|----:|------:|-------:|--------:|--------:|:------|
| size          |         0 |             1 | 1352431.5 | 1709399.47 | 357 | 23262 | 312990 | 2731282 | 5677952 | ▇▁▂▂▁ |
| ip_id         |         0 |             1 |    8991.1 |   17116.04 |   1 |   127 |   2360 |    9447 |  143633 | ▇▁▁▁▁ |

**Variable type: POSIXct**

| skim_variable | n_missing | complete_rate | min                 | max                 | median              | n_unique |
|:--------------|----------:|--------------:|:--------------------|:--------------------|:--------------------|---------:|
| date_time     |         0 |             1 | 2020-11-23 09:00:41 | 2022-12-25 22:07:22 | 2022-01-15 09:51:40 |    31771 |

**Variable type: Timespan**

| skim_variable | n_missing | complete_rate | min | max |      median | n_unique |
|:--------------|----------:|--------------:|----:|----:|------------:|---------:|
| time          |         0 |             1 |   0 |  59 | 11H 37M 59S |       60 |

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
    ## 5 healthyR.ai   <tibble [491 × 2]> <tibble [28 × 2]> <split [463|28]>
    ## 6 TidyDensity   <tibble [339 × 2]> <tibble [28 × 2]> <split [311|28]>

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
    ## 5 healthyR.ai   <tibble [491 × 2]> <tibble [28 × 2]> <split [463|28]> <mdl_tm_t>
    ## 6 TidyDensity   <tibble [339 × 2]> <tibble [28 × 2]> <split [311|28]> <mdl_tm_t>
    ## # … with abbreviated variable name ¹​.modeltime_tables

### Model Accuracy

``` r
nested_modeltime_tbl %>%
  extract_nested_test_accuracy() %>%
  knitr::kable()
```

| package       | .model_id | .model_desc | .type |       mae |      mape |      mase |     smape |      rmse |       rsq |
|:--------------|----------:|:------------|:------|----------:|----------:|----------:|----------:|----------:|----------:|
| healthyR      |         1 | ARIMA       | Test  | 0.7053804 |  73.64168 | 0.8165370 |  97.81631 | 0.8723502 | 0.1292155 |
| healthyR      |         2 | NULL        | NA    |        NA |        NA |        NA |        NA |        NA |        NA |
| healthyR      |         3 | LM          | Test  | 0.8456412 |  85.10740 | 0.9789007 | 144.94360 | 0.9826591 | 0.1002087 |
| healthyR      |         4 | EARTH       | Test  | 0.6391581 |  73.12955 | 0.7398792 |  81.09145 | 0.8201955 | 0.1002087 |
| healthyR      |         5 | NNAR        | Test  | 0.8625244 | 100.35896 | 0.9984444 | 122.62019 | 1.0920355 | 0.0064440 |
| healthyR.data |         1 | ARIMA       | Test  | 0.6242280 | 157.27288 | 0.7764376 |  72.96937 | 0.7966258 | 0.0470665 |
| healthyR.data |         2 | NULL        | NA    |        NA |        NA |        NA |        NA |        NA |        NA |
| healthyR.data |         3 | LM          | Test  | 0.5935507 | 178.17282 | 0.7382801 |  67.98776 | 0.7535189 | 0.0048179 |
| healthyR.data |         4 | EARTH       | Test  | 0.6226887 | 165.00508 | 0.7745229 |  72.55916 | 0.7893023 | 0.0048179 |
| healthyR.data |         5 | NNAR        | Test  | 0.9837243 | 201.00595 | 1.2235922 | 141.75482 | 1.1876116 | 0.0001957 |
| healthyR.ts   |         1 | ARIMA       | Test  | 0.8732322 | 451.53922 | 1.0044336 | 159.07532 | 0.9906529 | 0.0432956 |
| healthyR.ts   |         2 | NULL        | NA    |        NA |        NA |        NA |        NA |        NA |        NA |
| healthyR.ts   |         3 | LM          | Test  | 0.7561444 | 206.94055 | 0.8697536 | 176.77569 | 0.8954685 | 0.0751689 |
| healthyR.ts   |         4 | EARTH       | Test  | 0.9684010 | 409.14403 | 1.1139012 | 164.02870 | 1.0952476 | 0.0751689 |
| healthyR.ts   |         5 | NNAR        | Test  | 0.6827884 | 114.92483 | 0.7853760 | 178.70397 | 0.8572475 | 0.0128871 |
| healthyverse  |         1 | ARIMA       | Test  | 0.5493965 |  58.93071 | 0.7869331 |  66.25618 | 0.7157485 | 0.0123383 |
| healthyverse  |         2 | NULL        | NA    |        NA |        NA |        NA |        NA |        NA |        NA |
| healthyverse  |         3 | LM          | Test  | 0.5382871 |  60.65120 | 0.7710205 |  63.89254 | 0.6817674 | 0.0287325 |
| healthyverse  |         4 | EARTH       | Test  | 0.4886387 |  63.10170 | 0.6999061 |  56.22787 | 0.6210741 | 0.0287325 |
| healthyverse  |         5 | NNAR        | Test  | 0.7342309 |  75.13481 | 1.0516823 | 130.46266 | 0.8660621 | 0.1147798 |
| healthyR.ai   |         1 | ARIMA       | Test  | 0.7082189 | 217.71270 | 1.0092653 | 178.84578 | 0.8232426 | 0.0082579 |
| healthyR.ai   |         2 | NULL        | NA    |        NA |        NA |        NA |        NA |        NA |        NA |
| healthyR.ai   |         3 | LM          | Test  | 0.5770522 |  97.74977 | 0.8223428 | 189.90799 | 0.7247709 | 0.0336395 |
| healthyR.ai   |         4 | EARTH       | Test  | 0.9164764 | 335.64209 | 1.3060479 | 172.39486 | 1.0362928 | 0.0336395 |
| healthyR.ai   |         5 | NNAR        | Test  | 0.5702291 | 128.96385 | 0.8126195 | 143.13026 | 0.7178131 | 0.0002383 |
| TidyDensity   |         1 | ARIMA       | Test  | 0.5730020 | 110.25899 | 0.6884928 | 142.95421 | 0.7000969 | 0.0070403 |
| TidyDensity   |         2 | NULL        | NA    |        NA |        NA |        NA |        NA |        NA |        NA |
| TidyDensity   |         3 | LM          | Test  | 0.5676019 | 109.05973 | 0.6820043 | 140.60156 | 0.6981122 | 0.0308242 |
| TidyDensity   |         4 | EARTH       | Test  | 1.8575317 | 587.95870 | 2.2319245 | 165.62755 | 1.9811623 | 0.0308242 |
| TidyDensity   |         5 | NNAR        | Test  | 0.5951325 | 123.36600 | 0.7150838 | 148.46032 | 0.7339050 | 0.0193311 |

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
    ## 1 healthyR              4 EARTH      Test  0.639  73.1 0.740  81.1 0.820 1.00e-1
    ## 2 healthyR.data         3 LM         Test  0.594 178.  0.738  68.0 0.754 4.82e-3
    ## 3 healthyR.ts           5 NNAR       Test  0.683 115.  0.785 179.  0.857 1.29e-2
    ## 4 healthyverse          4 EARTH      Test  0.489  63.1 0.700  56.2 0.621 2.87e-2
    ## 5 healthyR.ai           5 NNAR       Test  0.570 129.  0.813 143.  0.718 2.38e-4
    ## 6 TidyDensity           3 LM         Test  0.568 109.  0.682 141.  0.698 3.08e-2
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
    ## 5 healthyR.ai   <tibble [491 × 2]> <tibble [28 × 2]> <split [463|28]> <mdl_tm_t>
    ## 6 TidyDensity   <tibble [339 × 2]> <tibble [28 × 2]> <split [311|28]> <mdl_tm_t>
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
