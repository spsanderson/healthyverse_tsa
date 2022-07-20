Time Series Analysis and Modeling of the Healthyverse Packages
================
Steven P. Sanderson II, MPH - Data Scientist/IT Manager
19 July, 2022

## Get Data

``` r
glimpse(downloads_tbl)
```

    ## Rows: 41,963
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

The last day in the data set is 2022-07-17 23:35:45, the file was
birthed on: 2022-07-02 23:58:17, and at report knit time is -355.62
hours old. Happy analyzing!

Now that we have our data lets take a look at it using the `skimr`
package.

``` r
skim(downloads_tbl)
```

|                                                  |               |
|:-------------------------------------------------|:--------------|
| Name                                             | downloads_tbl |
| Number of rows                                   | 41963         |
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
| r_version     |     28210 |          0.33 |   5 |   5 |     0 |       32 |          0 |
| r_arch        |     28210 |          0.33 |   3 |   7 |     0 |        5 |          0 |
| r_os          |     28210 |          0.33 |   7 |  15 |     0 |       11 |          0 |
| package       |         0 |          1.00 |   8 |  13 |     0 |        6 |          0 |
| version       |         0 |          1.00 |   5 |   5 |     0 |       24 |          0 |
| country       |      3365 |          0.92 |   2 |   2 |     0 |      114 |          0 |

**Variable type: Date**

| skim_variable | n_missing | complete_rate | min        | max        | median     | n_unique |
|:--------------|----------:|--------------:|:-----------|:-----------|:-----------|---------:|
| date          |         0 |             1 | 2020-11-23 | 2022-07-17 | 2021-11-08 |      602 |

**Variable type: numeric**

| skim_variable | n_missing | complete_rate |       mean |         sd |  p0 |   p25 |    p50 |     p75 |    p100 | hist  |
|:--------------|----------:|--------------:|-----------:|-----------:|----:|------:|-------:|--------:|--------:|:------|
| size          |         0 |             1 | 1418350.81 | 1790369.21 | 357 | 16873 | 289681 | 2971375 | 5677952 | ▇▁▂▂▁ |
| ip_id         |         0 |             1 |    8721.54 |   16720.77 |   1 |   167 |   2580 |    8798 |  143633 | ▇▁▁▁▁ |

**Variable type: POSIXct**

| skim_variable | n_missing | complete_rate | min                 | max                 | median              | n_unique |
|:--------------|----------:|--------------:|:--------------------|:--------------------|:--------------------|---------:|
| date_time     |         0 |             1 | 2020-11-23 09:00:41 | 2022-07-17 23:35:45 | 2021-11-08 17:07:30 |    24550 |

**Variable type: Timespan**

| skim_variable | n_missing | complete_rate | min | max |      median | n_unique |
|:--------------|----------:|--------------:|----:|----:|------------:|---------:|
| time          |         0 |             1 |   0 |  59 | 11H 23M 33S |       60 |

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
    ## 1 healthyR      <tibble [546 × 2]> <tibble [28 × 2]> <split [518|28]>
    ## 2 healthyR.data <tibble [543 × 2]> <tibble [28 × 2]> <split [515|28]>
    ## 3 healthyR.ts   <tibble [539 × 2]> <tibble [28 × 2]> <split [511|28]>
    ## 4 healthyverse  <tibble [515 × 2]> <tibble [28 × 2]> <split [487|28]>
    ## 5 healthyR.ai   <tibble [330 × 2]> <tibble [28 × 2]> <split [302|28]>
    ## 6 TidyDensity   <tibble [178 × 2]> <tibble [28 × 2]> <split [150|28]>

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
    ##   package       .actual_data       .future_data .splits          .modeltime_ta…¹
    ##   <fct>         <list>             <list>       <list>           <list>         
    ## 1 healthyR      <tibble [546 × 2]> <tibble>     <split [518|28]> <mdl_time_tbl> 
    ## 2 healthyR.data <tibble [543 × 2]> <tibble>     <split [515|28]> <mdl_time_tbl> 
    ## 3 healthyR.ts   <tibble [539 × 2]> <tibble>     <split [511|28]> <mdl_time_tbl> 
    ## 4 healthyverse  <tibble [515 × 2]> <tibble>     <split [487|28]> <mdl_time_tbl> 
    ## 5 healthyR.ai   <tibble [330 × 2]> <tibble>     <split [302|28]> <mdl_time_tbl> 
    ## 6 TidyDensity   <tibble [178 × 2]> <tibble>     <split [150|28]> <mdl_time_tbl> 
    ## # … with abbreviated variable name ¹​.modeltime_tables

### Model Accuracy

``` r
nested_modeltime_tbl %>%
  extract_nested_test_accuracy() %>%
  knitr::kable()
```

| package       | .model_id | .model_desc | .type |       mae |      mape |      mase |     smape |      rmse |       rsq |
|:--------------|----------:|:------------|:------|----------:|----------:|----------:|----------:|----------:|----------:|
| healthyR      |         1 | ARIMA       | Test  | 0.5655915 | 132.38179 | 0.5379867 | 146.80899 | 0.7007541 | 0.1516892 |
| healthyR      |         2 | NULL        | NA    |        NA |        NA |        NA |        NA |        NA |        NA |
| healthyR      |         3 | LM          | Test  | 0.6094394 |  98.86571 | 0.5796945 | 192.83442 | 0.7480666 | 0.0020997 |
| healthyR      |         4 | EARTH       | Test  | 0.9263144 | 480.14945 | 0.8811038 | 132.01585 | 1.1446782 | 0.0020997 |
| healthyR      |         5 | NNAR        | Test  | 0.6448237 | 171.89251 | 0.6133518 | 151.22267 | 0.7909987 | 0.0048388 |
| healthyR.data |         1 | ARIMA       | Test  | 0.6757179 | 128.87844 | 0.6470700 |  94.42837 | 0.8606833 | 0.0004054 |
| healthyR.data |         2 | NULL        | NA    |        NA |        NA |        NA |        NA |        NA |        NA |
| healthyR.data |         3 | LM          | Test  | 0.6694996 |  84.17816 | 0.6411153 | 112.92765 | 0.8577534 | 0.0039213 |
| healthyR.data |         4 | EARTH       | Test  | 1.3246507 | 312.48453 | 1.2684904 | 112.29818 | 1.5790628 | 0.0039213 |
| healthyR.data |         5 | NNAR        | Test  | 0.8059671 | 112.40281 | 0.7717971 | 153.71988 | 1.0222615 | 0.0311393 |
| healthyR.ts   |         1 | ARIMA       | Test  | 0.6468247 |  92.81588 | 0.5241874 | 148.36539 | 0.8029875 | 0.1879242 |
| healthyR.ts   |         2 | NULL        | NA    |        NA |        NA |        NA |        NA |        NA |        NA |
| healthyR.ts   |         3 | LM          | Test  | 0.7188285 | 103.31940 | 0.5825393 | 183.23084 | 0.8678163 | 0.0008364 |
| healthyR.ts   |         4 | EARTH       | Test  | 0.7173667 | 122.44168 | 0.5813547 | 139.32954 | 0.8840634 | 0.0008364 |
| healthyR.ts   |         5 | NNAR        | Test  | 0.8481029 | 273.00314 | 0.6873034 | 156.05893 | 1.0325425 | 0.0077811 |
| healthyverse  |         1 | ARIMA       | Test  | 0.6273316 | 201.18068 | 0.6195471 | 114.86819 | 0.7995790 | 0.0004859 |
| healthyverse  |         2 | NULL        | NA    |        NA |        NA |        NA |        NA |        NA |        NA |
| healthyverse  |         3 | LM          | Test  | 0.6282850 | 224.99738 | 0.6204886 | 115.30304 | 0.7808559 | 0.0110279 |
| healthyverse  |         4 | EARTH       | Test  | 0.8274908 | 556.02513 | 0.8172225 | 109.39905 | 0.9909475 | 0.0110279 |
| healthyverse  |         5 | NNAR        | Test  | 0.6834608 | 168.61343 | 0.6749798 | 142.82301 | 0.8498168 | 0.0057138 |
| healthyR.ai   |         1 | ARIMA       | Test  | 0.7098056 | 182.68351 | 0.6605898 | 165.52423 | 0.8933548 | 0.0646031 |
| healthyR.ai   |         2 | NULL        | NA    |        NA |        NA |        NA |        NA |        NA |        NA |
| healthyR.ai   |         3 | LM          | Test  | 0.7369270 | 116.68286 | 0.6858307 | 167.42754 | 0.9338307 | 0.1459483 |
| healthyR.ai   |         4 | EARTH       | Test  | 0.7017602 |  97.84553 | 0.6531023 | 190.78522 | 0.8937152 | 0.1459483 |
| healthyR.ai   |         5 | NNAR        | Test  | 0.7220606 | 173.74582 | 0.6719951 | 134.28932 | 0.9626504 | 0.0210975 |
| TidyDensity   |         1 | ARIMA       | Test  | 0.6045902 | 100.79892 | 0.6522139 | 181.68558 | 0.6978543 | 0.0267364 |
| TidyDensity   |         2 | NULL        | NA    |        NA |        NA |        NA |        NA |        NA |        NA |
| TidyDensity   |         3 | LM          | Test  | 0.6023867 |  99.56834 | 0.6498369 | 165.10303 | 0.6966865 | 0.0142188 |
| TidyDensity   |         4 | EARTH       | Test  | 0.6363870 | 114.16753 | 0.6865154 | 138.05602 | 0.7588403 | 0.0142188 |
| TidyDensity   |         5 | NNAR        | Test  | 0.6963791 | 149.69327 | 0.7512331 | 143.89741 | 0.8371439 | 0.0000800 |

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
    ## 1 healthyR              1 ARIMA      Test  0.566 132.  0.538  147. 0.701 0.152  
    ## 2 healthyR.data         3 LM         Test  0.669  84.2 0.641  113. 0.858 0.00392
    ## 3 healthyR.ts           1 ARIMA      Test  0.647  92.8 0.524  148. 0.803 0.188  
    ## 4 healthyverse          3 LM         Test  0.628 225.  0.620  115. 0.781 0.0110 
    ## 5 healthyR.ai           1 ARIMA      Test  0.710 183.  0.661  166. 0.893 0.0646 
    ## 6 TidyDensity           3 LM         Test  0.602  99.6 0.650  165. 0.697 0.0142 
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
    ##   package       .actual_data       .future_data .splits          .modeltime_ta…¹
    ##   <fct>         <list>             <list>       <list>           <list>         
    ## 1 healthyR      <tibble [546 × 2]> <tibble>     <split [518|28]> <mdl_time_tbl> 
    ## 2 healthyR.data <tibble [543 × 2]> <tibble>     <split [515|28]> <mdl_time_tbl> 
    ## 3 healthyR.ts   <tibble [539 × 2]> <tibble>     <split [511|28]> <mdl_time_tbl> 
    ## 4 healthyverse  <tibble [515 × 2]> <tibble>     <split [487|28]> <mdl_time_tbl> 
    ## 5 healthyR.ai   <tibble [330 × 2]> <tibble>     <split [302|28]> <mdl_time_tbl> 
    ## 6 TidyDensity   <tibble [178 × 2]> <tibble>     <split [150|28]> <mdl_time_tbl> 
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
