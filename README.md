Time Series Analysis and Nested Modeling of the Healthyverse Packages
================
Steven P. Sanderson II, MPH - Date:
09 August, 2024

This analysis follows a *Nested Modeltime Workflow*.

## Get Data

``` r
glimpse(downloads_tbl)
```

    ## Rows: 110,168
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

The last day in the data set is 2024-08-07 23:41:51, the file was
birthed on: 2024-08-07 07:35:44, and at report knit time is -12.1 hours
old. Happy analyzing!

Now that we have our data lets take a look at it using the `skimr`
package.

``` r
skim(downloads_tbl)
```

|                                                  |               |
|:-------------------------------------------------|:--------------|
| Name                                             | downloads_tbl |
| Number of rows                                   | 110168        |
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
| r_version     |     77106 |          0.30 |   5 |   5 |     0 |       43 |          0 |
| r_arch        |     77106 |          0.30 |   3 |   7 |     0 |        5 |          0 |
| r_os          |     77106 |          0.30 |   7 |  15 |     0 |       19 |          0 |
| package       |         0 |          1.00 |   7 |  13 |     0 |        7 |          0 |
| version       |         0 |          1.00 |   5 |  17 |     0 |       59 |          0 |
| country       |      9476 |          0.91 |   2 |   2 |     0 |      157 |          0 |

**Variable type: Date**

| skim_variable | n_missing | complete_rate | min        | max        | median     | n_unique |
|:--------------|----------:|--------------:|:-----------|:-----------|:-----------|---------:|
| date          |         0 |             1 | 2020-11-23 | 2024-08-07 | 2023-01-18 |     1354 |

**Variable type: numeric**

| skim_variable | n_missing | complete_rate |       mean |         sd |  p0 |      p25 |      p50 |     p75 |    p100 | hist  |
|:--------------|----------:|--------------:|-----------:|-----------:|----:|---------:|---------:|--------:|--------:|:------|
| size          |         0 |             1 | 1182559.00 | 1562375.86 | 355 | 14701.00 | 289680.0 | 2373577 | 5677952 | ▇▁▂▁▁ |
| ip_id         |         0 |             1 |   10333.58 |   18081.02 |   1 |   312.75 |   3081.5 |   11485 |  143633 | ▇▁▁▁▁ |

**Variable type: POSIXct**

| skim_variable | n_missing | complete_rate | min                 | max                 | median              | n_unique |
|:--------------|----------:|--------------:|:--------------------|:--------------------|:--------------------|---------:|
| date_time     |         0 |             1 | 2020-11-23 09:00:41 | 2024-08-07 23:41:51 | 2023-01-18 19:48:33 |    66719 |

**Variable type: Timespan**

| skim_variable | n_missing | complete_rate | min | max | median | n_unique |
|:--------------|----------:|--------------:|----:|----:|-------:|---------:|
| time          |         0 |             1 |   0 |  59 |     12 |       60 |

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

    ## # A tibble: 8 × 4
    ##   package       .actual_data         .future_data      .splits          
    ##   <fct>         <list>               <list>            <list>           
    ## 1 healthyR.data <tibble [1,325 × 2]> <tibble [28 × 2]> <split [1297|28]>
    ## 2 healthyR      <tibble [1,317 × 2]> <tibble [28 × 2]> <split [1289|28]>
    ## 3 <NA>          <tibble [23 × 2]>    <tibble [28 × 2]> <split [0|23]>   
    ## 4 healthyR.ts   <tibble [1,263 × 2]> <tibble [28 × 2]> <split [1235|28]>
    ## 5 healthyverse  <tibble [1,234 × 2]> <tibble [28 × 2]> <split [1206|28]>
    ## 6 healthyR.ai   <tibble [1,060 × 2]> <tibble [28 × 2]> <split [1032|28]>
    ## 7 TidyDensity   <tibble [914 × 2]>   <tibble [28 × 2]> <split [886|28]> 
    ## 8 tidyAML       <tibble [530 × 2]>   <tibble [28 × 2]> <split [502|28]>

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
  knitr::kable()
```

| package       | .model_id | .model_desc | .type |       mae |     mape |      mase |    smape |      rmse |       rsq |
|:--------------|----------:|:------------|:------|----------:|---------:|----------:|---------:|----------:|----------:|
| healthyR.data |         1 | ARIMA       | Test  | 0.8355494 | 263.1566 | 0.6622722 | 140.5302 | 1.0721298 | 0.1642342 |
| healthyR.data |         2 | LM          | Test  | 0.8434577 | 347.9278 | 0.6685405 | 142.6668 | 1.0001643 | 0.0392799 |
| healthyR.data |         3 | EARTH       | Test  | 0.9514420 | 444.4872 | 0.7541308 | 135.6656 | 1.2327351 | 0.0392799 |
| healthyR.data |         4 | NNAR        | Test  | 0.8261422 | 196.1870 | 0.6548159 | 169.3616 | 1.0302126 | 0.0020327 |
| healthyR      |         1 | ARIMA       | Test  | 0.6720315 | 154.5761 | 0.7309824 | 145.8884 | 0.8125536 | 0.0591842 |
| healthyR      |         2 | LM          | Test  | 0.6745741 | 125.4390 | 0.7337481 | 158.9976 | 0.8661361 | 0.0000643 |
| healthyR      |         3 | EARTH       | Test  | 1.8054948 | 938.5018 | 1.9638736 | 146.3789 | 2.0226830 | 0.0000643 |
| healthyR      |         4 | NNAR        | Test  | 0.6146702 | 129.0926 | 0.6685893 | 139.4786 | 0.8097057 | 0.0724713 |
| NA            |         1 | NULL        | NA    |        NA |       NA |        NA |       NA |        NA |        NA |
| NA            |         2 | NULL        | NA    |        NA |       NA |        NA |       NA |        NA |        NA |
| NA            |         3 | NULL        | NA    |        NA |       NA |        NA |       NA |        NA |        NA |
| NA            |         4 | NULL        | NA    |        NA |       NA |        NA |       NA |        NA |        NA |
| healthyR.ts   |         1 | ARIMA       | Test  | 1.1161810 | 217.8079 | 0.8108130 | 156.7744 | 1.2465490 | 0.0230008 |
| healthyR.ts   |         2 | LM          | Test  | 1.0715167 | 186.6637 | 0.7783680 | 153.8695 | 1.2160228 | 0.0445120 |
| healthyR.ts   |         3 | EARTH       | Test  | 1.0732966 | 190.0235 | 0.7796610 | 153.1779 | 1.2170387 | 0.0445120 |
| healthyR.ts   |         4 | NNAR        | Test  | 0.9918980 | 101.1191 | 0.7205317 | 178.7174 | 1.1945025 | 0.2235858 |
| healthyverse  |         1 | ARIMA       | Test  | 0.7775483 | 429.6144 | 0.9366218 | 112.2093 | 0.9233848 | 0.0008115 |
| healthyverse  |         2 | LM          | Test  | 0.7830896 | 495.0009 | 0.9432968 | 107.3911 | 0.9427037 | 0.0040999 |
| healthyverse  |         3 | EARTH       | Test  | 0.7757163 | 347.6834 | 0.9344150 | 120.6267 | 0.9152934 | 0.0040999 |
| healthyverse  |         4 | NNAR        | Test  | 0.7988950 | 264.8274 | 0.9623358 | 134.6069 | 0.9474952 | 0.0086736 |
| healthyR.ai   |         1 | ARIMA       | Test  | 1.1528011 | 110.2552 | 0.8462042 | 160.9937 | 1.5404039 | 0.0016175 |
| healthyR.ai   |         2 | LM          | Test  | 1.1550041 | 109.8907 | 0.8478213 | 149.6577 | 1.5546553 | 0.0042561 |
| healthyR.ai   |         3 | EARTH       | Test  | 1.1535651 | 109.3199 | 0.8467650 | 151.4439 | 1.5492365 | 0.0042561 |
| healthyR.ai   |         4 | NNAR        | Test  | 1.1191246 | 109.3637 | 0.8214843 | 165.4401 | 1.4707526 | 0.0909927 |
| TidyDensity   |         1 | ARIMA       | Test  | 0.6591255 | 214.7077 | 0.8612111 | 110.8545 | 0.7990440 | 0.2047257 |
| TidyDensity   |         2 | LM          | Test  | 0.6977153 | 241.9701 | 0.9116324 | 111.7523 | 0.8440642 | 0.0000295 |
| TidyDensity   |         3 | EARTH       | Test  | 0.6311230 | 143.6246 | 0.8246231 | 136.5609 | 0.7838414 | 0.0000295 |
| TidyDensity   |         4 | NNAR        | Test  | 0.5647654 | 101.1015 | 0.7379205 | 139.6226 | 0.7008881 | 0.3276580 |
| tidyAML       |         1 | ARIMA       | Test  | 0.8542294 | 156.3037 | 0.9330052 | 124.7780 | 1.0105389 | 0.0584653 |
| tidyAML       |         2 | LM          | Test  | 0.8385747 | 130.0197 | 0.9159069 | 128.5211 | 1.0039399 | 0.0382068 |
| tidyAML       |         3 | EARTH       | Test  | 1.0876814 | 292.8763 | 1.1879858 | 120.8455 | 1.2922551 | 0.0382068 |
| tidyAML       |         4 | NNAR        | Test  | 0.9270799 | 197.9913 | 1.0125738 | 122.8157 | 1.0843353 | 0.0125162 |

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
    ##   # A tibble: 7 × 10
    ##   package      .model_id .model_desc .type   mae  mape  mase smape  rmse     rsq
    ##   <fct>            <int> <chr>       <chr> <dbl> <dbl> <dbl> <dbl> <dbl>   <dbl>
    ## 1 healthyR.da…         2 LM          Test  0.843  348. 0.669  143. 1.00  0.0393 
    ## 2 healthyR             4 NNAR        Test  0.615  129. 0.669  139. 0.810 0.0725 
    ## 3 healthyR.ts          4 NNAR        Test  0.992  101. 0.721  179. 1.19  0.224  
    ## 4 healthyverse         3 EARTH       Test  0.776  348. 0.934  121. 0.915 0.00410
    ## 5 healthyR.ai          4 NNAR        Test  1.12   109. 0.821  165. 1.47  0.0910 
    ## 6 TidyDensity          4 NNAR        Test  0.565  101. 0.738  140. 0.701 0.328  
    ## 7 tidyAML              2 LM          Test  0.839  130. 0.916  129. 1.00  0.0382

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
    ##   # A tibble: 7 × 5
    ##   package       .actual_data .future_data .splits           .modeltime_tables 
    ##   <fct>         <list>       <list>       <list>            <list>            
    ## 1 healthyR.data <tibble>     <tibble>     <split [1297|28]> <mdl_tm_t [1 × 5]>
    ## 2 healthyR      <tibble>     <tibble>     <split [1289|28]> <mdl_tm_t [1 × 5]>
    ## 3 healthyR.ts   <tibble>     <tibble>     <split [1235|28]> <mdl_tm_t [1 × 5]>
    ## 4 healthyverse  <tibble>     <tibble>     <split [1206|28]> <mdl_tm_t [1 × 5]>
    ## 5 healthyR.ai   <tibble>     <tibble>     <split [1032|28]> <mdl_tm_t [1 × 5]>
    ## 6 TidyDensity   <tibble>     <tibble>     <split [886|28]>  <mdl_tm_t [1 × 5]>
    ## 7 tidyAML       <tibble>     <tibble>     <split [502|28]>  <mdl_tm_t [1 × 5]>

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
