Time Series Analysis and Nested Modeling of the Healthyverse Packages
================
Steven P. Sanderson II, MPH - Date:
15 August, 2024

This analysis follows a *Nested Modeltime Workflow*.

## Get Data

``` r
glimpse(downloads_tbl)
```

    ## Rows: 110,787
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

The last day in the data set is 2024-08-13 22:54:26, the file was
birthed on: 2024-08-07 07:35:44, and at report knit time is -155.31
hours old. Happy analyzing!

Now that we have our data lets take a look at it using the `skimr`
package.

``` r
skim(downloads_tbl)
```

|                                                  |               |
|:-------------------------------------------------|:--------------|
| Name                                             | downloads_tbl |
| Number of rows                                   | 110787        |
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
| r_version     |     77582 |          0.30 |   5 |   5 |     0 |       43 |          0 |
| r_arch        |     77582 |          0.30 |   3 |   7 |     0 |        5 |          0 |
| r_os          |     77582 |          0.30 |   7 |  15 |     0 |       19 |          0 |
| package       |         0 |          1.00 |   7 |  13 |     0 |        7 |          0 |
| version       |         0 |          1.00 |   5 |  17 |     0 |       59 |          0 |
| country       |      9519 |          0.91 |   2 |   2 |     0 |      157 |          0 |

**Variable type: Date**

| skim_variable | n_missing | complete_rate | min        | max        | median     | n_unique |
|:--------------|----------:|--------------:|:-----------|:-----------|:-----------|---------:|
| date          |         0 |             1 | 2020-11-23 | 2024-08-13 | 2023-01-22 |     1360 |

**Variable type: numeric**

| skim_variable | n_missing | complete_rate |       mean |         sd |  p0 |   p25 |    p50 |     p75 |    p100 | hist  |
|:--------------|----------:|--------------:|-----------:|-----------:|----:|------:|-------:|--------:|--------:|:------|
| size          |         0 |             1 | 1181703.73 | 1561349.57 | 355 | 14701 | 289680 | 2373526 | 5677952 | ▇▁▂▁▁ |
| ip_id         |         0 |             1 |   10323.45 |   18063.15 |   1 |   317 |   3091 |   11474 |  143633 | ▇▁▁▁▁ |

**Variable type: POSIXct**

| skim_variable | n_missing | complete_rate | min                 | max                 | median              | n_unique |
|:--------------|----------:|--------------:|:--------------------|:--------------------|:--------------------|---------:|
| date_time     |         0 |             1 | 2020-11-23 09:00:41 | 2024-08-13 22:54:26 | 2023-01-22 20:17:53 |    67078 |

**Variable type: Timespan**

| skim_variable | n_missing | complete_rate | min | max |     median | n_unique |
|:--------------|----------:|--------------:|----:|----:|-----------:|---------:|
| time          |         0 |             1 |   0 |  59 | 12H 7M 25S |       60 |

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
    ## 1 healthyR.data <tibble [1,331 × 2]> <tibble [28 × 2]> <split [1303|28]>
    ## 2 healthyR      <tibble [1,323 × 2]> <tibble [28 × 2]> <split [1295|28]>
    ## 3 <NA>          <tibble [23 × 2]>    <tibble [28 × 2]> <split [0|23]>   
    ## 4 healthyR.ts   <tibble [1,269 × 2]> <tibble [28 × 2]> <split [1241|28]>
    ## 5 healthyverse  <tibble [1,240 × 2]> <tibble [28 × 2]> <split [1212|28]>
    ## 6 healthyR.ai   <tibble [1,066 × 2]> <tibble [28 × 2]> <split [1038|28]>
    ## 7 TidyDensity   <tibble [920 × 2]>   <tibble [28 × 2]> <split [892|28]> 
    ## 8 tidyAML       <tibble [536 × 2]>   <tibble [28 × 2]> <split [508|28]>

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

| package       | .model_id | .model_desc | .type |       mae |      mape |      mase |    smape |      rmse |       rsq |
|:--------------|----------:|:------------|:------|----------:|----------:|----------:|---------:|----------:|----------:|
| healthyR.data |         1 | ARIMA       | Test  | 0.6612501 |  182.7469 | 0.6309272 | 164.2890 | 0.8154862 | 0.0821903 |
| healthyR.data |         2 | LM          | Test  | 0.8101541 |  336.1964 | 0.7730028 | 152.4174 | 0.9504299 | 0.0011648 |
| healthyR.data |         3 | EARTH       | Test  | 2.5546469 | 1711.2185 | 2.4374985 | 161.4691 | 2.8388930 | 0.0011648 |
| healthyR.data |         4 | NNAR        | Test  | 0.6521408 |  132.1956 | 0.6222355 | 148.0896 | 0.8538034 | 0.0006407 |
| healthyR      |         1 | ARIMA       | Test  | 0.7253749 |  128.0158 | 0.9622269 | 167.1513 | 0.8521315 | 0.0728576 |
| healthyR      |         2 | LM          | Test  | 0.7674557 |  113.2734 | 1.0180479 | 161.6821 | 0.9607457 | 0.0230253 |
| healthyR      |         3 | EARTH       | Test  | 0.7135074 |  134.9068 | 0.9464843 | 148.3505 | 0.8485978 | 0.0230253 |
| healthyR      |         4 | NNAR        | Test  | 0.7246933 |  102.0154 | 0.9613227 | 163.3197 | 0.8944606 | 0.1606476 |
| NA            |         1 | NULL        | NA    |        NA |        NA |        NA |       NA |        NA |        NA |
| NA            |         2 | NULL        | NA    |        NA |        NA |        NA |       NA |        NA |        NA |
| NA            |         3 | NULL        | NA    |        NA |        NA |        NA |       NA |        NA |        NA |
| NA            |         4 | NULL        | NA    |        NA |        NA |        NA |       NA |        NA |        NA |
| healthyR.ts   |         1 | ARIMA       | Test  | 1.6074641 |  871.0402 | 1.2844585 | 139.1865 | 1.8048034 | 0.0072976 |
| healthyR.ts   |         2 | LM          | Test  | 0.9338746 |  239.6357 | 0.7462208 | 142.4929 | 1.1432794 | 0.0068742 |
| healthyR.ts   |         3 | EARTH       | Test  | 0.9365208 |  245.1320 | 0.7483353 | 141.9117 | 1.1449335 | 0.0068742 |
| healthyR.ts   |         4 | NNAR        | Test  | 0.9552037 |  126.3175 | 0.7632640 | 190.2001 | 1.1991858 | 0.0168610 |
| healthyverse  |         1 | ARIMA       | Test  | 0.7640279 |  438.1556 | 0.9880189 | 111.5566 | 0.9172342 | 0.0080659 |
| healthyverse  |         2 | LM          | Test  | 0.7750745 |  528.4332 | 1.0023041 | 106.4582 | 0.9286200 | 0.0235566 |
| healthyverse  |         3 | EARTH       | Test  | 0.7791889 |  378.3826 | 1.0076247 | 119.6080 | 0.9156721 | 0.0235566 |
| healthyverse  |         4 | NNAR        | Test  | 0.8199671 |  272.6748 | 1.0603580 | 137.9416 | 0.9841614 | 0.1385426 |
| healthyR.ai   |         1 | ARIMA       | Test  | 1.0918119 |  129.1963 | 0.9072348 | 150.7169 | 1.5345231 | 0.0184230 |
| healthyR.ai   |         2 | LM          | Test  | 1.0853167 |  141.9145 | 0.9018376 | 136.1182 | 1.5399744 | 0.1284102 |
| healthyR.ai   |         3 | EARTH       | Test  | 1.0851212 |  145.4034 | 0.9016751 | 135.0829 | 1.5411142 | 0.1284102 |
| healthyR.ai   |         4 | NNAR        | Test  | 1.0783176 |  111.5954 | 0.8960218 | 167.1843 | 1.4945153 | 0.0318659 |
| TidyDensity   |         1 | ARIMA       | Test  | 0.6637824 |  196.6299 | 0.8089019 | 109.0652 | 0.7913925 | 0.4102070 |
| TidyDensity   |         2 | LM          | Test  | 0.7153341 |  208.7295 | 0.8717241 | 111.1165 | 0.8561489 | 0.0012452 |
| TidyDensity   |         3 | EARTH       | Test  | 0.7852945 |  126.8853 | 0.9569797 | 174.0477 | 0.9671511 | 0.0012452 |
| TidyDensity   |         4 | NNAR        | Test  | 0.6377724 |  105.3716 | 0.7772055 | 149.2831 | 0.7857874 | 0.2561111 |
| tidyAML       |         1 | ARIMA       | Test  | 0.7030665 |  113.0755 | 0.8687103 | 119.8426 | 0.8729920 | 0.2660633 |
| tidyAML       |         2 | LM          | Test  | 0.7688074 |  127.0606 | 0.9499398 | 121.3281 | 0.9577087 | 0.1043895 |
| tidyAML       |         3 | EARTH       | Test  | 0.9309588 |  221.5958 | 1.1502944 | 119.6478 | 1.1089922 | 0.1043895 |
| tidyAML       |         4 | NNAR        | Test  | 0.7271577 |  153.7794 | 0.8984773 | 115.1081 | 0.8709953 | 0.3207300 |

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
    ## 1 healthyR.da…         1 ARIMA       Test  0.661  183. 0.631  164. 0.815 0.0822 
    ## 2 healthyR             3 EARTH       Test  0.714  135. 0.946  148. 0.849 0.0230 
    ## 3 healthyR.ts          2 LM          Test  0.934  240. 0.746  142. 1.14  0.00687
    ## 4 healthyverse         3 EARTH       Test  0.779  378. 1.01   120. 0.916 0.0236 
    ## 5 healthyR.ai          4 NNAR        Test  1.08   112. 0.896  167. 1.49  0.0319 
    ## 6 TidyDensity          4 NNAR        Test  0.638  105. 0.777  149. 0.786 0.256  
    ## 7 tidyAML              4 NNAR        Test  0.727  154. 0.898  115. 0.871 0.321

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
    ## 1 healthyR.data <tibble>     <tibble>     <split [1303|28]> <mdl_tm_t [1 × 5]>
    ## 2 healthyR      <tibble>     <tibble>     <split [1295|28]> <mdl_tm_t [1 × 5]>
    ## 3 healthyR.ts   <tibble>     <tibble>     <split [1241|28]> <mdl_tm_t [1 × 5]>
    ## 4 healthyverse  <tibble>     <tibble>     <split [1212|28]> <mdl_tm_t [1 × 5]>
    ## 5 healthyR.ai   <tibble>     <tibble>     <split [1038|28]> <mdl_tm_t [1 × 5]>
    ## 6 TidyDensity   <tibble>     <tibble>     <split [892|28]>  <mdl_tm_t [1 × 5]>
    ## 7 tidyAML       <tibble>     <tibble>     <split [508|28]>  <mdl_tm_t [1 × 5]>

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
