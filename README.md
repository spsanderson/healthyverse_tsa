Time Series Analysis and Nested Modeling of the Healthyverse Packages
================
Steven P. Sanderson II, MPH - Date:
13 August, 2024

This analysis follows a *Nested Modeltime Workflow*.

## Get Data

``` r
glimpse(downloads_tbl)
```

    ## Rows: 110,583
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

The last day in the data set is 2024-08-11 23:18:02, the file was
birthed on: 2024-08-07 07:35:44, and at report knit time is -107.7 hours
old. Happy analyzing!

Now that we have our data lets take a look at it using the `skimr`
package.

``` r
skim(downloads_tbl)
```

|                                                  |               |
|:-------------------------------------------------|:--------------|
| Name                                             | downloads_tbl |
| Number of rows                                   | 110583        |
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
| r_version     |     77442 |          0.30 |   5 |   5 |     0 |       43 |          0 |
| r_arch        |     77442 |          0.30 |   3 |   7 |     0 |        5 |          0 |
| r_os          |     77442 |          0.30 |   7 |  15 |     0 |       19 |          0 |
| package       |         0 |          1.00 |   7 |  13 |     0 |        7 |          0 |
| version       |         0 |          1.00 |   5 |  17 |     0 |       59 |          0 |
| country       |      9503 |          0.91 |   2 |   2 |     0 |      157 |          0 |

**Variable type: Date**

| skim_variable | n_missing | complete_rate | min        | max        | median     | n_unique |
|:--------------|----------:|--------------:|:-----------|:-----------|:-----------|---------:|
| date          |         0 |             1 | 2020-11-23 | 2024-08-11 | 2023-01-21 |     1358 |

**Variable type: numeric**

| skim_variable | n_missing | complete_rate |    mean |         sd |  p0 |   p25 |    p50 |     p75 |    p100 | hist  |
|:--------------|----------:|--------------:|--------:|-----------:|----:|------:|-------:|--------:|--------:|:------|
| size          |         0 |             1 | 1181219 | 1561654.03 | 355 | 14701 | 289676 | 2373537 | 5677952 | ▇▁▂▁▁ |
| ip_id         |         0 |             1 |   10322 |   18060.58 |   1 |   317 |   3091 |   11474 |  143633 | ▇▁▁▁▁ |

**Variable type: POSIXct**

| skim_variable | n_missing | complete_rate | min                 | max                 | median              | n_unique |
|:--------------|----------:|--------------:|:--------------------|:--------------------|:--------------------|---------:|
| date_time     |         0 |             1 | 2020-11-23 09:00:41 | 2024-08-11 23:18:02 | 2023-01-21 01:03:06 |    66931 |

**Variable type: Timespan**

| skim_variable | n_missing | complete_rate | min | max |     median | n_unique |
|:--------------|----------:|--------------:|----:|----:|-----------:|---------:|
| time          |         0 |             1 |   0 |  59 | 12H 7M 54S |       60 |

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
    ## 1 healthyR.data <tibble [1,329 × 2]> <tibble [28 × 2]> <split [1301|28]>
    ## 2 healthyR      <tibble [1,321 × 2]> <tibble [28 × 2]> <split [1293|28]>
    ## 3 <NA>          <tibble [23 × 2]>    <tibble [28 × 2]> <split [0|23]>   
    ## 4 healthyR.ts   <tibble [1,267 × 2]> <tibble [28 × 2]> <split [1239|28]>
    ## 5 healthyverse  <tibble [1,238 × 2]> <tibble [28 × 2]> <split [1210|28]>
    ## 6 healthyR.ai   <tibble [1,064 × 2]> <tibble [28 × 2]> <split [1036|28]>
    ## 7 TidyDensity   <tibble [918 × 2]>   <tibble [28 × 2]> <split [890|28]> 
    ## 8 tidyAML       <tibble [534 × 2]>   <tibble [28 × 2]> <split [506|28]>

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

| package       | .model_id | .model_desc | .type |       mae |       mape |      mase |    smape |      rmse |       rsq |
|:--------------|----------:|:------------|:------|----------:|-----------:|----------:|---------:|----------:|----------:|
| healthyR.data |         1 | ARIMA       | Test  | 0.6707850 |  181.47872 | 0.6039593 | 161.8303 | 0.8360494 | 0.0720078 |
| healthyR.data |         2 | LM          | Test  | 0.8004777 |  377.50954 | 0.7207316 | 148.6186 | 0.9437003 | 0.0050369 |
| healthyR.data |         3 | EARTH       | Test  | 0.7018576 |  212.96615 | 0.6319363 | 140.7278 | 0.9121794 | 0.0050369 |
| healthyR.data |         4 | NNAR        | Test  | 0.6895250 |  127.67379 | 0.6208323 | 153.6320 | 0.8931929 | 0.0064791 |
| healthyR      |         1 | ARIMA       | Test  | 0.6815198 |  131.12336 | 0.8606502 | 148.5351 | 0.8133042 | 0.0132019 |
| healthyR      |         2 | LM          | Test  | 0.7084566 |  112.54748 | 0.8946670 | 159.8810 | 0.8951152 | 0.0069750 |
| healthyR      |         3 | EARTH       | Test  | 0.6779416 |  137.08993 | 0.8561316 | 147.3298 | 0.8133118 | 0.0069750 |
| healthyR      |         4 | NNAR        | Test  | 0.6672375 |  109.25565 | 0.8426139 | 153.2207 | 0.8452387 | 0.0396696 |
| NA            |         1 | NULL        | NA    |        NA |         NA |        NA |       NA |        NA |        NA |
| NA            |         2 | NULL        | NA    |        NA |         NA |        NA |       NA |        NA |        NA |
| NA            |         3 | NULL        | NA    |        NA |         NA |        NA |       NA |        NA |        NA |
| NA            |         4 | NULL        | NA    |        NA |         NA |        NA |       NA |        NA |        NA |
| healthyR.ts   |         1 | ARIMA       | Test  | 2.0107419 | 1182.56066 | 1.5647154 | 137.4780 | 2.3013620 | 0.0025846 |
| healthyR.ts   |         2 | LM          | Test  | 0.9834032 |  237.95214 | 0.7652629 | 147.6456 | 1.1667120 | 0.0041038 |
| healthyR.ts   |         3 | EARTH       | Test  | 0.9860010 |  243.27680 | 0.7672844 | 147.1442 | 1.1677033 | 0.0041038 |
| healthyR.ts   |         4 | NNAR        | Test  | 0.9815193 |  108.29580 | 0.7637968 | 187.8389 | 1.2289532 | 0.0189989 |
| healthyverse  |         1 | ARIMA       | Test  | 0.7510271 |  474.97989 | 0.9664274 | 113.0141 | 0.9039970 | 0.0232924 |
| healthyverse  |         2 | LM          | Test  | 0.7706703 |  617.93069 | 0.9917044 | 107.1110 | 0.9298251 | 0.0298216 |
| healthyverse  |         3 | EARTH       | Test  | 0.7744903 |  443.36161 | 0.9966200 | 120.8293 | 0.9132408 | 0.0298216 |
| healthyverse  |         4 | NNAR        | Test  | 0.8075087 |  304.09549 | 1.0391084 | 141.8578 | 0.9703760 | 0.0310847 |
| healthyR.ai   |         1 | ARIMA       | Test  | 1.1255553 |  101.72996 | 0.8823143 | 151.0742 | 1.5476856 | 0.0233168 |
| healthyR.ai   |         2 | LM          | Test  | 1.1105115 |   99.65577 | 0.8705216 | 138.3291 | 1.5460091 | 0.0749650 |
| healthyR.ai   |         3 | EARTH       | Test  | 1.1106040 |  100.10334 | 0.8705942 | 137.1070 | 1.5486552 | 0.0749650 |
| healthyR.ai   |         4 | NNAR        | Test  | 1.0734372 |   97.89274 | 0.8414594 | 153.6386 | 1.4746175 | 0.0260317 |
| TidyDensity   |         1 | ARIMA       | Test  | 0.6699092 |  193.53719 | 0.7926990 | 110.2302 | 0.7908162 | 0.3834650 |
| TidyDensity   |         2 | LM          | Test  | 0.7168364 |  208.75544 | 0.8482276 | 111.2662 | 0.8559200 | 0.0042985 |
| TidyDensity   |         3 | EARTH       | Test  | 0.7655710 |  123.33631 | 0.9058949 | 169.3506 | 0.9578915 | 0.0042985 |
| TidyDensity   |         4 | NNAR        | Test  | 0.6236573 |  103.30635 | 0.7379694 | 142.3129 | 0.7671958 | 0.2640931 |
| tidyAML       |         1 | ARIMA       | Test  | 0.8200268 |  138.97236 | 0.9452668 | 129.9479 | 0.9680151 | 0.1248846 |
| tidyAML       |         2 | LM          | Test  | 0.8318719 |  133.56737 | 0.9589209 | 129.2774 | 1.0009996 | 0.0785101 |
| tidyAML       |         3 | EARTH       | Test  | 1.0196047 |  244.04218 | 1.1753255 | 124.8282 | 1.2014367 | 0.0785101 |
| tidyAML       |         4 | NNAR        | Test  | 0.9270494 |  196.94028 | 1.0686345 | 128.2361 | 1.0729511 | 0.0338816 |

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
    ## 1 healthyR.da…         1 ARIMA       Test  0.671 181.  0.604  162. 0.836 0.0720 
    ## 2 healthyR             1 ARIMA       Test  0.682 131.  0.861  149. 0.813 0.0132 
    ## 3 healthyR.ts          2 LM          Test  0.983 238.  0.765  148. 1.17  0.00410
    ## 4 healthyverse         1 ARIMA       Test  0.751 475.  0.966  113. 0.904 0.0233 
    ## 5 healthyR.ai          4 NNAR        Test  1.07   97.9 0.841  154. 1.47  0.0260 
    ## 6 TidyDensity          4 NNAR        Test  0.624 103.  0.738  142. 0.767 0.264  
    ## 7 tidyAML              1 ARIMA       Test  0.820 139.  0.945  130. 0.968 0.125

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
    ## 1 healthyR.data <tibble>     <tibble>     <split [1301|28]> <mdl_tm_t [1 × 5]>
    ## 2 healthyR      <tibble>     <tibble>     <split [1293|28]> <mdl_tm_t [1 × 5]>
    ## 3 healthyR.ts   <tibble>     <tibble>     <split [1239|28]> <mdl_tm_t [1 × 5]>
    ## 4 healthyverse  <tibble>     <tibble>     <split [1210|28]> <mdl_tm_t [1 × 5]>
    ## 5 healthyR.ai   <tibble>     <tibble>     <split [1036|28]> <mdl_tm_t [1 × 5]>
    ## 6 TidyDensity   <tibble>     <tibble>     <split [890|28]>  <mdl_tm_t [1 × 5]>
    ## 7 tidyAML       <tibble>     <tibble>     <split [506|28]>  <mdl_tm_t [1 × 5]>

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
