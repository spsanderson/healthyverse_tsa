Time Series Analysis and Nested Modeling of the Healthyverse Packages
================
Steven P. Sanderson II, MPH - Date:
05 June, 2023

This analysis follows a *Nested Modeltime Workflow*.

## Get Data

``` r
glimpse(downloads_tbl)
```

    ## Rows: 68,708
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

The last day in the data set is 2023-06-03 23:05:14, the file was
birthed on: 2022-07-02 23:58:17, and at report knit time is -8059.12
hours old. Happy analyzing!

Now that we have our data lets take a look at it using the `skimr`
package.

``` r
skim(downloads_tbl)
```

|                                                  |               |
|:-------------------------------------------------|:--------------|
| Name                                             | downloads_tbl |
| Number of rows                                   | 68708         |
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
| r_version     |     46875 |          0.32 |   5 |   5 |     0 |       37 |          0 |
| r_arch        |     46875 |          0.32 |   3 |   7 |     0 |        5 |          0 |
| r_os          |     46875 |          0.32 |   7 |  15 |     0 |       14 |          0 |
| package       |         0 |          1.00 |   7 |  13 |     0 |        7 |          0 |
| version       |         0 |          1.00 |   5 |   6 |     0 |       44 |          0 |
| country       |      5758 |          0.92 |   2 |   2 |     0 |      138 |          0 |

**Variable type: Date**

| skim_variable | n_missing | complete_rate | min        | max        | median     | n_unique |
|:--------------|----------:|--------------:|:-----------|:-----------|:-----------|---------:|
| date          |         0 |             1 | 2020-11-23 | 2023-06-03 | 2022-04-16 |      923 |

**Variable type: numeric**

| skim_variable | n_missing | complete_rate |      mean |         sd |  p0 |   p25 |    p50 |     p75 |    p100 | hist  |
|:--------------|----------:|--------------:|----------:|-----------:|----:|------:|-------:|--------:|--------:|:------|
| size          |         0 |             1 | 1302714.2 | 1657520.97 | 357 | 24837 | 322860 | 2689150 | 5677952 | ▇▁▂▂▁ |
| ip_id         |         0 |             1 |    9952.8 |   18170.09 |   1 |   132 |   2693 |   10911 |  143633 | ▇▁▁▁▁ |

**Variable type: POSIXct**

| skim_variable | n_missing | complete_rate | min                 | max                 | median              | n_unique |
|:--------------|----------:|--------------:|:--------------------|:--------------------|:--------------------|---------:|
| date_time     |         0 |             1 | 2020-11-23 09:00:41 | 2023-06-03 23:05:14 | 2022-04-16 13:46:44 |    41279 |

**Variable type: Timespan**

| skim_variable | n_missing | complete_rate | min | max | median | n_unique |
|:--------------|----------:|--------------:|----:|----:|-------:|---------:|
| time          |         0 |             1 |   0 |  59 |   54.5 |       60 |

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

    ## # A tibble: 7 × 4
    ##   package       .actual_data       .future_data      .splits         
    ##   <fct>         <list>             <list>            <list>          
    ## 1 healthyR      <tibble [545 × 2]> <tibble [28 × 2]> <split [517|28]>
    ## 2 healthyR.ai   <tibble [544 × 2]> <tibble [28 × 2]> <split [516|28]>
    ## 3 healthyR.data <tibble [543 × 2]> <tibble [28 × 2]> <split [515|28]>
    ## 4 healthyR.ts   <tibble [538 × 2]> <tibble [28 × 2]> <split [510|28]>
    ## 5 healthyverse  <tibble [535 × 2]> <tibble [28 × 2]> <split [507|28]>
    ## 6 TidyDensity   <tibble [499 × 2]> <tibble [28 × 2]> <split [471|28]>
    ## 7 tidyAML       <tibble [108 × 2]> <tibble [28 × 2]> <split [80|28]>

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
    ##   # A tibble: 7 × 5
    ##   package       .actual_data .future_data .splits          .modeltime_tables 
    ##   <fct>         <list>       <list>       <list>           <list>            
    ## 1 healthyR      <tibble>     <tibble>     <split [517|28]> <mdl_tm_t [5 × 5]>
    ## 2 healthyR.ai   <tibble>     <tibble>     <split [516|28]> <mdl_tm_t [5 × 5]>
    ## 3 healthyR.data <tibble>     <tibble>     <split [515|28]> <mdl_tm_t [5 × 5]>
    ## 4 healthyR.ts   <tibble>     <tibble>     <split [510|28]> <mdl_tm_t [5 × 5]>
    ## 5 healthyverse  <tibble>     <tibble>     <split [507|28]> <mdl_tm_t [5 × 5]>
    ## 6 TidyDensity   <tibble>     <tibble>     <split [471|28]> <mdl_tm_t [5 × 5]>
    ## 7 tidyAML       <tibble>     <tibble>     <split [80|28]>  <mdl_tm_t [5 × 5]>

### Model Accuracy

``` r
nested_modeltime_tbl %>%
  extract_nested_test_accuracy() %>%
  knitr::kable()
```

| package       | .model_id | .model_desc | .type |       mae |       mape |      mase |    smape |      rmse |       rsq |
|:--------------|----------:|:------------|:------|----------:|-----------:|----------:|---------:|----------:|----------:|
| healthyR      |         1 | ARIMA       | Test  | 0.7896430 |  103.42243 | 0.7479234 | 159.3971 | 0.9415638 | 0.0985919 |
| healthyR      |         2 | NULL        | NA    |        NA |         NA |        NA |       NA |        NA |        NA |
| healthyR      |         3 | LM          | Test  | 0.8156944 |   98.62933 | 0.7725985 | 170.0875 | 0.9943271 | 0.0147070 |
| healthyR      |         4 | EARTH       | Test  | 1.1957074 |  191.21543 | 1.1325341 | 140.7668 | 1.4664671 | 0.0147070 |
| healthyR      |         5 | NNAR        | Test  | 0.7919550 |  105.60151 | 0.7501133 | 155.3480 | 0.9323907 | 0.1033094 |
| healthyR.ai   |         1 | ARIMA       | Test  | 0.8696976 |  124.58286 | 0.8240508 | 170.2389 | 1.0340005 | 0.0001001 |
| healthyR.ai   |         2 | NULL        | NA    |        NA |         NA |        NA |       NA |        NA |        NA |
| healthyR.ai   |         3 | LM          | Test  | 0.8778700 |  129.27463 | 0.8317943 | 154.8169 | 1.0534781 | 0.0309813 |
| healthyR.ai   |         4 | EARTH       | Test  | 0.8828527 |  138.80975 | 0.8365155 | 153.3289 | 1.0569468 | 0.0309813 |
| healthyR.ai   |         5 | NNAR        | Test  | 0.8384319 |  131.08568 | 0.7944262 | 146.0753 | 1.0210127 | 0.0097784 |
| healthyR.data |         1 | ARIMA       | Test  | 0.8706611 |  187.60707 | 0.8099675 | 167.3995 | 1.0809006 | 0.0000178 |
| healthyR.data |         2 | NULL        | NA    |        NA |         NA |        NA |       NA |        NA |        NA |
| healthyR.data |         3 | LM          | Test  | 0.9868595 |  272.10786 | 0.9180658 | 148.2592 | 1.2407374 | 0.2108452 |
| healthyR.data |         4 | EARTH       | Test  | 0.8522788 |  140.18871 | 0.7928666 | 156.8953 | 1.0633977 | 0.2108452 |
| healthyR.data |         5 | NNAR        | Test  | 0.8917130 |  188.27448 | 0.8295519 | 151.6098 | 1.1723151 | 0.0112790 |
| healthyR.ts   |         1 | ARIMA       | Test  | 0.6831768 |  108.52618 | 0.5493429 | 126.6913 | 0.8304577 | 0.5980076 |
| healthyR.ts   |         2 | NULL        | NA    |        NA |         NA |        NA |       NA |        NA |        NA |
| healthyR.ts   |         3 | LM          | Test  | 0.8791325 |  169.63527 | 0.7069110 | 136.5163 | 1.0900772 | 0.0331369 |
| healthyR.ts   |         4 | EARTH       | Test  | 3.0536696 | 1238.96405 | 2.4554577 | 150.7588 | 3.4948480 | 0.0331369 |
| healthyR.ts   |         5 | NNAR        | Test  | 0.8226358 |  101.76750 | 0.6614819 | 163.3593 | 1.0587540 | 0.0926415 |
| healthyverse  |         1 | ARIMA       | Test  | 0.6734636 |  171.92116 | 0.9459492 | 145.7179 | 0.8050338 | 0.1273544 |
| healthyverse  |         2 | NULL        | NA    |        NA |         NA |        NA |       NA |        NA |        NA |
| healthyverse  |         3 | LM          | Test  | 0.7084661 |  335.41910 | 0.9951139 | 122.5006 | 0.8188062 | 0.0950410 |
| healthyverse  |         4 | EARTH       | Test  | 1.4553448 |  965.33990 | 2.0441821 | 129.3571 | 1.6731788 | 0.0950410 |
| healthyverse  |         5 | NNAR        | Test  | 0.6628341 |  158.50105 | 0.9310190 | 135.2256 | 0.8021702 | 0.0088138 |
| TidyDensity   |         1 | ARIMA       | Test  | 0.9339446 |  127.54388 | 0.9374644 | 135.1682 | 1.2095956 | 0.0075482 |
| TidyDensity   |         2 | NULL        | NA    |        NA |         NA |        NA |       NA |        NA |        NA |
| TidyDensity   |         3 | LM          | Test  | 0.9356711 |  134.98359 | 0.9391974 | 135.0507 | 1.2004736 | 0.0255544 |
| TidyDensity   |         4 | EARTH       | Test  | 2.8736509 |  720.17307 | 2.8844811 | 187.4857 | 3.2256199 | 0.0255544 |
| TidyDensity   |         5 | NNAR        | Test  | 1.0126091 |  148.89741 | 1.0164254 | 138.4282 | 1.3314577 | 0.0037193 |
| tidyAML       |         1 | ARIMA       | Test  | 0.6320965 |   99.53640 | 0.9737825 | 181.4013 | 0.8701115 | 0.0729488 |
| tidyAML       |         2 | NULL        | NA    |        NA |         NA |        NA |       NA |        NA |        NA |
| tidyAML       |         3 | LM          | Test  | 0.6982533 |  144.97680 | 1.0757010 | 152.4742 | 0.9638596 | 0.0726147 |
| tidyAML       |         4 | EARTH       | Test  | 3.7353528 | 1963.85445 | 5.7545344 | 162.1433 | 4.2287476 | 0.0726147 |
| tidyAML       |         5 | NNAR        | Test  | 0.9235964 |  346.48761 | 1.4228554 | 140.6930 | 1.3117021 | 0.0040715 |

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
    ## 1 healthyR             5 NNAR        Test  0.792 106.  0.750  155. 0.932 0.103  
    ## 2 healthyR.ai          5 NNAR        Test  0.838 131.  0.794  146. 1.02  0.00978
    ## 3 healthyR.da…         4 EARTH       Test  0.852 140.  0.793  157. 1.06  0.211  
    ## 4 healthyR.ts          1 ARIMA       Test  0.683 109.  0.549  127. 0.830 0.598  
    ## 5 healthyverse         5 NNAR        Test  0.663 159.  0.931  135. 0.802 0.00881
    ## 6 TidyDensity          3 LM          Test  0.936 135.  0.939  135. 1.20  0.0256 
    ## 7 tidyAML              1 ARIMA       Test  0.632  99.5 0.974  181. 0.870 0.0729

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
    ##   package       .actual_data .future_data .splits          .modeltime_tables 
    ##   <fct>         <list>       <list>       <list>           <list>            
    ## 1 healthyR      <tibble>     <tibble>     <split [517|28]> <mdl_tm_t [1 × 5]>
    ## 2 healthyR.ai   <tibble>     <tibble>     <split [516|28]> <mdl_tm_t [1 × 5]>
    ## 3 healthyR.data <tibble>     <tibble>     <split [515|28]> <mdl_tm_t [1 × 5]>
    ## 4 healthyR.ts   <tibble>     <tibble>     <split [510|28]> <mdl_tm_t [1 × 5]>
    ## 5 healthyverse  <tibble>     <tibble>     <split [507|28]> <mdl_tm_t [1 × 5]>
    ## 6 TidyDensity   <tibble>     <tibble>     <split [471|28]> <mdl_tm_t [1 × 5]>
    ## 7 tidyAML       <tibble>     <tibble>     <split [80|28]>  <mdl_tm_t [1 × 5]>

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
