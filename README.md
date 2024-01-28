Time Series Analysis and Nested Modeling of the Healthyverse Packages
================
Steven P. Sanderson II, MPH - Date:
28 January, 2024

This analysis follows a *Nested Modeltime Workflow*.

## Get Data

``` r
glimpse(downloads_tbl)
```

    ## Rows: 90,653
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

The last day in the data set is 2024-01-26 20:41:00, the file was
birthed on: 2022-07-02 23:58:17.511888, and at report knit time is
-1.374471^{4} hours old. Happy analyzing!

Now that we have our data lets take a look at it using the `skimr`
package.

``` r
skim(downloads_tbl)
```

|                                                  |               |
|:-------------------------------------------------|:--------------|
| Name                                             | downloads_tbl |
| Number of rows                                   | 90653         |
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
| r_version     |     62788 |          0.31 |   5 |   5 |     0 |       40 |          0 |
| r_arch        |     62788 |          0.31 |   3 |   7 |     0 |        5 |          0 |
| r_os          |     62788 |          0.31 |   7 |  15 |     0 |       17 |          0 |
| package       |         0 |          1.00 |   7 |  13 |     0 |        7 |          0 |
| version       |         0 |          1.00 |   5 |   6 |     0 |       50 |          0 |
| country       |      7447 |          0.92 |   2 |   2 |     0 |      151 |          0 |

**Variable type: Date**

| skim_variable | n_missing | complete_rate | min        | max        | median     | n_unique |
|:--------------|----------:|--------------:|:-----------|:-----------|:-----------|---------:|
| date          |         0 |             1 | 2020-11-23 | 2024-01-26 | 2022-08-25 |     1160 |

**Variable type: numeric**

| skim_variable | n_missing | complete_rate |       mean |         sd |  p0 |   p25 |    p50 |     p75 |    p100 | hist  |
|:--------------|----------:|--------------:|-----------:|-----------:|----:|------:|-------:|--------:|--------:|:------|
| size          |         0 |             1 | 1219315.28 | 1596493.16 | 355 | 14701 | 289912 | 2386674 | 5677952 | ▇▁▂▁▁ |
| ip_id         |         0 |             1 |   10240.22 |   18105.39 |   1 |   245 |   2972 |   11342 |  143633 | ▇▁▁▁▁ |

**Variable type: POSIXct**

| skim_variable | n_missing | complete_rate | min                 | max                 | median              | n_unique |
|:--------------|----------:|--------------:|:--------------------|:--------------------|:--------------------|---------:|
| date_time     |         0 |             1 | 2020-11-23 09:00:41 | 2024-01-26 20:41:00 | 2022-08-25 19:22:15 |    54501 |

**Variable type: Timespan**

| skim_variable | n_missing | complete_rate | min | max |     median | n_unique |
|:--------------|----------:|--------------:|----:|----:|-----------:|---------:|
| time          |         0 |             1 |   0 |  59 | 12H 4M 27S |       60 |

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
    ## 1 TidyDensity   <tibble [550 × 2]> <tibble [28 × 2]> <split [522|28]>
    ## 2 healthyR      <tibble [547 × 2]> <tibble [28 × 2]> <split [519|28]>
    ## 3 healthyR.ai   <tibble [547 × 2]> <tibble [28 × 2]> <split [519|28]>
    ## 4 healthyR.data <tibble [548 × 2]> <tibble [28 × 2]> <split [520|28]>
    ## 5 healthyR.ts   <tibble [543 × 2]> <tibble [28 × 2]> <split [515|28]>
    ## 6 healthyverse  <tibble [538 × 2]> <tibble [28 × 2]> <split [510|28]>
    ## 7 tidyAML       <tibble [345 × 2]> <tibble [28 × 2]> <split [317|28]>

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
nested_modeltime_tbl
```

    ## # Nested Modeltime Table
    ##   # A tibble: 7 × 5
    ##   package       .actual_data .future_data .splits          .modeltime_tables 
    ##   <fct>         <list>       <list>       <list>           <list>            
    ## 1 TidyDensity   <tibble>     <tibble>     <split [522|28]> <mdl_tm_t [4 × 5]>
    ## 2 healthyR      <tibble>     <tibble>     <split [519|28]> <mdl_tm_t [4 × 5]>
    ## 3 healthyR.ai   <tibble>     <tibble>     <split [519|28]> <mdl_tm_t [4 × 5]>
    ## 4 healthyR.data <tibble>     <tibble>     <split [520|28]> <mdl_tm_t [4 × 5]>
    ## 5 healthyR.ts   <tibble>     <tibble>     <split [515|28]> <mdl_tm_t [4 × 5]>
    ## 6 healthyverse  <tibble>     <tibble>     <split [510|28]> <mdl_tm_t [4 × 5]>
    ## 7 tidyAML       <tibble>     <tibble>     <split [317|28]> <mdl_tm_t [4 × 5]>

### Model Accuracy

``` r
nested_modeltime_tbl %>%
  extract_nested_test_accuracy() %>%
  knitr::kable()
```

| package       | .model_id | .model_desc | .type |       mae |       mape |      mase |     smape |     rmse |       rsq |
|:--------------|----------:|:------------|:------|----------:|-----------:|----------:|----------:|---------:|----------:|
| TidyDensity   |         1 | ARIMA       | Test  | 1.0685594 |  126.91558 | 1.1916705 | 112.22876 | 1.508652 | 0.0480264 |
| TidyDensity   |         2 | LM          | Test  | 1.0598530 |  153.66392 | 1.1819611 | 106.46321 | 1.473862 | 0.2845227 |
| TidyDensity   |         3 | EARTH       | Test  | 1.0955274 |  136.62158 | 1.2217455 | 113.98798 | 1.529134 | 0.2845227 |
| TidyDensity   |         4 | NNAR        | Test  | 1.1812058 |   99.57474 | 1.3172951 | 135.67154 | 1.659800 | 0.0238155 |
| healthyR      |         1 | ARIMA       | Test  | 0.9364903 |  144.17464 | 0.7170439 | 156.81986 | 1.434405 | 0.1808383 |
| healthyR      |         2 | LM          | Test  | 1.0274248 |  114.29013 | 0.7866698 | 189.42708 | 1.501767 | 0.1733152 |
| healthyR      |         3 | EARTH       | Test  | 1.0279064 |  193.53874 | 0.7870385 | 146.41660 | 1.540398 | 0.1733152 |
| healthyR      |         4 | NNAR        | Test  | 0.9042257 |  205.42070 | 0.6923397 | 132.33139 | 1.422522 | 0.1289518 |
| healthyR.ai   |         1 | ARIMA       | Test  | 0.8765685 |  378.51913 | 0.7259813 | 122.25924 | 1.403127 | 0.1247389 |
| healthyR.ai   |         2 | LM          | Test  | 0.9621254 |  375.79393 | 0.7968402 | 135.50880 | 1.455748 | 0.1275605 |
| healthyR.ai   |         3 | EARTH       | Test  | 0.9591247 |  376.25056 | 0.7943550 | 134.16017 | 1.454221 | 0.1275605 |
| healthyR.ai   |         4 | NNAR        | Test  | 0.8977922 |  315.75436 | 0.7435589 | 144.38860 | 1.368119 | 0.1100852 |
| healthyR.data |         1 | ARIMA       | Test  | 1.0653759 |  111.09154 | 0.7451320 | 153.16836 | 1.435176 | 0.0298517 |
| healthyR.data |         2 | LM          | Test  | 1.1183562 |  100.29596 | 0.7821868 | 197.02133 | 1.429695 | 0.1836677 |
| healthyR.data |         3 | EARTH       | Test  | 1.1450041 |  123.65575 | 0.8008245 | 144.54784 | 1.509103 | 0.1836677 |
| healthyR.data |         4 | NNAR        | Test  | 1.0552429 |   97.51147 | 0.7380448 | 151.51672 | 1.400047 | 0.1155934 |
| healthyR.ts   |         1 | ARIMA       | Test  | 1.2230226 |  123.42866 | 0.6805452 | 126.01127 | 1.749731 |        NA |
| healthyR.ts   |         2 | LM          | Test  | 1.2379371 |  131.48856 | 0.6888443 | 125.12855 | 1.746893 | 0.1745028 |
| healthyR.ts   |         3 | EARTH       | Test  | 1.2439010 |  133.95769 | 0.6921629 | 125.18552 | 1.746925 | 0.1745028 |
| healthyR.ts   |         4 | NNAR        | Test  | 1.1592786 |   91.81415 | 0.6450752 | 152.41630 | 1.780751 | 0.0113414 |
| healthyverse  |         1 | ARIMA       | Test  | 0.7617246 |  293.17289 | 0.7746259 |  85.43812 | 1.324944 | 0.1116313 |
| healthyverse  |         2 | LM          | Test  | 0.8379892 |  462.12334 | 0.8521822 |  86.56079 | 1.389460 | 0.2008277 |
| healthyverse  |         3 | EARTH       | Test  | 0.8334584 |  473.50838 | 0.8475747 |  85.47322 | 1.389593 | 0.2008277 |
| healthyverse  |         4 | NNAR        | Test  | 0.8887777 |  583.76275 | 0.9038309 |  90.29941 | 1.506514 | 0.0608706 |
| tidyAML       |         1 | ARIMA       | Test  | 1.0549560 |  393.83935 | 1.3662729 | 142.30307 | 1.509506 | 0.0681884 |
| tidyAML       |         2 | LM          | Test  | 1.1657461 |  801.06377 | 1.5097571 | 129.26484 | 1.671466 | 0.2591893 |
| tidyAML       |         3 | EARTH       | Test  | 2.4056941 | 1900.48999 | 3.1156128 | 149.37997 | 2.953960 | 0.2591893 |
| tidyAML       |         4 | NNAR        | Test  | 1.0948281 |  814.02831 | 1.4179111 | 131.76486 | 1.577756 | 0.0894924 |

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
    ##   package       .model_id .model_desc .type   mae  mape  mase smape  rmse    rsq
    ##   <fct>             <int> <chr>       <chr> <dbl> <dbl> <dbl> <dbl> <dbl>  <dbl>
    ## 1 TidyDensity           2 LM          Test  1.06  154.  1.18  106.   1.47 0.285 
    ## 2 healthyR              4 NNAR        Test  0.904 205.  0.692 132.   1.42 0.129 
    ## 3 healthyR.ai           4 NNAR        Test  0.898 316.  0.744 144.   1.37 0.110 
    ## 4 healthyR.data         4 NNAR        Test  1.06   97.5 0.738 152.   1.40 0.116 
    ## 5 healthyR.ts           2 LM          Test  1.24  131.  0.689 125.   1.75 0.175 
    ## 6 healthyverse          1 ARIMA       Test  0.762 293.  0.775  85.4  1.32 0.112 
    ## 7 tidyAML               1 ARIMA       Test  1.05  394.  1.37  142.   1.51 0.0682

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
    ## 1 TidyDensity   <tibble>     <tibble>     <split [522|28]> <mdl_tm_t [1 × 5]>
    ## 2 healthyR      <tibble>     <tibble>     <split [519|28]> <mdl_tm_t [1 × 5]>
    ## 3 healthyR.ai   <tibble>     <tibble>     <split [519|28]> <mdl_tm_t [1 × 5]>
    ## 4 healthyR.data <tibble>     <tibble>     <split [520|28]> <mdl_tm_t [1 × 5]>
    ## 5 healthyR.ts   <tibble>     <tibble>     <split [515|28]> <mdl_tm_t [1 × 5]>
    ## 6 healthyverse  <tibble>     <tibble>     <split [510|28]> <mdl_tm_t [1 × 5]>
    ## 7 tidyAML       <tibble>     <tibble>     <split [317|28]> <mdl_tm_t [1 × 5]>

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
