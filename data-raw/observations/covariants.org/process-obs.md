Aggregate case notification and sequence data for ECDC case studies
================

``` r
library(data.table)
library(jsonlite)
library(gh)
library(purrr)
```

    ## 
    ## Attaching package: 'purrr'

    ## The following object is masked from 'package:jsonlite':
    ## 
    ##     flatten

    ## The following object is masked from 'package:data.table':
    ## 
    ##     transpose

``` r
library(ggplot2)
library(here)
```

## Case notification data

  - Load truth data from the ECDC via the ECDC forecasting hub and
    process.

<!-- end list -->

``` r
cases <- fread("https://raw.githubusercontent.com/epiforecasts/covid19-forecast-hub-europe/main/data-truth/JHU/truth_JHU-Incident%20Cases.csv") # nolint

# Format date
cases[, date := as.Date(date)]

# Summarise to weekly cases starting on Saturday to Sync with the forecast hubs
cases[, cases := frollsum(value, n = 7)]

# Filter from the 1st of January and keep only Saturdays
cases <- cases[date >= as.Date("2021-01-01")]
cases <- cases[weekdays(date) %in% "Saturday"]

# Only most recent case data is available
cases[, cases_available := date]

# Drop unnecessary columns
set(cases, j = c("value"), value = NULL)

# Summary
summary(cases)
```

    ##    location         location_name           date                cases        cases_available     
    ##  Length:1184        Length:1184        Min.   :2021-01-02   Min.   :   703   Min.   :2021-01-02  
    ##  Class :character   Class :character   1st Qu.:2021-03-06   1st Qu.:  7568   1st Qu.:2021-03-06  
    ##  Mode  :character   Mode  :character   Median :2021-05-08   Median : 17761   Median :2021-05-08  
    ##                                        Mean   :2021-05-08   Mean   : 22682   Mean   :2021-05-08  
    ##                                        3rd Qu.:2021-07-10   3rd Qu.: 32887   3rd Qu.:2021-07-10  
    ##                                        Max.   :2021-09-11   Max.   :111257   Max.   :2021-09-11

# Sequence notification data

  - Define a function to download, load, and process sequence data from
    [covariants.org](https://covariants.org) (which itself process
    sequence data from [GISAID](https://www.gisaid.org)).

<!-- end list -->

``` r
download_covariants_sequences <- function(sha, path = "cluster_tables/21A.Delta_data.json") {
  if (missing(sha)) {
    url <- paste0(
      "https://raw.githubusercontent.com/hodcroftlab/covariants/master/", path
    )
  }else{
    url <- paste(
      "https://raw.githubusercontent.com/hodcroftlab/covariants",
       sha, path, sep = "/"
    )
  }
  sequences <- jsonlite::fromJSON(url)
  sequences <- purrr::map(sequences, as.data.table)
  sequences <- data.table::rbindlist(sequences, idcol = "location_name")
  return(sequences[])
}
```

  - Test by downloading the latest available data

<!-- end list -->

``` r
latest_sequences <- download_covariants_sequences()
latest_sequences
```

    ##             location_name       week total_sequences cluster_sequences
    ##    1:               India 2020-04-27              73                 0
    ##    2:               India 2020-05-04             170                 0
    ##    3:               India 2020-05-11             237                 0
    ##    4:               India 2020-05-18             288                 0
    ##    5:               India 2020-05-25             330                 0
    ##   ---                                                                 
    ## 4432: Trinidad and Tobago 2021-07-12               5                 0
    ## 4433: Trinidad and Tobago 2021-07-19               7                 0
    ## 4434: Trinidad and Tobago 2021-07-26              12                 0
    ## 4435: Trinidad and Tobago 2021-08-02              19                 0
    ## 4436: Trinidad and Tobago 2021-08-09              19                 0
    ##       unsmoothed_cluster_sequences unsmoothed_total_sequences
    ##    1:                            0                        250
    ##    2:                            0                        221
    ##    3:                            0                        280
    ##    4:                            0                        369
    ##    5:                            0                        357
    ##   ---                                                        
    ## 4432:                            0                          8
    ## 4433:                            0                          3
    ## 4434:                            1                         34
    ## 4435:                            1                         15
    ## 4436:                            0                         21

  - Get the commits to the target file in order to construct
    retrospective data

<!-- end list -->

``` r
covariants_file_commits <- function(
  path = "cluster_tables/21A.Delta_data.json"
) {
  commits <- gh::gh(
    "/repos/hodcroftlab/covariants/commits?path={path}",
    owner = "hodcroftlab",
    repo = "covariants",
    path = path
  )

  commits <- purrr::map(commits, 
    ~ data.table(date = as.Date(as.character(.$commit$committer$date)),
                 datetime = lubridate::as_datetime(
                   as.character(.$commit$committer$date)
                  ), 
                 author = .$commit$committer$name,
                 message = .$commit$message,
                 sha = .$sha)
  )
  commits <- data.table::rbindlist(commits)
  return(commits[])
}

delta_sequence_commits <- covariants_file_commits()
delta_sequence_commits
```

    ##           date            datetime        author                                         message
    ##  1: 2021-09-13 2021-09-13 23:21:26 Emma Hodcroft                                new data 13 sept
    ##  2: 2021-09-13 2021-09-13 22:23:18 Emma Hodcroft                     reverting to previous state
    ##  3: 2021-09-13 2021-09-13 21:25:16 Emma Hodcroft                                new data 13 Sept
    ##  4: 2021-09-09 2021-09-09 19:53:16 Emma Hodcroft                                 new data 9 sept
    ##  5: 2021-09-08 2021-09-08 09:09:40 Emma Hodcroft                                 new data 7 Sept
    ##  6: 2021-09-02 2021-09-02 20:56:47 Emma Hodcroft                                 new data 2 Sept
    ##  7: 2021-08-31 2021-08-31 17:23:10 Emma Hodcroft                                 new data 31 aug
    ##  8: 2021-08-27 2021-08-27 11:41:45 Emma Hodcroft                                 new data 26 Aug
    ##  9: 2021-08-24 2021-08-24 16:03:34 Emma Hodcroft                                 new data 24 Aug
    ## 10: 2021-08-19 2021-08-19 10:18:28 Emma Hodcroft                                 new data 18 aug
    ## 11: 2021-08-16 2021-08-16 13:42:49 Emma Hodcroft                                 new data 16 aug
    ## 12: 2021-08-12 2021-08-12 17:20:26 Emma Hodcroft                                 new data 12 Aug
    ## 13: 2021-08-07 2021-08-07 19:06:34 Emma Hodcroft                                  new data 9 Aug
    ## 14: 2021-08-07 2021-08-07 12:00:42 Emma Hodcroft                                  new data 6 aug
    ## 15: 2021-08-04 2021-08-04 12:40:46 Emma Hodcroft                                  new data 3 aug
    ## 16: 2021-07-29 2021-07-29 16:36:45 Emma Hodcroft                                new data 28 July
    ## 17: 2021-07-26 2021-07-26 19:10:57 Emma Hodcroft                                 new data 26 jul
    ## 18: 2021-07-23 2021-07-23 07:49:40 Emma Hodcroft                                 new data 22 Jul
    ## 19: 2021-07-20 2021-07-20 12:31:24 Emma Hodcroft                                 new data 19 jul
    ## 20: 2021-07-19 2021-07-19 17:30:03 Emma Hodcroft                                 new data 16 Jul
    ## 21: 2021-07-15 2021-07-15 13:45:20 Emma Hodcroft                                 new data 14 Jul
    ## 22: 2021-07-12 2021-07-12 16:09:10 Emma Hodcroft                    data replotted new var rules
    ## 23: 2021-07-12 2021-07-12 12:09:29 Emma Hodcroft                                  new data 9 jul
    ## 24: 2021-07-09 2021-07-09 10:23:51 Emma Hodcroft                                  new data 8 jul
    ## 25: 2021-07-06 2021-07-06 12:28:54 Emma Hodcroft                                 new data 6 july
    ## 26: 2021-06-30 2021-06-30 18:39:12 Emma Hodcroft                                 new data 29 Jun
    ## 27: 2021-06-28 2021-06-28 09:59:18 Emma Hodcroft                                 new data 26 jun
    ## 28: 2021-06-25 2021-06-25 16:42:37 Emma Hodcroft                                new data 24 June
    ## 29: 2021-06-24 2021-06-24 09:28:09 Emma Hodcroft new data generated using NextClade designations
    ## 30: 2021-06-23 2021-06-23 15:26:47 Emma Hodcroft                                new data 22 June
    ##           date            datetime        author                                         message
    ##                                          sha
    ##  1: 5f60ecf481dfb046ccf3dca5c86b780551f4458a
    ##  2: 8b4cb1438f41b8ed23f2f0bdd5c7012c8a0ffd40
    ##  3: 96fe12066eb71db9ab7679ee87a14d5735f8d0b4
    ##  4: c127f3ff4a9f8f208ed9064b4e9159d32a9b9818
    ##  5: ac958e9000ed08b7c6deb1622b9ffb47db7eae94
    ##  6: 0c7acdf40ca4ac3ae553bf412a988f085c68943a
    ##  7: 2430eb4e101c57505a630ddd894aa0c4b2ad70cf
    ##  8: 7cf11eec867fe928e1c5bd81f920a52cfe5b33ab
    ##  9: 4b6fa860c69ab47a723d5ce9f6f045a838710010
    ## 10: 351080f3f1589fd28ef9b422afce614a32265b4e
    ## 11: d71725a87033f93beb7fd24b8524a930a098c557
    ## 12: 129ae1643b1c4cbeb22e8d7544d4572b333220d9
    ## 13: 198da54a00d1ce80c9f1352b99a85771a6384470
    ## 14: ef6cbd29b7e428ff9b6549c8273b0b06151bc73f
    ## 15: cf043431414f7f67ff1256c6737b413ca13f460e
    ## 16: 0208e32e74c0bb6ddde5308a32454c5a07225777
    ## 17: 1de09fdb4c367caa00271ba50a734780237bbb02
    ## 18: 6b9722c2a1f215670f6624336d183175379eab4d
    ## 19: 3bcddc093bcf52eff245e4965d559d0e2970a78e
    ## 20: a022fda5d641f49b7ec0f3ed9107227a4a367d3c
    ## 21: 518ed2567557009307d0e9dbdfd1ef1646f175b1
    ## 22: 76a8638751df8add53cb0861dbdd3015e3730215
    ## 23: 1e9fb3051f7c02c8f994a9533e40daded3e30451
    ## 24: 4b8f686acb434b52e06f8035aab3e8bbbc0b7237
    ## 25: 3b7df3acae79ea0dee6afbfd8e673e799e14bbc0
    ## 26: 391f2da6da1d24eb32656c5f5b86f421d3cfc76f
    ## 27: 9469e2c735bb381e4d9bead9eac2f7e550ece97d
    ## 28: e07687cc89bd25013e780127ce493e81d509864e
    ## 29: 9e9ef20f25a41b44018d6c9607344eb48ceb4146
    ## 30: 7f58fae66e8e659a35eef80592d59e1af6c62802
    ##                                          sha

  - Keep only the last commits from any given day and download data from
    this commit.

<!-- end list -->

``` r
delta_sequences <- delta_sequence_commits[order(date)][,
   .SD[datetime == max(datetime)], by = date
]
setnames(delta_sequences, "date", "seq_available")
delta_sequences[, data := purrr::map(sha, download_covariants_sequences)]
delta_sequences <- delta_sequences[, rbindlist(data), by = seq_available]
delta_sequences
```

    ##        seq_available       location_name       week total_sequences cluster_sequences
    ##     1:    2021-06-23               India 2020-04-27              73                 0
    ##     2:    2021-06-23               India 2020-05-04             168                 0
    ##     3:    2021-06-23               India 2020-05-11             235                 0
    ##     4:    2021-06-23               India 2020-05-18             286                 0
    ##     5:    2021-06-23               India 2020-05-25             326                 0
    ##    ---                                                                               
    ## 76208:    2021-09-13 Trinidad and Tobago 2021-07-12               5                 0
    ## 76209:    2021-09-13 Trinidad and Tobago 2021-07-19               7                 0
    ## 76210:    2021-09-13 Trinidad and Tobago 2021-07-26              12                 0
    ## 76211:    2021-09-13 Trinidad and Tobago 2021-08-02              19                 0
    ## 76212:    2021-09-13 Trinidad and Tobago 2021-08-09              19                 0
    ##        unsmoothed_cluster_sequences unsmoothed_total_sequences
    ##     1:                            0                        248
    ##     2:                            0                        218
    ##     3:                            0                        278
    ##     4:                            0                        369
    ##     5:                            0                        357
    ##    ---                                                        
    ## 76208:                            0                          8
    ## 76209:                            0                          3
    ## 76210:                            1                         34
    ## 76211:                            1                         15
    ## 76212:                            0                         21

# Merge, explore, and save data
