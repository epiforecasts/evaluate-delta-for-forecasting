Aggregate case notification and sequence data for ECDC case studies
================

``` r
library(data.table)
library(jsonlite)
library(gh)
library(purrr)
library(ggplot2)
library(here)
library(covidregionaldata)
```

## Case notification data

  - Load truth data from the ECDC via the ECDC forecasting hub and
    process.

<!-- end list -->

``` r
cases <- fread("https://raw.githubusercontent.com/epiforecasts/covid19-forecast-hub-europe/main/data-truth/JHU/truth_JHU-Incident%20Cases.csv") # nolint

# Format date
cases[, date := as.Date(date)]

# Order data by date and location
setkey(cases, location_name, date)

# Summarise to weekly cases starting on Saturday to Sync with the forecast hubs
cases[, cases := frollsum(value, n = 7), by = c("location_name")]

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

    ##    location         location_name           date           
    ##  Length:1280        Length:1280        Min.   :2021-01-02  
    ##  Class :character   Class :character   1st Qu.:2021-03-11  
    ##  Mode  :character   Mode  :character   Median :2021-05-18  
    ##                                        Mean   :2021-05-18  
    ##                                        3rd Qu.:2021-07-25  
    ##                                        Max.   :2021-10-02  
    ##      cases         cases_available     
    ##  Min.   :-272773   Min.   :2021-01-02  
    ##  1st Qu.:   1575   1st Qu.:2021-03-11  
    ##  Median :   5587   Median :2021-05-18  
    ##  Mean   :  22716   Mean   :2021-05-18  
    ##  3rd Qu.:  18606   3rd Qu.:2021-07-25  
    ##  Max.   : 417620   Max.   :2021-10-02

## Sequence notification data

  - Define a function to download, load, and process sequence data from
    [covariants.org](https://covariants.org) (which itself process
    sequence data from [GISAID](https://www.gisaid.org)).

<!-- end list -->

``` r
download_covariants_sequences <- function(sha, path = "cluster_tables/21A.Delta_data.json") { # nolint
  if (missing(sha)) {
    url <- paste0(
      "https://raw.githubusercontent.com/hodcroftlab/covariants/master/", path
    )
  } else {
    url <- paste(
      "https://raw.githubusercontent.com/hodcroftlab/covariants",
      sha, path,
      sep = "/"
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
    ## 4611: Trinidad and Tobago 2021-07-12               5                 0
    ## 4612: Trinidad and Tobago 2021-07-19               7                 0
    ## 4613: Trinidad and Tobago 2021-07-26              12                 0
    ## 4614: Trinidad and Tobago 2021-08-02              19                 0
    ## 4615: Trinidad and Tobago 2021-08-09              19                 0
    ##       unsmoothed_cluster_sequences unsmoothed_total_sequences
    ##    1:                            0                        250
    ##    2:                            0                        221
    ##    3:                            0                        280
    ##    4:                            0                        369
    ##    5:                            0                        357
    ##   ---                                                        
    ## 4611:                            0                          8
    ## 4612:                            0                          3
    ## 4613:                            1                         34
    ## 4614:                            1                         15
    ## 4615:                            0                         21

  - Get the commits to the target file in order to construct
    retrospective data

<!-- end list -->

``` r
covariants_file_commits <- function(path = "cluster_tables/21A.Delta_data.json") { # nolint
  commits <- gh::gh(
    "/repos/hodcroftlab/covariants/commits?path={path}",
    owner = "hodcroftlab",
    repo = "covariants",
    path = path
  )

  commits <- purrr::map(
    commits,
    ~ data.table(
      date = as.Date(as.character(.$commit$committer$date)),
      datetime = lubridate::as_datetime(
        as.character(.$commit$committer$date)
      ),
      author = .$commit$committer$name,
      message = .$commit$message,
      sha = .$sha
    )
  )
  commits <- data.table::rbindlist(commits)
  return(commits[])
}

delta_sequence_commits <- covariants_file_commits()
delta_sequence_commits
```

    ##           date            datetime        author
    ##  1: 2021-10-06 2021-10-06 12:14:54 Emma Hodcroft
    ##  2: 2021-10-01 2021-10-01 20:20:21 Emma Hodcroft
    ##  3: 2021-09-25 2021-09-25 09:54:57 Emma Hodcroft
    ##  4: 2021-09-23 2021-09-23 13:07:51 Emma Hodcroft
    ##  5: 2021-09-16 2021-09-16 18:27:24 Emma Hodcroft
    ##  6: 2021-09-13 2021-09-13 23:21:26 Emma Hodcroft
    ##  7: 2021-09-13 2021-09-13 22:23:18 Emma Hodcroft
    ##  8: 2021-09-13 2021-09-13 21:25:16 Emma Hodcroft
    ##  9: 2021-09-09 2021-09-09 19:53:16 Emma Hodcroft
    ## 10: 2021-09-08 2021-09-08 09:09:40 Emma Hodcroft
    ## 11: 2021-09-02 2021-09-02 20:56:47 Emma Hodcroft
    ## 12: 2021-08-31 2021-08-31 17:23:10 Emma Hodcroft
    ## 13: 2021-08-27 2021-08-27 11:41:45 Emma Hodcroft
    ## 14: 2021-08-24 2021-08-24 16:03:34 Emma Hodcroft
    ## 15: 2021-08-19 2021-08-19 10:18:28 Emma Hodcroft
    ## 16: 2021-08-16 2021-08-16 13:42:49 Emma Hodcroft
    ## 17: 2021-08-12 2021-08-12 17:20:26 Emma Hodcroft
    ## 18: 2021-08-07 2021-08-07 19:06:34 Emma Hodcroft
    ## 19: 2021-08-07 2021-08-07 12:00:42 Emma Hodcroft
    ## 20: 2021-08-04 2021-08-04 12:40:46 Emma Hodcroft
    ## 21: 2021-07-29 2021-07-29 16:36:45 Emma Hodcroft
    ## 22: 2021-07-26 2021-07-26 19:10:57 Emma Hodcroft
    ## 23: 2021-07-23 2021-07-23 07:49:40 Emma Hodcroft
    ## 24: 2021-07-20 2021-07-20 12:31:24 Emma Hodcroft
    ## 25: 2021-07-19 2021-07-19 17:30:03 Emma Hodcroft
    ## 26: 2021-07-15 2021-07-15 13:45:20 Emma Hodcroft
    ## 27: 2021-07-12 2021-07-12 16:09:10 Emma Hodcroft
    ## 28: 2021-07-12 2021-07-12 12:09:29 Emma Hodcroft
    ## 29: 2021-07-09 2021-07-09 10:23:51 Emma Hodcroft
    ## 30: 2021-07-06 2021-07-06 12:28:54 Emma Hodcroft
    ##           date            datetime        author
    ##                          message                                      sha
    ##  1:               new data 5 oct a24581f2f6cab04a499f6d892a9a25081010c6d0
    ##  2:             new data 30 sept 50744ec6940d1a1a7c8c5eb9024a708de262027e
    ##  3:             new data 27 sept e2e460eb2ba7ebb50452dd50660611839b4e5aa5
    ##  4:             new data 22 sept 5d48b9537052b0d51bef557ea9cc74da15e40a9e
    ##  5:             new data 16 Sept aad7732893fe58464206ef5dac91c9e92324586b
    ##  6:             new data 13 sept 5f60ecf481dfb046ccf3dca5c86b780551f4458a
    ##  7:  reverting to previous state 8b4cb1438f41b8ed23f2f0bdd5c7012c8a0ffd40
    ##  8:             new data 13 Sept 96fe12066eb71db9ab7679ee87a14d5735f8d0b4
    ##  9:              new data 9 sept c127f3ff4a9f8f208ed9064b4e9159d32a9b9818
    ## 10:              new data 7 Sept ac958e9000ed08b7c6deb1622b9ffb47db7eae94
    ## 11:              new data 2 Sept 0c7acdf40ca4ac3ae553bf412a988f085c68943a
    ## 12:              new data 31 aug 2430eb4e101c57505a630ddd894aa0c4b2ad70cf
    ## 13:              new data 26 Aug 7cf11eec867fe928e1c5bd81f920a52cfe5b33ab
    ## 14:              new data 24 Aug 4b6fa860c69ab47a723d5ce9f6f045a838710010
    ## 15:              new data 18 aug 351080f3f1589fd28ef9b422afce614a32265b4e
    ## 16:              new data 16 aug d71725a87033f93beb7fd24b8524a930a098c557
    ## 17:              new data 12 Aug 129ae1643b1c4cbeb22e8d7544d4572b333220d9
    ## 18:               new data 9 Aug 198da54a00d1ce80c9f1352b99a85771a6384470
    ## 19:               new data 6 aug ef6cbd29b7e428ff9b6549c8273b0b06151bc73f
    ## 20:               new data 3 aug cf043431414f7f67ff1256c6737b413ca13f460e
    ## 21:             new data 28 July 0208e32e74c0bb6ddde5308a32454c5a07225777
    ## 22:              new data 26 jul 1de09fdb4c367caa00271ba50a734780237bbb02
    ## 23:              new data 22 Jul 6b9722c2a1f215670f6624336d183175379eab4d
    ## 24:              new data 19 jul 3bcddc093bcf52eff245e4965d559d0e2970a78e
    ## 25:              new data 16 Jul a022fda5d641f49b7ec0f3ed9107227a4a367d3c
    ## 26:              new data 14 Jul 518ed2567557009307d0e9dbdfd1ef1646f175b1
    ## 27: data replotted new var rules 76a8638751df8add53cb0861dbdd3015e3730215
    ## 28:               new data 9 jul 1e9fb3051f7c02c8f994a9533e40daded3e30451
    ## 29:               new data 8 jul 4b8f686acb434b52e06f8035aab3e8bbbc0b7237
    ## 30:              new data 6 july 3b7df3acae79ea0dee6afbfd8e673e799e14bbc0
    ##                          message                                      sha

  - Keep only the last commits from any given day and download data from
    this commit.

<!-- end list -->

``` r
sequences <- delta_sequence_commits[order(date)][,
  .SD[datetime == max(datetime)],
  by = date
]
setnames(sequences, "date", "seq_available")
sequences[, data := purrr::map(sha, download_covariants_sequences)]
sequences <- sequences[, rbindlist(data), by = seq_available]
sequences
```

    ##        seq_available       location_name       week total_sequences
    ##     1:    2021-07-06               India 2020-04-27              73
    ##     2:    2021-07-06               India 2020-05-04             169
    ##     3:    2021-07-06               India 2020-05-11             237
    ##     4:    2021-07-06               India 2020-05-18             287
    ##     5:    2021-07-06               India 2020-05-25             327
    ##    ---                                                             
    ## 95677:    2021-10-06 Trinidad and Tobago 2021-07-12               5
    ## 95678:    2021-10-06 Trinidad and Tobago 2021-07-19               7
    ## 95679:    2021-10-06 Trinidad and Tobago 2021-07-26              12
    ## 95680:    2021-10-06 Trinidad and Tobago 2021-08-02              19
    ## 95681:    2021-10-06 Trinidad and Tobago 2021-08-09              19
    ##        cluster_sequences unsmoothed_cluster_sequences
    ##     1:                 0                            0
    ##     2:                 0                            0
    ##     3:                 0                            0
    ##     4:                 0                            0
    ##     5:                 0                            0
    ##    ---                                               
    ## 95677:                 0                            0
    ## 95678:                 0                            0
    ## 95679:                 0                            1
    ## 95680:                 0                            1
    ## 95681:                 0                            0
    ##        unsmoothed_total_sequences
    ##     1:                        250
    ##     2:                        221
    ##     3:                        279
    ##     4:                        369
    ##     5:                        357
    ##    ---                           
    ## 95677:                          8
    ## 95678:                          3
    ## 95679:                         34
    ## 95680:                         15
    ## 95681:                         21

  - Select and rename variables of interest.

<!-- end list -->

``` r
sequences <- sequences[
  ,
  .(
    seq_available = seq_available,
    location_name,
    week_starting = as.Date(week),
    week_ending = as.Date(week) + 6,
    seq_voc = unsmoothed_cluster_sequences,
    seq_total = unsmoothed_total_sequences
  )
][, share_voc := seq_voc / seq_total][]
sequences
```

    ##        seq_available       location_name week_starting week_ending seq_voc
    ##     1:    2021-07-06               India    2020-04-27  2020-05-03       0
    ##     2:    2021-07-06               India    2020-05-04  2020-05-10       0
    ##     3:    2021-07-06               India    2020-05-11  2020-05-17       0
    ##     4:    2021-07-06               India    2020-05-18  2020-05-24       0
    ##     5:    2021-07-06               India    2020-05-25  2020-05-31       0
    ##    ---                                                                    
    ## 95677:    2021-10-06 Trinidad and Tobago    2021-07-12  2021-07-18       0
    ## 95678:    2021-10-06 Trinidad and Tobago    2021-07-19  2021-07-25       0
    ## 95679:    2021-10-06 Trinidad and Tobago    2021-07-26  2021-08-01       1
    ## 95680:    2021-10-06 Trinidad and Tobago    2021-08-02  2021-08-08       1
    ## 95681:    2021-10-06 Trinidad and Tobago    2021-08-09  2021-08-15       0
    ##        seq_total  share_voc
    ##     1:       250 0.00000000
    ##     2:       221 0.00000000
    ##     3:       279 0.00000000
    ##     4:       369 0.00000000
    ##     5:       357 0.00000000
    ##    ---                     
    ## 95677:         8 0.00000000
    ## 95678:         3 0.00000000
    ## 95679:        34 0.02941176
    ## 95680:        15 0.06666667
    ## 95681:        21 0.00000000

## Filter, merge, and save cases notifications and sequences

  - Filter sequences to include dates by location starting from when two
    consecutive weeks have at least two sequences that were positive for
    the Delta variant and the Delta variant makes up at least 0.001% of
    available sequences. This filtering step is based on the first
    available data and then used as a common starting point across all
    releases. First find the introduction week for each location based
    on these filtering steps.

<!-- end list -->

``` r
first_seq <- sequences[,
  .SD[seq_available == min(seq_available)][
    seq_voc >= 2 & shift(seq_voc, type = "lead") >= 2
  ][
    share_voc >= 0.001 & shift(share_voc, type = "lead") >= 0.001
  ][
    order(week_ending)
  ][1, ],
  by = "location_name"
][!is.na(seq_voc)][]
first_seq
```

    ##      location_name seq_available week_starting week_ending seq_voc seq_total
    ##  1:          India    2021-07-06    2020-11-16  2020-11-22       3        67
    ##  2:      Indonesia    2021-07-06    2021-01-04  2021-01-10       2       175
    ##  3:      Singapore    2021-07-06    2021-03-29  2021-04-04       2       107
    ##  4: United Kingdom    2021-07-06    2021-03-29  2021-04-04      22     12160
    ##  5:    Netherlands    2021-07-06    2021-04-12  2021-04-18       4      1774
    ##  6:   South Africa    2021-07-06    2021-03-08  2021-03-14       6       243
    ##  7:            USA    2021-07-06    2021-04-05  2021-04-11      56     36346
    ##  8:      Australia    2021-07-06    2021-04-12  2021-04-18      26        94
    ##  9:        Germany    2021-07-06    2021-04-05  2021-04-11       9      8280
    ## 10:         Canada    2021-07-06    2021-04-05  2021-04-11      13      2357
    ## 11:          Japan    2021-07-06    2021-04-05  2021-04-11       7      2358
    ## 12:        Denmark    2021-07-06    2021-04-19  2021-04-25      12      3560
    ## 13:         Sweden    2021-07-06    2021-04-05  2021-04-11       3      2413
    ## 14:          Italy    2021-07-06    2021-04-12  2021-04-18       3      1666
    ## 15:        Belgium    2021-07-06    2021-04-05  2021-04-11       3      1270
    ## 16:    Switzerland    2021-07-06    2021-04-05  2021-04-11       3      1445
    ## 17:        Ireland    2021-07-06    2021-04-05  2021-04-11       5       697
    ## 18:         Russia    2021-07-06    2021-04-19  2021-04-25       5       157
    ## 19:         France    2021-07-06    2021-04-12  2021-04-18       3      2977
    ## 20:        Austria    2021-07-06    2021-05-17  2021-05-23       2       234
    ## 21:       Portugal    2021-07-06    2021-04-26  2021-05-02       3        44
    ## 22:         Mexico    2021-07-06    2021-05-03  2021-05-09      12       824
    ## 23:          Spain    2021-07-06    2021-04-19  2021-04-25       3      1078
    ## 24:         Israel    2021-07-09    2021-04-05  2021-04-11      26       187
    ## 25:        Romania    2021-07-12    2021-04-26  2021-05-02       2        13
    ## 26:          Kenya    2021-07-12    2021-04-26  2021-05-02       9        68
    ## 27:      Sri Lanka    2021-07-12    2021-06-07  2021-06-13       4        36
    ## 28:     Luxembourg    2021-07-12    2021-05-03  2021-05-09       4       333
    ## 29:         Norway    2021-07-12    2021-04-19  2021-04-25       2       458
    ## 30:          Qatar    2021-07-12    2021-04-12  2021-04-18       3        91
    ## 31:        Finland    2021-07-12    2021-04-26  2021-05-02      15       240
    ## 32:         Poland    2021-07-12    2021-04-26  2021-05-02      15      1214
    ## 33: Czech Republic    2021-07-12    2021-05-03  2021-05-09      11       133
    ## 34:     Bangladesh    2021-07-12    2021-04-26  2021-05-02      10        23
    ## 35:       Malaysia    2021-07-12    2021-04-26  2021-05-02       3        33
    ## 36:       Botswana    2021-07-12    2021-05-24  2021-05-30       2         8
    ## 37:       Thailand    2021-07-12    2021-05-10  2021-05-16      52       303
    ## 38:         Latvia    2021-07-12    2021-05-10  2021-05-16       3       125
    ## 39:       Cambodia    2021-07-12    2021-05-24  2021-05-30       3        24
    ## 40:      Lithuania    2021-07-12    2021-05-24  2021-05-30       2       706
    ## 41:         Uganda    2021-08-04    2021-05-24  2021-05-30       4         4
    ## 42:        Lebanon    2021-08-16    2021-06-21  2021-06-27       4         4
    ## 43:    New Zealand    2021-08-24    2021-03-29  2021-04-04       3        18
    ## 44:        Estonia    2021-08-24    2021-06-21  2021-06-27       2         5
    ## 45:        Bahrain    2021-08-27    2021-05-03  2021-05-09       3        12
    ## 46:     Costa Rica    2021-08-27    2021-06-14  2021-06-20       2        26
    ## 47:     Mozambique    2021-08-27    2021-06-28  2021-07-04      13        13
    ## 48:       Zimbabwe    2021-08-31    2021-06-21  2021-06-27       2         3
    ## 49:       Pakistan    2021-09-02    2021-05-17  2021-05-23       7        13
    ## 50:        Iceland    2021-09-08    2021-06-14  2021-06-20       8        16
    ## 51:      Hong Kong    2021-09-25    2021-04-05  2021-04-11       2        34
    ## 52:         Kosovo    2021-10-01    2021-07-05  2021-07-11       2         2
    ## 53:         Zambia    2021-10-01    2021-05-24  2021-05-30      85        85
    ## 54:     Guadeloupe    2021-10-01    2021-07-19  2021-07-25      24        34
    ## 55:        Bonaire    2021-10-01    2021-07-05  2021-07-11       3         6
    ##      location_name seq_available week_starting week_ending seq_voc seq_total
    ##       share_voc
    ##  1: 0.044776119
    ##  2: 0.011428571
    ##  3: 0.018691589
    ##  4: 0.001809211
    ##  5: 0.002254791
    ##  6: 0.024691358
    ##  7: 0.001540747
    ##  8: 0.276595745
    ##  9: 0.001086957
    ## 10: 0.005515486
    ## 11: 0.002968617
    ## 12: 0.003370787
    ## 13: 0.001243266
    ## 14: 0.001800720
    ## 15: 0.002362205
    ## 16: 0.002076125
    ## 17: 0.007173601
    ## 18: 0.031847134
    ## 19: 0.001007726
    ## 20: 0.008547009
    ## 21: 0.068181818
    ## 22: 0.014563107
    ## 23: 0.002782931
    ## 24: 0.139037433
    ## 25: 0.153846154
    ## 26: 0.132352941
    ## 27: 0.111111111
    ## 28: 0.012012012
    ## 29: 0.004366812
    ## 30: 0.032967033
    ## 31: 0.062500000
    ## 32: 0.012355848
    ## 33: 0.082706767
    ## 34: 0.434782609
    ## 35: 0.090909091
    ## 36: 0.250000000
    ## 37: 0.171617162
    ## 38: 0.024000000
    ## 39: 0.125000000
    ## 40: 0.002832861
    ## 41: 1.000000000
    ## 42: 1.000000000
    ## 43: 0.166666667
    ## 44: 0.400000000
    ## 45: 0.250000000
    ## 46: 0.076923077
    ## 47: 1.000000000
    ## 48: 0.666666667
    ## 49: 0.538461538
    ## 50: 0.500000000
    ## 51: 0.058823529
    ## 52: 1.000000000
    ## 53: 1.000000000
    ## 54: 0.705882353
    ## 55: 0.500000000
    ##       share_voc

  - Restrict included sequences to start from these introduction dates.

<!-- end list -->

``` r
filt_sequences <- merge(
  sequences, first_seq[, .(location_name, intro_date = week_ending)],
  by = "location_name"
)
filt_sequences <- filt_sequences[week_ending >= intro_date][
  ,
  intro_date := NULL
][]
```

  - Find the week in each location when 99% of sequences are positive
    for Delta in the final sequence data for two consequetive weeks and
    more than 10 sequences are available.

<!-- end list -->

``` r
last_seq <- sequences[,
  .SD[seq_total > 10][
    share_voc >= 0.99 & shift(share_voc, type = "lead") >= 0.99
  ][
    order(week_ending)
  ][1, ],
  by = "location_name"
][!is.na(seq_voc)][]
last_seq
```

    ##       location_name seq_available week_starting week_ending seq_voc
    ##  1:           India    2021-08-07    2021-07-05  2021-07-11     281
    ##  2:       Indonesia    2021-08-16    2021-07-26  2021-08-01      52
    ##  3:       Singapore    2021-07-29    2021-07-12  2021-07-18     340
    ##  4:  United Kingdom    2021-07-15    2021-06-28  2021-07-04   21794
    ##  5:     Netherlands    2021-08-31    2021-08-02  2021-08-08     973
    ##  6:    South Africa    2021-08-27    2021-08-02  2021-08-08     171
    ##  7:             USA    2021-08-31    2021-08-09  2021-08-15   23491
    ##  8:       Australia    2021-08-07    2021-07-12  2021-07-18     468
    ##  9:         Germany    2021-08-27    2021-08-09  2021-08-15    1292
    ## 10:          Canada    2021-09-25    2021-08-16  2021-08-22    2494
    ## 11:           Japan    2021-09-23    2021-08-30  2021-09-05      24
    ## 12:         Denmark    2021-08-07    2021-07-26  2021-08-01    4591
    ## 13:          Sweden    2021-09-02    2021-07-26  2021-08-01    1831
    ## 14:           Italy    2021-08-31    2021-08-09  2021-08-15     875
    ## 15:         Belgium    2021-08-24    2021-08-09  2021-08-15     572
    ## 16:     Switzerland    2021-08-31    2021-08-09  2021-08-15     963
    ## 17:         Ireland    2021-09-02    2021-08-02  2021-08-08     885
    ## 18:          Russia    2021-07-29    2021-06-28  2021-07-04      75
    ## 19:          France    2021-09-25    2021-08-23  2021-08-29    2861
    ## 20:         Austria    2021-09-13    2021-08-02  2021-08-08     228
    ## 21:        Portugal    2021-08-19    2021-07-26  2021-08-01     448
    ## 22:          Mexico    2021-09-13    2021-08-23  2021-08-29     350
    ## 23:           Spain    2021-09-08    2021-08-23  2021-08-29      86
    ## 24:          Israel    2021-09-08    2021-07-19  2021-07-25     200
    ## 25:         Romania    2021-09-16    2021-08-23  2021-08-29     109
    ## 26:           Kenya    2021-09-08    2021-07-12  2021-07-18      73
    ## 27:     South Korea    2021-09-08    2021-08-09  2021-08-15     168
    ## 28:          Norway    2021-08-31    2021-08-09  2021-08-15      85
    ## 29:           Aruba    2021-08-31    2021-07-26  2021-08-01      46
    ## 30:         Finland    2021-09-25    2021-08-02  2021-08-08     656
    ## 31:        Slovenia    2021-08-16    2021-07-19  2021-07-25     187
    ## 32:          Malawi    2021-10-01    2021-07-12  2021-07-18      12
    ## 33:          Poland    2021-09-16    2021-08-09  2021-08-15     101
    ## 34:  Czech Republic    2021-08-16    2021-07-26  2021-08-01      48
    ## 35:      Bangladesh    2021-07-20    2021-06-21  2021-06-27      17
    ## 36:        Malaysia    2021-08-24    2021-07-12  2021-07-18      12
    ## 37:        Botswana    2021-08-07    2021-07-05  2021-07-11      16
    ## 38:          Turkey    2021-09-13    2021-08-23  2021-08-29     252
    ## 39:         Croatia    2021-09-25    2021-08-02  2021-08-08     218
    ## 40:       Lithuania    2021-08-16    2021-07-19  2021-07-25     430
    ## 41:        Slovakia    2021-08-24    2021-08-02  2021-08-08     117
    ## 42: North Macedonia    2021-08-24    2021-07-19  2021-07-25      13
    ## 43:         Curacao    2021-08-31    2021-07-26  2021-08-01      21
    ## 44:          Uganda    2021-08-24    2021-05-24  2021-05-30      11
    ## 45:         Lebanon    2021-08-16    2021-06-28  2021-07-04      24
    ## 46:     New Zealand    2021-08-24    2021-07-12  2021-07-18      31
    ## 47:         Estonia    2021-09-16    2021-08-02  2021-08-08     171
    ## 48:      Mozambique    2021-08-27    2021-06-28  2021-07-04      13
    ## 49:        Zimbabwe    2021-08-31    2021-07-05  2021-07-11      34
    ## 50:        Pakistan    2021-09-02    2021-08-09  2021-08-15      32
    ## 51:         Iceland    2021-09-08    2021-07-12  2021-07-18      95
    ## 52:       Hong Kong    2021-09-25    2021-06-14  2021-06-20      12
    ## 53:          Kosovo    2021-10-01    2021-08-02  2021-08-08      67
    ## 54:          Zambia    2021-10-01    2021-06-28  2021-07-04      26
    ## 55:      Guadeloupe    2021-10-01    2021-08-23  2021-08-29      26
    ## 56:         Bonaire    2021-10-01    2021-08-09  2021-08-15      24
    ##       location_name seq_available week_starting week_ending seq_voc
    ##     seq_total share_voc
    ##  1:       281 1.0000000
    ##  2:        52 1.0000000
    ##  3:       341 0.9970674
    ##  4:     21926 0.9939798
    ##  5:       979 0.9938713
    ##  6:       171 1.0000000
    ##  7:     23727 0.9900535
    ##  8:       470 0.9957447
    ##  9:      1301 0.9930822
    ## 10:      2513 0.9924393
    ## 11:        24 1.0000000
    ## 12:      4623 0.9930781
    ## 13:      1848 0.9908009
    ## 14:       881 0.9931896
    ## 15:       577 0.9913345
    ## 16:       971 0.9917611
    ## 17:       888 0.9966216
    ## 18:        75 1.0000000
    ## 19:      2883 0.9923691
    ## 20:       229 0.9956332
    ## 21:       452 0.9911504
    ## 22:       353 0.9915014
    ## 23:        86 1.0000000
    ## 24:       201 0.9950249
    ## 25:       110 0.9909091
    ## 26:        73 1.0000000
    ## 27:       169 0.9940828
    ## 28:        85 1.0000000
    ## 29:        46 1.0000000
    ## 30:       661 0.9924357
    ## 31:       188 0.9946809
    ## 32:        12 1.0000000
    ## 33:       102 0.9901961
    ## 34:        48 1.0000000
    ## 35:        17 1.0000000
    ## 36:        12 1.0000000
    ## 37:        16 1.0000000
    ## 38:       253 0.9960474
    ## 39:       218 1.0000000
    ## 40:       431 0.9976798
    ## 41:       117 1.0000000
    ## 42:        13 1.0000000
    ## 43:        21 1.0000000
    ## 44:        11 1.0000000
    ## 45:        24 1.0000000
    ## 46:        31 1.0000000
    ## 47:       171 1.0000000
    ## 48:        13 1.0000000
    ## 49:        34 1.0000000
    ## 50:        32 1.0000000
    ## 51:        95 1.0000000
    ## 52:        12 1.0000000
    ## 53:        67 1.0000000
    ## 54:        26 1.0000000
    ## 55:        26 1.0000000
    ## 56:        24 1.0000000
    ##     seq_total share_voc

  - Restrict included sequences to stop at these dates.

<!-- end list -->

``` r
filt_sequences <- merge(
  sequences, last_seq[, .(location_name, end_date = week_ending)],
  by = "location_name"
)
filt_sequences[is.na(end_date), end_date := max(week_ending),
  by = "location_name"
]
filt_sequences <- filt_sequences[week_ending <= end_date][
  ,
  end_date := NULL
][]
```

  - Filter case notifications to start 4 weeks prior to the introduction
    date of the Delta variant and to stop 4 weeks after Delta has become
    dominant.

<!-- end list -->

``` r
filt_cases <- Reduce(
  function(x, y) {
    merge(x, y, by = "location_name", all.x = TRUE)
  },
  list(
    cases,
    first_seq[, .(location_name, intro_date = week_ending)],
    last_seq[, .(location_name, end_date = week_ending)]
  )
)
filt_cases <- filt_cases[date >= (intro_date - 7 * 4)]
filt_cases <- filt_cases[date <= (end_date + 7 * 4)]
filt_cases[, c("intro_date", "end_date") := NULL]
```

  - Merge duplicating case data for all sequence versions. Sequences are
    only available aggregated by week from Sunday. Approximate the same
    timespan as the case data by changing the weekly reference date

<!-- end list -->

``` r
adjusted_seq <- copy(filt_sequences)[
  ,
  date := week_ending - 1
][, c("week_starting", "week_ending") := NULL]

notifications <- merge(filt_cases, adjusted_seq,
  by = c("date", "location_name"), all.x = TRUE
)
setorder(notifications, seq_available)
setorder(notifications, location_name, date)
setorderv(notifications, c("location_name", "date", "seq_available"))
```

  - Check data for negative values and drop countries if present

<!-- end list -->

``` r
problem_countries <- unique(
  notifications[cases < 0 | seq_total < 0]$location_name
)
problem_countries
```

    ## [1] "France"

``` r
notifications <- notifications[!(location_name %in% problem_countries)]
```

  - Save and summarise filtered notification data.

<!-- end list -->

``` r
# save to observations folder
fwrite(notifications, file = here("data/observations/covariants.csv"))

# Summary
summary(notifications)
```

    ##       date            location_name        location             cases       
    ##  Min.   :2021-03-13   Length:7768        Length:7768        Min.   :    10  
    ##  1st Qu.:2021-05-01   Class :character   Class :character   1st Qu.:  2692  
    ##  Median :2021-05-29   Mode  :character   Mode  :character   Median :  7120  
    ##  Mean   :2021-05-31                                         Mean   : 22337  
    ##  3rd Qu.:2021-07-03                                         3rd Qu.: 29082  
    ##  Max.   :2021-09-25                                         Max.   :296447  
    ##                                                                             
    ##  cases_available      seq_available           seq_voc          seq_total    
    ##  Min.   :2021-03-13   Min.   :2021-07-06   Min.   :    0.0   Min.   :    1  
    ##  1st Qu.:2021-05-01   1st Qu.:2021-07-29   1st Qu.:    5.0   1st Qu.:  321  
    ##  Median :2021-05-29   Median :2021-08-24   Median :   39.0   Median : 1014  
    ##  Mean   :2021-05-31   Mean   :2021-08-22   Mean   :  685.8   Mean   : 2109  
    ##  3rd Qu.:2021-07-03   3rd Qu.:2021-09-13   3rd Qu.:  280.0   3rd Qu.: 1789  
    ##  Max.   :2021-09-25   Max.   :2021-10-06   Max.   :30917.0   Max.   :31194  
    ##                       NA's   :76           NA's   :76        NA's   :76     
    ##    share_voc      
    ##  Min.   :0.00000  
    ##  1st Qu.:0.00611  
    ##  Median :0.07435  
    ##  Mean   :0.32192  
    ##  3rd Qu.:0.72067  
    ##  Max.   :1.00000  
    ##  NA's   :76

## Explore notifications

  - Case notifications

<!-- end list -->

``` r
not_cases <- unique(notifications[, .(date, cases, location_name)])
# plot cases
ggplot(not_cases) +
  aes(x = date, y = cases, col = location_name) +
  geom_line(alpha = 0.6) +
  theme_bw() +
  theme(legend.position = "bottom")
```

![](process-obs_files/figure-gfm/cases-1.png)<!-- -->

  - Case notifications normalised by maximum number of notifications in
    that location.

<!-- end list -->

``` r
ggplot(copy(not_cases)[, cases := cases / max(cases), by = "location_name"]) +
  aes(x = date, y = cases, col = location_name) +
  geom_line(alpha = 0.6) +
  theme_bw() +
  theme(legend.position = "bottom")
```

![](process-obs_files/figure-gfm/normalised-by-max-1.png)<!-- -->

  - Compare case notifications in Germany in this data with case
    notifications in the RKI data source and the WHO data source.

<!-- end list -->

``` r
# get comparison data sources
rki <- fread(here("data", "observations", "rki.csv"))
who <- setDT(
  covidregionaldata::get_national_data(
    "Germany", source = "WHO", verbose = FALSE
  )
)
# make the who data source weekly
who[, cases := frollsum(cases_new, n = 7)]
who <- who[weekdays(date) %in% "Saturday"]

germany <- rbind(
  unique(rki[, .(date = as.Date(date), cases, source = "RKI")]),
  not_cases[location_name == "Germany"][,
     .(date, cases, source = "JHU")],
  who[date >= min(rki$date) & date <= max(rki$date)][,
      .(date, cases = cases, source = "WHO")]
)

ggplot(germany) +
  aes(x = date, y = cases, col = source) +
  geom_point(size = 1.4, alpha = 0.8) +
  geom_line(size = 1.1, alpha = 0.6) +
  scale_colour_brewer(palette = "Dark2") +
  theme_bw() +
  theme(legend.position = "bottom") +
  labs(x = "Date", y = "COVID-19 notifications", col = "Source")
```

![](process-obs_files/figure-gfm/rki-vs-covariants-cases-1.png)<!-- -->

  - Share of sequences that were positive for the Delta variant based on
    the lastest available data.

<!-- end list -->

``` r
ggplot(notifications[seq_available == max(seq_available, na.rm = TRUE)]) +
  aes(x = date, y = share_voc, col = location_name) +
  geom_line(alpha = 0.6) +
  theme_bw() +
  theme(legend.position = "bottom")
```

![](process-obs_files/figure-gfm/latest-share-voc-1.png)<!-- -->

  - Share of sequences that were positive for the Delta variant based on
    the first available data.

<!-- end list -->

``` r
ggplot(notifications[seq_available == min(seq_available, na.rm = TRUE)]) +
  aes(x = date, y = share_voc, col = location_name) +
  geom_line(alpha = 0.6) +
  theme_bw() +
  theme(legend.position = "bottom")
```

![](process-obs_files/figure-gfm/first-share-voc-1.png)<!-- -->

  - Share of sequences positive for Delta by availability

<!-- end list -->

``` r
ggplot(
  notifications[!is.na(seq_available)][
    ,
    seq_available := as.factor(seq_available)
  ]
) +
  aes(x = date, y = share_voc, col = seq_available) +
  geom_line(alpha = 0.6) +
  theme_bw() +
  theme(legend.position = "bottom") +
  facet_wrap(vars(location_name))
```

![](process-obs_files/figure-gfm/share-voc-by-avail-1.png)<!-- -->

  - Relative change in the proportion of sequences that were Delta
    between each release of sequence data and the latest available
    release.

<!-- end list -->

``` r
latest_seq <- notifications[,
  n := .N,
  by = c("date", "location_name")
][, .SD[seq_available == max(seq_available)],
  by = c("date", "location_name")
][n > 1]

seq_change <- merge(
  notifications[!is.na(seq_available)][
    ,
    seq_available := as.factor(seq_available)
  ][
    ,
    .(location_name, date, share_voc, seq_available)
  ],
  latest_seq[, .(location_name, date, latest_voc = share_voc)],
  by = c("location_name", "date")
)

seq_change[, per_latest := share_voc / latest_voc]

seq_change <- seq_change[date <= as.Date("2021-09-01")]

ggplot(seq_change) +
  aes(x = date, y = per_latest, col = seq_available, group = seq_available) +
  geom_point(size = 1.1, alpha = 0.8) +
  geom_line(alpha = 0.6) +
  theme_bw() +
  theme(legend.position = "bottom") +
  facet_wrap(vars(location_name), scales = "free")
```

    ## Warning: Removed 783 rows containing missing values (geom_point).

    ## Warning: Removed 3 row(s) containing missing values (geom_path).

![](process-obs_files/figure-gfm/sequences-change-1.png)<!-- -->
