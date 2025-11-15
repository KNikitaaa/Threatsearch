# Исследование метаданных DNS трафика
niki-tos29@yandex.ru

## Цель работы

1.  Закрепить практические навыки использования языка программирования R
    для обработки данных
2.  Закрепить знания основных функций обработки данных экосистемы
    tidyverse языка R
3.  Закрепить навыки исследования метаданных DNS трафика

## Исходные данные

1.  Программное обеспечение Windows 11 Pro
2.  Rstudio Desktop
3.  Интерпретатор языка R 4.5.1
4.  Программный пакет dplyr

## План

1.  Импорт DNS логов
2.  Подготовка импортированных данных
3.  Анализ DNS логов
4.  Обогащение данных
5.  Формирование отчета

## Шаги:

Установим и подключим необходимые библиотеки:

``` r
library(dplyr)
```


    Присоединяю пакет: 'dplyr'

    Следующие объекты скрыты от 'package:stats':

        filter, lag

    Следующие объекты скрыты от 'package:base':

        intersect, setdiff, setequal, union

``` r
library(tidyverse)
```

    ── Attaching core tidyverse packages ──────────────────────── tidyverse 2.0.0 ──
    ✔ forcats   1.0.1     ✔ readr     2.1.5
    ✔ ggplot2   4.0.0     ✔ stringr   1.5.2
    ✔ lubridate 1.9.4     ✔ tibble    3.3.0
    ✔ purrr     1.1.0     ✔ tidyr     1.3.1

    ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
    ✖ dplyr::filter() masks stats::filter()
    ✖ dplyr::lag()    masks stats::lag()
    ℹ Use the conflicted package (<http://conflicted.r-lib.org/>) to force all conflicts to become errors

``` r
library(readr)
library(httr)
library(lubridate)
```

### Подготовка данных

1.  Импортируем данные DNS

``` r
get_file <- function(url, filename) {
  download.file(url, filename)
  unzip(filename)
}

get_file("https://storage.yandexcloud.net/dataset.ctfsec/dns.zip", "dns_data.zip")
dns_data <- read_tsv("dns.log", comment = "#", col_names = F)
```

    Rows: 427935 Columns: 23
    ── Column specification ────────────────────────────────────────────────────────
    Delimiter: "\t"
    chr (13): X2, X3, X5, X7, X9, X10, X11, X12, X13, X14, X15, X21, X22
    dbl  (5): X1, X4, X6, X8, X20
    lgl  (5): X16, X17, X18, X19, X23

    ℹ Use `spec()` to retrieve the full column specification for this data.
    ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

1.  Добавим пропущенные данные о структуре данных (назначении столбцов)

``` r
column_names <- c(
  "time", "session_id", "src_ip", "src_port", "dst_ip", 
  "dst_port", "proto", "trans_id", "domain", "qclass_val", 
  "qclass_txt", "qtype_val", "qtype_txt", "response_code", "response_txt", 
  "authoritative", "truncated", "recursion_desired", "recursion_available", 
  "zero_field", "answers", "ttl_values", "blocked"
)

names(dns_data) <- column_names
```

1.  Преобразуем данные в столбцах в нужный формат

``` r
dns_data <- dns_data |> 
  mutate(
    time = as_datetime(time),
    src_port = as.numeric(src_port),
    qclass_val = as.numeric(qclass_val),
    qtype_val = as.numeric(qtype_val)
  )
```

    Warning: There were 2 warnings in `mutate()`.
    The first warning was:
    ℹ In argument: `qclass_val = as.numeric(qclass_val)`.
    Caused by warning:
    ! в результате преобразования созданы NA
    ℹ Run `dplyr::last_dplyr_warnings()` to see the 1 remaining warning.

Посмотрим общую структуру данных

``` r
glimpse(dns_data)
```

    Rows: 427,935
    Columns: 23
    $ time                <dttm> 2012-03-16 12:30:05, 2012-03-16 12:30:15, 2012-03…
    $ session_id          <chr> "CWGtK431H9XuaTN4fi", "C36a282Jljz7BsbGH", "C36a28…
    $ src_ip              <chr> "192.168.202.100", "192.168.202.76", "192.168.202.…
    $ src_port            <dbl> 45658, 137, 137, 137, 137, 137, 137, 137, 137, 137…
    $ dst_ip              <chr> "192.168.27.203", "192.168.202.255", "192.168.202.…
    $ dst_port            <dbl> 137, 137, 137, 137, 137, 137, 137, 137, 137, 137, …
    $ proto               <chr> "udp", "udp", "udp", "udp", "udp", "udp", "udp", "…
    $ trans_id            <dbl> 33008, 57402, 57402, 57402, 57398, 57398, 57398, 6…
    $ domain              <chr> "*\\x00\\x00\\x00\\x00\\x00\\x00\\x00\\x00\\x00\\x…
    $ qclass_val          <dbl> 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,…
    $ qclass_txt          <chr> "C_INTERNET", "C_INTERNET", "C_INTERNET", "C_INTER…
    $ qtype_val           <dbl> 33, 32, 32, 32, 32, 32, 32, 32, 32, 32, 33, 33, 33…
    $ qtype_txt           <chr> "SRV", "NB", "NB", "NB", "NB", "NB", "NB", "NB", "…
    $ response_code       <chr> "0", "-", "-", "-", "-", "-", "-", "-", "-", "-", …
    $ response_txt        <chr> "NOERROR", "-", "-", "-", "-", "-", "-", "-", "-",…
    $ authoritative       <lgl> FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, F…
    $ truncated           <lgl> FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, F…
    $ recursion_desired   <lgl> FALSE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, T…
    $ recursion_available <lgl> FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, F…
    $ zero_field          <dbl> 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 1, 1,…
    $ answers             <chr> "-", "-", "-", "-", "-", "-", "-", "-", "-", "-", …
    $ ttl_values          <chr> "-", "-", "-", "-", "-", "-", "-", "-", "-", "-", …
    $ blocked             <lgl> FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, F…

### Анализ

1.  Сколько участников информационного обмена в сети Доброй Организации?

``` r
unique_ips <- unique(c(dns_data$src_ip, dns_data$dst_ip))
total_participants <- length(unique_ips)
print(total_participants)
```

    [1] 1359

1.  Какое соотношение участников обмена внутри сети и участников
    обращений к внешним ресурсам?

``` r
internal_ips <- unique_ips[str_detect(unique_ips, "^(10\\.|192\\.168\\.|172\\.(1[6-9]|2[0-9]|3[0-1])\\.)")]
external_ips <- setdiff(unique_ips, internal_ips)
ratio <- length(internal_ips) / length(external_ips)
print(ratio)
```

    [1] 13.77174

1.  Найдите топ-10 участников сети, проявляющих наибольшую сетевую
    активность.

``` r
top_active <- dns_data |> 
  group_by(src_ip) |> 
  tally(sort = TRUE) |> 
  slice_head(n = 10)
print(top_active)
```

    # A tibble: 10 × 2
       src_ip              n
       <chr>           <int>
     1 10.10.117.210   75943
     2 192.168.202.93  26522
     3 192.168.202.103 18121
     4 192.168.202.76  16978
     5 192.168.202.97  16176
     6 192.168.202.141 14967
     7 10.10.117.209   14222
     8 192.168.202.110 13372
     9 192.168.203.63  12148
    10 192.168.202.106 10784

1.  Найдите топ-10 доменов, к которым обращаются пользователи сети и
    соответственное количество обращений

``` r
top_domains <- dns_data |> 
  count(domain, sort = TRUE) |> 
  head(10)
print(top_domains)
```

    # A tibble: 10 × 2
       domain                                                                      n
       <chr>                                                                   <int>
     1 "teredo.ipv6.microsoft.com"                                             39273
     2 "tools.google.com"                                                      14057
     3 "www.apple.com"                                                         13390
     4 "time.apple.com"                                                        13109
     5 "safebrowsing.clients.google.com"                                       11658
     6 "*\\x00\\x00\\x00\\x00\\x00\\x00\\x00\\x00\\x00\\x00\\x00\\x00\\x00\\x… 10401
     7 "WPAD"                                                                   9134
     8 "44.206.168.192.in-addr.arpa"                                            7248
     9 "HPE8AA67"                                                               6929
    10 "ISATAP"                                                                 6569

1.  Определите базовые статистические характеристики (функция summary()
    ) интервала времени между последовательными обращениями к топ-10
    доменам.

``` r
popular_domains <- top_domains |> pull(domain)

time_stats <- dns_data |> 
  filter(domain %in% popular_domains) |>
  arrange(time) |> 
  group_by(domain) |>
  mutate(time_diff = lead(time) - time) |>
  filter(!is.na(time_diff)) |>
  summarise(
    minimum = min(time_diff),
    first_quartile = quantile(time_diff, 0.25),
    middle = median(time_diff),
    third_quartile = quantile(time_diff, 0.75),
    maximum = max(time_diff),
    average = mean(time_diff)
  )
print(time_stats)
```

    # A tibble: 10 × 7
       domain           minimum first_quartile middle third_quartile maximum average
       <chr>            <drtn>  <drtn>         <drtn> <drtn>         <drtn>  <drtn> 
     1 "*\\x00\\x00\\x… 0 secs  0.1499999 secs 0.500…  1.5000 secs   52723.… 11.244…
     2 "44.206.168.192… 0 secs  2.0899999 secs 4.000… 20.0900 secs   49679.… 16.006…
     3 "HPE8AA67"       0 secs  0.7500000 secs 0.750… 25.4900 secs   50044.… 16.608…
     4 "ISATAP"         0 secs  0.7500000 secs 0.759…  1.0500 secs   51997.… 17.463…
     5 "WPAD"           0 secs  0.7500000 secs 0.750…  1.1100 secs   50049.… 12.608…
     6 "safebrowsing.c… 0 secs  0.0000000 secs 1.000…  2.0100 secs   49952.… 10.003…
     7 "teredo.ipv6.mi… 0 secs  0.0000000 secs 0.000…  0.5100 secs   50387.…  2.941…
     8 "time.apple.com" 0 secs  0.3699999 secs 1.760…  4.7225 secs   50924.…  8.665…
     9 "tools.google.c… 0 secs  0.0000000 secs 0.000…  1.0000 secs   50364.…  8.187…
    10 "www.apple.com"  0 secs  0.0000000 secs 1.000…  3.0100 secs   50963.…  8.607…

1.  Часто вредоносное программное обеспечение использует DNS канал в
    качестве канала управления, периодически отправляя запросы на
    подконтрольный злоумышленникам DNS сервер. По периодическим запросам
    на один и тот же домен можно выявить скрытый DNS канал. Есть ли
    такие IP адреса в исследуемом датасете?

``` r
suspicious_channels <- dns_data |>
  arrange(src_ip, domain, time) |>
  group_by(src_ip, domain) |>
  mutate(interval = as.numeric(lead(time) - time)) |>
  filter(!is.na(interval)) |>
  summarise(
    request_count = n() + 1,
    mean_interval = mean(interval),
    .groups = 'drop'
  ) |>
  filter(request_count >= 10, mean_interval <= 30) |>
  group_by(src_ip) |>
  summarise(
    suspicious_domains = n(),
    total_queries = sum(request_count)
  ) |>
  arrange(desc(suspicious_domains))

print(suspicious_channels)
```

    # A tibble: 106 × 3
       src_ip          suspicious_domains total_queries
       <chr>                        <int>         <dbl>
     1 192.168.202.97                 281          6614
     2 192.168.202.71                  51          1203
     3 192.168.202.84                  48          1157
     4 10.10.117.210                   47         69031
     5 192.168.202.79                  44          1141
     6 192.168.202.100                 37           578
     7 192.168.202.103                 28          9988
     8 192.168.202.110                 23          2378
     9 192.168.202.106                 18          1967
    10 192.168.204.45                  16           363
    # ℹ 96 more rows

### Обогащение данных

1.  Определите местоположение (страну, город) и организацию-провайдера
    для топ-10 доменов. Для этого можно использовать сторонние сервисы,
    например http://ip-api.com (API-эндпоинт – http://ip-api.com/json).

``` r
fetch_geo_data <- function(domain_name) {
  tryCatch({
    api_response <- GET(paste0("http://ip-api.com/json/", domain_name))
    response_data <- content(api_response, "parsed")
    return(list(
      domain = domain_name,
      address = response_data$query %||% NA,
      nation = response_data$country %||% NA,
      town = response_data$city %||% NA,
      provider = response_data$isp %||% NA
    ))
  }, error = function(e) {
    return(list(domain = domain_name, address = NA, nation = NA, town = NA, provider = NA))
  })
}

geo_results <- map_dfr(popular_domains, fetch_geo_data)
print(as.data.frame(geo_results))
```

                                                                        domain
    1                                                teredo.ipv6.microsoft.com
    2                                                         tools.google.com
    3                                                            www.apple.com
    4                                                           time.apple.com
    5                                          safebrowsing.clients.google.com
    6  *\\x00\\x00\\x00\\x00\\x00\\x00\\x00\\x00\\x00\\x00\\x00\\x00\\x00\\x00
    7                                                                     WPAD
    8                                              44.206.168.192.in-addr.arpa
    9                                                                 HPE8AA67
    10                                                                  ISATAP
                                                                       address
    1                                                teredo.ipv6.microsoft.com
    2                                                          142.250.151.100
    3                                                2a02:26f0:1c80:119b::1aca
    4                                                  2a01:b740:a20:3000::1f2
    5                                                          142.250.140.113
    6  *\\x00\\x00\\x00\\x00\\x00\\x00\\x00\\x00\\x00\\x00\\x00\\x00\\x00\\x00
    7                                                                     WPAD
    8                                              44.206.168.192.in-addr.arpa
    9                                                                 HPE8AA67
    10                                                                  ISATAP
               nation          town            provider
    1            <NA>          <NA>                <NA>
    2   United States Mountain View          Google LLC
    3  United Kingdom        London Akamai Technologies
    4          France        Clichy          Apple Inc.
    5   United States Mountain View          Google LLC
    6            <NA>          <NA>                <NA>
    7            <NA>          <NA>                <NA>
    8            <NA>          <NA>                <NA>
    9            <NA>          <NA>                <NA>
    10           <NA>          <NA>                <NA>

## Оценка результата

В результате лабораторной работы мы исследовали подозрительную сетевую
активность во внутренней сети Доброй Организации. Исследовали файлы,
восстановили данные, подготовили их к анализу и закрепили навыки
исследования метаданных DNS трафика.

## Вывод

В результате выполнения работы были закреплены знания основных функций
обработки данных экосистемы tidyverse и получены навыки исследования
метаданных DNS трафика
