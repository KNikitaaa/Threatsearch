# Исследование информации о состоянии беспроводных сетей
niki-tos29@yandex.ru

## Цель работы

1.  Получить знания о методах исследования радиоэлектронной обстановки.
2.  Составить представление о механизмах работы Wi-Fi сетей на канальном
    и сетевом уровне модели OSI.
3.  Закрепить практические навыки использования языка программирования R
    для обработки данных
4.  Закрепить знания основных функций обработки данных экосистемы
    tidyverse языка R

## Исходные данные

1.  Программное обеспечение Windows 11 Pro
2.  Rstudio Desktop
3.  Интерпретатор языка R 4.5.1
4.  Программный пакет dplyr
5.  Журналы программных средств анализа беспроводных сетей – tcpdump и
    airodump-ng

## План

1.  Подготовка данных для анализа
2.  Анализ точек доступа
3.  Анализ данных клиентов
4.  Оформление отчета

## Шаги:

### Подготовка данных

1.  Импортируйте данные.

``` r
options(repos = c(CRAN = "https://mirror.truenetwork.ru/CRAN/"))
install.packages("dplyr")
```

    Устанавливаю пакет в 'C:/Users/Никита/AppData/Local/R/win-library/4.5'
    (потому что 'lib' не определено)

    пакет 'dplyr' успешно распакован, MD5-суммы проверены

    Warning: не могу удалить прежнюю установку пакета 'dplyr'

    Warning in file.copy(savedcopy, lib, recursive = TRUE): проблема с копированием
    C:\Users\Никита\AppData\Local\R\win-library\4.5\00LOCK\dplyr\libs\x64\dplyr.dll
    в C:\Users\Никита\AppData\Local\R\win-library\4.5\dplyr\libs\x64\dplyr.dll:
    Permission denied

    Warning: восстановлен 'dplyr'


    Скачанные бинарные пакеты находятся в
        C:\Users\Никита\AppData\Local\Temp\Rtmpy6KqLB\downloaded_packages

``` r
library(tidyverse)
```

    ── Attaching core tidyverse packages ──────────────────────── tidyverse 2.0.0 ──
    ✔ dplyr     1.1.4     ✔ readr     2.1.5
    ✔ forcats   1.0.1     ✔ stringr   1.5.2
    ✔ ggplot2   4.0.0     ✔ tibble    3.3.0
    ✔ lubridate 1.9.4     ✔ tidyr     1.3.1
    ✔ purrr     1.1.0     

    ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
    ✖ dplyr::filter() masks stats::filter()
    ✖ dplyr::lag()    masks stats::lag()
    ℹ Use the conflicted package (<http://conflicted.r-lib.org/>) to force all conflicts to become errors

``` r
library(lubridate)
library(stringr)
url <- "https://storage.yandexcloud.net/dataset.ctfsec/P2_wifi_data.csv"
```

``` r
ap_data <- read_csv(url, skip = 1, col_names = FALSE)
```

    Warning: One or more parsing issues, call `problems()` on your data frame for details,
    e.g.:
      dat <- vroom(...)
      problems(dat)

    Rows: 12250 Columns: 15
    ── Column specification ────────────────────────────────────────────────────────
    Delimiter: ","
    chr (15): X1, X2, X3, X4, X5, X6, X7, X8, X9, X10, X11, X12, X13, X14, X15

    ℹ Use `spec()` to retrieve the full column specification for this data.
    ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

``` r
client_start_line <- which(ap_data$X1 == "Station MAC")
```

1.  Привести датасеты в вид “аккуратных данных”, преобразовать типы
    столбцов в соответствии с типом данных.

``` r
ap_data_raw <- ap_data[2:(client_start_line[1]-1), ]
client_data_raw <- ap_data[client_start_line[1]:nrow(ap_data), ]

ap_data_raw <- ap_data_raw[rowSums(is.na(ap_data_raw)) != ncol(ap_data_raw), ]
client_data_raw <- client_data_raw[rowSums(is.na(client_data_raw)) != ncol(client_data_raw), ]
client_data_raw <- client_data_raw[, 1:7]
ap_col_names <- c("BSSID", "First_time_seen", "Last_time_seen", "channel", 
                  "Speed", "Privacy", "Cipher", "Authentication", "Power",
                  "beacons", "IV", "LAN_IP", "ID_length", "ESSID", "Key")

client_col_names <- c("Station_MAC", "First_time_seen", "Last_time_seen", "Power",
                      "packets", "BSSID", "Probed_ESSIDs")
names(ap_data_raw) <- ap_col_names[1:ncol(ap_data_raw)]
names(client_data_raw) <- client_col_names[1:ncol(client_data_raw)]
ap_data_clean <- ap_data_raw %>%
  filter(!is.na(BSSID), BSSID != "", !is.na(ESSID), ESSID != "") %>%
  mutate(
    First_time_seen = as.POSIXct(First_time_seen, format = "%Y-%m-%d %H:%M:%S"),
    Last_time_seen = as.POSIXct(Last_time_seen, format = "%Y-%m-%d %H:%M:%S"),
    channel = as.numeric(channel),
    Speed = as.numeric(Speed),
    Power = as.numeric(Power),
    beacons = as.numeric(beacons),
    IV = as.numeric(IV),
    ID_length = as.numeric(ID_length)
  )

client_data_clean <- client_data_raw[-1, ] %>%
  filter(!is.na(Station_MAC), Station_MAC != "", Station_MAC != "Station MAC") %>%
  mutate(
    First_time_seen = as.POSIXct(First_time_seen, format = "%Y-%m-%d %H:%M:%S"),
    Last_time_seen = as.POSIXct(Last_time_seen, format = "%Y-%m-%d %H:%M:%S"),
    Power = as.numeric(Power),
    packets = as.numeric(packets)
  )
```

1.  Просмотрите общую структуру данных с помощью функции glimpse()

``` r
glimpse(ap_data_clean)
```

    Rows: 96
    Columns: 15
    $ BSSID           <chr> "BE:F1:71:D5:17:8B", "6E:C7:EC:16:DA:1A", "9A:75:A8:B9…
    $ First_time_seen <dttm> 2023-07-28 09:13:03, 2023-07-28 09:13:03, 2023-07-28 …
    $ Last_time_seen  <dttm> 2023-07-28 11:50:50, 2023-07-28 11:55:12, 2023-07-28 …
    $ channel         <dbl> 1, 1, 1, 7, 6, 11, 11, 11, 1, 6, 11, 11, 6, 6, 1, 1, 1…
    $ Speed           <dbl> 195, 130, 360, 360, 130, 195, 130, 130, 195, 180, 130,…
    $ Privacy         <chr> "WPA2", "WPA2", "WPA2", "WPA2", "OPN", "WPA2", "WPA2",…
    $ Cipher          <chr> "CCMP", "CCMP", "CCMP", "CCMP", NA, "CCMP", "CCMP", "C…
    $ Authentication  <chr> "PSK", "PSK", "PSK", "PSK", NA, "PSK", "PSK", "PSK", "…
    $ Power           <dbl> -30, -30, -68, -37, -63, -27, -38, -38, -66, -42, -73,…
    $ beacons         <dbl> 846, 750, 694, 510, 251, 1647, 1251, 704, 617, 1390, 2…
    $ IV              <dbl> 504, 116, 26, 21, 3430, 80, 11, 0, 0, 86, 0, 0, 907, 8…
    $ LAN_IP          <chr> "0.  0.  0.  0", "0.  0.  0.  0", "0.  0.  0.  0", "0.…
    $ ID_length       <dbl> 12, 4, 2, 14, 13, 12, 13, 24, 12, 10, 24, 24, 12, 8, 4…
    $ ESSID           <chr> "C322U13 3965", "Cnet", "KC", "POCO X5 Pro 5G", "MIREA…
    $ Key             <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA…

``` r
glimpse(client_data_clean)
```

    Rows: 12,081
    Columns: 7
    $ Station_MAC     <chr> "CA:66:3B:8F:56:DD", "96:35:2D:3D:85:E6", "5C:3A:45:9E…
    $ First_time_seen <dttm> 2023-07-28 09:13:03, 2023-07-28 09:13:03, 2023-07-28 …
    $ Last_time_seen  <dttm> 2023-07-28 10:59:44, 2023-07-28 09:13:03, 2023-07-28 …
    $ Power           <dbl> -33, -65, -39, -61, -53, -43, -31, -71, -74, -65, -45,…
    $ packets         <dbl> 858, 4, 432, 958, 1, 344, 163, 3, 115, 437, 265, 77, 7…
    $ BSSID           <chr> "BE:F1:71:D5:17:8B", "(not associated)", "BE:F1:71:D6:…
    $ Probed_ESSIDs   <chr> "C322U13 3965", "IT2 Wireless", "C322U21 0566", "C322U…

### Анализ

``` r
get_oui <- function(mac) {
  ifelse(!is.na(mac) & mac != "", 
         str_sub(mac, 1, 8) %>% str_replace_all(":", "-"),
         NA_character_)
}
```

1.  Определить небезопасные точки доступа (без шифрования – OPN).

``` r
insecure_ap <- ap_data_clean %>%
  filter(Privacy == "OPN") %>%
  select(BSSID, ESSID, Privacy, Power, channel) %>%
  arrange(desc(Power))
```

``` r
if(nrow(insecure_ap) > 0) {
  print(insecure_ap)
  cat("Всего небезопасных точек доступа:", nrow(insecure_ap), "\n")
  cat("Процент от общего числа:", round(nrow(insecure_ap)/nrow(ap_data_clean)*100, 1), "%\n")
} else {
  cat("Небезопасные точки доступа (OPN) не обнаружены.\n")
}
```

    # A tibble: 23 × 5
       BSSID             ESSID         Privacy Power channel
       <chr>             <chr>         <chr>   <dbl>   <dbl>
     1 00:3E:1A:5D:14:45 MT_FREE       OPN       -57      11
     2 E8:28:C1:DC:B2:52 MIREA_HOTSPOT OPN       -63       6
     3 E8:28:C1:DC:B2:50 MIREA_GUESTS  OPN       -63       6
     4 E8:28:C1:DD:04:52 MIREA_HOTSPOT OPN       -67      11
     5 02:BC:15:7E:D5:DC MT_FREE       OPN       -67      11
     6 00:03:7A:1A:03:56 MT_FREE       OPN       -68      11
     7 02:67:F1:B0:6C:98 MT_FREE       OPN       -68      11
     8 00:53:7A:99:98:56 MT_FREE       OPN       -68      11
     9 E8:28:C1:DE:74:32 MIREA_HOTSPOT OPN       -69       6
    10 E8:28:C1:DC:C8:32 MIREA_HOTSPOT OPN       -69       1
    # ℹ 13 more rows
    Всего небезопасных точек доступа: 23 
    Процент от общего числа: 24 %

1.  Определить производителя для каждого обнаруженного устройства.

``` r
ap_with_manufacturer <- ap_data_clean %>%
  mutate(OUI = get_oui(BSSID)) %>%
  select(BSSID, ESSID, OUI, Privacy, Power)

manufacturer_summary <- ap_with_manufacturer %>%
  count(OUI, name = "count") %>%
  arrange(desc(count))
```

``` r
print(head(manufacturer_summary, 10))
```

    # A tibble: 10 × 2
       OUI      count
       <chr>    <int>
     1 E8-28-C1    17
     2 38-1A-52     6
     3 00-23-EB     4
     4 00-26-99     4
     5 A6-02-B9     3
     6 BE-F1-71     3
     7 00-26-CB     2
     8 00-03-7A     1
     9 00-03-7F     1
    10 00-3E-1A     1

``` r
cat("Всего уникальных производителей:", n_distinct(ap_with_manufacturer$OUI, na.rm = TRUE), "\n")
```

    Всего уникальных производителей: 64 

1.  Выявить устройства, использующие последнюю версию протокола
    шифрования WPA3, и названия точек доступа, реализованных на этих
    устройствах.

``` r
wpa3_devices <- ap_data_clean %>%
  filter(
    str_detect(toupper(coalesce(Authentication, "")), "WPA3") | 
    str_detect(toupper(coalesce(Privacy, "")), "WPA3") |
    str_detect(toupper(coalesce(Cipher, "")), "WPA3")
  ) %>%
  select(BSSID, ESSID, Privacy, Authentication, Cipher, Power) %>%
  arrange(desc(Power))
```

``` r
if(nrow(wpa3_devices) > 0) {
  print(wpa3_devices)
  cat("Всего устройств с WPA3:", nrow(wpa3_devices), "\n")
} else {
  cat("Устройства с WPA3 не обнаружены.\n") }
```

    # A tibble: 5 × 6
      BSSID             ESSID                    Privacy Authentication Cipher Power
      <chr>             <chr>                    <chr>   <chr>          <chr>  <dbl>
    1 BE:FD:EF:18:92:44 "Димасик"                WPA3 W… SAE PSK        CCMP     -64
    2 CE:48:E7:86:4E:33 "iPhone (Анастасия)"     WPA3 W… SAE PSK        CCMP     -65
    3 3A:DA:00:F9:0C:02 "iPhone XS Max \U0001f9… WPA3 W… SAE PSK        CCMP     -65
    4 8E:1F:94:96:DA:FD "iPhone (Анастасия)"     WPA3 W… SAE PSK        CCMP     -67
    5 A2:FE:FF:B8:9B:C9 "Christie’s"             WPA3 W… SAE PSK        CCMP     -70
    Всего устройств с WPA3: 5 

1.  Отсортировать точки доступа по интервалу времени, в течение которого
    они находились на связи, по убыванию.

``` r
ap_duration <- ap_data_clean %>%
  arrange(BSSID, First_time_seen) %>%
  group_by(BSSID) %>%
  mutate(
    session_gap = as.numeric(difftime(First_time_seen, lag(Last_time_seen, default = first(First_time_seen)), units = "mins")),
    session_id = cumsum(session_gap > 45 | row_number() == 1)
  ) %>%
  group_by(BSSID, session_id) %>%
  summarise(
    ESSID = first(ESSID),
    session_start = min(First_time_seen),
    session_end = max(Last_time_seen),
    .groups = 'drop'
  ) %>%
  mutate(
    session_duration_minutes = as.numeric(difftime(session_end, session_start, units = "mins"))
  ) %>%
  arrange(desc(session_duration_minutes)) %>%
  select(BSSID, ESSID, session_start, session_end, session_duration_minutes)

print(head(ap_duration, 10))
```

    # A tibble: 10 × 5
       BSSID    ESSID session_start       session_end         session_duration_min…¹
       <chr>    <chr> <dttm>              <dttm>                               <dbl>
     1 E8:28:C… MIRE… 2023-07-28 09:13:09 2023-07-28 11:56:05                   163.
     2 E8:28:C… MIRE… 2023-07-28 09:13:03 2023-07-28 11:55:38                   163.
     3 6E:C7:E… Cnet  2023-07-28 09:13:03 2023-07-28 11:55:12                   162.
     4 E8:28:C… MIRE… 2023-07-28 09:13:06 2023-07-28 11:55:12                   162.
     5 8E:55:4… Vlad… 2023-07-28 09:13:06 2023-07-28 11:55:09                   162.
     6 00:26:9… GIVC  2023-07-28 09:13:20 2023-07-28 11:55:10                   162.
     7 00:26:9… GIVC  2023-07-28 09:13:06 2023-07-28 11:54:53                   162.
     8 1E:93:E… Gala… 2023-07-28 09:13:04 2023-07-28 11:53:37                   161.
     9 9A:75:A… KC    2023-07-28 09:13:03 2023-07-28 11:53:31                   160.
    10 00:23:E… GIVC  2023-07-28 09:13:40 2023-07-28 11:53:35                   160.
    # ℹ abbreviated name: ¹​session_duration_minutes

``` r
cat("Максимальное время на связи с учетом сессий:", round(max(ap_duration$session_duration_minutes, na.rm = TRUE), 1), "минут\n\n")
```

    Максимальное время на связи с учетом сессий: 162.9 минут

1.  Обнаружить топ-10 самых быстрых точек доступа.

``` r
fastest_ap <- ap_data_clean %>%
  filter(!is.na(Speed), Speed > 0) %>%
  arrange(desc(Speed)) %>%
  head(10) %>%
  select(BSSID, ESSID, Speed, channel, Power)

print(fastest_ap)
```

    # A tibble: 10 × 5
       BSSID             ESSID              Speed channel Power
       <chr>             <chr>              <dbl>   <dbl> <dbl>
     1 CE:48:E7:86:4E:33 iPhone (Анастасия)   866      44   -65
     2 8E:1F:94:96:DA:FD iPhone (Анастасия)   866      44   -67
     3 9A:75:A8:B9:04:1E KC                   360       1   -68
     4 4A:EC:1E:DB:BF:95 POCO X5 Pro 5G       360       7   -37
     5 56:C5:2B:9F:84:90 OnePlus 6T           360       1   -64
     6 E8:28:C1:DC:B2:41 MIREA_GUESTS         360      48   -89
     7 E8:28:C1:DC:B2:40 MIREA_HOTSPOT        360      48   -88
     8 E8:28:C1:DD:04:40 MIREA_HOTSPOT        360      52   -84
     9 E8:28:C1:DD:04:41 MIREA_GUESTS         360      52   -83
    10 14:EB:B6:6A:76:37 Gnezdo_lounge 2      360       3   -85

``` r
cat("Максимальная скорость:", max(fastest_ap$Speed, na.rm = TRUE), "Mbps\n")
```

    Максимальная скорость: 866 Mbps

1.  Отсортировать точки доступа по частоте отправки запросов (beacons) в
    единицу времени по их убыванию.

``` r
beacon_analysis <- ap_data_clean %>%
  mutate(
    total_time_hours = as.numeric(difftime(Last_time_seen, First_time_seen, units = "hours")),
    beacon_rate = ifelse(total_time_hours > 0, beacons / total_time_hours, beacons)
  ) %>%
  filter(!is.na(beacon_rate), total_time_hours > 0, beacon_rate > 0) %>%
  arrange(desc(beacon_rate)) %>%
  select(BSSID, ESSID, beacons, total_time_hours, beacon_rate, channel)

print(head(beacon_analysis, 10))
```

    # A tibble: 10 × 6
       BSSID             ESSID          beacons total_time_hours beacon_rate channel
       <chr>             <chr>            <dbl>            <dbl>       <dbl>   <dbl>
     1 F2:30:AB:E9:03:ED "iPhone (Ulia…       6         0.00194        3086.       1
     2 B2:CF:C0:00:4A:60 "Михаил's Gal…       4         0.00139        2880        6
     3 3A:DA:00:F9:0C:02 "iPhone XS Ma…       5         0.0025         2000        6
     4 02:BC:15:7E:D5:DC "MT_FREE"            1         0.000556       1800       11
     5 00:3E:1A:5D:14:45 "MT_FREE"            1         0.000556       1800       11
     6 D2:25:91:F6:6C:D8 "Саня"               5         0.00361        1385.      12
     7 BE:F1:71:D6:10:D7 "C322U21 0566"    1647         2.63            627.      11
     8 00:03:7A:1A:03:56 "MT_FREE"            1         0.00167         600       11
     9 38:1A:52:0D:84:D7 "EBFCD57F-EE8…     704         1.20            587.      11
    10 0A:C5:E1:DB:17:7B "AndroidAP177…    1251         2.39            523.      11

``` r
cat("Максимальная частота beacon-фреймов:", round(max(beacon_analysis$beacon_rate, na.rm = TRUE), 1), "в час\n")
```

    Максимальная частота beacon-фреймов: 3085.7 в час

### Данные клиентов

1.  Определить производителя для каждого обнаруженного устройства.

``` r
client_with_manufacturer <- client_data_clean %>%
  mutate(OUI = str_sub(Station_MAC, 1, 8)) %>%
  select(Station_MAC, OUI, Probed_ESSIDs) %>%
  distinct()

print(head(client_with_manufacturer, 10))
```

    # A tibble: 10 × 3
       Station_MAC       OUI      Probed_ESSIDs
       <chr>             <chr>    <chr>        
     1 CA:66:3B:8F:56:DD CA:66:3B C322U13 3965 
     2 96:35:2D:3D:85:E6 96:35:2D IT2 Wireless 
     3 5C:3A:45:9E:1A:7B 5C:3A:45 C322U21 0566 
     4 C0:E4:34:D8:E7:E5 C0:E4:34 C322U13 3965 
     5 5E:8E:A6:5E:34:81 5E:8E:A6 <NA>         
     6 10:51:07:CB:33:E7 10:51:07 <NA>         
     7 68:54:5A:40:35:9E 68:54:5A C322U06 5179 
     8 74:4C:A1:70:CE:F7 74:4C:A1 <NA>         
     9 8A:A3:5A:33:76:57 8A:A3:5A <NA>         
    10 CA:54:C4:8B:B5:3A CA:54:C4 GIVC         

``` r
cat("Всего уникальных производителей клиентских устройств:", n_distinct(client_with_manufacturer$OUI), "\n\n")
```

    Всего уникальных производителей клиентских устройств: 11792 

1.  Обнаружить устройства, которые НЕ рандомизируют свой MAC адрес.

``` r
non_randomized <- client_data_clean %>%
  mutate(
    second_char = str_sub(Station_MAC, 2, 2),
    is_randomized = second_char %in% c("2", "6", "a", "e", "A", "E")
  ) %>%
  filter(!is_randomized) %>%
  select(Station_MAC, First_time_seen, Last_time_seen, Power, packets, Probed_ESSIDs) %>%
  arrange(desc(packets))

print(head(non_randomized, 10))
```

    # A tibble: 10 × 6
       Station_MAC       First_time_seen     Last_time_seen      Power packets
       <chr>             <dttm>              <dttm>              <dbl>   <dbl>
     1 00:95:69:E7:7D:21 2023-07-28 09:13:15 2023-07-28 11:56:17   -33    8171
     2 00:95:69:E7:7C:ED 2023-07-28 09:13:11 2023-07-28 11:56:13   -55    4096
     3 00:95:69:E7:7F:35 2023-07-28 09:13:11 2023-07-28 11:56:07   -69    2245
     4 98:F6:21:72:9E:D6 2023-07-28 10:41:15 2023-07-28 11:56:14   -59    2143
     5 C0:E4:34:D8:E7:E5 2023-07-28 09:13:03 2023-07-28 11:53:16   -61     958
     6 74:DF:BF:7B:00:19 2023-07-28 09:45:48 2023-07-28 10:31:11   -65     911
     7 50:3E:AA:33:52:EC 2023-07-28 09:48:25 2023-07-28 11:56:06   -53     862
     8 14:13:33:59:9F:AB 2023-07-28 09:13:12 2023-07-28 10:26:21   -57     849
     9 04:8C:9A:0B:40:EA 2023-07-28 10:27:47 2023-07-28 11:55:51   -73     756
    10 BC:F1:71:D5:0E:53 2023-07-28 09:13:17 2023-07-28 11:50:10   -35     675
    # ℹ 1 more variable: Probed_ESSIDs <chr>

``` r
cat("Устройств без рандомизации MAC:", nrow(non_randomized), "\n")
```

    Устройств без рандомизации MAC: 220 

``` r
cat("Процент от общего числа:", round(nrow(non_randomized)/nrow(client_data_clean)*100, 1), "%\n")
```

    Процент от общего числа: 1.8 %

1.  Кластеризовать запросы от устройств к точкам доступа по их именам.
    Определить время появления устройства в зоне радиовидимости и время
    выхода его из нее.

``` r
signal_stability <- client_data_clean %>% 
  filter(!is.na(Probed_ESSIDs) & Probed_ESSIDs != "") %>% 
  group_by(Station_MAC, Probed_ESSIDs) %>% 
  summarise(
    mean_power = mean(Power, na.rm = TRUE),
    sd_power = sd(Power, na.rm = TRUE),
    n = n(),
    first_seen = min(First_time_seen),
    last_seen = max(Last_time_seen),
    .groups = "drop"
  ) %>% 
  arrange(sd_power)

signal_stability
```

    # A tibble: 1,477 × 7
       Station_MAC       Probed_ESSIDs mean_power sd_power     n first_seen         
       <chr>             <chr>              <dbl>    <dbl> <int> <dttm>             
     1 00:90:4C:E6:54:54 Redmi                -65       NA     1 2023-07-28 09:16:59
     2 00:95:69:E7:7C:ED nvripcsuite          -55       NA     1 2023-07-28 09:13:11
     3 00:95:69:E7:7D:21 nvripcsuite          -33       NA     1 2023-07-28 09:13:15
     4 00:95:69:E7:7F:35 nvripcsuite          -69       NA     1 2023-07-28 09:13:11
     5 00:F4:8D:F7:C5:19 Redmi 12             -73       NA     1 2023-07-28 10:45:04
     6 02:00:00:00:00:00 xt3 w64dtgv5…        -67       NA     1 2023-07-28 09:54:40
     7 02:06:2B:A5:0C:31 Avenue611            -65       NA     1 2023-07-28 09:55:12
     8 02:1D:0F:A4:94:74 iPhone (Дима…        -61       NA     1 2023-07-28 09:57:08
     9 02:32:DC:56:5C:82 Timo Resort          -84       NA     1 2023-07-28 10:58:23
    10 02:35:E9:C2:44:5F iPhone (Макс…        -88       NA     1 2023-07-28 10:00:55
    # ℹ 1,467 more rows
    # ℹ 1 more variable: last_seen <dttm>

1.  Оценить стабильность уровня сигнала внури кластера во времени.
    Выявить наиболее стабильный кластер.

``` r
# 4. Оценка стабильности уровня сигнала внутри кластера во времени

signal_stability <- signal_stability %>% mutate(stability_score = 1 / (sd_power + 1e-6))
most_stable_cluster <- signal_stability %>% arrange(desc(stability_score)) %>% slice(1)
most_stable_cluster
```

    # A tibble: 1 × 8
      Station_MAC       Probed_ESSIDs mean_power sd_power     n first_seen         
      <chr>             <chr>              <dbl>    <dbl> <int> <dttm>             
    1 00:90:4C:E6:54:54 Redmi                -65       NA     1 2023-07-28 09:16:59
    # ℹ 2 more variables: last_seen <dttm>, stability_score <dbl>

## Оценка результата

В результате лабораторной работы мы получили знания о методах
исследования радиоэлектронной обстановки, составили представление о
механизмах работы Wi-Fi сетей, закрепили практические навыки
использования языка программирования R и знания основных функций
обработки данных экосистемы tidyverse.

## Вывод

Таким образом, мы научились провоить анализ журналов с использованием
программного пакета dplyr.
