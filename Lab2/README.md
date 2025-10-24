# Основы обработки данных с помощью R и Dplyr
niki-tos29@yandex.ru

## Цель работы

1.  Развить практические навыки использования языка программирования R
    для обработки данных
2.  Закрепить знания базовых типов данных языка R
3.  Развить практические навыки использования функций обработки данных
    пакета dplyr – функции select(), filter(), mutate(), arrange(),
    group_by()

## Исходные данные

1.  Программное обеспечение Windows 11 Pro
2.  Rstudio Desktop
3.  Интерпретатор языка R 4.5.1
4.  Программный пакет dplyr

## План

1.  Загрузка пакета dplyr  
2.  Выполнение заданий практической работы
3.  Формирование отчета

## Шаги:

### Установка пакета dplyr, загрузка датасета.

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
        C:\Users\Никита\AppData\Local\Temp\Rtmpc5zTg4\downloaded_packages

``` r
library(dplyr)
```


    Присоединяю пакет: 'dplyr'

    Следующие объекты скрыты от 'package:stats':

        filter, lag

    Следующие объекты скрыты от 'package:base':

        intersect, setdiff, setequal, union

``` r
data(starwars)
```

1.  Сколько строк в датафрейме?

``` r
nrow(starwars)
```

    [1] 87

1.  Сколько столбцов в датафрейме?

``` r
ncol(starwars)
```

    [1] 14

1.  Как просмотреть примерный вид датафрейма?

``` r
starwars %>% glimpse()
```

    Rows: 87
    Columns: 14
    $ name       <chr> "Luke Skywalker", "C-3PO", "R2-D2", "Darth Vader", "Leia Or…
    $ height     <int> 172, 167, 96, 202, 150, 178, 165, 97, 183, 182, 188, 180, 2…
    $ mass       <dbl> 77.0, 75.0, 32.0, 136.0, 49.0, 120.0, 75.0, 32.0, 84.0, 77.…
    $ hair_color <chr> "blond", NA, NA, "none", "brown", "brown, grey", "brown", N…
    $ skin_color <chr> "fair", "gold", "white, blue", "white", "light", "light", "…
    $ eye_color  <chr> "blue", "yellow", "red", "yellow", "brown", "blue", "blue",…
    $ birth_year <dbl> 19.0, 112.0, 33.0, 41.9, 19.0, 52.0, 47.0, NA, 24.0, 57.0, …
    $ sex        <chr> "male", "none", "none", "male", "female", "male", "female",…
    $ gender     <chr> "masculine", "masculine", "masculine", "masculine", "femini…
    $ homeworld  <chr> "Tatooine", "Tatooine", "Naboo", "Tatooine", "Alderaan", "T…
    $ species    <chr> "Human", "Droid", "Droid", "Human", "Human", "Human", "Huma…
    $ films      <list> <"A New Hope", "The Empire Strikes Back", "Return of the J…
    $ vehicles   <list> <"Snowspeeder", "Imperial Speeder Bike">, <>, <>, <>, "Imp…
    $ starships  <list> <"X-wing", "Imperial shuttle">, <>, <>, "TIE Advanced x1",…

1.  Сколько уникальных рас персонажей (species) представлено в данных?

``` r
starwars %>% distinct(species) |> knitr::kable(format='markdown')
```

<table>
<thead>
<tr>
<th style="text-align: left;">species</th>
</tr>
</thead>
<tbody>
<tr>
<td style="text-align: left;">Human</td>
</tr>
<tr>
<td style="text-align: left;">Droid</td>
</tr>
<tr>
<td style="text-align: left;">Wookiee</td>
</tr>
<tr>
<td style="text-align: left;">Rodian</td>
</tr>
<tr>
<td style="text-align: left;">Hutt</td>
</tr>
<tr>
<td style="text-align: left;">NA</td>
</tr>
<tr>
<td style="text-align: left;">Yoda’s species</td>
</tr>
<tr>
<td style="text-align: left;">Trandoshan</td>
</tr>
<tr>
<td style="text-align: left;">Mon Calamari</td>
</tr>
<tr>
<td style="text-align: left;">Ewok</td>
</tr>
<tr>
<td style="text-align: left;">Sullustan</td>
</tr>
<tr>
<td style="text-align: left;">Neimodian</td>
</tr>
<tr>
<td style="text-align: left;">Gungan</td>
</tr>
<tr>
<td style="text-align: left;">Toydarian</td>
</tr>
<tr>
<td style="text-align: left;">Dug</td>
</tr>
<tr>
<td style="text-align: left;">Zabrak</td>
</tr>
<tr>
<td style="text-align: left;">Twi’lek</td>
</tr>
<tr>
<td style="text-align: left;">Aleena</td>
</tr>
<tr>
<td style="text-align: left;">Vulptereen</td>
</tr>
<tr>
<td style="text-align: left;">Xexto</td>
</tr>
<tr>
<td style="text-align: left;">Toong</td>
</tr>
<tr>
<td style="text-align: left;">Cerean</td>
</tr>
<tr>
<td style="text-align: left;">Nautolan</td>
</tr>
<tr>
<td style="text-align: left;">Tholothian</td>
</tr>
<tr>
<td style="text-align: left;">Iktotchi</td>
</tr>
<tr>
<td style="text-align: left;">Quermian</td>
</tr>
<tr>
<td style="text-align: left;">Kel Dor</td>
</tr>
<tr>
<td style="text-align: left;">Chagrian</td>
</tr>
<tr>
<td style="text-align: left;">Geonosian</td>
</tr>
<tr>
<td style="text-align: left;">Mirialan</td>
</tr>
<tr>
<td style="text-align: left;">Clawdite</td>
</tr>
<tr>
<td style="text-align: left;">Besalisk</td>
</tr>
<tr>
<td style="text-align: left;">Kaminoan</td>
</tr>
<tr>
<td style="text-align: left;">Skakoan</td>
</tr>
<tr>
<td style="text-align: left;">Muun</td>
</tr>
<tr>
<td style="text-align: left;">Togruta</td>
</tr>
<tr>
<td style="text-align: left;">Kaleesh</td>
</tr>
<tr>
<td style="text-align: left;">Pau’an</td>
</tr>
</tbody>
</table>

1.  Найти самого высокого персонажа.

``` r
print(starwars %>% select(name, height) %>% arrange(desc(height)), n=1)
```

    # A tibble: 87 × 2
      name        height
      <chr>        <int>
    1 Yarael Poof    264
    # ℹ 86 more rows

1.  Найти всех персонажей ниже 170.

``` r
select(starwars, name, height) %>% filter(height>170) |> knitr::kable(format='markdown')
```

<table>
<thead>
<tr>
<th style="text-align: left;">name</th>
<th style="text-align: right;">height</th>
</tr>
</thead>
<tbody>
<tr>
<td style="text-align: left;">Luke Skywalker</td>
<td style="text-align: right;">172</td>
</tr>
<tr>
<td style="text-align: left;">Darth Vader</td>
<td style="text-align: right;">202</td>
</tr>
<tr>
<td style="text-align: left;">Owen Lars</td>
<td style="text-align: right;">178</td>
</tr>
<tr>
<td style="text-align: left;">Biggs Darklighter</td>
<td style="text-align: right;">183</td>
</tr>
<tr>
<td style="text-align: left;">Obi-Wan Kenobi</td>
<td style="text-align: right;">182</td>
</tr>
<tr>
<td style="text-align: left;">Anakin Skywalker</td>
<td style="text-align: right;">188</td>
</tr>
<tr>
<td style="text-align: left;">Wilhuff Tarkin</td>
<td style="text-align: right;">180</td>
</tr>
<tr>
<td style="text-align: left;">Chewbacca</td>
<td style="text-align: right;">228</td>
</tr>
<tr>
<td style="text-align: left;">Han Solo</td>
<td style="text-align: right;">180</td>
</tr>
<tr>
<td style="text-align: left;">Greedo</td>
<td style="text-align: right;">173</td>
</tr>
<tr>
<td style="text-align: left;">Jabba Desilijic Tiure</td>
<td style="text-align: right;">175</td>
</tr>
<tr>
<td style="text-align: left;">Jek Tono Porkins</td>
<td style="text-align: right;">180</td>
</tr>
<tr>
<td style="text-align: left;">Boba Fett</td>
<td style="text-align: right;">183</td>
</tr>
<tr>
<td style="text-align: left;">IG-88</td>
<td style="text-align: right;">200</td>
</tr>
<tr>
<td style="text-align: left;">Bossk</td>
<td style="text-align: right;">190</td>
</tr>
<tr>
<td style="text-align: left;">Lando Calrissian</td>
<td style="text-align: right;">177</td>
</tr>
<tr>
<td style="text-align: left;">Lobot</td>
<td style="text-align: right;">175</td>
</tr>
<tr>
<td style="text-align: left;">Ackbar</td>
<td style="text-align: right;">180</td>
</tr>
<tr>
<td style="text-align: left;">Qui-Gon Jinn</td>
<td style="text-align: right;">193</td>
</tr>
<tr>
<td style="text-align: left;">Nute Gunray</td>
<td style="text-align: right;">191</td>
</tr>
<tr>
<td style="text-align: left;">Padmé Amidala</td>
<td style="text-align: right;">185</td>
</tr>
<tr>
<td style="text-align: left;">Jar Jar Binks</td>
<td style="text-align: right;">196</td>
</tr>
<tr>
<td style="text-align: left;">Roos Tarpals</td>
<td style="text-align: right;">224</td>
</tr>
<tr>
<td style="text-align: left;">Rugor Nass</td>
<td style="text-align: right;">206</td>
</tr>
<tr>
<td style="text-align: left;">Ric Olié</td>
<td style="text-align: right;">183</td>
</tr>
<tr>
<td style="text-align: left;">Quarsh Panaka</td>
<td style="text-align: right;">183</td>
</tr>
<tr>
<td style="text-align: left;">Darth Maul</td>
<td style="text-align: right;">175</td>
</tr>
<tr>
<td style="text-align: left;">Bib Fortuna</td>
<td style="text-align: right;">180</td>
</tr>
<tr>
<td style="text-align: left;">Ayla Secura</td>
<td style="text-align: right;">178</td>
</tr>
<tr>
<td style="text-align: left;">Mace Windu</td>
<td style="text-align: right;">188</td>
</tr>
<tr>
<td style="text-align: left;">Ki-Adi-Mundi</td>
<td style="text-align: right;">198</td>
</tr>
<tr>
<td style="text-align: left;">Kit Fisto</td>
<td style="text-align: right;">196</td>
</tr>
<tr>
<td style="text-align: left;">Eeth Koth</td>
<td style="text-align: right;">171</td>
</tr>
<tr>
<td style="text-align: left;">Adi Gallia</td>
<td style="text-align: right;">184</td>
</tr>
<tr>
<td style="text-align: left;">Saesee Tiin</td>
<td style="text-align: right;">188</td>
</tr>
<tr>
<td style="text-align: left;">Yarael Poof</td>
<td style="text-align: right;">264</td>
</tr>
<tr>
<td style="text-align: left;">Plo Koon</td>
<td style="text-align: right;">188</td>
</tr>
<tr>
<td style="text-align: left;">Mas Amedda</td>
<td style="text-align: right;">196</td>
</tr>
<tr>
<td style="text-align: left;">Gregar Typho</td>
<td style="text-align: right;">185</td>
</tr>
<tr>
<td style="text-align: left;">Cliegg Lars</td>
<td style="text-align: right;">183</td>
</tr>
<tr>
<td style="text-align: left;">Poggle the Lesser</td>
<td style="text-align: right;">183</td>
</tr>
<tr>
<td style="text-align: left;">Dooku</td>
<td style="text-align: right;">193</td>
</tr>
<tr>
<td style="text-align: left;">Bail Prestor Organa</td>
<td style="text-align: right;">191</td>
</tr>
<tr>
<td style="text-align: left;">Jango Fett</td>
<td style="text-align: right;">183</td>
</tr>
<tr>
<td style="text-align: left;">Dexter Jettster</td>
<td style="text-align: right;">198</td>
</tr>
<tr>
<td style="text-align: left;">Lama Su</td>
<td style="text-align: right;">229</td>
</tr>
<tr>
<td style="text-align: left;">Taun We</td>
<td style="text-align: right;">213</td>
</tr>
<tr>
<td style="text-align: left;">Wat Tambor</td>
<td style="text-align: right;">193</td>
</tr>
<tr>
<td style="text-align: left;">San Hill</td>
<td style="text-align: right;">191</td>
</tr>
<tr>
<td style="text-align: left;">Shaak Ti</td>
<td style="text-align: right;">178</td>
</tr>
<tr>
<td style="text-align: left;">Grievous</td>
<td style="text-align: right;">216</td>
</tr>
<tr>
<td style="text-align: left;">Tarfful</td>
<td style="text-align: right;">234</td>
</tr>
<tr>
<td style="text-align: left;">Raymus Antilles</td>
<td style="text-align: right;">188</td>
</tr>
<tr>
<td style="text-align: left;">Sly Moore</td>
<td style="text-align: right;">178</td>
</tr>
<tr>
<td style="text-align: left;">Tion Medon</td>
<td style="text-align: right;">206</td>
</tr>
</tbody>
</table>

1.  Подсчитать ИМТ (индекс массы тела) для всех персонажей.

``` r
mutate(starwars, BMI=mass/((height/100)^2)) %>% select(name, height, mass, BMI) |> knitr::kable(format='markdown')
```

<table>
<thead>
<tr>
<th style="text-align: left;">name</th>
<th style="text-align: right;">height</th>
<th style="text-align: right;">mass</th>
<th style="text-align: right;">BMI</th>
</tr>
</thead>
<tbody>
<tr>
<td style="text-align: left;">Luke Skywalker</td>
<td style="text-align: right;">172</td>
<td style="text-align: right;">77.0</td>
<td style="text-align: right;">26.02758</td>
</tr>
<tr>
<td style="text-align: left;">C-3PO</td>
<td style="text-align: right;">167</td>
<td style="text-align: right;">75.0</td>
<td style="text-align: right;">26.89232</td>
</tr>
<tr>
<td style="text-align: left;">R2-D2</td>
<td style="text-align: right;">96</td>
<td style="text-align: right;">32.0</td>
<td style="text-align: right;">34.72222</td>
</tr>
<tr>
<td style="text-align: left;">Darth Vader</td>
<td style="text-align: right;">202</td>
<td style="text-align: right;">136.0</td>
<td style="text-align: right;">33.33007</td>
</tr>
<tr>
<td style="text-align: left;">Leia Organa</td>
<td style="text-align: right;">150</td>
<td style="text-align: right;">49.0</td>
<td style="text-align: right;">21.77778</td>
</tr>
<tr>
<td style="text-align: left;">Owen Lars</td>
<td style="text-align: right;">178</td>
<td style="text-align: right;">120.0</td>
<td style="text-align: right;">37.87401</td>
</tr>
<tr>
<td style="text-align: left;">Beru Whitesun Lars</td>
<td style="text-align: right;">165</td>
<td style="text-align: right;">75.0</td>
<td style="text-align: right;">27.54821</td>
</tr>
<tr>
<td style="text-align: left;">R5-D4</td>
<td style="text-align: right;">97</td>
<td style="text-align: right;">32.0</td>
<td style="text-align: right;">34.00999</td>
</tr>
<tr>
<td style="text-align: left;">Biggs Darklighter</td>
<td style="text-align: right;">183</td>
<td style="text-align: right;">84.0</td>
<td style="text-align: right;">25.08286</td>
</tr>
<tr>
<td style="text-align: left;">Obi-Wan Kenobi</td>
<td style="text-align: right;">182</td>
<td style="text-align: right;">77.0</td>
<td style="text-align: right;">23.24598</td>
</tr>
<tr>
<td style="text-align: left;">Anakin Skywalker</td>
<td style="text-align: right;">188</td>
<td style="text-align: right;">84.0</td>
<td style="text-align: right;">23.76641</td>
</tr>
<tr>
<td style="text-align: left;">Wilhuff Tarkin</td>
<td style="text-align: right;">180</td>
<td style="text-align: right;">NA</td>
<td style="text-align: right;">NA</td>
</tr>
<tr>
<td style="text-align: left;">Chewbacca</td>
<td style="text-align: right;">228</td>
<td style="text-align: right;">112.0</td>
<td style="text-align: right;">21.54509</td>
</tr>
<tr>
<td style="text-align: left;">Han Solo</td>
<td style="text-align: right;">180</td>
<td style="text-align: right;">80.0</td>
<td style="text-align: right;">24.69136</td>
</tr>
<tr>
<td style="text-align: left;">Greedo</td>
<td style="text-align: right;">173</td>
<td style="text-align: right;">74.0</td>
<td style="text-align: right;">24.72518</td>
</tr>
<tr>
<td style="text-align: left;">Jabba Desilijic Tiure</td>
<td style="text-align: right;">175</td>
<td style="text-align: right;">1358.0</td>
<td style="text-align: right;">443.42857</td>
</tr>
<tr>
<td style="text-align: left;">Wedge Antilles</td>
<td style="text-align: right;">170</td>
<td style="text-align: right;">77.0</td>
<td style="text-align: right;">26.64360</td>
</tr>
<tr>
<td style="text-align: left;">Jek Tono Porkins</td>
<td style="text-align: right;">180</td>
<td style="text-align: right;">110.0</td>
<td style="text-align: right;">33.95062</td>
</tr>
<tr>
<td style="text-align: left;">Yoda</td>
<td style="text-align: right;">66</td>
<td style="text-align: right;">17.0</td>
<td style="text-align: right;">39.02663</td>
</tr>
<tr>
<td style="text-align: left;">Palpatine</td>
<td style="text-align: right;">170</td>
<td style="text-align: right;">75.0</td>
<td style="text-align: right;">25.95156</td>
</tr>
<tr>
<td style="text-align: left;">Boba Fett</td>
<td style="text-align: right;">183</td>
<td style="text-align: right;">78.2</td>
<td style="text-align: right;">23.35095</td>
</tr>
<tr>
<td style="text-align: left;">IG-88</td>
<td style="text-align: right;">200</td>
<td style="text-align: right;">140.0</td>
<td style="text-align: right;">35.00000</td>
</tr>
<tr>
<td style="text-align: left;">Bossk</td>
<td style="text-align: right;">190</td>
<td style="text-align: right;">113.0</td>
<td style="text-align: right;">31.30194</td>
</tr>
<tr>
<td style="text-align: left;">Lando Calrissian</td>
<td style="text-align: right;">177</td>
<td style="text-align: right;">79.0</td>
<td style="text-align: right;">25.21625</td>
</tr>
<tr>
<td style="text-align: left;">Lobot</td>
<td style="text-align: right;">175</td>
<td style="text-align: right;">79.0</td>
<td style="text-align: right;">25.79592</td>
</tr>
<tr>
<td style="text-align: left;">Ackbar</td>
<td style="text-align: right;">180</td>
<td style="text-align: right;">83.0</td>
<td style="text-align: right;">25.61728</td>
</tr>
<tr>
<td style="text-align: left;">Mon Mothma</td>
<td style="text-align: right;">150</td>
<td style="text-align: right;">NA</td>
<td style="text-align: right;">NA</td>
</tr>
<tr>
<td style="text-align: left;">Arvel Crynyd</td>
<td style="text-align: right;">NA</td>
<td style="text-align: right;">NA</td>
<td style="text-align: right;">NA</td>
</tr>
<tr>
<td style="text-align: left;">Wicket Systri Warrick</td>
<td style="text-align: right;">88</td>
<td style="text-align: right;">20.0</td>
<td style="text-align: right;">25.82645</td>
</tr>
<tr>
<td style="text-align: left;">Nien Nunb</td>
<td style="text-align: right;">160</td>
<td style="text-align: right;">68.0</td>
<td style="text-align: right;">26.56250</td>
</tr>
<tr>
<td style="text-align: left;">Qui-Gon Jinn</td>
<td style="text-align: right;">193</td>
<td style="text-align: right;">89.0</td>
<td style="text-align: right;">23.89326</td>
</tr>
<tr>
<td style="text-align: left;">Nute Gunray</td>
<td style="text-align: right;">191</td>
<td style="text-align: right;">90.0</td>
<td style="text-align: right;">24.67038</td>
</tr>
<tr>
<td style="text-align: left;">Finis Valorum</td>
<td style="text-align: right;">170</td>
<td style="text-align: right;">NA</td>
<td style="text-align: right;">NA</td>
</tr>
<tr>
<td style="text-align: left;">Padmé Amidala</td>
<td style="text-align: right;">185</td>
<td style="text-align: right;">45.0</td>
<td style="text-align: right;">13.14828</td>
</tr>
<tr>
<td style="text-align: left;">Jar Jar Binks</td>
<td style="text-align: right;">196</td>
<td style="text-align: right;">66.0</td>
<td style="text-align: right;">17.18034</td>
</tr>
<tr>
<td style="text-align: left;">Roos Tarpals</td>
<td style="text-align: right;">224</td>
<td style="text-align: right;">82.0</td>
<td style="text-align: right;">16.34247</td>
</tr>
<tr>
<td style="text-align: left;">Rugor Nass</td>
<td style="text-align: right;">206</td>
<td style="text-align: right;">NA</td>
<td style="text-align: right;">NA</td>
</tr>
<tr>
<td style="text-align: left;">Ric Olié</td>
<td style="text-align: right;">183</td>
<td style="text-align: right;">NA</td>
<td style="text-align: right;">NA</td>
</tr>
<tr>
<td style="text-align: left;">Watto</td>
<td style="text-align: right;">137</td>
<td style="text-align: right;">NA</td>
<td style="text-align: right;">NA</td>
</tr>
<tr>
<td style="text-align: left;">Sebulba</td>
<td style="text-align: right;">112</td>
<td style="text-align: right;">40.0</td>
<td style="text-align: right;">31.88776</td>
</tr>
<tr>
<td style="text-align: left;">Quarsh Panaka</td>
<td style="text-align: right;">183</td>
<td style="text-align: right;">NA</td>
<td style="text-align: right;">NA</td>
</tr>
<tr>
<td style="text-align: left;">Shmi Skywalker</td>
<td style="text-align: right;">163</td>
<td style="text-align: right;">NA</td>
<td style="text-align: right;">NA</td>
</tr>
<tr>
<td style="text-align: left;">Darth Maul</td>
<td style="text-align: right;">175</td>
<td style="text-align: right;">80.0</td>
<td style="text-align: right;">26.12245</td>
</tr>
<tr>
<td style="text-align: left;">Bib Fortuna</td>
<td style="text-align: right;">180</td>
<td style="text-align: right;">NA</td>
<td style="text-align: right;">NA</td>
</tr>
<tr>
<td style="text-align: left;">Ayla Secura</td>
<td style="text-align: right;">178</td>
<td style="text-align: right;">55.0</td>
<td style="text-align: right;">17.35892</td>
</tr>
<tr>
<td style="text-align: left;">Ratts Tyerel</td>
<td style="text-align: right;">79</td>
<td style="text-align: right;">15.0</td>
<td style="text-align: right;">24.03461</td>
</tr>
<tr>
<td style="text-align: left;">Dud Bolt</td>
<td style="text-align: right;">94</td>
<td style="text-align: right;">45.0</td>
<td style="text-align: right;">50.92802</td>
</tr>
<tr>
<td style="text-align: left;">Gasgano</td>
<td style="text-align: right;">122</td>
<td style="text-align: right;">NA</td>
<td style="text-align: right;">NA</td>
</tr>
<tr>
<td style="text-align: left;">Ben Quadinaros</td>
<td style="text-align: right;">163</td>
<td style="text-align: right;">65.0</td>
<td style="text-align: right;">24.46460</td>
</tr>
<tr>
<td style="text-align: left;">Mace Windu</td>
<td style="text-align: right;">188</td>
<td style="text-align: right;">84.0</td>
<td style="text-align: right;">23.76641</td>
</tr>
<tr>
<td style="text-align: left;">Ki-Adi-Mundi</td>
<td style="text-align: right;">198</td>
<td style="text-align: right;">82.0</td>
<td style="text-align: right;">20.91623</td>
</tr>
<tr>
<td style="text-align: left;">Kit Fisto</td>
<td style="text-align: right;">196</td>
<td style="text-align: right;">87.0</td>
<td style="text-align: right;">22.64681</td>
</tr>
<tr>
<td style="text-align: left;">Eeth Koth</td>
<td style="text-align: right;">171</td>
<td style="text-align: right;">NA</td>
<td style="text-align: right;">NA</td>
</tr>
<tr>
<td style="text-align: left;">Adi Gallia</td>
<td style="text-align: right;">184</td>
<td style="text-align: right;">50.0</td>
<td style="text-align: right;">14.76843</td>
</tr>
<tr>
<td style="text-align: left;">Saesee Tiin</td>
<td style="text-align: right;">188</td>
<td style="text-align: right;">NA</td>
<td style="text-align: right;">NA</td>
</tr>
<tr>
<td style="text-align: left;">Yarael Poof</td>
<td style="text-align: right;">264</td>
<td style="text-align: right;">NA</td>
<td style="text-align: right;">NA</td>
</tr>
<tr>
<td style="text-align: left;">Plo Koon</td>
<td style="text-align: right;">188</td>
<td style="text-align: right;">80.0</td>
<td style="text-align: right;">22.63468</td>
</tr>
<tr>
<td style="text-align: left;">Mas Amedda</td>
<td style="text-align: right;">196</td>
<td style="text-align: right;">NA</td>
<td style="text-align: right;">NA</td>
</tr>
<tr>
<td style="text-align: left;">Gregar Typho</td>
<td style="text-align: right;">185</td>
<td style="text-align: right;">85.0</td>
<td style="text-align: right;">24.83565</td>
</tr>
<tr>
<td style="text-align: left;">Cordé</td>
<td style="text-align: right;">157</td>
<td style="text-align: right;">NA</td>
<td style="text-align: right;">NA</td>
</tr>
<tr>
<td style="text-align: left;">Cliegg Lars</td>
<td style="text-align: right;">183</td>
<td style="text-align: right;">NA</td>
<td style="text-align: right;">NA</td>
</tr>
<tr>
<td style="text-align: left;">Poggle the Lesser</td>
<td style="text-align: right;">183</td>
<td style="text-align: right;">80.0</td>
<td style="text-align: right;">23.88844</td>
</tr>
<tr>
<td style="text-align: left;">Luminara Unduli</td>
<td style="text-align: right;">170</td>
<td style="text-align: right;">56.2</td>
<td style="text-align: right;">19.44637</td>
</tr>
<tr>
<td style="text-align: left;">Barriss Offee</td>
<td style="text-align: right;">166</td>
<td style="text-align: right;">50.0</td>
<td style="text-align: right;">18.14487</td>
</tr>
<tr>
<td style="text-align: left;">Dormé</td>
<td style="text-align: right;">165</td>
<td style="text-align: right;">NA</td>
<td style="text-align: right;">NA</td>
</tr>
<tr>
<td style="text-align: left;">Dooku</td>
<td style="text-align: right;">193</td>
<td style="text-align: right;">80.0</td>
<td style="text-align: right;">21.47709</td>
</tr>
<tr>
<td style="text-align: left;">Bail Prestor Organa</td>
<td style="text-align: right;">191</td>
<td style="text-align: right;">NA</td>
<td style="text-align: right;">NA</td>
</tr>
<tr>
<td style="text-align: left;">Jango Fett</td>
<td style="text-align: right;">183</td>
<td style="text-align: right;">79.0</td>
<td style="text-align: right;">23.58984</td>
</tr>
<tr>
<td style="text-align: left;">Zam Wesell</td>
<td style="text-align: right;">168</td>
<td style="text-align: right;">55.0</td>
<td style="text-align: right;">19.48696</td>
</tr>
<tr>
<td style="text-align: left;">Dexter Jettster</td>
<td style="text-align: right;">198</td>
<td style="text-align: right;">102.0</td>
<td style="text-align: right;">26.01775</td>
</tr>
<tr>
<td style="text-align: left;">Lama Su</td>
<td style="text-align: right;">229</td>
<td style="text-align: right;">88.0</td>
<td style="text-align: right;">16.78076</td>
</tr>
<tr>
<td style="text-align: left;">Taun We</td>
<td style="text-align: right;">213</td>
<td style="text-align: right;">NA</td>
<td style="text-align: right;">NA</td>
</tr>
<tr>
<td style="text-align: left;">Jocasta Nu</td>
<td style="text-align: right;">167</td>
<td style="text-align: right;">NA</td>
<td style="text-align: right;">NA</td>
</tr>
<tr>
<td style="text-align: left;">R4-P17</td>
<td style="text-align: right;">96</td>
<td style="text-align: right;">NA</td>
<td style="text-align: right;">NA</td>
</tr>
<tr>
<td style="text-align: left;">Wat Tambor</td>
<td style="text-align: right;">193</td>
<td style="text-align: right;">48.0</td>
<td style="text-align: right;">12.88625</td>
</tr>
<tr>
<td style="text-align: left;">San Hill</td>
<td style="text-align: right;">191</td>
<td style="text-align: right;">NA</td>
<td style="text-align: right;">NA</td>
</tr>
<tr>
<td style="text-align: left;">Shaak Ti</td>
<td style="text-align: right;">178</td>
<td style="text-align: right;">57.0</td>
<td style="text-align: right;">17.99015</td>
</tr>
<tr>
<td style="text-align: left;">Grievous</td>
<td style="text-align: right;">216</td>
<td style="text-align: right;">159.0</td>
<td style="text-align: right;">34.07922</td>
</tr>
<tr>
<td style="text-align: left;">Tarfful</td>
<td style="text-align: right;">234</td>
<td style="text-align: right;">136.0</td>
<td style="text-align: right;">24.83746</td>
</tr>
<tr>
<td style="text-align: left;">Raymus Antilles</td>
<td style="text-align: right;">188</td>
<td style="text-align: right;">79.0</td>
<td style="text-align: right;">22.35174</td>
</tr>
<tr>
<td style="text-align: left;">Sly Moore</td>
<td style="text-align: right;">178</td>
<td style="text-align: right;">48.0</td>
<td style="text-align: right;">15.14960</td>
</tr>
<tr>
<td style="text-align: left;">Tion Medon</td>
<td style="text-align: right;">206</td>
<td style="text-align: right;">80.0</td>
<td style="text-align: right;">18.85192</td>
</tr>
<tr>
<td style="text-align: left;">Finn</td>
<td style="text-align: right;">NA</td>
<td style="text-align: right;">NA</td>
<td style="text-align: right;">NA</td>
</tr>
<tr>
<td style="text-align: left;">Rey</td>
<td style="text-align: right;">NA</td>
<td style="text-align: right;">NA</td>
<td style="text-align: right;">NA</td>
</tr>
<tr>
<td style="text-align: left;">Poe Dameron</td>
<td style="text-align: right;">NA</td>
<td style="text-align: right;">NA</td>
<td style="text-align: right;">NA</td>
</tr>
<tr>
<td style="text-align: left;">BB8</td>
<td style="text-align: right;">NA</td>
<td style="text-align: right;">NA</td>
<td style="text-align: right;">NA</td>
</tr>
<tr>
<td style="text-align: left;">Captain Phasma</td>
<td style="text-align: right;">NA</td>
<td style="text-align: right;">NA</td>
<td style="text-align: right;">NA</td>
</tr>
</tbody>
</table>

1.  Найти 10 самых “вытянутых” персонажей. “Вытянутость” оценить по
    отношению массы (mass) к росту (height) персонажей.

``` r
knitr::kable(format='markdown', head(mutate(starwars, my_param=mass/height) %>% arrange(my_param) %>% select(name, height, mass, my_param), n=10))
```

<table>
<thead>
<tr>
<th style="text-align: left;">name</th>
<th style="text-align: right;">height</th>
<th style="text-align: right;">mass</th>
<th style="text-align: right;">my_param</th>
</tr>
</thead>
<tbody>
<tr>
<td style="text-align: left;">Ratts Tyerel</td>
<td style="text-align: right;">79</td>
<td style="text-align: right;">15</td>
<td style="text-align: right;">0.1898734</td>
</tr>
<tr>
<td style="text-align: left;">Wicket Systri Warrick</td>
<td style="text-align: right;">88</td>
<td style="text-align: right;">20</td>
<td style="text-align: right;">0.2272727</td>
</tr>
<tr>
<td style="text-align: left;">Padmé Amidala</td>
<td style="text-align: right;">185</td>
<td style="text-align: right;">45</td>
<td style="text-align: right;">0.2432432</td>
</tr>
<tr>
<td style="text-align: left;">Wat Tambor</td>
<td style="text-align: right;">193</td>
<td style="text-align: right;">48</td>
<td style="text-align: right;">0.2487047</td>
</tr>
<tr>
<td style="text-align: left;">Yoda</td>
<td style="text-align: right;">66</td>
<td style="text-align: right;">17</td>
<td style="text-align: right;">0.2575758</td>
</tr>
<tr>
<td style="text-align: left;">Sly Moore</td>
<td style="text-align: right;">178</td>
<td style="text-align: right;">48</td>
<td style="text-align: right;">0.2696629</td>
</tr>
<tr>
<td style="text-align: left;">Adi Gallia</td>
<td style="text-align: right;">184</td>
<td style="text-align: right;">50</td>
<td style="text-align: right;">0.2717391</td>
</tr>
<tr>
<td style="text-align: left;">Barriss Offee</td>
<td style="text-align: right;">166</td>
<td style="text-align: right;">50</td>
<td style="text-align: right;">0.3012048</td>
</tr>
<tr>
<td style="text-align: left;">Ayla Secura</td>
<td style="text-align: right;">178</td>
<td style="text-align: right;">55</td>
<td style="text-align: right;">0.3089888</td>
</tr>
<tr>
<td style="text-align: left;">Shaak Ti</td>
<td style="text-align: right;">178</td>
<td style="text-align: right;">57</td>
<td style="text-align: right;">0.3202247</td>
</tr>
</tbody>
</table>

1.  Найти средний возраст персонажей каждой расы вселенной Звездных
    войн.

``` r
starwars %>% group_by(species) %>% summarise(Avg_age = median(100+birth_year, na.rm = TRUE)) |> knitr::kable(format='markdown')
```

<table>
<thead>
<tr>
<th style="text-align: left;">species</th>
<th style="text-align: right;">Avg_age</th>
</tr>
</thead>
<tbody>
<tr>
<td style="text-align: left;">Aleena</td>
<td style="text-align: right;">NA</td>
</tr>
<tr>
<td style="text-align: left;">Besalisk</td>
<td style="text-align: right;">NA</td>
</tr>
<tr>
<td style="text-align: left;">Cerean</td>
<td style="text-align: right;">192</td>
</tr>
<tr>
<td style="text-align: left;">Chagrian</td>
<td style="text-align: right;">NA</td>
</tr>
<tr>
<td style="text-align: left;">Clawdite</td>
<td style="text-align: right;">NA</td>
</tr>
<tr>
<td style="text-align: left;">Droid</td>
<td style="text-align: right;">133</td>
</tr>
<tr>
<td style="text-align: left;">Dug</td>
<td style="text-align: right;">NA</td>
</tr>
<tr>
<td style="text-align: left;">Ewok</td>
<td style="text-align: right;">108</td>
</tr>
<tr>
<td style="text-align: left;">Geonosian</td>
<td style="text-align: right;">NA</td>
</tr>
<tr>
<td style="text-align: left;">Gungan</td>
<td style="text-align: right;">152</td>
</tr>
<tr>
<td style="text-align: left;">Human</td>
<td style="text-align: right;">150</td>
</tr>
<tr>
<td style="text-align: left;">Hutt</td>
<td style="text-align: right;">700</td>
</tr>
<tr>
<td style="text-align: left;">Iktotchi</td>
<td style="text-align: right;">NA</td>
</tr>
<tr>
<td style="text-align: left;">Kaleesh</td>
<td style="text-align: right;">NA</td>
</tr>
<tr>
<td style="text-align: left;">Kaminoan</td>
<td style="text-align: right;">NA</td>
</tr>
<tr>
<td style="text-align: left;">Kel Dor</td>
<td style="text-align: right;">122</td>
</tr>
<tr>
<td style="text-align: left;">Mirialan</td>
<td style="text-align: right;">149</td>
</tr>
<tr>
<td style="text-align: left;">Mon Calamari</td>
<td style="text-align: right;">141</td>
</tr>
<tr>
<td style="text-align: left;">Muun</td>
<td style="text-align: right;">NA</td>
</tr>
<tr>
<td style="text-align: left;">Nautolan</td>
<td style="text-align: right;">NA</td>
</tr>
<tr>
<td style="text-align: left;">Neimodian</td>
<td style="text-align: right;">NA</td>
</tr>
<tr>
<td style="text-align: left;">Pau’an</td>
<td style="text-align: right;">NA</td>
</tr>
<tr>
<td style="text-align: left;">Quermian</td>
<td style="text-align: right;">NA</td>
</tr>
<tr>
<td style="text-align: left;">Rodian</td>
<td style="text-align: right;">144</td>
</tr>
<tr>
<td style="text-align: left;">Skakoan</td>
<td style="text-align: right;">NA</td>
</tr>
<tr>
<td style="text-align: left;">Sullustan</td>
<td style="text-align: right;">NA</td>
</tr>
<tr>
<td style="text-align: left;">Tholothian</td>
<td style="text-align: right;">NA</td>
</tr>
<tr>
<td style="text-align: left;">Togruta</td>
<td style="text-align: right;">NA</td>
</tr>
<tr>
<td style="text-align: left;">Toong</td>
<td style="text-align: right;">NA</td>
</tr>
<tr>
<td style="text-align: left;">Toydarian</td>
<td style="text-align: right;">NA</td>
</tr>
<tr>
<td style="text-align: left;">Trandoshan</td>
<td style="text-align: right;">153</td>
</tr>
<tr>
<td style="text-align: left;">Twi’lek</td>
<td style="text-align: right;">148</td>
</tr>
<tr>
<td style="text-align: left;">Vulptereen</td>
<td style="text-align: right;">NA</td>
</tr>
<tr>
<td style="text-align: left;">Wookiee</td>
<td style="text-align: right;">300</td>
</tr>
<tr>
<td style="text-align: left;">Xexto</td>
<td style="text-align: right;">NA</td>
</tr>
<tr>
<td style="text-align: left;">Yoda’s species</td>
<td style="text-align: right;">996</td>
</tr>
<tr>
<td style="text-align: left;">Zabrak</td>
<td style="text-align: right;">154</td>
</tr>
<tr>
<td style="text-align: left;">NA</td>
<td style="text-align: right;">NA</td>
</tr>
</tbody>
</table>

1.  Найти самый распространенный цвет глаз персонажей вселенной Звездных
    войн.

``` r
starwars %>% count(eye_color) %>% filter(n == max(n))
```

    # A tibble: 1 × 2
      eye_color     n
      <chr>     <int>
    1 brown        21

1.  Подсчитать среднюю длину имени в каждой расе вселенной Звездных
    войн.

``` r
starwars %>% group_by(species) %>% summarise(Avg_age = median(nchar(name), na.rm = TRUE)) |> knitr::kable(format='markdown')
```

<table>
<thead>
<tr>
<th style="text-align: left;">species</th>
<th style="text-align: right;">Avg_age</th>
</tr>
</thead>
<tbody>
<tr>
<td style="text-align: left;">Aleena</td>
<td style="text-align: right;">12.0</td>
</tr>
<tr>
<td style="text-align: left;">Besalisk</td>
<td style="text-align: right;">15.0</td>
</tr>
<tr>
<td style="text-align: left;">Cerean</td>
<td style="text-align: right;">12.0</td>
</tr>
<tr>
<td style="text-align: left;">Chagrian</td>
<td style="text-align: right;">10.0</td>
</tr>
<tr>
<td style="text-align: left;">Clawdite</td>
<td style="text-align: right;">10.0</td>
</tr>
<tr>
<td style="text-align: left;">Droid</td>
<td style="text-align: right;">5.0</td>
</tr>
<tr>
<td style="text-align: left;">Dug</td>
<td style="text-align: right;">7.0</td>
</tr>
<tr>
<td style="text-align: left;">Ewok</td>
<td style="text-align: right;">21.0</td>
</tr>
<tr>
<td style="text-align: left;">Geonosian</td>
<td style="text-align: right;">17.0</td>
</tr>
<tr>
<td style="text-align: left;">Gungan</td>
<td style="text-align: right;">12.0</td>
</tr>
<tr>
<td style="text-align: left;">Human</td>
<td style="text-align: right;">11.0</td>
</tr>
<tr>
<td style="text-align: left;">Hutt</td>
<td style="text-align: right;">21.0</td>
</tr>
<tr>
<td style="text-align: left;">Iktotchi</td>
<td style="text-align: right;">11.0</td>
</tr>
<tr>
<td style="text-align: left;">Kaleesh</td>
<td style="text-align: right;">8.0</td>
</tr>
<tr>
<td style="text-align: left;">Kaminoan</td>
<td style="text-align: right;">7.0</td>
</tr>
<tr>
<td style="text-align: left;">Kel Dor</td>
<td style="text-align: right;">8.0</td>
</tr>
<tr>
<td style="text-align: left;">Mirialan</td>
<td style="text-align: right;">14.0</td>
</tr>
<tr>
<td style="text-align: left;">Mon Calamari</td>
<td style="text-align: right;">6.0</td>
</tr>
<tr>
<td style="text-align: left;">Muun</td>
<td style="text-align: right;">8.0</td>
</tr>
<tr>
<td style="text-align: left;">Nautolan</td>
<td style="text-align: right;">9.0</td>
</tr>
<tr>
<td style="text-align: left;">Neimodian</td>
<td style="text-align: right;">11.0</td>
</tr>
<tr>
<td style="text-align: left;">Pau’an</td>
<td style="text-align: right;">10.0</td>
</tr>
<tr>
<td style="text-align: left;">Quermian</td>
<td style="text-align: right;">11.0</td>
</tr>
<tr>
<td style="text-align: left;">Rodian</td>
<td style="text-align: right;">6.0</td>
</tr>
<tr>
<td style="text-align: left;">Skakoan</td>
<td style="text-align: right;">10.0</td>
</tr>
<tr>
<td style="text-align: left;">Sullustan</td>
<td style="text-align: right;">9.0</td>
</tr>
<tr>
<td style="text-align: left;">Tholothian</td>
<td style="text-align: right;">10.0</td>
</tr>
<tr>
<td style="text-align: left;">Togruta</td>
<td style="text-align: right;">8.0</td>
</tr>
<tr>
<td style="text-align: left;">Toong</td>
<td style="text-align: right;">14.0</td>
</tr>
<tr>
<td style="text-align: left;">Toydarian</td>
<td style="text-align: right;">5.0</td>
</tr>
<tr>
<td style="text-align: left;">Trandoshan</td>
<td style="text-align: right;">5.0</td>
</tr>
<tr>
<td style="text-align: left;">Twi’lek</td>
<td style="text-align: right;">11.0</td>
</tr>
<tr>
<td style="text-align: left;">Vulptereen</td>
<td style="text-align: right;">8.0</td>
</tr>
<tr>
<td style="text-align: left;">Wookiee</td>
<td style="text-align: right;">8.0</td>
</tr>
<tr>
<td style="text-align: left;">Xexto</td>
<td style="text-align: right;">7.0</td>
</tr>
<tr>
<td style="text-align: left;">Yoda’s species</td>
<td style="text-align: right;">4.0</td>
</tr>
<tr>
<td style="text-align: left;">Zabrak</td>
<td style="text-align: right;">9.5</td>
</tr>
<tr>
<td style="text-align: left;">NA</td>
<td style="text-align: right;">10.5</td>
</tr>
</tbody>
</table>

## Оценка результата

В результате лабораторной работы мы проанализировали встроенный в пакет
dplyr набор данных starwars с помощью языка R.

## Вывод

Таким образом, мы развили практические навыки использования языка R для
обработки данных, закрепили знания базовых типов данных языка R, развили
практические навыки использования функций обработки данных пакета dplyr.
