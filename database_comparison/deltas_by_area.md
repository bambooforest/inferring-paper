Get greatest deltas in segment coverage by macroarea
================
Steven Moran

06 December, 2020

``` r
library(tidyverse)
library(knitr)
library(xtable)
```

# Overview

This script calculates the frequency of segments in
[PHOIBLE](https://phoible.org/) by macroarea and the differences between
the two.

Load the PHOIBLE development data. For reproducibility purposes, we use
use the PHOIBLE respository at branch
[cf51223](https://github.com/phoible/dev/tree/cf5122362b2b85e9724474dbddd67291a442b8fa).

``` r
phoible <- read_csv(url("https://raw.githubusercontent.com/phoible/dev/cf5122362b2b85e9724474dbddd67291a442b8fa/data/phoible.csv"), col_types = c(InventoryID = "i", Marginal = "l", .default = "c"))
```

Load the [Glottolog data](https://glottolog.org/meta/downloads), which
contains the macroarea classifications for language..

``` r
glottolog <- read_csv("https://cdstar.shh.mpg.de/bitstreams/EAEA0-D501-DBB8-65C4-0/languages_and_dialects_geo.csv")
```

    ## 
    ## ── Column specification ────────────────────────────────────────────────────────
    ## cols(
    ##   glottocode = col_character(),
    ##   name = col_character(),
    ##   isocodes = col_character(),
    ##   level = col_character(),
    ##   macroarea = col_character(),
    ##   latitude = col_double(),
    ##   longitude = col_double()
    ## )

``` r
head(glottolog)
```

    ## # A tibble: 6 x 7
    ##   glottocode name       isocodes level    macroarea latitude longitude
    ##   <chr>      <chr>      <chr>    <chr>    <chr>        <dbl>     <dbl>
    ## 1 3adt1234   3Ad-Tekles <NA>     dialect  Africa       NA         NA  
    ## 2 aala1237   Aalawa     <NA>     dialect  Papunesia    NA         NA  
    ## 3 aant1238   Aantantara <NA>     dialect  Papunesia    NA         NA  
    ## 4 aari1239   Aari       aiw      language Africa        5.95      36.6
    ## 5 aari1240   Aariya     aay      language Eurasia      NA         NA  
    ## 6 aasa1238   Aasax      aas      language Africa       -4.01      36.9

Merge the datasets together.

``` r
merged <- left_join(phoible, glottolog, by = c("Glottocode" = "glottocode"))
```

Generate the total segment percentages.

``` r
totals <- merged %>%
  select(InventoryID, Phoneme) %>%
  group_by(Phoneme) %>%
  summarize(count = n())
```

    ## `summarise()` ungrouping output (override with `.groups` argument)

``` r
totals$phoible_freq <- totals$count / length(unique(phoible$InventoryID))
```

Generate the segment percentages by macroarea.

``` r
macros <- merged %>% select(InventoryID, macroarea, Phoneme, SegmentClass)
```

Note there are some NAs in the data, i.e. the Glottolog data doesn’t
contain information on data points in PHOIBLE.

``` r
table(macros$macroarea, exclude = FALSE)
```

    ## 
    ##        Africa     Australia       Eurasia North America     Papunesia 
    ##         36332         10464         33865          6421          5198 
    ## South America          <NA> 
    ##         12462           717

There are 717 *segments* that belong to languages that are NA. Let’s
identify which data points are NA.

``` r
missing_areas <- macros %>%
  filter(is.na(macroarea)) %>%
  select(InventoryID) %>%
  distinct()
missing_langs <- phoible %>% filter(InventoryID %in% missing_areas$InventoryID)
missing_langs %>%
  select(InventoryID, Glottocode, ISO6393, LanguageName) %>%
  distinct() %>%
  kable()
```

| InventoryID | Glottocode | ISO6393 | LanguageName      |
|------------:|:-----------|:--------|:------------------|
|         976 | osse1243   | oss     | Ossetian          |
|        1208 | jiar1240   | jya     | Jiarong           |
|        1398 | dink1262   | din     | Dinka             |
|        2257 | jiar1240   | jya     | Caodeng rGyalrong |
|        2283 | osse1243   | oss     | Iron Ossetic      |
|        2324 | osse1243   | oss     | Iron Ossetic      |
|        2351 | osse1243   | oss     | Iron Ossetic      |
|        2389 | osse1243   | oss     | Iron Ossetic      |
|        2408 | osse1243   | oss     | Iron Ossetic      |
|        2424 | osse1243   | oss     | Iron Ossetic      |
|        2432 | osse1243   | oss     | Iron Ossetic      |
|        2464 | mong1331   | mon     | Mongolian         |
|        2476 | osse1243   | oss     | Iron Ossetic      |
|        2509 | osse1243   | oss     | Iron Ossetic      |
|        2626 | osse1243   | oss     | Iron Ossetic      |
|        2729 | NA         | mis     | Djindewal         |

There are 16 data points, but only six different ISO 639-3 codes,
i.e. languages. Since there are so few, we simply drop them.

``` r
macros <- macros %>% filter(!is.na(macroarea))
rownames(macros) <- NULL
```

Now, get segment frequencies by area.

``` r
africa <- macros %>% filter(macroarea == "Africa")
total <- length(unique(africa$InventoryID))
africa <- africa %>%
  group_by(Phoneme, SegmentClass) %>%
  summarize(count = n())
```

    ## `summarise()` regrouping output by 'Phoneme' (override with `.groups` argument)

``` r
africa$freq <- africa$count / total
africa$macroarea <- "Africa"

australia <- macros %>% filter(macroarea == "Australia")
total <- length(unique(australia$InventoryID))
australia <- australia %>%
  group_by(Phoneme, SegmentClass) %>%
  summarize(count = n())
```

    ## `summarise()` regrouping output by 'Phoneme' (override with `.groups` argument)

``` r
australia$freq <- australia$count / total
australia$macroarea <- "Australia"

eurasia <- macros %>% filter(macroarea == "Eurasia")
total <- length(unique(eurasia$InventoryID))
eurasia <- eurasia %>%
  group_by(Phoneme, SegmentClass) %>%
  summarize(count = n())
```

    ## `summarise()` regrouping output by 'Phoneme' (override with `.groups` argument)

``` r
eurasia$freq <- eurasia$count / total
eurasia$macroarea <- "Eurasia"

na <- macros %>% filter(macroarea == "North America")
total <- length(unique(na$InventoryID))
na <- na %>%
  group_by(Phoneme, SegmentClass) %>%
  summarize(count = n())
```

    ## `summarise()` regrouping output by 'Phoneme' (override with `.groups` argument)

``` r
na$freq <- na$count / total
na$macroarea <- "North America"

papunesia <- macros %>% filter(macroarea == "Papunesia")
total <- length(unique(papunesia$InventoryID))
papunesia <- papunesia %>%
  group_by(Phoneme, SegmentClass) %>%
  summarize(count = n())
```

    ## `summarise()` regrouping output by 'Phoneme' (override with `.groups` argument)

``` r
papunesia$freq <- papunesia$count / total
papunesia$macroarea <- "Papunesia"

sa <- macros %>% filter(macroarea == "South America")
total <- length(unique(sa$InventoryID))
sa <- sa %>%
  group_by(Phoneme, SegmentClass) %>%
  summarize(count = n())
```

    ## `summarise()` regrouping output by 'Phoneme' (override with `.groups` argument)

``` r
sa$freq <- sa$count / total
sa$macroarea <- "South America"
```

An anonymous reviewer notes:

> > > Table 1. It is unclear to me why, in Tables 1 and 5-6, we are
> > > given the delta with respect to PHOIBLE and not with respect to
> > > PHOIBLE for all regions except that region being focused upon. Or
> > > perhaps this was done but not expressed clearly. If the frequency
> > > of phoneme /x/ is y in North America, for example, the delta could
> > > be quite different than if /x/ is y in Eurasia or Africa, because
> > > those continents constitute such a big chunk of PHOIBLE. That’s
> > > reasonable, but don’t we want to get a sense of how these regions
> > > contrast to other regions? That seems more intuitively useful.
> > > Perhaps both could be presented in the tables?

So we expand the counts to include PHOIBLE worldwide counts minus the
the counts in each macroarea.

``` r
phoible_no_africa <- macros %>% filter(macroarea != "Africa")
total <- length(unique(phoible_no_africa$InventoryID))
phoible_no_africa <- phoible_no_africa %>%
  group_by(Phoneme, SegmentClass) %>%
  summarize(count = n())
```

    ## `summarise()` regrouping output by 'Phoneme' (override with `.groups` argument)

``` r
phoible_no_africa$freq <- phoible_no_africa$count / total
phoible_no_africa$macroarea <- "Africa"

phoible_no_australia <- macros %>% filter(macroarea != "Australia")
total <- length(unique(phoible_no_australia$InventoryID))
phoible_no_australia <- phoible_no_australia %>%
  group_by(Phoneme, SegmentClass) %>%
  summarize(count = n())
```

    ## `summarise()` regrouping output by 'Phoneme' (override with `.groups` argument)

``` r
phoible_no_australia$freq <- phoible_no_australia$count / total
phoible_no_australia$macroarea <- "Australia"

phoible_no_eurasia <- macros %>% filter(macroarea != "Eurasia")
total <- length(unique(phoible_no_eurasia$InventoryID))
phoible_no_eurasia <- phoible_no_eurasia %>%
  group_by(Phoneme, SegmentClass) %>%
  summarize(count = n())
```

    ## `summarise()` regrouping output by 'Phoneme' (override with `.groups` argument)

``` r
phoible_no_eurasia$freq <- phoible_no_eurasia$count / total
phoible_no_eurasia$macroarea <- "Eurasia"

phoible_no_na <- macros %>% filter(macroarea != "North America")
total <- length(unique(phoible_no_na$InventoryID))
phoible_no_na <- phoible_no_na %>%
  group_by(Phoneme, SegmentClass) %>%
  summarize(count = n())
```

    ## `summarise()` regrouping output by 'Phoneme' (override with `.groups` argument)

``` r
phoible_no_na$freq <- phoible_no_na$count / total
phoible_no_na$macroarea <- "North America"

phoible_no_papunesia <- macros %>% filter(macroarea != "Papunesia")
total <- length(unique(phoible_no_papunesia$InventoryID))
phoible_no_papunesia <- phoible_no_papunesia %>%
  group_by(Phoneme, SegmentClass) %>%
  summarize(count = n())
```

    ## `summarise()` regrouping output by 'Phoneme' (override with `.groups` argument)

``` r
phoible_no_papunesia$freq <- phoible_no_papunesia$count / total
phoible_no_papunesia$macroarea <- "Papunesia"

phoible_no_sa <- macros %>% filter(macroarea != "South America")
total <- length(unique(phoible_no_sa$InventoryID))
phoible_no_sa <- phoible_no_sa %>%
  group_by(Phoneme, SegmentClass) %>%
  summarize(count = n())
```

    ## `summarise()` regrouping output by 'Phoneme' (override with `.groups` argument)

``` r
phoible_no_sa$freq <- phoible_no_sa$count / total
phoible_no_sa$macroarea <- "South America"
```

Drop the count and rbind them the area-specific dataframes.

``` r
africa <- africa %>% select(macroarea, Phoneme, SegmentClass, freq)
australia <- australia %>% select(macroarea, Phoneme, SegmentClass, freq)
eurasia <- eurasia %>% select(macroarea, Phoneme, SegmentClass, freq)
papunesia <- papunesia %>% select(macroarea, Phoneme, SegmentClass, freq)
na <- na %>% select(macroarea, Phoneme, SegmentClass, freq)
sa <- sa %>% select(macroarea, Phoneme, SegmentClass, freq)

all <- bind_rows(africa, australia, eurasia, papunesia, na, sa)
```

And again for the PHOIBLE minus macroarea counts.

``` r
phoible_no_africa <- phoible_no_africa %>% select(macroarea, Phoneme, SegmentClass, freq)
phoible_no_australia <- phoible_no_australia %>% select(macroarea, Phoneme, SegmentClass, freq)
phoible_no_eurasia <- phoible_no_eurasia %>% select(macroarea, Phoneme, SegmentClass, freq)
phoible_no_papunesia <- phoible_no_papunesia %>% select(macroarea, Phoneme, SegmentClass, freq)
phoible_no_na <- phoible_no_na %>% select(macroarea, Phoneme, SegmentClass, freq)
phoible_no_sa <- phoible_no_sa %>% select(macroarea, Phoneme, SegmentClass, freq)

phoible_no_all <- bind_rows(phoible_no_africa, phoible_no_australia, phoible_no_eurasia, phoible_no_papunesia, phoible_no_na, phoible_no_sa)
phoible_no_all <- phoible_no_all %>% rename(freq_phoible_no_macroarea = freq)
```

Get delta between phoneme frequencies by area.

``` r
totals <- totals %>% select(Phoneme, phoible_freq)
all <- left_join(all, totals, by = c("Phoneme" = "Phoneme"))
all$delta <- all$freq - all$phoible_freq
```

Now we need to add in the frequency and deltas for PHOIBLE without each
macroarea.

``` r
all <- left_join(all, phoible_no_all)
```

    ## Joining, by = c("macroarea", "Phoneme", "SegmentClass")

This introduces NAs because we removed macroareas that contain segments
that are not found in any other macroarea, e.g. certain complex tones
only found in Africa.

``` r
macroarea_specific_segments <- all %>% filter(is.na(freq_phoible_no_macroarea))
write_csv(macroarea_specific_segments, "macroarea_specific_segments.csv")
```

Replace NAs with zero.

``` r
all <- all %>% mutate(freq_phoible_no_macroarea = replace_na(freq_phoible_no_macroarea, 0))
```

Get the delta between phoneme frequencies by area without those areas.

``` r
all$delta_no_macroarea <- all$freq - all$freq_phoible_no_macroarea
```

# Some descriptive statistics

Where are the largest deltas by area?

``` r
all %>%
  group_by(macroarea) %>%
  slice_max(order_by = delta, n = 10) %>%
  kable()
```

| macroarea     | Phoneme | SegmentClass |      freq | phoible\_freq |     delta | freq\_phoible\_no\_macroarea | delta\_no\_macroarea |
|:--------------|:--------|:-------------|----------:|--------------:|----------:|-----------------------------:|---------------------:|
| Africa        | f       | consonant    | 0.8418079 |     0.4403974 | 0.4014106 |                    0.2704106 |            0.5713973 |
| Africa        | ˨       | tone         | 0.5785311 |     0.1807947 | 0.3977364 |                    0.0155734 |            0.5629577 |
| Africa        | ˦       | tone         | 0.5796610 |     0.1827815 | 0.3968796 |                    0.0179330 |            0.5617280 |
| Africa        | d       | consonant    | 0.8022599 |     0.4556291 | 0.3466307 |                    0.3133554 |            0.4889045 |
| Africa        | ɡ       | consonant    | 0.8779661 |     0.5668874 | 0.3110787 |                    0.4388863 |            0.4390798 |
| Africa        | ɔ       | vowel        | 0.6621469 |     0.3543046 | 0.3078423 |                    0.2269939 |            0.4351530 |
| Africa        | ɡb      | consonant    | 0.4203390 |     0.1238411 | 0.2964979 |                    0.0009438 |            0.4193951 |
| Africa        | kp      | consonant    | 0.4192090 |     0.1235099 | 0.2956991 |                    0.0009438 |            0.4182652 |
| Africa        | ɲ       | consonant    | 0.7062147 |     0.4158940 | 0.2903206 |                    0.2963662 |            0.4098485 |
| Africa        | b       | consonant    | 0.9129944 |     0.6311258 | 0.2818685 |                    0.5158093 |            0.3971850 |
| Australia     | ȵ       | consonant    | 0.8692661 |     0.1258278 | 0.7434382 |                    0.0000000 |            0.8692661 |
| Australia     | ȶ       | consonant    | 0.7155963 |     0.1036424 | 0.6119539 |                    0.0000000 |            0.7155963 |
| Australia     | ɻ       | consonant    | 0.6582569 |     0.1013245 | 0.5569324 |                    0.0070093 |            0.6512475 |
| Australia     | ɳ       | consonant    | 0.6582569 |     0.1321192 | 0.5261377 |                    0.0436137 |            0.6146432 |
| Australia     | ɭ       | consonant    | 0.6330275 |     0.1188742 | 0.5141534 |                    0.0323209 |            0.6007067 |
| Australia     | ʈ       | consonant    | 0.5665138 |     0.1592715 | 0.4072422 |                    0.0911215 |            0.4753923 |
| Australia     | n̪       | consonant    | 0.5756881 |     0.1764901 | 0.3991980 |                    0.1086449 |            0.4670432 |
| Australia     | ȴ       | consonant    | 0.4633028 |     0.0668874 | 0.3964153 |                    0.0000000 |            0.4633028 |
| Australia     | ŋ       | consonant    | 1.0000000 |     0.6284768 | 0.3715232 |                    0.5673676 |            0.4326324 |
| Australia     | t̪       | consonant    | 0.4954128 |     0.2344371 | 0.2609758 |                    0.1904206 |            0.3049923 |
| Eurasia       | pʰ      | consonant    | 0.5223881 |     0.1963576 | 0.3260304 |                    0.0727273 |            0.4496608 |
| Eurasia       | kʰ      | consonant    | 0.5161692 |     0.2006623 | 0.3155069 |                    0.0809091 |            0.4352601 |
| Eurasia       | d̪       | consonant    | 0.3582090 |     0.1440397 | 0.2141692 |                    0.0654545 |            0.2927544 |
| Eurasia       | tʰ      | consonant    | 0.3097015 |     0.1337748 | 0.1759267 |                    0.0700000 |            0.2397015 |
| Eurasia       | x       | consonant    | 0.3606965 |     0.1900662 | 0.1706303 |                    0.1281818 |            0.2325147 |
| Eurasia       | b       | consonant    | 0.7997512 |     0.6311258 | 0.1686254 |                    0.5718182 |            0.2279331 |
| Eurasia       | ɖ       | consonant    | 0.2425373 |     0.0850993 | 0.1574380 |                    0.0281818 |            0.2143555 |
| Eurasia       | ɡ       | consonant    | 0.7213930 |     0.5668874 | 0.1545056 |                    0.5122727 |            0.2091203 |
| Eurasia       | t̪ʰ      | consonant    | 0.1915423 |     0.0592715 | 0.1322708 |                    0.0104545 |            0.1810877 |
| Eurasia       | ʒ       | consonant    | 0.2898010 |     0.1582781 | 0.1315228 |                    0.1090909 |            0.1807101 |
| North America | ʔ       | consonant    | 0.8586957 |     0.3748344 | 0.4838612 |                    0.3450355 |            0.5136602 |
| North America | kʼ      | consonant    | 0.4184783 |     0.0804636 | 0.3380147 |                    0.0549645 |            0.3635137 |
| North America | h       | consonant    | 0.8478261 |     0.5639073 | 0.2839188 |                    0.5485816 |            0.2992445 |
| North America | ʃ       | consonant    | 0.6413043 |     0.3655629 | 0.2757414 |                    0.3471631 |            0.2941412 |
| North America | t̠ʃ      | consonant    | 0.6684783 |     0.4033113 | 0.2651670 |                    0.3851064 |            0.2833719 |
| North America | t̠ʃʼ     | consonant    | 0.3152174 |     0.0612583 | 0.2539591 |                    0.0425532 |            0.2726642 |
| North America | tsʼ     | consonant    | 0.2934783 |     0.0427152 | 0.2507630 |                    0.0262411 |            0.2672371 |
| North America | tʼ      | consonant    | 0.2989130 |     0.0519868 | 0.2469263 |                    0.0358156 |            0.2630974 |
| North America | kʷ      | consonant    | 0.3532609 |     0.1231788 | 0.2300821 |                    0.1056738 |            0.2475871 |
| North America | pʼ      | consonant    | 0.2880435 |     0.0592715 | 0.2287720 |                    0.0407801 |            0.2472633 |
| Papunesia     | ʔ       | consonant    | 0.5185185 |     0.3748344 | 0.1436841 |                    0.3654950 |            0.1530235 |
| Papunesia     | o̞       | vowel        | 0.1990741 |     0.0956954 | 0.1033787 |                    0.0846485 |            0.1144256 |
| Papunesia     | s       | consonant    | 0.7592593 |     0.6692053 | 0.0900540 |                    0.6653515 |            0.0939078 |
| Papunesia     | β       | consonant    | 0.1712963 |     0.1013245 | 0.0699718 |                    0.0964849 |            0.0748114 |
| Papunesia     | e̞       | vowel        | 0.1574074 |     0.0930464 | 0.0643610 |                    0.0850072 |            0.0724002 |
| Papunesia     | d       | consonant    | 0.5185185 |     0.4556291 | 0.0628894 |                    0.4526542 |            0.0658643 |
| Papunesia     | u       | vowel        | 0.9351852 |     0.8761589 | 0.0590262 |                    0.8708752 |            0.0643100 |
| Papunesia     | ɡ       | consonant    | 0.6250000 |     0.5668874 | 0.0581126 |                    0.5638451 |            0.0611549 |
| Papunesia     | ŋ       | consonant    | 0.6851852 |     0.6284768 | 0.0567084 |                    0.6258967 |            0.0592885 |
| Papunesia     | n̪\|n    | consonant    | 0.1064815 |     0.0533113 | 0.0531702 |                    0.0494978 |            0.0569836 |
| South America | ɾ       | consonant    | 0.7244259 |     0.2562914 | 0.4681345 |                    0.1687129 |            0.5557130 |
| South America | ɨ       | vowel        | 0.5114823 |     0.1625828 | 0.3488995 |                    0.0970297 |            0.4144526 |
| South America | ã       | vowel        | 0.4237996 |     0.1725166 | 0.2512830 |                    0.1259406 |            0.2978590 |
| South America | ĩ       | vowel        | 0.4175365 |     0.1794702 | 0.2380663 |                    0.1354455 |            0.2820910 |
| South America | t       | consonant    | 0.9102296 |     0.6834437 | 0.2267859 |                    0.6439604 |            0.2662692 |
| South America | t̠ʃ      | consonant    | 0.6263048 |     0.4033113 | 0.2229935 |                    0.3600000 |            0.2663048 |
| South America | ɨ̃       | vowel        | 0.2484342 |     0.0427152 | 0.2057190 |                    0.0039604 |            0.2444738 |
| South America | ẽ       | vowel        | 0.2922756 |     0.1069536 | 0.1853219 |                    0.0724752 |            0.2198003 |
| South America | õ       | vowel        | 0.2964509 |     0.1142384 | 0.1822125 |                    0.0803960 |            0.2160549 |
| South America | ũ       | vowel        | 0.3382046 |     0.1635762 | 0.1746284 |                    0.1314851 |            0.2067194 |

Obstruents in South America (Table 1 in the paper).

``` r
voiced_obstruents <- c("b", "d", "ɡ", "β", "v", "ð", "z", "ʒ", "ɣ")
all %>%
  filter(macroarea == "South America") %>%
  filter(Phoneme %in% voiced_obstruents) %>%
  slice(match(voiced_obstruents, Phoneme)) %>%
  kable()
```

| macroarea     | Phoneme | SegmentClass |      freq | phoible\_freq |      delta | freq\_phoible\_no\_macroarea | delta\_no\_macroarea |
|:--------------|:--------|:-------------|----------:|--------------:|-----------:|-----------------------------:|---------------------:|
| South America | b       | consonant    | 0.4405010 |     0.6311258 | -0.1906248 |                    0.6693069 |           -0.2288059 |
| South America | d       | consonant    | 0.3653445 |     0.4556291 | -0.0902847 |                    0.4748515 |           -0.1095070 |
| South America | ð       | consonant    | 0.0229645 |     0.0529801 | -0.0300156 |                    0.0590099 |           -0.0360454 |
| South America | ɡ       | consonant    | 0.2922756 |     0.5668874 | -0.2746118 |                    0.6205941 |           -0.3283185 |
| South America | ɣ       | consonant    | 0.0584551 |     0.1443709 | -0.0859157 |                    0.1603960 |           -0.1019409 |
| South America | v       | consonant    | 0.0313152 |     0.2701987 | -0.2388834 |                    0.3124752 |           -0.2811600 |
| South America | z       | consonant    | 0.0647182 |     0.2956954 | -0.2309772 |                    0.3401980 |           -0.2754799 |
| South America | ʒ       | consonant    | 0.0584551 |     0.1582781 | -0.0998230 |                    0.1762376 |           -0.1177825 |
| South America | β       | consonant    | 0.1711900 |     0.1013245 |  0.0698655 |                    0.0887129 |            0.0824771 |

``` r
all %>%
  group_by(macroarea) %>%
  slice_max(order_by = delta, n = 10) %>%
  kable()
```

| macroarea     | Phoneme | SegmentClass |      freq | phoible\_freq |     delta | freq\_phoible\_no\_macroarea | delta\_no\_macroarea |
|:--------------|:--------|:-------------|----------:|--------------:|----------:|-----------------------------:|---------------------:|
| Africa        | f       | consonant    | 0.8418079 |     0.4403974 | 0.4014106 |                    0.2704106 |            0.5713973 |
| Africa        | ˨       | tone         | 0.5785311 |     0.1807947 | 0.3977364 |                    0.0155734 |            0.5629577 |
| Africa        | ˦       | tone         | 0.5796610 |     0.1827815 | 0.3968796 |                    0.0179330 |            0.5617280 |
| Africa        | d       | consonant    | 0.8022599 |     0.4556291 | 0.3466307 |                    0.3133554 |            0.4889045 |
| Africa        | ɡ       | consonant    | 0.8779661 |     0.5668874 | 0.3110787 |                    0.4388863 |            0.4390798 |
| Africa        | ɔ       | vowel        | 0.6621469 |     0.3543046 | 0.3078423 |                    0.2269939 |            0.4351530 |
| Africa        | ɡb      | consonant    | 0.4203390 |     0.1238411 | 0.2964979 |                    0.0009438 |            0.4193951 |
| Africa        | kp      | consonant    | 0.4192090 |     0.1235099 | 0.2956991 |                    0.0009438 |            0.4182652 |
| Africa        | ɲ       | consonant    | 0.7062147 |     0.4158940 | 0.2903206 |                    0.2963662 |            0.4098485 |
| Africa        | b       | consonant    | 0.9129944 |     0.6311258 | 0.2818685 |                    0.5158093 |            0.3971850 |
| Australia     | ȵ       | consonant    | 0.8692661 |     0.1258278 | 0.7434382 |                    0.0000000 |            0.8692661 |
| Australia     | ȶ       | consonant    | 0.7155963 |     0.1036424 | 0.6119539 |                    0.0000000 |            0.7155963 |
| Australia     | ɻ       | consonant    | 0.6582569 |     0.1013245 | 0.5569324 |                    0.0070093 |            0.6512475 |
| Australia     | ɳ       | consonant    | 0.6582569 |     0.1321192 | 0.5261377 |                    0.0436137 |            0.6146432 |
| Australia     | ɭ       | consonant    | 0.6330275 |     0.1188742 | 0.5141534 |                    0.0323209 |            0.6007067 |
| Australia     | ʈ       | consonant    | 0.5665138 |     0.1592715 | 0.4072422 |                    0.0911215 |            0.4753923 |
| Australia     | n̪       | consonant    | 0.5756881 |     0.1764901 | 0.3991980 |                    0.1086449 |            0.4670432 |
| Australia     | ȴ       | consonant    | 0.4633028 |     0.0668874 | 0.3964153 |                    0.0000000 |            0.4633028 |
| Australia     | ŋ       | consonant    | 1.0000000 |     0.6284768 | 0.3715232 |                    0.5673676 |            0.4326324 |
| Australia     | t̪       | consonant    | 0.4954128 |     0.2344371 | 0.2609758 |                    0.1904206 |            0.3049923 |
| Eurasia       | pʰ      | consonant    | 0.5223881 |     0.1963576 | 0.3260304 |                    0.0727273 |            0.4496608 |
| Eurasia       | kʰ      | consonant    | 0.5161692 |     0.2006623 | 0.3155069 |                    0.0809091 |            0.4352601 |
| Eurasia       | d̪       | consonant    | 0.3582090 |     0.1440397 | 0.2141692 |                    0.0654545 |            0.2927544 |
| Eurasia       | tʰ      | consonant    | 0.3097015 |     0.1337748 | 0.1759267 |                    0.0700000 |            0.2397015 |
| Eurasia       | x       | consonant    | 0.3606965 |     0.1900662 | 0.1706303 |                    0.1281818 |            0.2325147 |
| Eurasia       | b       | consonant    | 0.7997512 |     0.6311258 | 0.1686254 |                    0.5718182 |            0.2279331 |
| Eurasia       | ɖ       | consonant    | 0.2425373 |     0.0850993 | 0.1574380 |                    0.0281818 |            0.2143555 |
| Eurasia       | ɡ       | consonant    | 0.7213930 |     0.5668874 | 0.1545056 |                    0.5122727 |            0.2091203 |
| Eurasia       | t̪ʰ      | consonant    | 0.1915423 |     0.0592715 | 0.1322708 |                    0.0104545 |            0.1810877 |
| Eurasia       | ʒ       | consonant    | 0.2898010 |     0.1582781 | 0.1315228 |                    0.1090909 |            0.1807101 |
| North America | ʔ       | consonant    | 0.8586957 |     0.3748344 | 0.4838612 |                    0.3450355 |            0.5136602 |
| North America | kʼ      | consonant    | 0.4184783 |     0.0804636 | 0.3380147 |                    0.0549645 |            0.3635137 |
| North America | h       | consonant    | 0.8478261 |     0.5639073 | 0.2839188 |                    0.5485816 |            0.2992445 |
| North America | ʃ       | consonant    | 0.6413043 |     0.3655629 | 0.2757414 |                    0.3471631 |            0.2941412 |
| North America | t̠ʃ      | consonant    | 0.6684783 |     0.4033113 | 0.2651670 |                    0.3851064 |            0.2833719 |
| North America | t̠ʃʼ     | consonant    | 0.3152174 |     0.0612583 | 0.2539591 |                    0.0425532 |            0.2726642 |
| North America | tsʼ     | consonant    | 0.2934783 |     0.0427152 | 0.2507630 |                    0.0262411 |            0.2672371 |
| North America | tʼ      | consonant    | 0.2989130 |     0.0519868 | 0.2469263 |                    0.0358156 |            0.2630974 |
| North America | kʷ      | consonant    | 0.3532609 |     0.1231788 | 0.2300821 |                    0.1056738 |            0.2475871 |
| North America | pʼ      | consonant    | 0.2880435 |     0.0592715 | 0.2287720 |                    0.0407801 |            0.2472633 |
| Papunesia     | ʔ       | consonant    | 0.5185185 |     0.3748344 | 0.1436841 |                    0.3654950 |            0.1530235 |
| Papunesia     | o̞       | vowel        | 0.1990741 |     0.0956954 | 0.1033787 |                    0.0846485 |            0.1144256 |
| Papunesia     | s       | consonant    | 0.7592593 |     0.6692053 | 0.0900540 |                    0.6653515 |            0.0939078 |
| Papunesia     | β       | consonant    | 0.1712963 |     0.1013245 | 0.0699718 |                    0.0964849 |            0.0748114 |
| Papunesia     | e̞       | vowel        | 0.1574074 |     0.0930464 | 0.0643610 |                    0.0850072 |            0.0724002 |
| Papunesia     | d       | consonant    | 0.5185185 |     0.4556291 | 0.0628894 |                    0.4526542 |            0.0658643 |
| Papunesia     | u       | vowel        | 0.9351852 |     0.8761589 | 0.0590262 |                    0.8708752 |            0.0643100 |
| Papunesia     | ɡ       | consonant    | 0.6250000 |     0.5668874 | 0.0581126 |                    0.5638451 |            0.0611549 |
| Papunesia     | ŋ       | consonant    | 0.6851852 |     0.6284768 | 0.0567084 |                    0.6258967 |            0.0592885 |
| Papunesia     | n̪\|n    | consonant    | 0.1064815 |     0.0533113 | 0.0531702 |                    0.0494978 |            0.0569836 |
| South America | ɾ       | consonant    | 0.7244259 |     0.2562914 | 0.4681345 |                    0.1687129 |            0.5557130 |
| South America | ɨ       | vowel        | 0.5114823 |     0.1625828 | 0.3488995 |                    0.0970297 |            0.4144526 |
| South America | ã       | vowel        | 0.4237996 |     0.1725166 | 0.2512830 |                    0.1259406 |            0.2978590 |
| South America | ĩ       | vowel        | 0.4175365 |     0.1794702 | 0.2380663 |                    0.1354455 |            0.2820910 |
| South America | t       | consonant    | 0.9102296 |     0.6834437 | 0.2267859 |                    0.6439604 |            0.2662692 |
| South America | t̠ʃ      | consonant    | 0.6263048 |     0.4033113 | 0.2229935 |                    0.3600000 |            0.2663048 |
| South America | ɨ̃       | vowel        | 0.2484342 |     0.0427152 | 0.2057190 |                    0.0039604 |            0.2444738 |
| South America | ẽ       | vowel        | 0.2922756 |     0.1069536 | 0.1853219 |                    0.0724752 |            0.2198003 |
| South America | õ       | vowel        | 0.2964509 |     0.1142384 | 0.1822125 |                    0.0803960 |            0.2160549 |
| South America | ũ       | vowel        | 0.3382046 |     0.1635762 | 0.1746284 |                    0.1314851 |            0.2067194 |

``` r
all %>%
  group_by(macroarea) %>%
  slice_min(order_by = delta, n = 10) %>%
  kable()
```

| macroarea     | Phoneme | SegmentClass |      freq | phoible\_freq |      delta | freq\_phoible\_no\_macroarea | delta\_no\_macroarea |
|:--------------|:--------|:-------------|----------:|--------------:|-----------:|-----------------------------:|---------------------:|
| Africa        | ʈ       | consonant    | 0.0259887 |     0.1592715 | -0.1332828 |                    0.2161397 |           -0.1901510 |
| Africa        | pʰ      | consonant    | 0.0632768 |     0.1963576 | -0.1330808 |                    0.2472865 |           -0.1840096 |
| Africa        | kʰ      | consonant    | 0.0677966 |     0.2006623 | -0.1328656 |                    0.2515337 |           -0.1837371 |
| Africa        | ɳ       | consonant    | 0.0033898 |     0.1321192 | -0.1287294 |                    0.1868806 |           -0.1834908 |
| Africa        | n̪       | consonant    | 0.0519774 |     0.1764901 | -0.1245127 |                    0.2284096 |           -0.1764322 |
| Africa        | ɾ       | consonant    | 0.1389831 |     0.2562914 | -0.1173083 |                    0.3067485 |           -0.1677654 |
| Africa        | ɭ       | consonant    | 0.0056497 |     0.1188742 | -0.1132245 |                    0.1670599 |           -0.1614102 |
| Africa        | t̪       | consonant    | 0.1333333 |     0.2344371 | -0.1011038 |                    0.2770175 |           -0.1436841 |
| Africa        | ɻ       | consonant    | 0.0045198 |     0.1013245 | -0.0968047 |                    0.1420481 |           -0.1375284 |
| Africa        | tʰ      | consonant    | 0.0576271 |     0.1337748 | -0.0761477 |                    0.1661161 |           -0.1084890 |
| Australia     | s       | consonant    | 0.0022936 |     0.6692053 | -0.6669117 |                    0.7858255 |           -0.7835320 |
| Australia     | b       | consonant    | 0.0229358 |     0.6311258 | -0.6081900 |                    0.7363707 |           -0.7134349 |
| Australia     | h       | consonant    | 0.0091743 |     0.5639073 | -0.5547330 |                    0.6616044 |           -0.6524300 |
| Australia     | ɡ       | consonant    | 0.0229358 |     0.5668874 | -0.5439516 |                    0.6608255 |           -0.6378898 |
| Australia     | d       | consonant    | 0.0183486 |     0.4556291 | -0.4372805 |                    0.5319315 |           -0.5135828 |
| Australia     | f       | consonant    | 0.0045872 |     0.4403974 | -0.4358102 |                    0.5124611 |           -0.5078739 |
| Australia     | t̠ʃ      | consonant    | 0.0022936 |     0.4033113 | -0.4010177 |                    0.4704050 |           -0.4681114 |
| Australia     | ɲ       | consonant    | 0.0389908 |     0.4158940 | -0.3769032 |                    0.4813084 |           -0.4423176 |
| Australia     | o       | vowel        | 0.2362385 |     0.6046358 | -0.3683972 |                    0.6693925 |           -0.4331540 |
| Australia     | ɛ       | vowel        | 0.0252294 |     0.3738411 | -0.3486117 |                    0.4341900 |           -0.4089607 |
| Eurasia       | t       | consonant    | 0.4502488 |     0.6834437 | -0.2331950 |                    0.7727273 |           -0.3224785 |
| Eurasia       | w       | consonant    | 0.5945274 |     0.8221854 | -0.2276581 |                    0.9045455 |           -0.3100181 |
| Eurasia       | a       | vowel        | 0.6380597 |     0.8609272 | -0.2228675 |                    0.9468182 |           -0.3087585 |
| Eurasia       | ˨       | tone         | 0.0174129 |     0.1807947 | -0.1633818 |                    0.2413636 |           -0.2239507 |
| Eurasia       | ˦       | tone         | 0.0223881 |     0.1827815 | -0.1603934 |                    0.2422727 |           -0.2198847 |
| Eurasia       | n       | consonant    | 0.6741294 |     0.7781457 | -0.1040163 |                    0.8204545 |           -0.1463252 |
| Eurasia       | ɻ       | consonant    | 0.0087065 |     0.1013245 | -0.0926180 |                    0.1354545 |           -0.1267481 |
| Eurasia       | i       | vowel        | 0.8283582 |     0.9201987 | -0.0918405 |                    0.9531818 |           -0.1248236 |
| Eurasia       | kʷ      | consonant    | 0.0323383 |     0.1231788 | -0.0908405 |                    0.1531818 |           -0.1208435 |
| Eurasia       | ã       | vowel        | 0.0845771 |     0.1725166 | -0.0879394 |                    0.2059091 |           -0.1213320 |
| North America | ŋ       | consonant    | 0.2445652 |     0.6284768 | -0.3839116 |                    0.6553191 |           -0.4107539 |
| North America | ɲ       | consonant    | 0.1141304 |     0.4158940 | -0.3017636 |                    0.4368794 |           -0.3227490 |
| North America | e       | vowel        | 0.3586957 |     0.6096026 | -0.2509070 |                    0.6276596 |           -0.2689639 |
| North America | f       | consonant    | 0.2010870 |     0.4403974 | -0.2393104 |                    0.4542553 |           -0.2531684 |
| North America | ɡ       | consonant    | 0.3532609 |     0.5668874 | -0.2136265 |                    0.5822695 |           -0.2290086 |
| North America | r       | consonant    | 0.2391304 |     0.4410596 | -0.2019292 |                    0.4556738 |           -0.2165433 |
| North America | z       | consonant    | 0.1141304 |     0.2956954 | -0.1815649 |                    0.3081560 |           -0.1940256 |
| North America | v       | consonant    | 0.0923913 |     0.2701987 | -0.1778074 |                    0.2790780 |           -0.1866867 |
| North America | b       | consonant    | 0.4619565 |     0.6311258 | -0.1691693 |                    0.6439716 |           -0.1820151 |
| North America | d       | consonant    | 0.2934783 |     0.4556291 | -0.1621509 |                    0.4680851 |           -0.1746068 |
| Papunesia     | ʃ       | consonant    | 0.0694444 |     0.3655629 | -0.2961185 |                    0.3880918 |           -0.3186474 |
| Papunesia     | z       | consonant    | 0.0740741 |     0.2956954 | -0.2216213 |                    0.3134864 |           -0.2394123 |
| Papunesia     | iː      | vowel        | 0.1250000 |     0.3178808 | -0.1928808 |                    0.3339311 |           -0.2089311 |
| Papunesia     | j       | consonant    | 0.7083333 |     0.8993377 | -0.1910044 |                    0.9135581 |           -0.2052248 |
| Papunesia     | uː      | vowel        | 0.1111111 |     0.2937086 | -0.1825975 |                    0.3084648 |           -0.1973537 |
| Papunesia     | aː      | vowel        | 0.1203704 |     0.2953642 | -0.1749939 |                    0.3098996 |           -0.1895292 |
| Papunesia     | v       | consonant    | 0.1018519 |     0.2701987 | -0.1683468 |                    0.2804878 |           -0.1786360 |
| Papunesia     | ˦       | tone         | 0.0185185 |     0.1827815 | -0.1642629 |                    0.1961980 |           -0.1776795 |
| Papunesia     | t̠ʃ      | consonant    | 0.2407407 |     0.4033113 | -0.1625705 |                    0.4149928 |           -0.1742521 |
| Papunesia     | ˨       | tone         | 0.0185185 |     0.1807947 | -0.1622762 |                    0.1940459 |           -0.1755274 |
| South America | ŋ       | consonant    | 0.2150313 |     0.6284768 | -0.4134455 |                    0.7089109 |           -0.4938796 |
| South America | r       | consonant    | 0.0605428 |     0.4410596 | -0.3805168 |                    0.5148515 |           -0.4543087 |
| South America | f       | consonant    | 0.0751566 |     0.4403974 | -0.3652408 |                    0.5077228 |           -0.4325662 |
| South America | l       | consonant    | 0.3235908 |     0.6768212 | -0.3532304 |                    0.7429703 |           -0.4193795 |
| South America | ɡ       | consonant    | 0.2922756 |     0.5668874 | -0.2746118 |                    0.6205941 |           -0.3283185 |
| South America | ɔ       | vowel        | 0.1106472 |     0.3543046 | -0.2436575 |                    0.4015842 |           -0.2909370 |
| South America | v       | consonant    | 0.0313152 |     0.2701987 | -0.2388834 |                    0.3124752 |           -0.2811600 |
| South America | z       | consonant    | 0.0647182 |     0.2956954 | -0.2309772 |                    0.3401980 |           -0.2754799 |
| South America | ɛ       | vowel        | 0.1565762 |     0.3738411 | -0.2172649 |                    0.4162376 |           -0.2596614 |
| South America | b       | consonant    | 0.4405010 |     0.6311258 | -0.1906248 |                    0.6693069 |           -0.2288059 |

``` r
pos <- all %>%
  group_by(macroarea) %>%
  filter(SegmentClass == "consonant") %>%
  slice_max(order_by = delta, n = 10)
pos %>% kable()
```

| macroarea     | Phoneme | SegmentClass |      freq | phoible\_freq |     delta | freq\_phoible\_no\_macroarea | delta\_no\_macroarea |
|:--------------|:--------|:-------------|----------:|--------------:|----------:|-----------------------------:|---------------------:|
| Africa        | f       | consonant    | 0.8418079 |     0.4403974 | 0.4014106 |                    0.2704106 |            0.5713973 |
| Africa        | d       | consonant    | 0.8022599 |     0.4556291 | 0.3466307 |                    0.3133554 |            0.4889045 |
| Africa        | ɡ       | consonant    | 0.8779661 |     0.5668874 | 0.3110787 |                    0.4388863 |            0.4390798 |
| Africa        | ɡb      | consonant    | 0.4203390 |     0.1238411 | 0.2964979 |                    0.0009438 |            0.4193951 |
| Africa        | kp      | consonant    | 0.4192090 |     0.1235099 | 0.2956991 |                    0.0009438 |            0.4182652 |
| Africa        | ɲ       | consonant    | 0.7062147 |     0.4158940 | 0.2903206 |                    0.2963662 |            0.4098485 |
| Africa        | b       | consonant    | 0.9129944 |     0.6311258 | 0.2818685 |                    0.5158093 |            0.3971850 |
| Africa        | z       | consonant    | 0.5581921 |     0.2956954 | 0.2624967 |                    0.1868806 |            0.3713115 |
| Africa        | v       | consonant    | 0.5175141 |     0.2701987 | 0.2473154 |                    0.1632846 |            0.3542296 |
| Africa        | s       | consonant    | 0.8937853 |     0.6692053 | 0.2245800 |                    0.5795186 |            0.3142667 |
| Australia     | ȵ       | consonant    | 0.8692661 |     0.1258278 | 0.7434382 |                    0.0000000 |            0.8692661 |
| Australia     | ȶ       | consonant    | 0.7155963 |     0.1036424 | 0.6119539 |                    0.0000000 |            0.7155963 |
| Australia     | ɻ       | consonant    | 0.6582569 |     0.1013245 | 0.5569324 |                    0.0070093 |            0.6512475 |
| Australia     | ɳ       | consonant    | 0.6582569 |     0.1321192 | 0.5261377 |                    0.0436137 |            0.6146432 |
| Australia     | ɭ       | consonant    | 0.6330275 |     0.1188742 | 0.5141534 |                    0.0323209 |            0.6007067 |
| Australia     | ʈ       | consonant    | 0.5665138 |     0.1592715 | 0.4072422 |                    0.0911215 |            0.4753923 |
| Australia     | n̪       | consonant    | 0.5756881 |     0.1764901 | 0.3991980 |                    0.1086449 |            0.4670432 |
| Australia     | ȴ       | consonant    | 0.4633028 |     0.0668874 | 0.3964153 |                    0.0000000 |            0.4633028 |
| Australia     | ŋ       | consonant    | 1.0000000 |     0.6284768 | 0.3715232 |                    0.5673676 |            0.4326324 |
| Australia     | t̪       | consonant    | 0.4954128 |     0.2344371 | 0.2609758 |                    0.1904206 |            0.3049923 |
| Eurasia       | pʰ      | consonant    | 0.5223881 |     0.1963576 | 0.3260304 |                    0.0727273 |            0.4496608 |
| Eurasia       | kʰ      | consonant    | 0.5161692 |     0.2006623 | 0.3155069 |                    0.0809091 |            0.4352601 |
| Eurasia       | d̪       | consonant    | 0.3582090 |     0.1440397 | 0.2141692 |                    0.0654545 |            0.2927544 |
| Eurasia       | tʰ      | consonant    | 0.3097015 |     0.1337748 | 0.1759267 |                    0.0700000 |            0.2397015 |
| Eurasia       | x       | consonant    | 0.3606965 |     0.1900662 | 0.1706303 |                    0.1281818 |            0.2325147 |
| Eurasia       | b       | consonant    | 0.7997512 |     0.6311258 | 0.1686254 |                    0.5718182 |            0.2279331 |
| Eurasia       | ɖ       | consonant    | 0.2425373 |     0.0850993 | 0.1574380 |                    0.0281818 |            0.2143555 |
| Eurasia       | ɡ       | consonant    | 0.7213930 |     0.5668874 | 0.1545056 |                    0.5122727 |            0.2091203 |
| Eurasia       | t̪ʰ      | consonant    | 0.1915423 |     0.0592715 | 0.1322708 |                    0.0104545 |            0.1810877 |
| Eurasia       | ʒ       | consonant    | 0.2898010 |     0.1582781 | 0.1315228 |                    0.1090909 |            0.1807101 |
| North America | ʔ       | consonant    | 0.8586957 |     0.3748344 | 0.4838612 |                    0.3450355 |            0.5136602 |
| North America | kʼ      | consonant    | 0.4184783 |     0.0804636 | 0.3380147 |                    0.0549645 |            0.3635137 |
| North America | h       | consonant    | 0.8478261 |     0.5639073 | 0.2839188 |                    0.5485816 |            0.2992445 |
| North America | ʃ       | consonant    | 0.6413043 |     0.3655629 | 0.2757414 |                    0.3471631 |            0.2941412 |
| North America | t̠ʃ      | consonant    | 0.6684783 |     0.4033113 | 0.2651670 |                    0.3851064 |            0.2833719 |
| North America | t̠ʃʼ     | consonant    | 0.3152174 |     0.0612583 | 0.2539591 |                    0.0425532 |            0.2726642 |
| North America | tsʼ     | consonant    | 0.2934783 |     0.0427152 | 0.2507630 |                    0.0262411 |            0.2672371 |
| North America | tʼ      | consonant    | 0.2989130 |     0.0519868 | 0.2469263 |                    0.0358156 |            0.2630974 |
| North America | kʷ      | consonant    | 0.3532609 |     0.1231788 | 0.2300821 |                    0.1056738 |            0.2475871 |
| North America | pʼ      | consonant    | 0.2880435 |     0.0592715 | 0.2287720 |                    0.0407801 |            0.2472633 |
| Papunesia     | ʔ       | consonant    | 0.5185185 |     0.3748344 | 0.1436841 |                    0.3654950 |            0.1530235 |
| Papunesia     | s       | consonant    | 0.7592593 |     0.6692053 | 0.0900540 |                    0.6653515 |            0.0939078 |
| Papunesia     | β       | consonant    | 0.1712963 |     0.1013245 | 0.0699718 |                    0.0964849 |            0.0748114 |
| Papunesia     | d       | consonant    | 0.5185185 |     0.4556291 | 0.0628894 |                    0.4526542 |            0.0658643 |
| Papunesia     | ɡ       | consonant    | 0.6250000 |     0.5668874 | 0.0581126 |                    0.5638451 |            0.0611549 |
| Papunesia     | ŋ       | consonant    | 0.6851852 |     0.6284768 | 0.0567084 |                    0.6258967 |            0.0592885 |
| Papunesia     | n̪\|n    | consonant    | 0.1064815 |     0.0533113 | 0.0531702 |                    0.0494978 |            0.0569836 |
| Papunesia     | ɸ       | consonant    | 0.0972222 |     0.0506623 | 0.0465600 |                    0.0473458 |            0.0498765 |
| Papunesia     | t̪\|t    | consonant    | 0.0972222 |     0.0506623 | 0.0465600 |                    0.0473458 |            0.0498765 |
| Papunesia     | mb      | consonant    | 0.1481481 |     0.1046358 | 0.0435124 |                    0.1011478 |            0.0470004 |
| South America | ɾ       | consonant    | 0.7244259 |     0.2562914 | 0.4681345 |                    0.1687129 |            0.5557130 |
| South America | t       | consonant    | 0.9102296 |     0.6834437 | 0.2267859 |                    0.6439604 |            0.2662692 |
| South America | t̠ʃ      | consonant    | 0.6263048 |     0.4033113 | 0.2229935 |                    0.3600000 |            0.2663048 |
| South America | h       | consonant    | 0.7139875 |     0.5639073 | 0.1500802 |                    0.5390099 |            0.1749776 |
| South America | ʔ       | consonant    | 0.5177453 |     0.3748344 | 0.1429109 |                    0.3497030 |            0.1680423 |
| South America | ʃ       | consonant    | 0.4697286 |     0.3655629 | 0.1041657 |                    0.3453465 |            0.1243821 |
| South America | ts      | consonant    | 0.3215031 |     0.2208609 | 0.1006422 |                    0.2023762 |            0.1191269 |
| South America | p       | consonant    | 0.9457203 |     0.8586093 | 0.0871110 |                    0.8415842 |            0.1041361 |
| South America | k       | consonant    | 0.9832985 |     0.9036424 | 0.0796562 |                    0.8883168 |            0.0949817 |
| South America | β       | consonant    | 0.1711900 |     0.1013245 | 0.0698655 |                    0.0887129 |            0.0824771 |

``` r
neg <- all %>%
  group_by(macroarea) %>%
  filter(SegmentClass == "consonant") %>%
  slice_min(order_by = delta, n = 10)
neg %>% kable()
```

| macroarea     | Phoneme | SegmentClass |      freq | phoible\_freq |      delta | freq\_phoible\_no\_macroarea | delta\_no\_macroarea |
|:--------------|:--------|:-------------|----------:|--------------:|-----------:|-----------------------------:|---------------------:|
| Africa        | ʈ       | consonant    | 0.0259887 |     0.1592715 | -0.1332828 |                    0.2161397 |           -0.1901510 |
| Africa        | pʰ      | consonant    | 0.0632768 |     0.1963576 | -0.1330808 |                    0.2472865 |           -0.1840096 |
| Africa        | kʰ      | consonant    | 0.0677966 |     0.2006623 | -0.1328656 |                    0.2515337 |           -0.1837371 |
| Africa        | ɳ       | consonant    | 0.0033898 |     0.1321192 | -0.1287294 |                    0.1868806 |           -0.1834908 |
| Africa        | n̪       | consonant    | 0.0519774 |     0.1764901 | -0.1245127 |                    0.2284096 |           -0.1764322 |
| Africa        | ɾ       | consonant    | 0.1389831 |     0.2562914 | -0.1173083 |                    0.3067485 |           -0.1677654 |
| Africa        | ɭ       | consonant    | 0.0056497 |     0.1188742 | -0.1132245 |                    0.1670599 |           -0.1614102 |
| Africa        | t̪       | consonant    | 0.1333333 |     0.2344371 | -0.1011038 |                    0.2770175 |           -0.1436841 |
| Africa        | ɻ       | consonant    | 0.0045198 |     0.1013245 | -0.0968047 |                    0.1420481 |           -0.1375284 |
| Africa        | tʰ      | consonant    | 0.0576271 |     0.1337748 | -0.0761477 |                    0.1661161 |           -0.1084890 |
| Australia     | s       | consonant    | 0.0022936 |     0.6692053 | -0.6669117 |                    0.7858255 |           -0.7835320 |
| Australia     | b       | consonant    | 0.0229358 |     0.6311258 | -0.6081900 |                    0.7363707 |           -0.7134349 |
| Australia     | h       | consonant    | 0.0091743 |     0.5639073 | -0.5547330 |                    0.6616044 |           -0.6524300 |
| Australia     | ɡ       | consonant    | 0.0229358 |     0.5668874 | -0.5439516 |                    0.6608255 |           -0.6378898 |
| Australia     | d       | consonant    | 0.0183486 |     0.4556291 | -0.4372805 |                    0.5319315 |           -0.5135828 |
| Australia     | f       | consonant    | 0.0045872 |     0.4403974 | -0.4358102 |                    0.5124611 |           -0.5078739 |
| Australia     | t̠ʃ      | consonant    | 0.0022936 |     0.4033113 | -0.4010177 |                    0.4704050 |           -0.4681114 |
| Australia     | ɲ       | consonant    | 0.0389908 |     0.4158940 | -0.3769032 |                    0.4813084 |           -0.4423176 |
| Australia     | z       | consonant    | 0.0022936 |     0.2956954 | -0.2934018 |                    0.3461838 |           -0.3438902 |
| Australia     | d̠ʒ      | consonant    | 0.0022936 |     0.2715232 | -0.2692296 |                    0.3177570 |           -0.3154634 |
| Eurasia       | t       | consonant    | 0.4502488 |     0.6834437 | -0.2331950 |                    0.7727273 |           -0.3224785 |
| Eurasia       | w       | consonant    | 0.5945274 |     0.8221854 | -0.2276581 |                    0.9045455 |           -0.3100181 |
| Eurasia       | n       | consonant    | 0.6741294 |     0.7781457 | -0.1040163 |                    0.8204545 |           -0.1463252 |
| Eurasia       | ɻ       | consonant    | 0.0087065 |     0.1013245 | -0.0926180 |                    0.1354545 |           -0.1267481 |
| Eurasia       | kʷ      | consonant    | 0.0323383 |     0.1231788 | -0.0908405 |                    0.1531818 |           -0.1208435 |
| Eurasia       | ɓ       | consonant    | 0.0248756 |     0.0993377 | -0.0744621 |                    0.1272727 |           -0.1023971 |
| Eurasia       | mb      | consonant    | 0.0385572 |     0.1046358 | -0.0660785 |                    0.1286364 |           -0.0900791 |
| Eurasia       | d       | consonant    | 0.3917910 |     0.4556291 | -0.0638381 |                    0.4813636 |           -0.0895726 |
| Eurasia       | ŋɡ      | consonant    | 0.0323383 |     0.0960265 | -0.0636882 |                    0.1190909 |           -0.0867526 |
| Eurasia       | nd      | consonant    | 0.0348259 |     0.0970199 | -0.0621940 |                    0.1200000 |           -0.0851741 |
| North America | ŋ       | consonant    | 0.2445652 |     0.6284768 | -0.3839116 |                    0.6553191 |           -0.4107539 |
| North America | ɲ       | consonant    | 0.1141304 |     0.4158940 | -0.3017636 |                    0.4368794 |           -0.3227490 |
| North America | f       | consonant    | 0.2010870 |     0.4403974 | -0.2393104 |                    0.4542553 |           -0.2531684 |
| North America | ɡ       | consonant    | 0.3532609 |     0.5668874 | -0.2136265 |                    0.5822695 |           -0.2290086 |
| North America | r       | consonant    | 0.2391304 |     0.4410596 | -0.2019292 |                    0.4556738 |           -0.2165433 |
| North America | z       | consonant    | 0.1141304 |     0.2956954 | -0.1815649 |                    0.3081560 |           -0.1940256 |
| North America | v       | consonant    | 0.0923913 |     0.2701987 | -0.1778074 |                    0.2790780 |           -0.1866867 |
| North America | b       | consonant    | 0.4619565 |     0.6311258 | -0.1691693 |                    0.6439716 |           -0.1820151 |
| North America | d       | consonant    | 0.2934783 |     0.4556291 | -0.1621509 |                    0.4680851 |           -0.1746068 |
| North America | d̠ʒ      | consonant    | 0.1250000 |     0.2715232 | -0.1465232 |                    0.2815603 |           -0.1565603 |
| Papunesia     | ʃ       | consonant    | 0.0694444 |     0.3655629 | -0.2961185 |                    0.3880918 |           -0.3186474 |
| Papunesia     | z       | consonant    | 0.0740741 |     0.2956954 | -0.2216213 |                    0.3134864 |           -0.2394123 |
| Papunesia     | j       | consonant    | 0.7083333 |     0.8993377 | -0.1910044 |                    0.9135581 |           -0.2052248 |
| Papunesia     | v       | consonant    | 0.1018519 |     0.2701987 | -0.1683468 |                    0.2804878 |           -0.1786360 |
| Papunesia     | t̠ʃ      | consonant    | 0.2407407 |     0.4033113 | -0.1625705 |                    0.4149928 |           -0.1742521 |
| Papunesia     | ɲ       | consonant    | 0.2592593 |     0.4158940 | -0.1566348 |                    0.4293400 |           -0.1700808 |
| Papunesia     | ts      | consonant    | 0.0648148 |     0.2208609 | -0.1560461 |                    0.2335007 |           -0.1686859 |
| Papunesia     | ʒ       | consonant    | 0.0138889 |     0.1582781 | -0.1443893 |                    0.1685796 |           -0.1546907 |
| Papunesia     | pʰ      | consonant    | 0.0601852 |     0.1963576 | -0.1361724 |                    0.2033716 |           -0.1431864 |
| Papunesia     | kʰ      | consonant    | 0.0648148 |     0.2006623 | -0.1358474 |                    0.2076758 |           -0.1428609 |
| South America | ŋ       | consonant    | 0.2150313 |     0.6284768 | -0.4134455 |                    0.7089109 |           -0.4938796 |
| South America | r       | consonant    | 0.0605428 |     0.4410596 | -0.3805168 |                    0.5148515 |           -0.4543087 |
| South America | f       | consonant    | 0.0751566 |     0.4403974 | -0.3652408 |                    0.5077228 |           -0.4325662 |
| South America | l       | consonant    | 0.3235908 |     0.6768212 | -0.3532304 |                    0.7429703 |           -0.4193795 |
| South America | ɡ       | consonant    | 0.2922756 |     0.5668874 | -0.2746118 |                    0.6205941 |           -0.3283185 |
| South America | v       | consonant    | 0.0313152 |     0.2701987 | -0.2388834 |                    0.3124752 |           -0.2811600 |
| South America | z       | consonant    | 0.0647182 |     0.2956954 | -0.2309772 |                    0.3401980 |           -0.2754799 |
| South America | b       | consonant    | 0.4405010 |     0.6311258 | -0.1906248 |                    0.6693069 |           -0.2288059 |
| South America | t̪       | consonant    | 0.0459290 |     0.2344371 | -0.1885081 |                    0.2704950 |           -0.2245660 |
| South America | n̪       | consonant    | 0.0187891 |     0.1764901 | -0.1577009 |                    0.2063366 |           -0.1875475 |

# Tables

This code generates tables for the appendix in the paper.

Sounds overrepresented with respect to the global mean, by macroaarea.

``` r
pos <- pos %>% select(macroarea, Phoneme, freq, phoible_freq, delta, freq_phoible_no_macroarea, delta_no_macroarea)

add.to.row <- list(pos = list(0), command = NULL)
command <- paste0(
  "\\hline\n\\endhead\n",
  "\\hline\n",
  "\\multicolumn{", dim(df)[2] + 1, "}{l}",
  "{\\footnotesize Continued on next page}\n",
  "\\endfoot\n",
  "\\endlastfoot\n"
)
add.to.row$command <- command

print(xtable(pos), hline.after = c(-1), add.to.row = add.to.row, tabular.environment = "longtable", include.rownames = FALSE)
```

    ## Warning in print.xtable(xtable(pos), hline.after = c(-1), add.to.row =
    ## add.to.row, : Attempt to use "longtable" with floating = TRUE. Changing to
    ## FALSE.

    ## % latex table generated in R 4.0.3 by xtable 1.8-4 package
    ## % Sun Dec  6 12:46:44 2020
    ## \begin{longtable}{llrrrrr}
    ##   \hline
    ## macroarea & Phoneme & freq & phoible\_freq & delta & freq\_phoible\_no\_macroarea & delta\_no\_macroarea \\ 
    ##   \hline
    ## \endhead
    ## \hline
    ## \multicolumn{}{l}{\footnotesize Continued on next page}
    ## \endfoot
    ## \endlastfoot
    ## Africa & f & 0.84 & 0.44 & 0.40 & 0.27 & 0.57 \\ 
    ##   Africa & d & 0.80 & 0.46 & 0.35 & 0.31 & 0.49 \\ 
    ##   Africa & ɡ & 0.88 & 0.57 & 0.31 & 0.44 & 0.44 \\ 
    ##   Africa & ɡb & 0.42 & 0.12 & 0.30 & 0.00 & 0.42 \\ 
    ##   Africa & kp & 0.42 & 0.12 & 0.30 & 0.00 & 0.42 \\ 
    ##   Africa & ɲ & 0.71 & 0.42 & 0.29 & 0.30 & 0.41 \\ 
    ##   Africa & b & 0.91 & 0.63 & 0.28 & 0.52 & 0.40 \\ 
    ##   Africa & z & 0.56 & 0.30 & 0.26 & 0.19 & 0.37 \\ 
    ##   Africa & v & 0.52 & 0.27 & 0.25 & 0.16 & 0.35 \\ 
    ##   Africa & s & 0.89 & 0.67 & 0.22 & 0.58 & 0.31 \\ 
    ##   Australia & ȵ & 0.87 & 0.13 & 0.74 & 0.00 & 0.87 \\ 
    ##   Australia & ȶ & 0.72 & 0.10 & 0.61 & 0.00 & 0.72 \\ 
    ##   Australia & ɻ & 0.66 & 0.10 & 0.56 & 0.01 & 0.65 \\ 
    ##   Australia & ɳ & 0.66 & 0.13 & 0.53 & 0.04 & 0.61 \\ 
    ##   Australia & ɭ & 0.63 & 0.12 & 0.51 & 0.03 & 0.60 \\ 
    ##   Australia & ʈ & 0.57 & 0.16 & 0.41 & 0.09 & 0.48 \\ 
    ##   Australia & n̪ & 0.58 & 0.18 & 0.40 & 0.11 & 0.47 \\ 
    ##   Australia & ȴ & 0.46 & 0.07 & 0.40 & 0.00 & 0.46 \\ 
    ##   Australia & ŋ & 1.00 & 0.63 & 0.37 & 0.57 & 0.43 \\ 
    ##   Australia & t̪ & 0.50 & 0.23 & 0.26 & 0.19 & 0.30 \\ 
    ##   Eurasia & pʰ & 0.52 & 0.20 & 0.33 & 0.07 & 0.45 \\ 
    ##   Eurasia & kʰ & 0.52 & 0.20 & 0.32 & 0.08 & 0.44 \\ 
    ##   Eurasia & d̪ & 0.36 & 0.14 & 0.21 & 0.07 & 0.29 \\ 
    ##   Eurasia & tʰ & 0.31 & 0.13 & 0.18 & 0.07 & 0.24 \\ 
    ##   Eurasia & x & 0.36 & 0.19 & 0.17 & 0.13 & 0.23 \\ 
    ##   Eurasia & b & 0.80 & 0.63 & 0.17 & 0.57 & 0.23 \\ 
    ##   Eurasia & ɖ & 0.24 & 0.09 & 0.16 & 0.03 & 0.21 \\ 
    ##   Eurasia & ɡ & 0.72 & 0.57 & 0.15 & 0.51 & 0.21 \\ 
    ##   Eurasia & t̪ʰ & 0.19 & 0.06 & 0.13 & 0.01 & 0.18 \\ 
    ##   Eurasia & ʒ & 0.29 & 0.16 & 0.13 & 0.11 & 0.18 \\ 
    ##   North America & ʔ & 0.86 & 0.37 & 0.48 & 0.35 & 0.51 \\ 
    ##   North America & kʼ & 0.42 & 0.08 & 0.34 & 0.05 & 0.36 \\ 
    ##   North America & h & 0.85 & 0.56 & 0.28 & 0.55 & 0.30 \\ 
    ##   North America & ʃ & 0.64 & 0.37 & 0.28 & 0.35 & 0.29 \\ 
    ##   North America & t̠ʃ & 0.67 & 0.40 & 0.27 & 0.39 & 0.28 \\ 
    ##   North America & t̠ʃʼ & 0.32 & 0.06 & 0.25 & 0.04 & 0.27 \\ 
    ##   North America & tsʼ & 0.29 & 0.04 & 0.25 & 0.03 & 0.27 \\ 
    ##   North America & tʼ & 0.30 & 0.05 & 0.25 & 0.04 & 0.26 \\ 
    ##   North America & kʷ & 0.35 & 0.12 & 0.23 & 0.11 & 0.25 \\ 
    ##   North America & pʼ & 0.29 & 0.06 & 0.23 & 0.04 & 0.25 \\ 
    ##   Papunesia & ʔ & 0.52 & 0.37 & 0.14 & 0.37 & 0.15 \\ 
    ##   Papunesia & s & 0.76 & 0.67 & 0.09 & 0.67 & 0.09 \\ 
    ##   Papunesia & β & 0.17 & 0.10 & 0.07 & 0.10 & 0.07 \\ 
    ##   Papunesia & d & 0.52 & 0.46 & 0.06 & 0.45 & 0.07 \\ 
    ##   Papunesia & ɡ & 0.62 & 0.57 & 0.06 & 0.56 & 0.06 \\ 
    ##   Papunesia & ŋ & 0.69 & 0.63 & 0.06 & 0.63 & 0.06 \\ 
    ##   Papunesia & n̪$|$n & 0.11 & 0.05 & 0.05 & 0.05 & 0.06 \\ 
    ##   Papunesia & ɸ & 0.10 & 0.05 & 0.05 & 0.05 & 0.05 \\ 
    ##   Papunesia & t̪$|$t & 0.10 & 0.05 & 0.05 & 0.05 & 0.05 \\ 
    ##   Papunesia & mb & 0.15 & 0.10 & 0.04 & 0.10 & 0.05 \\ 
    ##   South America & ɾ & 0.72 & 0.26 & 0.47 & 0.17 & 0.56 \\ 
    ##   South America & t & 0.91 & 0.68 & 0.23 & 0.64 & 0.27 \\ 
    ##   South America & t̠ʃ & 0.63 & 0.40 & 0.22 & 0.36 & 0.27 \\ 
    ##   South America & h & 0.71 & 0.56 & 0.15 & 0.54 & 0.17 \\ 
    ##   South America & ʔ & 0.52 & 0.37 & 0.14 & 0.35 & 0.17 \\ 
    ##   South America & ʃ & 0.47 & 0.37 & 0.10 & 0.35 & 0.12 \\ 
    ##   South America & ts & 0.32 & 0.22 & 0.10 & 0.20 & 0.12 \\ 
    ##   South America & p & 0.95 & 0.86 & 0.09 & 0.84 & 0.10 \\ 
    ##   South America & k & 0.98 & 0.90 & 0.08 & 0.89 & 0.09 \\ 
    ##   South America & β & 0.17 & 0.10 & 0.07 & 0.09 & 0.08 \\ 
    ##   \hline
    ## \end{longtable}

Sounds underrepresented with respect to the global mean, by macroaarea.

``` r
neg <- neg %>% select(macroarea, Phoneme, freq, phoible_freq, delta, freq_phoible_no_macroarea, delta_no_macroarea)
print(xtable(neg), hline.after = c(-1), add.to.row = add.to.row, tabular.environment = "longtable", include.rownames = FALSE)
```

    ## Warning in print.xtable(xtable(neg), hline.after = c(-1), add.to.row =
    ## add.to.row, : Attempt to use "longtable" with floating = TRUE. Changing to
    ## FALSE.

    ## % latex table generated in R 4.0.3 by xtable 1.8-4 package
    ## % Sun Dec  6 12:46:44 2020
    ## \begin{longtable}{llrrrrr}
    ##   \hline
    ## macroarea & Phoneme & freq & phoible\_freq & delta & freq\_phoible\_no\_macroarea & delta\_no\_macroarea \\ 
    ##   \hline
    ## \endhead
    ## \hline
    ## \multicolumn{}{l}{\footnotesize Continued on next page}
    ## \endfoot
    ## \endlastfoot
    ## Africa & ʈ & 0.03 & 0.16 & -0.13 & 0.22 & -0.19 \\ 
    ##   Africa & pʰ & 0.06 & 0.20 & -0.13 & 0.25 & -0.18 \\ 
    ##   Africa & kʰ & 0.07 & 0.20 & -0.13 & 0.25 & -0.18 \\ 
    ##   Africa & ɳ & 0.00 & 0.13 & -0.13 & 0.19 & -0.18 \\ 
    ##   Africa & n̪ & 0.05 & 0.18 & -0.12 & 0.23 & -0.18 \\ 
    ##   Africa & ɾ & 0.14 & 0.26 & -0.12 & 0.31 & -0.17 \\ 
    ##   Africa & ɭ & 0.01 & 0.12 & -0.11 & 0.17 & -0.16 \\ 
    ##   Africa & t̪ & 0.13 & 0.23 & -0.10 & 0.28 & -0.14 \\ 
    ##   Africa & ɻ & 0.00 & 0.10 & -0.10 & 0.14 & -0.14 \\ 
    ##   Africa & tʰ & 0.06 & 0.13 & -0.08 & 0.17 & -0.11 \\ 
    ##   Australia & s & 0.00 & 0.67 & -0.67 & 0.79 & -0.78 \\ 
    ##   Australia & b & 0.02 & 0.63 & -0.61 & 0.74 & -0.71 \\ 
    ##   Australia & h & 0.01 & 0.56 & -0.55 & 0.66 & -0.65 \\ 
    ##   Australia & ɡ & 0.02 & 0.57 & -0.54 & 0.66 & -0.64 \\ 
    ##   Australia & d & 0.02 & 0.46 & -0.44 & 0.53 & -0.51 \\ 
    ##   Australia & f & 0.00 & 0.44 & -0.44 & 0.51 & -0.51 \\ 
    ##   Australia & t̠ʃ & 0.00 & 0.40 & -0.40 & 0.47 & -0.47 \\ 
    ##   Australia & ɲ & 0.04 & 0.42 & -0.38 & 0.48 & -0.44 \\ 
    ##   Australia & z & 0.00 & 0.30 & -0.29 & 0.35 & -0.34 \\ 
    ##   Australia & d̠ʒ & 0.00 & 0.27 & -0.27 & 0.32 & -0.32 \\ 
    ##   Eurasia & t & 0.45 & 0.68 & -0.23 & 0.77 & -0.32 \\ 
    ##   Eurasia & w & 0.59 & 0.82 & -0.23 & 0.90 & -0.31 \\ 
    ##   Eurasia & n & 0.67 & 0.78 & -0.10 & 0.82 & -0.15 \\ 
    ##   Eurasia & ɻ & 0.01 & 0.10 & -0.09 & 0.14 & -0.13 \\ 
    ##   Eurasia & kʷ & 0.03 & 0.12 & -0.09 & 0.15 & -0.12 \\ 
    ##   Eurasia & ɓ & 0.02 & 0.10 & -0.07 & 0.13 & -0.10 \\ 
    ##   Eurasia & mb & 0.04 & 0.10 & -0.07 & 0.13 & -0.09 \\ 
    ##   Eurasia & d & 0.39 & 0.46 & -0.06 & 0.48 & -0.09 \\ 
    ##   Eurasia & ŋɡ & 0.03 & 0.10 & -0.06 & 0.12 & -0.09 \\ 
    ##   Eurasia & nd & 0.03 & 0.10 & -0.06 & 0.12 & -0.09 \\ 
    ##   North America & ŋ & 0.24 & 0.63 & -0.38 & 0.66 & -0.41 \\ 
    ##   North America & ɲ & 0.11 & 0.42 & -0.30 & 0.44 & -0.32 \\ 
    ##   North America & f & 0.20 & 0.44 & -0.24 & 0.45 & -0.25 \\ 
    ##   North America & ɡ & 0.35 & 0.57 & -0.21 & 0.58 & -0.23 \\ 
    ##   North America & r & 0.24 & 0.44 & -0.20 & 0.46 & -0.22 \\ 
    ##   North America & z & 0.11 & 0.30 & -0.18 & 0.31 & -0.19 \\ 
    ##   North America & v & 0.09 & 0.27 & -0.18 & 0.28 & -0.19 \\ 
    ##   North America & b & 0.46 & 0.63 & -0.17 & 0.64 & -0.18 \\ 
    ##   North America & d & 0.29 & 0.46 & -0.16 & 0.47 & -0.17 \\ 
    ##   North America & d̠ʒ & 0.12 & 0.27 & -0.15 & 0.28 & -0.16 \\ 
    ##   Papunesia & ʃ & 0.07 & 0.37 & -0.30 & 0.39 & -0.32 \\ 
    ##   Papunesia & z & 0.07 & 0.30 & -0.22 & 0.31 & -0.24 \\ 
    ##   Papunesia & j & 0.71 & 0.90 & -0.19 & 0.91 & -0.21 \\ 
    ##   Papunesia & v & 0.10 & 0.27 & -0.17 & 0.28 & -0.18 \\ 
    ##   Papunesia & t̠ʃ & 0.24 & 0.40 & -0.16 & 0.41 & -0.17 \\ 
    ##   Papunesia & ɲ & 0.26 & 0.42 & -0.16 & 0.43 & -0.17 \\ 
    ##   Papunesia & ts & 0.06 & 0.22 & -0.16 & 0.23 & -0.17 \\ 
    ##   Papunesia & ʒ & 0.01 & 0.16 & -0.14 & 0.17 & -0.15 \\ 
    ##   Papunesia & pʰ & 0.06 & 0.20 & -0.14 & 0.20 & -0.14 \\ 
    ##   Papunesia & kʰ & 0.06 & 0.20 & -0.14 & 0.21 & -0.14 \\ 
    ##   South America & ŋ & 0.22 & 0.63 & -0.41 & 0.71 & -0.49 \\ 
    ##   South America & r & 0.06 & 0.44 & -0.38 & 0.51 & -0.45 \\ 
    ##   South America & f & 0.08 & 0.44 & -0.37 & 0.51 & -0.43 \\ 
    ##   South America & l & 0.32 & 0.68 & -0.35 & 0.74 & -0.42 \\ 
    ##   South America & ɡ & 0.29 & 0.57 & -0.27 & 0.62 & -0.33 \\ 
    ##   South America & v & 0.03 & 0.27 & -0.24 & 0.31 & -0.28 \\ 
    ##   South America & z & 0.06 & 0.30 & -0.23 & 0.34 & -0.28 \\ 
    ##   South America & b & 0.44 & 0.63 & -0.19 & 0.67 & -0.23 \\ 
    ##   South America & t̪ & 0.05 & 0.23 & -0.19 & 0.27 & -0.22 \\ 
    ##   South America & n̪ & 0.02 & 0.18 & -0.16 & 0.21 & -0.19 \\ 
    ##   \hline
    ## \end{longtable}

``` r
library(xtable)
print(xtable(all %>% group_by(macroarea) %>% slice_max(order_by = delta, n = 10)))
```

    ## % latex table generated in R 4.0.3 by xtable 1.8-4 package
    ## % Sun Dec  6 12:46:44 2020
    ## \begin{table}[ht]
    ## \centering
    ## \begin{tabular}{rlllrrrrr}
    ##   \hline
    ##  & macroarea & Phoneme & SegmentClass & freq & phoible\_freq & delta & freq\_phoible\_no\_macroarea & delta\_no\_macroarea \\ 
    ##   \hline
    ## 1 & Africa & f & consonant & 0.84 & 0.44 & 0.40 & 0.27 & 0.57 \\ 
    ##   2 & Africa & ˨ & tone & 0.58 & 0.18 & 0.40 & 0.02 & 0.56 \\ 
    ##   3 & Africa & ˦ & tone & 0.58 & 0.18 & 0.40 & 0.02 & 0.56 \\ 
    ##   4 & Africa & d & consonant & 0.80 & 0.46 & 0.35 & 0.31 & 0.49 \\ 
    ##   5 & Africa & ɡ & consonant & 0.88 & 0.57 & 0.31 & 0.44 & 0.44 \\ 
    ##   6 & Africa & ɔ & vowel & 0.66 & 0.35 & 0.31 & 0.23 & 0.44 \\ 
    ##   7 & Africa & ɡb & consonant & 0.42 & 0.12 & 0.30 & 0.00 & 0.42 \\ 
    ##   8 & Africa & kp & consonant & 0.42 & 0.12 & 0.30 & 0.00 & 0.42 \\ 
    ##   9 & Africa & ɲ & consonant & 0.71 & 0.42 & 0.29 & 0.30 & 0.41 \\ 
    ##   10 & Africa & b & consonant & 0.91 & 0.63 & 0.28 & 0.52 & 0.40 \\ 
    ##   11 & Australia & ȵ & consonant & 0.87 & 0.13 & 0.74 & 0.00 & 0.87 \\ 
    ##   12 & Australia & ȶ & consonant & 0.72 & 0.10 & 0.61 & 0.00 & 0.72 \\ 
    ##   13 & Australia & ɻ & consonant & 0.66 & 0.10 & 0.56 & 0.01 & 0.65 \\ 
    ##   14 & Australia & ɳ & consonant & 0.66 & 0.13 & 0.53 & 0.04 & 0.61 \\ 
    ##   15 & Australia & ɭ & consonant & 0.63 & 0.12 & 0.51 & 0.03 & 0.60 \\ 
    ##   16 & Australia & ʈ & consonant & 0.57 & 0.16 & 0.41 & 0.09 & 0.48 \\ 
    ##   17 & Australia & n̪ & consonant & 0.58 & 0.18 & 0.40 & 0.11 & 0.47 \\ 
    ##   18 & Australia & ȴ & consonant & 0.46 & 0.07 & 0.40 & 0.00 & 0.46 \\ 
    ##   19 & Australia & ŋ & consonant & 1.00 & 0.63 & 0.37 & 0.57 & 0.43 \\ 
    ##   20 & Australia & t̪ & consonant & 0.50 & 0.23 & 0.26 & 0.19 & 0.30 \\ 
    ##   21 & Eurasia & pʰ & consonant & 0.52 & 0.20 & 0.33 & 0.07 & 0.45 \\ 
    ##   22 & Eurasia & kʰ & consonant & 0.52 & 0.20 & 0.32 & 0.08 & 0.44 \\ 
    ##   23 & Eurasia & d̪ & consonant & 0.36 & 0.14 & 0.21 & 0.07 & 0.29 \\ 
    ##   24 & Eurasia & tʰ & consonant & 0.31 & 0.13 & 0.18 & 0.07 & 0.24 \\ 
    ##   25 & Eurasia & x & consonant & 0.36 & 0.19 & 0.17 & 0.13 & 0.23 \\ 
    ##   26 & Eurasia & b & consonant & 0.80 & 0.63 & 0.17 & 0.57 & 0.23 \\ 
    ##   27 & Eurasia & ɖ & consonant & 0.24 & 0.09 & 0.16 & 0.03 & 0.21 \\ 
    ##   28 & Eurasia & ɡ & consonant & 0.72 & 0.57 & 0.15 & 0.51 & 0.21 \\ 
    ##   29 & Eurasia & t̪ʰ & consonant & 0.19 & 0.06 & 0.13 & 0.01 & 0.18 \\ 
    ##   30 & Eurasia & ʒ & consonant & 0.29 & 0.16 & 0.13 & 0.11 & 0.18 \\ 
    ##   31 & North America & ʔ & consonant & 0.86 & 0.37 & 0.48 & 0.35 & 0.51 \\ 
    ##   32 & North America & kʼ & consonant & 0.42 & 0.08 & 0.34 & 0.05 & 0.36 \\ 
    ##   33 & North America & h & consonant & 0.85 & 0.56 & 0.28 & 0.55 & 0.30 \\ 
    ##   34 & North America & ʃ & consonant & 0.64 & 0.37 & 0.28 & 0.35 & 0.29 \\ 
    ##   35 & North America & t̠ʃ & consonant & 0.67 & 0.40 & 0.27 & 0.39 & 0.28 \\ 
    ##   36 & North America & t̠ʃʼ & consonant & 0.32 & 0.06 & 0.25 & 0.04 & 0.27 \\ 
    ##   37 & North America & tsʼ & consonant & 0.29 & 0.04 & 0.25 & 0.03 & 0.27 \\ 
    ##   38 & North America & tʼ & consonant & 0.30 & 0.05 & 0.25 & 0.04 & 0.26 \\ 
    ##   39 & North America & kʷ & consonant & 0.35 & 0.12 & 0.23 & 0.11 & 0.25 \\ 
    ##   40 & North America & pʼ & consonant & 0.29 & 0.06 & 0.23 & 0.04 & 0.25 \\ 
    ##   41 & Papunesia & ʔ & consonant & 0.52 & 0.37 & 0.14 & 0.37 & 0.15 \\ 
    ##   42 & Papunesia & o̞ & vowel & 0.20 & 0.10 & 0.10 & 0.08 & 0.11 \\ 
    ##   43 & Papunesia & s & consonant & 0.76 & 0.67 & 0.09 & 0.67 & 0.09 \\ 
    ##   44 & Papunesia & β & consonant & 0.17 & 0.10 & 0.07 & 0.10 & 0.07 \\ 
    ##   45 & Papunesia & e̞ & vowel & 0.16 & 0.09 & 0.06 & 0.09 & 0.07 \\ 
    ##   46 & Papunesia & d & consonant & 0.52 & 0.46 & 0.06 & 0.45 & 0.07 \\ 
    ##   47 & Papunesia & u & vowel & 0.94 & 0.88 & 0.06 & 0.87 & 0.06 \\ 
    ##   48 & Papunesia & ɡ & consonant & 0.62 & 0.57 & 0.06 & 0.56 & 0.06 \\ 
    ##   49 & Papunesia & ŋ & consonant & 0.69 & 0.63 & 0.06 & 0.63 & 0.06 \\ 
    ##   50 & Papunesia & n̪$|$n & consonant & 0.11 & 0.05 & 0.05 & 0.05 & 0.06 \\ 
    ##   51 & South America & ɾ & consonant & 0.72 & 0.26 & 0.47 & 0.17 & 0.56 \\ 
    ##   52 & South America & ɨ & vowel & 0.51 & 0.16 & 0.35 & 0.10 & 0.41 \\ 
    ##   53 & South America & ã & vowel & 0.42 & 0.17 & 0.25 & 0.13 & 0.30 \\ 
    ##   54 & South America & ĩ & vowel & 0.42 & 0.18 & 0.24 & 0.14 & 0.28 \\ 
    ##   55 & South America & t & consonant & 0.91 & 0.68 & 0.23 & 0.64 & 0.27 \\ 
    ##   56 & South America & t̠ʃ & consonant & 0.63 & 0.40 & 0.22 & 0.36 & 0.27 \\ 
    ##   57 & South America & ɨ̃ & vowel & 0.25 & 0.04 & 0.21 & 0.00 & 0.24 \\ 
    ##   58 & South America & ẽ & vowel & 0.29 & 0.11 & 0.19 & 0.07 & 0.22 \\ 
    ##   59 & South America & õ & vowel & 0.30 & 0.11 & 0.18 & 0.08 & 0.22 \\ 
    ##   60 & South America & ũ & vowel & 0.34 & 0.16 & 0.17 & 0.13 & 0.21 \\ 
    ##    \hline
    ## \end{tabular}
    ## \end{table}

``` r
print(xtable(all %>% group_by(macroarea) %>% slice_min(order_by = delta, n = 10)))
```

    ## % latex table generated in R 4.0.3 by xtable 1.8-4 package
    ## % Sun Dec  6 12:46:45 2020
    ## \begin{table}[ht]
    ## \centering
    ## \begin{tabular}{rlllrrrrr}
    ##   \hline
    ##  & macroarea & Phoneme & SegmentClass & freq & phoible\_freq & delta & freq\_phoible\_no\_macroarea & delta\_no\_macroarea \\ 
    ##   \hline
    ## 1 & Africa & ʈ & consonant & 0.03 & 0.16 & -0.13 & 0.22 & -0.19 \\ 
    ##   2 & Africa & pʰ & consonant & 0.06 & 0.20 & -0.13 & 0.25 & -0.18 \\ 
    ##   3 & Africa & kʰ & consonant & 0.07 & 0.20 & -0.13 & 0.25 & -0.18 \\ 
    ##   4 & Africa & ɳ & consonant & 0.00 & 0.13 & -0.13 & 0.19 & -0.18 \\ 
    ##   5 & Africa & n̪ & consonant & 0.05 & 0.18 & -0.12 & 0.23 & -0.18 \\ 
    ##   6 & Africa & ɾ & consonant & 0.14 & 0.26 & -0.12 & 0.31 & -0.17 \\ 
    ##   7 & Africa & ɭ & consonant & 0.01 & 0.12 & -0.11 & 0.17 & -0.16 \\ 
    ##   8 & Africa & t̪ & consonant & 0.13 & 0.23 & -0.10 & 0.28 & -0.14 \\ 
    ##   9 & Africa & ɻ & consonant & 0.00 & 0.10 & -0.10 & 0.14 & -0.14 \\ 
    ##   10 & Africa & tʰ & consonant & 0.06 & 0.13 & -0.08 & 0.17 & -0.11 \\ 
    ##   11 & Australia & s & consonant & 0.00 & 0.67 & -0.67 & 0.79 & -0.78 \\ 
    ##   12 & Australia & b & consonant & 0.02 & 0.63 & -0.61 & 0.74 & -0.71 \\ 
    ##   13 & Australia & h & consonant & 0.01 & 0.56 & -0.55 & 0.66 & -0.65 \\ 
    ##   14 & Australia & ɡ & consonant & 0.02 & 0.57 & -0.54 & 0.66 & -0.64 \\ 
    ##   15 & Australia & d & consonant & 0.02 & 0.46 & -0.44 & 0.53 & -0.51 \\ 
    ##   16 & Australia & f & consonant & 0.00 & 0.44 & -0.44 & 0.51 & -0.51 \\ 
    ##   17 & Australia & t̠ʃ & consonant & 0.00 & 0.40 & -0.40 & 0.47 & -0.47 \\ 
    ##   18 & Australia & ɲ & consonant & 0.04 & 0.42 & -0.38 & 0.48 & -0.44 \\ 
    ##   19 & Australia & o & vowel & 0.24 & 0.60 & -0.37 & 0.67 & -0.43 \\ 
    ##   20 & Australia & ɛ & vowel & 0.03 & 0.37 & -0.35 & 0.43 & -0.41 \\ 
    ##   21 & Eurasia & t & consonant & 0.45 & 0.68 & -0.23 & 0.77 & -0.32 \\ 
    ##   22 & Eurasia & w & consonant & 0.59 & 0.82 & -0.23 & 0.90 & -0.31 \\ 
    ##   23 & Eurasia & a & vowel & 0.64 & 0.86 & -0.22 & 0.95 & -0.31 \\ 
    ##   24 & Eurasia & ˨ & tone & 0.02 & 0.18 & -0.16 & 0.24 & -0.22 \\ 
    ##   25 & Eurasia & ˦ & tone & 0.02 & 0.18 & -0.16 & 0.24 & -0.22 \\ 
    ##   26 & Eurasia & n & consonant & 0.67 & 0.78 & -0.10 & 0.82 & -0.15 \\ 
    ##   27 & Eurasia & ɻ & consonant & 0.01 & 0.10 & -0.09 & 0.14 & -0.13 \\ 
    ##   28 & Eurasia & i & vowel & 0.83 & 0.92 & -0.09 & 0.95 & -0.12 \\ 
    ##   29 & Eurasia & kʷ & consonant & 0.03 & 0.12 & -0.09 & 0.15 & -0.12 \\ 
    ##   30 & Eurasia & ã & vowel & 0.08 & 0.17 & -0.09 & 0.21 & -0.12 \\ 
    ##   31 & North America & ŋ & consonant & 0.24 & 0.63 & -0.38 & 0.66 & -0.41 \\ 
    ##   32 & North America & ɲ & consonant & 0.11 & 0.42 & -0.30 & 0.44 & -0.32 \\ 
    ##   33 & North America & e & vowel & 0.36 & 0.61 & -0.25 & 0.63 & -0.27 \\ 
    ##   34 & North America & f & consonant & 0.20 & 0.44 & -0.24 & 0.45 & -0.25 \\ 
    ##   35 & North America & ɡ & consonant & 0.35 & 0.57 & -0.21 & 0.58 & -0.23 \\ 
    ##   36 & North America & r & consonant & 0.24 & 0.44 & -0.20 & 0.46 & -0.22 \\ 
    ##   37 & North America & z & consonant & 0.11 & 0.30 & -0.18 & 0.31 & -0.19 \\ 
    ##   38 & North America & v & consonant & 0.09 & 0.27 & -0.18 & 0.28 & -0.19 \\ 
    ##   39 & North America & b & consonant & 0.46 & 0.63 & -0.17 & 0.64 & -0.18 \\ 
    ##   40 & North America & d & consonant & 0.29 & 0.46 & -0.16 & 0.47 & -0.17 \\ 
    ##   41 & Papunesia & ʃ & consonant & 0.07 & 0.37 & -0.30 & 0.39 & -0.32 \\ 
    ##   42 & Papunesia & z & consonant & 0.07 & 0.30 & -0.22 & 0.31 & -0.24 \\ 
    ##   43 & Papunesia & iː & vowel & 0.12 & 0.32 & -0.19 & 0.33 & -0.21 \\ 
    ##   44 & Papunesia & j & consonant & 0.71 & 0.90 & -0.19 & 0.91 & -0.21 \\ 
    ##   45 & Papunesia & uː & vowel & 0.11 & 0.29 & -0.18 & 0.31 & -0.20 \\ 
    ##   46 & Papunesia & aː & vowel & 0.12 & 0.30 & -0.17 & 0.31 & -0.19 \\ 
    ##   47 & Papunesia & v & consonant & 0.10 & 0.27 & -0.17 & 0.28 & -0.18 \\ 
    ##   48 & Papunesia & ˦ & tone & 0.02 & 0.18 & -0.16 & 0.20 & -0.18 \\ 
    ##   49 & Papunesia & t̠ʃ & consonant & 0.24 & 0.40 & -0.16 & 0.41 & -0.17 \\ 
    ##   50 & Papunesia & ˨ & tone & 0.02 & 0.18 & -0.16 & 0.19 & -0.18 \\ 
    ##   51 & South America & ŋ & consonant & 0.22 & 0.63 & -0.41 & 0.71 & -0.49 \\ 
    ##   52 & South America & r & consonant & 0.06 & 0.44 & -0.38 & 0.51 & -0.45 \\ 
    ##   53 & South America & f & consonant & 0.08 & 0.44 & -0.37 & 0.51 & -0.43 \\ 
    ##   54 & South America & l & consonant & 0.32 & 0.68 & -0.35 & 0.74 & -0.42 \\ 
    ##   55 & South America & ɡ & consonant & 0.29 & 0.57 & -0.27 & 0.62 & -0.33 \\ 
    ##   56 & South America & ɔ & vowel & 0.11 & 0.35 & -0.24 & 0.40 & -0.29 \\ 
    ##   57 & South America & v & consonant & 0.03 & 0.27 & -0.24 & 0.31 & -0.28 \\ 
    ##   58 & South America & z & consonant & 0.06 & 0.30 & -0.23 & 0.34 & -0.28 \\ 
    ##   59 & South America & ɛ & vowel & 0.16 & 0.37 & -0.22 & 0.42 & -0.26 \\ 
    ##   60 & South America & b & consonant & 0.44 & 0.63 & -0.19 & 0.67 & -0.23 \\ 
    ##    \hline
    ## \end{tabular}
    ## \end{table}
