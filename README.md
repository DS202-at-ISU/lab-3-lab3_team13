
<!-- README.md is generated from README.Rmd. Please edit the README.Rmd file -->

# Lab report \#3 - instructions

Follow the instructions posted at
<https://ds202-at-isu.github.io/labs.html> for the lab assignment. The
work is meant to be finished during the lab time, but you have time
until Monday evening to polish things.

Include your answers in this document (Rmd file). Make sure that it
knits properly (into the md file). Upload both the Rmd and the md file
to your repository.

All submissions to the github repo will be automatically uploaded for
grading once the due date is passed. Submit a link to your repository on
Canvas (only one submission per team) to signal to the instructors that
you are done with your submission.

# Lab 3: Avenger’s Peril

## As a team

Extract from the data below two data sets in long form `deaths` and
`returns`

``` r
av <- read.csv("https://raw.githubusercontent.com/fivethirtyeight/data/master/avengers/avengers.csv", stringsAsFactors = FALSE)
head(av)
```

    ##                                                       URL
    ## 1           http://marvel.wikia.com/Henry_Pym_(Earth-616)
    ## 2      http://marvel.wikia.com/Janet_van_Dyne_(Earth-616)
    ## 3       http://marvel.wikia.com/Anthony_Stark_(Earth-616)
    ## 4 http://marvel.wikia.com/Robert_Bruce_Banner_(Earth-616)
    ## 5        http://marvel.wikia.com/Thor_Odinson_(Earth-616)
    ## 6       http://marvel.wikia.com/Richard_Jones_(Earth-616)
    ##                    Name.Alias Appearances Current. Gender Probationary.Introl
    ## 1   Henry Jonathan "Hank" Pym        1269      YES   MALE                    
    ## 2              Janet van Dyne        1165      YES FEMALE                    
    ## 3 Anthony Edward "Tony" Stark        3068      YES   MALE                    
    ## 4         Robert Bruce Banner        2089      YES   MALE                    
    ## 5                Thor Odinson        2402      YES   MALE                    
    ## 6      Richard Milhouse Jones         612      YES   MALE                    
    ##   Full.Reserve.Avengers.Intro Year Years.since.joining Honorary Death1 Return1
    ## 1                      Sep-63 1963                  52     Full    YES      NO
    ## 2                      Sep-63 1963                  52     Full    YES     YES
    ## 3                      Sep-63 1963                  52     Full    YES     YES
    ## 4                      Sep-63 1963                  52     Full    YES     YES
    ## 5                      Sep-63 1963                  52     Full    YES     YES
    ## 6                      Sep-63 1963                  52 Honorary     NO        
    ##   Death2 Return2 Death3 Return3 Death4 Return4 Death5 Return5
    ## 1                                                            
    ## 2                                                            
    ## 3                                                            
    ## 4                                                            
    ## 5    YES      NO                                             
    ## 6                                                            
    ##                                                                                                                                                                              Notes
    ## 1                                                                                                                Merged with Ultron in Rage of Ultron Vol. 1. A funeral was held. 
    ## 2                                                                                                  Dies in Secret Invasion V1:I8. Actually was sent tto Microverse later recovered
    ## 3 Death: "Later while under the influence of Immortus Stark committed a number of horrible acts and was killed.'  This set up young Tony. Franklin Richards later brought him back
    ## 4                                                                               Dies in Ghosts of the Future arc. However "he had actually used a hidden Pantheon base to survive"
    ## 5                                                      Dies in Fear Itself brought back because that's kind of the whole point. Second death in Time Runs Out has not yet returned
    ## 6                                                                                                                                                                             <NA>

Get the data into a format where the five columns for Death\[1-5\] are
replaced by two columns: Time, and Death. Time should be a number
between 1 and 5 (look into the function `parse_number`); Death is a
categorical variables with values “yes”, “no” and ““. Call the resulting
data set `deaths`.

``` r
library(dplyr)
```

    ## 
    ## Attaching package: 'dplyr'

    ## The following objects are masked from 'package:stats':
    ## 
    ##     filter, lag

    ## The following objects are masked from 'package:base':
    ## 
    ##     intersect, setdiff, setequal, union

``` r
library(tidyverse)
```

    ## ── Attaching core tidyverse packages ──────────────────────── tidyverse 2.0.0 ──
    ## ✔ forcats   1.0.0     ✔ readr     2.1.5
    ## ✔ ggplot2   3.4.4     ✔ stringr   1.5.1
    ## ✔ lubridate 1.9.3     ✔ tibble    3.2.1
    ## ✔ purrr     1.0.2     ✔ tidyr     1.3.0

    ## ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
    ## ✖ dplyr::filter() masks stats::filter()
    ## ✖ dplyr::lag()    masks stats::lag()
    ## ℹ Use the conflicted package (<http://conflicted.r-lib.org/>) to force all conflicts to become errors

``` r
av_deaths <- av %>%
  gather(key = "Time", value = "Death", starts_with("Death")) %>%
  mutate(Time = as.integer(gsub("Death", "", Time)), 
         Death = case_when(
           Death == "YES" ~ "yes",
           Death == "NO" ~ "no",
           TRUE ~ ""
         ))

deaths_per_avenger <- av_deaths %>%
  filter(Death == "yes") %>%
  group_by(Name.Alias) %>%
  summarize(Deaths = n())

average_deaths <- mean(deaths_per_avenger$Deaths)
average_deaths
```

    ## [1] 1.390625

Similarly, deal with the returns of characters.

Based on these datasets calculate the average number of deaths an
Avenger suffers.

## Individually

For each team member, copy this part of the report.

Each team member picks one of the statements in the FiveThirtyEight
[analysis](https://fivethirtyeight.com/features/avengers-death-comics-age-of-ultron/)
and fact checks it based on the data. Use dplyr functionality whenever
possible.

### FiveThirtyEight Statement

> Quote the statement you are planning to fact-check. “Given the
> Avengers’ 53 years in operation and overall mortality rate, fans of
> the comics can expect one current or former member to die every seven
> months or so, with a permanent death occurring once every 20 months.”

### Include the code (Nicholas Olech)

``` r
#Calculate the total amount of deaths
total_deaths <- sum(av$Death1 =="YES"| av$Death2 =="YES" | av$Death3 =="Yes"|av$Death4 =="Yes"|av$Death5 =="YES")
#total_deaths

#Calculate the total amount of returns
total_returns <- sum(av$Return1 =="YES"| av$Return2 =="YES" | av$Return3 =="Yes"|av$Return4 =="Yes"|av$Return5 =="YES")
#total_returns

#Find the total amount of permanent deaths
perm_deaths <- total_deaths - total_returns

#Avengers have existed for 53 years, convert that to months to verify statistic
months_of_avengers <- 53 * 12

#To see how often an avenger is  killed, but not permanently
killed_frequency <- months_of_avengers / total_deaths
killed_frequency
```

    ## [1] 9.217391

``` r
#To see how often an avenger is killed permanently
perm_killed_frequency <- months_of_avengers / perm_deaths
perm_killed_frequency
```

    ## [1] 27.65217

Make sure to include the code to derive the (numeric) fact for the
statement

### Include your answer (Nicholas Olech)

Based on my analysis, this statistic appears to be misleading, likely
due to a discrepancy between the date the article was published and when
the data set was updated. Instead of a death occurring 7 months, like
the article stated, it appears to happen just once every ~9 months. Also
the permanent death occurs only once every ~28 months which is also
different from the articles statistic of 20.

### FiveThirtyEight Statement (Carter Parks)

> There’s a 2-in-3 chance that a member of the Avengers returned from
> their first stint in the afterlife, but only a 50 percent chance they
> recovered from a second or third death.8

### Include the code (Carter Parks)

``` r
death1Return <- av %>%
  filter(Death1 == 'YES', Return1 == 'YES') %>%
  nrow()

death1Dead <- av %>%
  filter(Death1 == 'YES', Return1 == 'NO') %>%
  nrow()

death1Total <- death1Return + death1Dead

death2Return <- av %>%
  filter(Death2 == 'YES', Return2 == 'YES') %>%
  nrow()

death2Dead <- av %>%
  filter(Death2 == 'YES', Return2 == 'NO') %>%
  nrow()

death2Total <- death2Return + death2Dead

death3Return <- av %>%
  filter(Death2 == 'YES', Return2 == 'YES') %>%
  nrow()

death3Dead <- av %>%
  filter(Death2 == 'YES', Return2 == 'NO') %>%
  nrow()

death3Total <- death3Return + death3Dead

print(paste("Percentage returning from death 1:", death1Return / death1Total))
```

    ## [1] "Percentage returning from death 1: 0.666666666666667"

``` r
print(paste("Percentage returning from death 2:", death2Return / death2Total))
```

    ## [1] "Percentage returning from death 2: 0.5"

``` r
print(paste("Percentage returning from death 3:", death3Return / death3Total))
```

    ## [1] "Percentage returning from death 3: 0.5"

Make sure to include the code to derive the (numeric) fact for the
statement

### Include your answer (Carter Parks)

Exactly 2/3s of avengers came back from their first death, while only
50% of avengers came back from their 2nd and 3rd deaths.

Upload your changes to the repository. Discuss and refine answers as a
team.

### FiveThirtyEight Statement (Chris Draper)

> “Out of 173 listed Avengers, my analysis found that 69 had died at
> least one time after they joined the team.5 That’s about 40 percent of
> all people who have ever signed on to the team.”

### Include the code (Chris Draper)

``` r
avengers_deaths <- av %>%
  
  mutate(Has_Died = if_else(Death1 == "YES" | Death2 == "YES" | Death3 == "YES" | Death4 == "YES" | Death5 == "YES", TRUE, FALSE)) %>%
 
  distinct(Name.Alias, .keep_all = TRUE)


total_avengers = nrow(avengers_deaths)

avengers_died_once = sum(avengers_deaths$Has_Died)

percentage_died_once = (avengers_died_once / total_avengers) * 100


cat("Out of", total_avengers, "listed Avengers,", avengers_died_once, "have died at least once after they joined the team.\n")
```

    ## Out of 163 listed Avengers, 64 have died at least once after they joined the team.

``` r
cat("That’s about", round(percentage_died_once, 2), "percent of all people who have ever signed on to the team.")
```

    ## That’s about 39.26 percent of all people who have ever signed on to the team.

Make sure to include the code to derive the (numeric) fact for the
statement

### Include your answer (Chris Draper)

- I am calculating less deaths and slightly less death rate than 40%.
  However, the original statement is true enough and not misleading.

Upload your changes to the repository. Discuss and refine answers as a
team.
