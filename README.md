
[![Build
Status](https://travis-ci.org/oganm/diceSyntax.svg?branch=master)](https://travis-ci.org/oganm/diceSyntax)
[![codecov](https://codecov.io/gh/oganm/diceSyntax/branch/master/graph/badge.svg)](https://codecov.io/gh/oganm/diceSyntax)

# diceSyntax

``` r
roll('d6') # roll d6
```

    ## [1] "Rolls: [ 5 ] (d6)"

    ## [1] 5

``` r
roll('4d6') # roll 4d6
```

    ## [1] "Rolls: [ *6* 5 *6* 3 ] (4d6)"

    ## [1] 20

``` r
roll('4d6k3') # roll 4d6 keep 3 highest
```

    ## [1] "Rolls: [ 2 4 5 ] (4d6k3)"
    ## [1] "Dropped: [ *1* ]"

    ## [1] 11

``` r
roll('4d6kl3') # roll 4d6 keep lowest 3
```

    ## [1] "Rolls: [ *1* *1* 5 ] (4d6kl3)"
    ## [1] "Dropped: [ *6* ]"

    ## [1] 7

``` r
roll('4d6d1') # roll 4d6 drop 1 lowest
```

    ## [1] "Rolls: [ 3 3 3 ] (4d6d1)"
    ## [1] "Dropped: [ *1* ]"

    ## [1] 9

``` r
roll('4d6dh1') # roll 4d6 drop highest 1
```

    ## [1] "Rolls: [ 3 2 3 ] (4d6dh1)"
    ## [1] "Dropped: [ *6* ]"

    ## [1] 8

``` r
roll('4d6+3') # roll 4d6 add 3
```

    ## [1] "Rolls: [ 2 *6* 5 4 ] (4d6)"

    ## [1] 20

``` r
roll('4d6r1r2') # roll 4d6 reroll 1s and 2s
```

    ## [1] "Rolls: [ 4 5 5 *3* ] (4d6r1r2)"

    ## [1] 17

``` r
roll('4d6r<2') # same as above
```

    ## [1] "Rolls: [ 4 *6* *3* *3* ] (4d6r<2)"

    ## [1] 16

``` r
roll('4d6r1ro2') # roll 4d6 reroll 1s but reroll 2s only once
```

    ## [1] "Rolls: [ 5 3 3 *6* ] (4d6r1ro2)"

    ## [1] 17

``` r
roll('5d2!') # exploding dice
```

    ## [1] "Rolls: [ *2* *2* *2* *1* *2* *1* *1* *2* *1* *1* ] (5d2!)"

    ## [1] 15

``` r
roll('5d2!!') # compounding dice
```

    ## [1] "Rolls: [ *1* *1* 3 3 5 ] (5d2!!)"

    ## [1] 13

``` r
roll('6d6+4d4-3d5') # multiple rolls
```

    ## [1] "Rolls: [ 5 *1* 3 2 5 2 ] (6d6)"
    ## [1] "Rolls: [ *4* *4* *4* *1* ] (4d4)"
    ## [1] "Rolls: [ *5* *1* 4 ] (3d5)"

    ## [1] 21

``` r
r('1d6') # shortcut function
```

    ## [1] "Rolls: [ *6* ] (1d6)"

    ## [1] 6

Other variables that `roll` funciton accepts are

  - `critMark`: `TRUE` by default. If `TRUE` it adds stars around the
    dice roll in printed output if it is max or min value for the dice
  - `vocal`: `TRUE` by default. If `FALSE` disables printing of dice
    rolls
  - `returnRolls`: `FALSE` by default. If `TRUE` returns a list instead
    of an integer that includes rolled and dropped dice along with the
    dice sum

<!-- end list -->

``` r
r("10d10dl3",returnRolls = TRUE, vocal = FALSE)
```

    ## $`10d10dl3`
    ## $`10d10dl3`$result
    ## [1] 47
    ## 
    ## $`10d10dl3`$dice
    ## [1]  7  6  4  7 10  7  6
    ## 
    ## $`10d10dl3`$drop
    ## [1] 4 1 2
    ## 
    ## 
    ## $result
    ## [1] 47

## Dice stats

Probabilities of outcomes can be calculated with `diceProb` function

``` r
diceProb('4d6d1') %>% plot(names(.) %>% as.numeric(),.)
```

![](README_files/figure-gfm/unnamed-chunk-3-1.png)<!-- -->
