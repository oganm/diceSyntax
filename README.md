
[![Build Status](https://travis-ci.org/oganm/diceSyntax.svg?branch=master)](https://travis-ci.org/oganm/diceSyntax) [![codecov](https://codecov.io/gh/oganm/diceSyntax/branch/master/graph/badge.svg)](https://codecov.io/gh/oganm/diceSyntax)

diceSyntax
==========

``` r
roll('4d6') # roll 4d6
```

    ## [1] "Rolls: [ 3 4 *6* *6* ]"

    ## [1] 19

``` r
roll('4d6k3') # roll 4d6 keep 3 highest
```

    ## [1] "Rolls: [ *6* *1* 5 ]"
    ## [1] "Dropped: [ *1* ]"

    ## [1] 12

``` r
roll('4d6kl3') # roll 4d6 keep lowest 3
```

    ## [1] "Rolls: [ 4 2 3 ]"
    ## [1] "Dropped: [ *6* ]"

    ## [1] 9

``` r
roll('4d6d1') # roll 4d6 drop 1 lowest
```

    ## [1] "Rolls: [ 2 *6* 3 ]"
    ## [1] "Dropped: [ *1* ]"

    ## [1] 11

``` r
roll('4d6dh1') # roll 4d6 drop highest 1
```

    ## [1] "Rolls: [ 3 4 2 ]"
    ## [1] "Dropped: [ 5 ]"

    ## [1] 9

``` r
roll('4d6+3') # roll 4d6 add 3
```

    ## [1] "Rolls: [ 5 2 *1* 4 ]"

    ## [1] 15

``` r
roll('4d6r1r2') # roll 4d6 reroll 1s and 2s
```

    ## [1] "Rolls: [ 5 4 4 4 ]"

    ## [1] 17

``` r
roll('4d6r<2') # same as above
```

    ## [1] "Rolls: [ *6* 4 4 4 ]"

    ## [1] 18

``` r
roll('4d6r1ro2') # roll 4d6 reroll 1s but reroll 2s only once
```

    ## [1] "Rolls: [ 2 *6* *6* 5 ]"

    ## [1] 19

``` r
r('1d6') # shortcut function
```

    ## [1] "Rolls: [ 4 ]"

    ## [1] 4

``` r
r(r1d6) # non standard evaluation
```

    ## [1] "Rolls: [ 3 ]"

    ## [1] 3

While `roll` and `r` allows non standard evaluation, variable names for character values can be used as long as they don't match the following regex: `^r[0-9]+d[0-9]+` or the variable name will be interpreted as a dice roll.

``` r
myRoll = '10d10'
r4d6 = '10d10'

r(myRoll)
```

    ## [1] "Rolls: [ 7 5 3 7 4 2 3 7 *10* 4 ]"

    ## [1] 52

``` r
r(r4d6)
```

    ## [1] "Rolls: [ *6* 5 2 3 ]"

    ## [1] 16

Other variables that `roll` funciton accepts are

-   `critMark`: `TRUE` by default. If `TRUE` it adds stars around the dice roll in printed output if it is max or min value for the dice
-   `vocal`: `TRUE` by default. If `FALSE` disables printing of dice rolls
-   `returnRolls`: `FALSE` by default. If `TRUE` returns a list instead of an integer that includes rolled and dropped dice along with the dice sum

``` r
r(r10d10dl3,returnRolls = TRUE, vocal = FALSE)
```

    ## $result
    ## [1] 47
    ## 
    ## $dice
    ## [1] 5 5 9 8 8 5 7
    ## 
    ## $drop
    ## [1] 3 4 1
