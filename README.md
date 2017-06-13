
[![Build Status](https://travis-ci.org/oganm/diceSyntax.svg?branch=master)](https://travis-ci.org/oganm/diceSyntax) [![codecov](https://codecov.io/gh/oganm/diceSyntax/branch/master/graph/badge.svg)](https://codecov.io/gh/oganm/diceSyntax)

diceSyntax
==========

``` r
roll('4d6') # roll 4d6
```

    ## [1] "Rolls: [ *1* 2 *1* 2 ]"

    ## [1] 6

``` r
roll('4d6k3') # roll 4d6 keep 3 highest
```

    ## [1] "Rolls: [ 4 5 4 ]"
    ## [1] "Dropped: [ *1* ]"

    ## [1] 13

``` r
roll('4d6kl3') # roll 4d6 keep lowest 3
```

    ## [1] "Rolls: [ 2 4 *1* ]"
    ## [1] "Dropped: [ *6* ]"

    ## [1] 7

``` r
roll('4d6d1') # roll 4d6 drop 1 lowest
```

    ## [1] "Rolls: [ *6* 3 4 ]"
    ## [1] "Dropped: [ 3 ]"

    ## [1] 13

``` r
roll('4d6dh1') # roll 4d6 drop highest 1
```

    ## [1] "Rolls: [ 5 4 2 ]"
    ## [1] "Dropped: [ *6* ]"

    ## [1] 11

``` r
roll('4d6+3') # roll 4d6 add 3
```

    ## [1] "Rolls: [ *6* *6* 3 4 ]"

    ## [1] 22

``` r
roll('4d6r1r2') # roll 4d6 reroll 1s and 2s
```

    ## [1] "Rolls: [ 4 4 3 5 ]"

    ## [1] 16

``` r
roll('4d6r<2') # same as above
```

    ## [1] "Rolls: [ 3 *6* 4 3 ]"

    ## [1] 16

``` r
roll('4d6r1ro2') # roll 4d6 reroll 1s but reroll 2s only once
```

    ## [1] "Rolls: [ *6* 2 2 3 ]"

    ## [1] 13

``` r
r('1d6') # shortcut function
```

    ## [1] "Rolls: [ 3 ]"

    ## [1] 3

``` r
r(r1d6) # non standard evaluation
```

    ## [1] "Rolls: [ *1* ]"

    ## [1] 1
