context('Dice check')

set.seed(1)

test_that('basic dice', {
    result = roll('100d10',vocal = FALSE)
    expect_gt(result,454)
    expect_lt(result,646)
})

test_that('keep drop', {
    result = sapply(1:1000,function(x){roll('4d6k3',vocal = FALSE)}) %>% mean
    expect_gt(result,11.5)
    expect_lt(result,13)


    result = sapply(1:1000,function(x){roll('4d6kl3',vocal = FALSE)}) %>% mean
    expect_gt(result,7)
    expect_lt(result,10.5)
})


test_that('add remove', {
    result = sapply(1:1000,function(x){roll('1d20+1',vocal = FALSE)}) %>% mean
    expect_gt(result,11)
    expect_lt(result,12)


    result = sapply(1:1000,function(x){roll('1d20-1',vocal = FALSE)}) %>% mean
    expect_gt(result,9)
    expect_lt(result,10)
})

test_that('reroll', {
    result = sapply(1:1000,function(x){roll('10d6r1r2r3r4r5',vocal = FALSE)}) %>% mean
    expect_equal(result,60)

    result = sapply(1:1000,function(x){roll('10d6r<5',vocal = FALSE)}) %>% mean
    expect_equal(result,60)

    result = sapply(1:1000,function(x){roll('10d6ro1',vocal = FALSE)}) %>% mean
    expect_gt(result,39)
    expect_lt(result,41)

})



test_that('swarm',{
    sapply(1:1000, function(x){
        sum(animate(19))
    }) %>% mean -> result
    expect_gt(result,33)
    expect_lt(result,35)
})
