#' @export
insertRoll = function(){
    rstudioapi::insertText('r(r')
}

#' Roll a dice
#' @description Rolls the dice described as a string
#' @param dice character. If a variable name, the variable must not be a valid dice syntax that starts with an r or the function will just roll that dice instead (eg. r4d6). description of the dice to be rolled. 4d6 rolls four six sided dice. 4d6+3 adds 3 to the result. 4d6k3 keeps the highest 3 dice. 4d6d1 drops the lowest one dice. 4d6kl3 keeps the lowest 3 dice. 4d6dh1 drops the highest 1 dice. 4d6r1 rerolls all 1s. 4d6ro1 rerolls 1s once. 4df rolls fate dice.
#' @param vocal Should it print individual rolls
#' @param returnRolls Logical. If true a list will be returned that includes rolled and dropped dice as well as the sum of accepted dice
#' @export
roll = function(dice, critMark = TRUE,vocal=TRUE,returnRolls = FALSE){

    rollingRules = compositeDiceParser(dice)

    rollingRules$rollingRules %>% lapply(function(rules){
        rollParam(rules$diceCount,
                  rules$diceSide,
                  rules$fate,
                  rules$sort,
                  rules$dropDice,
                  rules$dropLowest,
                  rules$add,
                  rules[['reroll']],
                  rules$rerollOnce,
                  rules$explode,
                  rules$diceString,
                  critMark,
                  vocal,
                  returnRolls)
    }) -> rolls

    if(!returnRolls){
        return(paste0(rollingRules$signs,rolls) %>% as.integer %>% sum)
    } else{
        names(rolls) = rollingRules$rollingRules %>% purrr::map_chr('diceString')
        result = rolls %>% purrr::map_dbl('result') %>% paste0(rollingRules$signs,.) %>% as.integer() %>% sum
        rolls$result = result
        return(rolls)
    }

}

#' Roll a dice
#'
#' @description  Alias to \code{\link{roll}}
#' @export
r = roll


#' @export
diceStats = function(dice,n=1000){
    rolls = sapply(1:n,function(i){roll(dice,vocal = FALSE)})
    plot = data.frame(rolls = rolls) %>%
        ggplot2::ggplot(ggplot2::aes(x = rolls)) + cowplot::theme_cowplot() +
        ggplot2::geom_density(fill = 'grey')
    mean = mean(rolls)
    return(list(mean,plot))
}

#' @export
diceProb = function(dice){
    rollingRules = diceParser(dice)

    if(!rollingRules$fate){
        possibleDice = (1:rollingRules$diceSide)[!1:rollingRules$diceSide %in% rollingRules$reroll]
    } else{
        possibleDice  = (-1:1)[!-1:1 %in% rollingRules$reroll]
    }

    baseProb = 1/length(possibleDice)

    # matrix has no reason to be here. it may in the future with exploding though..

    diceProbs = matrix(1/length(possibleDice),
                       nrow = length(possibleDice),
                       ncol = rollingRules$diceCount)
    row.names(diceProbs) = possibleDice

    if(length(rollingRules$rerollOnce)>0){
        diceProbsToAdd = matrix(0,
                                nrow = length(possibleDice),
                                ncol = rollingRules$diceCount)
        for (x in rollingRules$rerollOnce[rollingRules$rerollOnce %in% possibleDice]){
            diceProbs[x %>% as.character(),] = 0
            diceProbsToAdd = diceProbsToAdd + baseProb^2
        }
        diceProbs = diceProbs + diceProbsToAdd
    }

    allPossibs = expand.grid(rep(list(rownames(diceProbs) %>% as.integer()),ncol(diceProbs)))

    probabilities = apply(allPossibs,1,function(x){
        sapply(seq(ncol(diceProbs)),function(i){
            diceProbs[x[i] %>% unlist %>% as.character,i]
        }) %>% prod
    })

    possibSums = allPossibs %>% apply(1,function(x){
        dropDice = rollingRules$dropDice
        dropLowest = rollingRules$dropLowest
        if(!is.null(dropDice)){
            drop = x[order(x,decreasing = !dropLowest)[1:dropDice] %>% sort]
            x =  x[-order(x,decreasing = !dropLowest)[1:dropDice] %>% sort]
        }
        sum(x)
    })

    possibleResults = unique(possibSums)

    resultProbs = possibleResults %>% sapply(function(x){
        sum(probabilities[possibSums %in% x])
    })


    names(resultProbs) = possibleResults
    return(resultProbs)
}

#' Simulate swarm attack
#'
#' @description  simulates attack by a swarm to an opponent of known AC
#'
#' @param AC AC of the opponent
#' @param count number of individuals in the swarm
#' @param damageDice damage dice to roll
#' @param attackBonus bonus to hit
#' @param damageBonus bonus to damage
#' @param advantage N = normal, A = with advantage, D = with disadvantage.
#' @export
swarm = function(AC, count, damageDice, attackBonus = 0, damageBonus = 0, advantage = c('N','D','A')){
    advantage = match.arg(advantage)

    rolls = rollParam(count,diceSide = 20,returnRolls = TRUE,vocal = FALSE)$dice
    if(advantage != 'N'){
        rerolls = rollParam(count,diceSide = 20,returnRolls = TRUE, vocal = FALSE)$dice
        if(advantage == 'A'){
            rolls = cbind(rolls,rerolls) %>% apply(1,max)
        } else if(advantage =='D'){
            rolls = cbind(rolls,rerolls) %>% apply(1,min)
        }
    }
    rolls = rolls + attackBonus

    hits = rolls[(rolls >= AC & (rolls-attackBonus)!=1)|((rolls-attackBonus)==20)]

    if(length(hits) == 0){
        return(numeric(0))
    }

    sapply(hits,function(x){
        if ((x-attackBonus) == 20){
            rollingRules = compositeDiceParser(damageDice)
            rollingRules$rollingRules %>% lapply(function(rules){
                rollParam(rules$diceCount*2,
                          rules$diceSide,
                          rules$fate,
                          rules$sort,
                          rules$dropDice,
                          rules$dropLowest,
                          rules$add,
                          rules[['reroll']],
                          rules$rerollOnce,
                          rules$explode,
                          rules$diceString,
                          critMark=FALSE,
                          vocal= FALSE,
                          returnRolls = FALSE)
                }) -> rolls
            paste0(rollingRules$signs,rolls) %>% as.integer %>% sum %>% {.+damageBonus}
        } else{
            roll(damageDice,vocal = FALSE) + damageBonus
        }
    })
}

# AC: armor class
# bonus: attack bonus of the group
# count: number of entities in the group
# advantage: N = normal. A = with advantage, D = with disadvantage
# dice = number and type of die
# damageBonus = bonus to damage
# default settings are for animating 10 tiny objects but should work with any mob of identical creatures
#' @export
animate = function(AC, count = 10, damageDice = '1d4+4', attackBonus = 8, damageBonus = 0, advantage = c('N','D','A')){
    swarm(AC, count, damageDice, attackBonus, damageBonus,advantage)
}
