#' @export
insertRoll = function(){
    rstudioapi::insertText('r(r')
}

#' Roll a dice
#' @description Rolls the dice described as a string
#' @param dice character, a valid dice syntax
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
    signs = rollingRules$sign %>% paste0(.,1) %>% as.integer() %>% sign

    if(!returnRolls){
        numbers = rolls %>% unlist
        return(sum(signs * numbers))
    } else{
        numbers = rolls %>% purrr::map_dbl('result')
        rolls$result = sum(numbers*signs)


        names(rolls)[-length(rolls)] = rollingRules$rollingRules %>% purrr::map_chr('diceString')

        return(rolls)
    }

}

#' Roll a dice
#'
#' @description  Alias to \code{\link{roll}}
#' @export
r = roll

#' Get dice probabilities by rolling
#' @param dice character, a valid dice syntax
#' @param n how many times dice should be rolled
#' @export
diceStats = function(dice,n=1000){
    rolls = sapply(1:n,function(i){roll(dice,vocal = FALSE)})
    rolls %>% table %>% {./sum(.)} ->out
    nms = names(out)
    out %<>% as.numeric()
    names(out) = nms
    return(out)
}

#' Get dice probabilities
#' @param dice character, a valid dice syntax
#' @param explodeDepth if dice syntax include explosion, how many explosions should be calculated
#' @export
diceProb = function(dice, explodeDepth = 4){
    rollingRules = diceParser(dice)

    if(!rollingRules$fate){
        possibleDice = (1:rollingRules$diceSide)[!1:rollingRules$diceSide %in% rollingRules['reroll']]
    } else{
        stop('Fate dice is not supported')
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
    if(rollingRules$explode>0){
        explodedResults = resultProbs
        for(i in seq_len(explodeDepth)){
            addedProbs = explodedResults[length(explodedResults)]*resultProbs
            names(addedProbs) = as.numeric(names(resultProbs)) + max(as.numeric(names(explodedResults)))

            explodedResults = c(explodedResults[-length(explodedResults)],addedProbs)
        }
        resultProbs = explodedResults
    }

    return(resultProbs)
}

expectedValue = function(dice){
    probs = diceProb(dice)
    sum(as.integer(names(probs)) * probs)
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
