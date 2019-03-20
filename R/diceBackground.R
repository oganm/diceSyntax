#' @export
compositeDiceParser = function(dice){
    diceComponents = strsplit(dice,'\\+|-') %>% {.[[1]]}
    componentSigns = c('+',stringr::str_extract_all(dice,'\\+|-') %>% {.[[1]]})
    rollingRules = diceComponents %>% lapply(diceParser)
    return(list(rollingRules = rollingRules,
                signs = componentSigns))
}


#' @export
diceParser = function(dice){
    dice %<>% stringr::str_replace('^r|R','')


    rollingRules = list()
    validTokens = "[dkscrf+\\-!DKSCRF]"
    dice %<>% tolower  %>% gsub(pattern = '\\s',replacement = '',x = .)
    rollingRules$diceString = dice
    justInteger = suppressWarnings(as.integer(dice))
    if(!is.na(justInteger)){
        rollingRules$add = justInteger
        rollingRules$diceCount = 0
        rollingRules$diceSide = 0
        rollingRules$fate = FALSE
        rollingRules$sort = FALSE
        rollingRules$explode = 0
        return(rollingRules)
    }

    rollingRules$diceCount = stringr::str_extract(string = dice,pattern =  '^[0-9]+?(?=d)') %>% as.integer()
    if(is.na(rollingRules$diceCount) & grepl('^d',dice)){
        rollingRules$diceCount = 1
        dice = paste0('1',dice)
    }
    otherTokens =  stringr::str_extract_all(string = dice,
                                            pattern =  paste0(validTokens,'.*?((?=',validTokens, ')|$)')) %>% unlist


    rollingRules$diceSide = stringr::str_extract(string = otherTokens[1],pattern =  '(?<=d)[0-9f]*')
    if( rollingRules$diceSide == '' &  otherTokens[2] == 'f'){
        rollingRules$diceSide = 3
        rollingRules$fate = TRUE
        otherTokens = otherTokens[-1]
    } else{
        rollingRules$fate = FALSE
    }
    rollingRules$diceSide %<>% as.integer()

    if(rollingRules$diceSide == ''){
        stop('First parameter has to be dice side (eg. "1d6")')
    }

    otherTokens = otherTokens[-1]

    # sort the dice if s token is added ------------
    if('s' %in%  otherTokens){
        rollingRules$sort = TRUE
    } else{
        rollingRules$sort = FALSE
    }

    # drop dice if rules are given --------------
    dropRules = otherTokens %>% {.[grep(pattern = 'd|k',.)]}
    if(length(dropRules)>1){
        stop('Conflicting keep options given')
    } else if(length(dropRules)==0){
        rollingRules$dropDice = NULL
        rollingRules$dropLowest = TRUE # default configuration
    } else{
        dropNo = stringr::str_extract(string = dropRules,pattern =  '[0-9]+') %>% as.integer
        if(length(dropNo)==0){
            stop('Keep options require number of dice to keep or drop (eg. 10d6k3 10d6d3)')
        }
        rollingRules$dropDice = switch(substr(dropRules,1,1),
                                       d =  dropNo,
                                       k =  rollingRules$diceCount-dropNo)
        rollingRules$dropLowest = !(grepl(pattern = 'dh',dropRules) | grepl(pattern ='kl', dropRules))
    }

    # additon or substraction -----------------
    aditionRules = otherTokens %>% {.[grep(pattern = '\\+|-',.)]} %>% as.integer()
    if(any(is.na(aditionRules))){
        stop('"-" and "+" should always be followed by integers')
    }
    if(length(aditionRules)!=0){
        rollingRules$add = sum(aditionRules)
    } else{
        rollingRules$add = 0
    }


    # reroll ---------------------
    rerollDetermine = function(x){
        number=  stringr::str_extract(x,'[0-9]*$')
        if(grepl('<|>',x) & number ==''){
            stop('Rerolling with "<" or ">" identifiers requires an integer')
        } else if(grepl('<',x) & grepl('>',x)){
            stop('Single rerolling clause can only have one of "<" or ">"')
        } else if(!grepl('<|>',x) & number ==''){
            reroll = 1
            if (rollingRules$fate){
                reroll = -1
            }
        } else if(!grepl('<|>',x) & number!=''){
            reroll = number %>% as.integer()
        } else if (grepl('<',x)){
            reroll = 1:number
            if(rollingRules$fate){
                reroll = -1:number
            }
        } else if(grepl('>',x)){
            reroll = number:rollingRules$diceSide
            if(rollingRules$fate){
                reroll = number:1
            }
        }
        return(reroll)
    }

    rerollRules = otherTokens %>% {.[grep(pattern = 'r(?!o)',.,perl=TRUE)]}

    reroll = rerollRules %>% lapply(rerollDetermine) %>% unlist
    dicePossibilities = 1:rollingRules$diceSide
    if(rollingRules$fate){
        dicePossibilities = -1:1
    }
    if(all(dicePossibilities %in% reroll)){
        stop('You cannot reroll every possible result')
    }
    rollingRules$reroll = reroll


    rerollOnceRules = otherTokens %>% {.[grep(pattern = 'ro',.,perl=TRUE)]}
    rerollOnce = rerollOnceRules %>% lapply(rerollDetermine) %>% unlist
    if(length(intersect(reroll,rerollOnce))>0){
        warning('Why reroll something once and forever?')
    }
    rollingRules$rerollOnce = rerollOnce


    explodeRules = sum(otherTokens %in% '!')
    if(explodeRules>2){
        stop('! means exploding dice, !! means compounding exploding dice. !!! means nothing!')
    }
    rollingRules$explode = explodeRules

    return(rollingRules)
}



#' @export
rollParam =  function(diceCount,
                      diceSide = NULL,
                      fate = FALSE,
                      sort = FALSE,
                      dropDice = NULL,
                      dropLowest = TRUE,
                      add = 0,
                      reroll = c(),
                      rerollOnce = c(),
                      explode = 0,
                      diceString = '',
                      critMark = TRUE,
                      vocal=TRUE,
                      returnRolls = FALSE){
    resample <- function(x, ...) x[sample.int(length(x), ...)]

    if(!fate){
        dice = resample((1:diceSide)[!1:diceSide %in% reroll],diceCount,replace=TRUE)
        possibleDice = (1:diceSide)[!1:diceSide %in% reroll]
        minValue = min((1:diceSide)[!1:diceSide %in% reroll])
        maxValue = diceSide
    } else{
        dice = resample((-1:1)[!-1:1 %in% reroll],diceCount,replace=TRUE)
        minValue = NA
        maxValue = NA
    }

    if(length(rerollOnce)>0){
        dice[dice %in% rerollOnce] = resample((1:diceSide)[!1:diceSide %in% reroll],sum(dice %in% rerollOnce),replace=TRUE)
    }

    if(explode>0){
        dice = lapply(dice, function(x){
            manyRolls = c()
            while(x == max(possibleDice)){
                manyRolls = c(manyRolls,x)
                x = resample(possibleDice,1)
            }
            x = c(manyRolls,x)
            if(explode == 2){
                x = sum(x)
            }
            return(x)
        }) %>% unlist
    }

    if(!is.null(dropDice)){
        drop = dice[order(dice,decreasing = !dropLowest)[1:dropDice] %>% sort]
        dice =  dice[-order(dice,decreasing = !dropLowest)[1:dropDice] %>% sort]
    }

    if(sort){
        dice = sort(dice)
        if(!is.null(dropDice)){
            drop = sort(drop)
        }
    }
    result = sum(dice) + add
    if(vocal & diceCount>0){
        dicePrint = dice
        dropPrint = drop
        if(critMark){
            crits = dice %in% c(minValue,maxValue)
            dicePrint[crits] = glue::glue('*{dice[crits]}*')
        }
        print(paste0('Rolls: [ ',paste(dicePrint,collapse=' '),' ] (',diceString,')'))
        if(!is.null(dropDice)){
            if(critMark){
                crits = drop %in% c(minValue,maxValue)
                dropPrint[crits] = glue::glue('*{drop[crits]}*')
            }
            print(paste('Dropped: [',paste(dropPrint,collapse=' '),']'))
        }
    }
    if(!returnRolls){
        return(result)
    } else{
        if(is.null(dropDice)){
            drop = NULL
        }
        return(list(result = result, dice = dice, drop = drop))
    }
}


