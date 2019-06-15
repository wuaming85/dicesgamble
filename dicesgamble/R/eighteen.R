################自己骰###########
#' @title
#' Calculate the score by rule
#' @description
#' After throwing a dice, we can calculate the score by rule.
#' If the score we get in first time is zero, we can throw one more time.If we throw the second time but the score still be zero, the score will be zero.
#'
#' @param dices The result get by sausage function(sampling), it must be the vector form.
#'
#' @return If the player doesn't play the second time, the function will return only score.
#' If the player play the second time, it will return both score and the dices' points we get in the sceond time with array, and the first position of the return is score and another is points.
#' @export
#'
#' @examples
#' ##No second times
#' dices=c(2,2,5,6)
#' play_eighteen_la(dices)
#' ##second times
#' dices=c(1,1,1,4)
#' play_eighteen_la(dices)
play_eighteen_la=function(dices){
  score=array(0)  #儲存分數
  data.frame(table(dices))->time  #紀錄次數
  time$dices=as.numeric(as.matrix(time$dices))
  #################################################
  if(any(time$Freq==3)|all(time$Freq==1)){
    dicesreplace=sample(c(1:6),4,replace = TRUE)
    data.frame(table(dicesreplace))->timer
    timer$dicesreplace=as.numeric(as.matrix(timer$dicesreplace))
    if(all(timer$Freq==1)|any(timer$Freq==3)){
      score=0
      return(c(score,dicesreplace))
    }
    if(any(timer$Freq==2)&any(timer$Freq==1)){
      score=sum(as.numeric(timer$dicesreplace[which(timer$Freq!=2)]))
      return(c(score,dicesreplace))
    }
    if(all(timer$Freq==2)){
      score=max(as.numeric(timer$dicesreplace))*2
      record=c(score,dicesreplace)
      return(record)
    }
    if(all(timer$Freq==4)){
      score=(timer$dicesreplace)^4
      if(all(timer$dicesreplace==1)){
        score=15
      }
    }
  }
##########################################################
  if(all(time$Freq==2)){
    score=max(as.numeric(time$dices))*2
  }
  if(any(time$Freq==2)&any(time$Freq==1)){
    score=sum(as.numeric(time$dices[which(time$Freq!=2)]))
  }
  if(all(time$Freq==4)){
    score=(time$dices)^4
    if(all(time$dices==1)){
      score=15
    }
  }
  return(score)
}


###########跟香腸攤老闆玩########
###d=1~10###
#' @title
#' Compete with Sausage vendor
#' @description
#' Now you compete with Sausage vendor(Computer)!!
#' Enter the score and challenge with different difficulty!!
#'
#' @param scoreI Interger, the score you get in play_eighteen_la function
#' @param d difficulty, which range is 1~10
#'
#' @return It will tell you are win or lose or tie, and tell you the vendor's score and dices points.
#' @export
#'
#' @examples
#' sausage(throw)->dice
#' play_eighteen_la(dice)->score
#' eighteen_la(score[1],1)
eighteen_la=function(scoreI,d){
mark=0
probability=c(18-d,15-d,12-d,9+d,6+d,3+d)/63
score=array(0,dim=c(1,1)) #儲存分數
dies=sample(c(1:6),4,replace = TRUE,prob=probability)   #擲骰子
data.frame(table(dies))->time  #紀錄次數
time$dies=as.numeric(as.matrix(time$dies))
#################再給一次機會####################
if(any(time$Freq==3)|all(time$Freq==1)){
mark=1
diesreplace=sample(c(1:6),4,replace = T)
data.frame(table(diesreplace))->timer
timer$diesreplace=as.numeric(as.matrix(timer$diesreplace))
if(all(timer$Freq==1)|any(timer$Freq==3)){
score=0
}
if(any(timer$Freq==2)&any(timer$Freq==1)){
score=sum(as.numeric(timer$diesreplace[which(timer$Freq!=2)]))
}
if(all(timer$Freq==2))
{
score=max(as.numeric(timer$diesreplace))*2
}
if(all(timer$Freq==4)){
  if(all(timer$diesreplace==1))
  {
    score=15
  }
  score=(timer$diesreplace)^4
}
}
##########################################################
if(all(time$Freq==2)){
score=max(as.numeric(time$dies))*2
}
if(any(time$Freq==2)&any(time$Freq==1)){
score=sum(as.numeric(time$dies[which(time$Freq!=2)]))
}
if(all(time$Freq==4)){
  score=(time$dies)^4
  if(all(time$dies==1)){
    score=15
  }
}
##############################
if(mark==1){
pointer=c("1st",dies,"2nd",diesreplace)
}
if(mark==0){
pointer=dies
}
##############################
if(scoreI > score){
return(cat("You win!\n","Your score：",scoreI,"\n","playerscore：",score,"\n",pointer, "\n"))
}
if(scoreI < score){
return(cat("You lose!\n","Your score：",scoreI,"\n","playerscore：",score,"\n",pointer, "\n"))
}
if(scoreI == score){
return(cat("Tie!\n","Your score：",scoreI,"\n","playerscore：", score,"\n",pointer,append = T,"\n"))
}
}

###########十八啦!!!#############
#' @title
#' Tranditional taiwanese game at sausage street vendor
#' @description
#' Function for tranditional taiwanese game which usually appears in sausage street vendor in Taiwan.
#' This function can throw a dice with four dice.
#' @param throw type everything can execute this function, also NULL.
#'
#' @return equivlent to the result of throwing four dices.
#' @export
#'
#' @examples
#' ##Both are equivalent
#' sausage(throw)
#' sample(c(1:6),4,replace = TRUE)
sausage=function(throw)
{
  x=sample(c(1:6),4,replace=TRUE)
  return(x)
}
#############################

