######偵測類型########
#' @title  Type of Showhand game in dices
#' @description Showhand game is very popular game in casino, this function can classify the type of showhand with dices.We throw five dices and watch the type.
#' @param y  a result of throwing dices (sampling)
#' @details
#' the result is type of showhand, it appears 2~9.
#' 2 is High card ||
#' 3 is one pair ||
#' 4 is two pairs ||
#' 5 is Three of a kind ||
#' 6 is Straight ||
#' 7 is Full house ||
#' 8 is Four of a Kind ||
#' 9 is Five of a kind ||
#' @return type of showhand
#' @export
#'
#' @examples
#' ##########
#' show=sample(c(1:6),5,replace = TRUE)
#' show
#' showhand(show)
#'
showhand = function(y){
  diff.dices.num = length(unique(y)) #有幾個的點數
  max.freq.num = max(table(y)) #彙整點數並查看相同點數最多有幾顆
  seq=seq(min(y),max(y))
  as.numeric(seq)->seq
  if(diff.dices.num == 5){ #有五種不同點數就需要判斷是否為是否為順子
    if (length(seq)!=length(y)) res = 2 #無
    else res = 6 #順子
  }
  if (diff.dices.num == 4 ) res = 3 #有四種不同的點數必定為一對
  if (diff.dices.num == 3 & max.freq.num == 2) res = 4 #三種點數同點最多 2 張為兩對
  if (diff.dices.num == 3 & max.freq.num == 3) res = 5 #三種點數同點最多 3 張為三條
  if (diff.dices.num == 2 & max.freq.num == 3) res = 7 #二種點數同點最多 3 張為福祿
  if (diff.dices.num == 2 & max.freq.num == 4) res = 8 #二種點數同點最多 4 張為鐵隻
  if (diff.dices.num == 1 & max.freq.num == 5) res = 9 #一種點數同點最多 5 張為無敵
  return(res) #回傳數值 1 至 9 表示各種不同的牌型
}
#######轉換中文牌型########
#' @title Transform the showhand game card types into Chinese.
#' @description Because the creator is Taiwanese, so we change card types into Chinese.
#' @param x Number 1~9, if the input is not 1~9, it will appear NA.
#'
#' @return
#' The chinese type of showhand card type.
#' @export
#'
#' @examples
#' ############
#' tran(7)
#' #####appear NA#####
#' tran(20)
tran = function(x){
  as.character(factor(x, levels=1:9, labels=c("錯誤","落單","一對","二對","三條","順子",
                                              "葫蘆","鐵支","無敵")))
}
######多人遊戲結果#####
#' @title  Multiple players play Showhand with dices
#' @description We can put the result we get in function play_showhand into the function, and determine who is win!
#' @param result  The result we get in function play_showhand, which is data.frame type.
#'
#' @return The original data frame and tell you who is/are  win in the game! The sixth column is the dices type, it is chinese type.
#' @export
#'
#' @examples
#' #################
#' play_showhand(5)->play
#' showHand_com(play)
showHand_com = function(result){
  ################################################################
    ############同類型情況####################
    if(length(which(result$V6==max(result$V6)))>1){
    result_winer=result[which(result$V6==max(result$V6)),]
    ##########################################
    if(all(result_winer$V6 == 9)){ #無敵比大小
      winer = result_winer$id[which.max(result_winer$V1)]
      if(length(unique(result_winer$V1))==1) winer="Tie game"
    }
    if(all(result_winer$V6 == 8)){ #鐵枝比大小
      record=list()##紀錄次數
      ####排序####
      for(j in 1:nrow(result_winer)){
      result_winer[j,1:5]=sort(result_winer[j,1:5])
      }
      ####五個都一樣，則平手####
      if(nrow(unique(result_winer[,1:5]))==1)  winer = "Tie game"
      dup=array(0,nrow(result_winer))###重複4次的數###
      for(i in 1:nrow(result_winer)){
      record[[i]]=data.frame(table(as.numeric(result_winer[i,1:5])))
      record[[i]]$Var1=as.numeric(as.matrix(record[[i]]$Var1))
      dup[i]=record[[i]]$Var1[which(record[[i]]$Freq==4)]
      }
      winer = data.frame(result_winer)$id[which.max(dup)]
   ###############################################
      ####若重複的數一樣，比較單獨點####
      if(length(unique(dup))!=length(dup)&length(which(dup==max(dup)))!=1){
        winer = data.frame(result_winer)$id[which(dup==max(dup))]
        result_winer=result_winer[which(dup==max(dup)),]
        single=array(0,nrow(result_winer))
        record=list()
       for(i in 1:nrow(result_winer)){
         record[[i]]=data.frame(table(as.numeric(result_winer[i,1:5])))
         record[[i]]$Var1=as.numeric(as.matrix(record[[i]]$Var1))
       }
       for(j in 1:nrow(result_winer))
       {
        single[j]=record[[j]]$Var1[which(record[[i]]$Freq==1)]
       }
       winer = result_winer$id[which(single==max(single))]
       if(length(unique(single))<length(single)) winer = data.frame(result_winer)$id[which(single==max(single))]
      }
   #################一般比較########################
    }
    if(all(result_winer$V6 == 7)){ #福祿比大小
      record=list()##紀錄次數
      dup=array(0,nrow(result_winer))###重複3次的數###
      ####排序#####
      for(j in 1:nrow(result_winer)){
        result_winer[j,1:5]=sort(result_winer[j,c(1:5)])
      }
      if(nrow(unique(result_winer[,1:5]))==1) winer="Tie game"
      for(i in 1:nrow(result_winer)){
      record[[i]] = as.data.frame((table(as.numeric(result_winer[i,1:5]))))
      record[[i]]$Var1=as.numeric(as.matrix(record[[i]]$Var1))
      dup[i]=record[[i]]$Var1[which(record[[i]]$Freq==3)]
      }
      winer=result_winer$id[which.max(dup)]
      #####################################################
       if(length(unique(dup))!=length(dup)&length(which(dup==max(dup)))!=1){ #三點數一樣 比較兩點數
         winer = data.frame(result_winer)$id[which(dup==max(dup))]
         result_winer=result_winer[which(dup==max(dup)),]##留下最大
        single=array(0,nrow(result_winer))
        record=list()
        ######更新##
        for(i in 1:nrow(result_winer)){
          record[[i]] = as.data.frame((table(as.numeric(result_winer[i,1:5]))))
          record[[i]]$Var1=as.numeric(as.matrix(record[[i]]$Var1))
          dup[i]=record[[i]]$Var1[which(record[[i]]$Freq==3)]
        }
        for(j in 1:nrow(result_winer)){
          single[j]=record[[j]]$Var1[which(record[[j]]$Freq==2)]
        }
        if(length(unique(single))<length(single)) winer = data.frame(result_winer)$id[which(single==max(single))]
        winer = result_winer$id[which.max(single)]
      }
    }
    if(all(result_winer$V6 == 6)){ #順子比大小
      for(i in 1:nrow(result_winer)){
        result_winer[i,1:5]  = sort(result_winer[i,1:5])
      }
      winer = result_winer$id[which.max(result_winer$V5)]
      if(length(unique(result_winer[,5]))==1) winer="Tie game"
    }
    if(all(result_winer$V6 == 5)){ #三條比大小
      record=list()##紀錄次數
      for(i in 1:nrow(result_winer)){
       result_winer[i,1:5]=sort(result_winer[i,1:5])
      }
      if(nrow(unique(result_winer[,1:5]))==1) winer="Tie game"
      dup=array(0,nrow(result_winer))###重複3次的數###
      for(i in 1:nrow(result_winer)){
      record[[i]]=data.frame(table(as.numeric(result_winer[i,1:5])))
      record[[i]]$Var1=as.numeric(as.matrix(record[[i]]$Var1))
      dup[i]=record[[i]]$Var1[which(record[[i]]$Freq==3)]
      }
      winer = result_winer$id[which.max(dup)]
      ####若重複的數一樣，比較單獨點總和####
      if(length(unique(dup))!=length(dup)&length(which(dup==max(dup)))>1){
        winer = result_winer$id[which(dup==max(dup))]
        result_winer=result_winer[which(dup==max(dup)),]
        single=array(0,nrow(result_winer))
        record=list()
        dup=array(0,nrow(result_winer))
        for(i in 1:nrow(result_winer)){
          record[[i]]=data.frame(table(as.numeric(result_winer[i,1:5])))
          record[[i]]$Var1=as.numeric(as.matrix(record[[i]]$Var1))
          dup[i]=record[[i]]$Var1[which(record[[i]]$Freq==3)]
        }
        for(j in 1:nrow(result_winer))
        {
          single[j]=sum(record[[j]]$Var1[which(record[[i]]$Freq==1)])
        }
        winer = result_winer$id[which.max(single)]
        if(length(unique(single))<length(single)) winer=data.frame(result_winer)$id[which(single==max(single))]
      }
      }
    if(all(result_winer$V6 == 4)){ #兩對比大小

      record=list()

      for(i in 1:nrow(result_winer)){
      result_winer[i,c(1:5)] = sort(result_winer[i,c(1:5)])
      }
      if(nrow(unique(result_winer[,c(1:5)]))==1) winer="Tie game"
      dup=array(0,dim=c(nrow(result_winer),2))
      maxdup=array(0,nrow(result_winer))

      for(i in 1:nrow(result_winer)){
        record[[i]]=data.frame(table(as.numeric(result_winer[i,c(1:5)])))
        record[[i]]$Var1=as.numeric(as.matrix(record[[i]]$Var1))
        dup[i,]=record[[i]]$Var1[which(record[[i]]$Freq==2)]
        dup[i,]=sort(dup[i,])
        maxdup[i]=dup[i,2]
      }
      winer = result_winer$id[which.max(maxdup)]
      if(length(unique(maxdup))==length(maxdup)){
        winer = result_winer$id[which.max(maxdup)]
      }
      #最大的不只一個
      if(nrow(unique(dup))!=nrow(dup)){
        winer = result_winer$id[which(maxdup==max(maxdup))]
      #大家的對子都一樣
        if(nrow(unique(dup))==1){
          single=array(0,nrow(result_winer))
          for(i in 1:nrow(result_winer)){
          single[i]=record[[i]]$Var1[which(record[[i]]$Freq==1)]
          }
          winer=result_winer$id[which.max(single)]
          if(length(unique(single))==1){
            winer=data.frame(result_winer)$id[which(single==max(single))]}
        }
        }
      #最大對子一樣
        if(length(which(maxdup==max(maxdup)))!=1 & nrow(unique(dup))!=1){
          winer=result_winer$id[which(maxdup==max(maxdup))]
          result_winer1=result_winer[which(maxdup==max(maxdup)),]
          dup1=array(0,dim=c(nrow(result_winer1),2))
          maxdup1=array(0,nrow(result_winer1))
          record=list()
          for(i in 1:nrow(result_winer1)){
            record[[i]]=data.frame(table(as.numeric(result_winer1[i,c(1:5)])))
            record[[i]]$Var1=as.numeric(as.matrix(record[[i]]$Var1))
            dup1[i,]=record[[i]]$Var1[which(record[[i]]$Freq==2)]
            dup1[i,]=sort(dup1[i,])
            maxdup1[i]=dup1[i,2]
          }
          winer=data.frame(result_winer1)$id[which(dup1[,1]==max(dup1[,1]))]
          if(nrow(unique(dup1))==1){
            single=array(0,nrow(result_winer1))
            for(i in 1:nrow(result_winer1)){
              single[i]=record[[i]]$Var1[which(record[[i]]$Freq==1)]
            }
            winer=result_winer1$id[which.max(single)]
            if(length(unique(single))==1){
            winer=result_winer1$id[which(single==max(single))]
            }
            }
        }
      }
    if(all(result_winer$V6 == 3)){ #一對比大小
      record=list()##紀錄次數
      for(i in 1:nrow(result_winer)){
        result_winer[i,1:5]=sort(result_winer[i,1:5])
      }
      if(nrow(unique(result_winer[,c(1:5)]))==1) winer="Tie game"
      dup=array(0,nrow(result_winer))###重複2次的數###
      for(i in 1:nrow(result_winer)){
        record[[i]]=data.frame(table(as.numeric(result_winer[i,c(1:5)])))
        record[[i]]$Var1=as.numeric(as.matrix(record[[i]]$Var1))
        dup[i]=record[[i]]$Var1[which(record[[i]]$Freq==2)]
      }
      winer = result_winer$id[which.max(dup)]
      ####若重複的數一樣，比較單獨點總和####
      if(length(unique(dup))!=length(dup) & length(which(dup==max(dup)))!=1){
        winer = result_winer$id[which(dup==max(dup))]
        result_winer=result_winer[which(dup==max(dup)),]
        record=list()
        single=array(0,nrow(result_winer))
        for(i in 1:nrow(result_winer)){
          record[[i]]=data.frame(table(as.numeric(result_winer[i,c(1:5)])))
          record[[i]]$Var1=as.numeric(as.matrix(record[[i]]$Var1))
        }
        for(j in 1:nrow(result_winer))
        {
          single[j]=sum(record[[j]]$Var1[which(record[[i]]$Freq==1)])
        }
        winer = result_winer$id[which.max(single)]
        if(length(unique(single))<length(single))
          winer = result_winer$id[which(single==max(single))]
      }

    }
    if(all(result_winer$V6 == 2)){ #無比大小
      for(i in 1:nrow(result_winer)){
      result_winer[i,1:5]=sort(result_winer[i,1:5])
      }
      if(nrow(unique(result_winer))==1) winer="Tie game"
      winer=result_winer$id[which.max(result_winer[,5])]
      ###########如果最大一樣 比較剩下的加起來比大小######
      if(length(unique(result_winer[,5]))!=1)
      {
        result_winer=result_winer[which(result_winer[,5]==max(result_winer[,5])),]
        for(i in 1:nrow(result_winer)){
        remain[i]=sum(result_winer[i,1:4])
        }
        winer = result_winer$id[which.max(remain)]
        if(length(unique(remain))>1) winer=data.frame(result_winer)$id[which(remain==max(remain))]
      }

    }
    }

    if(length(which(result$V6==max(result$V6)))==1)
    {
      result$id[which.max(result$V6)]->winer
    }

     result$win="lose"

     if(winer=="Tie game"){

      winer = which(result$V6==max(result$V6))

      cat("\n","The winner are player number",winer,"\n","\n")

      result$win[which(result$V6==max(result$V6))]="win"
      ###################################################
      trann = NA
      for(i in 1:nrow(result)){
        trann[i] = tran(as.data.frame(result$V6)[i,1])
      }
      trann = as.data.frame(trann)
      result$V6 = trann
      result = as.matrix(result)
      colnames(result)=c("d1","d2","d3","d4","d5","牌型","id","輸贏")
      result=as.data.frame(result)
      return(result)
    }
     ########################################################
     result$win[winer]="win"
     cat("\n","The winner is player number",winer,"\n","\n")
####################################################
     trann = NA
     for(i in 1:nrow(result)){
       trann[i] = tran(as.data.frame(result$V6)[i,1])
     }
     trann = as.data.frame(trann)
     result$V6 = trann
     result = as.matrix(result)
     colnames(result)=c("d1","d2","d3","d4","d5","牌型","id","輸贏")
     result=as.data.frame(result)
 ##############################################################
     return(result)
}
######梭哈######
#' @title Play showhand with multiple persons
#' @description
#' In this function, you can throw it with many times, and it will tell you the result.
#' @param people person you want to choose.
#' @details
#' the result type(V6) of showhand, it appears 2~9
#' 2 is High card
#' 3 is one pair
#' 4 is two pairs
#' 5 is Three of a kind
#' 6 is Straight
#' 7 is Full house
#' 8 is Four of a Kind
#' 9 is Five of a kind
#' @return The result is data frame, and V6 is the cards type.
#' @export
#'
#' @examples
#' ###############
#' play_showhand(10)
#' ##############You can also see who wins the game######
#' play_showhand(10)->result
#' showHand_com(result)
#'
play_showhand=function(people){
y = list()
score = array(0,people)
result = list()
ids=c(1:people)
for(i in 1:people){
y[[i]] = as.numeric(sample(1:6, 5, replace = TRUE))
score[i] = showhand(y[[i]])
result[[i]] = c(y[[i]],score[i])
}
as.data.frame(do.call(rbind, result)) -> result
result$id=ids

return(result)
}











