
# L.Point Big Data Competition #  

### data ### ------------------------------------------------------------------------------

product = read.csv("C:/Users/jeeyeon/Desktop/data/lpoint1.csv")
product = product[product$HITS_SEQ>1,]

search_raw1 =  read.csv("C:/Users/jeeyeon/Desktop/data/lpoint2.csv")
search_raw2 =  read.csv("C:/Users/jeeyeon/Desktop/data/lpoint3.csv")
search1 = read.csv("C:/Users/jeeyeon/Desktop/data/search1_final.csv", fileEncoding = "utf8")
search1 = search1 %>% mutate(search1 = mySEARCH1)
search2 = read.csv("C:/Users/jeeyeon/Desktop/data/search2_final.csv", fileEncoding = "utf8")
search2 = search2 %>% mutate(search2 = mySEARCH2)

custom = read.csv("C:/Users/jeeyeon/Desktop/data/lpoint4.csv")
session =read.csv("C:/Users/jeeyeon/Desktop/data/lpoint5.csv")
session = session[complete.cases(session),] 
master =read.csv("C:/Users/jeeyeon/Desktop/data/lpoint6.csv")
mySALE_PD=read.csv("C:/Users/jeeyeon/Desktop/data/mySALE_PD.csv", header=F)
hit =read.csv("C:/Users/jeeyeon/Desktop/data/hit.csv")
hit = hit %>% mutate(hit = myHIT2)

master$CLAC1_NM = as.character(master$CLAC1_NM)
levels(master$CLAC1_NM) = c(levels(master$CLAC1_NM), "유아동가구")
levels(master$CLAC1_NM) = c(levels(master$CLAC1_NM), "유아동속옷/양말/홈웨어")
levels(master$CLAC1_NM) = c(levels(master$CLAC1_NM), "유아동스포츠패션")
# levels(master$CLAC1_NM) = c(levels(master$CLAC1_NM), "포장반찬")

newname1<-c("유아동속옷","유아동양말류","유아동화")  # 유아동속옷/양말/홈웨어
newname2<-c("유아동스포츠화","유아동일반스포츠의류") # 유아동스포츠패션
newname3<-c("유아동가구")                            # 유아동가구

for (i in 1:nrow(master)){
  if(master[i,"CLAC2_NM"] %in% newname1) {
    master[i,"CLAC1_NM"]<-"유아동속옷/양말/홈웨어"
  } else if (master[i,"CLAC2_NM"] %in% newname2){
    master[i,"CLAC1_NM"]<-"유아동스포츠패션" }
  else if (master[i,"CLAC2_NM"] %in% newname3){ 
    master[i,"CLAC1_NM"]<-"유아동가구" }
  else if (master[i,"CLAC2_NM"] == "포장반찬"){
    master[i,"CLAC1_NM"]<-"포장반찬" }}

# master CLAC1_NM 10개로 나누기
c1<-c('남성의류','여성의류','속옷/양말/홈웨어','스포츠패션','패션잡화')
c2<-c('건강식품','과일','냉동식품','음료','축산물', '포장반찬')   # 새로운 포장반찬 항목 추가 
c3<-c('식기/조리기구', '세제/위생', '인테리어/조명', '자동차용품','주방잡화', '청소/세탁/욕실용품', '침구/수예', '퍼스널케어')
c4<-c('가구')
c5<-c('계절가전', '냉장/세탁가전', '생활/주방가전', '영상/음향가전','컴퓨터', '모바일')
c6<-c('화장품/뷰티케어')
c7<-c('유아동의류', '출산/육아용품', '완구', '유아동가구','유아동속옷/양말/홈웨어','유아동스포츠패션')     # 새로운 유아동가구 항목 추가 
c8<-c('구기/필드스포츠', '시즌스포츠', '아웃도어/레저', '헬스/피트니스')
c9<-c('문구/사무용품', '상품권')
c10<-c('애완용품')

master =master %>% mutate('myCLAC_NM_name'=ifelse(CLAC1_NM %in% c1,"1_의류/패션",
                                                  ifelse(CLAC1_NM %in% c2,"2_식품/농수산물",
                                                         ifelse(CLAC1_NM %in% c3,"3_생활/자동차용품",
                                                                ifelse(CLAC1_NM %in% c4,"4_가구",
                                                                       ifelse(CLAC1_NM %in% c5,"5_가전/컴퓨터/통신기기",
                                                                              ifelse(CLAC1_NM %in% c6,"6_화장품",
                                                                                     ifelse(CLAC1_NM %in% c7,"7_아동/유아용품",
                                                                                            ifelse(CLAC1_NM %in% c8,"8_스포츠/레저용품",
                                                                                                   ifelse(CLAC1_NM %in% c9,"9_사무/문구","10_애완용품"))))))))))
master =master %>% mutate('myCLAC_NM'=ifelse(CLAC1_NM %in% c1,1,
                                             ifelse(CLAC1_NM %in% c2,2,
                                                    ifelse(CLAC1_NM %in% c3,3,
                                                           ifelse(CLAC1_NM %in% c4,4,
                                                                  ifelse(CLAC1_NM %in% c5,5,
                                                                         ifelse(CLAC1_NM %in% c6,6,
                                                                                ifelse(CLAC1_NM %in% c7,7,
                                                                                       ifelse(CLAC1_NM %in% c8,8,
                                                                                              ifelse(CLAC1_NM %in% c9,9,10))))))))))
master["mySALE_PD"] = mySALE_PD[,2]

final = left_join(product,custom, by = "CLNT_ID")
final = right_join(final,session, by = c("CLNT_ID", "SESS_ID"))
final = left_join(final, master,by = "PD_C")
final$myCLAC_NM = as.factor(final$myCLAC_NM)
final$CLNT_AGE = as.factor(final$CLNT_AGE)
final$myCLNT_AGE = final$CLNT_AGE

final_clean = final[complete.cases(final),]

# 파생변수 
final = final %>% mutate(SESS_DT = as.Date(SESS_DT),
                         myMONTH = month(SESS_DT),                                      # 월단위 
                         myWEEK = cut(SESS_DT, breaks = "week", start.on.monday = F),   # 주단위
                         myWEEKDAY = wday(SESS_DT),                                     # 요일단위 
                         myEACH_SESS_HR_V = TOT_SESS_HR_V / TOT_PAG_VIEW_CT,            # 한 페이지 평균 뷰시간 
                         myPD_BUY_TOT = PD_BUY_AM * PD_BUY_CT,                          # 총액 
                         myCLNT_AGE = ifelse(CLNT_AGE %in% c(60,70,80),'60+',CLNT_AGE)) # 60세 이상 나이 하나로 묶기 

### library  -------------------------------------------------------------------------------------------------------

library(ggplot2)
library(plyr)
library(dplyr)
library(tidyverse)
library(lubridate)
library(psych)
library(GPArotation)
library(corrplot)
library(psych)
options("scipen" = 100)

################################### FINAL : My Table ################################################

  input = final
  
  ## 총구매액 -> total table 
    total = input %>% group_by(myCLAC_NM,CLAC2_NM) %>% summarize(total = sum(myPD_BUY_TOT,na.rm =T))
  ## 고객수 -> client table
    client = input %>% group_by(CLNT_ID,myCLAC_NM,CLAC2_NM) %>% count()
    client = client %>% group_by(myCLAC_NM,CLAC2_NM) %>% count(); client$client = client$nn
  ## 세일 -> sale table 
    sale = input %>% group_by(myCLAC_NM,CLAC2_NM) %>% summarize(sale = sum(mySALE_PD, na.rm =T)) 
  ## 재구매율 -> rebuy table
    rebuy = input %>% distinct(CLNT_ID,SESS_ID,PD_C) %>% group_by(CLNT_ID,PD_C) %>% count() %>% filter(n>1)
    rebuy = rebuy %>% mutate(real_n = n-1) %>% left_join( master, by = "PD_C")
    rebuy = rebuy %>% group_by(myCLAC_NM,CLAC2_NM) %>% summarize(rebuy = sum( real_n ,na.rm =T))
    rebuy$myCLAC_NM = as.factor(rebuy$myCLAC_NM) 
  ## 구매건수 -> mytable table 
    mytable = input %>% group_by(myCLAC_NM,CLAC2_NM) %>% count()
    mytable = left_join(mytable,total, by = c("myCLAC_NM", "CLAC2_NM"))
    mytable = left_join(mytable,client, by = c("myCLAC_NM", "CLAC2_NM"))
    mytable = left_join(mytable,sale, by = c("myCLAC_NM", "CLAC2_NM"))
    mytable = left_join(mytable,rebuy, by = c("myCLAC_NM", "CLAC2_NM"))
    mytable = mytable %>% select(myCLAC_NM, CLAC2_NM, n, total, client, sale, rebuy)
    mytable = mytable[complete.cases(mytable),]
  ## 최종 table 
    final_index_table= mytable %>% ungroup(myCLAC_NM) %>% mutate( count = n,
                                                                  total.per.count = total/n,
                                                                  total.per.client = total/client,
                                                                  sale = sale/n,
                                                                  rebuy = rebuy/n)
  ### PCA, FACTOR 
    dataset = scale( select(final_index_table,count,total.per.client,sale,rebuy) )
  
    # correlation matrix --------------------------------------------------# 
        M = dataset[complete.cases(dataset),]
        M = cor(M)
        corrplot(M, type="upper",  tl.col="black", tl.srt=45, method="number")
    # --------------------------------------------------------------------# 
  
    PCA1 = prcomp(dataset,center = T, scale. = T)             # method1  ->PCA$rotation[,1]  #비정칙치분해 이용 주성분 구함
    # x들 마다 분산은 유의하게 다르기 때문에 scale = TRUE 옵션을 사용 
    PCA2 = principal(dataset, nfactors = 2, rotate="varimax") # method2  ->PCA$communality 
    FACTOR = factanal(dataset, factors = 1, rotation = "varimax")   #    ->FACTOR$loadings[,1]

    PCA1$rotation[,1]
    PCA2$communality 
    FACTOR$loadings[,1]
    biplot(PCA1)
    biplot(PCA2)
  
  final_index_table = final_index_table %>% select(myCLAC_NM,CLAC2_NM) %>% cbind(dataset) %>% 
    mutate(PCA = as.numeric(as.matrix(dataset) %*% as.matrix(PCA2$communality )),
           FACTOR = as.numeric(as.matrix(dataset) %*% as.matrix(FACTOR$loadings[,1])))
  
  
  
  
  
################################### FINAL 주별 table ################################################
  
  input = final[!is.na(final$PD_C),] %>% filter(myWEEK!='2018-09-30')
  
  hit = read.csv("C:/Users/jeeyeon/Desktop/data/hit.csv"); hit = hit %>% mutate(hit = myHIT2)
  # hit = input %>% group_by(myCLAC_NM,CLAC2_NM,myWEEK) %>% summarize(hit = mean(HITS_SEQ,na.rm =T))
  total = input %>% group_by(myCLAC_NM,CLAC1_NM,CLAC2_NM,myWEEK) %>% summarize(total = sum(myPD_BUY_TOT,na.rm =T))
  client = input %>% group_by(CLNT_ID,myCLAC_NM,CLAC1_NM,CLAC2_NM,myWEEK) %>% count() %>% 
    group_by(myCLAC_NM,CLAC1_NM,CLAC2_NM,myWEEK) %>% count(); client$client = client$nn
  sale = input %>% group_by(myCLAC_NM,CLAC1_NM,CLAC2_NM,myWEEK) %>% summarize(sale = sum(mySALE_PD, na.rm =T)) 
  rebuy = input %>% distinct(CLNT_ID, CLAC1_NM,CLAC2_NM, SESS_ID, myWEEK) %>%
    group_by(CLNT_ID, CLAC2_NM, myWEEK) %>% count() %>%
    group_by(CLNT_ID, CLAC2_NM) %>% arrange(CLNT_ID, myWEEK) %>%
    mutate(obs = 1:n(), repurchase = ifelse(obs==1, n-1, n)) %>%
    group_by(CLAC2_NM, myWEEK) %>% summarise(rebuy = sum(repurchase, na.rm=T))

  mytable = input %>% group_by(myCLAC_NM,CLAC1_NM,CLAC2_NM,myWEEK) %>% count() %>% 
    left_join(hit) %>% left_join(total) %>% left_join(client) %>% left_join(sale) %>% 
    left_join(search1) %>% left_join(search2) %>% left_join(rebuy) %>% 
    select(myCLAC_NM,CLAC1_NM,CLAC2_NM,myWEEK,hit,n,total,client,search1,search2,sale,rebuy)
  
  final_index_table= mytable %>% ungroup(myCLAC_NM) %>% mutate( count = n,
                                                                total.per.count = total/n, # 건당 평균구매액
                                                                total.per.client = total/client,#고객당 평균구매액
                                                                sale = sale/n,
                                                                rebuy = rebuy/n)
  ### PCA, FACTOR 에 넣을 numeric 변수들만 뽑아놓자 --------------- --- ---
  dataset = scale(select(final_index_table,hit,count,total,client,search1,search2,rebuy))
  
  # 변수들의 단위가 다른 것으로부터 나오는 BIAS 피하기 위해서 SCALING
  dataset[is.na(dataset)] = 0
  sum(is.na(dataset))
  
  # correlation matrix --------------------------------------------------# 
  M = dataset[complete.cases(dataset),]; M = cor(M)
  corrplot(M, type="upper",  tl.col="black", tl.srt=45, method="number")
  # ---------------------------------------------------------------------#
  
  # PCA
  # PCA = prcomp(dataset,center = T, scale. = T) 
  PCA = prcomp(dataset)
  summary(PCA)
  print(PCA)
  PCA$rotation[,1]
  biplot(PCA)
  screeplot(PCA, main = "", col = "green", type = "lines", pch = 1, npcs = length(PCA$sdev))
  
  # Factor Analysis
  
  FA = principal(dataset, rotate="none")
  FA$values
  plot(FA$values, type="b", xlab ="인자수", ylab = "고유근")
  biplot(FA)
  
  FACTOR = factanal(dataset, factors = 1, rotation = "varimax")   # -> FACTOR$loadings[,1]
  FACTOR$loadings[,1]
  
  temp = scale(select(final_index_table,count,total,client,search1,search2))
  FACTOR_temp = factanal(temp, factors = 1, rotation = "varimax")   # -> FACTOR$loadings[,1]
  FACTOR_temp$loadings[,1]
  
  
  final_index_table = final_index_table %>% select(myCLAC_NM,CLAC1_NM,CLAC2_NM,myWEEK) %>% cbind(dataset) %>% 
        mutate(PCA = as.numeric(as.matrix(dataset) %*% as.matrix(PCA$rotation[,1] )),
               FACTOR_raw = as.numeric(as.matrix(dataset) %*% as.matrix(FACTOR$loadings[,1])),
               FACTOR = ((FACTOR_raw - min(FACTOR_raw))/ (max(FACTOR_raw)-min(FACTOR_raw)))*100 )
  plot_table = final_index_table 
  
  # pca 만 
  ggplot(data=plot_table, aes(x=myWEEK, y=PCA, colour=CLAC2_NM, group=CLAC2_NM)) + theme_light() + geom_line(size = 2) +
    geom_point(size=1) + ggtitle("Time Seires Graph") +theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  # factor 만 
  ggplot(data=plot_table, aes(x=myWEEK, y=FACTOR, colour=CLAC2_NM, group=CLAC2_NM)) + theme_light() + geom_line() +
    geom_point(size=1) + ggtitle("상품군별 선호지수")+ylab("선호지수")+xlab("단위:주")+ theme(axis.text.x = element_text(angle = 45, hjust = 1)) 
  
  ##### 대분류별로 다시 보자     #####
  large = final_index_table %>% filter(CLAC1_NM == "남성의류"|CLAC1_NM == "여성의류") 
  g1_1=ggplot(data=large , aes(x=myWEEK, y=FACTOR, colour=CLAC2_NM, group=CLAC2_NM)) + 
    theme_bw() + geom_line(size = 1.5) + geom_point(size=1)+ 
    ylab("선호지수")+xlab(NULL)+ labs(colour="중분류")  + #ggtitle("대분류별 선호지수|1-패션의류") 
    theme(axis.text.x = element_text(angle = 45, hjust = 1,size = 7),axis.title.x = element_text(angle=0, size=16),
          axis.title.y = element_text(size=16)) #+ scale_colour_brewer(palette="Paired") 
  
  large = final_index_table %>% filter(CLAC1_NM == "속옷/양말/홈웨어"|CLAC1_NM == "스포츠패션")
  g1_2=ggplot(data=large , aes(x=myWEEK, y=FACTOR, colour=CLAC2_NM, group=CLAC2_NM)) + 
    theme_bw() + geom_line(size = 1.5) + geom_point(size=1)+ 
    ylab("선호지수")+xlab(NULL)+ labs(colour="중분류")  + # ggtitle("대분류별 선호지수|1-속옷양말홈웨어스포츠")
    theme(axis.text.x = element_text(angle = 45, hjust = 1,size = 9),axis.title.x = element_text(angle=0, size=16),
          axis.title.y = element_text(size=16)) #+ scale_colour_brewer(palette="Paired") 
  
  large = final_index_table %>% filter(CLAC1_NM == "패션잡화") 
  g1_3=ggplot(data=large , aes(x=myWEEK, y=FACTOR, colour=CLAC2_NM, group=CLAC2_NM)) + 
    theme_bw() + geom_line(size = 1.5) + geom_point(size=1)+ 
    ylab("선호지수")+xlab(NULL)+ labs(colour="중분류")  + # ggtitle("대분류별 선호지수|1-패션잡화")
    theme(axis.text.x = element_text(angle = 45, hjust = 1,size = 9),axis.title.x = element_text(angle=0, size=16),
          axis.title.y = element_text(size=16)) #+ scale_colour_brewer(palette="Paired") 
  
  large = final_index_table %>% filter(myCLAC_NM == "2") 
  g2 = ggplot(data=large , aes(x=myWEEK, y=FACTOR, colour=CLAC2_NM, group=CLAC2_NM)) +
    theme_bw() + geom_line(size = 1.5) + geom_point(size=1)+ 
    ylab("선호지수")+xlab(NULL)+ labs(colour="중분류")  + # ggtitle("대분류별 선호지수|2-식품/농수산물")
    theme(axis.text.x = element_text(angle = 45, hjust = 1,size = 9),axis.title.x = element_text(angle=0, size=16),
          axis.title.y = element_text(size=16)) #+ scale_colour_brewer(palette="Paired") 
  
  large = final_index_table %>% filter(myCLAC_NM == "3") 
  g3 = ggplot(data=large , aes(x=myWEEK, y=FACTOR, colour=CLAC2_NM, group=CLAC2_NM)) + 
    theme_bw() + geom_line(size = 1.5) + geom_point(size=1)+ 
    ylab("선호지수")+xlab(NULL)+ labs(colour="중분류")  + # ggtitle("대분류별 선호지수|3-생활/자동차용품")
    theme(axis.text.x = element_text(angle = 45, hjust = 1,size = 9),axis.title.x = element_text(angle=0, size=16),
          axis.title.y = element_text(size=16)) #+ scale_colour_brewer(palette="Paired") 
  
  large = final_index_table %>% filter(myCLAC_NM == "4") 
  g4 = ggplot(data=large , aes(x=myWEEK, y=FACTOR, colour=CLAC2_NM, group=CLAC2_NM)) + 
    theme_bw() + geom_line(size = 1.5) + geom_point(size=1)+ 
    ylab("선호지수")+xlab(NULL)+ labs(colour="중분류")  + # ggtitle("대분류별 선호지수|4-가구")
    theme(axis.text.x = element_text(angle = 45, hjust = 1,size = 9),axis.title.x = element_text(angle=0, size=16),
          axis.title.y = element_text(size=16)) #+ scale_colour_brewer(palette="Paired") 
  
  large = final_index_table %>% filter(myCLAC_NM == "5") 
  g5 = ggplot(data=large , aes(x=myWEEK, y=FACTOR, colour=CLAC2_NM, group=CLAC2_NM)) + 
    theme_bw() + geom_line(size = 1.5) + geom_point(size=1)+ 
    ylab("선호지수")+xlab(NULL)+ labs(colour="중분류")  + # ggtitle("대분류별 선호지수|5-가전/컴퓨터/통신기기")
    theme(axis.text.x = element_text(angle = 45, hjust = 1,size = 9),axis.title.x = element_text(angle=0, size=16),
          axis.title.y = element_text(size=16)) #+ scale_colour_brewer(palette="Paired") 
  
  large = final_index_table %>% filter(myCLAC_NM == "6") 
  g6 = ggplot(data=large , aes(x=myWEEK, y=FACTOR, colour=CLAC2_NM, group=CLAC2_NM)) + 
    theme_bw() + geom_line(size = 1.5) + geom_point(size=1)+ 
    ylab("선호지수")+xlab(NULL)+ labs(colour="중분류") + #ggtitle("대분류별 선호지수|6-화장품")
    theme(axis.text.x = element_text(angle = 45, hjust = 1,size = 9),axis.title.x = element_text(angle=0, size=16),
          axis.title.y = element_text(size=16)) #+ scale_colour_brewer(palette="Paired") 
  
  large = final_index_table %>% filter(myCLAC_NM == "7") 
  g7 = ggplot(data=large , aes(x=myWEEK, y=FACTOR, colour=CLAC2_NM, group=CLAC2_NM)) + 
    theme_bw() + geom_line(size = 1.5) + geom_point(size=1)+ 
    ylab("선호지수")+xlab(NULL)+ labs(colour="중분류") + #ggtitle("대분류별 선호지수|7-아동/유아용품")
    theme(axis.text.x = element_text(angle = 45, hjust = 1,size = 9),axis.title.x = element_text(angle=0, size=16),
          axis.title.y = element_text(size=16)) #+ scale_colour_brewer(palette="Paired") 
  
  large = final_index_table %>% filter(myCLAC_NM == "8") 
  g8 = ggplot(data=large , aes(x=myWEEK, y=FACTOR, colour=CLAC2_NM, group=CLAC2_NM)) +
    theme_bw() + geom_line(size = 1.5) + geom_point(size=1)+ 
    ylab("선호지수")+xlab(NULL)+ labs(colour="중분류") + # ggtitle("대분류별 선호지수|8-스포츠/레저용품")
    theme(axis.text.x = element_text(angle = 45, hjust = 1,size = 9),axis.title.x = element_text(angle=0, size=16),
          axis.title.y = element_text(size=16)) #+ scale_colour_brewer(palette="Paired") 
  
  large = final_index_table %>% filter(myCLAC_NM == "9") 
  g9 = ggplot(data=large , aes(x=myWEEK, y=FACTOR, colour=CLAC2_NM, group=CLAC2_NM)) + 
    theme_bw() + geom_line(size = 1.5) + geom_point(size=1)+ 
    ylab("선호지수")+xlab(NULL)+ labs(colour="중분류") + # ggtitle("대분류별 선호지수|9-사무/문구")
    theme(axis.text.x = element_text(angle = 45, hjust = 1,size = 9),axis.title.x = element_text(angle=0, size=16),
          axis.title.y = element_text(size=16)) #+ scale_colour_brewer(palette="Paired") 
  
  large = final_index_table %>% filter(myCLAC_NM == "10") 
  g10 = ggplot(data=large , aes(x=myWEEK, y=FACTOR, colour=CLAC2_NM, group=CLAC2_NM)) + 
    theme_bw() + geom_line(size = 1.5) + geom_point(size=1)+ 
    ylab("선호지수")+xlab(NULL)+ labs(colour="중분류") + # ggtitle("대분류별 선호지수|10-애완용품")
    theme(axis.text.x = element_text(angle = 45, hjust = 1,size = 9),axis.title.x = element_text(angle=0, size=16),
          axis.title.y = element_text(size=16)) #+ scale_colour_brewer(palette="Paired") 
  
  g1_1
  g1_2
  g1_3
  g2
  g3
  g4
  g5
  g6
  g7
  g8
  g9
  g10
  
  
  
  
  ##### 값 높은 중분류 순서      ####
  
  temp = final_index_table %>% group_by(myCLAC_NM,CLAC2_NM) %>% 
    summarise(topfactor= sum(FACTOR, na.rm = T)) %>% arrange(desc(topfactor,myWEEK,CLAC2_NM));n = nrow(temp)
  
  gr1 = temp[1:10,] ; gr1 = gr1$CLAC2_NM
  gr2 = temp[11:20,] ; gr2 = gr2$CLAC2_NM
  gr3 = temp[21:30,] ; gr3 = gr3$CLAC2_NM
  gr4 = temp[31:40,] ; gr4 = gr4$CLAC2_NM
  gr5 = temp[41:50,] ; gr5 = gr5$CLAC2_NM
  gr6 = temp[51:60,] ; gr6 = gr6$CLAC2_NM
  gr7 = temp[61:70,] ; gr7 = gr7$CLAC2_NM
  gr8 = temp[71:80,] ; gr8 = gr8$CLAC2_NM
  gr9 = temp[81:90,] ; gr9 = gr9$CLAC2_NM
  gr10 = temp[91:100,] ; gr10 = gr10$CLAC2_NM
  gr11 = temp[101:110,] ; gr11 = gr11$CLAC2_NM
  gr12 = temp[111:120,] ; gr12 = gr12$CLAC2_NM
  gr13 = temp[121:128,] ; gr13 = gr13$CLAC2_NM
  
  table = final_index_table[final_index_table$CLAC2_NM %in% gr1,] 
  table = final_index_table[final_index_table$CLAC2_NM %in% gr2,] 
  table = final_index_table[final_index_table$CLAC2_NM %in% gr3,] 
  
  table = final_index_table[final_index_table$CLAC2_NM %in% gr4,] 
  table = final_index_table[final_index_table$CLAC2_NM %in% gr5,] 
  table = final_index_table[final_index_table$CLAC2_NM %in% gr6,] 
  
  table = final_index_table[final_index_table$CLAC2_NM %in% gr7,] 
  table = final_index_table[final_index_table$CLAC2_NM %in% gr8,] 
  table = final_index_table[final_index_table$CLAC2_NM %in% gr9,]
  
  table = final_index_table[final_index_table$CLAC2_NM %in% gr10,]
  table = final_index_table[final_index_table$CLAC2_NM %in% gr11,] 
  table = final_index_table[final_index_table$CLAC2_NM %in% gr12,] 
  table = final_index_table[final_index_table$CLAC2_NM %in% gr13,] 

  ggplot(data=table, aes(x=myWEEK, y=FACTOR, colour=CLAC2_NM,group=CLAC2_NM)) + 
    theme_bw() + geom_line(size = 1.5) + geom_point(size=1)+ 
    ylab("선호지수")+xlab(NULL)+ labs(colour="중분류") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1,size = 9),axis.title.x = element_text(angle=0, size=16),
          axis.title.y = element_text(size=16)) + scale_colour_brewer(palette="Paired") 
  
  
  ##### 환경적 요인별로 보자     ######
  
  # 1    4,5월 증가 -> 어린이날, 어버이날
  family = final_index_table %>% filter(CLAC2_NM == "공기청정/가습/제습"|
                                          CLAC2_NM == "남성등산/아웃도어의류"| CLAC2_NM == "홍삼/인삼가공식품"|
                                          CLAC2_NM == "골프")
  
  family1 = final_index_table %>% filter(CLAC2_NM == "남아완구"| CLAC2_NM == "교육완구"|CLAC2_NM == "여아완구"|
                                          CLAC2_NM == "여아의류상의"|CLAC2_NM == "인라인/스케이트보드/킥보드")
  
  ggplot(data=family , aes(x=myWEEK, y=FACTOR, colour=CLAC2_NM, group=CLAC2_NM)) + 
    theme_bw() + geom_line(size = 1.5) + geom_point(size=1)+ 
    ylab("선호지수")+xlab(NULL)+ labs(colour="중분류")  + 
    theme(axis.text.x = element_text(angle = 45, hjust = 1,size = 7),axis.title.x = element_text(angle=0, size=16),
          axis.title.y = element_text(size=16)) + scale_colour_brewer(palette="Paired") 
  ggplot(data=family1 , aes(x=myWEEK, y=FACTOR, colour=CLAC2_NM, group=CLAC2_NM)) + 
    theme_bw() + geom_line(size = 1.5) + geom_point(size=1)+ 
    ylab("선호지수")+xlab(NULL)+ labs(colour="중분류")  + 
    theme(axis.text.x = element_text(angle = 45, hjust = 1,size = 7),axis.title.x = element_text(angle=0, size=16),
          axis.title.y = element_text(size=16)) + scale_colour_brewer(palette="Paired")
  
  
  # 2    7,8월 증가 -> 여름 
  summer = final_index_table %>% filter(CLAC2_NM == "수영/물놀이"|CLAC2_NM == "냉방가전")
  
  summer1 = final_index_table %>% filter(CLAC2_NM == "모자"|CLAC2_NM == "안경/선글라스"|
                                           CLAC2_NM == "선케어"|CLAC2_NM == "우산/양산류"|CLAC2_NM == "캠핑"|
                                           
                                           CLAC2_NM == "생수"|CLAC2_NM == "두유"|
                                           CLAC2_NM == "닭고기류"|CLAC2_NM == "건강보조식품")
                                        
  summer2 = final_index_table %>% filter(CLAC2_NM == "화장지/티슈"|CLAC2_NM == "여성위생용품"|
                                        CLAC2_NM == "모바일상품권"|CLAC2_NM == "밀폐/보관용기"|
                                        
                                        CLAC2_NM == "시공/DIY가구")
  
  ggplot(data=summer , aes(x=myWEEK, y=FACTOR, colour=CLAC2_NM, group=CLAC2_NM)) + 
    theme_bw() + geom_line(size = 1.5) + geom_point(size=1)+ 
    ylab("선호지수")+xlab(NULL)+ labs(colour="중분류")  + 
    theme(axis.text.x = element_text(angle = 45, hjust = 1,size = 7),axis.title.x = element_text(angle=0, size=16),
          axis.title.y = element_text(size=16)) + scale_colour_brewer(palette="Paired") 
  ggplot(data=summer1 , aes(x=myWEEK, y=FACTOR, colour=CLAC2_NM, group=CLAC2_NM)) + 
    theme_bw() + geom_line(size = 1.5) + geom_point(size=1)+ 
    ylab("선호지수")+xlab(NULL)+ labs(colour="중분류")  + 
    theme(axis.text.x = element_text(angle = 45, hjust = 1,size = 7),axis.title.x = element_text(angle=0, size=16),
          axis.title.y = element_text(size=16)) + scale_colour_brewer(palette="Paired") 
  ggplot(data=summer2 , aes(x=myWEEK, y=FACTOR, colour=CLAC2_NM, group=CLAC2_NM)) + 
    theme_bw() + geom_line(size = 1.5) + geom_point(size=1)+ 
    ylab("선호지수")+xlab(NULL)+ labs(colour="중분류")  + 
    theme(axis.text.x = element_text(angle = 45, hjust = 1,size = 7),axis.title.x = element_text(angle=0, size=16),
          axis.title.y = element_text(size=16)) + scale_colour_brewer(palette="Paired") 
  
  
  # 3. 9월 증가 -> 추석
  holiday = final_index_table %>% filter(CLAC2_NM == "축산선물세트"| CLAC2_NM == "영양제"| CLAC2_NM == "건강진액"| 
                                          CLAC2_NM == "홍삼/인삼가공식품"|CLAC2_NM == "기능성음료"|
                                           CLAC2_NM == "국산과일"| CLAC2_NM == "견과류"|CLAC2_NM == "남성의류아우터")
  
  holiday1 = final_index_table %>% filter(CLAC2_NM == "유아의류아우터"|CLAC2_NM == "여아의류아우터"|
                                           CLAC2_NM == "유아동일반스포츠의류" )
  
  ggplot(data=holiday , aes(x=myWEEK, y=FACTOR, colour=CLAC2_NM, group=CLAC2_NM)) + 
    theme_bw() + geom_line(size = 1.5) + geom_point(size=1)+ 
    ylab("선호지수")+xlab(NULL)+ labs(colour="중분류")  + 
    theme(axis.text.x = element_text(angle = 45, hjust = 1,size = 7),axis.title.x = element_text(angle=0, size=16),
          axis.title.y = element_text(size=16)) + scale_colour_brewer(palette="Paired") 
  ggplot(data=holiday1 , aes(x=myWEEK, y=FACTOR, colour=CLAC2_NM, group=CLAC2_NM)) + 
    theme_bw() + geom_line(size = 1.5) + geom_point(size=1)+ 
    ylab("선호지수")+xlab(NULL)+ labs(colour="중분류")  + 
    theme(axis.text.x = element_text(angle = 45, hjust = 1,size = 7),axis.title.x = element_text(angle=0, size=16),
          axis.title.y = element_text(size=16)) + scale_colour_brewer(palette="Paired") 
  
  
  # 4. 1인가구 증가 
  solo = final_index_table %>% filter(CLAC2_NM == "포장반찬"|CLAC2_NM == "냉동간편식"|
                                        CLAC2_NM == "시공/DIY가구" )
  
  ggplot(data=solo , aes(x=myWEEK, y=FACTOR, colour=CLAC2_NM, group=CLAC2_NM)) + 
    theme_bw() + geom_line(size = 1.5) + geom_point(size=1)+ 
    ylab("선호지수")+xlab(NULL)+ labs(colour="중분류")  + 
    theme(axis.text.x = element_text(angle = 45, hjust = 1,size = 7),axis.title.x = element_text(angle=0, size=16),
          axis.title.y = element_text(size=16)) + scale_colour_brewer(palette="Paired") 
  
  # 5. 여성 패션/ 잡화 추세 비슷 
  woman = final_index_table %>% filter(CLAC2_NM == "여성의류전신"|CLAC2_NM == "여성의류하의"|
                                        CLAC2_NM == "여성화"|CLAC2_NM == "여성속옷"|
                                         CLAC2_NM == "남성의류하의")
  
  ggplot(data=woman , aes(x=myWEEK, y=FACTOR, colour=CLAC2_NM, group=CLAC2_NM)) + 
    theme_bw() + geom_line(size = 1.5) + geom_point(size=1)+ 
    ylab("선호지수")+xlab(NULL)+ labs(colour="중분류")  + 
    theme(axis.text.x = element_text(angle = 45, hjust = 1,size = 7),axis.title.x = element_text(angle=0, size=16),
          axis.title.y = element_text(size=16)) + scale_colour_brewer(palette="Paired") 
  
  
  
  
  
  
  ##### 예측                     ############
  
  pred_data = final_index_table %>% filter(CLAC2_NM == "여성의류전신") %>% select(myWEEK, count ) %>% mutate(n = count)
  pred_data = pred_data %>% mutate(y = cumsum(n), y1 = lag(y),t = 1:n() ); pred_data$y1[1] = 0
  
  #quadratic function
  quadratic <- function(coeff) {
    a <- coeff[1];b <- coeff[2];c <- coeff[3]; d <- b^2 - 4*a*c
    if(d >= 0) {
      root1 <- (-b + sqrt(d))/(2*a); root2 <- (-b - sqrt(d))/(2*a)
      return(c(root1,root2))}}
  
  # bass model _________________
  bass_lm <- lm( n ~ y1 + I(y1^2) ,pred_data ); s.lm <- summary(bass_lm)
  bass_m <- quadratic(c(s.lm$coef[3],s.lm$coef[2],s.lm$coef[1]))[2]
  bass_p <- s.lm$coef[1] / bass_m
  bass_q <- -bass_m * s.lm$coef[3]
  bass_error <- 100*(bass_m - pred_data$y[26])*(1/pred_data$y[26])
  bass_mse <- mean(s.lm$residuals^2)
  # logistic model _________________
  logistic_lm <- lm( n ~ y1 + I(y1^2) + 0 , pred_data ); s.lm <- summary(logistic_lm)
  logistic_m <- -logistic_lm$coefficients[1]/logistic_lm$coefficients[2]
  logistic_q <- logistic_lm$coefficients[1]
  logistic_error <- 100*(logistic_m - pred_data$y[26])/pred_data$y[26]
  logistic_mse <- mean(s.lm$residuals^2)
  # gumbel model _________________
  gumbel_lm <- lm( n ~ y1 + I(y1*log(y1))+0 ,pred_data ); s.lm <- summary(gumbel_lm)
  gumbel_m <- exp(-s.lm$coefficients[1] / s.lm$coefficients[2])
  gumbel_q <- -s.lm$coefficients[2]
  gumbel_error <- 100*(gumbel_m - pred_data$y[26])/pred_data$y[26]
  gumbel_mse <- mean(s.lm$residuals^2)
  # exponential model _________________
  exp_lm <- lm(n ~ y1, data =pred_data );  s.lm <- summary(exp_lm)
  exp_p = -coef(exp_lm)[2]
  exp_m = coef(exp_lm)[1]/exp_p
  exp_error = 100*(exp_m-pred_data$y[26])/pred_data$y[26]
  exp_mse <- mean(s.lm$residuals^2)
  #qqplot
  k <- bass_p+bass_q; c <- bass_q/bass_p
  pred_data = pred_data %>% mutate(ubass = y/(bass_m+1),
                                   qbass = (1/k)*log((1+c*ubass)/(1-ubass)),
                                   St_pbass = (bass_p + bass_q*y/bass_m)*(bass_m-y),
                                   Yt_pbass = cumsum(St_pbass) ,
                                   
                                   ulogistic = y / (logistic_m+1),
                                   qlogistic = log(ulogistic/(1-ulogistic)),
                                   St_plogistic = qlogistic*y/logistic_m*(logistic_m-y),
                                   Yt_plogistic = cumsum(St_plogistic),
                                   
                                   ugumbel = y / (gumbel_m+1),
                                   qgumbel = -log(-log(ugumbel)),
                                   St_pgumbel = qgumbel*y*(log(gumbel_m)-log(y)),
                                   Yt_pgumbel = cumsum(St_pgumbel),
                                   
                                   uexp = y/(exp_m+1),
                                   qexp = (1/exp_p)*(-log(1-uexp)),
                                   St_pexp = qexp*y/exp_m*(exp_m-y),
                                   Yt_pexp = cumsum(St_pexp))
  
  #r^2
  qqlm <- lm( t ~ qbass ,pred_data );  bass_r2 <- summary(qqlm)$r.squared
  qqlm <- lm( t ~ qlogistic ,pred_data ); logistic_r2 <- summary(qqlm)$r.squared
  qqlm <- lm( t ~ qgumbel ,pred_data );  gumbel_r2 <- summary(qqlm)$r.squared
  qqlm <- lm( t ~ qexp ,pred_data );  exp_r2 <- summary(qqlm)$r.squared
  
  # graph 
  predqq1 <- ggplot(pred_data) +
    geom_line(aes(t, n), size = 1) + geom_line(aes(t, St_pbass), color = "red", size = 1) + 
    labs(x = "t", title = "선호지수 vs 예측된 선호지수") + theme_bw()
  predqq2 <- ggplot(pred_data) +
    geom_line(aes(t, y), size = 1) + geom_line(aes(t, Yt_pbass), color = "red", size = 1) + 
    labs(x = "t", title = "누적선호지수 & 예측된 선호지수") + theme_bw()
  gridExtra::grid.arrange(predqq1, predqq2, ncol = 2)
  
  predqq1 <- ggplot(pred_data) +
    geom_line(aes(t, n), size = 1) + geom_line(aes(t, St_plogistic), color = "blue", size = 1) + 
    labs(x = "t", title = "St & predicted St") + theme_bw()
  predqq2 <- ggplot(pred_data) +
    geom_line(aes(t, y), size = 1) + geom_line(aes(t, Yt_plogistic), color = "blue", size = 1) + 
    labs(x = "t", title = "Yt & predicted Yt") + theme_bw()
  gridExtra::grid.arrange(predqq1, predqq2, nrow = 2)
  
  predqq1 <- ggplot(pred_data) +
    geom_line(aes(t, n), size = 1) + geom_line(aes(t, St_pgumbel), color = "blue", size = 1) + 
    labs(x = "t", title = "St & predicted St") + theme_bw()
  predqq2 <- ggplot(pred_data) +
    geom_line(aes(t, y), size = 1) + geom_line(aes(t, Yt_pgumbel), color = "blue", size = 1) + 
    labs(x = "t", title = "Yt & predicted Yt") + theme_bw()
  gridExtra::grid.arrange(predqq1, predqq2, nrow = 2)
  
  predqq1 <- ggplot(pred_data) +
    geom_line(aes(t, n), size = 1) + geom_line(aes(t, St_pexp), color = "blue", size = 1) + 
    labs(x = "t", title = "St & predicted St") + theme_bw()
  predqq2 <- ggplot(pred_data) +
    geom_line(aes(t, y), size = 1) + geom_line(aes(t, Yt_pexp), color = "blue", size = 1) + 
    labs(x = "t", title = "Yt & predicted Yt") + theme_bw()
  gridExtra::grid.arrange(predqq1, predqq2, nrow = 2)
  
  ## QQ Plot
  g1 <- ggplot(pred_data, aes(x=qbass, y=t))+geom_point()+geom_smooth(method="lm",se=F)+ylab("X(r)")+
    ggtitle("Bass Q-Q Plot") + theme_bw()
  g2 <- ggplot(pred_data, aes(x=qlogistic, y=t))+geom_point()+geom_smooth(method="lm",se=F)+ylab("X(r)")+
    ggtitle("Logistic Q-Q Plot")+ theme_bw()
  g3 <- ggplot(pred_data, aes(x=qgumbel, y=t))+geom_point()+geom_smooth(method="lm",se=F)+ylab("X(r)")+
    ggtitle("Gumbel Q-Q Plot")+ theme_bw()

  gridExtra::grid.arrange(g1,g2,g3,ncol = 3)
  

  ##### 선호지수예측         ############
  
  pred_data = final_index_table %>% filter(CLAC2_NM == "남성의류상의") %>% select(myWEEK, FACTOR ) %>% mutate(n = FACTOR)
  pred_data = pred_data %>% mutate(y = cumsum(n), y1 = lag(y),t = 1:n() ); pred_data$y1[1] = 0
  
  #quadratic function
  quadratic <- function(coeff) {  
    a <- coeff[1];b <- coeff[2];c <- coeff[3]; d <- b^2 - 4*a*c
    if(d >= 0) {
      root1 <- (-b + sqrt(d))/(2*a); root2 <- (-b - sqrt(d))/(2*a)
      return(c(root1,root2))}}
  
  # bass model _________________
  bass_lm <- lm( n ~ y1 + I(y1^2) ,pred_data ); s.lm <- summary(bass_lm)
  bass_m <- quadratic(c(s.lm$coef[3],s.lm$coef[2],s.lm$coef[1]))[2]
  bass_p <- s.lm$coef[1] / bass_m
  bass_q <- -bass_m * s.lm$coef[3]
  bass_error <- 100*(bass_m - pred_data$y[26])*(1/pred_data$y[26])
  bass_mse <- mean(s.lm$residuals^2)
  # logistic model _________________
  logistic_lm <- lm( n ~ y1 + I(y1^2) + 0 , pred_data ); s.lm <- summary(logistic_lm)
  logistic_m <- -logistic_lm$coefficients[1]/logistic_lm$coefficients[2]
  logistic_q <- logistic_lm$coefficients[1]
  logistic_error <- 100*(logistic_m - pred_data$y[26])/pred_data$y[26]
  logistic_mse <- mean(s.lm$residuals^2)
  # gumbel model _________________
  gumbel_lm <- lm( n ~ y1 + I(y1*log(y1))+0 ,pred_data ); s.lm <- summary(gumbel_lm)
  gumbel_m <- exp(-s.lm$coefficients[1] / s.lm$coefficients[2])
  gumbel_q <- -s.lm$coefficients[2]
  gumbel_error <- 100*(gumbel_m - pred_data$y[26])/pred_data$y[26]
  gumbel_mse <- mean(s.lm$residuals^2)
  # exponential model _________________
  exp_lm <- lm(n ~ y1, data =pred_data );  s.lm <- summary(exp_lm)
  exp_p = -coef(exp_lm)[2]
  exp_m = coef(exp_lm)[1]/exp_p
  exp_error = 100*(exp_m-pred_data$y[26])/pred_data$y[26]
  exp_mse <- mean(s.lm$residuals^2)
  #qqplot
  k <- bass_p+bass_q; c <- bass_q/bass_p
  pred_data = pred_data %>% mutate(ubass = y/(bass_m+1),
                                   qbass = (1/k)*log((1+c*ubass)/(1-ubass)),
                                   St_pbass = (bass_p + bass_q*y/bass_m)*(bass_m-y),
                                   Yt_pbass = cumsum(St_pbass) ,
                                   
                                   ulogistic = y / (logistic_m+1),
                                   qlogistic = log(ulogistic/(1-ulogistic)),
                                   St_plogistic = qlogistic*y/logistic_m*(logistic_m-y),
                                   Yt_plogistic = cumsum(St_plogistic),
                                   
                                   ugumbel = y / (gumbel_m+1),
                                   qgumbel = -log(-log(ugumbel)),
                                   St_pgumbel = qgumbel*y*(log(gumbel_m)-log(y)),
                                   Yt_pgumbel = cumsum(St_pgumbel),
                                   
                                   uexp = y/(exp_m+1),
                                   qexp = (1/exp_p)*(-log(1-uexp)),
                                   St_pexp = qexp*y/exp_m*(exp_m-y),
                                   Yt_pexp = cumsum(St_pexp))
  
  #r^2
  qqlm <- lm( t ~ qbass ,pred_data );  bass_r2 <- summary(qqlm)$r.squared
  qqlm <- lm( t ~ qlogistic ,pred_data ); logistic_r2 <- summary(qqlm)$r.squared
  qqlm <- lm( t ~ qgumbel ,pred_data );  gumbel_r2 <- summary(qqlm)$r.squared
  qqlm <- lm( t ~ qexp ,pred_data );  exp_r2 <- summary(qqlm)$r.squared
  
  # graph 
  predqq1 <- ggplot(pred_data) +
    geom_line(aes(t, n), size = 1) + geom_line(aes(t, St_pbass), color = "red", size = 1) + 
    labs(x = "t", title = "구매건수 vs 예측된 구매건수") + theme_bw()
  predqq2 <- ggplot(pred_data) +
    geom_line(aes(t, y), size = 1) + geom_line(aes(t, Yt_pbass), color = "red", size = 1) + 
    labs(x = "t", title = "누적구매건수 & 예측된 누적구매건수") + theme_bw()
  gridExtra::grid.arrange(predqq1, predqq2, ncol = 2)
  
  predqq1 <- ggplot(pred_data) +
    geom_line(aes(t, n), size = 1) + geom_line(aes(t, St_plogistic), color = "blue", size = 1) + 
    labs(x = "t", title = "St & predicted St") + theme_bw()
  predqq2 <- ggplot(pred_data) +
    geom_line(aes(t, y), size = 1) + geom_line(aes(t, Yt_plogistic), color = "blue", size = 1) + 
    labs(x = "t", title = "Yt & predicted Yt") + theme_bw()
  gridExtra::grid.arrange(predqq1, predqq2, nrow = 2)
  
  predqq1 <- ggplot(pred_data) +
    geom_line(aes(t, n), size = 1) + geom_line(aes(t, St_pgumbel), color = "blue", size = 1) + 
    labs(x = "t", title = "St & predicted St") + theme_bw()
  predqq2 <- ggplot(pred_data) +
    geom_line(aes(t, y), size = 1) + geom_line(aes(t, Yt_pgumbel), color = "blue", size = 1) + 
    labs(x = "t", title = "Yt & predicted Yt") + theme_bw()
  gridExtra::grid.arrange(predqq1, predqq2, nrow = 2)
  
  predqq1 <- ggplot(pred_data) +
    geom_line(aes(t, n), size = 1) + geom_line(aes(t, St_pexp), color = "blue", size = 1) + 
    labs(x = "t", title = "St & predicted St") + theme_bw()
  predqq2 <- ggplot(pred_data) +
    geom_line(aes(t, y), size = 1) + geom_line(aes(t, Yt_pexp), color = "blue", size = 1) + 
    labs(x = "t", title = "Yt & predicted Yt") + theme_bw()
  gridExtra::grid.arrange(predqq1, predqq2, nrow = 2)
  
  ## QQ Plot
  g1 <- ggplot(pred_data, aes(x=qbass, y=t))+geom_point()+geom_smooth(method="lm",se=F)+ylab("X(r)")+
    ggtitle("Bass Q-Q Plot") + theme_bw()
  g2 <- ggplot(pred_data, aes(x=qlogistic, y=t))+geom_point()+geom_smooth(method="lm",se=F)+ylab("X(r)")+
    ggtitle("Logistic Q-Q Plot")+ theme_bw()
  g3 <- ggplot(pred_data, aes(x=qgumbel, y=t))+geom_point()+geom_smooth(method="lm",se=F)+ylab("X(r)")+
    ggtitle("Gumbel Q-Q Plot")+ theme_bw()
  
  gridExtra::grid.arrange(g1,g2,g3,ncol = 3)
  
  
################################### FINAL PPT Table  ################################################
  
  # 대분류별
  ppt = plot_table %>% group_by(myCLAC_NM,CLAC1_NM,CLAC2_NM) %>% 
                       summarize(FACTOR_raw = sum(FACTOR_raw, na.rm =T)) %>% ungroup() %>% 
                       mutate(FACTOR =((FACTOR_raw - min(FACTOR_raw))/(max(FACTOR_raw)-min(FACTOR_raw)))*100 )
  
  group = ppt %>% filter(myCLAC_NM == "1")
  group = ppt %>% filter(myCLAC_NM == "2")
  group = ppt %>% filter(myCLAC_NM == "3")
  group = ppt %>% filter(myCLAC_NM == "4")
  group = ppt %>% filter(myCLAC_NM == "5")
  group = ppt %>% filter(myCLAC_NM == "6")
  group = ppt %>% filter(myCLAC_NM == "7")
  group = ppt %>% filter(myCLAC_NM == "8")
  group = ppt %>% filter(myCLAC_NM == "9")
  group = ppt %>% filter(myCLAC_NM == "10")

  ggplot(group, aes(x =reorder(CLAC2_NM, -FACTOR), y = FACTOR, fill =CLAC1_NM)) + geom_bar(stat = "identity") + theme_light() + 
    ylab("선호지수")+xlab(NULL) +labs(fill="대분류") +scale_fill_brewer(palette="Set3") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  
  
  
  
  

#### pca, factor analysis 연습 ################################################################################
    
#초기 요인분석 실행
med.factor <- principal(dataset, rotate="none")
names(med.factor)
med.factor$values
plot(med.factor$values, type="b")  
# 우리의 고유근 2개 -> 2개 요인 사용해야함 


med.Varimax = principal(dataset, nfactors = 2, rotate="varimax") # rotate = "oblimin" rotate="none"
med.Varimax

# h2는 각변수의 공통성(communality): 다른 변수들과의 공통성, 0.3 보다 아래이면 다른 변수들과 공통점이 별로 없다
# Proportion var: 각 요인이 설명하는 총 분산의 비율
biplot(med.Varimax)
# 출처: http://datacookbook.kr/39 [DATA COOKBOOK]


result = factanal(dataset, factors = 1, rotation = "varimax")
# 출처: https://note.espriter.net/1241 [에스프리터]



# ------------------------------------------------------------
##### 선그래프로 추세를 살펴볼까?                          #####

## 예시 ## 
tt = final

day = tt %>% group_by(myCLAC_NM,CLAC2_NM,SESS_DT) %>% summarize(total = sum(PD_BUY_CT,na.rm =T)) %>% arrange(SESS_DT)
month = tt %>% group_by(myCLAC_NM,CLAC2_NM,myMONTH) %>% summarize(total = sum(PD_BUY_CT,na.rm =T)) %>% arrange(myMONTH)
ggplot(data=day, aes(x=SESS_DT, y=total, colour=CLAC2_NM, group=CLAC2_NM)) + theme_light() +
  geom_line() + geom_point(size=1) + ggtitle("Time Seires Graph")
ggplot(data=month, aes(x=myMONTH, y=total, colour=CLAC2_NM, group=CLAC2_NM)) + theme_light() +
  geom_line() + geom_point(size=1) + ggtitle("Time Seires Graph")

# *** WEEK ***
week = tt %>% group_by(myCLAC_NM,CLAC2_NM,myWEEK) %>% summarize(total = sum(PD_BUY_CT,na.rm =T)) %>% arrange(myWEEK)
ggplot(data=week, aes(x=myWEEK, y=total, colour=CLAC2_NM, group=CLAC2_NM)) + theme_light() +
  geom_line() + geom_point(size=1) + ggtitle("Time Seires Graph")  +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) # +theme(legend.position = c(0.85, 0.2))




tt = final %>% filter(myCLAC_NM=="9") %>% group_by(myCLAC_NM, CLAC2_NM, CLAC3_NM) %>% count() %>% arrange(desc(n))

day = tt %>% group_by(myCLAC_NM,CLAC2_NM,SESS_DT) %>% count() %>% arrange(SESS_DT)
month = tt %>% group_by(myCLAC_NM,CLAC2_NM,myMONTH) %>%  count()  %>% arrange(myMONTH)
ggplot(data=day, aes(x=SESS_DT, y=n, colour=CLAC2_NM, group=CLAC2_NM)) + theme_light() +
  geom_line() + geom_point(size=1) + ggtitle("Time Seires Graph")
ggplot(data=month, aes(x=myMONTH, y=n, colour=CLAC2_NM, group=CLAC2_NM)) + theme_light() +
  geom_line() + geom_point(size=1) + ggtitle("Time Seires Graph")

# *** WEEK ***
week = tt %>% group_by(myCLAC_NM,CLAC2_NM,myWEEK) %>%  count()  %>% arrange(myWEEK)
# %>% filter(CLAC2_NM == "여성의류상의") 
ggplot(data=week, aes(x=myWEEK, y=n, colour=CLAC2_NM, group=CLAC2_NM)) + theme_light() +
  geom_line() + geom_point(size=1) + ggtitle("Time Seires Graph")  +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) # +theme(legend.position = c(0.85, 0.2))





### 중분류 
week = final %>% filter(myCLAC_NM=="3"|myCLAC_NM=="9"|myCLAC_NM=="10") %>% group_by(myCLAC_NM,CLAC2_NM,myWEEK) %>% 
  summarize(total = sum(PD_BUY_CT,na.rm =T)) %>% arrange(myWEEK)
ggplot(data=week, aes(x=myWEEK, y=total, colour=CLAC2_NM, group=CLAC2_NM)) + theme_light() +
  geom_line() + geom_point(size=1) + ggtitle("Time Seires Graph")  +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

### 소분류 
week = final %>% filter(myCLAC_NM=="9") %>% filter(CLAC2_NM == "일반문구/사무용품") %>% group_by(myCLAC_NM,CLAC2_NM,CLAC3_NM,myWEEK) %>% 
  count() %>% arrange(myWEEK)
ggplot(data=week, aes(x=myWEEK, y=n, colour=CLAC3_NM, group=CLAC3_NM)) + theme_light() +
  geom_line() + geom_point(size=1) + ggtitle("Time Seires Graph")  +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))




##### 한번에 여러개씩 구매하는 제품은 무엇일까?  - package #####

input = final
package1 = input %>% group_by(CLNT_ID,PD_C) %>% count() %>% filter(n>1) 
package2 = package1 %>% group_by(PD_C,n) %>% count()
package3 = package2 %>% filter(nn >100) 
package = left_join(package3, master, by = "PD_C")
package = package[complete.cases(package),]

ggplot(package, aes(reorder(CLAC1_NM, -nn),nn)) + geom_bar(stat="identity") +theme_light() + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# ---

# 각 상품코드별 구매 건수에 대한 히스토그램 
package4 = input %>% group_by(CLNT_ID,PD_C) %>% count() %>% group_by(PD_C,n) %>% count() %>% 
package4 = package4[complete.cases(package4),]

package.PD_C = package[,1] %>% distinct(PD_C)
package.PD_C = as.numeric( package.PD_C[3,1])

find = package4[package4$PD_C == package.PD_C,] %>% filter(n >1)

ggplot(find, aes(reorder(n, -nn),nn)) + geom_bar(stat="identity") +theme_light() + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# ---

# 2개씩 가장 많이 구매한 각 상품코드 (n ==2 )
package5 = package2 %>% filter(n == 2) %>% arrange(desc(nn)) %>% filter(nn > 500) %>% left_join( master, by = "PD_C")
ggplot(package5, aes(reorder(PD_C, -nn),nn)) + geom_bar(stat="identity") +theme_light() + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
ggplot(package5, aes(reorder(CLAC3_NM, -nn),nn)) + geom_bar(stat="identity") +theme_light() + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

temp = final[complete.cases(final),] %>% group_by(CLNT_ID,PD_C) %>% count() %>% 
                                         group_by(PD_C,n) %>% count()

temp1 = temp %>% filter(PD_C == "753343") %>% filter(n <7)
temp2 = temp %>% filter(PD_C == "748308") %>% filter(n <7)
temp3 = temp %>% filter(PD_C == "692709") %>% filter(n <7)

ggplot(temp1, aes(as.factor(n), nn)) + geom_bar(stat = "identity")+theme_light() + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + xlab("구입상품수") + ylab("빈도")
ggplot(temp2, aes(as.factor(n), nn))  + geom_bar(stat = "identity")+theme_light() + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + xlab("구입상품수") + ylab("빈도")
ggplot(temp3, aes(as.factor(n), nn))  + geom_bar(stat = "identity")+theme_light() + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + xlab("구입상품수") + ylab("빈도")


a1 = final[complete.cases(final),] %>% filter(PD_C == "753343") %>% 
  mutate(GENDER_AGE = paste(CLNT_GENDER,CLNT_AGE,sep = "" )) %>% group_by(CLNT_GENDER,GENDER_AGE) %>% count()
a2 = final[complete.cases(final),] %>% filter(PD_C == "748308") %>% 
  mutate(GENDER_AGE = paste(CLNT_GENDER,CLNT_AGE,sep = "" )) %>% group_by(CLNT_GENDER,GENDER_AGE) %>% count()
a3 = final[complete.cases(final),] %>% filter(PD_C == "692709") %>% 
  mutate(GENDER_AGE = paste(CLNT_GENDER,CLNT_AGE,sep = "" )) %>% group_by(CLNT_GENDER,GENDER_AGE) %>% count()

ggplot(a1, aes(x =as.factor(GENDER_AGE), y = n, fill =CLNT_GENDER))+geom_bar(stat = "identity")+theme_light() + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
  xlab("연령/성별그룹") + ylab("빈도") + scale_fill_brewer(palette="Set1")
ggplot(a2, aes(x =as.factor(GENDER_AGE), y = n, fill =CLNT_GENDER))+geom_bar(stat = "identity")+theme_light() + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
  xlab("연령/성별그룹") + ylab("빈도") + scale_fill_brewer(palette="Set1")
ggplot(a3, aes(x =as.factor(GENDER_AGE), y = n, fill =CLNT_GENDER))+geom_bar(stat = "identity")+theme_light() + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
  xlab("연령/성별그룹") + ylab("빈도") + scale_fill_brewer(palette="Set1")





# ------------------------------------------------------------
######  Targeting                                          ###### 

### 구매 많이한 사람 순 client ID 
order(table(final$CLNT_ID),decreasing = TRUE)
client_table = final %>% group_by(CLNT_ID) %>% count() %>% arrange(desc(n))

ggplot(client_table[1:100,], aes(as.factor(reorder(CLNT_ID, -n)), n)) + geom_bar(stat = "identity")+theme_light() + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
ggplot(filter(client_table,n<500)[1:100,], aes(as.factor(reorder(CLNT_ID, -n)), n)) + geom_bar(stat = "identity")+theme_light() + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

filter(final, CLNT_ID == 4736937)


######  상품군별                                           ######  

top = mytable %>% arrange(desc(n)) 
ggplot(top[1:20,],aes(reorder(CLAC2_NM, -n),n)) + geom_bar(stat="identity") +theme_light() + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
top = mytable %>% arrange(desc(nn))
ggplot(top[1:20,],aes(reorder(CLAC2_NM, -nn),nn)) + geom_bar(stat="identity") +theme_light() + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

myCLAC_NM1 = mytable %>%filter(myCLAC_NM == "1") 
g1 = ggplot(myCLAC_NM1,aes(reorder(CLAC2_NM, -n),n)) + geom_bar(stat="identity") +theme_light() + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
g2 = ggplot(myCLAC_NM1,aes(reorder(CLAC2_NM, -nn),nn)) + geom_bar(stat="identity")+theme_light() + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
gridExtra::grid.arrange(g1,g2,ncol = 2)

myCLAC_NM2 = mytable %>%filter(myCLAC_NM == "2") 
g1 = ggplot(myCLAC_NM2,aes(reorder(CLAC2_NM, -n),n)) + geom_bar(stat="identity") +theme_light() + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
g2 = ggplot(myCLAC_NM2,aes(reorder(CLAC2_NM, -nn),nn)) + geom_bar(stat="identity")+theme_light() + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
gridExtra::grid.arrange(g1,g2,ncol = 2)

myCLAC_NM3 = mytable %>%filter(myCLAC_NM == "3") 
g1 = ggplot(myCLAC_NM3,aes(reorder(CLAC2_NM, -n),n)) + geom_bar(stat="identity") +theme_light() + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
g2 = ggplot(myCLAC_NM3,aes(reorder(CLAC2_NM, -nn),nn)) + geom_bar(stat="identity")+theme_light() + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
gridExtra::grid.arrange(g1,g2,ncol = 2)

myCLAC_NM4 = mytable %>%filter(myCLAC_NM == "4") 
g1 = ggplot(myCLAC_NM4,aes(reorder(CLAC2_NM, -n),n)) + geom_bar(stat="identity") +theme_light() + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
g2 = ggplot(myCLAC_NM4,aes(reorder(CLAC2_NM, -nn),nn)) + geom_bar(stat="identity")+theme_light() + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
gridExtra::grid.arrange(g1,g2,ncol = 2)





###### 상품군 탐색 소분류 알아보기                         ###### 
final %>% group_by(CLAC2_NM) %>% count() %>% print(n = length(final$CLAC2_NM)) %>% arrange(desc(CLAC2_NM))

### myCLAC_NM 별로 각각 살펴볼거야 
CLAC2 = final %>% group_by(myCLAC_NM, CLAC1_NM, CLAC2_NM) %>% count() 
CLAC3 = final %>% group_by(myCLAC_NM, CLAC1_NM, CLAC2_NM, CLAC3_NM) %>% count() 


###### 거시적 - 항목별                                     ######

myPD_BUY_TOT_0 = final %>% group_by(myCLAC_NM_name) %>% summarize(total = sum(myPD_BUY_TOT,na.rm =T)) %>% arrange(desc(total)) 
ggplot(myPD_BUY_TOT_0, aes(reorder(myCLAC_NM_name, -total), total))+geom_bar(stat="identity")+theme_light() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) 

myPD_BUY_TOT_0 = final %>% group_by(myCLAC_NM_name) %>% summarize(total = sum(PD_BUY_CT,na.rm =T)) %>% arrange(desc(total)) 
ggplot(myPD_BUY_TOT_0, aes(reorder(myCLAC_NM_name, -total), total))+geom_bar(stat="identity")+theme_light() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) 

myPD_BUY_TOT_3 = filter(final, myCLAC_NM == "3") %>% group_by(CLAC2_NM) %>% summarize(total = sum(PD_BUY_CT,na.rm =T)) %>% arrange(desc(total)) 
ggplot(myPD_BUY_TOT_3, aes(reorder(CLAC2_NM, -total), total))+geom_bar(stat="identity")+theme_light() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) 

# total amount
myPD_BUY_TOT_3 = filter(final, myCLAC_NM == "3") %>% group_by(CLAC2_NM) %>% summarize(total = sum(myPD_BUY_TOT,na.rm =T)) %>% arrange(desc(total)) 
ggplot(myPD_BUY_TOT_3, aes(reorder(CLAC2_NM, -total), total))+geom_bar(stat="identity")+theme_light() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) 

# each amount
myPD_BUY_TOT_3 = filter(final, myCLAC_NM == "3") %>% group_by(CLAC2_NM) %>% summarize(total = mean(myPD_BUY_TOT,na.rm =T)) %>% arrange(desc(total)) 
ggplot(myPD_BUY_TOT_3, aes(reorder(CLAC2_NM, -total), total))+geom_bar(stat="identity")+theme_light() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) 

myPD_BUY_TOT_3 = filter(final, myCLAC_NM == "3") %>% group_by(CLAC2_NM) %>% count() %>% arrange(desc(n)) 
ggplot(myPD_BUY_TOT_3, aes(reorder(CLAC2_NM, -n), n))+geom_bar(stat="identity")+theme_light() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) 


# 가구
myPD_BUY_TOT_4 = filter(final, myCLAC_NM == "4") %>% group_by(CLAC2_NM) %>% summarize(total = sum(PD_BUY_CT,na.rm =T)) %>% arrange(desc(total)) 
ggplot(myPD_BUY_TOT_4, aes(reorder(CLAC2_NM, -total), total))+geom_bar(stat="identity")+theme_light() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) 

# 애완 
myPD_BUY_TOT_10 = filter(final, myCLAC_NM == "10") %>% group_by(CLAC2_NM) %>% summarize(total = sum(PD_BUY_CT,na.rm =T)) %>% arrange(desc(total)) 
ggplot(myPD_BUY_TOT_10, aes(reorder(CLAC2_NM, -total), total))+geom_bar(stat="identity")+theme_light() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) 









###### 비회원                                              ##########################################################################################################


nonmember =  final[is.na(final$CLNT_GENDER),] %>% group_by(CLNT_ID, PD_C)# %>% unique()  # 비회원
nrow(nonmember)/nrow(final)   # 비회원 구매가 전체의 20 % 
non_rebuy =  nonmember %>% group_by(CLNT_ID,PD_C) %>% count() 
table2 = data.frame("Var" = data.frame(table(non_rebuy$n))$Var1, "Freq" = data.frame(table(non_rebuy$n))$Freq,
                    "gubun" = rep(2, nrow(table(non_rebuy$n))))


sum(table1$Freq)/ ( final %>% group_by(CLNT_ID) %>% count() )


join_table = full_join(table1,table2)
ggplot(join_table, aes(x = Freq)) + geom_histogram(fill = "white", color = "black", bins =5) + facet_grid(gubun ~ .)
# 회원인 사람 중 재구매율이 극단치로 큰 경우 발생 
ggplot(filter(join_table, Var <1000000), aes(x = Freq)) + geom_histogram(fill = "white", color = "black", bins =5) + facet_grid(gubun ~ .)

### 한 페이지당 평균 뷰시간 vs 구매총액 **** 오래걸림      ##############################################

ggplot(final) + geom_point(aes(x = myEACH_SESS_HR_V, y = myPD_BUY_TOT))
# 금액이 작을 수록 한 페이지당 평균 뷰시간 커지고 
# 금액이 클 수록 한 페이지당 평균 뷰시간 작아진다



### 성별                                                   #############################################################################################################
# 성별&나이대 F30 F40 가장 큼 
# 구매액 
target = final %>% group_by(CLNT_GENDER, CLNT_AGE) %>% summarize(total = sum(PD_BUY_CT,na.rm =T)) %>% arrange(desc(total)) %>% 
  mutate(GENDER_AGE = paste(CLNT_GENDER,CLNT_AGE,sep = "" ))
ggplot(target,aes(reorder(GENDER_AGE, -total),total)) + geom_bar(stat="identity") +theme_light()
# 구매건수 
target = final %>% group_by(CLNT_GENDER, CLNT_AGE) %>% summarize(total = sum(myPD_BUY_TOT,na.rm =T)) %>% arrange(desc(total)) %>% 
  mutate(GENDER_AGE = paste(CLNT_GENDER,CLNT_AGE,sep = "" ))
ggplot(target,aes(reorder(GENDER_AGE, -total),total)) + geom_bar(stat="identity")+theme_light()


## 상품 하나당
# 구매량 성별 연령별 분포
target = final %>% group_by(CLNT_GENDER, myCLNT_AGE) %>% summarize(each_price = mean(myPD_BUY_TOT,na.rm =T)) %>% arrange(desc(each_price)) %>% 
  mutate(GENDER_AGE = paste(CLNT_GENDER,myCLNT_AGE,sep = "" ))
ggplot(target,aes(reorder(GENDER_AGE, -each_price),each_price)) + geom_bar(stat="identity") +theme_light()
# 구매건수 성별 연령별 분포
target = final %>% group_by(CLNT_GENDER, CLNT_AGE) %>% summarize(each_price = mean(PD_BUY_AM,na.rm =T)) %>% arrange(desc(each_price)) %>% 
  mutate(GENDER_AGE = paste(CLNT_GENDER,CLNT_AGE,sep = "" ))
ggplot(target,aes(reorder(GENDER_AGE, -each_price),each_price)) + geom_bar(stat="identity")+theme_light()    

## 여성, 남성 소분류 기준 항목별 총 구매액의 top 10
all = final %>% group_by(CLAC2_NM) %>% summarize(total = sum(myPD_BUY_TOT,na.rm =T)) %>% arrange(desc(total)) %>% print(n = 10)
male = final %>% filter(CLNT_GENDER =="M")%>% group_by(CLAC2_NM) %>% summarize(total = sum(myPD_BUY_TOT,na.rm =T)) %>% 
  arrange(desc(total)) %>% print(n = 10) 
female = final %>% filter(CLNT_GENDER =="F")%>% group_by(CLAC2_NM) %>% summarize(total = sum(myPD_BUY_TOT,na.rm =T)) %>% 
  arrange(desc(total)) %>% print(n = 10) 

g1 =ggplot(all[1:10,], aes(reorder(CLAC2_NM, -total), total))+geom_bar(stat="identity")+theme_light() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + ggtitle("ALL top 10")
g2 =ggplot(male[1:10,], aes(reorder(CLAC2_NM, -total), total))+geom_bar(stat="identity")+theme_light() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + ggtitle("Male top 10")
g3 =ggplot(female[1:10,], aes(reorder(CLAC2_NM, -total), total))+geom_bar(stat="identity")+theme_light() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + ggtitle("Female top 10")

# 여성, 남성 소분류 기준 항목별 총 구매건수의 top 10
all = final %>% group_by(CLAC2_NM) %>% summarize(total = sum(PD_BUY_CT,na.rm =T)) %>% arrange(desc(total)) %>% print(n = 10)
male = final %>% filter(CLNT_GENDER =="M")%>% group_by(CLAC2_NM) %>% summarize(total = sum(PD_BUY_CT,na.rm =T)) %>% 
  arrange(desc(total)) %>% print(n = 10) 
female = final %>% filter(CLNT_GENDER =="F")%>% group_by(CLAC2_NM) %>% summarize(total = sum(PD_BUY_CT,na.rm =T)) %>% 
  arrange(desc(total)) %>% print(n = 10) 

g4 =ggplot(all[1:10,], aes(reorder(CLAC2_NM, -total), total))+geom_bar(stat="identity")+theme_light() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + ggtitle("ALL count top 10")
g5 =ggplot(male[1:10,], aes(reorder(CLAC2_NM, -total), total))+geom_bar(stat="identity")+theme_light() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + ggtitle("Male count top 10")
g6 =ggplot(female[1:10,], aes(reorder(CLAC2_NM, -total), total))+geom_bar(stat="identity")+theme_light() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + ggtitle("Female count top 10")

gridExtra::grid.arrange(g1,g2,g3,g4,g5,g6,nrow = 3, as.table=F)
# 총 구매액을 볼것인가 총 구매건수를 볼것인가 
# 총 구매액을 보게 될 경우, 상품 하나의 가격이 매우 크면 전체 금액이 커지기 때문에 비교할 때 문제 발생할 수 있음 
# 구매액을 무시하고 총 구매건수를 볼 경우 상품 가격이 무시됨,, 

# 남자와 여자의 평균 
final %>% group_by(CLNT_GENDER) %>% summarize(each_price = mean(PD_BUY_AM,na.rm =T)) %>% arrange(desc(each_price))
sum(is.na(final$TOT_PAG_VIEW_CT))


### 성별과 연령대별 세션 일련번호 분포 즉 관심도 정도가 될듯 ?
ggplot(filter(final,SESS_SEQ <1000),aes(SESS_SEQ))+geom_histogram(bins = 30)+
  facet_grid(CLNT_GENDER~CLNT_AGE)+theme_bw() + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
ggplot(filter(final,SESS_SEQ >4000),aes(SESS_SEQ))+geom_histogram(bins = 30)+
  facet_grid(CLNT_GENDER~CLNT_AGE)+theme_bw() + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
# 세션 일련번호가 5000 이상인 고객들은 30,40,50대 여성뿐 -> big fan 

### 성별, 연령별, 월별                                     #####################################

# temp = final %>% filter(CLNT_GENDER=="F" & CLNT_AGE ==30)
temp = final[!is.na(final$CLNT_GENDER),] # 회원 
temp = final[is.na(final$CLNT_GENDER),]  # 비회원 
month4 = filter(temp, substr(SESS_DT,6,7)=="04")
month5 = filter(temp, substr(SESS_DT,6,7)=="05")
month6 = filter(temp, substr(SESS_DT,6,7)=="06")
month7 = filter(temp, substr(SESS_DT,6,7)=="07")
month8 = filter(temp, substr(SESS_DT,6,7)=="08")
month9 = filter(temp, substr(SESS_DT,6,7)=="09")

# 구매액 top 15
tt4 = month4 %>% group_by(CLAC2_NM) %>% summarize(total = sum(myPD_BUY_TOT,na.rm =T)) %>% arrange(desc(total)) %>% print(n = 15)
tt5 = month5 %>% group_by(CLAC2_NM) %>% summarize(total = sum(myPD_BUY_TOT,na.rm =T)) %>% arrange(desc(total)) %>% print(n = 15)
tt6 = month6 %>% group_by(CLAC2_NM) %>% summarize(total = sum(myPD_BUY_TOT,na.rm =T)) %>% arrange(desc(total)) %>% print(n = 15)
tt7 = month7 %>% group_by(CLAC2_NM) %>% summarize(total = sum(myPD_BUY_TOT,na.rm =T)) %>% arrange(desc(total)) %>% print(n = 15)
tt8 = month8 %>% group_by(CLAC2_NM) %>% summarize(total = sum(myPD_BUY_TOT,na.rm =T)) %>% arrange(desc(total)) %>% print(n = 15)
tt9 = month9 %>% group_by(CLAC2_NM) %>% summarize(total = sum(myPD_BUY_TOT,na.rm =T)) %>% arrange(desc(total)) %>% print(n = 15)
t4 = ggplot(tt4[1:15,], aes(reorder(CLAC2_NM, -total), total))+geom_bar(stat="identity")+theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + ggtitle("4월 총 구매액 top 15")
t5 = ggplot(tt5[1:15,], aes(reorder(CLAC2_NM, -total), total))+geom_bar(stat="identity")+theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + ggtitle("5월 총 구매액 top 15")
t6 = ggplot(tt6[1:15,], aes(reorder(CLAC2_NM, -total), total))+geom_bar(stat="identity")+theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + ggtitle("6월 총 구매액 top 15")
t7 = ggplot(tt7[1:15,], aes(reorder(CLAC2_NM, -total), total))+geom_bar(stat="identity")+theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + ggtitle("7월 총 구매액 top 15")
t8 = ggplot(tt8[1:15,], aes(reorder(CLAC2_NM, -total), total))+geom_bar(stat="identity")+theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + ggtitle("8월 총 구매액 top 15")
t9 = ggplot(tt9[1:15,], aes(reorder(CLAC2_NM, -total), total))+geom_bar(stat="identity")+theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + ggtitle("9월 총 구매액 top 15")

gridExtra::grid.arrange(t4,t5,t6,t7,t8,t9,nrow = 3)

# 구매건수 top 15
tt4 = month4 %>% group_by(CLAC2_NM) %>% summarize(total = sum(PD_BUY_CT,na.rm =T)) %>% arrange(desc(total)) %>% print(n = 15)
tt5 = month5 %>% group_by(CLAC2_NM) %>% summarize(total = sum(PD_BUY_CT,na.rm =T)) %>% arrange(desc(total)) %>% print(n = 15)
tt6 = month6 %>% group_by(CLAC2_NM) %>% summarize(total = sum(PD_BUY_CT,na.rm =T)) %>% arrange(desc(total)) %>% print(n = 15)
tt7 = month7 %>% group_by(CLAC2_NM) %>% summarize(total = sum(PD_BUY_CT,na.rm =T)) %>% arrange(desc(total)) %>% print(n = 15)
tt8 = month8 %>% group_by(CLAC2_NM) %>% summarize(total = sum(PD_BUY_CT,na.rm =T)) %>% arrange(desc(total)) %>% print(n = 15)
tt9 = month9 %>% group_by(CLAC2_NM) %>% summarize(total = sum(PD_BUY_CT,na.rm =T)) %>% arrange(desc(total)) %>% print(n = 15)
t4 = ggplot(tt4[1:15,], aes(reorder(CLAC2_NM, -total), total))+geom_bar(stat="identity")+theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + ggtitle("4월 총 구매건수 top 15")
t5 = ggplot(tt5[1:15,], aes(reorder(CLAC2_NM, -total), total))+geom_bar(stat="identity")+theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + ggtitle("5월 총 구매건수 top 15")
t6 = ggplot(tt6[1:15,], aes(reorder(CLAC2_NM, -total), total))+geom_bar(stat="identity")+theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + ggtitle("6월 총 구매건수 top 15")
t7 = ggplot(tt7[1:15,], aes(reorder(CLAC2_NM, -total), total))+geom_bar(stat="identity")+theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + ggtitle("7월 총 구매건수 top 15")
t8 = ggplot(tt8[1:15,], aes(reorder(CLAC2_NM, -total), total))+geom_bar(stat="identity")+theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + ggtitle("8월 총 구매건수 top 15")
t9 = ggplot(tt9[1:15,], aes(reorder(CLAC2_NM, -total), total))+geom_bar(stat="identity")+theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + ggtitle("9월 총 구매건수 top 15")

gridExtra::grid.arrange(t4,t5,t6,t7,t8,t9,nrow = 3)




###### 기기유형별                                          ######
type = final %>% group_by(DVC_CTG_NM) %>% summarize(total = sum(PD_BUY_CT,na.rm =T)) %>% arrange(desc(total)) %>% print(n = 10)
ggplot(type, aes(reorder(DVC_CTG_NM, -total), total))+geom_bar(stat="identity")+theme_light() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + ggtitle("type count")

######지역별                                               ######
zon = final %>% group_by(ZON_NM) %>% summarize(total = sum(PD_BUY_CT,na.rm =T)) %>% arrange(desc(total)) %>% print(n = 10)
ggplot(zon, aes(reorder(ZON_NM, -total), total))+geom_bar(stat="identity")+theme_light() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + ggtitle("zon count")

city = final %>% group_by(CITY_NM) %>% summarize(total = sum(PD_BUY_CT,na.rm =T)) %>% arrange(desc(total)) %>% print(n = 10)
ggplot(city[1:80,], aes(reorder(CITY_NM, -total), total))+geom_bar(stat="identity")+theme_light() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + ggtitle("city count")


### 10 대분류 각각의 월별 구매액                           ####################################################################

# 총 구매액 
tt4 = filter(final, myCLAC_NM == "10",substr(SESS_DT,6,7)=="04" ) %>% group_by(CLAC2_NM) %>% summarize(total = sum(myPD_BUY_TOT,na.rm =T)) %>% arrange(desc(total))
tt5 = filter(final, myCLAC_NM == "10",substr(SESS_DT,6,7)=="05" ) %>% group_by(CLAC2_NM) %>% summarize(total = sum(myPD_BUY_TOT,na.rm =T)) %>% arrange(desc(total))
tt6 = filter(final, myCLAC_NM == "10",substr(SESS_DT,6,7)=="06" ) %>% group_by(CLAC2_NM) %>% summarize(total = sum(myPD_BUY_TOT,na.rm =T)) %>% arrange(desc(total))
tt7 = filter(final, myCLAC_NM == "10",substr(SESS_DT,6,7)=="07" ) %>% group_by(CLAC2_NM) %>% summarize(total = sum(myPD_BUY_TOT,na.rm =T)) %>% arrange(desc(total))
tt8 = filter(final, myCLAC_NM == "10",substr(SESS_DT,6,7)=="08" ) %>% group_by(CLAC2_NM) %>% summarize(total = sum(myPD_BUY_TOT,na.rm =T)) %>% arrange(desc(total))
tt9 = filter(final, myCLAC_NM == "10",substr(SESS_DT,6,7)=="09" ) %>% group_by(CLAC2_NM) %>% summarize(total = sum(myPD_BUY_TOT,na.rm =T)) %>% arrange(desc(total))
t4 = ggplot(tt4, aes(reorder(CLAC2_NM, -total), total))+geom_bar(stat="identity")+theme_bw() +ylim(c(0,300000000)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + ggtitle("4월 총 구매액 top 15")
t5 = ggplot(tt5, aes(reorder(CLAC2_NM, -total), total))+geom_bar(stat="identity")+theme_bw() +ylim(c(0,300000000)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + ggtitle("5월 총 구매액 top 15")
t6 = ggplot(tt6, aes(reorder(CLAC2_NM, -total), total))+geom_bar(stat="identity")+theme_bw() +ylim(c(0,300000000)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + ggtitle("6월 총 구매액 top 15")
t7 = ggplot(tt7, aes(reorder(CLAC2_NM, -total), total))+geom_bar(stat="identity")+theme_bw() +ylim(c(0,300000000)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + ggtitle("7월 총 구매액 top 15")
t8 = ggplot(tt8, aes(reorder(CLAC2_NM, -total), total))+geom_bar(stat="identity")+theme_bw() +ylim(c(0,300000000)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + ggtitle("8월 총 구매액 top 15")
t9 = ggplot(tt9, aes(reorder(CLAC2_NM, -total), total))+geom_bar(stat="identity")+theme_bw() +ylim(c(0,300000000)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + ggtitle("9월 총 구매액 top 15")

gridExtra::grid.arrange(t4,t5,t6,t7,t8,t9,nrow = 3)


# 총 구매건수 
tt4 = filter(final, myCLAC_NM == "10",substr(SESS_DT,6,7)=="04" ) %>% group_by(CLAC2_NM) %>% summarize(total = sum(PD_BUY_CT,na.rm =T)) %>% arrange(desc(total))
tt5 = filter(final, myCLAC_NM == "10",substr(SESS_DT,6,7)=="05" ) %>% group_by(CLAC2_NM) %>% summarize(total = sum(PD_BUY_CT,na.rm =T)) %>% arrange(desc(total))
tt6 = filter(final, myCLAC_NM == "10",substr(SESS_DT,6,7)=="06" ) %>% group_by(CLAC2_NM) %>% summarize(total = sum(PD_BUY_CT,na.rm =T)) %>% arrange(desc(total))
tt7 = filter(final, myCLAC_NM == "10",substr(SESS_DT,6,7)=="07" ) %>% group_by(CLAC2_NM) %>% summarize(total = sum(PD_BUY_CT,na.rm =T)) %>% arrange(desc(total))
tt8 = filter(final, myCLAC_NM == "10",substr(SESS_DT,6,7)=="08" ) %>% group_by(CLAC2_NM) %>% summarize(total = sum(PD_BUY_CT,na.rm =T)) %>% arrange(desc(total))
tt9 = filter(final, myCLAC_NM == "10",substr(SESS_DT,6,7)=="09" ) %>% group_by(CLAC2_NM) %>% summarize(total = sum(PD_BUY_CT,na.rm =T)) %>% arrange(desc(total))
t4 = ggplot(tt4, aes(reorder(CLAC2_NM, -total), total))+geom_bar(stat="identity")+theme_bw() +ylim(c(0,25000)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + ggtitle("4월 총 구매건수 ")
t5 = ggplot(tt5, aes(reorder(CLAC2_NM, -total), total))+geom_bar(stat="identity")+theme_bw() +ylim(c(0,25000)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + ggtitle("5월 총 구매건수 ")
t6 = ggplot(tt6, aes(reorder(CLAC2_NM, -total), total))+geom_bar(stat="identity")+theme_bw() +ylim(c(0,25000)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + ggtitle("6월 총 구매건수 ")
t7 = ggplot(tt7, aes(reorder(CLAC2_NM, -total), total))+geom_bar(stat="identity")+theme_bw() +ylim(c(0,25000)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + ggtitle("7월 총 구매건수 ")
t8 = ggplot(tt8, aes(reorder(CLAC2_NM, -total), total))+geom_bar(stat="identity")+theme_bw() +ylim(c(0,25000)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + ggtitle("8월 총 구매건수 ")
t9 = ggplot(tt9, aes(reorder(CLAC2_NM, -total), total))+geom_bar(stat="identity")+theme_bw() +ylim(c(0,25000)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + ggtitle("9월 총 구매건수 ")

gridExtra::grid.arrange(t4,t5,t6,t7,t8,t9,nrow = 3)


### 연관성분석 ###                                         ##########################################################################################################



library(arules)
# data.frame을 id별 list로 쪼갠 뒤 transaction 타입으로 변환해 줘야 연관성분석 가능 
F30 = final %>% filter(myMONTH == 4, CLNT_GENDER=="M" & CLNT_AGE ==20)
rioter.list<-split(F30$PD_C, F30$CLNT_ID)
rioter.list = unique(rioter.list)

rioter.transaction<-as(rioter.list, "transactions")
as.transactions(rioter.list)
rioter.transaction

# 규칙 생성 : 지지도(support),신뢰도(confidence), 향상도(lift) 
rules = apriori(rioter.transaction)
summary(rules)

# 규칙을 상세히 보기 위해서 inspect 함수 사용
rule.list<-as.data.frame(inspect(rules))
rule.list<-rule.list[order(rule.list$lift, decreasing=TRUE), ] # 향상도 순으로 정렬 
rule.list

# ### correlation matrix plot 걍한번해봄 
#     temp = select(final, HITS_SEQ, PD_BUY_AM, PD_BUY_CT, CLNT_AGE)
#     M = temp[complete.cases(temp),]
#     M = cor(M)
#     head(round(M,2))
#     library(corrplot)
#     corrplot(M, type="upper",  tl.col="black", tl.srt=45, method="number")

########

#######               
# 대분류별로 다시 보자 
# large = final_index_table %>% filter(CLAC1_NM == "남성의류"|CLAC1_NM == "여성의류") 
# g1_1=ggplot(data=large , aes(x=myWEEK, y=FACTOR, colour=CLAC2_NM, group=CLAC2_NM)) + theme_light() + geom_line(size = 1.5) +
#   geom_point(size=1)+ ggtitle("대분류별 선호지수|1-패션의류")+ylab("선호지수")+xlab("단위:주")+ theme(axis.text.x = element_text(angle = 45, hjust = 1))
# large = final_index_table %>% filter(CLAC1_NM == "속옷/양말/홈웨어"|CLAC1_NM == "스포츠패션")
# g1_2=ggplot(data=large , aes(x=myWEEK, y=FACTOR, colour=CLAC2_NM, group=CLAC2_NM)) + theme_light() + geom_line(size = 1.5) +
#   geom_point(size=1)+ ggtitle("대분류별 선호지수|1-속옷양말홈웨어스포츠")+ylab("선호지수")+xlab("단위:주")+ theme(axis.text.x = element_text(angle = 45, hjust = 1))
# large = final_index_table %>% filter(CLAC1_NM == "패션잡화") 
# g1_3=ggplot(data=large , aes(x=myWEEK, y=FACTOR, colour=CLAC2_NM, group=CLAC2_NM)) + theme_light() + geom_line(size = 1.5) +
#   geom_point(size=1)+ ggtitle("대분류별 선호지수|1-패션잡화")+ylab("선호지수")+xlab("단위:주")+ theme(axis.text.x = element_text(angle = 45, hjust = 1))
# large = final_index_table %>% filter(myCLAC_NM == "2") 
# g2 = ggplot(data=large , aes(x=myWEEK, y=FACTOR, colour=CLAC2_NM, group=CLAC2_NM)) + theme_light() + geom_line(size = 1.5) +
#   geom_point(size=1)+ ggtitle("대분류별 선호지수|2-식품/농수산물")+ylab("선호지수")+xlab("단위:주")+ theme(axis.text.x = element_text(angle = 45, hjust = 1))
# large = final_index_table %>% filter(myCLAC_NM == "3") 
# g3 = ggplot(data=large , aes(x=myWEEK, y=FACTOR, colour=CLAC2_NM, group=CLAC2_NM)) + theme_light() + geom_line(size = 1.5) +
#   geom_point(size=1)+ ggtitle("대분류별 선호지수|3-생활/자동차용품")+ylab("선호지수")+xlab("단위:주")+ theme(axis.text.x = element_text(angle = 45, hjust = 1))
# large = final_index_table %>% filter(myCLAC_NM == "4") 
# g4 = ggplot(data=large , aes(x=myWEEK, y=FACTOR, colour=CLAC2_NM, group=CLAC2_NM)) + theme_light() + geom_line(size = 1.5) +
#   geom_point(size=1)+ ggtitle("대분류별 선호지수|4-가구")+ylab("선호지수")+xlab("단위:주")+ theme(axis.text.x = element_text(angle = 45, hjust = 1))
# large = final_index_table %>% filter(myCLAC_NM == "5") 
# g5 = ggplot(data=large , aes(x=myWEEK, y=FACTOR, colour=CLAC2_NM, group=CLAC2_NM)) + theme_light() + geom_line(size = 1.5) +
#   geom_point(size=1)+ ggtitle("대분류별 선호지수|5-가전/컴퓨터/통신기기")+ylab("선호지수")+xlab("단위:주")+ theme(axis.text.x = element_text(angle = 45, hjust = 1))
# large = final_index_table %>% filter(myCLAC_NM == "6") 
# g6 = ggplot(data=large , aes(x=myWEEK, y=FACTOR, colour=CLAC2_NM, group=CLAC2_NM)) + theme_light() + geom_line(size = 1.5) +
#   geom_point(size=1)+ ggtitle("대분류별 선호지수|6-화장품")+ylab("선호지수")+xlab("단위:주")+ theme(axis.text.x = element_text(angle = 45, hjust = 1))
# large = final_index_table %>% filter(myCLAC_NM == "7") 
# g7 = ggplot(data=large , aes(x=myWEEK, y=FACTOR, colour=CLAC2_NM, group=CLAC2_NM)) + theme_light() + geom_line(size = 1.5) +
#   geom_point(size=1)+ ggtitle("대분류별 선호지수|7-아동/유아용품")+ylab("선호지수")+xlab("단위:주")+ theme(axis.text.x = element_text(angle = 45, hjust = 1))
# large = final_index_table %>% filter(myCLAC_NM == "8") 
# g8 = ggplot(data=large , aes(x=myWEEK, y=FACTOR, colour=CLAC2_NM, group=CLAC2_NM)) + theme_light() + geom_line(size = 1.5) +
#   geom_point(size=1)+ ggtitle("대분류별 선호지수|8-스포츠/레저용품")+ylab("선호지수")+xlab("단위:주")+ theme(axis.text.x = element_text(angle = 45, hjust = 1))
# large = final_index_table %>% filter(myCLAC_NM == "9") 
# g9 = ggplot(data=large , aes(x=myWEEK, y=FACTOR, colour=CLAC2_NM, group=CLAC2_NM)) + theme_light() + geom_line(size = 1.5) +
#   geom_point(size=1)+ ggtitle("대분류별 선호지수|9-사무/문구")+ylab("선호지수")+xlab("단위:주")+ theme(axis.text.x = element_text(angle = 45, hjust = 1))
# large = final_index_table %>% filter(myCLAC_NM == "10") 
# g10 = ggplot(data=large , aes(x=myWEEK, y=FACTOR, colour=CLAC2_NM, group=CLAC2_NM)) + theme_light() + geom_line(size = 1.5) +
#   geom_point(size=1)+ ggtitle("대분류별 선호지수|10-애완용품")+ylab("선호지수")+xlab("단위:주")+ theme(axis.text.x = element_text(angle = 45, hjust = 1))

















