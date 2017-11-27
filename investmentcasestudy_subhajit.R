



library(gdata)
library(dplyr)
library(tidyr)
library(magrittr)

#checkpoint 1: Data cleaning

companies<-read.delim("companies.txt",header = TRUE,stringsAsFactors = FALSE, na.strings = c(""))
rounds2<- read.csv("rounds2.csv",stringsAsFactors = FALSE, na.strings = c(""))
rounds2$company_permalink<-tolower(rounds2$company_permalink)
companies$permalink<-tolower(companies$permalink)

un_comp_companies <-length(unique(companies$permalink))
un_comp_rounds2<-length(unique(rounds2$company_permalink))

length(which(!rounds2$company_permalink %in% companies$permalink))

master_frame<- merge(rounds2,companies,by.x ="company_permalink",by.y ="permalink")



#checkpoint 2: Funding Type Analysis


avg_fund_amt<- master_frame %>% select(company_permalink,funding_round_permalink,funding_round_type,raised_amount_usd)


avg_fund_amt_ven<-avg_fund_amt %>% filter(funding_round_type=="venture") %>% summarise(mean(raised_amount_usd,na.rm=TRUE))

avg_fund_amt_angel<-avg_fund_amt %>% filter(funding_round_type=="angel") %>% summarise(mean(raised_amount_usd,na.rm=TRUE))

avg_fund_amt_seed<-avg_fund_amt %>% filter(funding_round_type=="seed") %>% summarise(mean(raised_amount_usd,na.rm=TRUE))

avg_fund_amt_pvteqty<-avg_fund_amt %>% filter(funding_round_type=="private_equity") %>% summarise(mean(raised_amount_usd,na.rm=TRUE))


#checkPoint 3 : country analysis


#contries<- master_frame %>%select(company_permalink,funding_round_permalink,funding_round_type,country_code,raised_amount_usd)

top9<- master_frame %>% select(country_code,raised_amount_usd)%>% group_by(country_code)%>% summarise(total=sum(raised_amount_usd,na.rm= TRUE))%>% arrange(desc(total))

top9<-head(na.omit(top9[order(-top9$total),]),9)

#checkpoint 4: sector analysis 1

map<- read.csv("mapping.csv",check.names = FALSE ,stringsAsFactors = FALSE, na.strings = c("")) %>% select(-Blanks)

colnames(map)<-c("category_list","Automotive_Sport","Cleantech_Semiconductors",
                 "Entertainment","Health","Manufacturing","News_Search_Messaging","Others","Social_Finance_Analytics_Advertising")

str(map)

map <- map %>% gather(key=main_sector,value=is_sector,Automotive_Sport:Social_Finance_Analytics_Advertising) 

mapping <- map[-which(map$is_sector==0),]

master_frame<-separate(master_frame,category_list,into=c("primary_sector","secondary_sector","x","y","z"),sep="\\|")

df<- merge(master_frame,mapping,by.x="primary_sector",by.y="category_list",all.x = TRUE)

str(df)

ult_master_frame<- df %>% select(company_permalink:homepage_url,is_sector,main_sector,primary_sector,secondary_sector:founded_at)


#checkpoint 5: sector analysic 2

is.between <- function(x, a, b) {((x - a)*(b - x))>=0 } #including the range values

##--all about D1--
D1<- ult_master_frame %>% filter(funding_round_type== "venture" & country_code=="USA" & is.between(raised_amount_usd,5000000,15000000))


str(D1)
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                               
length(which(!is.na(D1$raised_amount_usd)))

D1_total_amount<- D1%>%summarise(total_amount=sum(raised_amount_usd,na.rm=TRUE))

D1_inv_count<-D1 %>% group_by(main_sector)%>% summarise(investment_count=n(),na.rm=TRUE)%>% arrange(desc(investment_count))

D1_high_inv<- D1 %>% filter(main_sector=="Others") %>% group_by(name) %>% summarise(highest_inv=sum(raised_amount_usd,na.rm=TRUE))%>% arrange(desc(highest_inv))

D1_2nd_sector_high_inv<-D1 %>% filter(main_sector=="Social_Finance_Analytics_Advertising") %>% group_by(name) %>% summarise(highest_inv=sum(raised_amount_usd,na.rm=TRUE))%>% arrange(desc(highest_inv))


##--all about D2--

D2<- ult_master_frame %>% filter(funding_round_type== "venture" & country_code=="GBR" & is.between(raised_amount_usd,5000000,15000000))

length(which(!is.na(D2$raised_amount_usd)))

D2_total_amount<- D2%>%summarise(total_amount=sum(raised_amount_usd,na.rm=TRUE))

D2_inv_count<-D2 %>% group_by(main_sector)%>% summarise(investment_count=n(),na.rm=TRUE)%>% arrange(desc(investment_count))

D2_high_inv<- D2 %>% filter(main_sector=="Others") %>% group_by(name) %>% summarise(highest_inv=sum(raised_amount_usd,na.rm=TRUE))%>% arrange(desc(highest_inv))

D2_2nd_sector_high_inv<-D2 %>% filter(main_sector=="Social_Finance_Analytics_Advertising") %>% group_by(name) %>% summarise(highest_inv=sum(raised_amount_usd,na.rm=TRUE))%>% arrange(desc(highest_inv))


##--all about D3--

D3<- ult_master_frame %>% filter(funding_round_type== "venture" & country_code=="IND" & is.between(raised_amount_usd,5000000,15000000))

length(which(!is.na(D3$raised_amount_usd)))

D3_total_amount<- D3%>%summarise(total_amount=sum(raised_amount_usd,na.rm=TRUE))

D3_inv_count<-D3 %>% group_by(main_sector)%>% summarise(investment_count=n(),na.rm=TRUE)%>% arrange(desc(investment_count))


D3_high_inv<- D3 %>% filter(main_sector=="Others") %>% group_by(name) %>% summarise(highest_inv=sum(raised_amount_usd,na.rm=TRUE))%>% arrange(desc(highest_inv))

D3_2nd_sector_high_inv<-D3 %>% filter(main_sector=="Social_Finance_Analytics_Advertising") %>% group_by(name) %>% summarise(highest_inv=sum(raised_amount_usd,na.rm=TRUE))%>% arrange(desc(highest_inv))




