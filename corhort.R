library(dplyr)
library(data.table)
library(reshape2)
library(stringr)
library(ggplot2)
test<-read.csv('F:/relay-foods.csv',stringsAsFactors = F,encoding = 'gbk')
str(test)
test$OrderDate<-as.Date(test$OrderDate)
test$OrderPeriod<-str_sub(test$OrderDate,1,7)
test$TotalChargesNum<-as.numeric(str_extract(test$TotalCharges,'\\d+\\.\\d+'))
test<-data.table(test)
#按照userId，来确定期限
test.u<-test %>%
  group_by(UserId) %>%
  mutate(CohortGroup =min(OrderPeriod))
test.u<-data.table(test.u)
#unique(test.u[,.(UserId,CohortGroup)])
cohorts<-test.u[,.(TC=sum(TotalChargesNum),OrderN=length(unique(OrderId)),UserN=length(unique(UserId))),by=.(CohortGroup,OrderPeriod)]

#计算留存率
p<-function(df){
  df<-df[order(df$OrderPeriod),]
  t<-df %>%
    group_by(CohortGroup) %>%
    mutate(t.min=first(UserN)) %>%
    mutate(retention=UserN/t.min) %>%
    mutate(CohortPeriod=0:(length(OrderPeriod)-1))
  return(data.table(t))
}

final<- data.table(cohorts %>%
  group_by(CohortGroup) %>%
  p())

#留存率曲线
ggplot(final[CohortGroup %in% c('2009-06', '2009-07', '2009-08')],aes(x=CohortPeriod,y=retention,group=CohortGroup))+
  geom_line(aes(color=CohortGroup),size=1.2)
#总人数
final.n<-final[,.(users=sum(UserN)),by=.(CohortGroup)]

#留存率表格
final$retention<-round(final$retention,2)
final$retention.ratio<-paste0(round(final$retention,2)*100,'%')
p1<-dcast(final,CohortGroup~CohortPeriod,value.var = 'retention')
#合并留存率表格
final.final<-left_join(final.n,p1,by='CohortGroup')

#热力图 因子化
final$CohortGroup<-factor(final$CohortGroup)
ggplot(final,aes(x=CohortPeriod,y=CohortGroup,fill=retention))+
  geom_tile(size=0.2) + geom_text(aes(label=retention.ratio)) +
  scale_fill_continuous(low='#FFF0F5', high='#800080') +
  scale_y_discrete(limits=rev(levels(final$CohortGroup)))

data("iris")
