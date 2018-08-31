
setwd("~/trump_rtweet")
library(readr)
dft=read_lines('trump_pos.txt')
# make dataframe to build up columns for downstream analysis
dft=tibble::tibble(text=dft)
library(stringr)
library(purrr)
library(dplyr)
#add mutated column with split text by tab (really only care about pos, 2nd index)
dft=dft %>% mutate(spl=str_split(text,"\t"))

# write function to map over list
#extract 2nd feature, the POS tags, split them by space,
#returns a count of each POS as a total tibble
ppos=function(l){
  pos=l[2]
  res=strsplit(pos," ")[[1]]
  dft=data.frame(pos=res)
  dft=count(dft,pos)
}

library(tidyr)
# adds a tibble column, mapping over split text
dft=dft %>% mutate(pos=map(dft$spl,ppos))
#res_cmb=bind_rows(res)
#res_sum=res_cmb %>% group_by(pos) %>%
#  summarise(n=sum(n))


#rtweet data as a binary file
df=read_rds("rtweet_finalish_051618.rds")
#df=cbind(df,pos_cnt=res)

#subgraph to filter out non-useful screen names
#also remove extraneous modularity classes
dfn=read_csv("retweet_trump_top_001p_051718_loni.csv")
dfn= dfn %>%
  filter(modularity_class %in% c(19,13,12)) %>%
  mutate(
    class_label=case_when(modularity_class==19 ~ "cluster 1",
                          modularity_class==13 ~ "cluster 2",
                          modularity_class==12 ~ "cluster 3"
                          
    )
  )
dfn2 = dfn %>% 
  select(Label,Degree,Eccentricity:class_label)

df=distinct(df,status_id,.keep_all=TRUE)
dfc=inner_join(df,dfn2,by=c("screen_name"="Label"))

dft=dft[2:nrow(dft),]
#joins
#df=bind_cols(df,dft)
#dfc=inner_join(df,dfn2,by=c("screen_name"="Label"))

dfc=bind_cols(dfc,dft[,2:3])

dft=bind_cols(dft[,2:3],dfc)
dft=dft %>% mutate(
  class_label=case_when(modularity_class==19 ~ "cluster 1",
                        modularity_class==13 ~ "cluster 2",
                        modularity_class==12 ~ "cluster 3"
                        
  )
)


# summarizing POS
library(ggplot2)
pos_freq=dfc %>% 
  unnest(pos) %>% 
  group_by(class_label,screen_name,pos) %>% 
  summarize(n=sum(n)) %>% 
  mutate(freq=n/sum(n))

pos_freq2=pos_freq %>% filter(pos %in% c('^','@','#','O','G','U','~','Z','V','X','T','R','P','N','L','D','S','A',',')) 



pos_freq3=pos_freq2 %>% filter(pos %in% c('^','@','#','O','G','U','~')) %>% mutate(pos=recode(pos,`^`="proper noun",`@`='at-mention',`#`='hashtag',O='pronoun',G='garbage',U='URL',`~`='discourse'))

res<-pos_freq3 %>% group_by(class_label,pos) %>% summarise(mean=mean(n),median=median(n),std=sd(n))
pos_freq4=pos_freq2 %>% filter(pos %in% c('Z','V','X','T','R','P','N','L','D','S','A',','))  %>% mutate(pos=recode(pos,`Z`='proper + possesive',`V`='verb',`T`='verb particle',`X`='existential',`R`='adverb',`P`='pre/postposition',`N`='common noun',`L`='nomimal + verbal',`D`='determiner',`S`='nomimal + possesive',`A`='adjective',`,`='punctuation'))
res2<-pos_freq4 %>% group_by(class_label,pos) %>% summarise(mean=mean(n),median=median(n),std=sd(n))
library(WriteXLS)
reslist<-list(resultA=res,resultB=res2)

WriteXLS(reslist,"pos_summaries_060818.xls")


pos_freq4=pos_freq2 %>% filter(pos %in% c('Z','V','X','T','R','P','N','L','D','S','A',','))  %>% mutate(pos=recode(pos,`Z`='proper + possesive',`V`='verb',`T`='verb particle',`X`='existential',`R`='adverb',`P`='pre/postposition',`N`='common noun',`L`='nomimal + verbal',`D`='determiner',`S`='nomimal + possesive',`A`='adjective',`,`='punctuation'))


pos_freq4=pos_freq4  %>% mutate(pos=recode(pos,`T`='verb particle'))

pos_freq4=pos_freq4  %>% mutate(pos=recode(pos,`Verb`='verb'))

pos_freq3 %>% group_by


library(nnet)
multi1<-multinom(class_label~pos+n,data=pos_freq4)
pos_freq4$class_label<-as.factor(pos_freq4$class_label)
pos_freq4$class_label <- relevel(pos_freq4$class_label, ref = "cluster 2")
multi2<-multinom(class_label~pos+n,data=pos_freq4)

res<-tidy(multi2)
WriteXLS(res,"pos_nnet_cluster2contrast.xls")

res<-tidy(multi1)
WriteXLS(res,"pos_nnet.xls")
library(conover.test)
conover.test(pos_freq4$h)

pos_freq2=dft %>% 
  unnest(pos) %>% 
  group_by(class_label,screen_name,pos) %>% 
  add_tally(n) %>% 
  mutate(freq=n/nn)

library(parallel)
library(brms)
library(rstanarm)

df=data.frame(pos_freq)
library(forcats)
df$screen_name=as_factor(df$screen_name)
df$pos=as_factor(df$pos)
df$class_label=as_factor(df$class_label)
welp=complete(df,class_label,screen_name,pos,fill=list(n=0,freq=0))

welp=as.data.frame(welp)

welp=welp %>% group_by(class_label,screen_name) %>% 
  mutate(choice=case_when(sum(n) > 0 ~ TRUE,
                          sum(n) == 0 ~ FALSE
    
  ))
    
library(lme4)
mod<-glmer(class_label~pos+n + (1|screen_name),data=welp)
library(mlogit)
PD=mlogit.data(welp,choice="class_label",shape="long",alt.var=)

library(mnlogit)

fm=formula(choice~)

library(brglm2)
ctrast<-list(pos<-contr.treatment(levels(welp$pos),base=24 ),
screen_name=contr.treatment(levels(welp$screen_name),base=2345)             )

all_ml <- brmultinom(class_label~screen_name+pos,weights=n,ref=1,data=welp,contrasts=ctrast)


#mod <- stan_glm(class_label~pos + (1|n),family=categorical(link="logit"))


#mod <- stan_glmer(class_label~pos+(1|freq),family=categorical(link="logit"),data=welp,cores=4)

# <- stan_betareg(c)
mod <-brm(class_label~pos+(1|n),family=categorical(link="logit"),prior=set_prior("normal(0,5)"),data=welp,cores=4)

library(readr)
save_rds(mod,"brms_pos.rds")

loo_res<-loo(mod,cores=8)

ggplot(aes(pos,freq,colour=class_label),data=pos_freq)+geom_boxplot(position='dodge')+coord_flip()+theme(legend.position='bottom')

library(ggplot2)
library(cowplot)

p1<-ggplot(aes(pos,freq,colour=class_label),data=pos_freq3)+geom_boxplot(position='dodge')+coord_flip()+theme(legend.position='bottom')

save_plot("trump_pos_group1.png",p)

p2<-ggplot(aes(pos,freq,colour=class_label),data=pos_freq4)+geom_boxplot(position='dodge')+coord_flip()+theme(legend.position='none')

p<-plot_grid(p1,p2,labels=c("A","B"))
save_plot("trump_pos.png",p,ncol=2)


write_csv(pos_freq,"pos_byclass_byscreen.csv")

class_pos=pos_freq %>% 
  ungroup() %>% 
  group_by(class_label,pos) %>% 
  summarize(n=n()) %>% 
  mutate(freq=n/sum(n))

library(ggplot2)

p=ggplot(aes(pos,freq,colour=class_label),data=class_pos)+geom_col(position='dodge')+coord_flip()+theme(legend.position='bottom')
save_plot("trump_pos_groups.png",p)


library(brms)


dfc$account_created_at=as_datetime(dfc$account_created_at)
accs<-dfc %>% group_by(screen_name) %>% select(screen_name,class_label,modularity_class,account_created_at) %>% distinct(.keep_all =TRUE) 

p<-ggplot(aes(class_label,acc_created),data=accs)+geom_boxplot()
p<-p+xlab("Network Cluster")+ylab('Account Creation Date')
save_plot("acc_created_060818.png",p)
