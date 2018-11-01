##########################
#     香蜜油管评论分析     
#   作者 六界第一美男瓜    
# 出处 天涯六界第一美男楼    
##########################

# 前言:欢迎访问天涯六界第一美男楼，关注邓伦-公钰涵团队，带您走进资本控舆、按头审美、颠倒黑白、倒打一耙的奇幻世界。
# 数据来源于油管江苏卫视官方频道63集香蜜沉沉烬如霜视频评论。
# 最终解释权归天涯用户:@六界第一美男瓜 所有。
# 于2018年11月1日。

rm(list=ls())
#devtools::install_github("qinwf/ropencc")
library(ggplot2)
library(dplyr)
library(ropencc)
library(gridExtra)
library(reshape2)
library(RColorBrewer)
library(jiebaR)
library(stringi)
library(lubridate)

setwd("/YOUR WORKING DIRECTORY/")
Sys.setlocale("LC_ALL","zh_CN.utf-8")

#START OF READ IN:
###############
mycm= read.csv("MYXMSAFIN.csv",header = F)
colnames(mycm)=c("ep","page", "num","like","author","text","time","repnum","id","pos","con","neg","type")
###############

#CONVERT & TIME:
###############
mydf = mycm
nrow(mydf)
length(unique(mydf$author))
sum(mydf$x!=0)/63

#TO SIMPLIFIED CHINESE
ccst = converter(T2S)
temp = NA
for (i in 0:1000){
  temp = c(temp,ccst[mydf$text[(100*i+1):(100*(i+1))]])
}
#temp = c(temp,ccst[mydf$text[70501:nrow(mydf)]])
temp = temp[-1]
mydf$texts = temp[1:nrow(mydf)]
rm(list=c("temp","i"))

#TIME STRING AS TIME
mydf$time = ymd_hms(mydf$time)

#END OF READ IN
###############

#NOT RUN: SAVE IMAGE
###############
save.image(file = "mydf.RData")
###############

#FUNCTIONS:
###############
#NAMING NUMBER COUNT
yj = function(df,x){
  return(sum(df[,x]))
}
#NAMING ACCOUNT COUNT
ts = function(df,x){
  return(length(unique(df[df[,x]==T,"author"])))
}
#NAMING COUNT BY EPISODE
fj = function(df,m){
  return(aggregate(df[,m], by=list(Category=df$ep), FUN=sum)$x)
}
###############

#KEYWORD COUNT:
###############
#INPUT REQUIRY AS RULE (SUPPORTIVE OF REGULAR EXPRESSION)
rule="紫紫"
print(paste("油管言及数: ",sum(grepl(rule,mydf$texts))))


#NAMING: CAST/CHARA
################
#CAST
mydf$yz = grepl('杨紫', mydf$texts);sum(mydf$yz) 
mydf$dl = grepl('邓伦', mydf$texts);sum(mydf$dl) 
mydf$yx = grepl('罗云熙', mydf$texts);sum(mydf$yx) 
mydf$yq = grepl('陈钰琪', mydf$texts);sum(mydf$yq)
mydf$zh = grepl('王一菲', mydf$texts);sum(mydf$zh)
mydf$hm = grepl('周海媚', mydf$texts);sum(mydf$hm)
mydf$dd = grepl('萨顶顶', mydf$texts);sum(mydf$dd)
#mydf$jf = grepl('廖劲锋', mydf$texts);sum(mydf$jf)

#CP
mydf$yza = grepl('杨紫|紫妹|小紫|我紫|猴紫|紫紫', mydf$texts);sum(mydf$yza) 
mydf$dla = grepl('邓伦|伦哥|我伦|伦伦', mydf$texts);sum(mydf$dla) 
mydf$yxa = grepl('云熙|罗玉|熙熙', mydf$texts);sum(mydf$yxa)

#CHARA
mydf$jma = grepl('锦[觅蜜][她的]?[^<娘妈爸爹>]|葡萄[她的]?[^<娘妈爸爹>]]|小淘淘|霜花|果子|[觅蜜]儿|医女|圣女|女主[她的]?[^<娘妈爸爹>]|女[1一][她的]?[^<娘妈爸爹>]', mydf$texts) ;sum(mydf$jma)
mydf$xfa = grepl('旭凤[他的]?[^<娘妈爸爹>]|凤凰[他的]?[^<娘妈爸爹>]|二凤|凤娃|二殿[他的]?[^<娘妈爸爹>]|火神[他的]?[^<娘妈爸爹>]|战神|熠王|貂哥|鸦鸦|男主[他的]?[^<娘妈爸爹>]|男[1一][他的]?[^<娘妈爸爹>]', mydf$texts);sum(mydf$xfa)
mydf$rya = grepl('润玉[他的]?[^<娘妈爸爹>]|大龙|大殿[他的]?[^<娘妈爸爹>]|夜神[他的]?[^<娘妈爸爹>]|白龙|应龙|小鱼|男[2二][他的]?[^<娘妈爸爹>]', mydf$texts);sum(mydf$rya)
mydf$tha = grepl('天后|荼姚', mydf$text);sum(mydf$tha)
mydf$tda = grepl('天帝|太微', mydf$text);sum(mydf$tda)
mydf$sha = grepl('穗禾|鸟族公主', mydf$text);sum(mydf$sha)
mydf$ssa = grepl('月老|月下仙人|狐狸|红红', mydf$text);sum(mydf$ssa)
mydf$pca = grepl('[扑噗][嗤哧]|彦佑|蛇', mydf$text);sum(mydf$pca)
mydf$lya = grepl('鎏英|魔[族界]公主|卞城公主', mydf$text);sum(mydf$lya)
mydf$yja = grepl('缘机|机机', mydf$text);sum(mydf$yja)

#COMP
mydf$f1 = (mydf$yz|mydf$jma)
mydf$m1 = (mydf$dl|mydf$xfa)
mydf$m2 = (mydf$yx|mydf$rya)
mydf$f2 = (mydf$yq|mydf$lya)
mydf$thhm = (mydf$hm|mydf$tha)
################

myarch = mydf
myboth = mydf[mydf$time<"2018-10-03 00:00:00 UTC",]
myytin = myboth#[myboth$res=="Youtube",]
#mytcin = myboth[myboth$res=="Tencent",]

#save.image(file = "my2df.RData")

#CAST NAMING
################
pdf_cast_yt = data.frame(x=c('杨紫','邓伦','罗云熙','周海媚','萨顶顶','陈钰琪','王一菲'),
                      y=c(yj(myytin,"yz"),yj(myytin,"dl"),yj(myytin,"yx"),yj(myytin,"hm"),yj(myytin,"dd"),yj(myytin,"yq"),yj(myytin,"zh")),
                      z=c(ts(myytin,"yz"),ts(myytin,"dl"),ts(myytin,"yx"),ts(myytin,"hm"),ts(myytin,"dd"),ts(myytin,"yq"),ts(myytin,"zh")))
# pdf_cast_tc = data.frame(x=c('杨紫','邓伦','罗云熙','周海媚','萨顶顶','陈钰琪','王一菲'),
#                       y=c(yj(mytcin,"yz"),yj(mytcin,"dl"),yj(mytcin,"yx"),yj(mytcin,"hm"),yj(mytcin,"dd"),yj(mytcin,"yq"),yj(mytcin,"zh")),
#                       z=c(ts(mytcin,"yz"),ts(mytcin,"dl"),ts(mytcin,"yx"),ts(mytcin,"hm"),ts(mytcin,"dd"),ts(mytcin,"yq"),ts(mytcin,"zh")))

p1=ggplot(data=pdf_cast_yt) + geom_col(aes(x=x,y=y),alpha=.5,fill="azure3") + geom_col(aes(x=x,y=z),fill="azure3") +
  scale_x_discrete(limits=pdf_cast_yt$x) + geom_text(aes(label=z,x=x,y=z),vjust=1.4) +
  geom_text(aes(label=y,x=x,y=y),vjust=-.4) + xlab('演员全名·油管篇') + 
  ylab('言及次数')  +theme_classic() +theme(text = element_text(family = 'Yuppy SC'));p1

# p2=ggplot(data=pdf_cast_tc) + geom_col(aes(x=x,y=y),alpha=.5,fill="azure3") + geom_col(aes(x=x,y=z),fill="azure3") +
#   scale_x_discrete(limits=pdf_cast_tc$x) + geom_text(aes(label=z,x=x,y=z),vjust=1.4) +
#   geom_text(aes(label=y,x=x,y=y),vjust=-.4) + xlab('演员全名·企鹅篇') + 
#   ylab('言及次数')  +theme_classic() +theme(text = element_text(family = 'Yuppy SC'))
# 
# grid.arrange(p2, p1, nrow = 1)
################

#CAST BY EPISODE:
################
pdf_castep_yt = melt(data.frame(x=1:63,yz=fj(myytin,"yz"),dl=fj(myytin,"dl"),yx=fj(myytin,"yx")),id="x")
p7 = ggplot(data=pdf_castep_yt%>%filter(x!=1),aes(x=x,y=value,color=variable)) + geom_point() + geom_smooth(alpha=.1) + 
  scale_x_continuous(breaks = c(1,seq(4,60,by=5),63)) + xlab('第2集起演员言及·油管篇') + 
  scale_y_continuous(expand = c(0,0))+
  coord_cartesian(clip="off",ylim = c(0,60)) +
  scale_color_discrete(label=c('杨紫','邓伦','罗云熙'),name="言及数") +
  ylab('每集言及数')+theme_classic() +theme(text = element_text(family = 'Yuppy SC'));p7
# pdf_castep_tc = melt(data.frame(x=1:63,yz=fj(mytcin,"yz"),dl=fj(mytcin,"dl"),yx=fj(mytcin,"yx")),id="x")
# p8 = ggplot(data=pdf_castep_tc%>%filter(x!=1),aes(x=x,y=value,color=variable)) + geom_point() + geom_smooth(alpha=.1) + 
#   scale_x_continuous(breaks = c(1,seq(4,60,by=5),63)) + xlab('第2集起演员言及·企鹅篇') + 
#   scale_y_continuous(expand = c(0,0),limits=c(0,2200))+
#   coord_cartesian(clip="off",ylim = c(0,1500))+
#   scale_color_discrete(guide=F) +
#   ylab('每集言及数')+theme_classic() +theme(text = element_text(family = 'Yuppy SC'));p8

# grid.arrange(p8, p7, nrow = 1,widths=c(1,1.2))
###############


#CHARA NAMING:
###############
pdf_charaext_yt = data.frame(x=c('锦觅','旭凤','润玉','天后','天帝','穗禾','月下仙人','噗嗤君','鎏英','缘机'),
                       y=c(yj(myytin,"jma"),yj(myytin,"xfa"),yj(myytin,"rya"),yj(myytin,"tha"),yj(myytin,"tda"),yj(myytin,"sha"),yj(myytin,"ssa"),yj(myytin,"pca"),yj(myytin,"lya"),yj(myytin,"yja")),
                       z=c(ts(myytin,"jma"),ts(myytin,"xfa"),ts(myytin,"rya"),ts(myytin,"tha"),ts(myytin,"tda"),ts(myytin,"sha"),ts(myytin,"ssa"),ts(myytin,"pca"),ts(myytin,"lya"),ts(myytin,"yja")))

# pdf_charaext_tc = data.frame(x=c('锦觅','旭凤','润玉','天后','天帝','穗禾','月下仙人','噗嗤君','鎏英','缘机'),
#                              y=c(yj(mytcin,"jma"),yj(mytcin,"xfa"),yj(mytcin,"rya"),yj(mytcin,"tha"),yj(mytcin,"tda"),yj(mytcin,"sha"),yj(mytcin,"ssa"),yj(mytcin,"pca"),yj(mytcin,"lya"),yj(mytcin,"yja")),
#                              z=c(ts(mytcin,"jma"),ts(mytcin,"xfa"),ts(mytcin,"rya"),ts(mytcin,"tha"),ts(mytcin,"tda"),ts(mytcin,"sha"),ts(mytcin,"ssa"),ts(mytcin,"pca"),ts(mytcin,"lya"),ts(mytcin,"yja")))

p3=ggplot(pdf_charaext_yt) + geom_col(aes(x=x,y=y),alpha=.5,fill="azure3") + geom_col(aes(x=x,y=z),fill="azure3") +
  scale_x_discrete(limits=c('锦觅','旭凤','润玉','鎏英','天后','天帝','穗禾','月下仙人','噗嗤君','缘机')) + 
  geom_text(aes(label=z,x=x,y=z),vjust=1.4) +
  geom_text(aes(label=y,x=x,y=y),vjust=-.4) + xlab('角色昵称·油管篇') + 
  ylab('言及次数')+theme_classic() +theme(text = element_text(family = 'Yuppy SC'))

p3
# p4=ggplot(pdf_charaext_tc) + geom_col(aes(x=x,y=y),alpha=.5,fill="azure3") + geom_col(aes(x=x,y=z),fill="azure3") +
#   scale_x_discrete(limits=c('锦觅','旭凤','润玉','鎏英','天后','天帝','穗禾','月下仙人','噗嗤君','缘机')) + 
#   geom_text(aes(label=z,x=x,y=z),vjust=1.4) +
#   geom_text(aes(label=y,x=x,y=y),vjust=-.4) + xlab('角色昵称·企鹅篇') + 
#   #scale_y_continuous(expand = c(0,0))+coord_cartesian(clip="off")+
#   ylab('言及次数')+theme_classic() +theme(text = element_text(family = 'Yuppy SC'))
# 
# grid.arrange(p4, p3, nrow = 1)
###############

#CHARA BY EP:
###############
pdf_charaextep_yt = melt(data.frame(x=1:63,jm=fj(myytin,"jma"),xf=fj(myytin,"xfa"),ry=fj(myytin,"rya")),id="x")
p5 = ggplot(data=pdf_charaextep_yt%>%filter(x!=1),aes(x=x,y=value,color=variable)) + geom_point() + geom_smooth(alpha=.1) + 
  scale_x_continuous(breaks = c(1,seq(4,60,by=5),63)) + xlab('第2集起角色昵称言及·油管篇') + 
  scale_y_continuous(expand = c(0,0))+
  scale_color_discrete(label=c('锦觅','旭凤','润玉'),name="角色昵称") +
  coord_cartesian(clip="off")+
  ylab('分集言及次数')+theme_classic() +theme(text = element_text(family = 'Yuppy SC'));p5

# pdf_charaextep_tc = melt(data.frame(x=1:63,jm=fj(mytcin,"jma"),xf=fj(mytcin,"xfa"),ry=fj(mytcin,"rya")),id="x")
# p6 = ggplot(data=pdf_charaextep_tc%>%filter(x!=1),aes(x=x,y=value,color=variable)) + geom_point() + geom_smooth(alpha=.1) + 
#   scale_x_continuous(breaks = c(1,seq(4,60,by=5),63)) + xlab('第2集起角色昵称言及·企鹅篇') + 
#   scale_color_discrete(guide=F) +
#   scale_y_continuous(expand = c(0,0))+
#   coord_cartesian(clip="off")+
#   ylab('分集言及次数')+theme_classic() +theme(text = element_text(family = 'Yuppy SC'));p6
# 
# grid.arrange(p6, p5, nrow = 1,widths=c(1,1.2))

myytin%>%filter(jma==1|xfa==1|rya==1|yz==1|yx==1|dl==1)%>%filter(ep%in%c(2:10))%>%nrow(.)
# mytcin%>%filter(jma==1|xfa==1|rya==1|yz==1|yx==1|dl==1)%>%filter(ep%in%c(2:10))%>%nrow(.)

# p17 = ggplot() + geom_smooth(data=pdf_charaextep_tc%>%filter(x!=1),aes(x=x,y=value,color=variable),alpha=0) + 
#   geom_smooth(data=pdf_charaextep_yt%>%filter(x!=1),aes(x=x,y=value*20,color=variable),alpha=0,linetype="dashed") +
#   scale_x_continuous(breaks = c(1,seq(4,60,by=5),63)) + xlab('第2集起角色昵称言及趋势比较') + 
#   scale_color_discrete(guide=F) +
#   annotate("text", x = 9, y = 5000, label = "实线:腾讯\n虚线:油管*20",hjust = 0,
#            family = 'Yuppy SC', size = 4) +
#   scale_y_continuous(expand = c(0,0),limits=c(0,NA),breaks=c(0,1000,1500,2000,3000,3500,4000,5000))+
#   scale_color_discrete(label=c('锦觅','旭凤','润玉'),name="角色昵称") +
#   coord_cartesian(clip="off")+
#   geom_hline(yintercept=1500,alpha=0.3,linetype="dotted")+
#   geom_hline(yintercept=3500,alpha=0.3,linetype="dotted")+
#   ylab('分集言及次数')+theme_classic() +theme(text = element_text(family = 'Yuppy SC'));p17

###############

#CAST-CHARA BY EP:
###############
temp = myytin
# temp = mytcin

pdf_zh_yt=bind_rows(temp %>% group_by(ep) %>% summarise(nep = sum(f1==1)) %>% mutate(type="f1"),
                    temp %>% group_by(ep) %>% summarise(nep = sum(m1==1)) %>% mutate(type="m1"),
                    temp %>% group_by(ep) %>% summarise(nep = sum(m2==1)) %>% mutate(type="m2"),
                    temp %>% group_by(ep) %>% summarise(nep = sum(f2==1)) %>% mutate(type="f2"))

# pdf_zh_tc=bind_rows(temp %>% group_by(ep) %>% summarise(nep = sum(f1==1)) %>% mutate(type="f1"),
#                     temp %>% group_by(ep) %>% summarise(nep = sum(m1==1)) %>% mutate(type="m1"),
#                     temp %>% group_by(ep) %>% summarise(nep = sum(m2==1)) %>% mutate(type="m2"),
#                     temp %>% group_by(ep) %>% summarise(nep = sum(f2==1)) %>% mutate(type="f2"))
# 
p11 = ggplot(data=pdf_zh_yt%>%filter(ep!=1&type!="f2"),aes(x=ep,y=nep,color=type)) + geom_point() +geom_smooth(alpha=.1) + 
  scale_x_continuous(breaks = c(1,seq(4,60,by=5),63)) + xlab('第2集起演员或角色言及数·油管篇') + 
  #scale_y_log10() +
  scale_y_continuous(expand=c(0,0))+
  scale_color_discrete(label=c('女主','男一','男二'),name="综合言及数") +
  coord_cartesian(clip="off",ylim = c(0,420))+
  ylab('分集言及数')+theme_classic() +theme(text = element_text(family = 'Yuppy SC'));p11

# p12 = ggplot(data=pdf_zh_tc%>%filter(ep!=1&type!="f2"),aes(x=ep,y=nep,color=type)) + geom_point() +geom_smooth(alpha=.1) + 
#   scale_x_continuous(breaks = c(1,seq(4,60,by=5),63)) + xlab('第2集起演员或角色言及数·企鹅篇') + 
#   #scale_y_log10() +
#   scale_y_continuous(expand=c(0,0))+
#   scale_color_discrete(guide=F) +
#   coord_cartesian(clip="off",ylim = c(0,6000))+
#   ylab('分集言及数')+theme_classic() +theme(text = element_text(family = 'Yuppy SC'));p12
# grid.arrange(p12,p11,nrow=1,widths=c(1,1.2))

# pdf_comp_tc = pdf_zh_tc%>%group_by(variable)%>%summarise(all=sum(value))
# pdf_comp_yt = pdf_zh_yt%>%group_by(nep)%>%summarise(all=sum(value))
# p13=ggplot(pdf_comp_yt) + geom_col(aes(x=neo,y=all,fill=variable)) +
#   scale_x_discrete(labels=c('锦觅','旭凤','润玉','荼姚')) + scale_fill_discrete(guide=F)+
#   geom_text(aes(label=all,x=nep,y=all),vjust=-.6) +
#   scale_y_continuous(limits = c(0,85000),expand = c(0,0))+
#   xlab('全剧演员角色综合讨论度·油管篇') + 
#   ylab('综合讨论度:评论言及+0.3*点赞数+0.7*回复数')+theme_classic() +theme(text = element_text(family = 'Yuppy SC'))
# p13
# p14=ggplot(pdf_comp_tc) + geom_col(aes(x=variable,y=all,fill=variable)) +
#   scale_x_discrete(labels=c('锦觅','旭凤','润玉','荼姚')) + 
#   geom_text(aes(label=all,x=variable,y=all),vjust=-.6) +
#   xlab('全剧演员角色综合讨论度·企鹅篇') + 
#   scale_y_continuous(expand = c(0,0),limits=c(0,1000000))+scale_fill_discrete(guide=F)+
#   ylab('综合讨论度:评论言及+0.3*点赞数+0.7*回复数')+theme_classic() +theme(text = element_text(family = 'Yuppy SC'))
# p14
# 
# grid.arrange(p14, p13, nrow = 1)
###############

#CAST or CHARA BY EP:
###############
temp = myytin
# temp = mytcin

temp %>% group_by(ep) %>% summarise(casto = sum(yz&!jma),charao = sum(jma&!yz)) %>% 
  mutate(type="f1",carate = casto/(casto+charao))

pdf_zh_yt=bind_rows(temp %>% group_by(ep) %>% summarise(casto = sum(yz&!jma),charao = sum(jma&!yz)) %>% 
                      mutate(type="f1",carate = casto/(casto+charao)),
                    temp %>% group_by(ep) %>% summarise(casto = sum(dl&!xfa),charao = sum(xfa&!dl)) %>% 
                      mutate(type="m1",carate = casto/(casto+charao)),
                    temp %>% group_by(ep) %>% summarise(casto = sum(yx&!rya),charao = sum(rya&!yx)) %>% 
                      mutate(type="m2",carate = casto/(casto+charao)))

# pdf_zh_tc=bind_rows(temp %>% group_by(ep) %>% summarise(casto = sum(yz&!jma),charao = sum(jma&!yz)) %>% 
#                       mutate(type="f1",carate = casto/(casto+charao)),
#                     temp %>% group_by(ep) %>% summarise(casto = sum(dl&!xfa),charao = sum(xfa&!dl)) %>% 
#                       mutate(type="m1",carate = casto/(casto+charao)),
#                     temp %>% group_by(ep) %>% summarise(casto = sum(yx&!rya),charao = sum(rya&!yx)) %>% 
#                       mutate(type="m2",carate = casto/(casto+charao)))

p11 = ggplot(data=pdf_zh_yt%>%filter(type!="f2"),aes(x=ep,y=carate,color=type)) + geom_point() +geom_smooth(alpha=.1) + 
  scale_x_continuous(breaks = c(1,seq(4,60,by=5),63)) + xlab('仅演员/(仅演员+仅角色)言及数比值·油管篇') + 
  #scale_y_log10() +
  scale_y_continuous(expand=c(0,0))+
  scale_color_discrete(label=c('女主','男一','男二'),name="综合言及数") +
  coord_cartesian(clip="off",ylim = c(0,1))+
  ylab('演员分集言及数占比')+theme_classic() +theme(text = element_text(family = 'Yuppy SC'));p11

# p12 = ggplot(data=pdf_zh_tc%>%filter(type!="f2"),aes(x=ep,y=carate,color=type)) + geom_point() +geom_smooth(alpha=.1) + 
#   scale_x_continuous(breaks = c(1,seq(4,60,by=5),63)) + xlab('仅演员/(仅演员+仅角色)比值·企鹅篇') + 
#   #scale_y_log10() +
#   scale_y_continuous(expand=c(0,0))+
#   scale_color_discrete(guide=F) +
#   coord_cartesian(clip="off",ylim = c(0,1))+
#   ylab('演员分集言及数占比')+theme_classic() +theme(text = element_text(family = 'Yuppy SC'));p12
# grid.arrange(p12,p11,nrow=1,widths=c(1,1.2))

# pdf_comp_tc = pdf_zh_tc%>%group_by(variable)%>%summarise(all=sum(value))
# pdf_comp_yt = pdf_zh_yt%>%group_by(variable)%>%summarise(all=sum(value))
# p13=ggplot(pdf_comp_yt) + geom_col(aes(x=variable,y=all,fill=variable)) +
#   scale_x_discrete(labels=c('锦觅','旭凤','润玉','荼姚')) + scale_fill_discrete(guide=F)+
#   geom_text(aes(label=all,x=variable,y=all),vjust=-.6) +
#   scale_y_continuous(limits = c(0,85000),expand = c(0,0))+
#   xlab('全剧演员角色综合讨论度·油管篇') + 
#   ylab('综合讨论度:评论言及+0.3*点赞数+0.7*回复数')+theme_classic() +theme(text = element_text(family = 'Yuppy SC'))
# p13
# p14=ggplot(pdf_comp_tc) + geom_col(aes(x=variable,y=all,fill=variable)) +
#   scale_x_discrete(labels=c('锦觅','旭凤','润玉','荼姚')) + 
#   geom_text(aes(label=all,x=variable,y=all),vjust=-.6) +
#   xlab('全剧演员角色综合讨论度·企鹅篇') + 
#   scale_y_continuous(expand = c(0,0),limits=c(0,1000000))+scale_fill_discrete(guide=F)+
#   ylab('综合讨论度:评论言及+0.3*点赞数+0.7*回复数')+theme_classic() +theme(text = element_text(family = 'Yuppy SC'))
# p14
# 
# grid.arrange(p14, p13, nrow = 1)
#############


###############

#CAST BY CP
###############

# Disinf
temp = myytin
# temp = mytcin
pdf_cp = temp%>%
  mutate(disinf = 1+0.3*like+0.7*repnum)%>% #disinf = 
  group_by(yz,dl,yx)%>%summarise(inf = sum(disinf))%>%
  mutate(tag = case_when(
    yz&dl&yx ~ 111,
    !yz&!dl&!yx ~ 0,
    yz&!dl&yx ~ 101,
    yz&dl&!yx ~ 110,
    !yz&dl&yx ~ 011,
    yz&!dl&!yx ~ 100,
    !yz&!dl&yx ~ 001,
    !yz&dl&!yx ~ 010))
pdf_cp_yt = bind_rows(pdf_cp%>%filter(yz)%>%ungroup%>%arrange(desc(tag))%>%mutate(type="jm",infpro = inf/sum(inf),z = cumsum(inf)-0.5*inf),
                      pdf_cp%>%filter(dl)%>%ungroup%>%arrange(desc(tag))%>%mutate(type="xf",infpro = inf/sum(inf),z = cumsum(inf)-0.5*inf),
                      pdf_cp%>%filter(yx)%>%ungroup%>%arrange(desc(tag))%>%mutate(type="ry",infpro = inf/sum(inf),z = cumsum(inf)-0.5*inf))
# pdf_cp_tc = bind_rows(pdf_cp%>%filter(yz)%>%ungroup%>%arrange(desc(tag))%>%mutate(type="jm",infpro = inf/sum(inf),z = cumsum(inf)-0.5*inf),
#                       pdf_cp%>%filter(dl)%>%ungroup%>%arrange(desc(tag))%>%mutate(type="xf",infpro = inf/sum(inf),z = cumsum(inf)-0.5*inf),
#                       pdf_cp%>%filter(yx)%>%ungroup%>%arrange(desc(tag))%>%mutate(type="ry",infpro = inf/sum(inf),z = cumsum(inf)-0.5*inf))

p15 = ggplot(pdf_cp_yt[order(pdf_cp_yt$tag),])+geom_col(aes(x=type,y=inf,fill=factor(tag,levels=c(100,10,1,11,101,110,111))))+
  scale_x_discrete(limits=c("jm","xf","ry"),labels=c("杨紫","邓伦","罗云熙")) + 
  scale_fill_manual(name="言及成分",
                      labels=c("女一单人","男一单人","男二单人","男一男二","女一男二","女一男一","三人"),
                      values=brewer.pal(7,"Set3")[c(4,7,1,6,3,2,5)])+
  geom_text(aes(label=paste(round(infpro,2)*100,"%",sep=""),x=type,y=z),vjust=.4) +
  #geom_text(aes(label=y,x=x,y=y),vjust=-.4) + 
  xlab('演员全名言及·油管篇') + 
  ylab('言及数')  +theme_classic() +theme(text = element_text(family = 'Yuppy SC'));p15
# myhj=rep(0.5,12);myhj[4]=-1;myhj[7]=-1;myvj=rep(.4,12);myvj[4]=-.3;myvj[7]=.6
# p16 = ggplot(pdf_cp_tc[order(pdf_cp_yt$tag),])+geom_col(aes(x=type,y=inf,fill=factor(tag,levels=c(100,10,1,11,101,110,111))))+
#   scale_x_discrete(limits=c("jm","xf","ry"),labels=c("杨紫","邓伦","罗云熙")) + 
#   scale_fill_manual(guide=F,values=brewer.pal(7,"Set3")[c(4,7,1,6,3,2,5)])+
#   geom_text(aes(label=paste(round(infpro,2)*100,"%",sep=""),x=type,y=z),vjust=myvj,hjust=myhj) +
#   #geom_text(aes(label=z,x=x,y=z),vjust=1.4) +
#   #geom_text(aes(label=y,x=x,y=y),vjust=-.4) + 
#   xlab('演员全名言及·企鹅篇') + 
#   ylab('言及数')  +theme_classic() +theme(text = element_text(family = 'Yuppy SC'));p16
# 
# grid.arrange(p16,p15,nrow=1,widths=c(1,1.2))

###############


#CAST-NICKNAME BY CP
###############
temp = myytin
# temp = mytcin
pdf_cp = temp%>%
  mutate(disinf = 1)%>% #disinf = 1+0.3*like+0.7*repnum
  group_by(yza,dla,yxa)%>%summarise(inf = sum(disinf))%>%
  mutate(tag = case_when(
      yza&dla&yxa ~ 111,
      !yza&!dla&!yxa ~ 0,
      yza&!dla&yxa ~ 101,
      yza&dla&!yxa ~ 110,
      !yza&dla&yxa ~ 011,
      yza&!dla&!yxa ~ 100,
      !yza&!dla&yxa ~ 001,
      !yza&dla&!yxa ~ 010))
pdf_cp_yt = bind_rows(pdf_cp%>%filter(yza)%>%ungroup%>%arrange(desc(tag))%>%mutate(type="jm",infpro = inf/sum(inf),z = cumsum(inf)-0.5*inf),
                      pdf_cp%>%filter(dla)%>%ungroup%>%arrange(desc(tag))%>%mutate(type="xf",infpro = inf/sum(inf),z = cumsum(inf)-0.5*inf),
                      pdf_cp%>%filter(yxa)%>%ungroup%>%arrange(desc(tag))%>%mutate(type="ry",infpro = inf/sum(inf),z = cumsum(inf)-0.5*inf))
# pdf_cp_tc = bind_rows(pdf_cp%>%filter(yza)%>%ungroup%>%arrange(desc(tag))%>%mutate(type="jm",infpro = inf/sum(inf),z = cumsum(inf)-0.5*inf),
#                       pdf_cp%>%filter(dla)%>%ungroup%>%arrange(desc(tag))%>%mutate(type="xf",infpro = inf/sum(inf),z = cumsum(inf)-0.5*inf),
#                       pdf_cp%>%filter(yxa)%>%ungroup%>%arrange(desc(tag))%>%mutate(type="ry",infpro = inf/sum(inf),z = cumsum(inf)-0.5*inf))
pdf_cp_text_yt = pdf_cp_yt%>%group_by(type)%>%summarize(all=sum(inf))
# pdf_cp_text_tc = pdf_cp_tc%>%group_by(type)%>%summarize(all=sum(inf))

p15 = ggplot(pdf_cp_yt[order(pdf_cp_yt$tag),])+
  geom_col(aes(x=type,y=inf,fill=factor(tag,levels=c(100,10,1,11,101,110,111))))+
  scale_x_discrete(limits=c("jm","xf","ry"),labels=c("杨紫","邓伦","罗云熙")) + 
  #coord_cartesian(clip="off",ylim = c(0,12000))+
  scale_fill_manual(name="言及成分",
                    labels=c("女一单人","男一单人","男二单人","男一男二","女一男二","女一男一","三人"),
                    values=brewer.pal(7,"Set3")[c(4,7,1,6,3,2,5)])+
  geom_text(aes(label=paste(round(infpro,2)*100,"%",sep=""),x=type,y=z),vjust=.4) +
  geom_text(data=pdf_cp_text_yt,aes(label=all,x=type,y=all),vjust=-.2) +
  xlab('演员昵称言及·油管篇') + 
  ylab('评论言及数')  +theme_classic() +theme(text = element_text(family = 'Yuppy SC'));p15
myhj=rep(0.5,12);myhj[4]=-.7;myhj[7]=-1;
myvj=rep(.4,12);myvj[4]=-.3;myvj[7]=.6
# p16 = ggplot(pdf_cp_tc[order(pdf_cp_yt$tag),])+geom_col(aes(x=type,y=inf,fill=factor(tag,levels=c(100,10,1,11,101,110,111))))+
#   scale_x_discrete(limits=c("jm","xf","ry"),labels=c("杨紫","邓伦","罗云熙")) + 
#   scale_fill_manual(guide=F,values=brewer.pal(7,"Set3")[c(4,7,1,6,3,2,5)])+
#   geom_text(aes(label=paste(round(infpro,2)*100,"%",sep=""),x=type,y=z),vjust=myvj,hjust=myhj) +
#   geom_text(data=pdf_cp_text_tc,aes(label=all,x=type,y=all),vjust=-.2) +
#   #geom_text(aes(label=z,x=x,y=z),vjust=1.4) +
#   #geom_text(aes(label=y,x=x,y=y),vjust=-.4) + 
#   xlab('演员昵称言及·企鹅篇') + 
#   ylab('综合讨论度:评论言及+0.3*点赞数+0.7*回复数')  +theme_classic() +theme(text = element_text(family = 'Yuppy SC'));p16
# 
# grid.arrange(p16,p15,nrow=1,widths=c(1,1.2))

###############

#CAST BY TIME:
###############
pdf_tm = melt(data.frame(myytin[,c('time','yz','dl','yx')]),id="time")
pdf_tm_yt = pdf_tm[pdf_tm$value==TRUE,]

p19=ggplot(pdf_tm_yt)+geom_histogram(aes(x=as.Date(time),fill=variable),binwidth = 7,position='dodge',alpha=1)+
  scale_fill_discrete(label=c('杨紫','邓伦','罗云熙'),name="演员姓名言及") + 
  scale_color_discrete(guide=F) + #scale_y_continuous(limits=c(0,300),expand = c(0,0))+
  xlab('日期') + ylab('一周言及评论数') + 
  scale_x_date(date_breaks = "1 weeks",date_labels = "%b月%d日") + theme_classic() +
  theme(text = element_text(family = 'Yuppy SC'),axis.line.y = element_blank(),
        axis.text.x = element_text(angle = 30, hjust = 1))
p19
# p20=ggplot(pdf_tm_tc)+geom_histogram(aes(x=as.Date(time),fill=variable),binwidth = 7,position='dodge',alpha=1)+
#   #scale_fill_discrete(label=c('杨紫','邓伦','罗云熙'),name="演员姓名言及") + 
#   scale_fill_discrete(guide=F) + #scale_y_continuous(limits=c(0,6000),expand = c(0,0))+
#   xlab('日期') + ylab('一周言及评论数') + 
#   scale_x_date(date_breaks = "1 weeks",date_labels = "%b月%d日") + theme_classic() +
#   theme(text = element_text(family = 'Yuppy SC'),axis.line.y = element_blank(),
#         axis.text.x = element_text(angle = 30, hjust = 1))
# grid.arrange(p20,p19,nrow=1,widths=c(1,1.2))
# min(as.Date(mydf$time))


#CAST BY TIME:
pdf_tm = melt(data.frame(mydf[,c('time','f1','m1','m2')]),id="time")
pdf_tm = pdf_tm[pdf_tm$value==TRUE,]
head(pdf_tm); pdf_tm$time = ymd_hms(pdf_tm$time)
ggplot(pdf_tm)+geom_density(aes(x=as.Date(time),fill=variable),alpha=0.3)+
  scale_fill_discrete(label=c('女一','男一','男二'),name="演员角色综合言及") + 
  scale_color_discrete(guide=F) +
  xlab('日期') + ylab('频率密度') + 
  scale_x_date(date_breaks = "1 weeks",date_labels = "%b月%d日") + theme_classic() +
  theme(text = element_text(family = 'Yuppy SC'),axis.line.y = element_blank())


#CAST BY TIME:
pdf_tm = melt(data.frame(mydf[,c('time','jma','xfa','rya')]),id="time")
pdf_tm = pdf_tm[pdf_tm$value==TRUE,]
head(pdf_tm); pdf_tm$time = ymd_hms(pdf_tm$time)
ggplot(pdf_tm)+geom_density(aes(x=as.Date(time),fill=variable),alpha=0.3)+
  scale_fill_discrete(label=c('锦觅','旭凤','润玉'),name="角色昵称言及") + 
  scale_color_discrete(guide=F) +
  xlab('日期') + ylab('频率密度') + 
  scale_x_date(date_breaks = "1 weeks",date_labels = "%b月%d日") + theme_classic() +
  theme(text = element_text(family = 'Yuppy SC'),axis.line.y = element_blank())

###############


#SA
###################
mean(mydf[mydf$f1==1,"pos"],na.rm=T)
mean(mydf[mydf$m1==1,"pos"],na.rm=T)
mean(mydf[mydf$m2==1,"pos"],na.rm=T)

mean(mydf[mydf$yz==1,"pos"],na.rm=T)
mean(mydf[mydf$dl==1,"pos"],na.rm=T)
mean(mydf[mydf$yx==1,"pos"],na.rm=T)

mean(mydf[mydf$jma==1,"pos"],na.rm=T)
mean(mydf[mydf$xfa==1,"pos"],na.rm=T)
mean(mydf[mydf$rya==1,"pos"],na.rm=T)
mean(mydf[mydf$tha==1,"pos"],na.rm=T)
mean(mydf[mydf$ssa==1,"pos"],na.rm=T)
dfin = mydf[mydf$con>0.9,]
head(dfin)
ggplot()+geom_density(aes(x=mydf$con))

# CHARACTER SENTIMENT
###################

#CHARA SENTIMENT
###################
dfin=mydf
pdf_saex = data.frame(jm=as.vector(table(dfin[dfin$jma==1,"type"])),
                    xf=as.vector(table(dfin[dfin$xfa==1,"type"])),
                    ry=as.vector(table(dfin[dfin$rya==1,"type"])),
                    th=as.vector(table(dfin[dfin$tha==1,"type"])),
                    td=as.vector(table(dfin[dfin$tda==1,"type"])),
                    ss=as.vector(table(dfin[dfin$ssa==1,"type"])),
                    pc=as.vector(table(dfin[dfin$pca==1,"type"])),
                    ly=as.vector(table(dfin[dfin$lya==1,"type"])),
                    sh=as.vector(table(dfin[dfin$sha==1,"type"]))
                    )
pdf_saex$id = c("neg","neu","pos");pdf_saex = melt(pdf_saex,id="id")
pdf_saex = pdf_saex %>%
  group_by(variable,id) %>%
  summarise (n = value) %>%
  mutate(freq = n / sum(n)) %>%
  mutate(y = cumsum(freq)-0.5*freq)

ggplot(pdf_saex)+geom_bar(aes(x=variable,fill=factor(id, levels=c("pos","neu","neg")),y=n),stat='identity',position='fill') +
  scale_x_discrete(label=c("锦觅","旭凤","润玉","天后","天帝","月下仙人","噗嗤君","鎏英","穗禾")) + #geom_text(aes(label=z,x=x,y=value),vjust=1.4) +
  scale_fill_manual(label=c("正向","中性","负向"),limits=c("pos","neu","neg"),name=("情感倾向"),values=wes_palette(3,name = "Royal1")[c(2,3,1)])+
  scale_y_continuous(labels = scales::percent)+
  geom_text(aes(label=paste(100*round(freq,2),"%",", ",n,sep=""),x=variable,y=y),vjust=0.4) + xlab('角色昵称') + 
  ylab('情感占比')  +theme_classic() +
  theme(text = element_text(family = 'Yuppy SC'))

###################

#CAST SENTIMENT
###################
dfin = mydf
pdf_sa = data.frame(f1=as.vector(table(dfin[dfin$yz==1,"type"])),
                    m1=as.vector(table(dfin[dfin$dl==1,"type"])),
                    m2=as.vector(table(dfin[dfin$yx==1,"type"])))
pdf_sa$id = c("neg","neu","pos");pdf_sa = melt(pdf_sa,id="id")
pdf_sa = pdf_sa %>%
  group_by(variable,id) %>%
  summarise (n = value) %>%
  mutate(freq = n / sum(n)) %>%
  mutate(y = cumsum(freq)-0.5*freq)

ggplot(pdf_sa)+geom_bar(aes(x=variable,fill=factor(id, levels=c("pos","neu","neg")),y=n),stat='identity',position='fill') +
  scale_x_discrete(label=c("杨紫","邓伦","罗云熙")) + #geom_text(aes(label=z,x=x,y=value),vjust=1.4) +
  scale_fill_manual(label=c("正向","中性","负向"),limits=c("pos","neu","neg"),name=("情感倾向"),values=wes_palette(3,name = "Royal1")[c(2,3,1)])+
  scale_y_continuous(labels = scales::percent)+
  geom_text(aes(label=paste(100*round(freq,2),"%",", ",n,sep=""),x=variable,y=y),vjust=0.4) + xlab('演员姓名') + 
  ylab('情感占比')  +theme_classic() +
  theme(text = element_text(family = 'Yuppy SC'))

###################

#CAST BY TYPE SENTIMENT
###################
mydf$kt=grepl('台词|配音|口条|声音', mydf$texts);sum(mydf$kt)
mydf$yj=grepl('演技', mydf$texts);sum(mydf$yj)
mydf$zx=grepl('颜|看|长[相的得]|外貌|身', mydf$texts);sum(mydf$zx)

dfin=mydf

dfin %>% filter(f1==1&yj==1) %>% 
  group_by(type) %>%
  summarise(n=n()) %>%
  mutate(freq=n/sum(n)) %>%
  filter(type==2)

#
dfin = mydf[mydf$zx==1,]
pdf_sa = data.frame(f1=as.vector(table(dfin[dfin$f1==1,"type"])),
                    m1=as.vector(table(dfin[dfin$m1==1,"type"])),
                    m2=as.vector(table(dfin[dfin$m2==1,"type"])),
                    f2=as.vector(table(dfin[dfin$f2==1,"type"])))
pdf_sa$id = c("neg","neu","pos");pdf_sa = melt(pdf_sa,id="id")
pdf_sa = pdf_sa %>%
  group_by(variable,id) %>%
  summarise (n = value) %>%
  mutate(freq = n / sum(n)) %>%
  mutate(y = cumsum(freq)-0.5*freq)

ggplot(pdf_sa)+geom_bar(aes(x=variable,fill=factor(id, levels=c("pos","neu","neg")),y=n),stat='identity',position='fill') +
  scale_x_discrete(label=c("女一","男一","男二","女二")) + #geom_text(aes(label=z,x=x,y=value),vjust=1.4) +
  scale_fill_manual(label=c("正向","中性","负向"),limits=c("pos","neu","neg"),name=("情感倾向"),values=wes_palette(3,name = "Royal1")[c(2,3,1)])+
  scale_y_continuous(labels = scales::percent)+
  geom_text(aes(label=paste(100*round(freq,2),"%",", ",n,sep=""),x=variable,y=y),vjust=0.4) + xlab('演员/角色的外貌相关评论') + 
  ylab('情感占比')  +theme_classic() +
  theme(text = element_text(family = 'Yuppy SC'))

###################

#PROD SENTIMENT
###################
dfin=mydf
pdf_prod = data.frame(jm=as.vector(table(dfin[grepl('演员', dfin$texts),"type"])),
                      xf=as.vector(table(dfin[grepl('导演|摄影|摄像|剪辑', dfin$texts),"type"])),
                      ry=as.vector(table(dfin[grepl('编剧|原著|原作|作者', dfin$texts),"type"])),
                      th=as.vector(table(dfin[grepl('服化道|服装|化妆|造型|道具|打光', dfin$texts),"type"])),
                      ss=as.vector(table(dfin[grepl('音乐|歌|曲', dfin$texts),"type"])),
                      tx=as.vector(table(dfin[grepl('特效', dfin$texts),"type"])))
pdf_prod$id = c("neg","neu","pos");pdf_prod = melt(pdf_prod,id="id")
pdf_prod = pdf_prod %>%
  group_by(variable,id) %>%
  summarise (n = value) %>%
  mutate(freq = n / sum(n)) %>%
  mutate(y = cumsum(freq)-0.5*freq)

ggplot(pdf_prod)+geom_bar(aes(x=variable,fill=factor(id, levels=c("pos","neu","neg")),y=n),stat='identity',position='fill') +
  scale_x_discrete(label=c("演员","导演","编剧","服化道","音乐","特效")) + #geom_text(aes(label=z,x=x,y=value),vjust=1.4) +
  scale_fill_manual(label=c("正向","中性","负向"),limits=c("pos","neu","neg"),name=("情感倾向"),values=wes_palette(3,name = "Royal1")[c(2,3,1)])+
  scale_y_continuous(labels = scales::percent)+
  geom_text(aes(label=paste(100*round(freq,2),"%",", ",n,sep=""),x=variable,y=y),vjust=0.4) + xlab('制作') + 
  ylab('情感占比')  +theme_classic() +
  theme(text = element_text(family = 'Yuppy SC'))

#############################
#  BY 天涯论坛 @六界第一美男瓜  #
#############################
