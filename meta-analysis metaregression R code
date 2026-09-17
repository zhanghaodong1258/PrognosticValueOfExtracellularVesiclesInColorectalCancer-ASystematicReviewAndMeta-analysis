```R
#参考代码
#Load the required libraries for this tutorial
install.packages('metafor')
install.packages('readxl')
library(metafor)
library(readxl)

#Load the Artificial MEta-analysis dataset and explore it
mdata <- read_excel(
  "../data.xlsx")
View(mregdat) 
#We will use the Risk ratios and their  95% Confidence intervals to :
#derive log confidence intervals - logci=log(ci)
#derive logRRs - logRR=log(RR)
#derive standard errors of the log RRs  
#SE(logRR)=lowerci(logRR)-upperci(logRR)/3.92
mdata['logRR']=log(mdata$rr)
mdata['loglci']=log(mdata$lci)
mdata['loguci']=log(mdata$uci)
mdata['logRRse']=(mdata$loguci-mdata$loglci)/3.92
mdata['mod']=((mdata$logRR*1.75)+6+rnorm(19,0,0.1))

#Create DerSimonian&Laird meta-regression object 
#Summarize the results
meta_dl=rma(yi=mdata$logRR, 
            sei=mdata$logRRse,
            mods = mdata$mod,
            method = 'DL', measure = 'RR')

summary(meta_dl)

#Create DL meta-regression plot using logRR scale
#The meta-regression  plot will contain bubbles and regression lines
#Bubbles - individual studies - the larger the bubble the larger the weight
#of the study (inversely proportional to the variance and standard error)
mregplot=regplot(meta_dl, 
                  lcol='red', 
                  col = 'blue',
                  level=0.95)

#Added the labels of the studies to a meta-regression plot
#Added the transf=exp to convert logRR to RR scale
mregplotRR=regplot(meta_dl, 
                  lcol='red', 
                  col = 'blue',
                  level=0.95, 
                  label = TRUE, 
                  transf = exp )
```

```R
#我的部分代码
#安装包
meta
openxlsx
#加载包
。。。
#读入数据
data = read.xlsx("C:\\Users\\zhd\\Documents\\meta\\data.xlsx") 
attach(data)
#计算效应量
data$hrlog = log(HR); 
data$cillog = log(CI_L);
data$ciulog = log(CI_U); 
data$log_se = (log(CI_U)-log(OS_CI_L))/3.92
#Meta分析
data_m = metagen(data$hrlog, 
                 data$log_se, sm="HR", 
                 studlab = paste0(data$author,'/', s$year, '/', data$外泌体标志物), 
                 comb.fixed = F, 
                 comb.random = T
                )
#绘图
#森林图
forest(data_m)
#漏斗图
funnel(data_m)
#meta回归
t = metareg(data_m, year)
#气泡图
bubble(t)
#
forest(metainf(data_m, pooled = "random"))
```


