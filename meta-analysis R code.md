# meta-analysis R语言代码

## 1.合并HR

```R
#文件要预处理，不能有汉字、——等等的汉语字符
#德语字母特殊处理，文件数据中有中文字符，并且使用ANSI编码，将文件另存为并改为utf-8编码，就能正常读入
#study_info = read.csv2("study_info.txt", header = T, sep = "\t", check.names = F)
library(openxlsx)
library(metafor)
library(meta)
study_info = read.xlsx("study_info.xlsx",  sheet = 1, check.names = F, colNames = T, rowNames = T) #使用xlsx读入，支持德语字母
OS_uni_uni = read.xlsx("OS-单分子-单因素.xlsx", sheet = 1, check.names = F, colNames = T, rowNames = T)
#meta分析

#Section1.1 单分子，单因素，OS
OS_uni_uni = read.xlsx("OS-单分子-单因素.xlsx", sheet = 1, check.names = F, colNames = T, rowNames = T)
OS_uni_uni$OS_Hazard.ratio = round(OS_uni_uni$OS_Hazard.ratio, digits = 2)
temp = study_info[rownames(OS_uni_uni), ]
OS_uni_uni$Disease.stage = temp$Disease.stage #加入疾病阶段属性
#计算需要合并的效应量
OS_uni_uni$HRlog = log(OS_uni_uni$OS_Hazard.ratio)
OS_uni_uni$CI_L_log = log(OS_uni_uni$`OS_95%.CI_L`)
OS_uni_uni$CI_U_log = log(OS_uni_uni$`OS_95%.CI_U`)
OS_uni_uni$log_SE = (OS_uni_uni$CI_U_log - OS_uni_uni$CI_L_log) / 3.92
OS_uni_uni$Study = rownames(OS_uni_uni)
#进行meta分析
OS_uni_uni.meta = metagen(OS_uni_uni$HRlog, OS_uni_uni$log_SE, sm="HR", studlab = paste0(OS_uni_uni$Study, "/", OS_uni_uni$Biomarker.of.EV), comb.fixd = F, comb.random = T, common = F); forest(OS_uni_uni.meta)
#加标签
OS_uni_uni.meta = metagen(OS_uni_uni$HRlog, OS_uni_uni$log_SE, sm="HR", studlab = paste0(OS_uni_uni$Study, "/", OS_uni_uni$Biomarker.of.EV), common = F, label.right = "Mortality", label.left = "Survival"); forest(OS_uni_uni.meta)
#绘图
png("OS_uni_uni.png", height = 4600, width = 6800,units = "px", res = 600)
forest(OS_uni_uni.meta)
dev.off()

#亚组分析
#亚组分析时需要去掉分组依据中的空值
data = OS_uni_uni
data = data[!is.na(data$Disease.stage), ]
sub_OS_uni_uni.meta = metagen(data$HRlog, data = data, data$log_SE, sm="HR", studlab = paste0(data$Study, "/", data$Biomarker.of.EV), comb.fixd = F, comb.random = T, common = F, byvar = data$Disease.stage); forest(sub_OS_uni_uni.meta)
#处理亚组，规范分析
png("sub1_OS_uni_uni.png", height = 7200, width = 8100, res = 650)
forest(sub_OS_uni_uni.meta)
dev.off()
sub_OS_uni_uni.meta = metagen(
    data$HRlog, 
    data = data, 
    data$log_SE, 
    sm="HR", 
    studlab = paste0(data$`Author,.publication.year`, "/", data$Biomarker.of.EV), 
    common = F, 
    byvar = data$Classification.of.Biomarker, 
    label.right = "Mortality", 
    label.left = "Survival", 
    text.random = ""); graph(sub_OS_uni_uni.meta) #去掉random标识
sub_OS_uni_uni.meta = metagen(data$HRlog, data = data, data$log_SE, sm="HR", studlab = paste0(data$Study, "/", data$Biomarker.of.EV),  common = F, byvar = data$Classification.of.Biomarker, label.right = "Mortality", label.left = "Survival", text.random = "", print.subgroup.name = F); forest(sub_OS_uni_uni.meta) #去掉亚组标识的前缀
png("sub1_OS_uni_uni.png", height = 7200, width = 8500, res = 700)
forest(sub_OS_uni_uni.meta)
dev.off()

#重新做
rownames(OS_uni_uni)= OS_uni_uni$`Author,.publication.year`
rownames(study_info) = study_info$`Author,.publication.year`
temp = study_info[rownames(OS_uni_uni), ]
OS_uni_uni$Disease.stage = temp$Disease.stage
OS_uni_uni$Stage = temp$Stage

OS_uni_uni$HRlog = log(OS_uni_uni$OS_Hazard.ratio)
OS_uni_uni$CI_L_log = log(OS_uni_uni$`OS_95%.CI_L`)
OS_uni_uni$CI_U_log = log(OS_uni_uni$`OS_95%.CI_U`)
OS_uni_uni$log_SE = (OS_uni_uni$CI_U_log - OS_uni_uni$CI_L_log) / 3.92

OS_uni_uni.meta = metagen(
    OS_uni_uni$HRlog, 
    OS_uni_uni$log_SE, sm="HR", 
    studlab = paste0(OS_uni_uni$`Author,.publication.year`, "/", OS_uni_uni$Biomarker.of.EV), 
    common = F
    ); forest(OS_uni_uni.meta)
graph = function(p){
    pdf("1.pdf", height = 50, width = 50)
    forest(p)
    dev.off()
}
graph(OS_uni_uni.meta) #函数分装
OS_uni_uni.meta = metagen(
    OS_uni_uni$HRlog, 
    OS_uni_uni$log_SE, 
    sm="HR", 
    studlab = paste0(OS_uni_uni$`Author,.publication.year`, "/", OS_uni_uni$Biomarker.of.EV), 
    common = F, 
    label.right = "Mortality", 
    label.left = "Survival", 
    text.random = ""); forest(OS_uni_uni.meta); graph(OS_uni_uni.meta)
#Section1.1 OS单分子单因素的亚组分析
p.meta = metagen(
    OS_uni_uni$HRlog, 
    OS_uni_uni$log_SE, 
    sm="HR", 
    studlab = paste0(OS_uni_uni$`Author,.publication.year`, "/", OS_uni_uni$Biomarker.of.EV), 
    common = F, 
    label.right = "Mortality", 
    label.left = "Survival", 
    print.subgroup.name = F, 
    text.random = ""); graph(p.meta)
#按照分子标记物的种类分类
p.meta = metagen(
    OS_uni_uni$HRlog, 
    OS_uni_uni$log_SE, 
    sm = "HR", 
    byvar = OS_uni_uni$Classification.of.Biomarker, 
    studlab = paste0(OS_uni_uni$`Author,.publication.year`, "/", OS_uni_uni$Biomarker.of.EV), 
    common = F, 
    label.right = "Mortality", 
    label.left = "Survival", 
    print.subgroup.name = F,
    text.random = ""); graph(p.meta)
#按照疾病阶段分
data = OS_uni_uni[!is.na(OS_uni_uni$Stage), ] #去空值
p.meta = metagen(
    data$HRlog, 
    data$log_SE, 
    sm = "HR", 
    byvar = data$Classification.of.Biomarker, 
    studlab = paste0(data$`Author,.publication.year`, "/", data$Biomarker.of.EV), 
    common = F, 
    label.right = "Mortality", 
    label.left = "Survival", 
    print.subgroup.name = F,
    text.random = ""); graph(p.meta)
#all stage
res.meta = metagen(
    subset = (data$Disease.stage == c("All stages")),  
    data$HRlog, 
    data$log_SE, 
    sm = "HR", 
    studlab = paste0(data$`Author,.publication.year`, " /", data$Biomarker.of.EV), 
    common = F, 
    label.right = "Mortality", 
    label.left = "Survival", 
    print.subgroup.name = F,
    text.random = ""); graph(res.meta)
#resectable
p.meta = metagen(
    subset = (data$Disease.stage == c("resectable")),  
    data$HRlog, 
    data$log_SE, 
    sm = "HR", 
    studlab = paste0(data$`Author,.publication.year`, "/", data$Biomarker.of.EV), 
    common = F, 
    label.right = "Mortality", 
    label.left = "Survival", 
    print.subgroup.name = F,
    text.random = ""); graph(p.meta)
#metastatic
p.meta = metagen(
    subset = (data$Disease.stage == c("metastatic")),  
    data$HRlog, 
    data$log_SE, 
    sm = "HR", 
    studlab = paste0(data$`Author,.publication.year`, "/", data$Biomarker.of.EV), 
    common = F, 
    label.right = "Mortality", 
    label.left = "Survival", 
    print.subgroup.name = F,
    text.random = ""); graph(p.meta)
#按照EV subgroup划分
temp = study_info[rownames(data), ]
data$EV.Subgroup = temp$EV.Subgroup
p.meta = metagen(
    byvar = data$EV.Subgroup,  
    data$HRlog, 
    data$log_SE, 
    sm = "HR", 
    studlab = paste0(data$`Author,.publication.year`, "/", data$Biomarker.of.EV), 
    common = F, 
    label.right = "Mortality", 
    label.left = "Survival", 
    print.subgroup.name = F,
    text.random = ""); graph(p.meta)

#Section 1.2 OS—单分子—多因素
OS_uni_muti = read.xlsx("OS-单分子-多因素.xlsx", check.names = F, sheet = 1)
rownames(OS_uni_muti) = OS_uni_muti$`Author,.publication.year`
temp = study_info[rownames(OS_uni_muti), ]
OS_uni_muti$EV.Subgroup = temp$EV.Subgroup
OS_uni_muti$Disease.stage = temp$Disease.stage
OS_uni_muti$Stage = temp$Stage
OS_uni_muti$HRlog = log(OS_uni_muti$OS_Hazard.ratio)
OS_uni_muti$CI_L_log = log(OS_uni_muti$`OS_95%.CI_L`)
OS_uni_muti$CI_U_log = log(OS_uni_muti$`OS_95%.CI_U`)
OS_uni_muti$log_SE = (OS_uni_muti$CI_U_log - OS_uni_muti$CI_L_log) / 3.92
#总体
p.meta = metagen(
    data$HRlog, 
    data$log_SE, 
    sm = "HR", 
    studlab = paste0(data$`Author,.publication.year`, "/", data$Biomarker.of.EV), 
    common = F, 
    label.right = "Mortality", 
    label.left = "Survival", 
    print.subgroup.name = F,
    text.random = ""); graph(p.meta)
#EV亚组
p.meta = metagen(
    byvar = data$EV.Subgroup,  
    data$HRlog, 
    data$log_SE, 
    sm = "HR", 
    studlab = paste0(data$`Author,.publication.year`, "/", data$Biomarker.of.EV), 
    common = F, 
    label.right = "Mortality", 
    label.left = "Survival", 
    print.subgroup.name = F,
    text.random = ""); graph(p.meta)
#classfication 亚组
p.meta = metagen(
    #subset = (data$Classification.of.Bioarker != c("mRNA")), #控制是否含单个研究
    byvar = data$Classification.of.Bioarker,  
    data$HRlog, 
    data$log_SE, 
    sm = "HR", 
    studlab = paste0(data$`Author,.publication.year`, "/", data$Biomarker.of.EV), 
    common = F, 
    label.right = "Mortality", 
    label.left = "Survival", 
    print.subgroup.name = F,
    text.random = ""); graph(p.meta)
#all stages
p.meta = metagen(
    subset = (data$Disease.stage == "All stages"),  
    data$HRlog, 
    data$log_SE, 
    sm = "HR", 
    studlab = paste0(data$`Author,.publication.year`, "/", data$Biomarker.of.EV), 
    common = F, 
    label.right = "Mortality", 
    label.left = "Survival", 
    print.subgroup.name = F,
    text.random = ""); graph(p.meta)
#metastatic
p.meta = metagen(
    subset = (data$Disease.stage == "metastatic"),  
    data$HRlog, 
    data$log_SE, 
    sm = "HR", 
    studlab = paste0(data$`Author,.publication.year`, "/", data$Biomarker.of.EV), 
    common = F, 
    label.right = "Mortality", 
    label.left = "Survival", 
    print.subgroup.name = F,
    text.random = ""); graph(p.meta)
#Section 1.3 OS-多分子—单因素
OS_muti_uni = read.xlsx("OS-多分子-单因素.xlsx", sheet = 1, check.names = F)
rownames(OS_muti_uni)= OS_muti_uni$`Author,.publication.year`
temp = study_info[rownames(OS_muti_uni), ]
OS_muti_uni$Disease.stage = temp$Disease.stage
OS_muti_uni$Stage = temp$Stage
OS_muti_uni$EV.Subgroup = temp$EV.Subgroup
data = OS_muti_uni
data$HRlog = log(data$Hazard.ratio)
data$CI_L_log = log(data$`95%.CI_L`)
data$CI_U_log = log(data$`95%.CI_U`)
data$log_SE = (data$CI_U_log - data$CI_L_log) / 3.92
p.meta = metagen(
    #subset = (data$Disease.stage == "All stages"),  
    data$HRlog, 
    data$log_SE, 
    sm = "HR", 
    studlab = paste0(data$`Author,.publication.year`, "/", data$Biomarker.of.EV), 
    common = F, 
    label.right = "Mortality", 
    label.left = "Survival", 
    print.subgroup.name = F,
    text.random = ""); graph(p.meta)

#Section 1.4 OS—多分子—多因素
OS_muti_muti = read.xlsx("OS-多分子-多因素.xlsx", sheet = 1, check.names = F)
rownames(OS_muti_muti)= OS_muti_muti$`Author,.publication.year`
temp = study_info[rownames(OS_muti_muti), ]
OS_muti_muti$Disease.stage = temp$Disease.stage
OS_muti_muti$Stage = temp$Stage
OOS_muti_muti$EV.Subgroup = temp$EV.Subgroup
data = OS_muti_muti
data$HRlog = log(data$Hazard.ratio)
data$CI_L_log = log(data$`95%.CI_L`)
data$CI_U_log = log(data$`95%.CI_U`)
data$log_SE = (data$CI_U_log - data$CI_L_log) / 3.92
p.meta = metagen(
    #subset = (data$Disease.stage == "All stages"),  
    data$HRlog, 
    data$log_SE, 
    sm = "HR", 
    studlab = paste0(data$`Author,.publication.year`, "/", data$Biomarker.of.EV), 
    common = F, 
    label.right = "Mortality", 
    label.left = "Survival", 
    print.subgroup.name = F,
    text.random = ""); graph(p.meta)
#Section 1.5 EV的数量  
OS_EV_count = read.xlsx("OS-EV数量-单因素.xlsx", sheet = 1, check.names = F)
rownames(OS_EV_count)= OS_EV_count$`Author,.publication.year`
temp = study_info[rownames(OS_EV_count), ]
OS_EV_count$Disease.stage = temp$Disease.stage
OS_EV_count$Stage = temp$Stage
OS_EV_count$EV.Subgroup = temp$EV.Subgroup
data = OS_EV_count
data$HRlog = log(data$Hazard.ratio)
data$CI_L_log = log(data$`95%.CI_L`)
data$CI_U_log = log(data$`95%.CI_U`)
data$log_SE = (data$CI_U_log - data$CI_L_log) / 3.92
p.meta = metagen(
    #subset = (data$Disease.stage == "All stages"),  
    data$HRlog, 
    data$log_SE, 
    sm = "HR", 
    studlab = paste0(data$`Author,.publication.year`, "/", data$Biomarker.of.EV), 
    random = F, 
    label.right = "Mortality", 
    label.left = "Survival", 
    print.subgroup.name = F,
    text.random = ""); graph(p.meta)
#Section 2 PFS
PFS = read.xlsx("PFS-单分子-单因素.xlsx", sheet = 1, check.names = F)
rownames(PFS)= PFS$`Author,.publication.year`
temp = study_info[rownames(PFS), ]
PFS$Disease.stage = temp$Disease.stage
PFS$Stage = temp$Stage
PFS$EV.Subgroup = temp$EV.Subgroup
data = PFS
data$HRlog = log(data$Hazard.ratio)
data$CI_L_log = log(data$`95%.CI_L`)
data$CI_U_log = log(data$`95%.CI_U`)
data$log_SE = (data$CI_U_log - data$CI_L_log) / 3.92
p.meta = metagen(
    #subset = (data$Disease.stage == "All stages"),  
    data$HRlog, 
    data$log_SE, 
    sm = "HR", 
    studlab = paste0(data$`Author,.publication.year`, "/", data$Biomarker.of.EV), 
    common = F, 
    label.right = "Mortality", 
    label.left = "Survival", 
    print.subgroup.name = F,
    text.random = ""); graph(p.meta)
#Section 3.1 单分子单因素RFS
RFS = read.xlsx("RFS-单分子-单因素.xlsx", sheet = 1, check.names = F)
rownames(RFS)= RFS$`Author,.publication.year`
temp = study_info[rownames(RFS), ]
RFS$Disease.stage = temp$Disease.stage
RFS$Stage = temp$Stage
RFS$EV.Subgroup = temp$EV.Subgroup
data = RFS
data$HRlog = log(data$Hazard.ratio)
data$CI_L_log = log(data$`95%.CI_L`)
data$CI_U_log = log(data$`95%.CI_U`)
data$log_SE = (data$CI_U_log - data$CI_L_log) / 3.92
p.meta = metagen(
    #subset = (data$Disease.stage == "All stages"),  
    data$HRlog, 
    data$log_SE, 
    sm = "HR", 
    studlab = paste0(data$`Author,.publication.year`, "/", data$Biomarker.of.EV), 
    common = F, 
    label.right = "Mortality", 
    label.left = "Survival", 
    print.subgroup.name = F,
    text.random = ""); graph(p.meta)

#Section 3.2 RFS多分子单因素
RFS_muti_uni = read.xlsx("RFS-多分子-单因素.xlsx", sheet = 1, check.names = F)
rownames(RFS_muti_uni)= RFS_muti_uni$`Author,.publication.year`
temp = study_info[rownames(RFS_muti_uni), ]
RFS_muti_uni$Disease.stage = temp$Disease.stage
RFS_muti_uni$Stage = temp$Stage
RFS_muti_uni$EV.Subgroup = temp$EV.Subgroup
data = RFS_muti_uni
data$HRlog = log(data$Hazard.ratio)
data$CI_L_log = log(data$`95%.CI_L`)
data$CI_U_log = log(data$`95%.CI_U`)
data$log_SE = (data$CI_U_log - data$CI_L_log) / 3.92
p.meta = metagen(
    #subset = (data$Disease.stage == "All stages"),  
    data$HRlog, 
    data$log_SE, 
    sm = "HR", 
    studlab = paste0(data$`Author,.publication.year`, "/", data$Biomarker.of.EV), 
    common = F, 
    label.right = "Mortality", 
    label.left = "Survival", 
    print.subgroup.name = F,
    text.random = ""); graph(p.meta)

#Section 3.3 RFS多分子多因素
RFS_muti_muti = read.xlsx("RFS-多分子-多因素.xlsx", sheet = 1, check.names = F)
rownames(RFS_muti_muti)= RFS_muti_muti$`Author,.publication.year`
temp = study_info[rownames(RFS_muti_muti), ]
RFS_muti_muti$Disease.stage = temp$Disease.stage
RFS_muti_muti$Stage = temp$Stage
RFS_muti_muti$EV.Subgroup = temp$EV.Subgroup
data = RFS_muti_muti
data$HRlog = log(data$Hazard.ratio)
data$CI_L_log = log(data$`95%.CI_L`)
data$CI_U_log = log(data$`95%.CI_U`)
data$log_SE = (data$CI_U_log - data$CI_L_log) / 3.92
p.meta = metagen(
    #subset = (data$Disease.stage == "All stages"),  
    data$HRlog, 
    data$log_SE, 
    sm = "HR", 
    studlab = paste0(data$`Author,.publication.year`, "/", data$Biomarker.of.EV), 
    common = F, 
    label.right = "Mortality", 
    label.left = "Survival", 
    print.subgroup.name = F,
    text.random = ""); graph(p.meta)
```

```R
#备查原始代码
#R语言，meta包
#读入数据：
#安装并加载meta, openxlsx包
data = read.xlsx("C:/Users/zhanghaodong/Desktop/汇总-多变量.xlsx", sheet = 1) 
#计算效应量In值：
data$hrlog = log(OS_HR)
data$cillog = log(OS_CI_L);
data$ciulog = log(OS_CI_U); 
data$log_se = (log(OS_CI_U)-log(OS_CI_L))/3.92
#Meta分析：
data_m = metagen (data$hrlog, data$log_se, sm="HR", studlab = paste0(data$author,'/', s$year, '/', data$外泌体标志物), comb.fixed = F, comb.random = T)
#绘图 
forest(data_m)
funnel(data_m)
t = metareg(data_m, year); bubble(t)
forest(metainf(data_m, pooled = "random"))
```

## 2.敏感性检验

```R
#Section 2.1 敏感性分析
#OS_单分子单因素
temp = study_info[rownames(OS_uni_uni), ]
OS_uni_uni$Disease.stage = temp$Disease.stage
OS_uni_uni$Stage = temp$Stage
OS_uni_uni$EV.Subgroup = temp$EV.Subgroup
data = OS_uni_uni
res.meta = metagen(
    data$HRlog, 
    data$log_SE, 
    sm="HR", 
    studlab = paste0(data$`Author,.publication.year`, "/", data$Biomarker.of.EV), 
    common = F, 
    label.right = "Mortality", 
    label.left = "Survival", 
    print.subgroup.name = F, 
    text.random = ""); forest(res.meta)
#graph((metainf(res.meta), comb.fixed = TRUE))
res.inf = metainf(res.meta, pooled = "random"); graph(res.inf)
#OS_单分子多因素
data = OS_uni_muti
res.meta = metagen(
    data$HRlog, 
    data$log_SE, 
    sm="HR", 
    studlab = paste0(data$`Author,.publication.year`, "/", data$Biomarker.of.EV), 
    common = F, 
    label.right = "Mortality", 
    label.left = "Survival", 
    print.subgroup.name = F, 
    text.random = ""); forest(res.meta)
res.inf = metainf(res.meta, pooled = "random"); graph(res.inf)
#OS_多分子单因素
data = OS_muti_uni
res.meta = metagen(
    data$HRlog, 
    data$log_SE, 
    sm="HR", 
    studlab = paste0(data$`Author,.publication.year`, "/", data$Biomarker.of.EV), 
    common = F, 
    label.right = "Mortality", 
    label.left = "Survival", 
    print.subgroup.name = F, 
    text.random = ""); forest(res.meta)
res.inf = metainf(res.meta, pooled = "random"); graph(res.inf)
#OS_多分子多因素
data = OS_muti_muti
res.meta = metagen(
    data$HRlog, 
    data$log_SE, 
    sm="HR", 
    studlab = paste0(data$`Author,.publication.year`, "/", data$Biomarker.of.EV), 
    common = F, 
    label.right = "Mortality", 
    label.left = "Survival", 
    print.subgroup.name = F, 
    text.random = ""); forest(res.meta)
res.inf = metainf(res.meta, pooled = "random"); graph(res.inf)
#OS_EV count
data = OS_EV_count
res.meta = metagen(
    data$HRlog, 
    data$log_SE, 
    sm="HR", 
    studlab = paste0(data$`Author,.publication.year`, "/", data$Biomarker.of.EV), 
    random = F, #common合并
    label.right = "Mortality", 
    label.left = "Survival", 
    print.subgroup.name = F, 
    text.random = ""); forest(res.meta)
res.inf = metainf(res.meta, pooled = "fixed"); graph(res.inf)
#PFS
data = PFS
res.meta = metagen(
    data$HRlog, 
    data$log_SE, 
    sm="HR", 
    studlab = paste0(data$`Author,.publication.year`, "/", data$Biomarker.of.EV), 
    common = F, #common合并
    label.right = "Mortality", 
    label.left = "Survival", 
    print.subgroup.name = F, 
    text.random = ""); forest(res.meta)
res.inf = metainf(res.meta, pooled = "random"); graph(res.inf)
#RFS_单分子单因素
data = RFS
res.meta = metagen(
    data$HRlog, 
    data$log_SE, 
    sm="HR", 
    studlab = paste0(data$`Author,.publication.year`, "/", data$Biomarker.of.EV), 
    common = F, #common合并
    label.right = "Mortality", 
    label.left = "Survival", 
    print.subgroup.name = F, 
    text.random = ""); forest(res.meta)
res.inf = metainf(res.meta, pooled = "random"); graph(res.inf)
#RFS_多分子单因素
data = RFS_muti_uni
res.meta = metagen(
    data$HRlog, 
    data$log_SE, 
    sm="HR", 
    studlab = paste0(data$`Author,.publication.year`, "/", data$Biomarker.of.EV), 
    common = F, #common合并
    label.right = "Mortality", 
    label.left = "Survival", 
    print.subgroup.name = F, 
    text.random = ""); forest(res.meta)
res.inf = metainf(res.meta, pooled = "random"); graph(res.inf)
#RFS_多分子多因素
data = RFS_muti_muti
res.meta = metagen(
    data$HRlog, 
    data$log_SE, 
    sm="HR", 
    studlab = paste0(data$`Author,.publication.year`, "/", data$Biomarker.of.EV), 
    common = F, #common合并
    label.right = "Mortality", 
    label.left = "Survival", 
    print.subgroup.name = F, 
    text.random = ""); forest(res.meta)
res.inf = metainf(res.meta, pooled = "random"); graph(res.inf)
```

## 3.发表偏倚

```R
#处理字符串，加年份标注 
res = as.numeric(gsub(".*?([0-9]+).*", "\\1", study_info$`Author,.publication.year`))
study_info$Year = res
temp = study_info[rownames(OS_uni_uni), ]
OS_uni_uni$Year = temp$Year
#表格整理
OS_uni_uni = read.xlsx("OS-单分子-单因素.xlsx", sheet = 1, check.names = F)
rownames(OS_uni_uni) = OS_uni_uni$`Author,.publication.year`
temp = study_info[rownames(OS_uni_uni), ]
OS_uni_uni$Disease.stage = temp$Disease.stage
OS_uni_uni$Stage = temp$Stage
OS_uni_uni$EV.Subgroup = temp$EV.Subgroup
OS_uni_uni$Year = temp$Year

#产生meta对象
data = OS_uni_uni
res.meta = metagen(
    data = data, 
    data$HRlog, 
    data$log_SE, 
    sm="HR", 
    studlab = paste0(data$`Author,.publication.year`, "/", data$Biomarker.of.EV), 
    common = F, 
    label.right = "Mortality", 
    label.left = "Survival", 
    print.subgroup.name = F, 
    text.random = ""); forest(res.meta)
#漏斗图及轮廓增强漏斗图
funnel(res.meta)
funnel(res.meta, 
       #xlim = c(-0.5, 20), 
       pch = 20,
       cex = .8, 
       contour.levels = c(0.9, 0.95, 0.99), 
       studlab = F,
       col.contour = c("darkgray", "gray", "lightgray")
      )
legend(x = 5, y = 0, #p值坐标
       c("p<0.01", "0.01<p<0.05", "0.05<p<0.1"), 
       bty = "n",
       fill = c("lightgray", "gray", "darkgray")
      )
#画图
pdf("1.pdf", height = 8, width = 8)
funnel(res.meta, 
       #xlim = c(-0.5, 20), 
       pch = 20,
       cex = .8, 
       contour.levels = c(0.9, 0.95, 0.99), 
       studlab = F,
       col.contour = c("darkgray", "gray", "lightgray")
      )
legend(x = 5, y = 0, #p值坐标
       c("p<0.01", "0.01<p<0.05", "0.05<p<0.1"),  
       bty = "n",
       fill = c("lightgray", "gray", "darkgray")
      )
dev.off()
#作图代码封装
graph.f = function(p){
    pdf("轮廓图.pdf", height = 8, width = 8)
    funnel(p)
    dev.off()
    pdf("轮廓增强图.pdf", height = 8, width = 8)
    funnel(p, 
           #xlim = c(-0.5, 20), 
           pch = 20,
           cex = .8, 
           contour.levels = c(0.9, 0.95, 0.99), 
           studlab = F,
           col.contour = c("darkgray", "gray", "lightgray"))
    legend(x = 5, y = 0, #p值坐标
           c("p<0.01", "0.01<p<0.05", "0.05<p<0.1") , 
           bty = "n",
           fill = c("lightgray", "gray", "darkgray"))
    dev.off()
}
graph_trim.f = function(p){
    pdf("轮廓剪补图.pdf", height = 8, width = 8)
    funnel(p)
    dev.off()
    pdf("轮廓增强剪补图.pdf", height = 8, width = 8)
    funnel(p, 
           #xlim = c(-0.5, 20), 
           pch = 1,
           cex = .8, 
           contour.levels = c(0.9, 0.95, 0.99), 
           studlab = F,
           col.contour = c("darkgray", "gray", "lightgray"))
    legend(x = 5, y = 0, #p值坐标
          c("p<0.01", "0.01<p<0.05", "0.05<p<0.1") , 
           bty = "n",
           fill = c("lightgray", "gray", "darkgray"))
    dev.off()
}
#剪补法
res.trim = trimfill(res.meta, 
                    ma.random = T, #默认用固定效应模型减补
                    common = FALSE, 
                    random = TRUE) 
graph_trim.f(res.trim)
#Egger’s法
radial(res.meta)
graph.rad = function(p){
    pdf("radial.pdf", height = 8, width = 8)
    radial(p)
    dev.off()
}
graph.rad(res.meta)
#Egger’s法检验
metabias(res.meta, method.bias = "linreg", plotit = T)
#Egger测试，通常只需报告截距值、其 95% 置信区间以及t和p-价值; intercept, its 95% confidence interval, as well as the t and p-value
#截距非0，做截距非0的假设检验
#Peters’ Regression Test
#metabias(res.meta, method.bias = "peters")
#检验年份发表偏倚，气泡图
#做漏斗图，大于十项研究做meta回归，检验偏倚
#检验年份，检验年份、标志物分组、疾病阶段等
metareg(res.meta, data$Year) #无贡献
metareg(res.meta, data$EV.Subgroup) #无贡献
metareg(res.meta, data$Classification.of.Biomarker) #有贡献
metareg(res.meta, data$`High/Low.to.poor.prognosis`) #无贡献
metareg(res.meta, data$Disease.stage) #无贡献
metareg(res.meta, data$Biomarker.of.EV) #识别到的有贡献的标记物 
#buble图

#查看个别研究对异质性和结果的贡献
baujat(res.meta)
data = OS_uni_uni
res.meta = metagen(
    data = data, 
    data$HRlog, 
    data$log_SE, 
    sm="HR", 
    studlab = paste0(data$`Author,.publication.year`, "/", data$Biomarker.of.EV), 
    common = F, 
    label.right = "Mortality", 
    label.left = "Survival", 
    print.subgroup.name = F, 
    #text.random = ""
)
forest(res.meta, 
       lwd = 1.5, #设置线的宽度
       leftcols = "studlab", # 左边展示内容，不显示logHR
       #leftlabs = c("Study"),
       #colgap.forest.left = unit(0.3,"inches"), #左边与森林间距，需要加载grid包
       rightcols = c("effect", "ci", "w.random"), # 右边展示内容
       rightlabs = c("HR", "95% CI", "Weight"), 
       print.tau2 = F 
      ) 
forest(res.meta, 
       xlab.pos = 1, 
       #xlim = c(0.1 100), 
       lwd = 1.5, 
       header.line = T, 
       spacing = 1.5, #行距
       addrow = F, #结果上的空行
       addrow.overall = F, #亚组分析时用
       addrow.subgroups	= F, 
       addrows.below.overall = 0, 
       #bottom.lr = T, 
       #hetlab = "111", #异质性检验标签前面的文字, resid.hetlab = 
       leftcols = "studlab", # 左边展示内容，#不显示logHR
       #leftlabs = c("Study"),
       #colgap.forest.left = unit(0.3,"inches"), #左边与森林间距，需要加载grid包
       rightcols = c("effect", "ci", "w.random"), # 右边展示内容
       rightlabs = c("HR", "95% CI", "Weight"), 
       print.tau2 = F
      ) 



#最终的代码
res.meta = metagen(
    #subset = data[(data$Disease.stage == "resctabel"), ], 
    data = data, 
    data$HRlog, 
    data$log_SE, 
    sm="HR", 
    studlab = paste0(data$`Author,.publication.year`, " /", data$Biomarker.of.EV), 
    common = F, 
    label.right = "Mortality", 
    label.left = "Survival", 
    print.subgroup.name = F, 
    #text.random = ""
)
forest(res.meta, 
       xlab.pos = 1, 
       #xlim = c(0.1 100), 
       lwd = 1.5, 
       header.line = T, 
       spacing = 1.2, #行距
       addrow = F, #结果上的空行
       addrow.overall = F, #亚组分析时用
       addrow.subgroups	= F, 
       addrows.below.overall = 0, 
       #bottom.lr = T, 
       #hetlab = "111", #异质性检验标签前面的文字, resid.hetlab = 
       leftcols = "studlab", # 左边展示内容，#不显示logHR
       #leftlabs = c("Study"),
       #colgap.forest.left = unit(0.3,"inches"), #左边与森林间距，需要加载grid包
       rightcols = c("effect", "ci", "w.random"), # 右边展示内容
       rightlabs = c("HR", "95% CI", "Weight"), 
       print.tau2 = F
) 
pdf("1.pdf", height = 10, width = 10)
forest(
       res.meta, 
       xlab.pos = 1, 
       #xlim = c(0.1 100), 
       lwd = 1.5, 
       header.line = T, 
       spacing = 1.2, #行距
       addrow = F, #结果上的空行
       addrow.overall = F, #亚组分析时用
       addrow.subgroups	= F, 
       addrows.below.overall = 0, 
       #bottom.lr = T, 
       #hetlab = "111", #异质性检验标签前面的文字, resid.hetlab = 
       leftcols = "studlab", # 左边展示内容，#不显示logHR
       #leftlabs = c("Study"),
       #colgap.forest.left = unit(0.3,"inches"), #左边与森林间距，需要加载grid包
       rightcols = c("effect", "ci", "w.random"), # 右边展示内容
       rightlabs = c("HR", "95%-CI", "Weight"), 
       print.tau2 = F
)
dev.off()
```

```R
data$method.f = 
	ifelse(str_detect(data$method, "ultracentrifugation"), "uc", ifelse(str_detect(data$method, "exo"), "exo_kit", "other"))
```

```R
#最终使用的代码
#整理数据

#定义meta对象函数
meta = function(data){
    res.meta = metagen(
        #subset = data[(data$Disease.stage == "resctabel"), ], 
        data = data, 
        data$HRlog, 
        data$log_SE, 
        sm = "HR", 
        studlab = paste0(data$`Author,.publication.year`, " /", data$Biomarker.of.EV), 
        common = F, 
        label.right = "Mortality", 
        label.left = "Survival", 
        print.subgroup.name = F, 
        #text.random = ""
    )
    return(res.meta)
}

#定义绘图函数 
forest(res.meta, 
       #sortvar = data$Biomarker.of.EV,  #排序方法，长度要一致
       xlab.pos = 1, 
       hetstat = T , 
       resid.hetstat = T, 
       tau.random = TRUE, 
       #xlim = c(0.1 100), 
       wd = 1.5, 
       header.line = T, 
       #spacing = 1.5, #行距
       addrow = F, #结果上的空行
       addrow.overall = F, #亚组分析时用
       addrow.subgroups	= F, 
       addrows.below.overall = 0, 
       #bottom.lr = T, 
       #hetlab = "111", #异质性检验标签前面的文字, resid.hetlab = 
       leftcols = "studlab", # 左边展示内容，#不显示logHR
       #leftlabs = c("Study"),
       #colgap.forest.left = unit(0.3,"inches"), #左边与森林间距，需要加载grid包
       rightcols = c("effect", "ci", "w.random"), # 右边展示内容
       rightlabs = c("HR", "95%-CI", "Weight"), 
       print.tau2 = T, 
       #test.overall.random = T 
       #subgroup.hetstat = T
       #text.addline1 = "备注行1。", 
       #text.addline2 = "备注行2。"
       textcol = "black"
) 
for_graph = function(res.meta){
    forest(res.meta, 
           #sortvar = data$Biomarker.of.EV,  #排序方法，长度要一致
           xlab.pos = 1, 
           hetstat = T , 
           #resid.hetstat = T, 
           tau.random = TRUE, 
           #xlim = c(0.1 100), 
           wd = 1.5, 
           header.line = T, 
           spacing = 1.5, #行距
           addrow = F, #结果上的空行
           addrow.overall = F, #亚组分析时用
           addrow.subgroups	= F, 
           addrows.below.overall = 0, 
           #bottom.lr = T, 
           #hetlab = "111", #异质性检验标签前面的文字, resid.hetlab = 
           leftcols = "studlab", # 左边展示内容，#不显示logHR
           #leftlabs = c("Study"),
           #colgap.forest.left = unit(0.3,"inches"), #左边与森林间距，需要加载grid包
           rightcols = c("effect", "ci", "w.random"), # 右边展示内容
           rightlabs = c("HR", "95%-CI", "Weight"), 
           print.tau2 = T, 
           #test.overall.random = T 
           #subgroup.hetstat = T
           #text.addline1 = "备注行1。", 
           #text.addline2 = "备注行2。"
           textcol = "black"
    ) 
}
for_graph(res.meta)
#生成pdf的函数
pdf("1.pdf", height = 14, width = 10)
forest(res.meta, 
       #sortvar = data$Biomarker.of.EV,  #排序方法，长度要一致
       xlab.pos = 1, 
       hetstat = T , 
       #resid.hetstat = T, 
       tau.random = TRUE, 
       #xlim = c(0.1 100), 
       wd = 1.5, 
       header.line = T, 
       spacing = 1.5, #行距
       addrow = F, #结果上的空行
       addrow.overall = F, #亚组分析时用
       addrow.subgroups	= F, 
       addrows.below.overall = 0, 
       #bottom.lr = T, 
       #hetlab = "111", #异质性检验标签前面的文字, resid.hetlab = 
       leftcols = "studlab", # 左边展示内容，#不显示logHR
       #leftlabs = c("Study"),
       #colgap.forest.left = unit(0.3,"inches"), #左边与森林间距，需要加载grid包
       rightcols = c("effect", "ci", "w.random"), # 右边展示内容
       rightlabs = c("HR", "95%-CI", "Weight"), 
       print.tau2 = T, 
       #test.overall.random = T 
       #subgroup.hetstat = T
       #text.addline1 = "备注行1。", 
       #text.addline2 = "备注行2。"
       textcol = "black"
) 
dev.off()

for_pdf = function(res.meta){
    pdf("1.pdf", height = 14, width = 10)
    forest(res.meta, 
           #sortvar = data$Biomarker.of.EV,  #排序方法，长度要一致
           xlab.pos = 1, 
           hetstat = T , 
           #resid.hetstat = T, 
           tau.random = TRUE, 
           #xlim = c(0.1 100), 
           wd = 1.5, 
           header.line = T, 
           spacing = 1.5, #行距
           addrow = F, #结果上的空行
           addrow.overall = F, #亚组分析时用
           addrow.subgroups	= F, 
           addrows.below.overall = 0, 
           #bottom.lr = T, 
           #hetlab = "111", #异质性检验标签前面的文字, resid.hetlab = 
           leftcols = "studlab", # 左边展示内容，#不显示logHR
           #leftlabs = c("Study"),
           #colgap.forest.left = unit(0.3,"inches"), #左边与森林间距，需要加载grid包
           rightcols = c("effect", "ci", "w.random"), # 右边展示内容
           rightlabs = c("HR", "95%-CI", "Weight"), 
           print.tau2 = T, 
           #test.overall.random = T 
           #subgroup.hetstat = T
           #text.addline1 = "备注行1。", 
           #text.addline2 = "备注行2。"
           textcol = "black"
    ) 
    dev.off()
}

rightcols = c("effect", "ci", "tau2", "I2") 右边展示内容
       rightlabs = c("HR", "95%-CI", "Weight", "tau^2", "I2"), 
```

```R
#find.outliers 
find.outliers(res.meta)
data = OS_uni_uni[!OS_uni_uni$Biomarker.of.EV %in% c(), ]
```

```R
#Influence Diagnostics
library(ggrepel)
library(gridExtra)                      
res.meta.inf = InfluenceAnalysis(res.meta, random = TRUE)
```

```R
#亚组分析
data = OS_uni_uni
data = data[!is.na(data$Stage), ]
res.meta.sub = metagen(
    #subset = data[(data$Disease.stage == "resctabel"), ], 
    data = data,
    byvar = data$Stage, 
    data$HRlog, 
    data$log_SE, 
    sm = "HR", 
    studlab = paste0(data$`Author,.publication.year`, " /", data$Biomarker.of.EV), 
    common = F, 
    label.right = "Mortality", 
    label.left = "Survival", 
    print.subgroup.name = F, 
    #text.random = ""
)

data = OS_uni_uni
data = data[!is.na(data$Disease.stage), ]
data = data[data$Disease.stage == "resectable", ]
res.meta = meta(data)
for_pdf(res.meta)

data = OS_uni_uni
data = data[!is.na(data$Disease.stage), ]
data = data[data$Disease.stage == "metastatic", ]
res.meta = meta(data)
for_pdf(res.meta)

data = OS_uni_uni
res.meta.sub = metagen(
    #subset = data[(data$Disease.stage == "resctabel"), ], 
    data = data,
    byvar = data$Classification.of.Biomarker, 
    data$HRlog, 
    data$log_SE, 
    sm = "HR", 
    studlab = paste0(data$`Author,.publication.year`, " /", data$Biomarker.of.EV), 
    common = F, 
    label.right = "Mortality", 
    label.left = "Survival", 
    print.subgroup.name = F, 
    #text.random = ""
)
pdf("1.pdf", height = 20, width = 15)
forest(res.meta.sub, 
       #sortvar = data$Biomarker.of.EV,  #排序方法，长度要一致
       xlab.pos = 1, 
       hetstat = T , 
       #resid.hetstat = T, 
       tau.random = TRUE, 
       #xlim = c(0.1 100), 
       wd = 1.5, 
       header.line = T, 
       spacing = 1.5, #行距
       addrow = F, #结果上的空行
       addrow.overall = F, #亚组分析时用
       addrow.subgroups	= F, 
       addrows.below.overall = 0, 
       #bottom.lr = T, 
       #hetlab = "111", #异质性检验标签前面的文字, resid.hetlab = 
       leftcols = "studlab", # 左边展示内容，#不显示logHR
       #leftlabs = c("Study"),
       #colgap.forest.left = unit(0.3,"inches"), #左边与森林间距，需要加载grid包
       rightcols = c("effect", "ci", "w.random"), # 右边展示内容
       rightlabs = c("HR", "95%-CI", "Weight"), 
       print.tau2 = T, 
       #test.overall.random = T 
       #subgroup.hetstat = T
       #text.addline1 = "备注行1。", 
       #text.addline2 = "备注行2。"
       textcol = "black"
) 
dev.off()
write.xlsx(OS_uni_uni, "@最终数据/OS 单分子 单因素.xlsx", row.names = T)
#OS 单分子 单因素分析完毕
```

```R
#OS单分子，多因素
data = OS_uni_muti
res.meta = meta(data)
```

```R
#PFS
res.meta = meta(PFS)
for_pdf(res.meta )
find.outliers(res.meta)
#RFS
res.meta = meta(RFS)
for_pdf(res.meta )
find.outliers(res.meta)
#DFS单分子单因素
DFS_uni_uni = read.xlsx("DFS-单分子-单因素.xlsx", sheet = 1, rowNames = T)
DFS_uni_uni$HRlog = log(DFS_uni_uni$OS_Hazard.ratio)
DFS_uni_uni$CI_L_log = log(DFS_uni_uni$`95%.CI_L`)
DFS_uni_uni$CI_U_log = log(DFS_uni_uni$`95%.CI_U`)
DFS_uni_uni$log_SE = (DFS_uni_uni$CI_U_log - DFS_uni_uni$CI_L_log) / 3.92
DFS_uni_uni$`Author,.publication.year` = rownames(DFS_uni_uni)
res.meta = meta(DFS_uni_uni)
for_pdf(res.meta)
find.outliers(res.meta)
#提出对异质性有显著贡献的研究
data = DFS_uni_uni
data = data[-6, ]
res.meta = meta(data)
for_pdf(res.meta)
find.outliers(res.meta)
#DFS单分子多因素
DFS_uni_multi = read.xlsx("DFS-单分子-多因素.xlsx", sheet = 1, rowNames = T)
DFS_uni_multi$HRlog = log(DFS_uni_multi$Hazard.ratio)
DFS_uni_multi$CI_L_log = log(DFS_uni_multi$`95%.CI_L`)
DFS_uni_multi$CI_U_log = log(DFS_uni_multi$`95%.CI_U`)
DFS_uni_multi$log_SE = (DFS_uni_multi$CI_U_log - DFS_uni_multi$CI_L_log) / 3.92
DFS_uni_multi$`Author,.publication.year` = rownames(DFS_uni_multi)
res.meta = meta(DFS_uni_multi)
for_pdf(res.meta)
find.outliers(res.meta)
```

```R
#多分子
#OS单因素
res.meta = meta(OS_muti_uni)
for_pdf(res.meta)
find.outliers(res.meta)
#OS多因素
res.meta = meta(OS_muti_muti)
for_pdf(res.meta)
find.outliers(res.meta)
#mutation allelic frequency更改为MAF，名字太长
data = OS_muti_muti
data = data[-1, ]
res.meta = meta(data)
for_pdf(res.meta)
find.outliers(res.meta)
#RFS单因素
res.meta = meta(RFS_muti_uni)
for_pdf(res.meta)
find.outliers(res.meta)
#RFS多因素
res.meta = meta(RFS_muti_muti)
for_pdf(res.meta)
find.outliers(res.meta)
```

```R
#EVs数量
res.meta = meta(OS_EV_count)
for_pdf(res.meta)
find.outliers(res.meta)
```

```R
#Begg法：
#metabias(res.meta, k.min = 7, method.bias = "Begg", correct =T, plotit = T)
#metabias(res.meta, k.min = 7, method.bias = "Egger")
#OS单分子单因素
metabias(res.meta, k.min = 5, method.bias = "linreg") 
#其他的研究数量不够
```

```R
#bubble图
#OS
res.meta = meta(OS_uni_uni)
res.reg = metareg(res.meta, ~ Year)
bubble(res.reg)
#函数封装
bubble.graph = function(P){
    pdf("1.pdf", height = 10, width = 10)
    bubble(P, 
           xlim = c(2015, 2023), 
           cex = 1.5
    )  
    dev.off()
}
bubble.graph(res.reg)
```

## 4.多因素OS的漏斗图

```R
#产生meta对象
data = OS_uni_multF
res.meta = metagen(
    data = data, 
    data$HRlog, 
    data$log_SE, 
    sm = "HR", 
    studlab = paste0(data$`Author,.publication.year`, "/", data$Biomarker.of.EV), 
    common = F, 
    label.right = "Mortality", 
    label.left = "Survival", 
    print.subgroup.name = F, 
    text.random = ""); forest(res.meta)
#
funnel(res.meta)
funnel(res.meta, 
       #xlim = c(-0.5, 20), 
       pch = 20,
       cex = .8, 
       contour.levels = c(0.9, 0.95, 0.99), 
       studlab = F,
       col.contour = c("darkgray", "gray", "lightgray")
)
legend(x = 5, y = 0, #p值坐标
       c("p<0.01", "0.01<p<0.05", "0.05<p<0.1"), 
       bty = "n",
       fill = c("lightgray", "gray", "darkgray")
)
#画图
pdf("OS uni multi funnel.pdf", height = 10, width = 10)
funnel(res.meta, 
       #xlim = c(-0.5, 20), 
       pch = 20,
       cex = .8, 
       contour.levels = c(0.9, 0.95, 0.99), 
       studlab = F,
       col.contour = c("darkgray", "gray", "lightgray")
)
legend(x = 5, y = 0, #p值坐标
       c("p<0.01", "0.01<p<0.05", "0.05<p<0.1"),  
       bty = "n",
       fill = c("lightgray", "gray", "darkgray")
)
dev.off()
```

```R
#Egger’s法检验
metabias(res.meta, method.bias = "linreg", plotit = T)
```

## 5.自定义函数

```R
my.meta_subgroup.pdf = function(data = data, var = colnames(data)[1], mult = F){
    data_m.1 = metagen(data$HRlog, data$log_SE, sm = "HR", studlab = paste0(data$`Author,.publication.year`, " /", data$Biomarker.of.EV), comb.fixed = F, comb.random = T, byvar = data[, var])
    if(mult == F){
        uni_mult = "OS_uni_uniF"
    }else{
        uni_mult = "OS_uni_multF"
    }
    pdf(paste0("subgroup ", var, " ", uni_mult, ".pdf"), width = 15, height = 20)
    forest(data_m.1)
    dev.off()
}
```

```R
my.meta_subgroup.pdf = function(data = data, var = colnames(data)[1], mult = F){
    data_m.1 = metagen(data$HRlog, data$log_SE, sm = "HR", studlab = paste0(data$`Author,.publication.year`, " /", data$Biomarker.of.EV), comb.fixed = F, comb.random = T, byvar = data[, var])
    if(mult == F){
        uni_mult = "OS_uni_uniF"
    }else{
        uni_mult = "OS_uni_multF"
    }
    pdf(paste0("subgroup ", var, " ", uni_mult, ".pdf"), width = 12, height = 16)
    forest(data_m.1, 
           common = F,
           random = T,
           fontsize = 12,
           print.subgroup.labels = T,
           addrow.subgroups = T, 
           #subgroup.name = x$subgroup.name
           header.line = T,
           leftcols = "studlab",
           lwd	= 0.8, 
           xlab = c("Survival           Mortality"), 
           rightlabs = c("HR", "95% CI", "Weight")
           )
    dev.off()
}; my.meta_subgroup.pdf(data = OS_uni_uniF, var = "Method")
```

```R
OS_uni_multF.var = colnames(OS_uni_multF)
OS_uni_multF.var
OS_uni_uniF.var = colnames(OS_uni_uniF)
OS_uni_uniF.var  
```

```R
my.meta_subgroup.pdf(data = OS_uni_multF, var = OS_uni_multF.var[3], mult = T)
my.meta_subgroup.pdf(data = OS_uni_multF, var = OS_uni_multF.var[4], mult = T)
my.meta_subgroup.pdf(data = OS_uni_multF, var = OS_uni_multF.var[5], mult = T)
my.meta_subgroup.pdf(data = OS_uni_multF, var = OS_uni_multF.var[6], mult = T)
my.meta_subgroup.pdf(data = OS_uni_multF, var = OS_uni_multF.var[8], mult = T)
my.meta_subgroup.pdf(data = OS_uni_multF, var = OS_uni_multF.var[13], mult = T)
my.meta_subgroup.pdf(data = OS_uni_multF, var = OS_uni_multF.var[14], mult = T)
my.meta_subgroup.pdf(data = OS_uni_multF, var = OS_uni_multF.var[15], mult = T)
my.meta_subgroup.pdf(data = OS_uni_multF, var = OS_uni_multF.var[20], mult = T)
my.meta_subgroup.pdf(data = OS_uni_multF, var = OS_uni_multF.var[21], mult = T)
my.meta_subgroup.pdf(data = OS_uni_multF, var = OS_uni_multF.var[22], mult = T)
my.meta_subgroup.pdf(data = OS_uni_multF, var = OS_uni_multF.var[23], mult = T)
my.meta_subgroup.pdf(data = OS_uni_uniF, var = OS_uni_uniF.var[3], mult = F)
my.meta_subgroup.pdf(data = OS_uni_uniF, var = OS_uni_uniF.var[4], mult = F)
my.meta_subgroup.pdf(data = OS_uni_uniF, var = OS_uni_uniF.var[5], mult = F)
my.meta_subgroup.pdf(data = OS_uni_uniF, var = OS_uni_uniF.var[6], mult = F)
my.meta_subgroup.pdf(data = OS_uni_uniF, var = OS_uni_uniF.var[8], mult = F)
my.meta_subgroup.pdf(data = OS_uni_uniF, var = OS_uni_uniF.var[17], mult = F)
my.meta_subgroup.pdf(data = OS_uni_uniF, var = OS_uni_uniF.var[18], mult = F)
my.meta_subgroup.pdf(data = OS_uni_uniF, var = OS_uni_uniF.var[19], mult = F)
my.meta_subgroup.pdf(data = OS_uni_uniF, var = OS_uni_uniF.var[20], mult = F)
my.meta_subgroup.pdf(data = OS_uni_uniF, var = OS_uni_uniF.var[21], mult = F)
my.meta_subgroup.pdf(data = OS_uni_uniF, var = OS_uni_uniF.var[22], mult = F)
my.meta_subgroup.pdf(data = OS_uni_uniF, var = OS_uni_uniF.var[23], mult = F)
```

## 6.生物标志物靶点的生物信息学分析

```R
biomarker_table = study_info
View(biomarker_table)
biomarker_table = biomarker_table[biomarker_table$Classification.of.biomarker != "EV levels", ]
biomarker_table = edit(biomarker_table)
biomarker_table = biomarker_table[!is.na(biomarker_table$Classification.of.biomarker), ]
biomarker_table = biomarker_table[!is.na(biomarker_table$Biomarker.of.EV), ]
biomarker_table = biomarker_table[biomarker_table$Biomarker.of.EV != "EV protein signature*", ]
biomarker_table = biomarker_table[biomarker_table$Biomarker.of.EV != "four miRNAs model*", ]
biomarker_table = biomarker_table[biomarker_table$Biomarker.of.EV != "5-exosomal lncRNAs panel*", ]
biomarker_table$biomarker = biomarker_table$Biomarker.of.EV
biomarker_table = edit(biomarker_table)
```

```R
#RNA互作：代码分享：使用R语言构建ceRNA网络（circRNA-miRNA-mRNA） 
library(igraph) 
library(dplyr) 
library(magrittr)
#①输入文件，"data.csv"表头如下（分别对应有三列）：
#network_data = read.csv("data.csv", header = TRUE) 
network_data = data.frame(
    c("cir1", "cir2", "cir3"), 
    c("mir1", "mir2", "mir3"), 
    c("mR1", "mR2", "mR3")
)
colnames(network_data) =  c("circRNA","miRNA","mRNA")
#②定义网络参数与属性：
# 创建空的网络对象 
g = graph.empty(n = length(c(unique(network_data$miRNA),
                             unique(network_data$circRNA), 
                             unique(network_data$mRNA))), 
                directed = TRUE) 
# 添加节点 
g = set_vertex_attr(g, 
                    "type", 
                     value = c(
                       rep("circRNA", 
                           length(unique(network_data$circRNA))),                              rep("miRNA", 
                            length(unique(network_data$miRNA))),
                       rep("mRNA", length(unique(network_data$mRNA))))) 
g = set_vertex_attr(g,
                    "name", 
                    value = c(unique(network_data$circRNA), 
                              unique(network_data$miRNA), 
                              unique(network_data$mRNA))) 
g = set_vertex_attr(g,
                    "color", 
                    value = ifelse(
                      V(g)$type == "circRNA", 
                      "#fb8072", 
                      ifelse(
                        V(g)$type == "miRNA", 
                        "yellow3",
                        "#80b1d3"))) 
# 添加边与边长 
afedge = c() 
aflength = c() 
for(i in 1:nrow(network_data)) { 
  circRNA_node = which(V(g)$name == network_data[i, 1]) 
  miRNA_node = which(V(g)$name == network_data[i, 2]) 
  mRNA_node = which(V(g)$name == network_data[i, 3]) 
  aflength = c(aflength, 20, 10) 
  afedge = c(afedge, circRNA_node, miRNA_node, miRNA_node, mRNA_node) 
} 
g = g %>% add_edges(afedge) %>% set_edge_attr("edge.length", value = aflength) 
# 添加节点大小 
circRNA.size = as.vector(scale(as.vector(table(network_data$circRNA)), center = F))+15 
miRNA.size = as.vector(scale(as.vector(table(network_data$miRNA)), center = F))+8 
mRNA.size = as.vector(scale(as.vector(table(network_data$mRNA)), center = F))+3
V(g)$size = c(circRNA.size, miRNA.size, mRNA.size)
#④绘制并保存图片，igraph包中提供了多种布局算法，可以将节点和边布局在平面上。以下是一些常见的布局算法：
#Ⅰ：layout.circle：在圆形上均匀分布所有节点。
#Ⅱ：layout.fruchterman.reingold：使用Fruchterman-Reingold算法，根据节点之间的力学模型，计算节点的位置。该算法可以确保相邻节点之间的距离尽量相等，并且可以避免节点之间的重叠。
#Ⅲ：layout.graphopt：使用Graphopt算法，通过将节点移动到合适的位置以最小化边的长度来优化图的布局。
#Ⅳ：layout.kamada.kawai：使用Kamada-Kawai算法，通过最小化图的能量来计算节点的位置。该算法可以确保相邻节点之间的距离尽量相等，并且可以保持图形的对称性。
#Ⅴ：layout.lgl：使用Large Graph Layout算法，对于大型图形而言，布局更加高效
# 使用Graphopt算进行布局，保存为ceRNA.net.pdf文件
pdf(file = "ceRNA.net.pdf", height = 10, width = 10) 
plot(g, 
     layout=layout.graphopt(g), 
     vertex.label = V(g)$name, 
     vertex.label.family = "sans", 
     vertex.label.cex = ifelse(V(g)$type == "circRNA", 0.8, ifelse(V(g)$type == "miRNA", 0.5, 0.2)), 
     vertex.size = V(g)$size, 
     vertex.color = V(g)$color, 
     vertex.label.color = "black", 
     edge.arrow.size = 0.5, 
     edge.width = 1) 
dev.off()
```



