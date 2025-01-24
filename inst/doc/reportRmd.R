## ----setup, include=FALSE-----------------------------------------------------
library(reportRmd)
# knitr::opts_chunk$set(message = FALSE, warning = FALSE,dev="cairo_pdf")
knitr::opts_chunk$set(message = FALSE, warning = FALSE)

## -----------------------------------------------------------------------------
data("pembrolizumab")
rm_covsum(data=pembrolizumab, 
covs=c('age','sex'))

## -----------------------------------------------------------------------------
rm_covsum(data=pembrolizumab, 
covs=c('age','sex'),IQR=TRUE)

## -----------------------------------------------------------------------------
rm_covsum(data=pembrolizumab, 
covs=c('age','sex'),all.stats = TRUE)

## -----------------------------------------------------------------------------
rm_covsum(data=pembrolizumab, maincov = 'sex',
covs=c('age','pdl1','change_ctdna_group'))

## -----------------------------------------------------------------------------
rm_covsum(data=pembrolizumab, maincov = 'sex',
covs=c('age','pdl1','change_ctdna_group'),
show.tests=TRUE)

## -----------------------------------------------------------------------------
rm_covsum(data=pembrolizumab, maincov = 'sex',
covs=c('age','change_ctdna_group'),
effSize=TRUE)

## -----------------------------------------------------------------------------
rm_covsum(data=pembrolizumab, maincov = 'sex',
covs=c('age','pdl1'),
testcont='ANOVA',
show.tests=TRUE, effSize=TRUE)

## -----------------------------------------------------------------------------
rm_covsum(data=pembrolizumab, maincov = 'sex',
covs=c('cohort'),
pvalue = FALSE)

## -----------------------------------------------------------------------------
rm_covsum(data=pembrolizumab, maincov = 'sex',
covs=c('cohort'),
pvalue = FALSE,
percentage='row')

## -----------------------------------------------------------------------------
rm_compactsum(data = pembrolizumab, xvars = c("change_ctdna_group", "l_size"))

## -----------------------------------------------------------------------------
rm_compactsum(data=pembrolizumab, xvars = c("change_ctdna_group", "l_size"), iqr=TRUE)

## -----------------------------------------------------------------------------
rm_compactsum(data=pembrolizumab, xvars = c("change_ctdna_group", "l_size"), all.stats = T)

## -----------------------------------------------------------------------------
rm_compactsum(data=pembrolizumab, xvars=c('age','pdl1','change_ctdna_group'), grp = 'sex')

## -----------------------------------------------------------------------------
rm_compactsum(data=pembrolizumab, xvars=c('age','pdl1','change_ctdna_group'), grp = 'sex', show.tests=TRUE)

## -----------------------------------------------------------------------------
rm_compactsum(data=pembrolizumab, xvars=c('age','pdl1','change_ctdna_group'), grp = 'sex', effSize = T, show.tests = T)

## -----------------------------------------------------------------------------
rm_compactsum(data=pembrolizumab, xvars=c('age','pdl1','change_ctdna_group'), grp = 'sex', use_mean = c("pdl1"), effSize = T, show.tests = T)

## -----------------------------------------------------------------------------
rm_compactsum(data=pembrolizumab, xvars=c('age','pdl1','change_ctdna_group'), grp = 'sex', use_mean = c("pdl1"), digits = 2, digits.cat = 1, effSize = T, show.tests = T)

## -----------------------------------------------------------------------------
rm_compactsum(data=pembrolizumab, xvars=c('age','pdl1','l_size'), grp = 'sex', digits = c("age" = 3, "l_size" = 2), effSize = T, show.tests = T)

## -----------------------------------------------------------------------------
rm_compactsum(data=pembrolizumab, xvars=c('change_ctdna_group','orr'), grp = 'cohort', effSize = T, show.tests = T, percentage = "row")

## ----results = 'asis'---------------------------------------------------------
summary_tab <- rm_compactsum(data=pembrolizumab, xvars=c('change_ctdna_group','orr', 'age'), grp = 'cohort', effSize = T, show.tests = T)
cat(attr(summary_tab, "description"))

## -----------------------------------------------------------------------------
rm_uvsum(data=pembrolizumab, response='orr',
covs=c('age','pdl1','change_ctdna_group'))

## -----------------------------------------------------------------------------
rm_uvsum(data=pembrolizumab, response='l_size',
covs=c('age','cohort'))

## -----------------------------------------------------------------------------
rm_uvsum(data=pembrolizumab, response='orr',
covs=c('age','cohort'))

## -----------------------------------------------------------------------------
pembrolizumab$Counts <- rpois(nrow(pembrolizumab),lambda = 3)
rm_uvsum(data=pembrolizumab, response='Counts',covs=c('age','cohort'))

## -----------------------------------------------------------------------------
pembrolizumab$length_followup <- rnorm(nrow(pembrolizumab),mean = 72,sd=3)
pembrolizumab$log_length_followup <- log(pembrolizumab$length_followup)
rm_uvsum(data=pembrolizumab, response='Counts',covs=c('age','cohort'),
         offset = "log_length_followup")

## -----------------------------------------------------------------------------
rm_uvsum(data=pembrolizumab, response='Counts', type='negbin',
         covs=c('age','cohort'),
         offset = "log_length_followup")


## -----------------------------------------------------------------------------
rm_uvsum(data=pembrolizumab, response=c('os_time','os_status'),
covs=c('age','pdl1','change_ctdna_group'),whichp = "levels")


## -----------------------------------------------------------------------------
rm_uvsum(data=pembrolizumab, response=c('os_time','os_status'),
covs=c('age','pdl1','change_ctdna_group'),
type='crr')

## -----------------------------------------------------------------------------
data("ctDNA")
 rm_uvsum(response = 'size_change',
 covs=c('time','ctdna_status'),
 gee=TRUE,
 id='id', corstr="exchangeable",
 family=gaussian("identity"),
 data=ctDNA,showN=TRUE)

## -----------------------------------------------------------------------------
 rm_uvsum(response = 'orr',
 covs=c('age'),
 data=pembrolizumab,returnModels = TRUE)

## -----------------------------------------------------------------------------
mList <-  rm_uvsum(response = 'orr',
 covs=c('age'),
 data=pembrolizumab,returnModels = TRUE)
head(mList$age$data)

## -----------------------------------------------------------------------------
 rm_uvsum(response = 'orr',
 covs=c('age','sex','pdl1'),
 data=pembrolizumab,p.adjust = 'fdr')

## -----------------------------------------------------------------------------
glm_fit <- glm(orr~change_ctdna_group+pdl1+age,
               family='binomial',
               data = pembrolizumab)
rm_mvsum(glm_fit, showN = TRUE, vif=TRUE)

## -----------------------------------------------------------------------------
rm_mvsum(glm_fit, showN = TRUE, vif=TRUE,p.adjust = 'holm')

## -----------------------------------------------------------------------------
uvsumTable <- rm_uvsum(data=pembrolizumab, response='orr',
covs=c('age','sex','pdl1','change_ctdna_group'),tableOnly = TRUE)

glm_fit <- glm(orr~change_ctdna_group+pdl1,
               family='binomial',
               data = pembrolizumab)
mvsumTable <- rm_mvsum(glm_fit, showN = TRUE,tableOnly = TRUE)

rm_uv_mv(uvsumTable,mvsumTable)

## -----------------------------------------------------------------------------
uvsumTable <- rm_uvsum(data=pembrolizumab, response='orr',
covs=c('age','sex','pdl1','change_ctdna_group'),tableOnly = TRUE,p.adjust='holm')

glm_fit <- glm(orr~change_ctdna_group+pdl1,
               family='binomial',
               data = pembrolizumab)
mvsumTable <- rm_mvsum(glm_fit,tableOnly = TRUE,p.adjust='holm')

rm_uv_mv(uvsumTable,mvsumTable)

## -----------------------------------------------------------------------------
mvsumTable <- rm_mvsum(glm_fit, showN = TRUE,tableOnly = TRUE)
names(mvsumTable)[1] <-'Predictor'
outTable(mvsumTable)

## -----------------------------------------------------------------------------
cohortA <- rm_uvsum(data=subset(pembrolizumab,cohort=='A'), 
                     response = 'pdl1',
                     covs=c('age','sex'),
                     tableOnly = T)
cohortA$Cohort <- 'Cohort A'
cohortE <- rm_uvsum(data=subset(pembrolizumab,cohort=='E'), 
                     response = 'pdl1',
                     covs=c('age','sex'),
                     tableOnly = T)
cohortE$Cohort <- 'Cohort E'
nestTable(rbind(cohortA,cohortE),head_col = 'Cohort',to_col = 'Covariate')


## -----------------------------------------------------------------------------
long_table <- rm_compactsum(data=pembrolizumab,xvars = c(age,sex,cohort,pdl1,tmb,baseline_ctdna,change_ctdna_group,orr,cbr))
scrolling_table(long_table,pixelHeight = 300)

## -----------------------------------------------------------------------------
rm_survsum(data=pembrolizumab,time='os_time',status='os_status',
 group="sex",survtimes=seq(12,36,12),survtimeunit='months')

## -----------------------------------------------------------------------------
rm_survtime(data=pembrolizumab,time='os_time',status='os_status',
 strata="sex",survtimes=c(12,24),survtimeunit='mo',type='PH')

## -----------------------------------------------------------------------------
rm_survtime(data=pembrolizumab,time='os_time',status='os_status', covs='age',
 strata="sex",survtimes=c(12,24),survtimeunit='mo',type='PH')

## -----------------------------------------------------------------------------
rm_survdiff(data=pembrolizumab,time='os_time',status='os_status', 
            covs='sex',strata='cohort',digits=1)

## -----------------------------------------------------------------------------
rm_covsum(data=ctDNA,
          covs=c('cohort','ctdna_status','size_change'))


## -----------------------------------------------------------------------------

ctDNA_names <- data.frame(var=names(ctDNA),
                          label=c('Patient ID',
                                  'Study Cohort',
                                  'Change in ctDNA since baseline',
                                  'Number of weeks on treatment',
                                  'Percentage change in tumour measurement'))
ctDNA <- set_labels(ctDNA,ctDNA_names)

rm_covsum(data=ctDNA,
          covs=c('cohort','ctdna_status','size_change'))

## -----------------------------------------------------------------------------
ctDNA <- set_var_labels(ctDNA,
                        cohort="A new cohort label")
rm_covsum(data=ctDNA,
          covs=c('cohort','ctdna_status','size_change'))

## -----------------------------------------------------------------------------
var_labels <- extract_labels(ctDNA)
var_labels

## -----------------------------------------------------------------------------
library(ggplot2)
p <- ggplot(data=ctDNA,aes(x=ctdna_status,y=size_change,colour=cohort))+
  geom_point()
replace_plot_labels(p)

## -----------------------------------------------------------------------------
ctDNA <- clear_labels(ctDNA)

## ----eval=FALSE,echo=TRUE-----------------------------------------------------
# plotuv(data=pembrolizumab, response='orr',
# covs=c('age','cohort','pdl1','change_ctdna_group'))

## ----eval=FALSE,echo=FALSE----------------------------------------------------
# plotuv(data=pembrolizumab, response='orr',
# covs=c('age','cohort','pdl1','change_ctdna_group'))
# ggsave('images/plotuv.png',scale = 0.5,dpi = 300)

## ----eval=FALSE,echo=TRUE-----------------------------------------------------
# plotuv(data = pembrolizumab, covs=c('age','cohort','pdl1','change_ctdna_group'), showN = T)

## ----eval=FALSE,echo=FALSE----------------------------------------------------
# plotuv(data = pembrolizumab, covs=c('age','cohort','pdl1','change_ctdna_group'), showN = T)
# ggsave('images/plotuv_nores.png',scale = 0.5,dpi = 300)

## ----eval=F,echo=T------------------------------------------------------------
# ggkmcif2(response = c('os_time','os_status'),
# cov='cohort',
# data=pembrolizumab)

## ----eval=F,echo=FALSE--------------------------------------------------------
# ggkmcif2(response = c('os_time','os_status'),
# cov='cohort',
# data=pembrolizumab)
# ggsave('images/ggkmcif.png',dpi=300,width = 5,height = 5)
# p <- ggkmcif2(response = c('os_time','os_status'),
# cov='cohort',
# data=pembrolizumab,returns = T)
# plot(p[[1]])
# ggsave('images/ggkmcif_sm.png',scale = 0.5)

## ----eval=F,echo=T------------------------------------------------------------
# forestplotUV(response="orr", covs=c("change_ctdna_group", "sex", "age", "l_size"),
# data=pembrolizumab, family='binomial')

## ----eval=F,echo=FALSE--------------------------------------------------------
# forestplotUV(response="orr", covs=c("change_ctdna_group", "sex", "age", "l_size"),
# data=pembrolizumab, family='binomial')
# ggsave('images/forestuv.png', scale = 0.5)

## ----eval=F,echo=T------------------------------------------------------------
# glm_fit <- glm(orr~change_ctdna_group+pdl1+age,
#                family='binomial',
#                data = pembrolizumab)
# forestplotMV(glm_fit)

## ----eval=F,echo=FALSE--------------------------------------------------------
# glm_fit <- glm(orr~change_ctdna_group+pdl1+age,
#                family='binomial',
#                data = pembrolizumab)
# forestplotMV(glm_fit)
# ggsave('images/forestmv.png', scale = 0.5)

## ----eval=F,echo=T------------------------------------------------------------
# UVp = forestplotUV(response="orr", covs=c("change_ctdna_group", "sex", "age",
#  "l_size"), data=pembrolizumab, family='binomial')
#  MVp = forestplotMV(glm(orr~change_ctdna_group+sex+age+l_size,
#  data=pembrolizumab,family = 'binomial'))
#  forestplotUVMV(UVp, MVp)

## ----eval=F,echo=F------------------------------------------------------------
# UVp = forestplotUV(response="orr", covs=c("change_ctdna_group", "sex", "age",
#  "l_size"), data=pembrolizumab, family='binomial')
#  MVp = forestplotMV(glm(orr~change_ctdna_group+sex+age+l_size,
#  data=pembrolizumab,family = 'binomial'))
#  forestplotUVMV(UVp, MVp)
# ggsave('images/forestuvmv.png', scale = 0.5)

## ----eval=F,echo=T------------------------------------------------------------
# uvFP <- forestplotUV(data=pembrolizumab, response='orr',
# covs=c('age','sex','pdl1','change_ctdna_group'))
# 
# glm_fit <- glm(orr~change_ctdna_group+pdl1,
#                family='binomial',
#                data = pembrolizumab)
# mvFP <- forestplotMV(glm_fit)
# 
# forestplotUVMV(uvFP,mvFP,showN=F,showEvent=F,colours=c("orange","black","blue"),logScale=F)

## ----eval=F,echo=F------------------------------------------------------------
# uvFP <- forestplotUV(data=pembrolizumab, response='orr',
# covs=c('age','sex','pdl1','change_ctdna_group'))
# 
# glm_fit <- glm(orr~change_ctdna_group+pdl1,
#                family='binomial',
#                data = pembrolizumab)
# mvFP <- forestplotMV(glm_fit)
# 
# forestplotUVMV(uvFP,mvFP,showN=F,showEvent=F,colours=c("orange","black","blue"),logScale=F)
# ggsave('images/forestuvmvlin.png', scale = 0.5)

## -----------------------------------------------------------------------------
excelCol(G,AB,Az)


## -----------------------------------------------------------------------------
excelColLetters(c(7,28,52))

## -----------------------------------------------------------------------------
 rm_uvsum(response = 'baseline_ctdna',
 covs=c('age','sex','l_size','pdl1','tmb'),
 data=pembrolizumab)
 
 options('reportRmd.digits'=1) 
 
rm_uvsum(response = 'baseline_ctdna',
 covs=c('age','sex','l_size','pdl1','tmb'),
 data=pembrolizumab)

