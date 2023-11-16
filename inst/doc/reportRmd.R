## ----setup, include=FALSE-----------------------------------------------------
library(reportRmd)
knitr::opts_chunk$set(message = FALSE, warning = FALSE,dev="cairo_pdf")

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
covs=c('age','pdl1','change_ctdna_group'),
show.tests=TRUE, effSize=TRUE)

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
rm_uvsum(data=pembrolizumab, response='orr',
covs=c('age','pdl1','change_ctdna_group'),p.adjust = 'holm')



## -----------------------------------------------------------------------------
rm_uvsum(data=pembrolizumab, response='l_size',
covs=c('age','cohort'))


## -----------------------------------------------------------------------------
rm_uvsum(data=pembrolizumab, response=c('os_time','os_status'),
covs=c('age','pdl1','change_ctdna_group'))


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
 covs=c('age','sex'),
 data=pembrolizumab,returnModels = TRUE)

## -----------------------------------------------------------------------------
glm_fit <- glm(orr~change_ctdna_group+pdl1+age,
               family='binomial',
               data = pembrolizumab)
rm_mvsum(glm_fit,p.adjust = 'holm')


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
data(ctDNA)
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
ctDNA <- clear_labels(ctDNA)

## ----, fig.width=7, fig.height=4----------------------------------------------
plotuv(data=pembrolizumab, response='orr',
covs=c('age','cohort','pdl1','change_ctdna_group'))


## ----, fig.width=7, fig.height=2----------------------------------------------
forestplotUV(data=pembrolizumab, response='orr',
covs=c('age','sex','pdl1','change_ctdna_group'))

## ----, fig.width=7, fig.height=2----------------------------------------------
require(ggplot2)
glm_fit <- glm(orr~change_ctdna_group+pdl1,
               family='binomial',
               data = pembrolizumab)
forestplotMV(glm_fit)

## -----------------------------------------------------------------------------
uvFP <- forestplotUV(data=pembrolizumab, response='orr',
covs=c('age','sex','pdl1','change_ctdna_group'))

glm_fit <- glm(orr~change_ctdna_group+pdl1,
               family='binomial',
               data = pembrolizumab)
mvFP <- forestplotMV(glm_fit)

forestplotUVMV(uvFP,mvFP,showN=T,showEvent=T)

## -----------------------------------------------------------------------------
uvFP <- forestplotUV(data=pembrolizumab, response='orr',
covs=c('age','sex','pdl1','change_ctdna_group'))

glm_fit <- glm(orr~change_ctdna_group+pdl1,
               family='binomial',
               data = pembrolizumab)
mvFP <- forestplotMV(glm_fit)

forestplotUVMV(uvFP,mvFP,showN=F,showEvent=F,colours=c("orange","black","blue"),logScale=T)

## ----fig.width=7,fig.height=5-------------------------------------------------
ggkmcif(response = c('os_time','os_status'),
cov='cohort',
data=pembrolizumab)

## -----------------------------------------------------------------------------
 rm_uvsum(response = 'baseline_ctdna',
 covs=c('age','sex','l_size','pdl1','tmb'),
 data=pembrolizumab)
 
 options('reportRmd.digits'=1) 
 
rm_uvsum(response = 'baseline_ctdna',
 covs=c('age','sex','l_size','pdl1','tmb'),
 data=pembrolizumab)


