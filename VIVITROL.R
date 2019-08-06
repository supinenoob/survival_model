install.packages(c("survival","survminer"))
library("survival")
library("survminer")
attach(Ec2)
vivitrol<-coxph(Surv(time,dincome)~age+ed+liv+em,data=Ec2)
summary(vivitrol)
ggsurvplot(survfit(vivitrol),palette="#2E9FDF",
           ggtheme=theme_minimal())
vivitrol2<-coxph(Surv(time,dincome)~dage+ed+liv+em,data=Ec2)
summary(vivitrol2)
age.df<-with(Ec2,data.frame(dage=c(0,1),
                            ed=rep(mean(ed,na.rm=TRUE),2),
                            liv=rep(mean(liv,na.rm=TRUE),2),
                            em=rep(mean(em,na.rm=TRUE),2)))
age.df
curve<-survfit(vivitrol2,newdata=age.df)
ggsurvplot(curve,conf.int=TRUE,legend.labs=c("age=22to32","age=33to48"),
           ggtheme=theme_minimal())

fit<-survfit(Surv(time,dincome)~dage,data=Ec2)
summary(fit)$table
d<-data.frame(time = fit$time,
           n.risk = fit$n.risk,
           n.event = fit$n.event,
           n.censor = fit$n.censor,
           surv = fit$surv,
           upper = fit$upper,
           lower = fit$lower)
d
ggsurvplot(fit,
           pval = TRUE, conf.int = TRUE,
           risk.table = TRUE, # Add risk table
           risk.table.col = "strata", # Change risk table color by groups
           linetype = "strata", # Change line type by groups
           surv.median.line = "hv", # Specify median survival
           ggtheme = theme_bw(), # Change ggplot2 theme
           palette = c("#E7B800", "#2E9FDF"))
ggsurvplot(
  fit,                     # survfit object with calculated statistics.
  pval = TRUE,             # show p-value of log-rank test.
  conf.int = TRUE,         # show confidence intervals for 
  # point estimaes of survival curves.
  conf.int.style = "step",  # customize style of confidence intervals
  xlab = "Time in days",   # customize X axis label.
  break.time.by = 200,     # break X axis in time intervals by 200.
  ggtheme = theme_light(), # customize plot and risk table with a theme.
  risk.table = "abs_pct",  # absolute number and percentage at risk.
  risk.table.y.text.col = T,# colour risk table text annotations.
  risk.table.y.text = FALSE,# show bars instead of names in text annotations
  # in legend of risk table.
  ncensor.plot = TRUE,      # plot the number of censored subjects at time t
  surv.median.line = "hv",  # add the median survival pointer.
  legend.labs = 
    c("age22to32", "age33to48"),    # change legend labels.
  palette = 
    c("#E7B800", "#2E9FDF") # custom color palettes.
)
detach(Ec2)
ggsurvplot(fit)
