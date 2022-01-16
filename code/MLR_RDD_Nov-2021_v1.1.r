# remove.packages("rlang")

# Running regression on the same data as RDD: Sep 10 2021

# install.packages("https://cran.r-project.org/src/contrib/Archive/rlang/rlang_0.4.7.tar.gz", repo=NULL, type="source")

pacman::p_load(tidyverse, data.table, lubridate, 
               readxl, stargazer, broom, knitr, 
               rdd, MASS, foreign, scales, Hmisc)

# read the file : complete_processed_data_2021_v1.1.csv below
df_com_daily = read.csv('file name above')    

# head(df_com_daily, 3)
df_com_daily$date = ymd(df_com_daily$date)
df_com_daily$phase = factor(df_com_daily$phase, 
                            levels = c('PrCB', 'CB', 'P2', 'PoP2'))


summary(df_com_daily)
# check the level conts
table(df_com_daily$phase)

levels(df_com_daily$phase) = list(PrLD="PrCB", 
                                  LD="CB", 
                                  PtLD1="P2",
                                  PtLD2="PoP2")



# data after Jan 15
df_com_daily = df_com_daily[df_com_daily$date >= '2020-01-15',]
summary(df_com_daily$date)

table(df_com_daily$E1_Income.support)


# PrLD
dt1 = df_com_daily[df_com_daily$date < '2020-04-07',]
summary(dt1$date)

m11 = lm(senti_comp ~ days+senti_lag+
#          Retail.sales.index +Consumer.Price.Index..CPI. +
#          Inflation.monthly.percent.change.in.the.CPI +Unemploymentrate+
         new_deaths +positive_rate +
         + C1_School.closing + 
  C3_Cancel.public.events + 
  C5_Close.public.transport + 
  C8_International.travel.controls +
         E3_Fiscal.measures + 
  E4_International.support +
         H1_Public.information.campaigns +
  H2_Testing.policy + 
  H3_Contact.tracing  + 
  H4_Emergency.investment.in.healthcare +
         H5_Investment.in.vaccines + 
  H6_Facial.Coverings + 
  H7_Vaccination.policy +
         M1_Wildcard , data =dt1)
summary(m11)

m11_step = step(m11, direction = 'both', trace = F)
stargazer(m11_step, type='text')
vif(m11_step)
# 2 LD
dt21 = df_com_daily[df_com_daily$date >= '2020-04-07' & df_com_daily$date < '2020-06-02',]# earlier it was June 18

m21 = lm(senti_comp ~ days+I(days**2)+senti_lag+
#          Retail.sales.index +Consumer.Price.Index..CPI. +
#          Inflation.monthly.percent.change.in.the.CPI +Unemploymentrate+
         new_deaths +#positive_rate +
         +C1_School.closing +
  C3_Cancel.public.events +
#          C5_Close.public.transport +
         C8_International.travel.controls +
         E1_Income.support +
  E3_Fiscal.measures+
  E4_International.support +
         H1_Public.information.campaigns +
  H2_Testing.policy +
  H3_Contact.tracing  +
         H4_Emergency.investment.in.healthcare +
  H5_Investment.in.vaccines +H6_Facial.Coverings +
         H7_Vaccination.policy +
  M1_Wildcard , data =dt21)

m21_step = step(m21, direction = 'backward', trace = F)
stargazer(m21_step, type='text')

# 3 PtLD1 till Aug 05
# regression analysis
dt31 = df_com_daily[df_com_daily$date >= '2020-06-02'& df_com_daily$date < '2020-08-04',]

m31 = lm(senti_comp ~ days+I(days**2)+senti_lag+
#          Retail.sales.index +Consumer.Price.Index..CPI. +
#          Inflation.monthly.percent.change.in.the.CPI +Unemploymentrate+
         # new_deaths +
         +C1_School.closing +
  C3_Cancel.public.events +
         C5_Close.public.transport +
         E1_Income.support +
  E3_Fiscal.measures+
  E4_International.support +
         H1_Public.information.campaigns +
  H2_Testing.policy +
  H3_Contact.tracing  +
         H4_Emergency.investment.in.healthcare +
  H5_Investment.in.vaccines +
  H6_Facial.Coverings +
         H7_Vaccination.policy +
  M1_Wildcard , data =dt31)

m31_step = step(m31, direction = 'backward', trace = F)
stargazer(m31_step, type='text')

# 4 PtLD2 After Aug 05
# regression analysis
dt41 = df_com_daily[df_com_daily$date >= '2020-08-05',]

m41 = lm(senti_comp ~ days+I(days**2)+senti_lag+
           #          Retail.sales.index +Consumer.Price.Index..CPI. +
           #          Inflation.monthly.percent.change.in.the.CPI +Unemploymentrate+
           new_deaths +
           +C1_School.closing +
           C3_Cancel.public.events +
           C5_Close.public.transport +
           E1_Income.support +
           E3_Fiscal.measures+
           E4_International.support +
           H1_Public.information.campaigns +
           H2_Testing.policy +
           H3_Contact.tracing  +
           H4_Emergency.investment.in.healthcare +
           H5_Investment.in.vaccines +
           H6_Facial.Coverings +
           H7_Vaccination.policy +
           M1_Wildcard ,
         data =dt41)

m41_step = step(m41, direction = 'backward', trace = F)
stargazer(m41_step, type='text')




# 
# stargazer(m11_step, m21_step, m31_step, m41_step,
#           type='text',  single.row = F,header=F,
#           no.space = TRUE,
#           column.sep.width = "3pt",
#           title = 'Regression Analysis (Jan 2020 - Nov 2020)',
#          column.labels = c('PrLD', 'LD','PtLD1', 'PtLD2'))

#### trying for an html table

stargazer(m11_step, m21_step, m31_step, m41_step,
          type='html',  single.row = F,header=F,
          no.space = TRUE,
          column.sep.width = "3pt",
          title = 'Regression Analysis (Jan 2020 - Nov 2020)',
          column.labels = c('PrLD', 'LD','PtLD1', 'PtLD2'),
          out = 'PNAS-MLR.html', out.header = T)







df_com_daily[df_com_daily$date >= '2020-01-15',]%>%
  ggplot(aes(x = phase, y = senti_comp , color = phase))+ geom_boxplot() +
  ggtitle("Distribution of average sentiment across phases")

lckdwn_rdd = df_com_daily%>%        
  filter(date <='2020-06-01')

summary(lckdwn_rdd$date)


## START: July 16 2021
library(rddtools)
df_com_daily = df_com_daily[df_com_daily$date >= '2020-01-15',]
rdf = df_com_daily[df_com_daily$dat > lines$num_lines[2] & df_com_daily$dat <= lines$num_lines[4] ,]
summary(rdf$date)

# rdd model
rd_data = rdd_data(y = rdf$senti_comp, 
                   x = rdf$dat, 
                   cutpoint = lines$num_lines[3]) 
summary(rd_data)
plot(rd_data)


rd_model = rd_data %>% 
  rdd_reg_lm(slope = "separate", order=1) 
summary(rd_model)

rdf%>%
  ggplot(aes(x = date, y = senti_comp , color = phase))+ geom_point() + 
  geom_smooth(method = "lm", se=F, formula = y ~ x + I(x ^ 1) )+ 
  theme_grey(base_size = 22)+
  geom_vline( data = lines[3,], aes(xintercept = vlines), size=1.5, linetype="dotted")

# ## non-parametric
# bw_ik <- rdd_bw_ik(rd_data)
# reg_nonpara <- rdd_reg_np(rdd_object=rd_data, bw=bw_ik)
# plot(x = reg_nonpara)
# 
# 
# 
# plotSensi(reg_nonpara, from=0.05, to=1, by=0.1)
# plotPlacebo(reg_nonpara)
# dens_test(reg_nonpara)

## END: July 16 2021



















lckdwn_rdd[lckdwn_rdd$date =='2020-04-07','days']

# Plot oucome var vs. score variable
qplot(lckdwn_rdd$days, lckdwn_rdd$senti_comp,data=lckdwn_rdd) + 
  xlab("Days") + 
  ylab("Sentiment Scores")

bw <- with(lckdwn_rdd, IKbandwidth(days, senti_comp, cutpoint = 158))
rdd_simple <- RDestimate(senti_comp ~days, data = lckdwn_rdd, cutpoint = 158, bw = bw)
stargazer(summary(rdd_simple), type='text')
plot(rdd_simple)

























summary(df_com_daily$senti_comp)

df_com_daily[df_com_daily$date == '2020-06-19' ,]

bw <- with(df_com_daily, IKbandwidth(days, senti_comp, cutpoint = 158))
bw
rdd_sim <- RDestimate(senti_comp ~ days, data = df_com_daily, cutpoint = 158)
summary(rdd_sim)
plot(rdd_sim)

#write.csv(ld_wkly, 'ld_wkly.csv', row.names = F)
write.csv(df_com_daily,'df_com_daily.csv',  row.names=F)

getwd()








