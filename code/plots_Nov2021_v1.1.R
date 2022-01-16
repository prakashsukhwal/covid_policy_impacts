
# Aug 29: changing the phase names

pacman::p_load(dplyr, lubridate, ggplot2, rddtools, 
               ggpubr, gridExtra, data.table, plyr)

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
                                  LD="P2",
                                  PtLD2="PoP2")

# check the level conts
table(df_com_daily$phase)

(lines = df_com_daily %>% 
  group_by(phase) %>% 
  summarise(vlines = max(date), 
            num_lines = max(dat)))
lines = data.frame(phase = c('PrLD', 'LD', 'PtLD1', 'PtLD2'),
                   vlines = ymd(c('2020-04-06', '2020-06-01', '2020-08-03','2020-11-30')),
                   num_lines = c(18358, 18460, 18596,18596))
# as.numeric(ymd(c('2020-04-06', '2020-06-01', '2020-08-03','2020-11-30')))
# plot-1
p1 = df_com_daily[df_com_daily$date >= '2020-01-15',]%>%
  ggplot(aes(x = date, y = senti_comp , color = phase))+ geom_point() + 
  geom_smooth(method = "lm", formula = y ~ poly(x, 2))+  
  labs(title = "", x = '', face= 'bold')+
  # geom_vline( data = lines, aes(xintercept = vlines), 
  #             size=1.2, linetype="dotted", color='black')+
  theme(
    # LABLES APPEARANCE
    plot.title = element_text(size=14, face= "bold", colour= "black" ),
    axis.title.x = element_text(size=14, face="bold", colour = "black"),    
    axis.title.y = element_text(size=14, face="bold", colour = "black"),    
    axis.text.x = element_text(size=11, face="bold", colour = "black"), 
    # axis.text.y = element_text(size=12,  colour = "black"), # unbold
    axis.text.y = element_text(size=9, face="bold", colour = "black"), # bold
    strip.text.x = element_text(size = 9, face="bold", colour = "black" ),
    strip.text.y = element_text(size = 9, face="bold", colour = "black"),
    axis.line.x = element_line(color="black", size = 0.3),
    axis.line.y = element_line(color="black", size = 0.3),
    panel.border = element_rect(colour = "black", fill=NA, size=0.3), legend.position="top",legend.title=element_text(size=14), 
    legend.text=element_text(size=9))


df_com_daily = df_com_daily[df_com_daily$date >= '2020-01-15',]

# creating days since Jan 2020 [ Sep 4]
df_com_daily$days.sincejan = interval(ymd('2020-01-01'), df_com_daily$date)/ddays(1)
summary(df_com_daily$days.sincejan)
# tabulate/summarise the policy variables
summary(df_com_daily$senti_lag)



p2 = df_com_daily%>%
  ggplot(aes(x = phase, y = senti_comp , color = phase))+ 
  geom_boxplot() + labs(title = "", x = 'phase', face= 'bold')+
  theme(
    # LABLES APPEARANCE
    plot.title = element_text(size=14, face= "bold", colour= "black" ),
    axis.title.x = element_text(size=14, face="bold", colour = "black"),    
    axis.title.y = element_text(size=14, face="bold", colour = "black"),    
    axis.text.x = element_text(size=11, face="bold", colour = "black"), 
    # axis.text.y = element_text(size=12,  colour = "black"), # unbold
    axis.text.y = element_text(size=9, face="bold", colour = "black"), # bold
    strip.text.x = element_text(size = 9, face="bold", colour = "black" ),
    strip.text.y = element_text(size = 9, face="bold", colour = "black"),
    axis.line.x = element_line(color="black", size = 0.3),
    axis.line.y = element_line(color="black", size = 0.3),
    panel.border = element_rect(colour = "black", fill=NA, size=0.3), legend.position="top",legend.title=element_text(size=14), 
    legend.text=element_text(size=9)
  )

###########
# Arrange
###########
# layout <- cbind(c(1 ), c(2))
# grid.arrange(p1, p2, layout_matrix = layout)
ggarrange(p1, p2,   nrow = 1)



### RDD plots


str(df_com_daily, 2)
# rdf = df_com_daily[df_com_daily$dat > lines$num_lines[1] &
#                      df_com_daily$dat <= lines$num_lines[3] ,]
rdf = df_com_daily[df_com_daily$dat < lines$num_lines[2] ,] # first subset
summary(rdf$date)
# covariates
covs = c('senti_lag', 'new_deaths',"C1_School.closing" ,'C3_Cancel.public.events','C5_Close.public.transport', 'E1_Income.support','E3_Fiscal.measures','E4_International.support',
         'H1_Public.information.campaigns' ,'H2_Testing.policy' ,'H3_Contact.tracing' , 'H4_Emergency.investment.in.healthcare' ,'H5_Investment.in.vaccines' ,'H6_Facial.Coverings' ,
         'H7_Vaccination.policy' ,'M1_Wildcard')
Z <- rdf[covs]

# RDD data
rd_data = rdd_data(y = rdf$senti_comp, 
                   x = rdf$dat, 
                   cutpoint = lines$num_lines[1], covar = Z) 
summary(rd_data)
plot(rd_data)
## p3, p4 are similarly drawn using code below for p5
p3 = rdf%>%
  ggplot(aes(x = date, y = senti_comp , color = phase))+ 
  geom_point() + 
  geom_smooth(method = "lm", se=F, formula = y ~ x + I(x ^ 2) )+ 
  theme_grey(base_size = 22)+
  geom_vline( data = lines[1,], 
              aes(xintercept = vlines), 
              size=1.5, linetype="dotted")+
  theme(
    # LABLES APPEARANCE
    plot.title = element_text(size=14, face= "bold", colour= "black" ),
    axis.title.x = element_text(size=14, face="bold", colour = "black"),    
    axis.title.y = element_text(size=14, face="bold", colour = "black"),    
    axis.text.x = element_text(size=12, face="bold", colour = "black"), 
    # axis.text.y = element_text(size=12,  colour = "black"), # unbold
    axis.text.y = element_text(size=12, face="bold", colour = "black"), # bold
    strip.text.x = element_text(size = 10, face="bold", colour = "black" ),
    strip.text.y = element_text(size = 10, face="bold", colour = "black"),
    axis.line.x = element_line(color="black", size = 0.3),
    axis.line.y = element_line(color="black", size = 0.3),
    panel.border = element_rect(colour = "black", fill=NA, size=0.3), legend.position="top",legend.title=element_text(size=14), 
    legend.text=element_text(size=9)
  )

ggarrange(p3, p4, p5,ncol=3,  nrow = 1)

################
### topic modeling plots
# PrLD data
prcb = read.csv('https://docs.google.com/spreadsheets/d/e/2PACX-1vQ8BTRfF3yf6PEZaN30fnjONSMnoUyMGo37wwyVcDDpQ0zXruA1-OJtMKoZXn8RKw/pub?gid=1683522283&single=true&output=csv', 
                na.strings=c(""), stringsAsFactors = T)
prcb$date2 = ymd(prcb$date)                
prcb$date = as.numeric(ymd(prcb$date))
head(prcb, 15)
# LD data
cb = read.csv('https://docs.google.com/spreadsheets/d/e/2PACX-1vRDkRaoKNoYOxYvKciTLWp0fRrCxdJnl3jnpWzzkxZ02MGg-4vqebx_ncKyGkfpWg/pub?gid=2091069697&single=true&output=csv', 
              na.strings=c(""), stringsAsFactors = T)

cb$date2 = ymd(cb$date)                
cb$date = as.numeric(ymd(cb$date))
summary(cb$date2)
# PtLD1 data
pocb = read.csv('https://docs.google.com/spreadsheets/d/e/2PACX-1vSpzka-z11GbE1qXoSpnsNJstFEeC5A8r_c6X1iKoJLRsx2DNMAP_CCE-Xzqc_7SQ/pub?gid=674802259&single=true&output=csv', 
                na.strings=c(""), stringsAsFactors = T)
pocb$date2 = ymd(pocb$date)                
pocb$date = as.numeric(ymd(pocb$date))
summary(pocb$date2)
# PtLD2 data
pocb2 = read.csv('https://docs.google.com/spreadsheets/d/e/2PACX-1vTG-A9JJmYnOg7E2_SmRxGnXvqFHwaTPG5GNtloa8JY15rDY-dTxT_s6Yln6Z251Q/pub?gid=1938681714&single=true&output=csv', 
                 na.strings=c(""), stringsAsFactors = T)
pocb2$date2 = ymd(pocb2$date)                
pocb2$date = as.numeric(ymd(pocb2$date))
summary(pocb2$date2)

summary(prcb$date2)
##################################
# collect the subsets 
# PrLD
##################################

# mask1
masks1 = prcb[prcb$Name %like% "5_mask_masks_fac",]
masks1$phase = 'prcb'
str(masks1)

# virus1
virus1 = prcb[prcb$Name %like% "10_virus_spread",]
virus1$phase = 'prcb'
str(virus1)
# school1
sch1 = prcb[prcb$Name %like% "6_school_schools_parent",]
sch1$phase = 'prcb'
str(sch1)
# leadership1
ldr1 = prcb[prcb$Name %like% "9_leadership_leader",]
ldr1$phase = 'prcb'
str(ldr1)


##################################
# collect the subsets 
# LD
##################################
#mask2
masks2 = cb[cb$Name %like% "5_mask_wear_ma",]
masks2$phase = 'cb'
str(masks2)
# virus2
virus2 = cb[cb$Name %like% "16_virus_coronavirus",]
virus2$phase = 'cb'
str(virus2)
# school2
sch2 = cb[cb$Name %like% "14_school_parents_sc",]
sch2$phase = 'cb'
str(sch2)
# leadership2
ldr2 = cb[cb$Name %like% "4_leadership_leaders_",]
ldr2$phase = 'cb'
str(ldr2)
# stayhome2 
sth2 = cb[cb$Name %like% "11_home_stay_safe",]
sth2$phase = 'cb'
str(sth2)

# 1_money_pay_jobs
# job_money2 
jb2 = cb[cb$Name %like% "1_money_pay_jobs",]
jb2$phase = 'cb'
str(jb2)

# 17_nurses_doctors_nurse_
# nurses, docs 
nur2 = cb[cb$Name %like% "17_nurses_doctors_nurse_",]
nur2$phase = 'cb'
str(nur2)

# 20_supermarket_shop_supermarkets
# shops, supermarkets
shop2 = cb[cb$Name %like% "20_supermarket_shop_supermarkets",]
shop2$phase = 'cb'
str(shop2)

#26_dorms_dormitories
# dorms
dorm2 = cb[cb$Name %like% "26_dorms_dormitories",]
dorm2$phase = 'cb'
str(dorm2)

# 28_circuit_breaker_measures
extend2 = cb[cb$Name %like% "40_extend_cb_cc",]
extend2$phase = 'cb'
str(extend2)

#8_police_jail_enforcement_
pol2 = cb[cb$Name %like% "8_police_jail_enforcement_",]
pol2$phase = 'cb'
str(pol2)



##################################
# collect the subsets 
# PtLD1
##################################
# mask3
masks3 = pocb[pocb$Name %like% "32_mask_eating_ea",]
masks3$phase = 'pocb1'
str(masks3)
# virus3
virus3 = pocb[pocb$Name %like% "15_virus_infected_vac",]
virus3$phase = 'pocb1'
str(virus3)
# leadership3
ldr3 = pocb[pocb$Name %like% "1_health_healthy_stay_leadersh",]
ldr3$phase = 'pocb1'
str(ldr3)

#10_border_malaysia_borders
# border3
bdr3 = pocb[pocb$Name %like% "10_border_malaysia_borders",]
bdr3$phase = 'pocb1'
str(bdr3)

#12_money_pay_salary
# job_money3 
jb3 = pocb[pocb$Name %like% "12_money_pay_salary",]
jb3$phase = 'pocb1'
str(jb3)

#3_frontliners_fu_fue
# nurses, docs 
nur3 = pocb[pocb$Name %like% "3_frontliners_fu_fue",]
nur3$phase = 'pocb1'
str(nur3)

# 48_police_enforcement_
# fines
pol3 = pocb[pocb$Name %like% "48_police_enforcement_",]
pol3$phase = 'pocb1'
str(pol3)

#9_suicide_pain_p
sui3 = pocb[pocb$Name %like% "9_suicide_pain_p",]
sui3$phase = 'pocb1'
str(sui3)


##################################
# collect the subsets 
# PtLD2
##################################
#mask4
masks4 = pocb2[pocb2$Name %like% "6_mask_wear_masks_",]
masks4$phase = 'pocb2'
str(masks4)
# virus4
#56_virus_infected_virus s
virus4 = pocb2[pocb2$Name %like% "56_virus_infected_virus",]
virus4$phase = 'pocb2'
str(virus4)

#16_leader_leadership_le
# leadership4
ldr4 = pocb2[pocb2$Name %like% "16_leader_leadership_le",]
ldr4$phase = 'pocb2'
str(ldr4)

#8_salary_pay
# job_money4 
# 31_singaporean_singaporeans_jobs 7
jb4 = pocb2[pocb2$Name %like% "31_singaporean_singaporeans_jobs",]
jb4$phase = 'pocb2'
str(jb4)

#3_jail_court_justice
# fines
pol4 = pocb2[pocb2$Name %like% "3_jail_court_justice",]
pol4$phase = 'pocb2'
str(pol4)

#1_parents_son_father_m
# 97_pandemic_fatig
sui4 = pocb2[pocb2$Name %like% "1_parents_son_father_m",]
sui4$phase = 'pocb2'
str(sui4)

#15_flats_rental_rent_
# 17_buy_sell_sale_s
rent4 = pocb2[pocb2$Name %like% "15_flats_rental_rent_",]
rent4$phase = 'pocb2'
str(rent4)

#4_rice_food_cook_res
fd4 = pocb2[pocb2$Name %like% "4_rice_food_cook_res",]
fd4$phase = 'pocb2'
str(fd4)

#22_covid_fight_control_
#35_imported_cases_import_i
import4 = pocb2[pocb2$Name %like% "22_covid_fight_control_",]
import4$phase = 'pocb2'
str(import4)

# 48_malaysia_brunei_indone
bdr4 = pocb2[pocb2$Name %like% "48_malaysia_brunei_indone",]
bdr4$phase = 'pocb2'
str(bdr4)

#61_vaccine_vaccines_va
vac4 = pocb2[pocb2$Name %like% "61_vaccine_vaccines_va",]
vac4$phase = 'pocb2'
str(vac4)






##############
## combine all dfs

# masks
# code change Sep 2021 to fix the phase names

mask = bind_rows( masks1, masks2, masks3, masks4)%>%
  dplyr::select(c(Frequency, date2, phase, date))
mask$phase = as.factor(mask$phase)
table(mask$phase)
mask$phase = revalue(mask$phase, c(prcb='PrLD', 
                                   cb = 'LD', 
                                   pocb1 = 'PtLD1',
                                   pocb2 = 'PtLD2'))
mask$phase = factor(mask$phase, levels = c('PrLD', 'LD', 'PtLD1', 'PtLD2'))

head(mask, 4)
summary(mask$date2)
str(mask)
table(mask$phase)

# plots
# wear masks

lines = data.frame(phase = c('PrLD', 'LD', 'PtLD1', 'PtLD2'),
                   vlines = ymd(c('2020-04-06', '2020-06-01', '2020-08-03','2020-11-30')),
                   num_lines = c(18358, 18414, 18477,18596))
p11 = mask%>%
ggplot(aes(x = date2, y = log(Frequency) , color = phase))+ geom_point() +
geom_smooth(method = "lm", se=F)+ 
geom_vline( data = lines, aes(xintercept = vlines), size=1.5, linetype="dotted")+
  labs(title = "", x = '')+
  lims(y= c(NA, 6), colour = unique(lines$phase))+
  theme(
    # LABLES APPEARANCE
    plot.title = element_text(size=14, face= "bold", colour= "black" ),
    axis.title.x = element_text(size=14, face="bold", colour = "black"),    
    axis.title.y = element_text(size=14, face="bold", colour = "black"),    
    axis.text.x = element_text(size=12, face="bold", colour = "black"), 
    # axis.text.y = element_text(size=12,  colour = "black"), # unbold
    axis.text.y = element_text(size=12, face="bold", colour = "black"), # bold
    strip.text.x = element_text(size = 10, face="bold", colour = "black" ),
    strip.text.y = element_text(size = 10, face="bold", colour = "black"),
    axis.line.x = element_line(color="black", size = 0.3),
    axis.line.y = element_line(color="black", size = 0.3),
    panel.border = element_rect(colour = "black", fill=NA, size=0.3), legend.position="top",legend.title=element_text(size=14), 
    legend.text=element_text(size=9)
  )

# Virus Infection
p12 = mask%>%
ggplot(aes(x = date2, y = log(Frequency) , color = phase))+ 
  geom_point() +
geom_smooth(method = "lm", se=F)+ theme_grey(base_size = 22)+
geom_vline( data = lines, aes(xintercept = vlines), size=1.5, linetype="dotted")+ 
  labs(title = "", x = '')+
  lims(y= c(NA, 6), colour = unique(lines$phase))+
  theme(
    # LABLES APPEARANCE
    plot.title = element_text(size=14, face= "bold", colour= "black" ),
    axis.title.x = element_text(size=14, face="bold", colour = "black"),    
    axis.title.y = element_text(size=14, face="bold", colour = "black"),    
    axis.text.x = element_text(size=12, face="bold", colour = "black"), 
    # axis.text.y = element_text(size=12,  colour = "black"), # unbold
    axis.text.y = element_text(size=12, face="bold", colour = "black"), # bold
    strip.text.x = element_text(size = 10, face="bold", colour = "black" ),
    strip.text.y = element_text(size = 10, face="bold", colour = "black"),
    axis.line.x = element_line(color="black", size = 0.3),
    axis.line.y = element_line(color="black", size = 0.3),
    panel.border = element_rect(colour = "black", fill=NA, size=0.3), legend.position="top",legend.title=element_text(size=14), 
    legend.text=element_text(size=9)
  )

# salary & job
p13 = mask%>%
ggplot(aes(x = date2, y = log(Frequency) , color = phase))+ geom_point() +
geom_smooth(method = "lm", se=F)+ theme_grey(base_size = 22)+
geom_vline( data = lines, aes(xintercept = vlines), size=1.5, linetype="dotted")+ 
  labs(title = "", x = '')+
  lims(y= c(NA, 6), colour = unique(lines$phase))+
  theme(
    # LABLES APPEARANCE
    plot.title = element_text(size=14, face= "bold", colour= "black" ),
    axis.title.x = element_text(size=14, face="bold", colour = "black"),    
    axis.title.y = element_text(size=14, face="bold", colour = "black"),    
    axis.text.x = element_text(size=12, face="bold", colour = "black"), 
    # axis.text.y = element_text(size=12,  colour = "black"), # unbold
    axis.text.y = element_text(size=12, face="bold", colour = "black"), # bold
    strip.text.x = element_text(size = 10, face="bold", colour = "black" ),
    strip.text.y = element_text(size = 10, face="bold", colour = "black"),
    axis.line.x = element_line(color="black", size = 0.3),
    axis.line.y = element_line(color="black", size = 0.3),
    panel.border = element_rect(colour = "black", fill=NA, size=0.3), legend.position="top",legend.title=element_text(size=14), 
    legend.text=element_text(size=9)
  )

# Suicide & Depression
p14 = mask%>%
  ggplot(aes(x = date2, y = log(Frequency) , color = phase))+ geom_point() +
  geom_smooth(method = "lm", se=F)+ theme_grey(base_size = 22)+
  geom_vline( data = lines, aes(xintercept = vlines), size=1.5, linetype="dotted")+
  labs(title = "", x = '', face= 'bold')+
  lims(y= c(NA, 6), colour = unique(lines$phase))+
  theme(
    # LABLES APPEARANCE
    plot.title = element_text(size=14, face= "bold", colour= "black" ),
    axis.title.x = element_text(size=14, face="bold", colour = "black"),    
    axis.title.y = element_text(size=14, face="bold", colour = "black"),    
    axis.text.x = element_text(size=12, face="bold", colour = "black"), 
    # axis.text.y = element_text(size=12,  colour = "black"), # unbold
    axis.text.y = element_text(size=12, face="bold", colour = "black"), # bold
    strip.text.x = element_text(size = 10, face="bold", colour = "black" ),
    strip.text.y = element_text(size = 10, face="bold", colour = "black"),
    axis.line.x = element_line(color="black", size = 0.3),
    axis.line.y = element_line(color="black", size = 0.3),
    panel.border = element_rect(colour = "black", fill=NA, size=0.3), legend.position="top",legend.title=element_text(size=14), 
    legend.text=element_text(size=9)
  )

###########
# Arrange
###########
ggarrange(p11,p12, p13, p14)











###########
# Correlation
# Dec 2021
###########
# phase: PrLD
# topics: masks, virus infection, schools, leadership

prld_msk = merge(df_com_daily, masks1,by.x = 'date', by.y = 'date2')%>%
          dplyr::select(c(date,Frequency, senti_comp))
cor.test(log(prld_msk$Frequency), prld_msk$senti_comp*10)

prld_virus = merge(df_com_daily, virus1,by.x = 'date', by.y = 'date2')%>%
  dplyr::select(c(date,Frequency, senti_comp))

cor.test(log(prld_virus$Frequency), prld_virus$senti_comp)

prld_sch = merge(df_com_daily, sch1,by.x = 'date', by.y = 'date2')%>%
  dplyr::select(c(date,Frequency, senti_comp))

cor.test(log(prld_sch$Frequency), prld_sch$senti_comp)

prld_ldr = merge(df_com_daily, ldr1,by.x = 'date', by.y = 'date2')%>%
  dplyr::select(c(date,Frequency, senti_comp))

cor.test(log(prld_ldr$Frequency), prld_ldr$senti_comp)


# phase: LD
# topics: masks, virus infection, schools, leadership and more
prld_msk2 = merge(df_com_daily, masks2,by.x = 'date', by.y = 'date2')%>%
  dplyr::select(c(date,Frequency, senti_comp))
cor.test(log(prld_msk2$Frequency), prld_msk2$senti_comp)

prld_virus2 = merge(df_com_daily, virus2,by.x = 'date', by.y = 'date2')%>%
  dplyr::select(c(date,Frequency, senti_comp))
cor.test(log(prld_virus2$Frequency), prld_virus2$senti_comp)

prld_sch2 = merge(df_com_daily, sch2,by.x = 'date', by.y = 'date2')%>%
  dplyr::select(c(date,Frequency, senti_comp))
cor.test(log(prld_sch2$Frequency), prld_sch2$senti_comp)

prld_ldr2 = merge(df_com_daily, ldr2,by.x = 'date', by.y = 'date2')%>%
  dplyr::select(c(date,Frequency, senti_comp))
cor.test(log(prld_ldr2$Frequency), prld_ldr2$senti_comp)

prld_sth2 = merge(df_com_daily, sth2,by.x = 'date', by.y = 'date2')%>%
  dplyr::select(c(date,Frequency, senti_comp))
cor.test(log(prld_sth2$Frequency), prld_sth2$senti_comp)

prld_jb2 = merge(df_com_daily, jb2,by.x = 'date', by.y = 'date2')%>%
  dplyr::select(c(date,Frequency, senti_comp))
cor.test(log(prld_jb2$Frequency), prld_jb2$senti_comp)

prld_nur2 = merge(df_com_daily, nur2,by.x = 'date', by.y = 'date2')%>%
  dplyr::select(c(date,Frequency, senti_comp))
cor.test(log(prld_nur2$Frequency), prld_nur2$senti_comp)

prld_shop2 = merge(df_com_daily, shop2,by.x = 'date', by.y = 'date2')%>%
  dplyr::select(c(date,Frequency, senti_comp))
cor.test(log(prld_shop2$Frequency), prld_shop2$senti_comp)

prld_dorm2 = merge(df_com_daily, dorm2,by.x = 'date', by.y = 'date2')%>%
  dplyr::select(c(date,Frequency, senti_comp))
cor.test(log(prld_dorm2$Frequency), prld_dorm2$senti_comp)

prld_extend2 = merge(df_com_daily, extend2,by.x = 'date', by.y = 'date2')%>%
  dplyr::select(c(date,Frequency, senti_comp))
cor.test(log(prld_extend2$Frequency), prld_extend2$senti_comp)

prld_pol2 = merge(df_com_daily, pol2,by.x = 'date', by.y = 'date2')%>%
  dplyr::select(c(date,Frequency, senti_comp))
cor.test(log(prld_pol2$Frequency), prld_pol2$senti_comp)

# phase: PtLD1
# topics: masks, virus infection, schools, leadership and more
prld_msk3 = merge(df_com_daily, masks3,by.x = 'date', by.y = 'date2')%>%
  dplyr::select(c(date,Frequency, senti_comp))
cor.test(log(prld_msk3$Frequency), prld_msk3$senti_comp)

prld_virus3 = merge(df_com_daily, virus3,by.x = 'date', by.y = 'date2')%>%
  dplyr::select(c(date,Frequency, senti_comp))
cor.test(log(prld_virus3$Frequency), prld_virus3$senti_comp)

prld_ldr3 = merge(df_com_daily, ldr3,by.x = 'date', by.y = 'date2')%>%
  dplyr::select(c(date,Frequency, senti_comp))
cor.test(log(prld_ldr3$Frequency), prld_ldr3$senti_comp)

prld_bdr3 = merge(df_com_daily, bdr3,by.x = 'date', by.y = 'date2')%>%
  dplyr::select(c(date,Frequency, senti_comp))
cor.test(log(prld_bdr3$Frequency), prld_bdr3$senti_comp)

prld_jb3 = merge(df_com_daily, jb3,by.x = 'date', by.y = 'date2')%>%
  dplyr::select(c(date,Frequency, senti_comp))
cor.test(log(prld_jb3$Frequency), prld_jb3$senti_comp)

prld_nur3 = merge(df_com_daily, nur3,by.x = 'date', by.y = 'date2')%>%
  dplyr::select(c(date,Frequency, senti_comp))
cor.test(log(prld_nur3$Frequency), prld_nur3$senti_comp)

prld_pol3 = merge(df_com_daily, pol3,by.x = 'date', by.y = 'date2')%>%
  dplyr::select(c(date,Frequency, senti_comp))
cor.test(log(prld_pol3$Frequency), prld_pol3$senti_comp)

prld_sui3 = merge(df_com_daily, sui3,by.x = 'date', by.y = 'date2')%>%
  dplyr::select(c(date,Frequency, senti_comp))
cor.test(log(prld_sui3$Frequency), prld_sui3$senti_comp)

# phase: PtLD2
# topics: masks, virus infection, schools, leadership and more
prld_msk4 = merge(df_com_daily, masks4,by.x = 'date', by.y = 'date2')%>%
  dplyr::select(c(date,Frequency, senti_comp))
cor.test(log(prld_msk4$Frequency), prld_msk4$senti_comp)

prld_virus4 = merge(df_com_daily, virus4,by.x = 'date', by.y = 'date2')%>%
  dplyr::select(c(date,Frequency, senti_comp))
cor.test(log(prld_virus4$Frequency), prld_virus4$senti_comp)

prld_ldr4 = merge(df_com_daily, ldr4,by.x = 'date', by.y = 'date2')%>%
  dplyr::select(c(date,Frequency, senti_comp))
cor.test(log(prld_ldr4$Frequency), prld_ldr4$senti_comp)

prld_jb4 = merge(df_com_daily, jb4,by.x = 'date', by.y = 'date2')%>%
  dplyr::select(c(date,Frequency, senti_comp))
cor.test(log(prld_jb4$Frequency), prld_jb4$senti_comp)

prld_pol4 = merge(df_com_daily, pol4,by.x = 'date', by.y = 'date2')%>%
  dplyr::select(c(date,Frequency, senti_comp))
cor.test(log(prld_pol4$Frequency), prld_pol4$senti_comp)

prld_sui4 = merge(df_com_daily, sui4,by.x = 'date', by.y = 'date2')%>%
  dplyr::select(c(date,Frequency, senti_comp))
cor.test(log(prld_sui4$Frequency), prld_sui4$senti_comp)

prld_rent4 = merge(df_com_daily, rent4,by.x = 'date', by.y = 'date2')%>%
  dplyr::select(c(date,Frequency, senti_comp))
cor.test(log(prld_rent4$Frequency), prld_rent4$senti_comp)  

prld_fd4 = merge(df_com_daily, fd4,by.x = 'date', by.y = 'date2')%>%
  dplyr::select(c(date,Frequency, senti_comp))
cor.test(log(prld_fd4$Frequency), prld_fd4$senti_comp)
prld_imp4 = merge(df_com_daily, import4,by.x = 'date', by.y = 'date2')%>%
  dplyr::select(c(date,Frequency, senti_comp))
cor.test(log(prld_imp4$Frequency), prld_imp4$senti_comp)

prld_bdr4 = merge(df_com_daily, bdr4,by.x = 'date', by.y = 'date2')%>%
  dplyr::select(c(date,Frequency, senti_comp))
cor.test(log(prld_bdr4$Frequency), prld_bdr4$senti_com)

prld_vac4 = merge(df_com_daily, vac4,by.x = 'date', by.y = 'date2')%>%
  dplyr::select(c(date,Frequency, senti_comp))
cor.test(log(prld_vac4$Frequency), prld_vac4$senti_com)
