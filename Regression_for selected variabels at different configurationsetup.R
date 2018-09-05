#Linear regression using different monitoring station configuration
lm_monitor_1<-lm(annual~buildcount_125+rdcount_1000+rdcount_500+rdlength_500+mjrdlength_500+mjrdlength_250+minrdlength_250+congestion_mjrd,monitor_1)

summary(lm_monitor_1)

lm_monitor_2<-lm(annual~buildcount_125+rdcount_1000+rdcount_500+rdlength_500+mjrdlength_500+mjrdlength_250+minrdlength_250+congestion_mjrd,monitor_2)

summary(lm_monitor_2)

lm_monitor_3<-lm(annual~buildcount_125+rdcount_1000+rdcount_500+rdlength_500+mjrdlength_500+mjrdlength_250+minrdlength_250+congestion_mjrd,monitor_3)

summary(lm_monitor_3)

lm_monitor_4<-lm(annual~buildcount_125+rdcount_1000+rdcount_500+rdlength_500+mjrdlength_500+mjrdlength_250+minrdlength_250+congestion_mjrd,monitor_4)

summary(lm_monitor_2)
summary(model_lm)
plot(shape_mun)
plot(buildinggrid)
plot(monitor_1,add=TRUE,pch=1)
plot(monitor_2,add=T,pch=2)
plot(monitor_3,add=T,pch=3)
plot(monitor_4,add=T,pch=4)
