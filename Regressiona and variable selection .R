#to find the correlation between the variabels and how to remove them. 
cor(monitor_1@data[,14],monitor_1@data[,15:72])    




LM.all_above<-lm(annual~buildcount_5000+buildcount_1000+buildcount_500+ buildcount_300+buildcount_100+buildcount_25+rdcount_1000+rdcount_500+rdcount_300+rdcount_100+rdcount_50+rdcount_5000+mjrdcount_5000+mjrdcount_1000+mjrdcount_500+mjrdcount_300+mjrdcount_100+mjrdcount_50+minrdcount_5000+minrdcount_1000+minrdcount_500+minrdcount_300+minrdcount_100+minrdcount_50+rdlength_1000+rdlength_500+rdlength_300+rdlength_100+rdlength_5000+rdlength_50+mjrdlength_5000+mjrdlength_1000+mjrdlength_300+mjrdlength_100+dist.mjrd+congestion_minrd+ minrdlength_5000+ minrdlength_1000 +minrdlength_500+minrdlength_300 +minrdlength_100 +minrdlength_50,monitor_1)
summary(LM.all_above)
# r improvement bases later p value
summary(lm(annual~rdcount_300+rdlength_100+mjrdlength_5000+minrdlength_5000,monitor_1))

#selection 2 p value of whole model and later p vlaue based removal of the variable 
summary(lm(annual~rdcount_1000+minrdcount_100+minrdcount_500+rdlength_100+rdlength_5000+rdlength_50+mjrdlength_300+dist.mjrd+minrdlength_5000,monitor_1))

plot(shape_mun)

#final model for  Muenster 
LM_selected <-lm(annual~rdcount_1000+minrdcount_100+minrdcount_500+rdlength_100+rdlength_5000+rdlength_50+mjrdlength_300+dist.mjrd+minrdlength_5000,monitor_1)
summary(LM_selected)
summary(stepAIC(LM_selected))


prediction.LM.muenster<-predict(LM_muenster,newdata=newdat4pred)
prediciton.df<-data.frame(newdat4pred@coords,prediction.LM.muenster)

coordinates(prediciton.df)<-~x1+x2
proj4string(prediciton.df)<-proj4string(shape_mun)
class(prediciton.df)
plot(shape_mun)
plot(prediciton.df)
spplot(prediciton.df)
gridded(prediciton.df) <- TRUE
plot(prediciton.df,main="Predicted After forward slection based on R^2 increase")
plot(roads,add=T)
plot(major.roads,add=T)



monitor.raw<-readShapePoints('~/Documents/Data for analysis Muenster/Shape files 32632/SHAPE FILE/Prediction points 1000 /New monitor /new_monitors1')
projection(monitor.raw) <- CRS("+proj=utm +zone=32 +ellps=WGS84 +datum=WGS84 +units=m +no_defs")
monitor.raw.transform<-spTransform(monitor.raw,"+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs" )
monitor.raw.transform$X<-monitor.raw.transform@coords[,1]
monitor.raw.transform$Y<-monitor.raw.transform@coords[,2]
dat1=data.frame(monitor.raw.transform[c('X','Y')],res=rstudent(LM.For.selection_no2_play))
dat2=dat1
geo1=as.geodata(dat1,coords.col=1:2,data.col=3)
plot(geo1)
var1=variog(geo1,estimator.type = 'modulus')
var_clas<-variog(geo1)
plot(var1)
covars


ev=eyefit(var1)
mod_vg=as.vgm.variomodel(ev[[1]])
dat3=monitor.raw.transform@data
coordinates(dat3)=c('X','Y')
class(dat3)
newdat1=newdat4pred
gridded(newdat1)<-TRUE
class(newdat1)
plot(newdat1)
pred_rk=krige(annual~buildcount_125+rdcount_1000+rdcount_500+rdlength_500+mjrdlength_500+mjrdlength_250+minrdlength_250+congestion_mjrd,monitor,newdata=newdat4pred,model=mod_vg)
spplot(pred_rk,sp.layout=list(li,pts),scales=list(draw=T),pretty=T,key.space='right')

