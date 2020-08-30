base <- subset(base_saz, base_saz$CONSOLID %in% "Embarque AIRJ_80_N")



Modelo <- lm(base$lnvendas  ~ base$lndolar + as.factor(base$CARNAVAL) + as.factor(base$MES) + as.factor(base$ANO) )
summary(Modelo)
