#Local moran

dat <- read.csv("for_local_moran.csv") 

dat$Year = as.numeric(dat$Year)

dat$latitude = as.numeric(dat$latitude)
dat$longitude = as.numeric(dat$longitude)
ad = dat

ad[c(10:17)][ad[c(10:17)]==0]<-0.000001

str(ad)
ad.net = ad[complete.cases(ad$longitude), ]

#Adult

# Assuming ad.net is a data frame
ad.net.s <- ad.net[c(10:17)]
ad.net.s = as.data.frame(ad.net.s)
ad.net = cbind(ad.net[c(1:8)], ad.net.s)
coords.ad <- cbind(ad.net$longitude, ad.net$latitude) #coordinates are combined
summary(ad.net$pH) 
head(ad.net)

nb.ad <- knn2nb(knearneigh(coords.ad, k = 5)) # neighbor object created using k- nearest neighbor
w.ad <- nb2listw (nb.ad, style = "minmax") #spatial wiight matrix to normalize

lisa.pH <- localmoran(ad.net$pH, w.ad)
lisa.pH = as.data.frame(lisa.pH)
colnames(lisa.pH) <- c("Ii", "E.Ii", "Var.Ii", "Z.Ii", "Pr")
# High-High, Low-High, High-Low, Low-Low ????
lisa.pH$group <- NA # ???ο? ?? ????
lisa.pH$group[lisa.pH$Z.Ii > 0 & lisa.pH$Ii > 0] <- "High-High" # High-High ��??
lisa.pH$group[lisa.pH$Z.Ii > 0 & lisa.pH$Ii < 0] <- "High-Low" # High-Low ��??
lisa.pH$group[lisa.pH$Z.Ii < 0 & lisa.pH$Ii > 0] <- "Low-High" # Low-High ��??
lisa.pH$group[lisa.pH$Z.Ii < 0 & lisa.pH$Ii < 0] <- "Low-Low" # Low-Low ��??
head(lisa.pH)
names(lisa.pH)[5] <- c("p.value")
head(lisa.pH)

lisa.NO3.N <- localmoran(ad.net$NO3.N, w.ad)
lisa.NO3.N = as.data.frame(lisa.NO3.N)
colnames(lisa.NO3.N) <- c("Ii", "E.Ii", "Var.Ii", "Z.Ii", "Pr")
# High-High, Low-High, High-Low, Low-Low 
lisa.NO3.N$group <- NA
lisa.NO3.N$group[lisa.NO3.N$Z.Ii > 0 & lisa.NO3.N$Ii > 0] <- "High-High" # High-High 
lisa.NO3.N$group[lisa.NO3.N$Z.Ii > 0 & lisa.NO3.N$Ii < 0] <- "High-Low" # High-Low 
lisa.NO3.N$group[lisa.NO3.N$Z.Ii < 0 & lisa.NO3.N$Ii > 0] <- "Low-High" # Low-High 
lisa.NO3.N$group[lisa.NO3.N$Z.Ii < 0 & lisa.NO3.N$Ii < 0] <- "Low-Low" # Low-Low 
head(lisa.NO3.N)
names(lisa.NO3.N)[5] <- c("p.value")
head(lisa.NO3.N)

lisa.F <- localmoran(ad.net$F, w.ad)
lisa.F = as.data.frame(lisa.F)
colnames(lisa.F) <- c("Ii", "E.Ii", "Var.Ii", "Z.Ii", "Pr")
# High-High, Low-High, High-Low, Low-Low 
lisa.F$group <- NA 
lisa.F$group[lisa.F$Z.Ii > 0 & lisa.F$Ii > 0] <- "High-High" # High-High 
lisa.F$group[lisa.F$Z.Ii > 0 & lisa.F$Ii < 0] <- "High-Low" # Low-Low 
lisa.F$group[lisa.F$Z.Ii < 0 & lisa.F$Ii > 0] <- "Low-High" # High-Low 
lisa.F$group[lisa.F$Z.Ii < 0 & lisa.F$Ii < 0] <- "Low-Low" # Low-High 
head(lisa.F)
names(lisa.F)[5] <- c("p.value")
head(lisa.F)

lisa.Al <- localmoran(ad.net$Al, w.ad)
lisa.Al = as.data.frame(lisa.Al)
colnames(lisa.Al) <- c("Ii", "E.Ii", "Var.Ii", "Z.Ii", "Pr")
# High-High, Low-High, High-Low, Low-Low ????
lisa.Al$group <- NA # ???ο? ?? ????
lisa.Al$group[lisa.Al$Z.Ii > 0 & lisa.Al$Ii > 0] <- "High-High" # High-High ��??
lisa.Al$group[lisa.Al$Z.Ii > 0 & lisa.Al$Ii < 0] <- "High-Low" # Low-Low ��??
lisa.Al$group[lisa.Al$Z.Ii < 0 & lisa.Al$Ii > 0] <- "Low-High" # High-Low ��??
lisa.Al$group[lisa.Al$Z.Ii < 0 & lisa.Al$Ii < 0] <- "Low-Low" # Low-High ��??
head(lisa.Al)
names(lisa.Al)[5] <- c("p.value")
head(lisa.Al)

lisa.As <- localmoran(ad.net$As, w.ad)
lisa.As = as.data.frame(lisa.As)
colnames(lisa.As) <- c("Ii", "E.Ii", "Var.Ii", "Z.Ii", "Pr")
# High-High, Low-High, High-Low, Low-Low ????
lisa.As$group <- NA # ???ο? ?? ????
lisa.As$group[lisa.As$Z.Ii > 0 & lisa.As$Ii > 0] <- "High-High" # High-High ��??
lisa.As$group[lisa.As$Z.Ii > 0 & lisa.As$Ii < 0] <- "High-Low" # Low-Low ��??
lisa.As$group[lisa.As$Z.Ii < 0 & lisa.As$Ii > 0] <- "Low-High" # High-Low ��??
lisa.As$group[lisa.As$Z.Ii < 0 & lisa.As$Ii < 0] <- "Low-Low" # Low-High ��??
head(lisa.As)
names(lisa.As)[5] <- c("p.vAsue")
head(lisa.As)

lisa.Mn <- localmoran(ad.net$Mn, w.ad)
lisa.Mn = as.data.frame(lisa.Mn)
colnames(lisa.Mn) <- c("Ii", "E.Ii", "Var.Ii", "Z.Ii", "Pr")
# High-High, Low-High, High-Low, Low-Low ????
lisa.Mn$group <- NA # ???ο? ?? ????
lisa.Mn$group[lisa.Mn$Z.Ii > 0 & lisa.Mn$Ii > 0] <- "High-High" # High-High ��??
lisa.Mn$group[lisa.Mn$Z.Ii > 0 & lisa.Mn$Ii < 0] <- "High-Low" # Low-Low ��??
lisa.Mn$group[lisa.Mn$Z.Ii < 0 & lisa.Mn$Ii > 0] <- "Low-High" # High-Low ��??
lisa.Mn$group[lisa.Mn$Z.Ii < 0 & lisa.Mn$Ii < 0] <- "Low-Low" # Low-High ��??
head(lisa.Mn)
names(lisa.Mn)[5] <- c("p.value")
head(lisa.Mn)

lisa.Fe <- localmoran(ad.net$Fe, w.ad)
lisa.Fe = as.data.frame(lisa.Fe)
colnames(lisa.Fe) <- c("Ii", "E.Ii", "Var.Ii", "Z.Ii", "Pr")
# High-High, Low-High, High-Low, Low-Low ????
lisa.Fe$group <- NA # ???ο? ?? ????
lisa.Fe$group[lisa.Fe$Z.Ii > 0 & lisa.Fe$Ii > 0] <- "High-High" # High-High ��??
lisa.Fe$group[lisa.Fe$Z.Ii > 0 & lisa.Fe$Ii < 0] <- "High-Low" # Low-Low ��??
lisa.Fe$group[lisa.Fe$Z.Ii < 0 & lisa.Fe$Ii > 0] <- "Low-High" # High-Low ��??
lisa.Fe$group[lisa.Fe$Z.Ii < 0 & lisa.Fe$Ii < 0] <- "Low-Low" # Low-High ��??
head(lisa.Fe)
names(lisa.Fe)[5] <- c("p.value")
head(lisa.Fe)

lisa.Turbidity <- localmoran(ad.net$Turbidity, w.ad)
lisa.Turbidity = as.data.frame(lisa.Turbidity)
colnames(lisa.Turbidity) <- c("Ii", "E.Ii", "Var.Ii", "Z.Ii", "Pr")
# High-High, Low-High, High-Low, Low-Low ????
lisa.Turbidity$group <- NA # ???ο? ?? ????
lisa.Turbidity$group[lisa.Turbidity$Z.Ii > 0 & lisa.Turbidity$Ii > 0] <- "High-High" # High-High ��??
lisa.Turbidity$group[lisa.Turbidity$Z.Ii > 0 & lisa.Turbidity$Ii < 0] <- "High-Low" # Low-Low ��??
lisa.Turbidity$group[lisa.Turbidity$Z.Ii < 0 & lisa.Turbidity$Ii > 0] <- "Low-High" # High-Low ��??
lisa.Turbidity$group[lisa.Turbidity$Z.Ii < 0 & lisa.Turbidity$Ii < 0] <- "Low-Low" # Low-High ��??
head(lisa.Turbidity)
names(lisa.Turbidity)[5] <- c("p.value")
head(lisa.Turbidity)
#=======================================
ad.cn = cbind(ad.net[c(1:8)],
              lisa.pH[5],lisa.pH[1],lisa.pH[4],lisa.pH[6],
              lisa.NO3.N[5],lisa.NO3.N[1],lisa.NO3.N[4],lisa.NO3.N[6],
              lisa.F[5],lisa.F[1],lisa.F[4],lisa.F[6],
              lisa.Al[5],lisa.Al[1],lisa.Al[4],lisa.Al[6],
              lisa.As[5],lisa.As[1],lisa.As[4],lisa.As[6],
              
              lisa.Mn[5],lisa.Mn[1],lisa.Mn[4],lisa.Mn[6],
              lisa.Fe[5],lisa.Fe[1],lisa.Fe[4],lisa.Fe[6],
              lisa.Turbidity[5],lisa.Turbidity[1],lisa.Turbidity[4],lisa.Turbidity[6])
names(ad.cn)[9] <- c("pH.lisa.p")
names(ad.cn)[10] <- c("pH.lisa.i")
names(ad.cn)[11] <- c("pH.lisa.z")
names(ad.cn)[12] <- c("pH.lisa.r")
names(ad.cn)[13] <- c("NO3.N.lisa.p")
names(ad.cn)[14] <- c("NO3.N.lisa.i")
names(ad.cn)[15] <- c("NO3.N.lisa.z")
names(ad.cn)[16] <- c("NO3.N.lisa.r")
names(ad.cn)[17] <- c("F.lisa.p")
names(ad.cn)[18] <- c("F.lisa.i")
names(ad.cn)[19] <- c("F.lisa.z")
names(ad.cn)[20] <- c("F.lisa.r")
names(ad.cn)[21] <- c("Al.lisa.p")
names(ad.cn)[22] <- c("Al.lisa.i")
names(ad.cn)[23] <- c("Al.lisa.z")
names(ad.cn)[24] <- c("Al.lisa.r")
names(ad.cn)[25] <- c("As.lisa.p")
names(ad.cn)[26] <- c("As.lisa.i")
names(ad.cn)[27] <- c("As.lisa.z")
names(ad.cn)[28] <- c("As.lisa.r")
names(ad.cn)[29] <- c("Mn.lisa.p")
names(ad.cn)[30] <- c("Mn.lisa.i")
names(ad.cn)[31] <- c("Mn.lisa.z")
names(ad.cn)[32] <- c("Mn.lisa.r")
names(ad.cn)[33] <- c("Fe.lisa.p")
names(ad.cn)[34] <- c("Fe.lisa.i")
names(ad.cn)[35] <- c("Fe.lisa.z")
names(ad.cn)[36] <- c("Fe.lisa.r")
names(ad.cn)[37] <- c("Turbidity.lisa.p")
names(ad.cn)[38] <- c("Turbidity.lisa.i")
names(ad.cn)[39] <- c("Turbidity.lisa.z")
names(ad.cn)[40] <- c("Turbidity.lisa.r")

write.csv(ad.cn, "lisa.csv", na="-", fileEncoding = "CP949")
