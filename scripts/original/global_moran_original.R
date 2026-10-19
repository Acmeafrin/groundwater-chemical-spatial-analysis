#=Global Moran 
{
  dat <- read.csv("Groundwater(17-22).csv")
  {
    dat$No. = as.numeric(dat$No.)
    dat$Year = as.numeric(dat$Year)
    dat$latitude = as.numeric(dat$latitude)
    dat$longitude = as.numeric(dat$longitude)
    dat$GQI = as.numeric(dat$GQI)
    dat$General_Bacteria = as.numeric(dat$General_Bacteria)
  }
  dat = subset(dat, dat$?õ? == "??û????")
  ad = subset(dat, dat$Age == "Adult")
  ch = subset(dat, dat$Age == "Child")
  
  #?ϴ? 0????�� ?? ??�� => ?????м?�� ��?ؼ? 0?? X
  ad[c(17:56)][ad[c(17:56)]==0]<-0.000001 # Zero values in certain columns (from 17 to 56) are replaced with a very small number (0.000001). This might be done to avoid issues with calculations that do not handle zeros wel
  ch[c(17:56)][ch[c(17:56)]==0]<-0.000001
  str(ad)
  ad.net = ad[complete.cases(ad$longitude), ] #NA?? ��??! (3?? ?? ��??)
  ch.net = ch[complete.cases(ch$longitude), ] #NA?? ��??! (3?? ?? ��??)
  
  #?????? ǥ??ȭ (x?? ????= (???????? ????-????)/ǥ??????) ?? z-score ???? -> ????�� 0��?? ǥ???????? 1?? ??????
  #Adult
  ad.net.s = scale(ad.net[c(17,19,20,22,23:56)])
  ad.net.s = as.data.frame(ad.net.s)
  ad.net = cbind(ad.net[c(1:11)], ad.net.s)
  summary(ad.net$General_Bacteria) #ǥ??ȭ?? ?? ?Ƴ? Ȯ??
  summary(ad.net$HQ.total) #ǥ??ȭ?? ?? ?Ƴ? Ȯ??
  #Child
  ch.net.s = scale(ch.net[c(17,19,20,22,23:56)])
  ch.net.s = as.data.frame(ch.net.s)
  ch.net = cbind(ch.net[c(1:11)], ch.net.s)
  summary(ch.net$General_Bacteria) #ǥ??ȭ?? ?? ?Ƴ? Ȯ??
  summary(ch.net$HQ.total) #ǥ??ȭ?? ?? ?Ƴ? Ȯ??
  
  #Good
  head(ad.net)
  head(ch.net)
  
  #?̿? ???? ????Ʈ ?????ϱ?
  coords.ad <- cbind(ad.net$longitude, ad.net$latitude)
  coords.ch <- cbind(ch.net$longitude, ch.net$latitude)
  nb.ad <- knn2nb(knearneigh(coords.ad, k = 20)) # Data normalization or standardization is applied to selected columns to ensure that different scales do not distort the analysis. 
  nb.ch <- knn2nb(knearneigh(coords.ch, k = 20)) # 
  
  #???? ????ġ ???? ?????ϱ?
  # dmat <- as.matrix (dist (cbind (ad.net$longitude, ad.net$latitude)))
  w.ad <- nb2listw (nb.ad, style = "minmax") #Coordinates for each data point are prepared, and nearest neighbors are identified using the knn2nb function, which is part of creating a spatial weights matrix using the nb2listw function. This matrix is crucial for calculating Moran's I as it defines the spatial relationship between observations.
  w.ch <- nb2listw (nb.ch, style = "minmax")
  
  #=================================================  
  #moran??s index ?????ϱ?
  
  #NO3-N
  moran.test(ad.net$NO3.N, w.ad)
  mt <- moran.test(ad.net$NO3.N, w.ad)
  z_score <- (mt$estimate["Moran I statistic"] - mt$estimate["Expectation"]) / sqrt(mt$estimate["Variance"]) # z-score ?????? ?????ϱ?
  print(z_score) # z-score ?? ?????ϱ?
  
  #NO3.N
  x = ad.net$NO3.N #???????? ??��
  wx <- lag.listw(w.ad, x) # The lag.listw function computes the spatial lag of a variable, which is the weighted average of neighboring values. This is often plotted against the variable itself to visualize spatial autocorrelation.
  xwx.lm <- lm(wx ~ x) # ȸ?͸??? 
  infl.xwx <- influence.measures(xwx.lm) # ?????? ??�� 
  is.inf <- which(apply(infl.xwx$is.inf, 1, any)) # ???????? ū ��???? ?ε??? 
  labels <- as.character(ad.net$id) # ???̺?
  
  #moran??s index ??��?? ?׷??? ?׸???
  NO3.N.moran = ggplot(data.frame(x = x, wx = wx), aes(x = x, y = wx)) + # ?????? ?��??Ӱ? ???? ??�� 
    geom_point(aes(color = interaction(cut(x, c(-Inf, mean(x), Inf)), cut(wx, c(-Inf, mean(wx), Inf)))),size = 2.5) +
    scale_color_manual(values = c("yellow2", "dodgerblue3", "springgreen3", "red2")) + # ��?? ????�� ????��?? ??��
    geom_smooth(method = "lm", se = FALSE) + # ȸ?ͼ? ǥ?? 
    geom_hline(yintercept = mean(wx), lty = 2) + # y=x ?? ǥ?? 
    geom_vline(xintercept = mean(x), lty = 2) + # y=x ?? ǥ?? 
    # geom_point(data = data.frame(x = x[is.inf], wx = wx[is.inf]), shape = 9) + # ???????? ū ��?? ǥ?? 
    # geom_text(data = data.frame(x = x[is.inf], wx = wx[is.inf], labels = labels[is.inf]), aes(label = labels), vjust = -0.5) + # ???????? ū ��?鿡 ???̺? ???̱? 
    labs(title = expression(paste("Moran??s index scatterplot (", NO[3]~"-"~N, ")")),
         x = expression(paste("Standardized ", NO[3]~"-"~N)),
         y = expression(paste("Spatially lagged ", NO[3]~"-"~N))) + # y?? ???̺? ??�� 
    theme_bw() + # ?׸? ??�� 
    theme(panel.grid= element_blank(),
          axis.title = element_text(size = 20),
          axis.text = element_text(size = 20),
          title = element_text(size=0),
          legend.text = element_text(size=20),
          legend.title=element_text(size=20),
          legend.position = "NA")+
    xlim(-0.5,10) + ylim(-1,2.4)+
    annotate("text",x=7.5, y=2.1, label = "Moran's I = 0.172", size=10)
  
  NO3.N.moran
  #=================
  #Turbidity
  moran.test(ad.net$Turbidity, w.ad)
  mt <- moran.test(ad.net$Turbidity, w.ad)
  z_score <- (mt$estimate["Moran I statistic"] - mt$estimate["Expectation"]) / sqrt(mt$estimate["Variance"]) # z-score ?????? ?????ϱ?
  print(z_score) # z-score ?? ?????ϱ?
  
  #Turbidity
  x = ad.net$Turbidity #???????? ??��
  wx <- lag.listw(w.ad, x) # ?????? ?????? 
  xwx.lm <- lm(wx ~ x) # ȸ?͸??? 
  infl.xwx <- influence.measures(xwx.lm) # ?????? ??�� 
  is.inf <- which(apply(infl.xwx$is.inf, 1, any)) # ???????? ū ��???? ?ε??? 
  labels <- as.character(ad.net$id) # ???̺?
  
  #moran??s index ??��?? ?׷??? ?׸???
  Turbidity.moran = ggplot(data.frame(x = x, wx = wx), aes(x = x, y = wx)) + # ?????? ?��??Ӱ? ???? ??�� 
    geom_point(aes(color = interaction(cut(x, c(-Inf, mean(x), Inf)), cut(wx, c(-Inf, mean(wx), Inf)))),size = 2.5) +
    scale_color_manual(values = c("yellow2", "dodgerblue3", "springgreen3", "red2")) + # ��?? ????�� ????��?? ??��
    geom_smooth(method = "lm", se = FALSE) + # ȸ?ͼ? ǥ?? 
    geom_hline(yintercept = mean(wx), lty = 2) + # y=x ?? ǥ?? 
    geom_vline(xintercept = mean(x), lty = 2) + # y=x ?? ǥ?? 
    # geom_point(data = data.frame(x = x[is.inf], wx = wx[is.inf]), shape = 9) + # ???????? ū ��?? ǥ?? 
    # geom_text(data = data.frame(x = x[is.inf], wx = wx[is.inf], labels = labels[is.inf]), aes(label = labels), vjust = -0.5) + # ???????? ū ��?鿡 ???̺? ???̱? 
    labs(title = "Moran??s index scatterplot (Turbidity)", # ��?? ??�� 
         x = "Standardized Turbidity", # x?? ???̺? ??�� 
         y = "Spatially lagged Turbidity") + # y?? ???̺? ??�� 
    theme_bw() + # ?׸? ??�� 
    theme(panel.grid= element_blank(),
          axis.title = element_text(size = 20),
          axis.text = element_text(size = 20),
          title = element_text(size=0),
          legend.text = element_text(size=20),
          legend.title=element_text(size=20),
          legend.position = "NA")+
    xlim(-2,10) + ylim(-0.3,1)+
    annotate("text",x=7, y=0.9, label = "Moran's I = 0.002", size=10)
  
  Turbidity.moran
  #=================
  #As
  moran.test(ad.net$As, w.ad)
  mt <- moran.test(ad.net$As, w.ad)
  z_score <- (mt$estimate["Moran I statistic"] - mt$estimate["Expectation"]) / sqrt(mt$estimate["Variance"]) # z-score ?????? ?????ϱ?
  print(z_score) # z-score ?? ?????ϱ?
  
  #As
  x = ad.net$As #???????? ??��
  wx <- lag.listw(w.ad, x) # ?????? ?????? 
  xwx.lm <- lm(wx ~ x) # ȸ?͸??? 
  infl.xwx <- influence.measures(xwx.lm) # ?????? ??�� 
  is.inf <- which(apply(infl.xwx$is.inf, 1, any)) # ???????? ū ��???? ?ε??? 
  labels <- as.character(ad.net$id) # ???̺?
  
  #moran??s index ??��?? ?׷??? ?׸???
  As.moran = ggplot(data.frame(x = x, wx = wx), aes(x = x, y = wx)) + # ?????? ?��??Ӱ? ???? ??�� 
    geom_point(aes(color = interaction(cut(x, c(-Inf, mean(x), Inf)), cut(wx, c(-Inf, mean(wx), Inf)))),size = 2.5) +
    scale_color_manual(values = c("yellow2", "dodgerblue3", "springgreen3", "red2")) + # ��?? ????�� ????��?? ??��
    geom_smooth(method = "lm", se = FALSE) + # ȸ?ͼ? ǥ?? 
    geom_hline(yintercept = mean(wx), lty = 2) + # y=x ?? ǥ?? 
    geom_vline(xintercept = mean(x), lty = 2) + # y=x ?? ǥ?? 
    # geom_point(data = data.frame(x = x[is.inf], wx = wx[is.inf]), shape = 9) + # ???????? ū ��?? ǥ?? 
    # geom_text(data = data.frame(x = x[is.inf], wx = wx[is.inf], labels = labels[is.inf]), aes(label = labels), vjust = -0.5) + # ???????? ū ��?鿡 ???̺? ???̱? 
    labs(title = "Moran??s index scatterplot (As)", # ��?? ??�� 
         x = "Standardized As", # x?? ???̺? ??�� 
         y = "Spatially lagged As") + # y?? ???̺? ??�� 
    theme_bw() + # ?׸? ??�� 
    theme(panel.grid= element_blank(),
          axis.title = element_text(size = 20),
          axis.text = element_text(size = 20),
          title = element_text(size=0),
          legend.text = element_text(size=20),
          legend.title=element_text(size=20),
          legend.position = "NA")+
    xlim(-2,12) + ylim(-1,4)+ annotate("text",x=8.7, y=3.6, label = "Moran's I = 0.069", size=10)
  
  As.moran
  #=================
  #Fe
  moran.test(ad.net$Fe, w.ad)
  mt <- moran.test(ad.net$Fe, w.ad)
  z_score <- (mt$estimate["Moran I statistic"] - mt$estimate["Expectation"]) / sqrt(mt$estimate["Variance"]) # z-score ?????? ?????ϱ?
  print(z_score) # z-score ?? ?????ϱ?
  
  #Fe
  x = ad.net$Fe #???????? ??��
  wx <- lag.listw(w.ad, x) # ?????? ?????? 
  xwx.lm <- lm(wx ~ x) # ȸ?͸??? 
  infl.xwx <- influence.measures(xwx.lm) # ?????? ??�� 
  is.inf <- which(apply(infl.xwx$is.inf, 1, any)) # ???????? ū ��???? ?ε??? 
  labels <- as.character(ad.net$id) # ???̺?
  
  #moran??s index ??��?? ?׷??? ?׸???
  Fe.moran = ggplot(data.frame(x = x, wx = wx), aes(x = x, y = wx)) + # ?????? ?��??Ӱ? ???? ??�� 
    geom_point(aes(color = interaction(cut(x, c(-Inf, mean(x), Inf)), cut(wx, c(-Inf, mean(wx), Inf)))),size = 2.5) +
    scale_color_manual(values = c("yellow2", "dodgerblue3", "springgreen3", "red2")) + # ��?? ????�� ????��?? ??��
    geom_smooth(method = "lm", se = FALSE) + # ȸ?ͼ? ǥ?? 
    geom_hline(yintercept = mean(wx), lty = 2) + # y=x ?? ǥ?? 
    geom_vline(xintercept = mean(x), lty = 2) + # y=x ?? ǥ?? 
    # geom_point(data = data.frame(x = x[is.inf], wx = wx[is.inf]), shape = 9) + # ???????? ū ��?? ǥ?? 
    # geom_text(data = data.frame(x = x[is.inf], wx = wx[is.inf], labels = labels[is.inf]), aes(label = labels), vjust = -0.5) + # ???????? ū ��?鿡 ???̺? ???̱? 
    labs(title = "Moran??s index scatterplot (Fe)", # ��?? ??�� 
         x = "Standardized Fe", # x?? ???̺? ??�� 
         y = "Spatially lagged Fe") + # y?? ???̺? ??�� 
    theme_bw() + # ?׸? ??�� 
    theme(panel.grid= element_blank(),
          axis.title = element_text(size = 20),
          axis.text = element_text(size = 20),
          title = element_text(size=0),
          legend.text = element_text(size=20),
          legend.title=element_text(size=20),
          legend.position = "NA")+
    xlim(-1,4) + ylim(-0.3,0.8)+ annotate("text",x=2.7, y=0.7, label = "Moran's I = 0.004", size=10)
  
  Fe.moran
  #=================
  #Al
  moran.test(ad.net$Al, w.ad)
  mt <- moran.test(ad.net$Al, w.ad)
  z_score <- (mt$estimate["Moran I statistic"] - mt$estimate["Expectation"]) / sqrt(mt$estimate["Variance"]) # z-score ?????? ?????ϱ?
  print(z_score) # z-score ?? ?????ϱ?
  
  #Al
  x = ad.net$Al #???????? ??��
  wx <- lag.listw(w.ad, x) # ?????? ?????? 
  xwx.lm <- lm(wx ~ x) # ȸ?͸??? 
  infl.xwx <- influence.measures(xwx.lm) # ?????? ??�� 
  is.inf <- which(apply(infl.xwx$is.inf, 1, any)) # ???????? ū ��???? ?ε??? 
  labels <- as.character(ad.net$id) # ???̺?
  
  #moran??s index ??��?? ?׷??? ?׸???
  Al.moran = ggplot(data.frame(x = x, wx = wx), aes(x = x, y = wx)) + # ?????? ?��??Ӱ? ???? ??�� 
    geom_point(aes(color = interaction(cut(x, c(-Inf, mean(x), Inf)), cut(wx, c(-Inf, mean(wx), Inf)))),size = 2.5) +
    scale_color_manual(values = c("yellow2", "dodgerblue3", "springgreen3", "red2")) + # ��?? ????�� ????��?? ??��
    geom_smooth(method = "lm", se = FALSE) + # ȸ?ͼ? ǥ?? 
    geom_hline(yintercept = mean(wx), lty = 2) + # y=x ?? ǥ?? 
    geom_vline(xintercept = mean(x), lty = 2) + # y=x ?? ǥ?? 
    # geom_point(data = data.frame(x = x[is.inf], wx = wx[is.inf]), shape = 9) + # ???????? ū ��?? ǥ?? 
    # geom_text(data = data.frame(x = x[is.inf], wx = wx[is.inf], labels = labels[is.inf]), aes(label = labels), vjust = -0.5) + # ???????? ū ��?鿡 ???̺? ???̱? 
    labs(title = "Moran??s index scatterplot (Al)", # ��?? ??�� 
         x = "Standardized Al", # x?? ???̺? ??�� 
         y = "Spatially lagged Al") + # y?? ???̺? ??�� 
    theme_bw() + # ?׸? ??�� 
    theme(panel.grid= element_blank(),
          axis.title = element_text(size = 20),
          axis.text = element_text(size = 20),
          title = element_text(size=0),
          legend.text = element_text(size=20),
          legend.title=element_text(size=20),
          legend.position = "NA")+
    xlim(-2,15) + ylim(-0.5,2)+ annotate("text",x=10.5, y=1.85, label = "Moran's I = 0.074", size=10)
  
  Al.moran
  #==============
  #F
  moran.test(ad.net$F, w.ad) 
  mt <- moran.test(ad.net$F, w.ad) 
  z_score <- (mt$estimate["Moran I statistic"] - mt$estimate["Expectation"]) / sqrt(mt$estimate["Variance"]) # z-score ?????? ?????ϱ?
  print(z_score) # z-score ?? ?????ϱ?
  
  #F
  x = ad.net$F #???????? ??��
  wx <- lag.listw(w.ad, x) # ?????? ?????? 
  xwx.lm <- lm(wx ~ x) # ȸ?͸??? 
  infl.xwx <- influence.measures(xwx.lm) # ?????? ??�� 
  is.inf <- which(apply(infl.xwx$is.inf, 1, any)) # ???????? ū ��???? ?ε??? 
  labels <- as.character(ad.net$id) # ???̺?
  #moran??s index ??��?? ?׷??? ?׸???
  F.moran = ggplot(data.frame(x = x, wx = wx), aes(x = x, y = wx)) + # ?????? ?��??Ӱ? ???? ??�� 
    geom_point(aes(color = interaction(cut(x, c(-Inf, mean(x), Inf)), cut(wx, c(-Inf, mean(wx), Inf)))),size = 2.5) +
    scale_color_manual(values = c("yellow2", "dodgerblue3", "springgreen3", "red2")) + # ��?? ????�� ????��?? ??��
    geom_smooth(method = "lm", se = FALSE) + # ȸ?ͼ? ǥ?? 
    geom_hline(yintercept = mean(wx), lty = 2) + # y=x ?? ǥ?? 
    geom_vline(xintercept = mean(x), lty = 2) + # y=x ?? ǥ?? 
    # geom_point(data = data.frame(x = x[is.inf], wx = wx[is.inf]), shape = 9) + # ???????? ū ��?? ǥ?? 
    # geom_text(data = data.frame(x = x[is.inf], wx = wx[is.inf], labels = labels[is.inf]), aes(label = labels), vjust = -0.5) + # ???????? ū ��?鿡 ???̺? ???̱? 
    labs(title = "Moran??s index scatterplot (F)", # ��?? ??�� 
         x = "Standardized F", # x?? ???̺? ??�� 
         y = "Spatially lagged F") + # y?? ???̺? ??�� 
    theme_bw() + # ?׸? ??�� 
    theme(panel.grid= element_blank(),
          axis.title = element_text(size = 20),
          axis.text = element_text(size = 20),
          title = element_text(size=0),
          legend.text = element_text(size=20),
          legend.title=element_text(size=20),
          legend.position = "NA") +
    # xlim(-0.5,5) + ylim(-0.3,0.8)+ 
    annotate("text",x=12, y=3.2, label = "Moran's I = 0.171", size=10)
  F.moran
  #==============
  #Mn
  moran.test(ad.net$Mn, w.ad)
  mt <- moran.test(ad.net$Mn, w.ad) 
  z_score <- (mt$estimate["Moran I statistic"] - mt$estimate["Expectation"]) / sqrt(mt$estimate["Variance"]) # z-score ?????? ?????ϱ?
  print(z_score) # z-score ?? ?????ϱ?
  
  #Mn
  x = ad.net$Mn #???????? ??��
  wx <- lag.listw(w.ad, x) # ?????? ?????? 
  xwx.lm <- lm(wx ~ x) # ȸ?͸??? 
  infl.xwx <- influence.measures(xwx.lm) # ?????? ??�� 
  is.inf <- which(apply(infl.xwx$is.inf, 1, any)) # ???????? ū ��???? ?ε??? 
  labels <- as.character(ad.net$id) # ???̺?
  #moran??s index ??��?? ?׷??? ?׸???
  Mn.moran = ggplot(data.frame(x = x, wx = wx), aes(x = x, y = wx)) + # ?????? ?��??Ӱ? ???? ??�� 
    geom_point(aes(color = interaction(cut(x, c(-Inf, mean(x), Inf)), cut(wx, c(-Inf, mean(wx), Inf)))),size = 2.5) +
    scale_color_manual(values = c("yellow2", "dodgerblue3", "springgreen3", "red2")) + # ��?? ????�� ????��?? ??��
    geom_smooth(method = "lm", se = FALSE) + # ȸ?ͼ? ǥ?? 
    geom_hline(yintercept = mean(wx), lty = 2) + # y=x ?? ǥ?? 
    geom_vline(xintercept = mean(x), lty = 2) + # y=x ?? ǥ?? 
    # geom_point(data = data.frame(x = x[is.inf], wx = wx[is.inf]), shape = 9) + # ???????? ū ��?? ǥ?? 
    # geom_text(data = data.frame(x = x[is.inf], wx = wx[is.inf], labels = labels[is.inf]), aes(label = labels), vjust = -0.5) + # ???????? ū ��?鿡 ???̺? ???̱? 
    labs(title = "Moran??s index scatterplot (Mn)", # ��?? ??�� 
         x = "Standardized Mn", # x?? ???̺? ??�� 
         y = "Spatially lagged Mn") + # y?? ???̺? ??�� 
    theme_bw() + # ?׸? ??�� 
    theme(panel.grid= element_blank(),
          axis.title = element_text(size = 20),
          axis.text = element_text(size = 20),
          title = element_text(size=0),
          legend.text = element_text(size=20),
          legend.title=element_text(size=20),
          legend.position = "NA")+
    xlim(-2,15) + ylim(-1,3)+ annotate("text",x=10, y=2.7, label = "Moran's I = 0.027", size=10)
  Mn.moran
  #==============
  #pH
  moran.test(ad.net$pH, w.ad)
  mt <- moran.test(ad.net$pH, w.ad) 
  z_score <- (mt$estimate["Moran I statistic"] - mt$estimate["Expectation"]) / sqrt(mt$estimate["Variance"]) # z-score ?????? ?????ϱ?
  print(z_score) # z-score ?? ?????ϱ?
  
  #pH
  x = ad.net$pH #???????? ??��
  wx <- lag.listw(w.ad, x) # ?????? ?????? 
  xwx.lm <- lm(wx ~ x) # ȸ?͸??? 
  infl.xwx <- influence.measures(xwx.lm) # ?????? ??�� 
  is.inf <- which(apply(infl.xwx$is.inf, 1, any)) # ???????? ū ��???? ?ε??? 
  labels <- as.character(ad.net$id) # ???̺?
  #moran??s index ??��?? ?׷??? ?׸???
  pH.moran = ggplot(data.frame(x = x, wx = wx), aes(x = x, y = wx)) + # ?????? ?��??Ӱ? ???? ??�� 
    geom_point(aes(color = interaction(cut(x, c(-Inf, mean(x), Inf)), cut(wx, c(-Inf, mean(wx), Inf)))),size = 2.5) +
    scale_color_manual(values = c("yellow2", "dodgerblue3", "springgreen3", "red2")) + # ��?? ????�� ????��?? ??��
    geom_smooth(method = "lm", se = FALSE) + # ȸ?ͼ? ǥ?? 
    geom_hline(yintercept = mean(wx), lty = 2) + # y=x ?? ǥ?? 
    geom_vline(xintercept = mean(x), lty = 2) + # y=x ?? ǥ?? 
    # geom_point(data = data.frame(x = x[is.inf], wx = wx[is.inf]), shape = 9) + # ???????? ū ��?? ǥ?? 
    # geom_text(data = data.frame(x = x[is.inf], wx = wx[is.inf], labels = labels[is.inf]), aes(label = labels), vjust = -0.5) + # ???????? ū ��?鿡 ???̺? ???̱? 
    labs(title = "Moran??s index scatterplot (pH)", # ��?? ??�� 
         x = "Standardized pH", # x?? ???̺? ??�� 
         y = "Spatially lagged pH") + # y?? ???̺? ??�� 
    theme_bw() + # ?׸? ??�� 
    theme(panel.grid= element_blank(),
          axis.title = element_text(size = 20),
          axis.text = element_text(size = 20),
          title = element_text(size=0),
          legend.text = element_text(size=20),
          legend.title=element_text(size=20),
          legend.position = "NA")+
    xlim(-4,5) + ylim(-2,3)+ annotate("text",x=2.8, y=2.7, label = "Moran's I = 0.348", size=10)
  pH.moran