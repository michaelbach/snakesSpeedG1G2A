# History
# =======
#
# 2013-11-21 begun
#

mdbToolPath = file.path(Sys.getenv("HOME"), "Documents", "Bach", "myProgs", "R","mdbTools")
source(file.path(mdbToolPath, "setThemeBach01.R"));  setThemeBach01(base_size=18)
#source(file.path(mdbToolPath, "pValueString.R"))
#source(file.path(mdbToolPath, "multiplot.R"))
#source(file.path(mdbToolPath, "rstr.R"))

setwd(file.path(Sys.getenv("HOME"), "Documents", "Bach", "Projekte", "FORSCHUNG", "Snake Illusion (LG)", "analysis"))
p = file.path("../", "data", "Messungen_2.xlsx")
if (file.exists(p)) {
  require(gdata);  d <- read.xls(xls = p, verbose=T, sheet=1)
} else {
  p = "Messungen.ods"
  if (file.exists(p)) {
    require(ROpenOffice);  d = read.ods(file = p);   # omegahat 
  } else {
    p = file.choose()
  }
}
names(d)
#colnames(d)[names(d) == "v_end"] = "velocity"
d$g1=d$grey11*100;  d$g2=d$grey21*100
d$direction = trim(as.character(d$direction)) # make sure only "l" and "r" exist without white space
stopifnot(identical(sort(unique(d$direction)), c("l", "r")))

d$velocity <- ifelse(d$direction == "l", d$v_end, -d$v_end)
if (p == "Messungen.xlsx") {
  d$g1 = ifelse(d$g1==12.5, 13, d$g1)
  d$g1 = ifelse(d$g1==62.5, 63, d$g1)
  d$g2 = ifelse(d$g2==37.5, 37, d$g2)
  d$g2 = ifelse(d$g2==38, 37, d$g2)
  d$g2 = ifelse(d$g2==62.5, 63, d$g2)
  for (i in 1:length(d$id)) { # aus Versehen wurden einige Werte unten gemessen, per Symmetrie klappen wir die um
    if (d$g1[i] > d$g2[i]) {
      temp = d$g2[i];  d$g2[i] = d$g1[i];  d$g1[i] = temp
      if (d$direction[i] == "l")  d$direction[i] = "r"
      else  d$direction[i] = "l"
    }
  }
}

#d = subset(d, d$id != 0)

require(reshape2)
dm = melt(data=d, id.vars=c("id", "trial", "conditionSequence", "g1", "g2"), measure.vars=c("velocity"));  #head(dm)
dc = dcast(data=dm, formula=g1 + g2 ~ variable, fun.aggregate=mean, na.rm=TRUE);  head(dc)

dc = rbind(dc, c(0, 0, 0));  dc = rbind(dc, c(100, 100, 0)) # add zeros bottom left + top right

# now create a simple plot to check on basic data integrity
dx = 5; dy=5; sze = 6
mmax = 1.05 * max(abs(dc$velocity))
setThemeBach01(base_size=20)
ggplot(data=dc, aes(x=g1, y=g2, fill=velocity)) +
  geom_tile(width=sze, height=sze) +
  coord_cartesian(xlim=c(-dx, 100+dx), ylim=c(-dy, 100+dy)) + 
  scale_fill_gradient2(name="Velocity [°/s]  ", low="green", mid = "grey", high="red", limits=c(-mmax, mmax), breaks=c(-1, 0, 1)) +
  theme(aspect.ratio = 1/1) +
  labs(x="g1 [%]", y="g2 [%]") +
  theme(legend.justification=c(1, 0), legend.position=c(1, 0))
