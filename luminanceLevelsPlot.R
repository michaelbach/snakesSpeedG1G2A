# 2013-11-23

makeDF = function(ix, iy) {
  dx = 0.04
  f = 0.1;  y1 = 0;  y2 = f*ix; f;  y3 = f; y4 = f*iy
  d = data.frame(x   =c( 0, dx,    dx,  2*dx, 2*dx, 3*dx, 3*dx), 
             y   =c(y1, y1,    y2,  y2,     y3, y3, y4), 
             xend=c(dx, dx,  2*dx,  2*dx, 3*dx, 3*dx, 4*dx),
             yend=c(y1, y2,    y2,  y3,     y3, y4, y4))
  d$x = d$x+ix;  d$xend=d$xend+ix;  d$y = d$y+iy;  d$yend=d$yend+iy
  d = d*100
  return(d)
}


rd = .6;  dx=8; dy=5
p = ggplot() +
  theme(aspect.ratio = 1/1) + coord_cartesian(xlim=c(-dx-1, 100+dx+1), ylim=c(-dy-1, 100+dy+1)) +
  scale_color_continuous(low=grey(0), high=grey(0.9), na.value="light blue", name="relative\ngrey\nlevel") +
  labs(x="g1 [%]", y="g2 [%]") +
  theme(legend.justification=c(1, 0), legend.position=c(1, 0)) 
for (ix in seq(from=0, to = 1, by=0.2)) {
  for (iy in seq(from=0, to = 1, by=0.2)) {
    if (iy>= ix) {
      dp = makeDF(ix=ix, iy=iy)
      dpr = data.frame(xmin=dp$x[1], ymin=dp$y[1], xmax=dp$xend[7], ymax=max(dp$yend))
      dp$col = c(0, NA, ix, NA, 1, NA, iy)
      p = p + geom_segment(data=dp, aes(x=x-dx, y=y-dy, xend=xend-dx, yend=yend-dy, colour=col), size=3, lineend="round")
      p = p + geom_rect(data=dpr, aes(xmin=xmin-rd-dx, ymin=ymin-rd-dy, xmax=xmax+rd-dx, ymax=ymax+rd-dy), colour=rgb(.3,1,.3), fill=NA)
        
    }
  }
}
print(p)