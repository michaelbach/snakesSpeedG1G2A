setThemeBach01(base_size=18)

# plot raw data as contour
require(ggplot2)
ggplot(data=dc, aes(x=g1, y=g2, z=velocity)) + 
  stat_contour(aes(colour = ..level..), size=3) + 
  scale_colour_gradient(low = "blue", high = "red") +
  theme(aspect.ratio = 1/1) +
  coord_cartesian(xlim=c(0, 100), ylim=c(0, 100))   


# plot interpolated data as contour
nPoints = 100
require(akima)
interpolationResult = interp(x=dc$g1, y=dc$g2, z=dc$velocity, xo=seq(0, 100, length=nPoints),
       yo=seq(0, 100, length=nPoints),
       linear=TRUE, extrap=FALSE, duplicate = "error", dupfun = NULL, ncp = NULL)
#persp(x=seq(0, 100, length.out = nPoints), y=seq(0, 100, length.out = nPoints), z=interpolationResult$z)
#filled.contour(x=seq(0, 100, length.out = nPoints), y=seq(0, 100, length.out=nPoints), z=zMatrix)

zm = melt(interpolationResult$z);  names(zm) <- c("g1", "g2", "velocity")
zm =na.omit(zm)
require(ggplot2)
ggplot(data=zm, aes(x=g1, y=g2, z=velocity)) +
  geom_tile(aes(fill=velocity), colour=NA) + 
  stat_contour(colour="dark gray", alpha=0.5, size=2) +
  scale_fill_gradient(low = "blue", high = "red") +
  #theme(panel.grid = element_blank()) +
  theme(aspect.ratio = 1/1) +
  coord_cartesian(xlim=c(0, 100), ylim=c(0, 100)) +
  labs(title=paste0("Subjects ", paste(unique(d$id), collapse=", ")))

