# create interpolation
nPoints = 100
require(akima)
interpolResult = interp(x=dc$g1, y=dc$g2, z=dc$velocity, xo=seq(0, 100, length=nPoints), yo=seq(0, 100, length=nPoints),
                             linear=TRUE, extrap=FALSE, duplicate = "error", dupfun = NULL, ncp = NULL)
zm = melt(interpolResult$z);  names(zm) <- c("g1", "g2", "velocity")
zm =na.omit(zm)


# plot interpolated data as contour
require(plot3D)
scatter3D(x=zm$g1, y=zm$g2, z=zm$velocity, 
          phi = 30, theta = 40,
          type = "p", bty = "g", pch = 20, cex = 2, ticktype = "detailed",
          xlim = c(0, 100), ylim = c(0, 100), zlim = c(-.5, .5))


# convert 2 columns x,y,z to matrix
require(reshape2);  surface = acast(data=zm, formula = g1~g2, value.var="velocity")


# now plot the surface with various options

require(plot3D)
image2D(z=surface, shade = 0.4, asp=1)
image2D(z=surface, rasterImage = TRUE, contour = list(lwd = 2, col = jet.col(11)))
image2D(z=surface, lighting = TRUE, rasterImage = TRUE, contour = list(col = "white", labcex = 0.8, lwd = 3, alpha = 0.5), las=1)
persp3D(z=surface, clab = "m", shade = 0.2, phi = 30, theta = 40)


oldPar = par(pty="s", xpd=TRUE) # am attempt to make it square…
image2D(z=surface, rasterImage = TRUE, contour = list(lwd = 2, col = jet.col(11)),
        xlim=c(0, 1.0), ylim=c(0, 1.0), yaxs="i", xaxs="i", las=1, asp=1,
        xlab="g1", ylab="g2")
par(oldPar)

