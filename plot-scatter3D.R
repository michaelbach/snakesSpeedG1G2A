require(reshape2)
dm = melt(data=d, id.vars=c("id", "trial", "conditionSequence", "g1", "g2"), measure.vars=c("velocity"));  head(dm)

dc1 = dcast(data=dm, formula=g1 + g2 ~ variable, fun.aggregate=mean, na.rm=TRUE)
dc2 = dcast(data=dm, formula=g1 + g2 ~ variable, fun.aggregate=function(x, y){length(x)})
dc3 = dcast(data=dm, formula=g1 + g2 ~ variable, fun.aggregate=sd, na.rm=TRUE)
dc = cbind(dc1, n=dc2$velocity, sd=dc3$velocity); head(dc)
dc$sem = dc$sd /sqrt(dc$n-1); head(dc)
#dc <- dc[order(dc[, 1], dc[, 2]), ] # Sortieren x, dann y nötig? NEIN


# with sticks
require(plot3D)
scatter3D(x=dc$g1, y=dc$g2, z=dc$velocity, 
          type = "h", bty = "g", pch = 20, cex = 2, ticktype = "detailed",
          xlim = c(0, 100), ylim = c(0, 100), zlim = c(-1, 1))


# with SEM
type = "h" # "h" oder "p"
sems=list(z=matrix(data=c(dc$sem, dc$sem), nrow=length(dc$sem)))
require(plot3D)
scatter3D(x=dc$g1, y=dc$g2, z=dc$velocity,
          phi = -10, theta = 45,
          type = type, bty = "g", pch = 20, cex = 2, ticktype = "detailed",
          xlim = c(0, 100), ylim = c(0, 100), zlim = c(0, 1.3), xlab="g1", ylab="g2", zlab="Velocity", lwd=5,
          CI=sems)
