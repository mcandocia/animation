\name{pollen}
\alias{pollen}
\title{Synthetic dataset about the geometric features of pollen grains...}
\description{Synthetic dataset about the geometric features of pollen grains
There are 3848 observations on 5 variables. From the 1986 ASA Data Exposition
dataset, made up by David Coleman of RCA Labs.}
\docType{data}
\keyword{datasets}
\examples{data(pollen)
## some dense points in the center?
plot(pollen[, 1:2], pch = 20, col = rgb(0, 0, 0, 0.1))

## check with rgl
\dontrun{
library(rgl)
# ajust the view
uM = matrix(c(-0.370919227600098, -0.513357102870941,
-0.773877620697021, 0, -0.73050606250763, 0.675815105438232,
-0.0981751680374146, 0, 0.573396027088165, 0.528906404972076,
-0.625681936740875, 0, 0, 0, 0, 1), 4, 4)
open3d(userMatrix = uM, windowRect = c(10, 10, 510, 510))
plot3d(pollen[, 1:3])
zm = seq(1, 0.045, length = 200)
par3d(zoom = 1)
for (i in 1:length(zm)) {
par3d(zoom = zm[i])
# remove the comment if you want to save the snapshots
# rgl.snapshot(paste(formatC(i, width = 3, flag = 0), ".png", sep = ""))
Sys.sleep(0.01)
}
}}
