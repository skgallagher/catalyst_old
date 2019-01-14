library(testthat)

context("put hagelloch households into a grid")


testthat("main fxn", {


    library(surveillance)
    data(hagelloch)
    coords <- hagelloch.df[, c("HN", "x.loc", "y.loc")]
    devtools::load_all("~/catalyst/catalyst")
    rad <- 3
    out <- hh_to_grid(coords, radius = rad)
    plot(out$x, out$y, pch = ".")
    centers <- coords[!duplicated(coords), ]
    bd <- rad + 1.5
    centers$xleft <- 0
    centers$xright <- 0
    centers$ytop <- 0
    centers$ybottom <- 0
    centers$xleft <- centers$x.loc - bd
    centers$xright <- centers$x.loc + bd
    centers$ybottom <- centers$y.loc - bd
    centers$ytop <- centers$y.loc + bd
    
    with(centers, rect(xleft, ybottom, xright, ytop))



    houses <- ggplot() + geom_rect(data = centers, aes(xmin = xleft,
                         xmax = xright, ymin = ybottom,
                         ymax = ytop),
                         fill = NA,
                         col = "black") + 
        geom_point(data = out,
                          aes(x = x.loc, y = y.loc))

})

         
