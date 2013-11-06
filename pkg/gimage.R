w <- gwindow("hello")
sb <- gstatusbar("Powered by gWidgetsWWW and Rook", cont=w)
g <- ggroup(cont=w, horizontal=FALSE)

f <- get_tempfile(ext=".png")
png(f)
hist(rnorm(100))
dev.off()

i <- gimage(f, container=g)
size(i) <- c(400, 400)
