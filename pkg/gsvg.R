w <- gwindow("hello")
sb <- gstatusbar("Powered by gWidgetsWWW and Rook", cont=w)
g <- ggroup(cont=w, horizontal=FALSE)
require(RSVGTipsDevice)

f <- get_tempfile(ext=".svg") ## use this extension
svg(f)
hist(rnorm(100))
dev.off()

#glabel("test", container = g)

i <- gsvg(f, container=g, width=480, height=480)

b <- gbutton("click", cont=g, handler=function(h,...) {
  f <- get_tempfile(ext=".svg")
  svg(f)
  hist(rnorm(100))
  dev.off()
  svalue(i) <- f
})
