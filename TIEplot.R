TIEplot <- function(x, y, error, bodEx = 1.3, wingEx = 0.8, ylim = c(minLim, maxLim), names = NULL, ...){
	arrowsVar <- function(x0, y0, y1, length, ...){
		for(i in 1:length(x0)){
			arrows(x0 = x0[i], y0 = y0[i], y1 = y1[i], length = length[i], ...)
		}
	}
	maxLim <- 1.1 * max(mapply(sum, y, error))
	minLim <- 0.9 * min(mapply(sum, y, - error))

	plot(x, y, type="n", ylim = ylim, xaxt = "n", ...)
	arrowsVar(x0 = x, y0 = y, y1 = y + 1.96 * error, angle = 90, code = 2, length = wingEx * error, col = "grey75", lwd = 40 * wingEx * error)
	arrowsVar(x0 = x, y0 = y, y1 = y - 1.96 * error, angle = 90, code = 2, length = wingEx * error, col = "grey75", lwd = 40 * wingEx * error)
	points(x, y, pch = 21, col = "grey75", bg = "grey75", cex = 20 * bodEx * error)
	points(x, y, pch = 21, col = "grey25", bg = "grey25", cex = 10 * bodEx * error)
	points(x, y, pch = -0x2055, col = "grey75", cex = 10 * bodEx * error)
	points(x, y, pch = 21, col = "grey25", bg = "grey25", cex = 3 * bodEx * error)
	if(!is.null(names)) axis(1, at = x, labels = names) else axis(1)
}
