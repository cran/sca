## running in R or S+ ? -- the following even works in S(not plus):
if(!(Ex <- exists("version")) || is.null(vl <- version$language) || vl != "R")
{ ## we are not in R :
    is.R <- function () FALSE
    if(!Ex || version$major < 6)
        "%in%" <- function(x, table) !is.na(match(x, table, nomatch = NA))

    dev.interactive <- function () {
	interactive() && exists(".Device") &&
	.Device %in%
        c("motif", "graphsheet", "java.graph", "X11", "openlook")
    }

    which.min <- function(x) sort.list(x)[1]
}
if(Ex) rm(vl)
rm(Ex)
