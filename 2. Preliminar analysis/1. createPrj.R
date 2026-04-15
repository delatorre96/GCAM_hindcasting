library(rgcam)

pathToDbs <- "C:/Users/ignacio.delatorre/Documents/GCAM/outputs_gcam"
my_gcamdb_basexdb <- "BaseYear"

conn <- localDBConn(pathToDbs, my_gcamdb_basexdb)

myQueryfile  <- "allQueries_2010.xml"

scenariosAnalyze<-c( "Reference","BaseYear2010")

prj1 <- addScenario(conn = conn, proj = 'BaseYear2010.dat', scenario  = scenariosAnalyze, queryFile = myQueryfile)
queries <- listQueries(prj1)
