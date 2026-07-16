library(rgcam)

pathToDbs <- "C:/GCAM/Nacho/outputs_gcam"
my_gcamdb_basexdb <- "hindcasting"

conn <- localDBConn(pathToDbs, my_gcamdb_basexdb)

myQueryfile  <- "allQueries_2015.xml"

scenariosAnalyze<-c('BaseYear2015_shwt', 'Reference')

prj1 <- addScenario(conn = conn, proj = 'BaseYear2015.dat', scenario  = scenariosAnalyze, queryFile = myQueryfile)
queries <- listQueries(prj1)



