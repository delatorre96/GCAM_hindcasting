library(rgcam)

pathToDbs <- "C:/GCAM/Nacho/outputs_gcam"
my_gcamdb_basexdb <- "hindcasting"

conn <- localDBConn(pathToDbs, my_gcamdb_basexdb)

myQueryfile  <- "query_outputsByTech_2015.xml"

scenariosAnalyze<-c('BaseYear2015_policy_test_noEEAtradetax', 'Reference')

prj1 <- addScenario(conn = conn, proj = 'BaseYear2015_outputsByTech.dat', scenario  = scenariosAnalyze, queryFile = myQueryfile)
queries <- listQueries(prj1)



