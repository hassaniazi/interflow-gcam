# load GCAM-USA scenarios and query results from Constance
#
# Hassan Niazi, July 2024

source("./R/0_functions.R")

df_im3scen_water <- query_im3_scen("water")

# GCAM-USA databases list
db_loc <- "/pic/projects/im3/gcamusa/gcam-usa-im3/output"
dbFile_names <- c("database_rcp45cooler_ssp3",
                  "database_rcp45cooler_ssp5",
                  "database_rcp45hotter_ssp3",
                  "database_rcp45hotter_ssp5",
                  "database_rcp85cooler_ssp3",
                  "database_rcp85cooler_ssp5",
                  "database_rcp85hotter_ssp3",
                  "database_rcp85hotter_ssp5")

# # attach dbLoc to all dbFile_names (not needed)
# db_paths <- paste0(db_loc, "/", dbFile_names, "/")

query.fname <- 'data/queries_io.xml'
prj.name.remote <- 'im3scen_io.dat'

# ssh niaz981@constance03
# ./basex-server-helper.sh /pic/projects/im3/gcamusa/gcam-usa-im3/output
# constance_node  <- paste("constance0", readline(prompt = "Current constance node: "), sep = "")
constance_node <- "constance03"

# loop over to query all databases
for (dbFile in dbFile_names){

  ########################%
  # load project data from constance ----
  # list scenarios
  scenario.names_remote <- (listScenariosInDB(remoteDBConn(dbFile, "test", "test", constance_node)))$name

  # open connection and add scenario and query to project
  conn_remote <- remoteDBConn(dbFile, "test", "test", constance_node)
  # prj_im3scen_remote <- addScenario(conn_remote, prj.name.remote, scenario.names_remote, queryFile = query.fname)
  addScenario(conn_remote, prj.name.remote, scenario.names_remote, queryFile = query.fname)
}

########################%
# load already extracted project ----
im3scen_io <- loadProject(proj = prj.name.remote)
listScenarios(im3scen_io)
listQueries(im3scen_io)

# query outputs
df_im3scen_io_ibt <- getQuery(im3scen_io, "inputs by tech")

rcp45cooler_ssp3_inputs_by_tech <- im3scen_io[["rcp45cooler_ssp3"]][["inputs by tech"]]
