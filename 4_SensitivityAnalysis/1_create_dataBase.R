library(DBI)
library(RSQLite)

create_database <- function(db_path = "gcam_sensitivity.sqlite") {

con <- dbConnect(
  SQLite(),
  "gcam_sensitivity.sqlite"
)


dbExecute(con, "
CREATE TABLE IF NOT EXISTS Experiments (
    experiment_id INTEGER PRIMARY KEY AUTOINCREMENT,
    project TEXT ,
    gcam_version TEXT NOT NULL,
    name TEXT NOT NULL,
    description TEXT,
    created_at TEXT,
    perturbation_strategy TEXT,
    distribution TEXT,
    distribution_parameters TEXT
);
")


dbExecute(con, "
CREATE TABLE IF NOT EXISTS Runs (
    run_id INTEGER PRIMARY KEY AUTOINCREMENT,
    experiment_id INTEGER NOT NULL,
    timestamp TEXT,
    execution_time REAL,
    execution_errors TEXT,
    delta REAL,
    xml_files TEXT,

    FOREIGN KEY (experiment_id)
        REFERENCES Experiments(experiment_id)
);
")


dbExecute(con, "
CREATE TABLE IF NOT EXISTS SampledParameters (
    run_id INTEGER NOT NULL,
    xml_file TEXT,
    region TEXT,
    supplysector TEXT,
    subsector TEXT,
    nesting_subsector TEXT,
    year INTEGER,
    logit REAL,

    FOREIGN KEY (run_id)
        REFERENCES Runs(run_id)
);
")



dbExecute(con, "
CREATE TABLE IF NOT EXISTS Queries (
    query_id INTEGER PRIMARY KEY AUTOINCREMENT,
    name TEXT NOT NULL,
    original_columns TEXT
);
")

dbExecute(con, "
CREATE TABLE IF NOT EXISTS Outputs (
    run_id INTEGER NOT NULL,
    query_id INTEGER NOT NULL,
    filepath TEXT NOT NULL,
    output_columns TEXT,
    nrow INTEGER,

    PRIMARY KEY (run_id, query_id),

    FOREIGN KEY (run_id)
        REFERENCES Runs(run_id),

    FOREIGN KEY (query_id)
        REFERENCES Queries(query_id)
);
")
return(con)
}
