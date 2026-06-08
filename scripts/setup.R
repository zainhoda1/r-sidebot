library(duckdb)
library(DBI)
library(here)

db_path <- here("dragons.duckdb")

# Delete if exists
if (file.exists(db_path)) {
  unlink(db_path)
}

# Load dragons.csv into a table named `dragon`
conn <- dbConnect(duckdb(), dbdir = db_path)
duckdb_read_csv(conn, "dragon", here("data", "dragons.csv"))
dbDisconnect(conn)
