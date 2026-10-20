feed_url <- "https://broomecountyny.gov/sites/default/files/dept/transit/BCT_GTFS.zip"
download.file(feed_url, mode="wb", destfile="data/raw/bct_gtfs.zip")

