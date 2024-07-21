`%notin%` = Negate(`%in%`)
`%>%` = magrittr::`%>%`

pos_groups = c("QB", "RB", "WR", "TE")
min_year = 2016
max_year = 2023

config = jsonlite::fromJSON("config.json") # get environment variables from JSON configuration file.
# Key idea here is that if you put config.json in your .gitignore, you can access your database password without exposing it anywhere you commit to Git.
db_pw = config$MYSQL_FANTASY_FOOTBALL_PW
Sys.setenv("MYSQL_FANTASY_FOOTBALL_PW" = db_pw)
