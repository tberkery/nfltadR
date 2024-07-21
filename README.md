# nfltadR

## Overview
A simple package for generating fantasy football projections using machine learning. The name is a play on `nflfastR`, a highly utilized package by many armchair analysts for accessing NFL data. This R package offers many useful functions for conditioning data from `nflreadR` and `nflfastR` and writes the condition data to a local MySQL (relational) database. It then trains and offers a series of optimized, cross-validated XGBoost models and builds a `draft_board` table in the MySQL database to store the fantasy-point projections for the upcoming season. All of this can be done by simply calling the `get_data(min_year, max_year)` function exposed by the package to acquire and condition data and write the result to the database and then calling `run_projections` to query this data from the database and build XGBoost models for each scoring system (standard and points-per-reception) and classic position (quarterback, running back, wide receiver, and tight end). Projections from these models for the upcoming models are what is written to the draft board table in the database. The result is a validated, data-driven full projection and draft suite achievable at your fingertips with just two function calls.

## Installation

To install this package, open an RSession (and make sure you have `devtools` installed). Make sure your working directory is the root of this project and run `devtools::install("nfltadR")`. From this point onwards, you should have access to the package through traditional R measures (e.g. `library(nfltadR)`). Note that there is `renv.lock` file in the project repository that provides information on all project dependencies. Run `renv::init()` to initialize a `renv` environment for your proejct and then use `renv::restore()` to install all necessary packages.

Note that this package was developed using R4.2. The package should work with R4.3 as well, but any usage of this package with a version other than R4.2 runs the risk of unforeseen issues. I recommend using R4.2.

## Configuration

If you don't have `mysql` installed locally, you will need to install it. If you are a Mac user, I recommend executing `brew install mysql`. Then start a MySQL server using `brew services start mysql`. Configure the root user via `mysql -u root -p`. Then configure a new user, which this package will interact with your database through, via `mysql -u [username] -p` (where `[username]` is replace with your username, no square brackets included). This will involve setting a password. To ensure this password is accessible to the package without hard-coding it in code in an insecure manner, execute `SYS.setenv(MYSQL_PASSWORD = "[password]")` in your RSession (where `[password]` is replaced with the password you set for the user). This will allow the package to access the password via `Sys.setenv("MYSQL_FANTASY_FOOTBALL_PW" = [your_password])`, where `[your_password]` is replaced with your password (again excluding the square brackets). You will need to do this every time you start a new R session. An alternative is to make `MYSQL_FANTASY_FOOTBALL_PW` a system variable (e.g. add it to your `.zshrc` file or other file where appliable depending on your operating system). Make sure to source this file after the change and start a new R session after sourcing to ensure any changes take effect if you opt to go this route.

Once you have your user configured, create a database titled `fantasy_football` which you will write to using this package. This will contain the draft board and data that the package curates after its key functions are run.

Note that interaction with the database from a Mac typically requires a configuration file. I like using the `.my.cnf` file in this regard. Create a `.my.cnf` file in the root of your computer (not the root of the project directory) if you don't already have one. It should look something like this:

```{}
[client]
user = [your username from earlier]
password = [your password from earlier]
host = localhost
local_infile = 1

[mysql]
local_infile = 1

[mysqld]
local_infile = 1
```

Note that `localhost` implies that the database is hosted locally (rather than, e.g., in the cloud). Also note the persistent inclusion of `local_infile = 1`... this is pivotal for ensuring that you can actually write to the database programatically from the code in the package. If you don't have this, you will run into issues.

## Restrictions

* Note that `nflfastR` data only goes back to 2006. Accordingly, 2006 is the earliest `min_year` value you can select and still have working functions.
