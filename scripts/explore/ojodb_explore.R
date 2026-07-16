# === OJODB Package Exploration, developed by Open Justice Oklahoma ===

# Purpose and Goals ----

# The goal of Open Justice Oklahoma is to collect and analyze hard-to-access data in order to better understand our state’s justice system. The backbone of our work is our database, which consists of administrative data, produced in court’s daily activities, gathered mainly from courts, jails, and prisons across the state. The data is collected through a variety of methods including webscraping and database file downloads. The ojodb package was built to give analysts a way to access this data and analyze it using shared methodological standards.
# 
# Because the data we analyze is mostly administrative data generated for case-by-case uses, it is always messy and contains errors. OJO’s work depends on our processes to work through and around the imperfections in order to extract useful information, while acknowledging the limitations of the data.
# 
# For some data sources, OJO processes periodically pull new data into our database. For example, we have OSCN scrapers set up to periodically visit small claims case pages like this one every few days, gathering new data that appears in the course of a case.

# Git repository: https://github.com/openjusticeok/ojodb

library(pacman)

p_load(tidyverse, ojodb)

ojodb::vignette("vignette-pulling-data")
ojodb::ojo_query()