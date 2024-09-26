# Update settings ---------------------------------------------------------------------------------------------------------------------
setwd("V:\\Glencora\\GitHub\\metro-resource-bank")
Sys.setenv(root = getwd(), overwrite = TRUE, save = TRUE)

#######################################################################################################################################

monitor.packages <- function() {
  packages <- c('githubinstall', 'dplyr', 'rvest', 'tidyverse', 'tidycensus', 'stringr', 'writexl', 'openxlsx', 
                'readxl', 'haven', 'stringi', 'reshape', 'reshape2', 'censusapi', 'httr', 'bea.R', 'tigris', 
                'purrr', 'sf', 'data.table', 'jsonlite', 'readr', 'janitor', 'devtools', 'blsAPI', 'RSelenium', 
                'XML', 'osmextract', 'curl', 'parallel', 'R.utils', 'ipumsr', 'zoo', 'xml2', 'archive', 'hms',
                'lubridate', 'leaflet', 'htmlwidgets', 'leaflet.extras', 'osrm', 'parallel', 'raster', 'terra')
  installed_packages <- packages %in% rownames(installed.packages())
  if (any(installed_packages == FALSE)) {
    install.packages(packages[!installed_packages], ask = FALSE, update = FALSE)
  }
  installed_packages <- packages %in% rownames(installed.packages())
  if (any(installed_packages == FALSE)) {
    githubinstall(packages[!installed_packages], ask = FALSE)
  }
  invisible(lapply(packages, library, character.only = TRUE))
  rm(installed_packages)
  rm(packages)
}
monitor.packages()
rm(monitor.packages)
gc()

options(tigris_use_cache = TRUE)
options(scipen = 999)

Sys.setenv(censusKey = "0ccc9ebd9112a7ca28c9d900f8aac70dedc4ffbc", overwrite = TRUE, save = TRUE)
Sys.setenv(beaKey = "D0DE604A-D593-4D7A-B575-D915BAF7A444", overwrite = TRUE, save = TRUE)
Sys.setenv(hudKey = "eyJ0eXAiOiJKV1QiLCJhbGciOiJSUzI1NiIsImp0aSI6ImRlYmM4YTE4ZDkxNTBhZWFjYTgwMDc2OGMzY2FjNjQyYzJjZjQ1NWVkNzNjODIzYjE5ZjRjMjM2ODRmYjFlODNlY2I0MDA2ZjgzMmQzYmFiIn0.eyJhdWQiOiI2IiwianRpIjoiZGViYzhhMThkOTE1MGFlYWNhODAwNzY4YzNjYWM2NDJjMmNmNDU1ZWQ3M2M4MjNiMTlmNGMyMzY4NGZiMWU4M2VjYjQwMDZmODMyZDNiYWIiLCJpYXQiOjE2ODkzNTg5MzAsIm5iZiI6MTY4OTM1ODkzMCwiZXhwIjoyMDA0OTc4MTMwLCJzdWIiOiIzNjA1MSIsInNjb3BlcyI6W119.BKP2wmohTmFgL8NCY3H0TbadcK3Il_kfd0w_FY62kZYrJZi1GAgi8PHOCAwgELi0n0g-4VmvJZsrURg9ffwubQ", overwrite = TRUE, save = TRUE)
Sys.setenv(blsKey = "29870c4aac514c528a87e507f6e45f94", overwrite = TRUE, save = TRUE)
Sys.setenv(ipumsKey = "59cba10d8a5da536fc06b59d2495d8ef219247d6a3811e761dca58c7", overwrite = TRUE, save = TRUE)
Sys.setenv(fccKey = "hjpdt0zb2Yz3w7KbICr1DAZYak6PH3FtFo9F6/0PfVY=", overwrite = TRUE, save = TRUE)
Sys.setenv(UA = "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/126.0.0.0 Safari/537.36", overwrite = TRUE, save = TRUE)

##########################################################################################

dir <- str_replace_all(paste0(Sys.getenv("root"), "/datasets/crosswalks"), "/", "\\\\")
suppressWarnings(dir.create(file.path(dir), recursive = TRUE))
setwd(dir)

wait_for_element <- function(remote_driver, element_id, timeout = 60) {
  start_time <- Sys.time()
  repeat {
    tryCatch({
      element <- remDr$findElement(using = "css selector", value = element_id)
      if (!is.null(element)) {
        message("Element found!")
        break
      }
    }, error = function(e) {
      message("Element not found yet, retrying...")
    })
    if (difftime(Sys.time(), start_time, units = "secs") > timeout) {
      stop("Timeout reached, element not found.")
    } 
    Sys.sleep(1)}}

eCaps <- list(
  browserName = "firefox",
  "moz:firefoxOptions" = list(
    prefs = list('browser.download.dir' = dir,
                 'browser.download.folderList' = 2,
                 'browser.helperApps.neverAsk.saveToDisk' = "application/octet-stream,application/pdf,application/vnd.ms-excel")
  )
)
driver <- rsDriver(browser="firefox", port = 4567L, chromever = NULL, verbose=F, extraCapabilities = eCaps)
remDr <- driver[["client"]]
remDr$open()

remDr$navigate("https://mcdc.missouri.edu/applications/geocorr2022.html")
remDr$maxWindowSize()
Sys.sleep(1)

#Unselect Missouri from list (selected by default) and select Connecticut
xpath <- c('//*[@value="Mo29"]', '//*[@value="Ct09"]')
for (i in 1:length(xpath)) {
  element <- remDr$findElement(using = "xpath", xpath[[i]])
  remDr$executeScript("arguments[0].scrollIntoView(true);", list(element))
  element$clickElement()
  Sys.sleep(0.5)
}

#Select "counties" as source geography and "planning regions" as target geography
css <- c(
  '#inputOptions > div:nth-child(4) > p > select > optgroup:nth-child(3) > option:nth-child(1)',
  '#inputOptions > div:nth-child(5) > p > select > optgroup:nth-child(7) > option:nth-child(2)'
)

for (i in 1:length(css)) {
  element <- remDr$findElement(using = "css selector", css[[i]])
  remDr$executeScript("arguments[0].scrollIntoView(true);", list(element))
  element$clickElement()
  Sys.sleep(0.5)
}

#Submit request and download file 
submit <- c(
  '#body > div > form > div:nth-child(10) > input[type=submit]:nth-child(1)',
  'body > p:nth-child(11) > b > a'
)

for (i in 1:length(submit)) {
  element <- remDr$findElement(using = "css selector", submit[[i]])
  remDr$executeScript("arguments[0].scrollIntoView(true);", list(element))
  element$clickElement()
  Sys.sleep(0.5)
}

#End session
Sys.sleep(2)
remDr$close()
driver$server$stop()
suppressWarnings(rm(eCaps, css, xpath, submit, wait_for_element, remDr, driver, element, i))

#Rename file for preservation and remove geocorrelation file 
setwd(dir)
geocorr <- read.csv(list.files(pattern = "geocorr*")) %>% row_to_names(1) %>% clean_names()
write.csv(geocorr, "counties2ctregions.csv", row.names = FALSE, na = "")
file.remove(list.files(pattern = "geocorr*"))


