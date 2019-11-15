library(aws.s3) # https://github.com/cloudyr/aws.s3
library(stringr)
library(dplyr)
library(rvest)

licence$id <- str_extract(licence$urls, "\\d{7,}")

# zjisti, zda html obsahuje "Sluneční"

counter <- 0
solar <- logical()

for (i in licence$id) {
  raw <- get_object(paste0(i, ".html"), "eru-licence-scraper")
  html <- rawToChar(raw)
  solar <- c(solar, str_detect(html, '<th align="left">Sluneční</th>'))
  print(counter)
  counter <- counter + 1
}

licence$solar <- solar

# jen udělené solární

licence_udelene_solarni <- licence %>%
  filter(stav=="Udělená licence") %>%
  filter(solar==TRUE)

# zjisti datum a výkon

counter <- 0
datum <- numeric()
class(datum) <- "Date"
vykon <- numeric()

for (i in licence_udelene_solarni$id) {
  raw <- get_object(paste0(i, ".html"), "eru-licence-scraper")
  html <- rawToChar(raw)
  vysek <- str_sub(html, str_locate(html, "Datum zahájení výkonu licencované činnosti")[2], str_locate(html, "Datum zahájení výkonu licencované činnosti")[2]+50 )
  datum <- c(datum, as.Date(str_extract(vysek, "\\d{2}\\.\\d{2}\\.\\d{4}"), "%d.%m.%Y"))
  vysek <- str_sub(html, str_locate(html, '<th align="left">Sluneční</th>')[2], str_locate(html, '<th align="left">Sluneční</th>')[2]+40 )
  vykon <- c(vykon, as.numeric(str_extract(vysek, "\\d+\\.\\d{3}")))
  print(counter)
  counter <- counter + 1
}

licence_udelene_solarni$datum <- datum

licence_udelene_solarni$vykon <- vykon


licence_udelene_solarni <- licence_udelene_solarni %>%
  select(id, urls, datum, vykon)

write.csv(licence_udelene_solarni, "licence_solarni.csv")

