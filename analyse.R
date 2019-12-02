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
  counter <- counter + 1
  print(counter)
}

licence_udelene_solarni$datum <- datum

licence_udelene_solarni$vykon <- vykon


licence_udelene_solarni <- licence_udelene_solarni %>%
  select(id, urls, datum, vykon)

write.csv(licence_udelene_solarni, "licence_solarni.csv")

### stáhni všechny solární pobočky

detekujSlunecni <- function(zaznam) {
  str_detect(html_text(zaznam), "Sluneční")
}

id_licence <- numeric()
evidencni_cislo <- numeric()
nazev <- character()
katastralni_uzemi <- character()
kod_katastru <- numeric()
obec <- character()
vymezeni <- character()
vykon <- numeric()

counter <- 0

for (i in licence_udelene_solarni$id) {
  raw <- get_object(paste0(i, ".html"), "eru-licence-scraper")
  html <- read_html(rawToChar(raw))
  nazvy <- html %>%
    html_nodes(".lic-tez-header-table")
  
  vykony <- html %>%
    html_nodes(".lic-tez-data-table")
  
  nazvy <- nazvy[sapply(vykony, detekujSlunecni)]
  vykony <- vykony[sapply(vykony, detekujSlunecni)]       
  
  counter <- counter + 1
  
  print(counter)
    
  for (j in 1:length(vykony)) {
    id_licence <- c(id_licence, i)
    evidencni_cislo <- c(evidencni_cislo, html_table(nazvy[[j]])[1,1] %>% str_extract(., "\\Evidenční číslo: \\d+") %>% str_sub(., 18) %>%as.numeric())
    nazev <- c(nazev, html_table(nazvy[[j]])[1,1] %>% str_sub(., 21) %>% str_squish(.))
    katastralni_uzemi <- c(katastralni_uzemi, html_table(nazvy[[j]])[3,1])
    kod_katastru <- c(kod_katastru, html_table(nazvy[[j]])[3,2])
    obec <- c(obec, html_table(nazvy[[j]])[3,3])
    vymezeni <- c(vymezeni, html_table(nazvy[[j]])[3,4])
    vysek <- str_sub(html_text(vykony[[j]]), str_locate(html_text(vykony[[j]]), "Sluneční")[2], str_locate(html_text(vykony[[1]]), "Sluneční")[2]+12)
    vykon <- c(vykon, as.numeric(str_extract(vysek, "\\d+\\.\\d{3}")))

    }
  
}

provozovny_solarni <- data.frame(id_licence, evidencni_cislo, nazev, katastralni_uzemi, kod_katastru, obec, vymezeni, vykon)

write.csv(provozovny_solarni, "provozovny_solarni.csv", row.names = F)       

