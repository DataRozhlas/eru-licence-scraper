library(aws.s3) # https://github.com/cloudyr/aws.s3
library(rvest)
library(stringr)


url <- "http://licence.eru.cz/index.php?SelAdProcStatusId=%22Ud%C4%9Blen%C3%A1+licence%22"

i <- seq(0, 35520, 30)

# stáhni všechny URL na detaily licence

urls <- character()

for (i in i) {
  stranka_vypisu <- read_html(paste0("http://licence.eru.cz/index.php?roff=", i))
  result <- stranka_vypisu %>%
    html_nodes(".indexOddRow a, .indexEvenRow a") %>%
    html_attr("href")
  urls <- c(urls, result)  
  print(i)
}

# zjisti stav (udělená, ve správním řízení, zamítnutá žádost, zaniklá, zastavené správní řízení, zrušená)

stav <- character()
i <- seq(0, 35520, 30)


for (i in i) {
  stranka_vypisu <- read_html(paste0("http://licence.eru.cz/index.php?roff=", i))
  result <- stranka_vypisu %>%
    html_nodes(".indexOddRow td:nth-child(6), .indexEvenRow td:nth-child(6)") %>%
    html_text()
  stav <- c(stav, result)  
  print(i)
}

licence <- data.frame(stav, urls)

bucketlist()

# stáhni HTML každé licence a nahraj ho na S3

counter <- 0 

for (j in licence$urls) {
  filename <- paste0(str_extract(j, "\\d{7,}"), ".html")
  download.file(url=paste0("http://licence.eru.cz/", substr(j, 2, 1000000L)), destfile=filename, quiet=T)
  put_object(filename, filename, "eru-licence-scraper")
  file.remove(filename)
  counter <- counter + 1
  print(counter)
}