library(readr)
library(stringr)
library(dplyr)

pozemky <- read_csv("~/Downloads/pozemky_body.csv")
provozovny <- read_csv("provozovny_solarni.csv")


# vezmi jen první číslo parcely
parcely <- str_split(provozovny$vymezeni, ";|,")


parcely <- sapply(parcely, function(x) {
  return(x[[1]])
})

# detekuj stavební parcely
parcely_stavebni <- str_detect(parcely, "([S-Ts-t])\\w+")

parcely <- data.frame(parcely, parcely_stavebni)

# vyčisti parcely

parcely$parcely_ciste <- str_extract(parcely$parcely, "\\d+\\/*\\d*")

# rozděl číslo před lomítkem a za lomítkem

parcely$predlom <- sapply(str_split(parcely$parcely_ciste, "/"), function(x){
 return(as.numeric(x[[1]])) 
})

parcely$zalom <- sapply(str_split(parcely$parcely_ciste, "/"), function(x){
  if (length(x)>1) {return(as.numeric(x[[2]]))} else {return(NA)}
})


# spoj data

provozovny$stavebni <- parcely$parcely_stavebni 
provozovny$predlom <- parcely$predlom
provozovny$zalom <- parcely$zalom

provozovny$stavebni[is.na(provozovny$stavebni)] <- FALSE



# geokóduj

X <- numeric()
Y <- numeric()

counter <- 0

for (i in 1:nrow(provozovny)) {
  kod_katastru <- as.numeric(provozovny[i,5])
  stavebni <- as.logical(provozovny[i,9])
  predlom <- as.numeric(provozovny[i,10])
  zalom <- as.numeric(provozovny[i,11])
  vybrane <- pozemky %>% filter(KatastralniUzemiKod==kod_katastru)
  if (stavebni==T) {vybrane <- vybrane %>% filter(DruhCislovaniKod==1)} else {vybrane <- vybrane %>% filter(DruhCislovaniKod==2)}
  vybrane <- vybrane %>% filter(KmenoveCislo==predlom)
  if (!is.na(zalom)) {vybrane <- vybrane %>% filter(PododdeleniCisla==zalom)}
  X <- c(X, vybrane$X[1])
  Y <- c(Y, vybrane$Y[1])
  counter <- counter + 1
  print(c(counter))
}

provozovny$longtitude <- X 
provozovny$latitude <- Y

write_csv(provozovny, "provozovny_solarni_geo.csv")
