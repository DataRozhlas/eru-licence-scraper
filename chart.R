library(ggplot2)

licence_udelene_solarni <- licence_udelene_solarni %>%
  mutate(typ=ifelse(vykon>0.010, "velké", "malé"))

grafdata <- licence_udelene_solarni %>%
  arrange(datum) %>%
  select(datum, vykon, typ) %>%
  group_by(datum, typ) %>%
  summarise(vykon_celkem=sum(vykon))

grafdata <- grafdata %>%
  group_by(typ) %>%
  mutate(cumsum=cumsum(vykon_celkem))

g <- ggplot(grafdata, aes(x=datum, y=cumsum, colour=typ))
g <- g + geom_line()

g
