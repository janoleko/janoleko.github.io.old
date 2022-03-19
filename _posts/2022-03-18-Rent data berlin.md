---
layout: post
title: "Webscraping Immowelt data"
subtitle: "And creating an interactive 3D-Plot with the plotly R package"
background: '/img/posts/Rent/webscraping_bg.jpg'
---
# Webscraping data from Immowelt

![Immowelt page](/img/posts/Rent/immowelt-bg.png)

## Afterwards fitting a regression model and visualising results with the plotly R package

### Scraping

Getting all the links for overview pages of Immowelt flats in Berlin:

```{r}
library(rvest)
library(RSelenium)
library(tidyverse)
library(netstat)


rD <- rsDriver(browser=c("firefox"), free_port())
remDr <- rD$client

remDr$navigate("https://www.immowelt.de/")

links_next2 = c("https://www.immowelt.de/liste/berlin/wohnungen/mieten?sort=relevanz", paste0("https://www.immowelt.de/liste/berlin/wohnungen/mieten?d=true&sd=DESC&sf=RELEVANCE&sp=",2:20))

all_links2 <-c()

for (i in 1:20){
  remDr$navigate(links_next2[i])
  
  webElem <- remDr$findElement("css", "body")
  webElem$sendKeysToElement(list(key = "end")) # runterscrollen
  
  links <- links_next2[i] %>% read_html() %>%
    html_nodes("a") %>%
    html_attr("href") # nur die Links von der einzelnen Seite scrapen
  
  links <- links %>%
    str_subset(pattern = "/expose/") %>% # nur die Exposes nehmen
    unique() # Duplikate löschen
  
  all_links2 <- c(all_links2, links)
}

```

Scraping the flat data from the individual pages:

```{r}
flats_berlin = tribble(~kaltmiete, ~flaeche, ~zimmer)

for (i in 1:length(all_links2)){
  html = read_html(all_links2[i])
  
  kaltmiete = html %>% 
    html_elements("div.has-font-300 > strong:nth-child(1)") %>% 
    html_text(trim=TRUE) %>% 
    str_replace("\\.","") %>% 
    str_replace(",",".") %>% 
    parse_number
  
  flaeche = html %>% 
    html_elements("div.flex:nth-child(2) > div:nth-child(1) > span:nth-child(1)") %>% 
    html_text(trim=TRUE) %>%
    str_replace(",",".") %>% 
    parse_number()
  
  zimmer = html %>% 
    html_elements("div.hardfact:nth-child(2) > span:nth-child(1)") %>% 
    html_text(trim=TRUE) %>%
    str_replace(",",".") %>% 
    parse_number
  
  flats_berlin = add_row(flats_berlin, kaltmiete, flaeche, zimmer)
}
```

```{r}
flats_berlin = flats_berlin[-which.max(flats_berlin$kaltmiete),]

flats_berlin = drop_na(flats_berlin)
```

Fitting a linear regression model:
```{r}
mod = lm(flats_berlin$kaltmiete~flats_berlin$flaeche + flats_berlin$zimmer + I(flats_berlin$flaeche^2) +
           I(flats_berlin$flaeche*flats_berlin$zimmer))
```

Creating the plotly 3D-Plot:
```{r}
cf.mod = coef(mod)

x.seq = seq(min(flats_berlin$flaeche),max(flats_berlin$flaeche), length.out=100)
y.seq = seq(min(flats_berlin$zimmer),max(flats_berlin$zimmer), length.out=100)

z = t(outer(x.seq,y.seq, function(x,y) cf.mod[1] + cf.mod[2]*x + cf.mod[3]*y + cf.mod[4]*x^2 + cf.mod[5]*x*y))


rentplot = plot_ly(x=x.seq, y=y.seq, z=z,
                type = "surface", colors = c("#326ba8","#32a87d"), showscale = FALSE, showlegend = FALSE) %>% 
  add_trace(data=flats_berlin, x=flats_berlin$flaeche, y=flats_berlin$zimmer, z=flats_berlin$kaltmiete, 
            mode="markers", type="scatter3d",
            marker = list(color="#000000", opacity=0.9, size=4)) %>% 
  layout(scene = list(xaxis=list(title="Area"),
                      yaxis=list(title="Rooms"),
                      zaxis=list(title="Rent")))
rentplot
```
<iframe src="/img/posts/Rent/Rentplot1.html" height="550px" width="100%" frameBorder="0"></iframe>

