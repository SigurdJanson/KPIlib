library(jsonlite)
source("../utils.R")
source("../tagstr.R")

#
# READ DATA
cat("READ DATA")
x <- readKpiData(c("../www/kpis.json", "../www/kpis_digitalproducts.json"))
x <- filterKpiData(x)
#x <- head(x, 40L)
domains <- parseDomains(x$domain)

# vector of logicals that indicate per domain for which KPI the domain is set.
cat("LOCATE DOMAINS")
domloc <- sapply(x$domain, \(y) {domains %isin% y})
colnames(domloc) <- x$title
rownames(domloc) <- domains


cat("# DOMAIN DISTANCES")
domdist <- dist(domloc, method="binary") |>
  as.matrix()
# Remove outliers with extremely high distance to everything except self
dismissThis <- apply(as.matrix(domdist), 1L, \(x) all(x > 0.9 | x < 0.00000001)) |>
  which()
domdist <- domdist[-dismissThis,-dismissThis]


cat("CLUSTER")
mydata.hclust = kmeans(domdist, nrow(domdist) %/% 3L, nstart = nrow(domdist) %/% 3L)
#plot(mydata.hclust, labels=mydata$Company,main='Domains from hclust')
pdf("hclust_domdist.pdf")
plot(domdist, col=mydata.hclust$cluster, main='Domains from hclust')
dev.off() 


#any(apply(as.matrix(domdist), 1L, \(x) all(x > 0.8 | x < 0.00000001)))
#> 


