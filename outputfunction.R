library(MuMIn)
library(dplyr)

output=function (x)
{
r<-data.frame(subset(dredge(x), delta<=2))
d<-r[,2:((ncol(r)-5))]
d2<-r[,((ncol(r)-4)):ncol(r)]
d<-replace(data.frame(lapply(d, as.character), stringsAsFactors = FALSE),
           !is.na(d), "Data")
w<-which(d=="Data", arr.ind = TRUE)
d[w]<-names(d)[w[,"col"]]
d$total<-do.call(paste, c(d[1:ncol(d)], sep="+"))

for  (i in 1:nrow(d)){
  d$total<-gsub('NA','',paste(d$total)) #Remove NAs
  d$total<-gsub('([[:punct:]])\\1+', '\\1', paste(d$total)) #Remove duplicated plus signs
  d$total<-sub('[[:punct:]]+$', '', paste(d$total)) #Remove plus at end
  d$total<-sub('^+[[:punct:]]', '', paste(d$total)) #Remove plus at beginning
  d$total<-gsub("\\.", "*", paste(d$total)) #Replace period with asterisk
  
}

table<-tibble(bind_cols(d$total, d2))
names(table)[names(table) == "...1"] <- "Model"
table<-table %>% 
  mutate(weight = sprintf("%0.2f", weight),
         delta = sprintf("%0.2f", delta),
         logLik=sprintf("%0.2f", logLik),
         AICc=sprintf("%0.2f", AICc))
table$name <-deparse(substitute(x))
tablemodel<<-table
return(tablemodel)
}
