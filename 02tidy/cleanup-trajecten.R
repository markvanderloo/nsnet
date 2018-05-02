
dat <- read.csv("01raw/20180502_trajecten.csv",stringsAsFactors = FALSE)
# deduplicate the edges
out <- setNames(as.data.frame(t(apply(as.matrix(dat[1:2]),1,sort))),c("start","stop"))
out <- unique(out)
write.csv(out, file="02tidy/trajecten.csv",row.names=FALSE)


