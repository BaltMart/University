library(tidyverse)
library(tidyr)
library(lubridate)

dt = read.csv("C:\\Users\\martynas\\Desktop\\uni\\Year 2\\DATA\\Lecture 1\\time_series_covid19_confirmed_global_05022023.csv", stringsAsFactors=FALSE)

str(dt) #looking at what r understands data as
#important to fix how dates are written and understood

## Are the time series in the first row increasing or not?
anhui = dt[1,5:745]
anhui = unlist(anhui)
all(diff(anhui) > 0)

## Are all of the time series increasing?
res = rep(NA, nrow(dt))
for(i in 1:nrow(dt)) { 
  anhui = dt[i, 5:745]
  anhui = unlist(anhui)
  res[i] = all(diff(anhui)>=0)
}
res

nm <- colnames(dt)[5:745]
nm1 <- gsub("X", "", nm)
nm2 <- strsplit(nm1, ".", fixed = TRUE)
nm3 <- nm2
for (i in 1:length(nm3)) {
  nm3[[i]][3] <- paste0("20", nm3[[i]][3]) 
}
nm4 <- nm3
for (i in 1:length(nm3)) {
  nm4[[i]] <- paste(nm4[[i]], collapse = ".") 
}
nm5 <- rep("", length(nm4))
for (i in 1:length(nm5)) {
  nm5[i] <- nm4[[i]]
}
tm <- mdy(nm5)
view(tm)


# Min max values on the last observation in the data set
df = dt[,c(1,2,ncol(dt))]

minNum = min(df[,3])
maxNum = max(df[,3])

dt_long= cbind(Country = rownames(dt), dt)
dt_long
