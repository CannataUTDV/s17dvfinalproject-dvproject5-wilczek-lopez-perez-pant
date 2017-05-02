require(readr)
require(plyr)

# Set the Working Directory to the 00 Doc folder
file_path = "../01Data/zipLatLong.csv"
df <- readr::read_csv(file_path)
print(head(df))

measures <- c()

dimensions <- setdiff(names(df), measures)

# Get rid of special characters in each column.
for(n in names(df)) {
  df[n] <- data.frame(lapply(df[n], gsub, pattern="[^ -~]",replacement= ""))
}


na2emptyString <- function (x) {
  x[is.na(x)] <- ""
  return(x)
}

if( length(dimensions) > 0) {
  for(d in dimensions) {
    # Get rid of " and ' in dimensions.
    df[d] <- data.frame(lapply(df[d], gsub, pattern="[\"']",replacement= ""))
    # Change & to and in dimensions.
    
    #put spaces between lowercase and upercase eg. NorthAmerica -> North America
    df[d] <- data.frame(lapply(df[d], gsub, pattern="([a-z])([A-Z])",replacement= "\\1 \\2"))
    
    #Korea,Republicof -> Republic of Korea
    df[d] <- data.frame(lapply(df[d], gsub, pattern="Korea,Republicof",replacement= "Republic of Korea"))
    
    # Bosniaand Herzegovina -> Bosnia and Herzegovina
    df[d] <- data.frame(lapply(df[d], gsub, pattern="and ",replacement= " and "))
    
  }
}

na2zero <- function (x) {
  x[is.na(x)] <- 0
  return(x)
}
# Get rid of all characters in measures except for numbers, the - sign, and period.dimensions, and change NA to 0.
if( length(measures) > 1) {
  for(m in measures) {
    df[m] <- data.frame(lapply(df[m], gsub, pattern="[^--.0-9]",replacement= ""))
    #df[m] <- data.frame(lapply(df[m], na2zero))
    df[m] <- data.frame(lapply(df[m], function(x) as.numeric(as.character(x)))) # This is needed to turn measures back to numeric because gsub turns them into strings.
  }
}

write.csv(df, file="../01Data/zipLatLong.csv", row.names=FALSE, na = "NA")
print(head(df))