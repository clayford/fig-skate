# web scraping figure skater personal bests

# http://www.isuresults.com/bios/fsbiosmen.htm and http://www.isuresults.com/bios/fsbiosladies.htm
# biographies of figure skaters. 

library(rvest)
library(stringr)
library(pbapply) # for progress bar 


# get URLs for each skater
male_urls <- "http://www.isuresults.com/bios/fsbiosmen.htm"
males <- readLines(male_urls)
males <- males[grepl(pattern = "HyperLink_Biography", x = males)]
urlsM <- str_extract(males, pattern = "/bios/.+\\.htm")
urlsM <- paste0("http://www.isuresults.com",urlsM)

female_urls <- "http://www.isuresults.com/bios/fsbiosladies.htm"
females <- readLines(female_urls)
females <- females[grepl(pattern = "HyperLink_Biography", x = females)]
urlsF <- str_extract(females, pattern = "/bios/.+\\.htm")
urlsF <- paste0("http://www.isuresults.com",urlsF)

# allURLS <- rbind(data.frame(URL = urlsF, gender = "Female", stringsAsFactors = F),
#             data.frame(URL= urlsM, gender = "Male", stringsAsFactors = F))

# allURLS <- c(urlsF, urlsM)


rm(female_urls, male_urls, females, males)

# function to get URLs for personal bests

getPBurl <- function(x){
  tmp <- readLines(x)
  get <- grepl("Personal Best", tmp)
  if(any(get)){
    tmp <- tmp[get]
    tmp <- unlist(str_split(tmp, pattern = "\""))
    if(any(grepl(pattern = ".htm$", tmp))){
      pburl <- tmp[grepl(pattern = ".htm$", tmp)]
      pburl <- paste0("http://www.isuresults.com/bios/", pburl)
      pburl  
    } else {
      NA
    }
  }
}

# get personal best URLs - female
pburlsF <- pblapply(urlsF, getPBurl)
pburlsF <- na.omit(unlist(pburlsF)) # 1090

# get personal best URLs - male
pburlsM <- pblapply(urlsM, getPBurl)
pburlsM <- na.omit(unlist(pburlsM)) # 674


# function to get personal bests

getPBinfo <- function(x, gender){
  tryCatch({   # to skip empty tables, which throws an error
    tmp <- read_html(x)
    name <- html_table(html_nodes(tmp, "table")[[1]])[1,1]
    dat <- html_table(html_nodes(tmp, "table")[[2]], fill = TRUE)
    if(all(dat[nrow(dat),1] == dat[nrow(dat),], na.rm = TRUE)){
      dat <- dat[-nrow(dat),] # drop last row if it has a date
    }
    dat$Name <- name
    dat$Gender <- gender
    if(names(dat)[1]==""){   # drop 1st 1st column if empty
      dat <- dat[,-1]
    }
    dat  
  }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
}

pbsF <- pblapply(pburlsF, getPBinfo, gender = "Female")
pbsM <- pblapply(pburlsM, getPBinfo, gender = "Male")


pbsDataF <- do.call(rbind, pbsF)
pbsDataM <- do.call(rbind, pbsM)


pbData <- rbind(pbsDataF, pbsDataM)

# clean up "historical record" rows
# table(pbsDataF$Type)
pbData <- pbData[pbData$Type != "H - Historic Record achieved before 2010/2011 season",]

save(urlsF, urlsM, pburlsF, pburlsM, pbData, pbsDataF, pbsDataM, file = "figSkate.Rda")

load("figSkate.Rda")

