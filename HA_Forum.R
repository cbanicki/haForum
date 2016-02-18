HA_Scrape <- function(Burl) {
  
  mybrowser$navigate(Burl)
  
  wxmessage <- mybrowser$findElement(using = 'css selector', "p") 
  
  #Get the curren URL 
  HA_ThreadDtl <- read_html(wxmessage$getCurrentUrl()[[1]])
  
  #Capture the text from the first page of comments
  # Will need to turn this into a vector with one element for each Post potentially
  # Also, will need to loop through the pages on each forum post to collect all of the comments
  # i <- i + 1
  
  ThreadText <- HA_ThreadDtl %>%
    html_nodes("p") %>%
    html_text() 
  
  # HA_ThreadDtl
  
  #append(ThreadDtl,ThreadText)
  return(ThreadText)
  #ThreadText
}


HA_Forum <- function() {
  
  
  library(rvest)
  library(XML)
  library("RSelenium")
  
  
  startServer()
  mybrowser <- remoteDriver(browserName = "chrome")
  mybrowser$open()
  
  # --------------------------------------------------------------------------------------------------------------  
  # Not used unless you need to log in
  # --------------------------------------------------------------------------------------------------------------
  
  #     Can just log in manually first (save password) time if you want to avoid these steps
  #     #Go to the forum site
  #     mybrowser$navigate("https://community.homeaway.com/community/us")
  #     
  #     # find the login button
  #     wxbutton <- mybrowser$findElement(using = 'css selector', "#navLogin") 
  #     wxbutton$clickElement()
  #     Sys.sleep(2)
  #     
  #     # Log in to forum
  #     logbutton <- mybrowser$findElement(using = 'css selector', "#username01")
  #     logbutton$sendKeysToElement(list("SECRET"))
  #     passbutton <- mybrowser$findElement(using = 'css selector', "#password01") 
  #     passbutton$sendKeysToElement(list("SECRET2"))
  #     GoButton <- mybrowser$findElement(using = 'css selector', "#login-submit")
  #     GoButton$clickElement()
  #     
  #     #Wait for page to load
  #     Sys.sleep(2)
  
  
  
  #Go back to forum page now that you are logged in
  mybrowser$navigate("https://community.homeaway.com/community/us")
  
  
  #  Murl <- "https://community.homeaway.com/community/us"
  library(stringr)
  html <- paste(readLines(url), collapse="\n")
  matched <- str_match_all(html, "<a href=\"(.*?)\"")
  links <- matched[[1]][, 2]
  
  # Get just the thread links
  TLinks <- grep("\\<thread\\>",links, value=T)
  
  #Append the base Url to the Thread to get the full URL
  urls <- sapply("https://community.homeaway.com",paste,TLinks,sep="")
  
  all <- file.path(path, "ALL")  
  
  download.maybe <- function(Turl, refetch=TRUE, path=".") {
    dest <- file.path(path, basename(Turl))
 
    #if (refetch || !file.exists(dest))
      HA_Scrape(Turl)
   
    write.table(ThreadText, dest, sep="\t")

    write.table(ThreadText, all, sep="\t",append=TRUE, col.names=FALSE)
  }
  
  path <- "threadData"
  dir.create(path, showWarnings=FALSE)
  files <- sapply(urls, download.maybe, path=path)



}
