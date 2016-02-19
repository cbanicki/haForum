

#HA_Forum.Rmd
#Chad Banicki  
#February 19, 2016  

#Synopsis:
# Vacation Rental Owner Discussion Forum Analysis
## A look at comments on discussion threads.  
### Analyzing only the first 100 threads and looking at each of their 3 most recent forum pages


# Naviage to forum, collect latest 100 thread links, and capture the latest three pages from those threads

HA_Forum <- function() {
  
  
    library(rvest)
    library(XML)
    library("RSelenium")
    library(stringr)
  
  # You may have to download a driver for your browser
  
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
    
    
    # Base url for the forum
    url <- "https://community.homeaway.com/community/us"
    
    
    # Get thread links
   urls <- GetLinks(url)
  
    
    #For testing only
    #subUrls <- unique(c(urls[grep("52741",url, fixed=T)],urls[grep("52481",urls, fixed=T)]))
    
    
    download.maybe <- function(Turl, refetch=TRUE, path=".") {
      dest <- file.path(path, basename(Turl))
   
        HA_Scrape(Turl) #52741 has one page and 52481 has many pages
  
    }
    
    #In case you want to output the data to a path
    path <- "threadData"
   # all <- file.path(path, "ALL")  
    dir.create(path, showWarnings=FALSE)
    
    # The main loop that goes through each url and starts collecting the text
    files <- sapply(urls, download.maybe, path=path) # use urls for prod and subUrls for testing
  
    #Combine the files and convert them into a data frame
    files.df <- as.data.frame(do.call(cbind, files))
    
    #Rename the columns with the thread number
    colNames <- substr(urls, 39, 45) 
    
    names(files.df) <- colNames
    
  # Not currently working due to the stucture of the data  
   # write.table(files.df,"ThreadText.txt", sep="\t")
    
    # Save the data file to working directory
    save(files.df,file="threads.Rda")
    
    #Can be used to load the data file later
    #  load("threads.Rda")
    


}


#***************************************************************************************************************

#                         Loop through and collect the latest 3 pages of Text for each thread

#**************************************************************************************************************


  HA_Scrape <- function(Burl) {
    
        mybrowser$navigate(Burl)
        
        ThreadText <- c()
      
        ThreadUrls <- GetLinks(Burl)
       
        
        
        
        ThreadScrape <- function(ThreadUrls) {
          
            ThreadDetails <- c() 
        
        
          out <- tryCatch({
            
               mybrowser$navigate(ThreadUrls)
            
                wxmessage <- mybrowser$findElement(using = 'css selector', "p") 
            
               #Get the curren URL 
               HA_ThreadDtl <- read_html(wxmessage$getCurrentUrl()[[1]])
            
                #Capture the text from the comments
                ThreadDetails <- HA_ThreadDtl %>%
                  html_nodes("p") %>%
                  html_text() 
          },
              error=function(cond){
                  message(paste("URL does not seem to exist:", ThreadUrls))
                  message("Here's the original error message:")
                  message(cond)
                  # Choose a return value in case of error
                  return(NA)},
              finally={
                return(ThreadDetails)
      
      }
      
      )
      
    }
    
        #Loop that captures the text from the urls
        ThreadText <- sapply(ThreadUrls, ThreadScrape) # append(ThreadText,sapply(ThreadUrls, ThreadScrape))
      
                
      return(ThreadText)
  
   
  }      


#***************************************************************************************************************

#                           Capture the URLs
  
#**************************************************************************************************************


    GetLinks <- function(Zurl)  {
    
      
      mybrowser$navigate(Zurl)
      
     
      html <- paste(readLines(Zurl), collapse="\n")
      matched <- str_match_all(html, "<a href=\"(.*?)\"")
      links <- matched[[1]][, 2]
      
      # Get just the thread links
      TSLinks <- grep("/thread/",links, value=T)
      
      #Append the base Url to the Thread to get the full URL
      Nurls <- sapply("https://community.homeaway.com",paste,TSLinks,sep="")
      
      Nurls <- unique(Nurls)
      
      return(Nurls)
      
    }
  
  

#***************************************************************************************************************
    
#                           Make a graph of the most common words used  
    
#**************************************************************************************************************





  makeGraph <- function() {


    png("HA_Forum.png", width=1200,height=800)  
    
    require(wordcloud) 
    require(tm)
    
    #data <- files.df #read.csv("threadData//ALL.csv")
    
    
    #mydata = read.table("mydata.txt")
    
    pal <- brewer.pal(9,"YlGnBu")
    pal <- pal[-(1:4)]
    
    dataCorpus <- Corpus(VectorSource(files.df))
    
    dataCorpus <- tm_map(dataCorpus, stripWhitespace)
    
    dataCorpus <- tm_map(dataCorpus, removeWords, stopwords("english"))
    
    dataCorpus <- tm_map(dataCorpus, removeWords, c("will", "voir", "just","get","can"))
    
    dataCorpus <- tm_map(dataCorpus, PlainTextDocument)
    
    
    dataCorpus <- tm_map(dataCorpus, stemDocument)
    
    wordcloud(dataCorpus, max.words = 50, random.order = FALSE,
              rot.per=0.35, use.r.layout=TRUE, colors=pal, main="HomeAway Community Forum")
    
    dev.off()

  }

#***************************************************************************************************************
  
#**************************************************************************************************************


