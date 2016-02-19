

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
    
    #mineThread(files.df)

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

 mindThread <- function(files.df) {
   #All of this was taken from here:
   #https://rstudio-pubs-static.s3.amazonaws.com/31867_8236987cf0a8444e962ccd2aec46d9c3.html
   
   
   library(tm)   
   threads <- Corpus(VectorSource(files.df))  
   
   #summary(threads)
   
    #inspect(threads[2])
   
   #remove puncutation
   threads <- tm_map(threads, removePunctuation)  
   
      # To remove special characters
      #    for(j in seq(docs))   
      #    {   
      #      docs[[j]] <- gsub("/", " ", docs[[j]])   
      #      docs[[j]] <- gsub("@", " ", docs[[j]])   
      #      docs[[j]] <- gsub("\\|", " ", docs[[j]])   
      #    } 
   
   #remove numbers
   threads <- tm_map(threads, removeNumbers)
   
   # make lowercase
   threads <- tm_map(threads, tolower) 
   
   #remove stopwords
   threads <- tm_map(threads, removeWords, stopwords("english"))
   
   #remove specific words
  threads <- tm_map(threads, removeWords, c("will", "voir", "just","get","can"))
  
  #combining words that should stay together
#   for (j in seq(docs))
#   {
#     docs[[j]] <- gsub("qualitative research", "QDA", docs[[j]])
#     docs[[j]] <- gsub("qualitative studies", "QDA", docs[[j]])
#     docs[[j]] <- gsub("qualitative analysis", "QDA", docs[[j]])
#     docs[[j]] <- gsub("research methods", "research_methods", docs[[j]])
#   }
  
  #remove word endings like 'ing' 'es' 's'
  library(SnowballC)   
  threads <- tm_map(threads, stemDocument)   

  #strip whitespace
  threads <- tm_map(threads, stripWhitespace)  
  
  #treat as text documents...make sure to use this   
  threads <- tm_map(threads, PlainTextDocument) 
  
  #create a document term matrix
  dtm <- DocumentTermMatrix(threads)
  
  #transpose the matrix
  tdm <- TermDocumentMatrix(threads) 
  
  freq <- colSums(as.matrix(dtm))  
  
  #length(freq)
  
  ord <- order(freq) 
  
#   If you prefer to export the matrix to Excel:   
#     m <- as.matrix(dtm)   
#   dim(m)   
#   write.csv(m, file="dtm.csv") 
  
  #  Start by removing sparse terms:   
  dtms <- removeSparseTerms(dtm, 0.1) # This makes a matrix that is 10% empty space, maximum.   
  #inspect(dtms) 
  
  #There are a lot of terms, so for now, just check out some of the most and least frequently occurring words.
  #freq[head(ord)]  
  
  #Check out the frequency of frequencies.
  
  #head(table(freq), 20)   
  
  #tail(table(freq), 20)   
  
  #For a less, fine-grained look at term freqency we can view a table of the terms we selected when we removed sparse terms, above. (Look just under the word "Focus".)
  freq <- colSums(as.matrix(dtms))   
 # freq
  
  
  #The above matrix was created using a data transformation we made earlier. What follows is an alternative that will accomplish essentially the same thing.
  freq <- sort(colSums(as.matrix(dtm)), decreasing=TRUE)   
 # head(freq, 14) 
  
#   An alternate view of term frequency:
#     This will identify all terms that appear frequently (in this case, 50 or more times).
  
  findFreqTerms(dtm, lowfreq=200)   # Change "50" to whatever is most appropriate for your text data.
#   
#   Yet another way to do this:
#     
    wf <- data.frame(word=names(freq), freq=freq)   
  head(wf)
  
#   Nice bar plot
#   Plot Word Frequencies
#   Plot words that appear at least 50 times.
  library(ggplot2)   
  p <- ggplot(subset(wf, freq>500), aes(word, freq))    
  p <- p + geom_bar(stat="identity")   
  p <- p + theme(axis.text.x=element_text(angle=45, hjust=1))   
  p  
  
  #interesting association
  #If words always appear together, then correlation=1.0.
  findAssocs(dtm, c("fee" , "airbnb"), corlimit=0.98) # specifying a correlation limit of 0.98  
#   $fee
#    avoid expens 
#    0.98    0.98 
#   
#   $airbnb
#   advertis     soon 
#   0.99     0.99 
  
 # In this case, "question" and "analysi" were highly correlated with numerous other terms. Setting corlimit= to 0.98 prevented the list from being overly long. Feel free to adjust the corlimit= to any value you feel is necessary.
  
  findAssocs(dtms, "contrast", corlimit=0.90) # specifying a correlation limit of 0.95   
  
  # Wordcloud at least 25 times
  library(wordcloud) 
  set.seed(142)   
  wordcloud(names(freq), freq, min.freq=25)  
  
  
  #Plot the 100 most frequently used words.
  
  set.seed(142)   
  wordcloud(names(freq), freq, max.words=100) 
  
  #Add some color and plot words occurring at least 20 times.
  
  set.seed(142)   
  wordcloud(names(freq), freq, min.freq=25, scale=c(5, .1), colors=brewer.pal(6, "Dark2")) 
  
 # Plot the 100 most frequently occurring words.
  set.seed(142)   
  dark2 <- brewer.pal(6, "Dark2")   
  wordcloud(names(freq), freq, max.words=100, rot.per=0.2, colors=dark2) 
  
  
 # Clustering by Term Similarity
  #To do this well, you should always first remove a lot of the uninteresting or infrequent words. If you have not done so already, you can remove these with the following code.
  
  dtmss <- removeSparseTerms(dtm, 0.15) # This makes a matrix that is only 15% empty space, maximum.   
  inspect(dtmss)  
  
  
  #Hierarchal Clustering
  #First calculate distance between words & then cluster them according to similarity.
  
  library(cluster)   
  d <- dist(t(dtmss), method="euclidian")   
  fit <- hclust(d=d, method="ward")   
  fit  
  
  plot(fit, hang=-1)   
#   
#   Helping to Read a Dendrogram
#   If you find dendrograms difficult to read, then there is still hope.
#   To get a better idea of where the groups are in the dendrogram, you can also ask R to help identify the clusters. Here, I have arbitrarily chosen to look at five clusters, as indicated by the red boxes. If you would like to highlight a different number of groups, then feel free to change the code accordingly.
#   
  plot.new()
  plot(fit, hang=-1)
  groups <- cutree(fit, k=5)   # "k=" defines the number of clusters you are using   
  rect.hclust(fit, k=5, border="red") # draw dendogram with red borders around the 5 clusters 
  
  
#   K-means clustering
#   The k-means clustering method will attempt to cluster words into a specified number of groups (in this case 2), such that the sum of squared distances between individual words and one of the group centers. You can change the number of groups you seek by changing the number specified within the kmeans() command.
#   
  library(fpc)   
  d <- dist(t(dtmss), method="euclidian")   
  kfit <- kmeans(d, 2)   
  clusplot(as.matrix(d), kfit$cluster, color=T, shade=T, labels=2, lines=0)   
 }
