# Load library packages 
library(tm)
library(SnowballC)
library(wordcloud)
library(RColorBrewer)
library(syuzhet)
library(ggplot2)
library(rvest)
library(curl)
library(xml2)
library(RCurl)
library(XML)
library(httr)

# Install packages if they are not already installed
install.packages("tm")
install.packages("SnowballC")
install.packages("wordcloud")
install.packages("RColorBrewer")
install.packages("syuzhet")
install.packages("ggplot2")
install.packages("rvest")
install.packages("curl")
install.packages("RCurl")
install.packages("XML")
install.packages("httr")

#------------------------ Code to scrape GPU inventory, price, html link, review rating, and review count -----------------------------#

# Initialize data frame 
df_total = data.frame()

# For loop to scrape 43 pages of GPU inventory
for(page in 1:43){
  link = paste0("https://www.newegg.com/p/pl?d=video+card&N=4115%204114%204113%204112%204111%204814&page=", page)
  webpage = read_html(link)
  
  # Use CSS selector data to scrape the product name
  product = html_nodes(webpage, '.item-cell .item-container .item-title')
  
  # Use CSS selector data to scrape the URL of the product
  product_url = html_nodes(webpage, '.item-cell .item-container .item-title') %>% html_attr('href')
  
  # Create a list of the product URLs
  list_data = product_url
  
  # Use CSS selector data to retrieve the current price of the video card
  price = html_node(webpage, '.open-popup , .item-action strong')
  
  # Use CSS selector to scrape the total number of reviews
  rating_count = html_nodes(webpage, ".item-cell .item-rating-num")
  
  # Use CSS selector to scrape the rating count: a scale between 1 and 5
  star_count = html_nodes(webpage, ".item-cell .rating") %>% html_attr('aria-label') 
  
  # Convert the heading data to text
  product1 = html_text(product)
  price = html_text(price)
  rating_num = html_text(rating_count)
  
  # Combine the scraped data into a dataframe
  scraped <- data.frame(
    Title = product1, 
    Price = price,
    Reviews = rating_num,
    Eggs = star_count,
    product_url
  )
  
  # Concatenate each page scraped on Newegg: Pages 1 through 43 using a 36 item per page view.
  df_total <- rbind(df_total, scraped)
}



#------------------------Remove keywords "Pros:", "Cons:", and "Overall Review: " from reviews and store new data in a list -----------#    



#Create a list with removed keywords that contains all reviews 
list_new[] = gsub("Pros: |Cons: |Overall Review: ", "", df_clean_reviews[,1:1][1:10988])



#------------------------ Code to scrape GPU actual inventory ------------------------------------------------------------------------#


# A for loop to loop through all 1,528 URL of GPUs
for (page in df_total[,5:5][1:1528]){
  # A try catch to prevent errors from stopping the loop
  tryCatch(  {
    
    #The read_html code had to be written this way to avoid "Error in open. connection(x, "rb") : HTTP error 500/503
    con <- url(page)
    print(page) #Show URL currently being scraped
    webpage = read_html(con)
    
    product_reviews = html_nodes(webpage, '.comments-content')
    
    print(product_reviews) #Show reviews currently being scraped 
    
    review_txt = html_text(product_reviews)
    
    # Combine the scraped data into a dataframe
    reviews <- data.frame(
      Review = review_txt, 
      Product_URL = page)
    
    # Concatenate each page scraped on Newegg: Pages 1 through 43 using a 36 item per page view.
    df_reviews <- rbind(df_reviews, reviews)
    
  }, silent = TRUE)
  
  # Attempt to avoid bot detection by letting the system sleep between each URL scrape
  Sys.sleep(2)
}




#------------------------Create graphs for data analysis and discussion --------------------------------------------------------------#   



# Create a corpus / vector source for the text analysis
text = list_new #stores raw reviews
TextDoc <- Corpus(VectorSource(list_new)) #Creates a corpus function


# Build a term-document matrix
TextDoc_dtm <- TermDocumentMatrix(TextDoc)
dtm_m <- as.matrix(TextDoc_dtm)

# Sort by decreasing value of frequency
dtm_v <- sort(rowSums(dtm_m),decreasing=TRUE)
dtm_d <- data.frame(word = names(dtm_v),freq=dtm_v)

# Display the top 5 most frequent words
head(dtm_d, 20)

# Plot the most frequent words
barplot(dtm_d[1:20,]$freq, las = 2, names.arg = dtm_d[1:20,]$word,
        col ="lightgreen", main ="Top 20 most frequent words",
        ylab = "Word frequencies")

# Generate a word cloud
set.seed(1234)
wordcloud(words = dtm_d$word, freq = dtm_d$freq, min.freq = 5,
          max.words=100, random.order=FALSE, rot.per=0.40, 
          colors=brewer.pal(8, "Dark2"))

# Find associations 
findAssocs(TextDoc_dtm, terms = c("good","work","health"), corlimit = 0.25)		

# Find associations for words that occur at least 50 times
findAssocs(TextDoc_dtm, terms = findFreqTerms(TextDoc_dtm, lowfreq = 50), corlimit = 0.25)

# Convert list to vector for sentiment analysis 
text <- unlist(list_new, use.names = FALSE)

# Obtain regular sentiment score using get_sentiment() function 
syuzhet_vector <- get_sentiment(text, method="syuzhet")

# See the first row of the vector
head(syuzhet_vector)

# See summary statistics of the vector
summary(syuzhet_vector)

# Bing sentiment tool
bing_vector <- get_sentiment(text, method="bing")
head(bing_vector)
summary(bing_vector)

# Affin vector creation
afinn_vector <- get_sentiment(text, method="afinn")
head(afinn_vector)
summary(afinn_vector)

# run nrc sentiment analysis to return data frame with each row classified as one of the following emotions, rather than a score: 
# anger, anticipation, disgust, fear, joy, sadness, surprise, trust 
# It also counts the number of positive and negative emotions found in each row
d<-get_nrc_sentiment(text)
# See top 10 lines of the get_nrc_sentiment dataframe
head (d,10)

# Transpose
td<-data.frame(t(d))

# The function rowSums computes column sums across rows for each level of a grouping variable.
td_new <- data.frame(rowSums(td[2:253]))

# Transformation and cleaning
names(td_new)[1] <- "count"
td_new <- cbind("sentiment" = rownames(td_new), td_new)
rownames(td_new) <- NULL
td_new2<-td_new[1:8,]

# Plot One - count of words associated with each sentiment
quickplot(sentiment, data=td_new2, weight=count, geom="bar", fill=sentiment, ylab="count")+ggtitle("Overall Review Sentiment")

# Plot two - count of words associated with each sentiment, expressed as a percentage
barplot(
  sort(colSums(prop.table(d[, 1:8]))), 
  horiz = TRUE, 
  cex.names = 0.7, 
  las = 1, 
  main = "Sentiment as Percentage", xlab="Percentage"
)

