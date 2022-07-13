# build function
article_summarizer <- function(article_url,sentence_count,page_css_code){
  
  # load libraries
  library(rvest,quietly = T)
  library(stringr,quietly = T)
  library(lexRankr,quietly = T)
  
  # read web article
  url = read_html(article_url)
  
  # extract text
  text <- url |> 
    html_nodes(page_css_code) |> 
    html_text()
  
  # clean text
  text_clean <- gsub("\n"," ",text) |> str_squish()
  
  # summarize article into sentences
  summarize <- lexRank(text_clean,
                       docId = "create",
                       n = sentence_count,
                       continuous = TRUE)
  
  # reorder sentences in order of rank
  text_reorder <- order(as.integer(gsub("_","",summarize$sentenceId)))
  
  # extract top n sentences
  topN_sentences <- summarize[text_reorder,]$sentence
  
  return(topN_sentences)
  
}

summarized <- article_summarizer(article_url = "https://www.bbc.com/news/world-asia-62154311",
                                 sentence_count = 10,
                                 page_css_code = ".eq5iqo00")

summarized






