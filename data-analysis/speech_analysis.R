install.packages("languageserver")
install.packages("readxl")
install.packages("httr")
library(httr)
library(readxl)

url <- "https://api-inference.huggingface.co/models/cardiffnlp/twitter-roberta-base-sentiment" 
headers <- add_headers(Authorization = "Bearer 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")

get_roberta_score <- function(text){
    text <- text # nolint
    response <- POST(url, headers, body = list(inputs = text), encode = "json")
    content <- content(response)
    return (content)
}

#TODO: automatize file read in from the excel sheet
speeches_info <- read_excel("trump_speech_analysis/speeches.xlsx")
speeches_info <- as.data.frame(speeches_info)


text1 <- readLines('/Users/barnabasepres/University/TDK/trump_speech_analysis/text_files/speech_1_"2024-06-06"_AZ.txt')  # nolint
words <- strsplit(text1, "\\s+")[[1]]

for (i in 1:(length(words)-1)){
    print(get_roberta_score(words[i:i+1]))
    break
    #TODO: combine words in bigrams
    #get normal output
    #collect output in a dataframe (maybe a list of sentiment scores which will be part of a df later)
}
get_roberta_score("thank you")
