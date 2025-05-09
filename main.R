PATH_TO_DATA_FILE = "/cloud/project/training_data.zip"

library(stringr)
library(readtext)
library(tm)
library(textstem)

load_dataset = function(path_to_zip_file){
  #unzip("training_data.zip",exdir="training_data")
  
  # decompress the .zip file sepcified in the parameter "path_to_zip_file"
  dataset_files = unzip(path_to_zip_file)
  
  #delete wrong format file
  WrongFile = c("./training_data/109_ Ocean/zuRn.zip","./training_data/109_ Ocean/zuRn",
                "./training_data/109_ Ocean/g1Gl","./training_data/109_ Ocean/g1Gl.zip")
  unlink(WrongFile,recursive = TRUE)
  
  
  # calculate the total number of datasets inside the .zip file
  num_files = length(dataset_files)
  
  # create empty vectors for storing
  # 1. text values for the "Text" column of the data frame;
  # 2. path to .txt files for the "File" column of the data frame;
  # 3. topics for the "Topic" column of the data frame;
  # 4. files that cannot be processed.
  txt_vector = vector()
  file_vector = vector()
  topic_vector = vector()
  error_datasets = vector()
  
  for (i in 1:num_files){
    # obtain the path to the .zip file of the dataset in the current iteration
    file_path = dataset_files[i]
    print(paste0("Reading dataset ", as.character(i), "/", as.character(num_files), ": ", file_path))
    
    # check whether the path contain the character ".zip"
    if (grepl(".zip", file_path, fixed=TRUE)){
      # if yes, obtain the path to the directory after decompressing the .zip file
      dir_path = stringr::str_replace(file_path,".zip","")
      # decompress the .zip file
      unzip(file_path,exdir = dir_path)
      
      # obtain the topic from the path to the dataset
      # by splitting the path and obtain the appropriate element inside
      topic = unlist(strsplit(file_path,split="/"))[3]
      
      # obtain a vector of all the .txt files under the the directory of the dataset
      
      txt_files = list.files(dir_path,pattern = ".txt",recursive = TRUE)
      
      
      
      # check whether the there are at least 10 .txt file inside the dataset
      if (length(txt_files) >= 10){
        # if yes, for each txt file, read the text values inside and save corresponding values inside the vectors above
        for (f in txt_files){
          # obtain the path to the .txt file
          txt_file_path = paste(dir_path,f,sep="/")
          # read text values inside the .txt file
          txt = readLines(txt_file_path)
          # convert multiple lines of text into a single character
          txt = paste(txt,collapse = "\n")
          
          # append corresponding values to the above vectors of "txt_vector", "file_vector", and "topic vector"
          txt_vector = c(txt_vector,txt)
          file_vector = c(file_vector,txt_file_path)
          topic_vector = c(topic_vector,topic)
          
        } 
      } else {
        # if the dataset contain less than 10 datasets, put the path to the dataset file into the vector of "error_datasets"
        error_datasets = c(error_datasets,file_path)
      }
    } else {
      # if the dataset file is not in .zip format, also put its path into the vector of "error_datasets"
      error_datasets = c(error_datasets,file_path)
    }
  }
  print("Error datasets:")
  print(error_datasets)
  
  # store all the value into a data frame object
  # by using the above "txt_vector", "file_vector", and "topic vector"
  dataframe = data.frame("Text"=txt_vector,"File"=file_vector,"Topic"=topic_vector)
  
  return(dataframe)
}
pre_process = function(text){
  #error
  # Normalize encoding from Latin1 to ASCII
  x <- iconv(text, "latin1", "ASCII", sub="")
  
  # change \n and \t to ""
  x <- gsub("\n", " ", x)
  x <- gsub("\t", " ", x)
  
  x=tolower(x)
  x<-removeWords(x,stopwords())
  x<-removePunctuation(x)
  
  x_list<-str_split(x," ")
  x_vec<-unlist(x_list)
  x_vec<-lemmatize_words(x_vec)
  
  custom_stopwords <- c("use","can","increase","change","model","large","high","datum","also" ) 
  
  # Remove custom stopwords
  x_vec <- x_vec[!(x_vec) %in% custom_stopwords]
  
  #Removes words that are two or fewer characters long
  x_vec <- x_vec[nchar(x_vec) > 2]
  x_vec <- x_vec[x_vec !=""]
  return (x_vec)
}
classify<-function(x_vec,topicKeyWord){
  #demo code
  x_dict<-unique(x_vec)
  y<-rep(0,length(x_dict))
  for (x_ind in seq_along(x_dict)){
    y[x_ind]<-length(which(match(x_vec,x_dict[x_ind])==1))
  }
  df <- data.frame(matrix(ncol = length(x_dict), nrow = 0))
  df<-rbind(df,y)
  colnames(df) <- x_dict
  
  #Keyword Analysis
  total_keyword_count = 0
  keyword_count = 0
  ocean_count = 0  
  for (i in seq_along(topicKeyWord)) {
    if (topicKeyWord[i] %in% colnames(df)) {
      keyword_count = keyword_count + 1
      total_keyword_count = total_keyword_count + df[1, topicKeyWord[i]]
      if (topicKeyWord[i] == "ocean") {  #if have "ocean"
        ocean_count <- df[1, topicKeyWord[i]]  # update ocean count
      }
    }
  }
  
  #Classification Decision
  if (total_keyword_count > 80 & keyword_count>=6 & ocean_count >= 8){
    tmd= TRUE
  }else{
    tmd= FALSE
  }
  
  return (tmd)
}
save_results = function(dataset,classification){
  dataset <- cbind(dataset, classification)
  colnames(dataset)[ncol(dataset)] <- "Classification"
  write.csv(dataset, file = "result.csv",row.names = FALSE)
}
evaluate = function(true_labels,predictions){
  #True Positive
  TP <- sum(true_labels & predictions) #28
  #True Negative
  TN <- sum(!true_labels & !predictions) #1879
  #False Positive
  FP <- sum(!true_labels & predictions) #17
  #False Negative
  FN <- sum(true_labels & !predictions) #2

  
  accuracy = (TP + TN) / (TP + FP + TN + FN)
  precision <- TP / (TP + FP)
  recall <- TP / (TP + FN)
  
  cat("Accuracy:", round(accuracy, 4), "\n")
  cat("Precision:", round(precision, 4), "\n")
  cat("Recall:",  round(recall, 4), "\n")
  
}
visualize <- function(df) {
  # Determine the unique value of the Topic and sub number and _
  df$Topic <- sub("^[0-9]+_", "", df$Topic)
  topics <- unique(df$Topic)
  
  #Dataset visualize
  jpeg(filename="dataset.jpg", width = 800, height = 1000)
  par(mar=c(5, 10, 5, 5)) 

  topic_counts <- table(factor(df$Topic, levels = topics)) #Create a frequency table for the Topic column using sorted unique values as levels
  
  barplot(topic_counts, main="Data Set", 
          xlab="Number of txt Files", horiz=TRUE,
          col="orange", las=1) #las=1 the X-axis label appear horizontal
  dev.off()
  
  #Classification Results
  jpeg(filename="classificationresults.jpg", width = 800, height = 1000)
  par(mar=c(5, 10, 5, 5)) 
  
  result_table <- table(Topic=factor(df$Topic, levels = topics), Classification=factor(df$Classification, levels = c("FALSE", "TRUE")))

  barplot(t(result_table), 
          names.arg=topics, 
          main="Classifcation Results (Ocean)", 
          xlab="Number of txt Files", 
          horiz=TRUE, 
          legend.text=c("FALSE", "TRUE"),
          col=c("red", "blue"), 
          las=1)  # las=1 the X-axis label appear horizontal
  
  dev.off()
}

df <- load_dataset(PATH_TO_DATA_FILE)

#Topic Ocean
myTopicIndex=(which(df$Topic=="109_ Ocean")) # find all Ocean index

#save Ocean all word
myTopicAllWord= c() 
for (i in myTopicIndex){
  wordVec = pre_process(df$Text[i])
  myTopicAllWord = c(myTopicAllWord, wordVec)
}

#Finding Keywords for the Topic Ocean
frequency_table <- table(myTopicAllWord) #find myTopicAllWord each word frequency

sorted_frequency_table <- sort(frequency_table, decreasing = TRUE)
#View(sorted_frequency_table) # use to view sorted frequency of each words

OceanKeyWord <- names(head(sorted_frequency_table, 10)) #Gets the top ten most frequent strings
#"ocean","water","sea","marine","warm","plastic","surface",temperature","deep","climate"



#using forloop to claasify all text and save in one vector
allClassificationResult=c()
for (i in 1:length(df$File)){
  print(df$File[i]) # for known working on which file
  wordVec = pre_process(df$Text[i])
  labelResult = classify(wordVec,OceanKeyWord)
  allClassificationResult <- c(allClassificationResult, labelResult)
}

#save allClassificationResult to df and output result.csv
save_results(df,allClassificationResult)


# make trueLabels for evaluate (only Ocean is TRUE,other is FALSE)
trueLabels = rep(FALSE,length(df$Text)) #set all index to FALSE
trueLabels[myTopicIndex]= TRUE #change "Ocean" topic index to TRUE

#calculate accuracy,precision and recall
evaluate(trueLabels,allClassificationResult)

df_result = read.csv("result.csv")

#visualize Dataset and Classification Results
visualize(df_result)


