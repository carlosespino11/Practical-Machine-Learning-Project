download_data = function() {
  if(!file.exists("training.csv")){
    download.file("https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv", "training.csv")
  }
  if(!file.exists("testing.csv")){
    download.file("https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv", "testing.csv")
  }
  
}

clean_data = function(data) { 
  data = data %>% as.tbl() %>% 
    mutate(cvtd_timestamp = dmy_hm(cvtd_timestamp), day = wday(cvtd_timestamp, label=T), 
           hour = factor(hour(cvtd_timestamp))) %>% select(new_window:hour) 
  
  data[,which(!apply(data,2,FUN = function(x){(sum(is.na(x))/length(x)) > .95}))]
}

get_data = function() {
  download_data()
  training = read.csv("training.csv", na.strings=c("NA","","#DIV/0!")) %>% clean_data()
  testing = read.csv("testing.csv", na.strings=c("NA","","#DIV/0!")) %>% clean_data()
  
  set.seed(11)
  inTrain = createDataPartition(y = training$classe, p=0.7, list=FALSE)
  
  training = training[inTrain, ]
  validation = training[-inTrain, ]
  
  allData = list(training = training, validation = validation, testing = testing)
}

pml_write_files = function(x){
  n = length(x)
  for(i in 1:n){
    filename = paste0("problem_id_",i,".txt")
    write.table(x[i],file=filename,quote=FALSE,row.names=FALSE,col.names=FALSE)
  }
}
