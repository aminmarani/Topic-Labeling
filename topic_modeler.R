createModel <- function(documents, stopwords, model_method="lda", label_method="topn", label_len=3, model_use_public=F, n.topics=50){
  source(paste(c("modellers/", model_method, "_topic_modeler.R"), collapse = ''))
  source(paste(c("labellers/", label_method, "_topic_labeler.R"), collapse = ''))
  
  if(model_use_public){
    print("Using existing public model")
    model = public.model
  }else{
    print(paste("Building model: Method:", model_method))
    model = findTopics(documents, n.topics, stop_words=stopwords, word_min_freq=2)
    public.model <<- model
  }
  
  print(paste("Creating labels: Method:", label_method))
  labels <- getLabels(documents, model, stop_words=stopwords, label_len=label_len)
  
  print("Building final output ...")
  doc.topics.frame <- data.frame(model$doc_distr)
  colnames(doc.topics.frame) <- labels
  
  print("Done!")
  return(doc.topics.frame)
}