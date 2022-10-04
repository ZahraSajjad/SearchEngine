search_index = function(query.vector, Mydtm){
  
  index = NULL
  
  while(TRUE){
    
    if(length(index) == 0){query = paste0("c10",".RData")}
    
    if(length(index) == 1){query = paste0("c10_",toString(index[1]),".RData")}
    
    if(length(index) == 2){query = paste0("c10_",toString(index[1]),"_",toString(index[2]),".RData")}
    
    if(length(index) == 3){query = paste0("c10_",toString(index[1]),"_",toString(index[2]),"_",toString(index[3]),".RData")}
    
    center = get(load(query))$center
    
    d = dist.matrix(t(query.vector), center, method = "euclidean")
    
    ind = which.min(d)
    
    index = c(index,ind)
    
    if (length(index) == 4){break}
    
  }
  
  L4 = paste0("c10_",toString(index[1]),"_",toString(index[2]),"_",toString(index[3]),".RData")
  L3 = paste0("c10_",toString(index[1]),"_",toString(index[2]),".RData")
  L2 = paste0("c10_",toString(index[1]),".RData")
  L1 = paste0("c10",".RData")
  
  L4_data = get(load(L4))
  L3_data = get(load(L3))
  L2_data = get(load(L2))
  L1_data = get(load(L1))
  
  
  ind0 = which(L4_data$cluster == index[4])
  ind1 = which(L3_data$cluster == index[3])
  ind2 = which(L2_data$cluster == index[2])
  ind3 = which(L1_data$cluster == index[1])
  
  result = ind3[ind2][ind1][ind0]
  
  result_dtm = as.matrix(Mydtm[result,])
  
  d = dist.matrix(t(query.vector), result_dtm, method = "euclidean")
  
  doc_id = rownames(result_dtm)
  
  Dsort <- sort(as.numeric(d), index.return = TRUE,decreasing = FALSE)
  
  sorted_doc = doc_id[Dsort$ix] 
  
  sorted_doc = data.frame(as.integer(sorted_doc))
  
  colnames(sorted_doc) = "Articles"
  
  
  return(sorted_doc)
  
  
  
}