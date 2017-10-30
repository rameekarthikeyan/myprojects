gowersimilarity <- function(X) {
  n <- ncol(X)
  m <- nrow(X)
  xcol <-lapply(colnames(X), function(u) {eval(parse(text=paste0("X$",u)))})
  multi_fn <- function(q) {
    w <- as.data.table(q)
    w <- rbind(w)
    w <- as.matrix(w)
    return(w)
  }
  id_cate <- function(p){
    if (is.character(p)) {
      score_qual <- function(w) {
        as.numeric(w == p)
      }
      weight_qual <- function(k) {
        lapply(p, function(h) { as.numeric(!is.na(h))})
      }
      val_qual <-multi_fn(lapply(p, score_qual))
      wei_qual <- multi_fn(lapply(p,weight_qual))
      return(list(val_qual,wei_qual))
      }
  if ((min(p) == 0) && (max(p)==1) && (is.integer(p) == FALSE)){
    score_bin <- function(t) {
      scoreb <- unlist(lapply(p, function(g) {if ((t == g ) && (t !=0)) {
        return(1)
      }
        else {
          return(0)
        }}))
      return(scoreb)
    }
    weight_bin <- function (b){
      weightb <- unlist(lapply(p, function(h) {if (!((b == 0) && (h ==0))) {
        return(1)
      }
        else {
         return(0)
        }}))
      return(weightb)
    }
   val_bin <- as.data.table(lapply(p, score_bin))
   val_bin <- as.matrix(val_bin)
   wei_bin <- as.data.table(lapply(p, weight_bin))
   wei_bin <- as.matrix(wei_bin)
   return(list(val_bin, wei_bin))
  }
    else {
      if (is.double(p)){
        range_val <- max(p) - min(p)
        score_quant <- function(v) {
          1-(abs(v-p)/range_val)
        }
        weight_quant <- function(k) {
          lapply(p, function(h) { as.numeric(!is.na(h))})
        }
        val_quant <- multi_fn(lapply(p, score_quant))
        wei_quant <- multi_fn(lapply(p, weight_quant))
        return(list(val_quant, wei_quant))
      }
    }
    if (is.integer(p)) {
      score_cate<- function(z) {
        as.numeric(z == p)
      }
      weight_cate <- function(k) {
        lapply(p, function(h) { as.numeric(!is.na(h))})
      }
      val_cate <-multi_fn(lapply(p, score_cate))
      wei_cate <- multi_fn(lapply(p, weight_cate))
      return(list(val_cate, wei_cate))
      }
  }
  resu <- lapply(xcol,id_cate)
  score_list <-lapply(resu, function(v) { v[[1]]})
  weight_list <- lapply(resu, function(v) { v[[2]]})
  weight_list <- matrix(unlist(weight_list), nrow = m, ncol = m)
  score <- matrix(rep(0,m^2), nrow = m, ncol = m)
  weight <- score
  for(i in 1:length(score_list)) {
    weight <- weight + weight_list[[i]]
    score <- score + score_list[[i]]
  }
  similarity_score <- score/weight
  similarity_score[upper.tri(similarity_score, diag = TRUE)] <- 0
 return(similarity_score)
}