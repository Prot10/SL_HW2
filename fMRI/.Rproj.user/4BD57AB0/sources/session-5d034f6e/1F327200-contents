mat_sana <- as.matrix(train[-1])

start <- 4

for (i in 1:(N_ROIs)){
  mat_sana[,start:(start+ROI_length-1)] <- apply(mat_sana[,start:(start+ROI_length-1)], 
                                                  MARGIN=2, 
                                                  FUN=scale)
  start <- start + ROI_length
}



for (i in 1:(N_ROIs)){
  sub_set <- train[, start:(start+ROI_length-1)]
  ROIs_mean <- apply(sub_set, MARGIN=1, FUN=mean)
  #ROIs_sd <- apply(sub_set, MARGIN=1, FUN=sd)
  train_partial[, i] <- ROIs_mean
  #train_partial[, i+N_ROIs] <- ROIs_sd
  start <- start + ROI_length
}

