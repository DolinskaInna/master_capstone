ipc.df <- data.frame(matrix(ncol = length(total.unique.ipc), nrow = NROW(work.data)))
colnames(ipc.df) <- unique(unlist(strsplit(paste(work.data$IPC, collapse = " | "), split = " | ", fixed = T)))
