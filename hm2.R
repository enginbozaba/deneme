library(stringr) # for parse ,https://stringr.tidyverse.org/reference/str_remove.html
library(gplots)   # contains the heatmap.2 package
library(colorspace)


norml <- normalized_counts

# PARSE

type_cancer<- as.vector(str_remove_all(colnames(norml), "[.01234546789]"))
unique_type_cancer <- unique(type_cancer)    


tumor_ind <- c(which(type_cancer==unique_type_cancer[1]),which(type_cancer==unique_type_cancer[2]))
not_tumor_ind <-c(which(type_cancer==unique_type_cancer[3]))


# t-test

t_test_df <- data.frame(t_value =c() , p_value= c())

for (i in 1:length(normalized_counts[,1]) ) {
  
  t <- t.test(normalized_counts[i,tumor_ind], normalized_counts[i,not_tumor_ind])
  t_test_df[i,1] <- t$statistic
  t_test_df[i,2] <- t$p.value
  print(i)
  
  
}

# p value min 100

p_value_min_100_gene_ind<- which(t_test_df[,2]<0.05)[1:100] 

min_100_gene <- normalized_counts[p_value_min_100_gene_ind,]

min_100_gene_people_100 <-min_100_gene[1:25,c(tumor_ind[1:25],not_tumor_ind[1:25])]




#heatmap

col_breaks = c(seq(-3    ,-0.5  ,length=50),  # for green
               seq(-0.51    ,-0.1  ,length=100),  # for white
               seq(-0.11   ,3  , length=10)) #red


my_palette <- colorRampPalette(c("white", "yellow", "red"))(n = length(col_breaks)-1)

plott<-heatmap.2(data.matrix(min_100_gene_people_100), # specify the (scaled) data to be used in the heatmap
          scale="row", # we have already scaled the data
          trace="none",# cleaner heatmap
          breaks = col_breaks,
          col=my_palette,
          margins=c(10,9)# widens margins around plot
) 

plott

