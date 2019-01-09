library(data.table)
library(ggplot2)
library(magrittr)
library(stats)
library(cluster) # hierarchical clustering
library(factoextra) # clustering visualization
library(purrr) # for map_dbl

# PCA tells us that under our current definition of gentrification, 
# NYU Furman Center's classification doesn't do a very good job 
# separating different types of neighborhood. Therefore, we produce 
# our own classification using hierarchical clustering, which does not 
# assume a certain number of clusters. Then we can vary the number of 
# clusters to find one that works best for us. 

data_path = '../data/'

read_table = function(fname, data_path=data_path) {
  data.table(read.csv(paste0(data_path, fname)))
}

furman_class = read_table('furman_class.csv')

bachelors = read_table('bachelors_pctg_robust.csv')
bachelors_delta = read_table('bachelors_pctg_delta_robust.csv')

rent = read_table('rent_recent_robust.csv')
rent_delta = read_table('rent_recent_delta_robust.csv')

income = read_table('renter_income_robust.csv')
income_delta = read_table('renter_income_delta_robust.csv')

dt = merge(furman_class, bachelors) 
dt = merge(dt, bachelors_delta)
dt = merge(dt, rent)
dt = merge(dt, rent_delta)
dt = merge(dt, income)
dt = merge(dt, income_delta)
all_data = dt

# Standardize variables to have mean 0 and variance 1
all_data_std = data.table(scale(all_data[, !c('Sub.Borough.Area', 
                                              'furman_class'), with=FALSE]))

# # Compare the agglomerative coefficient of each clustering method, 
# # which measures how much structure the method extracted from the data
# m <- c( "average", "single", "complete", "ward")
# names(m) <- c( "average", "single", "complete", "ward")
# 
# # function to compute coefficient
# ac <- function(x) {
#   agnes(all_data_std, method = x)$ac
# }
# 
# map_dbl(m, ac)

# Choose "ward"
hc3 <- agnes(all_data_std, method = "ward")

all_data_std = data.frame(all_data_std)
rownames(all_data_std) = all_data$Sub.Borough

# # Diagnostic plots
# fviz_nbclust(all_data_std, FUN = hcut, method = "wss")
# # Visualizing Gap Statistics finds that 4 is the best
# gap_stat <- clusGap(all_data_std, FUN = hcut, nstart = 25, K.max = 10, B = 50)
# fviz_gap_stat(gap_stat)

# Cut tree into 4 groups and visualize clusters
sub_grp <- cutree(hc3, k = 4)
fviz_cluster(list(data = all_data_std, cluster = sub_grp), repel=T,
             main='Neighborhood Grouping by Similarity') + 
  coord_fixed(ratio=1.25) +
  theme(legend.position="bottom")
# all_data_std$grp = sub_grp
# write.csv(all_data_std, file='groups.csv', sep=',', row.names = T)
