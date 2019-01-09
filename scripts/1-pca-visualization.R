library(data.table)
library(ggplot2)
library(magrittr)
library(stats)

data_path = '../data/'

read_table = function(fname, data_path=data_path) {
  data.table(read.csv(paste0(data_path, fname)))
}

# We define gentrification as the process by which a neighborhood
# sees faster-than-normal increase in renter income, education level, 
# and average rent

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

# Now finally the PCA
all_data_pca = prcomp(all_data[, !c('Sub.Borough.Area', 'furman_class'),
                               with=FALSE], 
                      center = TRUE, scale = TRUE)
# Check the proportions of variance explained
# summary(all_data_pca) 
# Check the weights of the first 14 principal components, 
# which add up to summarize 95% of variance in the data
# View(all_data_pca$rotation[, 1:14]) 

# Try ggbiplot as suggested by DataCamp
# library(devtools)
# install_github("vqv/ggbiplot")
library(ggbiplot)
ggbiplot(all_data_pca, labels=all_data$Sub.Borough.Area, ellipse = T,
         groups = all_data$furman_class) +
  theme(legend.position="bottom") + coord_fixed(ratio=0.75) + 
  ggtitle('Descriptors by Furman Classification')
