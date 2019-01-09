library(data.table)
library(ggplot2)
library(magrittr)
library(stats)
library(cluster) # hierarchical clustering
library(factoextra) # clustering visualization
library(purrr) # for map_dbl

data_path = '../data/'

read_table = function(fname, data_path=data_path) {
  data.table(read.csv(paste0(data_path, fname)))
}

# We performed panel logistic regression of gentrifying/non-gentrifying on 
# predictors we constructed from external data. To maximize the number of 
# data points we have, we ran clustering on 3-year rolling windows to produce
# clusters that we can associate with each area in each year. 
# We didn't end up with a lot of data but that's all we've got
# The regression was done in STATA. We will upload the code to this repo 
# when it becomes ready

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

# We partition the data into 3-year rolling windows to cluster from,
# so we were not using future information to judge whether an area 
# was gentrifying in the past. We simulate the situation where we are 
# forecasting gentrification. 
data_colnames = colnames(all_data)
cols_2012 = c(grep('2005.2009', data_colnames, value=TRUE),
              grep('2006.2010', data_colnames, value=TRUE),
              grep('2007.2011', data_colnames, value=TRUE))
data_2012 = all_data[, cols_2012, with=FALSE]

cols_2013 = c(grep('2006.2010', data_colnames, value=TRUE),
              grep('2007.2011', data_colnames, value=TRUE),
              grep('2008.2012', data_colnames, value=TRUE))
data_2013 = all_data[, cols_2013, with=FALSE]

cols_2014 = c(grep('2007.2011', data_colnames, value=TRUE),
              grep('2008.2012', data_colnames, value=TRUE),
              grep('2009.2013', data_colnames, value=TRUE))
data_2014 = all_data[, cols_2014, with=FALSE]

cols_2015 = c(grep('2008.2012', data_colnames, value=TRUE),
              grep('2009.2013', data_colnames, value=TRUE),
              grep('2010.2014', data_colnames, value=TRUE))
data_2015 = all_data[, cols_2015, with=FALSE]

cols_2016 = c(grep('2009.2013', data_colnames, value=TRUE),
              grep('2010.2014', data_colnames, value=TRUE),
              grep('2011.2015', data_colnames, value=TRUE))
data_2016 = all_data[, cols_2016, with=FALSE]

# Select one window to do clustering
subs_data_std = data.table(data_2013 %>% scale())
hc3 <- agnes(subs_data_std, method = "ward")

subs_data_std = data.frame(subs_data_std)
rownames(subs_data_std) = all_data.Sub.Borough

# Cut tree into 4 groups and visualize
sub_grp <- cutree(hc3, k = 4)
fviz_cluster(list(data = subs_data_std, cluster = sub_grp))