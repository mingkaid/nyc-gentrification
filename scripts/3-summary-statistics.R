# We visualize the summary statistics for each cluster to observe the 
# characteristics of each cluster. We had interesting observations

source('2-area-clustering.R')

housing_units_delta = read_table('housing_units_delta.csv')

burden = read_table('severe_rent_burden_low_income.csv')
burden_delta = read_table('severe_rent_burden_low_income_delta.csv')

affordable = read_table('affordable_30ami_robust.csv')

dt_sum = merge(dt, housing_units_delta)
dt_sum = merge(dt_sum, burden)
dt_sum = merge(dt_sum, burden_delta)
dt_sum = merge(dt_sum, affordable)
examine_data = dt_sum

# Compare the centroid of each cluster
centroid_data = examine_data[!is.na('X2005.2009_affordable')]
centroid_data$grp = sub_grp
centroid_data[, c('Sub.Borough.Area', 'furman_class') := NULL]

# Compute the summary statistics of each cluster's progress through time
centroids = data.table()
g_map = list()
g_map[[1]] = 'Gentrifying'
g_map[[2]] = 'Stagnating High-income'
g_map[[3]] = 'Non-gentrifying'
g_map[[4]] = 'High-income'
se <- function(x) sd(x, na.rm = T)/sqrt(length(x))
for (g in 1:4) {
  col_means = colMeans(centroid_data[grp == g], na.rm=T)
  col_se = as.matrix(centroid_data[grp == g, lapply(.SD, se)])[1,]
  col_names = names(col_means)
  
  #print(data.table(grp = g_map[[g]], name = col_names, avg = col_means))
  centroids = rbind(centroids, data.table(grp = g_map[[g]], name = col_names, 
                                          avg = col_means, se = col_se))
}
centroids[, year := as.integer(substring(name, first = 7, last = 10))]
centroids[, type := substring(name, first=12)]
centroids = centroids[!is.na(year)]

# Plot each cluster's progress through time
pd <- position_dodge(0.1) # move them .05 to the left and right should they overlap
centroids %>% ggplot(aes(x=year, y=avg, group=factor(grp), color=factor(grp))) + 
  geom_point() + geom_line() + facet_wrap(type ~., scale='free_y', nrow=2) + 
  geom_errorbar(aes(ymin=avg-se, ymax=avg+se), width=.5, position=pd) + 
  geom_hline(yintercept=0, linetype="dashed", color = "red") +
  theme(legend.position="bottom")

#dt = dcast(centroids, grp ~ name)
#write.table(dt, file='centroids.csv', sep=',', row.names = FALSE)