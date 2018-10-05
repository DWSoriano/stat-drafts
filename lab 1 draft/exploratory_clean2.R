############################
# Data quality and cleaning
############################

###################################################
# Cleaning step 1
# Get rid of observations with no answers (all 0s)
###################################################
# Get rid of rows with all Questions missing answer (row sum = 0)
ling_data$question_sum <-
  rowSums(ling_data[,names(ling_data)[grepl("Q{1}[[:digit:]]{3}", 
                                                  names(ling_data))]])
# Remove 1,040 observations with no answers
ling_data_no_missing <-
  filter(ling_data, !(question_sum == 0))

# 995 NAs for lat/long
nrow(filter(ling_data_no_missing, is.na(lat)))

# Is there a way to get the lat/long data from data with lat/long data
missing_zips <-
  ling_data_no_missing %>%
  filter(is.na(lat)) %>%
  distinct(ZIP)

# See if zip codes with missing lat/long data are in other rows with lat/long:
nrow(filter(ling_data_no_missing, !is.na(lat), ZIP %in% missing_zips$ZIP))

# No, zip codes with missing lat/long values don't appear in data with lat/long

# Notice that, for the same ZIP Code, we have cities ranging from Los Angeles
# to New York to Toronto. I assume that the ZIP Code and longitude/latitude
# values are accurate and that the city and state values are inaccurate since
# they were self reported by respondents.
distinct(filter(ling_data_no_missing, ZIP == 90210), CITY, STATE)

# Get rid of observations without values for long/lat
ling_data_long_lat <-
  filter(ling_data_no_missing, !is.na(lat))

rm(ling_data_no_missing)

###############################################################################
# 2. Pick two survey questions and investigate their relationship to each other 
# and geography. You will need to use maps and can experiment with 
# interactivity, e.g. using linked brushing (see the crosstalk R package: 
# https://rstudio.github.io/crosstalk/). Do the answers to the two questions 
# define any distinct geographical groups? Does a response to one question help
# predict the other? Try to analyze the categorical data for more than 2 
# questions.
###############################################################################

####################################################################
# Examine Q050: 
# What word(s) do you use to address a group of two or more people?
####################################################################

# Filter for Q050 most common answers and longitude only for contiguous USA
plural_you <- 
  ling_data_long_lat %>%
  filter(Q050 %in% c(1,4,7,9), long > -125)

# Extract the answers to question 50
answers_plural_you <- all.ans[[50]]

# Make column to join ling data with answers
answers_plural_you$Q050 <- rownames(answers_plural_you)
plural_you$Q050 <- as.character(plural_you$Q050)
# Join in answers
answers_plural_you_plot <- 
  inner_join(plural_you, answers_plural_you, 
             by = "Q050")

# For purposes of plotting, make lat_plot to move up points south of Florida
answers_plural_you_plot$lat_plot <-
  with(answers_plural_you_plot,
       ifelse(lat >= 25.5,
              lat,
              25.5))

# Plot on map
ggplot(answers_plural_you_plot) +
  geom_point(aes(x = long, y = lat_plot, color = ans), 
             size = 3, alpha = 0.5) +
  geom_polygon(aes(x = long, y = lat, group = group),
               data = state_df, colour = "black", fill = NA) +
  ggtitle(filter(quest.use, qnum == 50)$quest) +
  coord_fixed(1.3) +
  map_settings

rm(answers_plural_you, plural_you, answers_plural_you_plot)

##############################################
# Examine Q089: Can you call coleslaw 'slaw'?
##############################################

# Filter for Q089 most common answers and longitude only for contiguous USA
slaw <- 
  ling_data_long_lat %>%
  filter(Q089 %in% c(1:4), long > -125)

# Extract the answers to question 89
answers_slaw <- all.ans[[89]]

# Make column to join ling data with answers
answers_slaw$Q089 <- rownames(answers_slaw)
slaw$Q089 <- as.character(slaw$Q089)
# Join in answers
answers_slaw_plot <- 
  inner_join(slaw, answers_slaw, 
             by = "Q089")

# For purposes of plotting, make lat_plot to move up points south of Florida
answers_slaw_plot$lat_plot <-
  with(answers_slaw_plot,
       ifelse(lat >= 25.5,
              lat,
              25.5))

# Plot on map
ggplot(answers_slaw_plot) +
  geom_point(aes(x = long, y = lat_plot, color = ans), 
             size = 3, alpha = 0.5) +
  geom_polygon(aes(x = long, y = lat, group = group),
               data = state_df, colour = "black", fill = NA) +
  ggtitle(filter(quest.use, qnum == 89)$quest) +
  coord_fixed(1.3) +
  map_settings

rm(answers_slaw, slaw, answers_slaw_plot)

# Examine relationship between questions

# Get data frame with most common answers to questions 50 and 89
q50_q89 <-
  select(ling_data_long_lat, Q050, Q089) %>%
  filter(Q050 %in% c(1,4,7,9), Q089 %in% c(1,3))

# Change from numeric to categorical
q50_q89$q50_ans <-
  with(q50_q89,
       ifelse(Q050 == 1,
              "you all",
              ifelse(Q050 == 4,
                     "you guys",
                     ifelse(Q050 == 7,
                            "you",
                                   "y'all"))))

q50_q89$q89_ans <-
  with(q50_q89,
       ifelse(Q089 == 1,
              "yes",
              "no"))

total <- sum
kable(addmargins(table(select(q50_q89, q50_ans, q89_ans )), FUN = total)) %>%
  kable_styling(bootstrap_options = "striped", full_width = F, 
                position = "center")

# Of the 7,061 survey participants who responded that they address a group of
# two or more people as y'all, 73% of them responded yes when asked if you can
# call coleslaw 'slaw.'
                      
                      
  
###############
# COME BACK TO
# Plot the ling_location data (which lives on a grid).  
# Note that this doesn't look great with
# state outlines. You can probably do better!
ling_location %>%
  filter(Longitude > -125) %>%
  ggplot() +
  geom_tile(aes(x = Longitude, y = Latitude, 
                color = log10(V12), fill = log10(V12))) +
  geom_polygon(aes(x = long, y = lat, group = group),
               data = state_df, colour = "gray", fill = NA) +
  blank_theme

#####
# 2 To come back to:
# experiment with interactivity
# Do the answers to the two questions define any distinct geographical groups? 
# Does a response to one question help predict the other? 
# Try to analyze the categorical data for more than 2 questions.
#####

# Does a response to one question help predict the other? 
# Try to analyze the categorical data for more than 2 questions.
105 What is your generic term for a sweetened carbonated beverage?

# Get participants who answered that their generic term for a sweetened 
# carbonated beverage is coke.
coke <-
  filter(ling_data_long_lat,
         Q105 == 3, Q050 !=0, Q089 !=0) %>%
  select(Q050, Q089)

# Of people who answered coke, how many also answered y'all
nrow(filter(coke, Q050 == 9))/nrow(coke)
# 57%. Compared to 14% of all people who answered y'all

# Of people who answered coke, how many also answered yes to slaw
nrow(filter(coke, Q089 %in% c(1,2)))/nrow(coke)
# 71%. Compared to 48% of all people who answered y'all

# Of people who answered coke, how many also answered y'all and yes to slaw
nrow(filter(coke, Q089 %in% c(1,2), Q050 == 9))/nrow(coke)
nrow(filter(ling_data_long_lat, Q089 %in% c(1,2), Q050 == 9))/nrow(
  ling_data_long_lat)
# 44%. Compared to 13% of all people who answered y'all and yes to slaw

# Make interactive plot for slaw:
answers_slaw_plot_interactive <-
  filter(answers_slaw_plot, Q089 %in% c(1,2,3)) %>%
  select(Q089, long, lat)

answers_slaw_plot_interactive$q89_ans <-
  with(answers_slaw_plot_interactive,
       ifelse(Q089 == 1,
              "yes, you can call coleslaw 'slaw'",
              "no, you cannot call coleslaw 'slaw'"))

l <- leaflet() %>%
  addProviderTiles(providers$Esri.OceanBasemap)

unique(answers_slaw_plot_interactive$q89_ans) %>%
  purrr::walk( function(df) {
    l <<- l %>%
      addMarkers(data=answers_slaw_plot_interactive, lng=~long, lat=~lat,
                 label=~as.character(q89_ans), popup=~as.character(q89_ans),
                 group = df,
                 clusterOptions = markerClusterOptions())
  })

l <- l %>%
  addLayersControl(
    overlayGroups = unique(answers_slaw_plot_interactive$q89_ans),
    options = layersControlOptions(collapsed = FALSE)) %>%
  addMiniMap(tiles = providers$Esri.OceanBasemap, width = 120, height=80)

l

#######
# 3
# Encode the data so that the response is binary instead of categorical. 
# In the previous example of John and Paul, the encoded binary vectors would be 
# (1,0,0,0,0,0,1) for John and (0,1,0,0,0,0,1) for Paul. 
# (You might want to do this for the previous question as well.) 
# This makes p = 468 and n = 47,471. Experiment with dimension reduction 
# techniques. What do you see? If you do not see anything, change your 
# projection. Does that make things look different?
#######

# Make 47,471 x 468 matrix:

# Keep only columns that correspond to a question
ling_matrix <-
  ling_data[,names(ling_data)[grepl("Q{1}[[:digit:]]+", names(ling_data))]]
colnames(ling_matrix) <- sub("Q0", "Q", colnames(ling_matrix))

# Create matrix with binary encoding
# list of question numbers
q_numbers <- str_extract(names(ling_matrix), "[[:digit:]]+")
# loop through all question numbers, create a binary variable for each 
# possible answer choice. Set variable corresponding to categorical encoding
# equal to 1
for (i in 1:length(q_numbers)) {
  v = 1
  while (v <= nrow(all.ans[[q_numbers[i]]]) ) {
    ling_matrix[[paste0("Q", q_numbers[i], "_",v)]] <-
      ifelse(v == ling_matrix[[paste0("Q", q_numbers[i])]],
             1,
             0)
    v <- v + 1
  }
}
 # Get rid of original columns with categorical encoding
ling_matrix <- ling_matrix[,names(ling_matrix)[grepl("_",names(ling_matrix))]]

#################################################
# Experiment with dimension reduction techniques
#################################################

###############################
# Principal component analysis
###############################

# Perform Principal Components Analysis on matrix
ling.pca <- prcomp(ling_matrix, scale = TRUE)

# Examine principal component loading vector for first two principal components
pc <- data.frame(ling.pca$rotation[, 1:2])
pc$question <- row.names(pc)

# which variables contribute most to the first two principal components?
kable(slice(arrange(pc, desc(abs(PC1))), 1:10))

# Observe that Question 73 answers 1 and 6 contribute most to the first 
# principal component and have opposite signs.
filter(quest.use, qnum == 73)
# Q73:  What is your *general* term for the rubber-soled shoes worn in 
# gym class, for athletic activities, etc.?
all.ans[[73]]
# 45.5% answer sneakers, while 41.34 answer tennis shoes
# Observations vary along this dimension/variable
# Perhaps this spread with significant number of people choosing each answer 
# is correlated with how people will answer other questions.
# Confused by data quality:
sum(ling_matrix$Q73_1)
sum(ling_matrix$Q73_6)
# indicates that more people answered for tennis shoes, which contradicts above

tail(arrange(pc, desc(abs(PC1))))
# Q121_4 contributes least to PC1
filter(quest.use, qnum == 121)
all.ans[[121]]
# Can see a small number of people (only 3.65%) selected this
# obscure question
# choosing this answer doesn't explain how you'd answer other questions
# question that isn't very correlated

# Second PC
kable(slice(arrange(pc, desc(abs(PC2))), 1:10))
# Observe that Question 76 answers 1 and 4 and Q 103 answers 3 and 4 contribute 
# highly to the second PC
filter(quest.use, qnum == 76)
# What term do you use to refer to something that is across both streets from 
# you at an intersection (or diagonally across from you in general)?
all.ans[[76]]
# 49.53% answer kitty-corner, while 30.38  answer catty-corner
# Perhaps this spread with significant number of people choosing each answer 
# is correlated with how people will answer other questions.

# Cumulative proportion of variance explained by each PC:
cumulative_variability <- cumsum(ling.pca$sdev^2) / sum(ling.pca$sdev^2)
variability <- ling.pca$sdev^2 / sum(ling.pca$sdev^2)
kable(data.frame(`principal component` = 1:length(ling.pca$sdev),
                 `cumulative variability explained` = cumulative_variability))

# First two PCs combine to explain only 3.5% of the variance.Need 126 PCs to
# explain over 50% of variance.

#By default, the prcomp() function centers the variables to have mean zero.
#By using the option scale=TRUE, we scale the variables to have standard
#deviation one

# PCA without centering and scaling 
ling.pca_no_scale_center <- prcomp(ling_matrix, center = FALSE, scale = FALSE)
# Cumulative proportion of variance explained by each PC:
cumulative_variability_no_scale_center <-
  cumsum(ling.pca_no_scale_center$sdev^2) / sum(ling.pca_no_scale_center$sdev^2)

# PCA without scaling 
ling.pca_no_scale <- prcomp(ling_matrix, center = TRUE, scale = FALSE)
# Cumulative proportion of variance explained by each PC:
cumulative_variability_no_scale <-
  cumsum(ling.pca_no_scale$sdev^2) / sum(ling.pca_no_scale$sdev^2)

# PCA without centering
ling.pca_no_center <- prcomp(ling_matrix, center = FALSE, scale = TRUE)
# Cumulative proportion of variance explained by each PC:
cumulative_variability_no_center <-
  cumsum(ling.pca_no_center$sdev^2) / sum(ling.pca_no_center$sdev^2)

cumulative_variability[1:10]
cumulative_variability_no_scale[1:10]
cumulative_variability_no_center[1:10]
cumulative_variability_no_scale_center[1:10]
# With scaling and centering, 10 PCs only explains 11.7% of variability. 
# Without, 57.4%
# Perhaps doesn't make sense to scale with this data.

###################################################
# Plot Cumulative Proportion of Variance Explained
###################################################

# Combine Cumulative variances into 1 data frame
cumulative_var_df <-
  data_frame(scale_center = cumulative_variability,
             no_scale = cumulative_variability_no_scale,
             no_center = cumulative_variability_no_center,
             no_scale_no_center = cumulative_variability_no_scale_center)

cumulative_var_df <-
  data_frame(cumulative_var = c(cumulative_variability, 
                                cumulative_variability_no_scale,
                                cumulative_variability_no_center,
                                cumulative_variability_no_scale_center),
             pc = c(1:length(cumulative_variability), 
                    1:length(cumulative_variability),
                    1:length(cumulative_variability),
                    1:length(cumulative_variability)),
             scale_center = 
               factor(
               c(rep("scale and center", length(cumulative_variability)),
                 rep("center and no scale", length(cumulative_variability)),
                 rep("scale and no center", length(cumulative_variability)),
                 rep("no scale and no center", length(cumulative_variability))),
               levels = c("no scale and no center", "center and no scale",
                          "scale and no center", "scale and center") 
                     ))

# Plot cumulative variability explained by component
ggplot(cumulative_var_df) + 
  geom_line(aes(x = pc, y = cumulative_var, color = scale_center)) +
  xlab("Principal Component") + 
  ylab("Cumulative Proportion of Variance Explained") +
  theme_bw() +
  theme(legend.title = element_blank())

# Can clearly see that the first few principal components explain a far
# higher proportion of varaince without scaling 
# Makes sense. Variables are all measured on the same scale, so scaling and 
# centering doesn't make sense in this context

# Changing projection onto principal components using unscaled and uncentered 
# data, rather than scaled and centered data.




The figure below shows the data projected onto the first two
principal components




# Plot without scaling and centering
```{r}
# plot ling data projected onto first two PCs
ggplot() + 
  geom_point(aes(x = ling.pca_no_scale_center$x[, 1], 
                 y = ling.pca_no_scale_center$x[, 2]), alpha = 0.5)+
  ggtitle("") +
  xlab("PC1") + 
  ylab("PC2")
```

# 4.
# Use the methods we learned in class for dimension reduction and clustering 
# to try to gain insight into the full dataset. Are there any groups? 
# Do these groups relate to geography? What questions separate the groups? 
# Is there a continuum? From where to where? 
# Which questions produce this continuum? 
# Does the mathematical model behind your dimension reduction strategy 
# make sense for these clusters?

# Create binary matrix but keep lat/long variables
# Keep only columns that correspond to a question
ling_matrix_lat_long <-
  ling_data[,c(names(ling_data)[grepl("Q{1}[[:digit:]]+", names(ling_data))],
               "lat", "long")]
colnames(ling_matrix_lat_long) <- sub("Q0", "Q", colnames(ling_matrix_lat_long))

# Create matrix with binary encoding
# list of question numbers
q_numbers <- str_extract(names(ling_matrix_lat_long)[-c(68,69)], "[[:digit:]]+")
# loop through all question numbers, create a binary variable for each 
# possible answer choice. Set variable corresponding to categorical encoding
# equal to 1
for (i in 1:length(q_numbers)) {
  v = 1
  while (v <= nrow(all.ans[[q_numbers[i]]]) ) {
    ling_matrix_lat_long[[paste0("Q", q_numbers[i], "_",v)]] <-
      ifelse(v == ling_matrix_lat_long[[paste0("Q", q_numbers[i])]],
             1,
             0)
    v <- v + 1
  }
}
# Get rid of original columns with categorical encoding
ling_matrix_lat_long <- 
  ling_matrix_lat_long[,c(
    names(ling_matrix_lat_long)[grepl("_",names(ling_matrix_lat_long))], "lat", "long")]

###################
# Try on Full Data
###################

###############################
# Principal component analysis
###############################
# Use full dataset and location: 
ling_full_cluster <- 
  ling_matrix_lat_long %>%
  filter(!is.na(lat),
         # Remove observations without an answer to all questions
         rowSums(ling_matrix_lat_long[,names(ling_matrix_lat_long)[
           grepl("_{1}"
                 ,names(ling_matrix_lat_long))]]) !=0,
         # Keep only contiguous USA
         long > -125)

# Perform Principal Components Analysis

ling_full.pca <- prcomp(select(ling_full_cluster,-c(lat,long)), scale = TRUE)

# Examine principal component loading vector for first two principal components
# to see which variables contribute most to the first two principal components?
pc_full <- data.frame(ling_full.pca$rotation[, 1:2])
pc_full$var <- row.names(pc_full)

kable(arrange(pc_full, desc(abs(PC1))))
kable(arrange(pc_full, desc(abs(PC2))))
# Question 73 answers 1 and 6  contribute
# highly to the first principal component.
all.ans[[73]]

# what is the cumulative proportion of variance explained by each PC?
cumulative_variability_full <-
  cumsum(ling_full.pca$sdev^2) / sum(ling_full.pca$sdev^2)
kable(data.frame(`principal component` = 1:ncol(ling_full.pca$x),
                 `cumulative variability explained` = 
                   cumulative_variability_full))

#The figure below shows the data projected onto the first two
#principal components

# plot data projected onto first two PCs
ggplot() + 
  geom_point(aes(x = ling_full.pca$x[, 1],y = ling_full.pca$x[, 2]
  ),alpha = 0.2) +
  ggtitle("") +
  xlab("PC1") + 
  ylab("PC2")

# Tough to see how many clusters

# Use Elbow Method to determine appropriate number of clusters:

# examine proportion of variance explained as a function of 
# the number of clusters, Look for a drop in the proportion of variance explained
# by adding an additional cluster.

# Elbow Method for finding the optimal number of clusters
set.seed(2)
# Compute and plot wss for k = 2 to k = 10.
k.max <- 10
data <- ling_full.pca$x[, 1:2]
wss <- 
  sapply(1:k.max, 
         function(k){kmeans(data, k, nstart=50,iter.max = 15)$tot.withinss})

elbow = data.frame(cluster = 1:k.max, wss = wss)

ggplot(elbow, aes(x=cluster, y=wss/100)) + 
  geom_point(color = "blue") +  
  geom_line(color = "blue") +
  labs(title = "Elbow Plot", 
  x = "Number of Clusters", 
  y = "Total Within-Clusters Sum of Squares",
  caption = "Total within-clusters sum of squares divided by 100") +
  scale_x_continuous(breaks=seq(from=1, to=10, by= 1)) +
  geom_vline(xintercept = 3, linetype = 2, color = "steelblue") +
  theme_bw() +
  theme(plot.caption = element_text(size = 7))
  
# Can see elbow at k = 3, so choose 3 clusters

# cluster data in original and PC spaces and plot along first two PCs
# set seed to results reproducible:
set.seed(2)
k <- 3
# Use nstart = 20 to run clustering multiple times with multiple 
# initial cluster assignments to avoid obtaining a local optimum
# k-means using all variables
kmeans.raw_full_3 <- kmeans(scale(ling_full_cluster), centers = k, 
                           nstart = 20)
# spectral clustering using first 2 pc only
kmeans.reduced_full_3 <- kmeans(ling_full.pca$x[, 1:2], centers = k, nstart = 20)

# plot kmeans results
p.raw_full_3 <- plotLabeledData(ling_full.pca$x[, 1], ling_full.pca$x[, 2],
                               labels = as.factor(kmeans.raw_full_3$cluster)) +
  ggtitle("Clusters based on raw data") +
  xlab("PC1") + 
  ylab("PC2")

# plot spectral clustering
p.reduced_full_3 <- plotLabeledData(ling_full.pca$x[, 1], ling_full.pca$x[, 2], 
                                   labels = as.factor(kmeans.reduced_full_3$cluster))  +
  ggtitle("Clusters based on reduced dataset") +
  xlab("PC1") + 
  ylab("PC2")
grid.arrange(p.raw_full_3, p.reduced_full_3, ncol=2) 

# Join clusters back into original data:
ling_full_cluster_with <-
  cbind(ling_full_cluster, data.frame(cluster = kmeans.reduced_full_3$cluster))

# Plot on map
cluster_plot <-
  ggplot(ling_full_cluster_with) +
  geom_polygon(aes(x = long, y = lat, group = group),
               data = state_df, colour = "black", fill = NA) + 
  coord_fixed(1.3) +
  geom_point(aes(x = long, y = lat, color = as.factor(cluster)), 
             size = 3, alpha = 0.2) + 
  theme_bw() +
  theme(plot.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank())

cluster_plot

# Clear geographic relationship for clusters
# We thought data was in clusters by region and this clustering appears to have
# confirmed this.
# With these questions and choosing 3 clusters, we see that the data breaks up
# geographically into 3 regions: the south, north east, and the rest of the USA.


# 5. Choose one of your interesting findings. Analyze and discuss the 
# robustness of the finding. What happens when you perturb the data set? 
# Different starting points? What can be generalized from this finding?

# Finding: variability in the data based on geography.
cluster_plot

# Different starting points:

# Use nstart = 20 to run clustering multiple times with multiple 
# initial cluster assignments to avoid obtaining a local optimum
# k-means using all variables
# The k-means clustering algorithm finds a local optimum, not a global optimum,
# so the results of the algorithm depend on the random initial cluster
# assignments. Try a few. See if noticable change in results 

# see % same cluster

# Mention more robust to use large number for nstart

# Try 3 different

set.seed(3)
kmeans.reduced_seed3 <- kmeans(ling_full.pca$x[, 1:2], centers = 3)
set.seed(4)
kmeans.reduced_seed4 <- kmeans(ling_full.pca$x[, 1:2], centers = 3)
set.seed(5)
kmeans.reduced_seed5 <- kmeans(ling_full.pca$x[, 1:2], centers = 3)

ling_full_cluster_with_seeds <-
  cbind(ling_full_cluster_with, 
        data.frame(cluster3 = kmeans.reduced_seed3$cluster,
                   cluster4 = kmeans.reduced_seed4$cluster,
                   cluster5 = kmeans.reduced_seed5$cluster))

# By examination, it is clear that cluster 1 from the original clustering 
# and with seed = 3 is equivalent to cluster 2 from the clusterings
# with seed = 4 and seed = 5. Additionally, cluster 2 = cluster 3 and
# cluster 3 = cluster 1

ling_full_cluster_with_seeds$cluster4 <-
  with(ling_full_cluster_with_seeds,
       ifelse(cluster4 == 2,
              1,
              ifelse(cluster4 == 3,
                     2, 3)))

ling_full_cluster_with_seeds$cluster5 <-
  with(ling_full_cluster_with_seeds,
       ifelse(cluster5 == 2,
              1,
              ifelse(cluster5 == 3,
                     2, 3)))

# Plot different clustrings on map

# with seed = 3
cluster_plot3 <-
  ggplot(ling_full_cluster_with_seeds) +
  geom_polygon(aes(x = long, y = lat, group = group),
               data = state_df, colour = "black", fill = NA) + 
  coord_fixed(1.3) +
  geom_point(aes(x = long, y = lat, color = as.factor(cluster3)), 
             size = 3, alpha = 0.2) + 
  theme_bw() +
  theme(plot.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank())

# with seed = 4
cluster_plot4 <-
  ggplot(ling_full_cluster_with_seeds) +
  geom_polygon(aes(x = long, y = lat, group = group),
               data = state_df, colour = "black", fill = NA) + 
  coord_fixed(1.3) +
  geom_point(aes(x = long, y = lat, color = as.factor(cluster4)), 
             size = 3, alpha = 0.2) + 
  theme_bw() +
  theme(plot.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank())

# with seed = 5
cluster_plot5 <-
  ggplot(ling_full_cluster_with_seeds) +
  geom_polygon(aes(x = long, y = lat, group = group),
               data = state_df, colour = "black", fill = NA) + 
  coord_fixed(1.3) +
  geom_point(aes(x = long, y = lat, color = as.factor(cluster5)), 
             size = 3, alpha = 0.2) + 
  theme_bw() +
  theme(plot.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank())

grid.arrange(cluster_plot, cluster_plot3, cluster_plot4, cluster_plot5, 
             nrow = 2)

# Visually from maps, tough to see any differences. If there are any, they do
# not appear to be substantial.

# Calculate number of observations for which cluster assignment differs.

ling_full_cluster_with_seeds$cluster3_diff <-
  with(ling_full_cluster_with_seeds,
       ifelse(cluster3 == cluster,
              0, 1))

ling_full_cluster_with_seeds$cluster4_diff <-
  with(ling_full_cluster_with_seeds,
       ifelse(cluster4 == cluster,
              0, 1))

ling_full_cluster_with_seeds$cluster5_diff <-
  with(ling_full_cluster_with_seeds,
       ifelse(cluster5 == cluster,
              0, 1))

# Check how many observations assigned different clusters:
sum(ling_full_cluster_with_seeds$cluster3_diff)
sum(ling_full_cluster_with_seeds$cluster4_diff)
sum(ling_full_cluster_with_seeds$cluster5_diff)

# For all 3 random seeds, cluster assignments exact same for all observations.

######################
# Perturb the data set
######################

# Take 10% of the data and randomly assign answers among possible answer 
# choices
# Then do everything same and examine final results/clusters, compare to 
# original

# Randomly sample observations to alter:
set.seed(1)
obs_to_alter <- sample(1:nrow(ling_data),
                       # take sample of 10% original
                       round(nrow(ling_data)/10,0))

# split data into data to alter and preserve
data_preserve <- ling_data[-obs_to_alter,]
data_alter <- ling_data[obs_to_alter,] 

# Get question numbers
q_numbers_alter <- 
      str_extract(names(data_alter)[grepl("Q{1}[[:digit:]]+",
                                         names(data_alter))], "[[:digit:]]+")

# Randomize answers for data to alter for each question
for (q in 1:length(q_numbers_alter)) {
  data_alter[[paste0("Q", q_numbers_alter[q])]] <-
    sample(1:nrow(all.ans[[as.numeric(q_numbers_alter[q])]]), nrow(data_alter),
           replace = T)
}

# Combine preserved and altered data

ling_data_alter_preserve <-
  rbind(data_alter, data_preserve)

 # Run same analysis but with altered data

# Keep only columns that correspond to a question
alter_matrix <-
  ling_data_alter_preserve[,
      c(names(ling_data_alter_preserve)[grepl("Q{1}[[:digit:]]+", 
                            names(ling_data_alter_preserve))], "lat", "long")]
colnames(alter_matrix) <- sub("Q0", "Q", colnames(alter_matrix))

# Create matrix with binary encoding
# list of question numbers
q_numbers <- str_extract(names(alter_matrix)[-c(68,69)], "[[:digit:]]+")
# loop through all question numbers, create a binary variable for each 
# possible answer choice. Set variable corresponding to categorical encoding
# equal to 1
for (i in 1:length(q_numbers)) {
  v = 1
  while (v <= nrow(all.ans[[q_numbers[i]]]) ) {
    alter_matrix[[paste0("Q", q_numbers[i], "_",v)]] <-
      ifelse(v == alter_matrix[[paste0("Q", q_numbers[i])]],
             1,
             0)
    v <- v + 1
  }
}
# Get rid of original columns with categorical encoding
alter_matrix <- 
  alter_matrix[,c(
    names(alter_matrix)[grepl("_",names(alter_matrix))], "lat", "long")]

###################
# Try on Full Data
###################

###############################
# Principal component analysis
###############################
# Use full dataset and location: 
ling_alter_cluster <- 
  alter_matrix %>%
  filter(!is.na(lat),
         # Remove observations without an answer to all questions
         rowSums(alter_matrix[,names(alter_matrix)[
           grepl("_{1}"
                 ,names(alter_matrix))]]) !=0,
         # Keep only contiguous USA
         long > -125)

# Perform Principal Components Analysis

ling_alter.pca <- prcomp(select(ling_alter_cluster,-c(lat,long)), scale = TRUE)

# cluster data in original and PC spaces and plot along first two PCs
# set seed to results reproducible:
set.seed(2)
k <- 3
# Use nstart = 20 to run clustering multiple times with multiple 
# initial cluster assignments to avoid obtaining a local optimum
# k-means using all variables
# spectral clustering using first 2 pc only
kmeans.reduced_alter_3 <- kmeans(ling_alter.pca$x[, 1:2], centers = k, nstart = 20)

# Join clusters back into original data:
ling_alter_cluster_with <-
  cbind(ling_alter_cluster, 
        data.frame(cluster = kmeans.reduced_alter_3$cluster))

# Plot on map
cluster_plot_alter <-
  ggplot(ling_alter_cluster_with) +
  geom_polygon(aes(x = long, y = lat, group = group),
               data = state_df, colour = "black", fill = NA) + 
  coord_fixed(1.3) +
  geom_point(aes(x = long, y = lat, color = as.factor(cluster)), 
             size = 3, alpha = 0.2) + 
  theme_bw() +
  theme(plot.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank())

grid.arrange(cluster_plot, cluster_plot_alter)

# Can see major changes. Only north east cluster seems to have been preserved.

# Not robust to this large of a pertubation to data.














