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


kable(table(select(q50_q89, q50_ans, q89_ans ))) %>%
  kable_styling(bootstrap_options = "striped", full_width = F, 
                position = "center")

kable(dt) %>%
  kable_styling(bootstrap_options = c("striped", "hover"),f ull_width = F)

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

###########################################
# Start by examining a subset of questions
###########################################
50 What word(s) do you use to address a group of two or more people?
66 What do you call the miniature lobster that one finds in lakes and streams for example (a crustacean of the family Astacidae)?
105 What is your generic term for a sweetened carbonated beverage?
84 What do you call a traffic situation in which several roads meet in a circle and you have to get off at a certain point?
103 What do you call the thing from which you might drink water in a school?
73 What is your *general* term for the rubber-soled shoes worn in gym class, for athletic activities, etc.?
79 What is your *general* term for a big road that you drive relatively fast on? 
118 What do you call a drive-through liquor store?
61 What do you call the area of grass that occurs in the middle of some streets?
110 What do you call the night before Halloween?
104 What do you call a public railway system (normally underground)?
###############################
# Principal component analysis
###############################
# Use dataset with subset of questions and location: 
ling_sub_cluster <- 
  ling_matrix_lat_long %>%
  filter(!is.na(lat),
         # Remove observations without an answer to all of these questions
         rowSums(ling_matrix_lat_long[,names(ling_matrix_lat_long)[
           grepl("(50|61|66|73|79|84|103|104|105|110|118){1}"
                 ,names(ling_matrix_lat_long))]]) !=0,
         # Keep only contiguous USA
         long > -125) %>%
  select(names(ling_matrix_lat_long)[
    grepl("(50|61|66|73|79|84|103|104|105|110|118){1}",
          names(ling_matrix_lat_long))], lat, long)

# Perform Principal Components Analysis

ling_sub.pca <- prcomp(select(ling_sub_cluster,-c(lat,long)), scale = TRUE)

# Examine principal component loading vector for first two principal components
# to see which variables contribute most to the first two principal components?
pc_sub <- data.frame(ling_sub.pca$rotation[, 1:2])
pc_sub$var <- row.names(pc_sub)

kable(arrange(pc_sub, desc(abs(PC1))))
kable(arrange(pc_sub, desc(abs(PC2))))
# Question 73 answers 1 and 6 and question 105 answers 1 and 2 contribute
# highly to the first principal component.
all.ans[[73]]
all.ans[[105]]

```{r}
# what is the cumulative proportion of variance explained by each PC?
cumulative_variability_sub <-
  cumsum(ling_sub.pca$sdev^2) / sum(ling_sub.pca$sdev^2)
kable(data.frame(`principal component` = 1:ncol(ling_sub.pca$x),
                 `cumulative variability explained` = 
                   cumulative_variability_sub))
```
The figure below shows the data projected onto the first two
principal components We see that the data cluster nicely with respect to cultivar in this space.

```{r}
# plot data projected onto first two PCs
ggplot() + 
  geom_point(aes(x = ling_sub.pca$x[, 1],y = ling_sub.pca$x[, 2]
                 ),alpha = 0.2) +
  ggtitle("") +
  xlab("PC1") + 
  ylab("PC2")
```
# Tough to see how many clusters

# cluster data in original and PC spaces and plot along first two PCs
# set seed to results reproducible:
set.seed(2)
k <- 3
# Use nstart = 20 to run clustering multiple times with multiple 
# initial cluster assignments to avoid obtaining a local optimum
# k-means using all variables
kmeans.raw_sub_3 <- kmeans(scale(ling_sub_cluster), centers = k, 
                         nstart = 20)
# spectral clustering using first 2 pc only
kmeans.reduced_sub_3 <- kmeans(ling_sub.pca$x[, 1:2], centers = k, nstart = 20)

# plot kmeans results
p.raw_sub_3 <- plotLabeledData(ling_sub.pca$x[, 1], ling_sub.pca$x[, 2],
                         labels = as.factor(kmeans.raw_sub_3$cluster)) +
  ggtitle("Clusters based on raw data") +
  xlab("PC1") + 
  ylab("PC2")

# plot spectral clustering
p.reduced_sub_3 <- plotLabeledData(ling_sub.pca$x[, 1], ling_sub.pca$x[, 2], 
                             labels = as.factor(kmeans.reduced_sub_3$cluster))  +
  ggtitle("Clusters based on reduced dataset") +
  xlab("PC1") + 
  ylab("PC2")
grid.arrange(p.raw_sub_3, p.reduced_sub_3, ncol=2) 

# Join clusters back into original data:
ling_sub_cluster_with <-
  cbind(ling_sub_cluster, data.frame(cluster = kmeans.reduced_sub_3$cluster))

# Plot on map
cluster_plot <-
  ggplot(ling_sub_cluster_with) +
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

# We thought data was in clusters by region and this clustering appears to have
# confirmed this.
# With these questions and choosing 3 clusters, we see that the data breaks up
# geographically into 3 regions: the south, north east, and the rest of the USA.

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

```{r}
# what is the cumulative proportion of variance explained by each PC?
cumulative_variability_full <-
  cumsum(ling_full.pca$sdev^2) / sum(ling_full.pca$sdev^2)
kable(data.frame(`principal component` = 1:ncol(ling_full.pca$x),
                 `cumulative variability explained` = 
                   cumulative_variability_full))
```
The figure below shows the data projected onto the first two
principal components We see that the data cluster nicely with respect to cultivar in this space.

```{r}
# plot data projected onto first two PCs
ggplot() + 
  geom_point(aes(x = ling_full.pca$x[, 1],y = ling_full.pca$x[, 2]
  ),alpha = 0.2) +
  ggtitle("") +
  xlab("PC1") + 
  ylab("PC2")
```
# Tough to see how many clusters

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


# Clear geographic relationship for clusters
# We thought data was in clusters by region and this clustering appears to have
# confirmed this.
# With these questions and choosing 3 clusters, we see that the data breaks up
# geographically into 3 regions: the south, north east, and the rest of the USA.
















# Clustering with various $k$: stability

# Typically the number of clusters is unknown and we need some way to evaluate
# the quality of a clustering. One useful metric for this is cluster stability: 
# do we recover similar clusters by changing part of our analysis 
# (e.g. using different starting points for algorithms that converge to local 
# optima, perturbing variables). We will cover more formal metrics for stability
# later in the course. For now, we will use visualization to examine cluster 
# stability. Try using the k-means algorithm to cluster data with different 
# values of k. For each k run the algorithm a few times
using different starting points (the kmeans function uses random starting
                                 points by default) and plot your results. 
What values of $k$ lead to stable
clusterings? 
  
  
  ```{r k3, message=FALSE, fig.cap="Cluster labels using k=3"}
# run spectral clustering 4 times with 3 centers
kmeans.3 <- lapply(1:4, function(k) {
  kmeans(ling_you.pca$x[,1:2], centers = 3)
})
# plot the resulting clusters from each run of spectral clustering
clusters <- lapply(kmeans.3, function(km) {
  p <- plotLabeledData(ling_you.pca$x[, 1], ling_you.pca$x[, 2],
                       labels = as.factor(km$cluster))
  p <- p + theme(legend.position = "none") +
    xlab("PC1") +
    ylab("PC2")
})
grid.arrange(clusters[[1]], clusters[[2]],
             clusters[[3]], clusters[[4]],
             ncol = 2)
```

```{r k10, message=FALSE, fig.cap="Cluster labels using k=10"}
# re-run with 10 clusters
kmeans.10 <- lapply(1:4, function(k) {
  kmeans(ling_you.pca$x[ ,1:2], centers = 10)
})
# plot the 4 runs of spectral clustering with 10 centers
clusters <- lapply(kmeans.10, function(km) {
  p <- plotLabeledData(ling_you.pca$x[, 1], ling_you.pca$x[, 2],
                       labels = as.factor(km$cluster))
  p <- p + theme(legend.position = "none") +
    xlab("PC1") +
    ylab("PC2")
})
grid.arrange(clusters[[1]], clusters[[2]], clusters[[3]], clusters[[4]], ncol=2)
```


# Silhouettes

#Intuitively, we expect good clusters to contain points that are close to one
#another and far from points in other clusters. Silhouettes are one way to
#measure this. Given a distance matrix and set of cluster labels, R's silhouette
#function in the package cluster calculates:


$$s_{i}=\frac{b_{i}-a_{i}}{\max(a_{i}, b_{i})}$$
  
  
  where $a_{i}=\sum_{j\in C_{i}} d(x_{i}, x_{j})$ is the average distance between
$x_i$ and all other points belonging to the same cluster $C_{i}$ and
$b{i}=\min_{C_k} \sum_{j\in C_{k}} d(x_{i}, x_{j})$ for $k\ne i$ is the average
dissimilarity between $x_{i}$ and the nearest cluster to which $x_{i}$ does not
belong. The figure below shows an example with $k=3$ (average silhouette width of 0.56). Try
experimenting with various $k$ and plot the resulting silhouettes.

```{r silhouette3, message=FALSE, fig.height=8}
# calculate a distance matrix based on the first 2 principal components
# distance matrix too large to calculate silhouette
# Use sample of principal component loading vector for first two principal 
# components
pca_sample <- sample_n(data.frame(ling_you.pca$x[, 1:2]),5000, replace = TRUE)

kmeans3 <- kmeans(pca_sample, centers=3, nstart = 20)
dist.mat <- dist(pca_sample)
#rm(ling_matrix, ling_location, ling_matrix_lat_long, ling_you_cluster, 
 #  ling_you_cluster_with, ling_you_cluster_with_raw, ling_data, ling_data_long_lat)
s <- silhouette(kmeans3$cluster, dist.mat)
# average silhouette width for k = 3 is 0.3904678
mean(s[, 3])
#plot(s)
```
# Compare to k=4
# Setting k = 4, we get an average silhouette width 0.6715964
kmeans2 <- kmeans(pca_sample, centers=2, nstart = 20)
s <- silhouette(kmeans2$cluster, dist.mat)
# average silhouette width similar for k = 3 and k = 4, 
# so we'll use less clusters (k=3)


mean(s[, 2])
#plot(s)
```

#You can probably make much prettier plots than these by calculating silhouette width manually and using ggplot! These plots have terrible headings and are just generally unsatisfactory.
s <- silhouette(kmeans.reduced_you_3$cluster, dist.mat)
s_test = data.frame(s[,1:3])
s_test = arrange(s_test, cluster, sil_width)



# Use position=position_dodge()
s_test$row_name <- row.names(s_test)

s_test <- within(s_test, 
                 row_name <- factor(row_name, 
                                    levels=row.names(s_test)))

ggplot(data=s_test, aes(x=cluster, y=sil_width, fill=row_name)) +
  geom_bar(stat="identity", position=position_dodge()) +
  theme(legend.position="none", panel.background = 
          element_rect(fill = "white")) +
  scale_fill_manual(values= c(rep("blue",nrow(filter(s_test, cluster==1 ))),
                              rep("red",nrow(filter(s_test, cluster==2 ))),
                              rep("green",nrow(filter(s_test, cluster==3 )))))+
  coord_flip()

#############
# Full Data:
#############

```
# Principal component analysis

First, we must calculate the principal components.
###############################
# Principal component analysis
###############################
# Use dataset with lat/long and answers to questions: 
ling_full_cluster <- 
  ling_matrix_lat_long %>%
  filter(!is.na(lat),
         # Remove observations without answers to any questions
         rowSums(ling_matrix_lat_long[,names(ling_matrix_lat_long)[
           grepl("_",names(ling_matrix_lat_long))]]) !=0,
         # Keep only contiguous USA
         long > -125)



# Perform Principal Components Analysis
ling_full.pca <- prcomp(ling_full_cluster, scale = FALSE, center = FALSE)
# Scale/center or don't? Looks like if don't, long/lat is all that matters

We can look to see the variable loadings for each variable on the first two principal components.

```{r}
# which variables contribute most to the first two principal components?
pc_full <- data.frame(ling_full.pca$rotation[, 1:2])
pc_full$var <- row.names(pc_full)

kable(arrange(pc_full, desc(abs(PC1))))
kable(arrange(pc_full, desc(abs(PC2))))
# 

```{r true-labels, message=FALSE}

Looking at the cumulative variability, not clear how many PCs to use.
Seems like each of first 10 provide value in explaining variability of data
```{r}
# what is the cumulative proportion of variance explained by each PC?
cumulative_variability_full <-
  cumsum(ling_full.pca$sdev^2) / sum(ling_full.pca$sdev^2)
kable(data.frame(`principal component` = 1:11,
                 `cumulative variability explained` = 
                   cumulative_variability_full))
```
The figure below shows the data projected onto the first two
principal components We see that the data cluster nicely with respect to cultivar in this space.

```{r}
# plot data projected onto first two PCs and compare across cultivar
ggplot() + 
  geom_point(aes(x = ling_full.pca$x[, 1],y = ling_full.pca$x[, 2]
                 #, color = ling_full_cluster$ans
  ),alpha = 0.5) +
  ggtitle("") +
  xlab("PC1") + 
  ylab("PC2")
```
#looks like 3 or 4 clusters


# Clustering in raw and reduced spaces



```{r raw-labels, message=FALSE, fig.cap="Clusterings from raw (left) and reduced (right) data"}

# cluster data in original and PC spaces and plot along first two PCs
# set seed to results reproducible:
set.seed(2)
k <- 3
# Use nstart = 20 to run clustering multiple times with multiple 
# initial cluster assignments to avoid obtaining a local optimum
# k-means using all variables
kmeans.raw_full_3 <- kmeans(scale(select(ling_full_cluster,-ans)), centers = k, 
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
```


# Clustering with various $k$: stability

For the previous example we set $k=3$. In some sense, we cheated using the known
number of cultivars to choose $k$. Typically the number of clusters is unknown
and we need some way to evaluate the quality of a clustering. One useful metric
for this is cluster stability: do we recover similar clusters by changing part
of our analysis (e.g. using different starting points for algorithms that
                 converge to local optima, perturbing variables). We will cover more formal
metrics for stability later in the course. For now, we will use visualization to
examine cluster stability. Try using the $k-$means algorithm to cluster the wine
data with different values of $k$. For each $k$ run the algorithm a few times
using different starting points (the kmeans function uses random starting
                                 points by default) and plot your results. What values of $k$ lead to stable
clusterings? 
  
  
  ```{r k3, message=FALSE, fig.cap="Cluster labels using k=3"}
# # run spectral clustering 4 times with 3 centers
# kmeans.3 <- lapply(1:4, function(k) {
#   kmeans(ling_full.pca$x[,1:2], centers = 3)
# })
# # plot the resulting clusters from each run of spectral clustering
# clusters <- lapply(kmeans.3, function(km) {
#   p <- plotLabeledData(ling_full.pca$x[, 1], ling_full.pca$x[, 2],
#                        labels = as.factor(km$cluster))
#   p <- p + theme(legend.position = "none") +
#     xlab("PC1") + 
#     ylab("PC2")
# })
# grid.arrange(clusters[[1]], clusters[[2]], 
#              clusters[[3]], clusters[[4]], 
#              ncol = 2)
# ```
# 
# ```{r k10, message=FALSE, fig.cap="Cluster labels using k=10"}
# # re-run with 10 clusters
# kmeans.10 <- lapply(1:4, function(k) {
#   kmeans(ling_full.pca$x[ ,1:2], centers = 10)
# })
# # plot the 4 runs of spectral clustering with 10 centers
# clusters <- lapply(kmeans.10, function(km) {
#   p <- plotLabeledData(ling_full.pca$x[, 1], ling_full.pca$x[, 2],
#                        labels = as.factor(km$cluster))
#   p <- p + theme(legend.position = "none") +
#     xlab("PC1") + 
#     ylab("PC2")
# })
# grid.arrange(clusters[[1]], clusters[[2]], clusters[[3]], clusters[[4]], ncol=2)
```


# Silhouettes

#Intuitively, we expect good clusters to contain points that are close to one
#another and far from points in other clusters. Silhouettes are one way to
#measure this. Given a distance matrix and set of cluster labels, R's silhouette
#function in the package cluster calculates:


$$s_{i}=\frac{b_{i}-a_{i}}{\max(a_{i}, b_{i})}$$
  
  
  where $a_{i}=\sum_{j\in C_{i}} d(x_{i}, x_{j})$ is the average distance between
$x_i$ and all other points belonging to the same cluster $C_{i}$ and
$b{i}=\min_{C_k} \sum_{j\in C_{k}} d(x_{i}, x_{j})$ for $k\ne i$ is the average
dissimilarity between $x_{i}$ and the nearest cluster to which $x_{i}$ does not
belong. The figure below shows an example with $k=3$ (average silhouette width of 0.56). Try
experimenting with various $k$ and plot the resulting silhouettes.

```{r silhouette3, message=FALSE, fig.height=8}
# calculate a distance matrix based on the first 2 principal components

dist.mat <- dist(ling_full.pca$x[, 1:2])
s <- silhouette(kmeans.reduced_full_3$cluster, dist.mat)
# average silhouette width for k = 3 is 0.6735773
mean(s[, 3])
#plot(s)
```
# Compare to k=4
# Setting k = 4, we get an average silhouette width 0.6715964
kmeans4 <- kmeans(ling_full.pca$x[, 1:2], centers=4, nstart = 20)
s <- silhouette(kmeans4$cluster, dist.mat)
# average silhouette width similar for k = 3 and k = 4, 
# so we'll use less clusters (k=3)


mean(s[, 3])
#plot(s)
```

#You can probably make much prettier plots than these by calculating silhouette width manually and using ggplot! These plots have terrible headings and are just generally unsatisfactory.
s <- silhouette(kmeans.reduced_full_3$cluster, dist.mat)
s_test = data.frame(s[,1:3])
s_test = arrange(s_test, cluster, sil_width)



# Use position=position_dodge()
s_test$row_name <- row.names(s_test)

s_test <- within(s_test, 
                 row_name <- factor(row_name, 
                                    levels=row.names(s_test)))

ggplot(data=s_test, aes(x=cluster, y=sil_width, fill=row_name)) +
  geom_bar(stat="identity", position=position_dodge()) +
  theme(legend.position="none", panel.background = 
          element_rect(fill = "white")) +
  scale_fill_manual(values= c(rep("blue",nrow(filter(s_test, cluster==1 ))),
                              rep("red",nrow(filter(s_test, cluster==2 ))),
                              rep("green",nrow(filter(s_test, cluster==3 )))))+
  coord_flip()

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
             size = 3, alpha = 0.5) + 
  blank_theme

# Compare to map by answers:
answer_plot <- 
  ggplot(ling_full_cluster_with) +
  geom_polygon(aes(x = long, y = lat, group = group),
               data = state_df, colour = "black", fill = NA) + 
  coord_fixed(1.3) +
  geom_point(aes(x = long, y = lat, color = ans), 
             size = 3, alpha = 0.5) + 
  blank_theme

grid.arrange(cluster_plot, answer_plot)

# We thought data was in clusters by region and this clustering appears to have
# confirmed this






