# Functions for plotting the data

kernelDensityTemp <- function(df_plot = sonoma_all_df_plot, 
                              kernel_type = "gaussian", bandwidth = 1) {
  # Arguments:
  #   df_plot: data frame to use for Kernal Density plot.
  #
  #   kernel_type: a character string giving the smoothing kernel to be
  #   used. This must partially match one of "gaussian", "rectangular", 
  #   "triangular", "epanechnikov", "biweight", "cosine" or "optcosine",
  #   with default "gaussian"
  #
  #   bandwidth: the smoothing bandwidth to be used. The kernels 
  #   scaled such that this is the standard deviation of the smoothing
  #   kernel. Default is 1.
  #
  # Returns:
  #   A kernel density plot for the distribution of temperature
  kd_plot <- ggplot(df_plot, aes(humid_temp)) +
    stat_density(bw = bandwidth, color="darkblue", fill="lightblue", 
                 kernel = kernel_type, alpha=0.8) +
    labs(title= paste0(str_to_title(kernel_type)," Kernel Density Plot\n", 
                       "with Bandwidth = ", bandwidth), x="Temperature (°C)", 
         y = "Density")+
    theme_bw() +
    ylim(0, 0.11) +
    theme(plot.title = element_text(size = 10), 
          axis.title.x = element_text(size = 8),
          axis.title.y = element_text(size = 8))
  
  return(kd_plot)
}


loessTimeTemp <- function(df_plot = sonoma_all_df_plot_time, 
                              span_in = 1, degree_poly = 1) {
  # Arguments:
  #   df_plot: data frame to use for plot.
  #
  #   span_in: controls the amount of smoothing for the default loess smoother. 
  #   Smaller numbers produce wigglier lines, larger numbers produce smoother 
  #   lines. Default is 1.
  #
  #   degree_poly: degree of polynomial to use in smoothing function. 
  #   Default is 1.
  #
  # Returns:
  #   A plot of temperature against humidity with an loess smoother,
  loess_plot <- 
    ggplot(sonoma_all_df_plot_time, aes(x = humid_temp, y = humidity)) +
    geom_point(alpha = 0.5) +
    ggtitle(paste0("Temperature v. Humidity\n","loess span: ", span_in,
    ", degree: ", degree_poly)) +
    xlab("Temperature (°C)") + 
    ylab("Humidity (%RH)") +
    theme_bw() +
    geom_smooth(method = "loess", size = 1, se = FALSE, span = span_in, 
                formula = y ~ poly(x, degree_poly)) +
    theme(plot.title = element_text(size = 9), 
          axis.title.x = element_text(size = 8),
          axis.title.y = element_text(size = 8))
  
  return(loess_plot)
}

# Map plot settings
  map_settings <- 
    theme_bw() +
    theme(plot.background = element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.border = element_blank(),
          legend.position = "bottom",
          legend.justification="left",
          legend.direction = "vertical",
          legend.margin= margin(-10,0,0,0),
          legend.box.margin=margin(-15,-10,-5,-10),
          legend.title=element_blank(), 
          legend.text= element_text(size=7),
          plot.title = element_text(size=8, hjust = 0.5, 
                                    margin=margin(0,0,-8,0)),
          axis.title = element_blank(),
          axis.text = element_blank(),
          axis.ticks = element_blank())
  
  plotLabeledData <- function(x, y, labels=NULL) {
    # Plot labeled data along two axes
    # Args:
    #   x: observation values for the first axis to plot over
    #   y: observation values for the second axis to plot over
    #   labels: factor giving the label of each point
    
    plotting.data <- data.frame(X = x, Y = y, label = labels)
    p <- ggplot(plotting.data) + 
      geom_point(aes(x = X, y = Y, color = label))
    return(p)
  }


