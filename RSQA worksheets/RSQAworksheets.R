# RSQA Worksheets
## Simone's Solutions

# Do First #####################################################################

# Load packages

  #For plotting
    library(dplyr)
      # Includes piping
    library(ggplot2)
      # Includes plotting
    library(readr)
      # Includes read.csv() function for loading data

  # For mapping
    library(rnaturalearth)
      # Had to install with devtools::install_github("ropenscilabs/rnaturalearth")
    library(rnaturalearthdata)
      # After loading, said masked by the package rnaturalearth
    library(ggspatial)
    library(rgeos)

  # For statistical tests
    library(ggpubr)

  # For multivariable analysis
    library(tidyr)
    library(pheatmap)
    library(ggbiplot)

# Loading and Building Data

  setwd("/Users/simone/Documents/UT/UrbanEco/Datamining/RSQA worksheets/Mydata")
    # Set working directory
  mydata <- read.csv("Results.csv")
    # California pesticide data I downloaded from RSQA
  sites <- read.csv("Sites.csv")
    # Site data accompanying california pesticide data
  
    # Data built from mydata
  
      expcount <- mydata %>% group_by(PARM_NM) %>% summarize(count=n())
        # Tibble describes how many observations were made for each parameter
      expcount <- expcount[expcount$count > 50,]
        # Removes parameters with less than 50 observations from the tibble
      expcount <- expcount[order(expcount$count, decreasing = T),]
        # Orders tibble rows from greatest to least amount of observations
    
      imidacloprid_data <- mydata[mydata$PARM_NM == "Imidacloprid, wf",]
        # Contains data only for imidacloprid
    
      top_5_data <- mydata[mydata$PARM_NM %in% expcount$PARM_NM[1:5],]
        # Data set containing the 5 parameters with the most observations
    
      top_5_data_with_locations <- left_join(top_5_data, sites)
        # Add location information to top_5_data
    
    # Mapping data
      
      world <- ne_countries(scale="medium", returnclass = "sf")
      # Data frame containing shape of the countries we want to map
    
  setwd("/Users/simone/Documents/UT/UrbanEco/Datamining/RSQA worksheets/Replicating Graphs")
    # Set working directory for replicating graphs
    
  results2 <- read.csv("Results.csv")
    # Results downloaded from RSQA worksheet for replicating graphs
  sites2 <- read.csv("Sites.csv")
    # Accompanying site data
    
    # Data built from replicating graphs data
    
      pyrene_data <- results2[results2$PARM_NM == "Pyrene, solids",] %>% group_by(SITE_NO) %>% arrange(RESULT_VA) %>% slice(n=1) %>% ungroup()
        # Make data set with max pyrene concentration for each site
      pyrene_data_with_locations <- left_join(pyrene_data, sites2)
        # Add location information to pyrene data
      
      myclobutanil_data <- results2[results2$PARM_NM == "Myclobutanil, wf",] %>% group_by(SITE_NO) %>% arrange(RESULT_VA) %>% slice(1) %>% ungroup()
        # Make data set with max myclobutanil concentration for each site
      myclobutanil_data_with_locations <- left_join(myclobutanil_data, sites2)
        # Add location information to myclobutanil data
      
      ampa_data <- results2[results2$PARM_NM == "AMPA, w,gf<0.7u",] %>% group_by(SITE_NO) %>% arrange(RESULT_VA) %>% slice(1) %>% ungroup()
        # Make data set with max ampa concentration for each site
      ampa_data_with_locations <- left_join(ampa_data, sites2)
        # Add location information to ampa data
      
    # Data built for statistical testing
      
      posthoc_results_pyrene <- compare_means(RESULT_VA ~ COUNTY_NM , pyrene_data, method="wilcox.test",paired=FALSE, p.adjust.method = "fdr")
        #running wilcoxon rank sum test on pyrene
      
      posthoc_significant_results_pyrene <- posthoc_results_pyrene[posthoc_results_pyrene$p.signif == "*",]
        # Data frame containing only significant results of post hoc test
      
  # Data for Multivariate Analysis
      
      setwd("/Users/simone/Documents/UT/UrbanEco/Datamining/RSQA worksheets/Multivariable Analysis")
        # Set working directory for new data downloaded for this section
      
      results3 <- read.csv("Results.csv")
        # Load data
      sites <- read.csv("Sites.csv")
        # Load site data associated with results
      
      median_results <- results3 %>% group_by(SITE_NO, PARM_NM) %>% dplyr::summarise(median_val = median(RESULT_VA)) %>% ungroup()
        # Make data frame containing site number and parameter name, with the median result value
    
      short_results <- pivot_wider(median_results, id_cols = SITE_NO, names_from = PARM_NM, values_from = median_val)
        # Convert median_results to short form
      
      Interestingcolumns_data <- as.data.frame(short_results[,c(2,7,8,9,11,12,16,17)])
        # Select the columns that we are interested in showing on the PCA
      
      rownames(Interestingcolumns_data) <- short_results$SITE_NO
        # Assigns site number to be the row name so that it is not included in the PCA
      
      final_results <- Interestingcolumns_data[complete.cases(Interestingcolumns_data), ]
        # Removes NAs from the data set
      
      metadata <- sites[match(rownames(final_results), sites$SITE_NO),]
        # Create metadata by matching the row names of final_results with the site numbers in the site data frame
      
# Solutions ####################################################################
  
# Q12
  
  expcount <- mydata %>% group_by(PARM_NM) %>% summarize(count=n())
    # Make tibble, describing how many observations were made for each parameter.
  expcount
    # View tibble

  expcount_histogram <- expcount %>% ggplot(aes(count)) + geom_histogram(color = "black", fill = "#ffc7c7", binwidth = 100) + theme_classic() + labs(title = "Distribution of Amount of Observations for Each Parameter", x = "Amount of Observations Per Parameter", y = "Count")
    # Make histogram of tibble
  expcount_histogram
    # View histogram.

# Q13

  expcount <- expcount[expcount$count > 50,]
    # Removes parameters with less than 50 observations from the tibble
  expcount <- expcount[order(expcount$count, decreasing = T),]
    # Orders tibble rows from greatest to least amount of observations
  head(expcount)
    # View tibble
  
# Q13 including non-detects for Dr. Ruth
  
  expcount <- mydatacorr %>% filter(mydatacorr$REMARK_CD != "<") %>% group_by(PARM_NM) %>% summarise(count=n())
  expcount <- expcount[order(expcount$count, decreasing = T),]
  
# Q14

  imidacloprid_data <- mydata[mydata$PARM_NM == "Imidacloprid, wf",]
    # Make new data set containing only observations of imidacloprid

# Q17

  ggplot(imidacloprid_data, aes(x = PARM_NM, y = RESULT_VA)) + geom_point(alpha=0.5) + labs(title = "Concentrations of Imidacloprid", x = "Imidacloprid", y = "Concnetration (ug/kg)") + theme_classic()
    # Making scatter plot for imidacloprid data
  
  ggplot(imidacloprid_data, aes(x = PARM_NM, y = RESULT_VA)) + geom_jitter(alpha=0.5) + labs(title = "Concentrations of Imidacloprid", x = "Imidacloprid", y = "Concnetration (ug/kg)") + theme_classic()
    # Making jitter plot for imidacloprid data

# Q20

  top_5_data <- mydata[mydata$PARM_NM %in% expcount$PARM_NM[1:5],]
    # Make new data set containing the 5 parameters with the most observations
  
# Q21
  
  ggplot(top_5_data, aes(x = PARM_NM, y = RESULT_VA)) + geom_jitter(alpha=0.5) + labs(title = "Concentrations of Top 5 Parameters", x = "Parameters", y = "Concnetration (ug/kg)") + theme_classic()
    # Make jitter plot of top 5 parameters

# Q22

  ggplot(top_5_data, aes(x = PARM_NM, y = RESULT_VA)) + geom_jitter(aes(color = PARM_NM), alpha=0.5) + labs(title = "Concentrations of Top 5 Parameters", x = "Parameters", y = "Concnetration (ug/kg)") + theme_classic() + theme(axis.text.x = element_blank())
    # Make the jitter plot pretty

# Q23

  ggplot(top_5_data, aes(x = PARM_NM, y = RESULT_VA)) + geom_jitter(alpha=0.5) + labs(title = "Concentrations of Top 5 Parameters", x = "Parameters", y = "Concnetration (ug/kg)") + theme_classic() + facet_grid(PARM_NM ~ .) + theme(axis.text.x = element_blank())
    # Make vertical faceted jitter plot
  
  ggplot(top_5_data, aes(x = PARM_NM, y = RESULT_VA)) + geom_jitter(alpha=0.5) + labs(title = "Concentrations of Top 5 Parameters", x = "Parameters", y = "Concnetration (ug/kg)") + theme_classic() + facet_wrap(. ~ PARM_NM) + theme(axis.text.x = element_blank())
    # Make horizontal faceted jitter plot

# Q24

  ggplot(top_5_data, aes(x = PARM_NM, y = RESULT_VA)) + geom_jitter(alpha=0.5) + labs(title = "Concentrations of Top 5 Parameters", x = "Parameters", y = "Concnetration (ug/kg)") + theme_classic() + facet_grid(PARM_NM ~ ., scales="free") + theme(axis.text.x = element_blank())
    # Make vertical faceted jitter plot with different scales for each parameter
  
  ggplot(top_5_data, aes(x = PARM_NM, y = RESULT_VA)) + geom_jitter(alpha=0.5) + labs(title = "Concentrations of Top 5 Parameters", x = "Parameters", y = "Concnetration (ug/kg)") + theme_classic() + facet_wrap(. ~ PARM_NM, scales="free") + theme(axis.text.x = element_blank())
    # Make horizontal faceted jitter plot with different scales for each parameter

# Q26

  top_5_data_with_locations <- left_join(top_5_data, sites)
    # Add location information to top_5_data
  
# Q27

  world <- ne_countries(scale="medium", returnclass = "sf")
    # Data frame containing shape of the countries we want to map
  
  world_map <-ggplot(data=world) + geom_sf() + theme_classic() + labs(title="World Map", x="Longitude", y="Latitude")
    # Make world map
  world_map
    # View world map
  
# Q28
  
  world_map_1 <-ggplot(data = world) + geom_sf() + theme_classic() + labs(title="Map of CSQA Pesticide Sampling Sites", x="Longitude", y="Latitude")
    # Add appropriate title to map
  world_map_1
  
  world_map_Map_2 <- ggplot(data = world) + geom_sf() + theme_classic() + labs(title="Map of CSQA Pesticide Sampling Sites", x="Longitude", y="Latitude") + geom_point(data = top_5_data_with_locations, aes(x = DEC_LONG_VA, y = DEC_LAT_VA), size = 1, shape = 19)
    # Add points to plot
  world_map_2
  
  world_map_3 <- ggplot(data = world) + geom_sf() + theme_classic() + labs(title="Map of CSQA Pesticide Sampling Sites", subtitle=paste0("A total of ", (length(unique(sites$SITE_NO))), " sites"), x="Longitude", y="Latitude") + geom_point(data = top_5_data_with_locations, mapping = aes(x = DEC_LONG_VA, y = DEC_LAT_VA)) 
    # Add subtitle to plot, including the number of sites in data set
  world_map_3
  
# Q29
  
  world_map_4 <- ggplot(data = world) + geom_sf() + theme_classic() + labs(title="Map of CSQA Pesticide Sampling Sites", subtitle=paste0("A total of ", (length(unique(sites$SITE_NO))), " sites"), x="Longitude", y="Latitude") + geom_point(data = top_5_data_with_locations, mapping = aes(x = DEC_LONG_VA, y = DEC_LAT_VA))+ coord_sf(xlim = c((min(sites$DEC_LONG_VA)), (max(sites$DEC_LONG_VA))),ylim = c((min(sites$DEC_LAT_VA)), (max(sites$DEC_LAT_VA)))) 
    # Narrow down region to max and min latitude/longitude
  world_map_4

# Q30
  
  world_map_5 <- ggplot(data = world) + geom_sf() + theme_classic() + labs(title="Map of CSQA Pesticide Sampling Sites", subtitle=paste0("A total of ", (length(unique(sites$SITE_NO))), " sites"), x="Longitude", y="Latitude") + geom_point(data = top_5_data_with_locations, mapping = aes(x = DEC_LONG_VA, y = DEC_LAT_VA))+ coord_sf(xlim = c((min(sites$DEC_LONG_VA-1)), (max(sites$DEC_LONG_VA+1))),ylim = c((min(sites$DEC_LAT_VA-1)), (max(sites$DEC_LAT_VA+1)))) 
    # Add some padding to region limits
  world_map_5
  
# Q33
  
  setwd("/Users/simone/Documents/UT/UrbanEco/Datamining/RSQA worksheets/Replicating Graphs")
    # Working directory for replicating graphs
    
  results2 <- read.csv("Results.csv")
  sites2 <- read.csv("Sites.csv")
    # Load data
  
# Q34
  
  # Graph one
  
    pyrene_data <- results2[results2$PARM_NM == "Pyrene, solids",] %>% group_by(SITE_NO) %>% arrange(RESULT_VA) %>% slice(1) %>% ungroup()
      # Make data set with max pyrene concentration for each site
    
    ggplot(pyrene_data, aes(x = COUNTY_NM, y = RESULT_VA)) + geom_jitter(aes(color = COUNTY_NM)) + labs(title = "Max Pyrene Concentration for Each Site by County", x = "", y = "Concnetration (ug/kg)") + theme_bw() + theme(axis.text.x = element_blank())
      # Making jitter plot with pyrene data
    
  # Graph Two
    
    pyrene_data_with_locations <- left_join(pyrene_data, sites2)
      # Add location information to pyrene data
    
    pyrene_map <- ggplot(data = world) + geom_sf() + labs(title="Map of California Sampling Sites", subtitle=paste0("A total of ", (length(unique(sites$SITE_NO))), " sites"), x="Longitude", y="Latitude") + geom_point(data = pyrene_data_with_locations, mapping = aes(x = DEC_LONG_VA, y = DEC_LAT_VA, color = COUNTY_NM, size = RESULT_VA), alpha = 0.5) + coord_sf(xlim = c((min(sites$DEC_LONG_VA-1)), (max(sites$DEC_LONG_VA+1))),ylim = c((min(sites$DEC_LAT_VA-1)), (max(sites$DEC_LAT_VA+1)))) 
      # Making map with pyrene data
    pyrene_map <- pyrene_map + labs(color = "California County", size = "Max Pyrene Concentration")
      # Changing legend titles
    pyrene_map
      # Show plot
    
# Q36
    
  # Graph one
    
      myclobutanil_data <- results2[results2$PARM_NM == "Myclobutanil, wf",] %>% group_by(SITE_NO) %>% arrange(RESULT_VA) %>% slice(1) %>% ungroup()
        # Make data set with max myclobutanil concentration for each site
      
      ggplot(myclobutanil_data, aes(x = COUNTY_NM, y = RESULT_VA)) + geom_jitter(aes(color = COUNTY_NM)) + labs(title = "Max Myclobutanil Concentration for Each Site by County", x = "", y = "Concnetration (ug/kg)") + theme_bw() + theme(axis.text.x = element_blank())
        # Making jitter plot with pyrene data
    
  # Graph Two
    
      myclobutanil_data_with_locations <- left_join(myclobutanil_data, sites2)
        # Add location information to myclobutanil data
      
      myclobutanil_map <- ggplot(data = world) + geom_sf() + labs(title="Map of California Sampling Sites", subtitle=paste0("A total of ", (length(unique(sites$SITE_NO))), " sites"), x="Longitude", y="Latitude") + geom_point(data = myclobutanil_data_with_locations, mapping = aes(x = DEC_LONG_VA, y = DEC_LAT_VA, color = COUNTY_NM, size = RESULT_VA), alpha = 0.5) + coord_sf(xlim = c((min(sites$DEC_LONG_VA-1)), (max(sites$DEC_LONG_VA+1))),ylim = c((min(sites$DEC_LAT_VA-1)), (max(sites$DEC_LAT_VA+1)))) 
        # Making map with myclobutanil data
      myclobutanil_map <- myclobutanil_map + labs(color = "California County", size = "Max Myclobutanil Concentration")
        # Changing legend titles
      myclobutanil_map
        # Show plot
  
# Q37
    
  # Graph one
    
      ampa_data <- results2[results2$PARM_NM == "AMPA, w,gf<0.7u",] %>% group_by(SITE_NO) %>% arrange(RESULT_VA) %>% slice(1) %>% ungroup()
        # Make data set with max ampa concentration for each site
      
      ggplot(ampa_data, aes(x = COUNTY_NM, y = RESULT_VA)) + geom_jitter(aes(color = COUNTY_NM)) + labs(title = "Max AMPA Concentration for Each Site by County", x = "", y = "Concnetration (ug/kg)") + theme_bw() + theme(axis.text.x = element_blank())
        # Making jitter plot with ampa data
    
  # Graph Two
    
      ampa_data_with_locations <- left_join(ampa_data, sites2)
        # Add location information to ampa data
      ampa_map <- ggplot(data = world) + geom_sf() + labs(title="Map of California Sampling Sites", subtitle=paste0("A total of ", (length(unique(sites$SITE_NO))), " sites"), x="Longitude", y="Latitude") + geom_point(data = ampa_data_with_locations, mapping = aes(x = DEC_LONG_VA, y = DEC_LAT_VA, color = COUNTY_NM, size = RESULT_VA), alpha = 0.5) + coord_sf(xlim = c((min(sites$DEC_LONG_VA-1)), (max(sites$DEC_LONG_VA+1))),ylim = c((min(sites$DEC_LAT_VA-1)), (max(sites$DEC_LAT_VA+1)))) 
        # Making map with ampa data
      ampa_map <- ampa_map + labs(color = "California County", size = "Max AMPA Concentration")
        # Changing legend titles
      ampa_map
        # Show plot
      
# Q38
      
  install.packages("ggpubr")
    library(ggpubr)
      
# Q41
    
  pyrene_data <- results2[results2$PARM_NM == "Pyrene, solids",] %>% group_by(SITE_NO) %>% arrange(RESULT_VA) %>% slice(1) %>% ungroup()
    # Make data set with max pyrene concentration for each site
  
  compare_means(RESULT_VA ~ COUNTY_NM, pyrene_data, method="kruskal.test", paired = FALSE , p.adjust.method="fdr")
    # Kruskal-Wallis test 
  
# Q43
  
  compare_means(RESULT_VA ~ COUNTY_NM, myclobutanil_data, method="kruskal.test", paired = FALSE , p.adjust.method="fdr")
    # Kruskal-Wallis test for myclobutanil
  
  compare_means(RESULT_VA ~ COUNTY_NM, ampa_data, method="kruskal.test", paired = FALSE , p.adjust.method="fdr")
    # Kruskal-Wallis test for ampa
  
# Q44
  
  posthoc_results_pyrene <- compare_means(RESULT_VA ~ COUNTY_NM , pyrene_data, method="wilcox.test",paired=FALSE, p.adjust.method = "fdr")
    #running wilcoxon rank sum test on pyrene
  View(posthoc_results_pyrene)

# Q45
  
  pyrene_boxplot <- pyrene_data %>% ggplot(aes(COUNTY_NM, RESULT_VA)) + labs(title = "Pyrene Concentration for Each Site by County", x = "California County", y = "Concentration of Pyrene per Site (ug/kg)", subtitle=paste0("A total of ", (length(unique(sites$SITE_NO))), " sites"), fill = "California County") + theme_classic() + geom_boxplot(aes(fill = COUNTY_NM)) + theme(axis.text.x = element_blank())
    # Box plot of pyrene data
  pyrene_boxplot
    # Show plot
  
# Q46
  
  posthoc_significant_results_pyrene <- posthoc_results_pyrene[posthoc_results_pyrene$p.signif == "*",]
    # Data frame containing only significant results of post hoc test

# Q47
  
  pyrene_boxplot_with_stats1 <- pyrene_data %>% ggplot(aes(COUNTY_NM, RESULT_VA)) + labs(title = "Pyrene Concentration for Each Site by County", x = "California County", y = "Concentration of Pyrene per Site (ug/kg)", subtitle=paste0("A total of ", (length(unique(sites$SITE_NO))), " sites"), fill = "California County") + theme_classic() + geom_boxplot(aes(fill = COUNTY_NM)) + theme(axis.text.x = element_blank()) + stat_pvalue_manual(inherit.aes=FALSE, data = posthoc_significant_results_pyrene, label = "p.signif",y.position=220)
    # Add ad hoc data to boxplot
  pyrene_boxplot_with_stats1

# Q48

  pyrene_boxplot_with_stats2 <- pyrene_data %>% ggplot(aes(COUNTY_NM, RESULT_VA)) + labs(title = "Pyrene Concentration for Each Site by County", x = "California County", y = "Concentration of Pyrene per Site (ug/kg)", subtitle=paste0("A total of ", (length(unique(sites$SITE_NO))), " sites"), fill = "California County") + theme_classic() + geom_boxplot(aes(fill = COUNTY_NM)) + theme(axis.text.x = element_blank()) + stat_pvalue_manual(inherit.aes=FALSE, data = posthoc_significant_results_pyrene, label = "p", y.position=220)
    # Change 'label = "p"'
  pyrene_boxplot_with_stats2

# Q49
  
  # Run Post-hoc testing on myclobutanil and ampa
    
    posthoc_results_myclobutanil <- compare_means(RESULT_VA ~ COUNTY_NM , myclobutanil_data, method="wilcox.test", paired=FALSE, p.adjust.method = "fdr")
      # Running wilcoxon rank sum test on myclobutanil
    View(posthoc_posthoc_results_myclobutanil)
      # Checking for significant result
    
    posthoc_results_ampa <- compare_means(RESULT_VA ~ COUNTY_NM, ampa_data, method="wilcox.test",paired=FALSE, p.adjust.method = "fdr")
      # Running wilcoxon rank sum test on myclobutanil
    View(posthoc_results_ampa)
      # Checking for significant result
    
  # Make Plots
    
    posthoc_significant_results_myclobutanil <- posthoc_results_myclobutanil[posthoc_results_myclobutanil$p.signif != "ns", ]
      # Data frame containing only significant results of post hoc test
    myclobutanil_boxplot_with_stats <- myclobutanil_data %>% ggplot(aes(COUNTY_NM, RESULT_VA)) + labs(title = "Myclobutanil Concentration for Each Site by County", x = "California County", y = "Concentration of Myclobutanil per Site (ug/kg)", subtitle=paste0("A total of ", (length(unique(sites$SITE_NO))), " sites"), color = "California County") + theme_classic() + geom_boxplot(aes(color = COUNTY_NM)) + theme(axis.text.x = element_blank()) + stat_pvalue_manual(inherit.aes=FALSE, data = posthoc_significant_results_myclobutanil, label = "p.signif",y.position=220, step.increase = .1)
      # Add ad hoc data to boxplot
    myclobutanil_boxplot_with_stats
    
    
    posthoc_significant_results_ampa <- posthoc_results_ampa[posthoc_results_ampa$p.signif != "ns",]
      # Data frame containing only significant results of post hoc test
    ampa_boxplot_with_stats <- ampa_data %>% ggplot(aes(COUNTY_NM, RESULT_VA)) + labs(title = "AMPA Concentration for Each Site by County", x = "California County", y = "Concentration of AMPA per Site (ug/kg)", subtitle=paste0("A total of ", (length(unique(sites$SITE_NO))), " sites"), color = "California County") + theme_classic() + geom_boxplot(aes(color = COUNTY_NM)) + theme(axis.text.x = element_blank()) + stat_pvalue_manual(inherit.aes=FALSE, data = posthoc_significant_results_ampa, label = "p.signif",y.position = 4, step.increase = .1)
      # Add ad hoc data to boxplot
    ampa_boxplot_with_stats

# Q50    
    
  setwd("/Users/simone/Documents/UT/UrbanEco/Datamining/RSQA worksheets/Multivariable Analysis")
    # Set working directory for new data downloaded for this section
      
  results3 <- read.csv("Results.csv")
    # Load data
  sites <- read.csv("Sites.csv")
    # Load site data associated with results
    
# Q52
    
  median_results <- results3 %>% group_by(SITE_NO, PARM_NM) %>% dplyr::summarise(median_val = median(RESULT_VA)) %>% ungroup()
    # Make data frame containing site number and parameter name, with the median result value
    
# Q53
    
  short_results <- pivot_wider(Median_results, id_cols = SITE_NO, names_from = PARM_NM, values_from = median_val)
    # Convert median_results to short form
    
# Q54    
    
  colSums(is.na(short_results))
    # Get a list of how many NAs are in each column
    
# Q55    
    
  Interestingcolumns_data <- as.data.frame(short_results[,c(2,7,8,9,11,12,16,17)])
    # Select the columns that we are interested in showing on the PCA
  
  rownames(Interestingcolumns_data) <- short_results$SITE_NO
    # Assigns site number to be the row name so that it is not included in the PCA
  
# Q56
  
  final_results <- Interestingcolumns_data[complete.cases(Interestingcolumns_data), ]
    # Removes NAs from the data set

# Q57

  metadata <- sites[match(rownames(final_results), sites$SITE_NO),]
    # Create metadata by matching the row names of final_results with the site numbers in the site data frame
  
# Q58
  
  data_correlation <- cor(final_results)
    # Make correlation matrix with Pearson's correlation
  View(data_correlation)
  
  data_correlation_nonparametric <- cor(final_results, method = "spearman")
    # Make correlation matrix with Spearman's correlation
  View(data_correlation)
  
# Q59  
  
  library(pheatmap)
    # Load package to make heat map
  
  pheatmap(data_correlation, annotations=rownames(data_correlation),  color = colorRampPalette(c("blue", "white", "red"))(50), show_rownames = T, show_colnames = T)
    # Make heat map
  
# Q60
  
  data_correlation <- cor(t(final_results))
    # Make correlation matrix for transposed data frame 
  
  pheatmap(data_correlation, show_rownames = F, show_colnames = F)
    # Make heat map of sites and chemicals  
  
# Q62
  
  # I already have ggbiplot installed
  library(ggbiplot)
  
# Q63
  
  prcompData <- prcomp(final_results, center = T, scale = T)
    # Make principal component axes  
  summary(prcompData)
  
# Edits from the github
