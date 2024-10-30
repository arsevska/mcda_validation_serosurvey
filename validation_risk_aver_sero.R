library(boot)
library(infer)
library(dplyr)
library(data.table)
library(tidyr)
library(ggplot2)
library(ggpubr)

# Set input and output dirs
input_dir <- "D:/Mes Donnees/git/mcda_validation_serosurvey/"
output_dir <- paste0(input_dir,"output/")
if (!dir.exists(output_dir)) {dir.create(output_dir)}

# Read the communes (estimate data sets)
bauchi <- read.csv(paste0(input_dir,"data/bau_risk_aver_ser.csv"), sep=";")
kano <- read.csv(paste0(input_dir, "data/kan_risk_aver_ser.csv"),  sep=";")
plat <- read.csv(paste0(input_dir, "data/pla_risk_aver_ser.csv"),  sep=";")

# Set the state
state <- plat

# Calculate quantiles 0% (min), 50% (median), 60%, 70%,..., 100%
# (max) = bounds of the MCDA index
median(state$risk)

## Define probs and quantiles
probabilites = c(0.5, 0.6, 0.7, 0.8, 0.9)
quantiles_labels = paste0("Q",seq(length(probabilites) + 1))

# Define bootstrap number
reps_bootstrap = 1000

# Define quantile intervals in the estimate data set
state$quantile <- with(state, 
                       factor(
                         findInterval(risk, 
                                      c(quantile(risk, 
                                                 probs=probabilites))),
                         labels=quantiles_labels),
                       )

state$quantile <- as.character(state$quantile)

# Save the results from the MCDA index with calculated quantile class for 
# further mapping for example
write.csv(state,paste0(output_dir,state$NAME_1[1],
                       "_mcda_6_quantiles_estimates.csv"), 
          row.names = FALSE)

# For each class $i$ (50-60%,..., 90-100%), calculate the average 
# seroprevalences $P_i$ of the communes in this class
state_av_class <- aggregate(aver_sero~quantile, data=state, mean)

# Calculate $RR_i = P_i / P_{0-50\%}$
state_av_class$rr <- with(state_av_class,aver_sero/aver_sero[quantile == 'Q1'])

# Remove LGAs without sero surveillance data (to be confirmed)
state_no_na = state[!is.na(state$aver_sero),]

# Draw pseudo-samples of communes, with replacement, of the same size 
# as the initial sample
state_boot <- state_no_na |>
  specify(response = NAME_2)|>
  generate(reps = reps_bootstrap, type = "bootstrap")

# Re-calculate the $RR_i$ for each pseudo-sample
state_boot <- merge(state_boot, state_no_na, by="NAME_2", all.x = TRUE)
state_boot$ID <- seq.int(nrow(state_boot))

# Loop rows by replicate groups
for (replicate_group in unique(state_boot$replicate)) {
  # Filter data set by group
  df_filter <- state_boot[state_boot$replicate == replicate_group,]
  # Add quantile intervals to the filtered group data frame
  df_filter$quantile <- with(df_filter, 
                             factor(findInterval( risk, c(quantile(risk, 
                                                             probs=probabilites))),
                                    ))
  # Relabel dynamically, since levels are not fixed due to bootstrap random strata
  df_filter$quantile <- as.character(lapply(df_filter$quantile, 
                                            function(x) quantiles_labels[as.numeric(x)]))

  #Left join the original data frame with the filtered data frame
  # containing the calculated quantile
  state_boot <- merge(state_boot,df_filter[,c("quantile","ID")],by="ID",all.x = TRUE)


  # Fill NA values and remove extra columns. 
  # Performed only if the number of strata is >= 2 
  if ("quantile.x" %in% colnames(state_boot)) {
    # Unite fields after left join
    state_boot$quantile <- ifelse(is.na(state_boot$quantile.x),
                                   state_boot$quantile.y, 
                                   state_boot$quantile.x )
    # Remove .x and .y columns created after left-join
    state_boot <- state_boot %>% 
      select(-ends_with(c(".x", ".y")))
  }

}

# Re-calculate the $RR_i$ for each pseudo-sample
state_boot_av_class <- aggregate(aver_sero ~ replicate + quantile, data=state_boot, mean)

state_boot_av_class <- state_boot_av_class %>%
  group_by(replicate) %>%
  mutate(rr = aver_sero/aver_sero[quantile == 'Q1'])

# Calculate CI for each series of $RR_i$
alpha <- 0.05

# Calculate mean, upper and lower RR in the pseudo-sample data set
state_boot_ci <- 
  state_boot_av_class %>% 
  group_by(quantile) %>% 
  summarize(
    mean = mean(rr),
    lower = quantile(rr, probs = 0.25),  # 50% bootstrap CI
    upper = quantile(rr, probs = 0.75)
  )

# Produce a summary table of the mean, upper and lower RR
state_final_df <- merge(state_boot_ci,state_av_class,by="quantile",all.x = TRUE)

# Save the results from the validation, in case for better visuals/tables, etc.
write.csv(state_final_df,paste0(output_dir,state$NAME_1[1],
                                "_risk_validation_6_quantiles.csv"), 
          row.names = FALSE)

# Plot the estimates and the bootstrapped CI
ggplot(state_final_df, aes(x = rr, y = quantile)) + 
  geom_vline(aes(xintercept = 1.0), size = .25, linetype = "dashed") + 
  geom_errorbarh(aes(xmax = upper, xmin = lower), linewidth = .5, height = 
                   .2, color = "gray50") +
  geom_point(size = 2.5, color = "orange") +
  theme_bw()+
  theme(panel.grid.minor = element_blank()) +
  ylab("Quantile group") +
  xlab("Relative risk") +
  ggtitle(paste0(state$NAME_1[1]," validation results"))

# In case you want to save all 3 figures of all 3 states
# First produce 3 plots for each of the states
# Name the plots as bauchi_gg, kano_gg, plat_gg respectively

# final_figure <- ggarrange(bauchi_gg, kano_gg, plat_gg,
#                     labels = c("A", "B", "C"),
#                     ncol = 2, nrow = 2)
# final_figure

# Specify where to save the plot
ggsave("D:/Mes Donnees/git/mcda_validation_serosurvey/output/figure_3_states.png", 
       plot=final_figure, dpi=300)
