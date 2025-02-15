
# Objective ---------------------------------------------------------------
# This script serves to recreate the case rate facet curves by HHS region,
# and the three epi curves (resident cases, resident deaths, and staff cases)
# for LTC residents and staff. The curves are currently made in Excel.
# Analysts: David Franklin and Jason Massey
# Last updated: 11 September 2023
# Project folder: TBD
# Source data folder:
  # Epi Curves
  # //XXXXFiles/

  # HHS case rate facet plots
  # //XXXX

# Load packages -----------------------------------------------------------


pacman::p_load(

  tidyverse,    # data management + ggplot2 graphics
  readxl,       # imports excel files
  cowplot,      # combing plots to have the dual axis
  ggpattern     # adding patterns to curve for differential shading
  ) 

## UPDATE THIS DATE EVERY WEEK. IT MUST COINCIDE WITH FILE PATH CONTAINING EXCEL SPREADSHEET THAT IS THE SOURCE DATA FOR MAKING EPI CURVES.
  # Typically the date falls on a Monday.
data_dat <- "11Sep2023"

## Pathway for input data (epi_curves). 
data_input_epi <- "XXXX"

## Pathway for input data (case rates by HHS region)
data_input_hhs <- "XXXX"

## UPDATE DATE SUFFIX FOR HHS DATA EVERY WEEK TO GET MOST RECENT HHS FILE. THE DATE FALLS ON A SUNDAY (EX. Sep 10 = 0910).
  # Refer to source data folders mentioned above to check most recent file names if necessary.
data_dat_hhs <- "0910"
## Pathway for output data. 
data_output <- paste0("XXXX", data_dat)

# Import resident and staff data ------------------------------------------
# Resident data for epi curves
resident_data <- read_excel(paste0(data_input_epi, data_dat,"/EpiCurveData.xlsx"), sheet = "Resident Data") |> 
  select( 1:4,6:7,`Case Rate`,`Death Rate`) |>
  rename(res_case_rate = `Case Rate`,
         res_death_rate = `Death Rate`,
         res_cases = Cases,
         res_deaths = Deaths,
         res_case_beds_inf = case_res_beds_inf,
         res_deaths_beds_inf = deaths_res_beds_inf
         ) |>
 # Filtering data to reflect records from present going back a year
  filter(week >= max(week) - 55) |>                    
  mutate(week_end = as.Date(week_end))
  

names(resident_data)
head(resident_data)

# Import staff data for epi curves
staff_data <- read_excel(paste0(data_input_epi, data_dat,"/EpiCurveData.xlsx"), sheet = "Staff Data") |> 
  select( 1:6,`Case Rate`,`Death Rate`) |>
  rename(staff_case_rate = `Case Rate`,
         staff_death_rate = `Death Rate`,
         staff_cases = Cases,
         staff_deaths = Deaths,
         staff_case_beds_inf = cases_staff_beds_inf,
         staff_deaths_beds_inf = deaths_staff_beds_inf
  ) |>
  # Filtering data to reflect records from present going back a year
  filter(week >= max(week) - 55) |>                    
  mutate(week_end = as.Date(week_end))
# QA on variable names and weeks
names(resident_data)
names(staff_data)
table(resident_data$week)
table(staff_data$week)
# Combine resident and staff data 
all_data <- staff_data |>
  select(week, starts_with("staff")) |> 
  inner_join(resident_data, by = "week") |> 
  mutate(max_week = if_else(week == max(week),"Yes", "No")) # If row is the max week, it will be shaded differently in epi curve plots.
  
 names(all_data) 
 str(all_data)
 
 # Import resident case rate data by HHS region. Need to start a count to add week_end instead of joining the variable from above data set.
 hhs_data <- read_excel(paste0(data_input_hhs, "PM_HHSEpiCurve_Data_", data_dat_hhs, ".xlsx"), sheet = "Cases") |>
   filter(week >= max(week) - 55) |> 
   left_join(all_data, by = "week") |> 
   select(starts_with("week"), HHS_Region, res_case_rate.x) |> 
   rename( res_case_rate = res_case_rate.x)
names(hhs_data)
# Plot epi curves ---------------------------------------------------------

# Set break points for x axis limits, adding one extra week before and after to avoid data points being omitted.
 weekly_breaks <- seq.Date(from = min(all_data$week_end) - 7, 
                           to = max(all_data$week_end) + 7,
                           by = "week")
 ########################### Epi curve for resident cases#####################################
 # 1a - epi curve for resident cases.
epi_curve_res_case <- ggplot(data = all_data, 
    mapping = aes(x = week_end, y = res_cases, pattern = max_week)) +   # map date column to x-axis
    geom_col_pattern(fill = "#F4B183",
                     pattern_fill = "white",
                     pattern_color = "white",
                     pattern_angle = 45,
                     pattern_density = .5,
                     pattern_spacing = .025,
                     pattern_key_scale_factor = 0.6,
                     show.legend = FALSE) +
   scale_pattern_manual(values = c(Yes = "stripe", No = "none")) + 
    geom_text(mapping = aes(label = res_cases), size = 1.8, nudge_y = 1500) +  # raising labels above bars for clarity
    coord_cartesian(ylim = c(0, 50000))+                                       # setting y axis limits
  # y-axis adjustments
    scale_y_continuous(breaks = seq(0, 50000, by = 5000), 
                       expand            = c(0,0) 
                       ) +
  # x-axis labels
  scale_x_date(
    limits = c(min(weekly_breaks), max(weekly_breaks)),
    expand            = c(0,0),                # remove excess x-axis space before and after case bars
    breaks = seq(min(all_data$week_end), # Setting break points so that x axis has values for each value of "week_end"
                 max(all_data$week_end),
                 by = "week"
                 ),
   
    date_labels       = "%m/%e"         # date labels format
   ) +
  theme_cowplot() +                       # clears background 
  theme(
    axis.title.y = element_text(size = 10),
    axis.text.x = element_text(size = 4)
    
  ) +  
  labs(x = "", 
       y = "Count of COVID-19 Confirmed Cases"
  )   # title
 
epi_curve_res_case 
# 1b - case rate plot for residents.
rate_curve_res_case <- ggplot(data = all_data,
                     mapping = aes(x = week_end, y = res_case_rate)) +
       geom_line(
         stat = "identity",
         linewidth = 1,
         col = "#9999FF"
         ) +
         geom_text(
                   mapping = aes(label = res_case_rate), size = 2, nudge_y = -1, fontface = "bold") +
        # y-axis adjustments
        scale_y_continuous(                 # adjust y-axis
             breaks = seq(0,45, by = 5),    # set break intervals of rate axis
             expand            = c(0,0),    # remove extra space on y axis
             limits = c(0, 45),             # set extent of rate axis
             position = "right") +          # move rate axis to the right
        scale_x_date(
         limits = c(min(weekly_breaks), max(weekly_breaks)),
         expand            = c(0,0),                         # remove excess y-axis space between curve and y axis labels
         breaks = seq(min(all_data$week_end),                 # Setting break points so that x axis has values for each value of "week_end"
                      max(all_data$week_end),
                      by = "week"
                      ),
         
         date_labels       = "%m/%e"           # date labels format
         ) +
         theme_cowplot() +
         theme(
          axis.title.y = element_text(size = 10),
          axis.text.x = element_text(size = 4)
    
         ) +
      # Y-axis label, no x-axis label
      labs(x = "",
           y = "COVID-19 Confirmed Case per 1,000 Resident-Weeks"       # rate axis label  
       ) 
       
      
rate_curve_res_case 
# View epi curve and rate line plots once more before combining.
epi_curve_res_case
rate_curve_res_case
# 1c - Overlay epi curve (cases) and rate line (cases) plots for residents.

aligned_plots_res_case <- cowplot::align_plots(epi_curve_res_case, rate_curve_res_case, align="hv", axis="tblr")
ggdraw(aligned_plots_res_case[[1]]) + draw_plot(aligned_plots_res_case[[2]])


# 1d - export completed resident epi curve. Need to change output destination.
  # Achieving dynamic naming (for most current week) of export image by placing within function that recalls maximum week. 
output_1 <- function(week) {
  ggsave(filename = paste0("Covid Cases Resident Epi curve Wk ", week,".jpeg"),           # This function pastes name plus current week number and file extension.
          plot = last_plot(), path = data_output,
          height = 5, width = 13, units = "in", dpi = 300, bg = "white")
  }
output_1(max(all_data$week))
########################### Epi curve for resident deaths#####################################
# 2a - epi curve for resident deaths.
epi_curve_res_death <- ggplot(data = all_data, 
                             mapping = aes(x = week_end, y = res_deaths, pattern = max_week)) +   # map date column to x-axis
  geom_col_pattern(fill = "#8FAADC",
                   pattern_fill = "white",
                   pattern_color = "white",
                   pattern_angle = 45,
                   pattern_density = .09,
                   pattern_spacing = .01,
                   pattern_key_scale_factor = 0.6,
                   show.legend = FALSE) +
  scale_pattern_manual(values = c(Yes = "stripe", No = "none")) +
  geom_text(mapping = aes(label = res_deaths), size = 2, nudge_y = 300) +  # raising labels above bars for clarity
  coord_cartesian(ylim = c(0, 10000))+                                       # setting y axis limits
  # y-axis adjustments
  scale_y_continuous(breaks = seq(0, 10000, by = 2000), 
                     expand            = c(0,0) 
  ) +
  # x-axis labels
  scale_x_date(
    limits = c(min(weekly_breaks), max(weekly_breaks)),
    expand            = c(0,0),                # remove excess x-axis space before and after case bars
    breaks = seq(min(all_data$week_end), # Setting break points so that x axis has values for each value of "week_end"
                 max(all_data$week_end),
                 by = "week"
    ),
    
    date_labels       = "%m/%e"         # date labels format
  ) +
  theme_cowplot() +                       # clears background 
  theme(
    axis.title.y = element_text(size = 10),
    axis.text.x = element_text(size = 4)
    
  ) +  
  labs(x = "", 
       y = "Count of COVID-19 Confirmed Deaths"
  )   # title

epi_curve_res_death 
# 2b - death rate plot for residents.
rate_curve_res_death <- ggplot(data = all_data,
                              mapping = aes(x = week_end, y = res_death_rate)) +
  geom_line(
    stat = "identity",
    linewidth = 1,
    col = "#F4B183"
  ) +
  geom_text(
    mapping = aes(label = res_death_rate), size = 2, fontface = "bold") +
  # y-axis adjustments
  scale_y_continuous(                 # adjust y-axis
    breaks = seq(0,10, by = 2),    # set break intervals of rate axis
    expand            = c(0,0),    # remove extra space on y axis
    limits = c(0, 10),             # set extent of rate axis
    position = "right") +          # move rate axis to the right
  scale_x_date(
    limits = c(min(weekly_breaks), max(weekly_breaks)),
    expand            = c(0,0),                         # remove excess y-axis space between curve and y axis labels
    breaks = seq(min(all_data$week_end),                 # Setting break points so that x axis has values for each value of "week_end"
                 max(all_data$week_end),
                 by = "week"
    ),
    
    date_labels       = "%m/%e"       # date labels format
  ) +
  theme_cowplot() +
  theme(
    axis.title.y = element_text(size = 10),
    axis.text.x = element_text(size = 4)
    
  ) +
  # Y-axis label, no x-axis label
  labs(x = "",
       y = "COVID-19 Deaths per 1,000 Resident-Weeks"       # rate axis label  
  ) 


rate_curve_res_death 
# View epi curve and rate line plots once more before combining.
epi_curve_res_death
rate_curve_res_death
# 2c - Overlay epi curve (deaths) and rate line (deaths) plots for residents.

aligned_plots_res_death <- cowplot::align_plots(epi_curve_res_death, rate_curve_res_death, align="hv", axis="tblr")
ggdraw(aligned_plots_res_death[[1]]) + draw_plot(aligned_plots_res_death[[2]])


# 2d - export completed resident epi curve. Need to change output destination.
  # Achieving dynamic naming (for most current week) of export image by placing within function that recalls maximum week. 
output_2 <- function(week) {
  ggsave(filename = paste0("Covid Deaths Resident Epi curve Wk ", week,".jpeg"),           # This function pastes name plus current week number and file extension.
         plot = last_plot(), path = data_output,
         height = 5, width = 13, units = "in", dpi = 300, bg = "white")
  }
output_2(max(all_data$week))
########################### Epi curve for staff cases#####################################
# 3a - epi curve for staff cases.
epi_curve_staff_case <- ggplot(data = all_data, 
                             mapping = aes(x = week_end, y = staff_cases, pattern = max_week)) +   # map date column to x-axis
   geom_col_pattern(fill = "#9999FF",
                   pattern_fill = "white",
                   pattern_color = "white",
                   pattern_angle = 45,
                   pattern_density = .5,
                   pattern_spacing = .025,
                   pattern_key_scale_factor = 0.6,
                   show.legend = FALSE) +
  scale_pattern_manual(values = c(Yes = "stripe", No = "none")) +
  geom_text(mapping = aes(label = staff_cases), size = 1.8, nudge_y = 1300) +  # raising labels above bars for clarity
  coord_cartesian(ylim = c(0, 50000))+                                       # setting y axis limits
  # y-axis adjustments
  scale_y_continuous(breaks = seq(0, 50000, by = 5000), 
                     expand            = c(0,0) 
  ) +
  # x-axis labels
  scale_x_date(
    limits = c(min(weekly_breaks), max(weekly_breaks)),
    expand            = c(0,0),                # remove excess x-axis space before and after case bars
    breaks = seq(min(all_data$week_end), # Setting break points so that x axis has values for each value of "week_end"
                 max(all_data$week_end),
                 by = "week"
    ),
    
    date_labels       = "%m/%e"         # date labels format
  ) +
  theme_cowplot() +                       # clears background 
  theme(
    axis.title.y = element_text(size = 10),
    axis.text.x = element_text(size = 4)
    
  ) +  
  labs(x = "", 
       y = "Count of COVID-19 Confirmed Cases"
  )   # title

epi_curve_staff_case 
# 3b - case rate plot for staff.
rate_curve_staff_case <- ggplot(data = all_data,
                              mapping = aes(x = week_end, y = staff_case_rate)) +
  geom_line(
    stat = "identity",
    linewidth = 1,
    col = "#F4B183"
  ) +
  geom_text(
    mapping = aes(label = staff_case_rate), size = 2, nudge_y = -.8, fontface = "bold") +
  # y-axis adjustments
  scale_y_continuous(                 # adjust y-axis
    breaks = seq(0,45, by = 5),    # set break intervals of rate axis
    expand            = c(0,0),    # remove extra space on y axis
    limits = c(0, 45),             # set extent of rate axis
    position = "right") +          # move rate axis to the right
  scale_x_date(
    limits = c(min(weekly_breaks), max(weekly_breaks)),
    expand            = c(0,0),                         # remove excess y-axis space between curve and y axis labels
    breaks = seq(min(all_data$week_end),                 # Setting break points so that x axis has values for each value of "week_end"
                 max(all_data$week_end),
                 by = "week"
    ),
    
    date_labels       = "%m/%e"          # date labels format
  ) +
  theme_cowplot() +
  theme(
    axis.title.y = element_text(size = 10),
    axis.text.x = element_text(size = 4)
    
  ) +
  # Y-axis label, no x-axis label
  labs(x = "",
       y = "COVID-19 Confirmed Case per 1,000 Resident-Weeks"       # rate axis label  
  ) 


rate_curve_staff_case 
# View epi curve and rate line plots once more before combining.
epi_curve_staff_case
rate_curve_staff_case
# 3c - Overlay epi curve (cases) and rate line (cases) plots for staff.

aligned_plots_staff_case <- cowplot::align_plots(epi_curve_staff_case, rate_curve_staff_case, align="hv", axis="tblr")
ggdraw(aligned_plots_staff_case[[1]]) + draw_plot(aligned_plots_staff_case[[2]])


# 3d - export completed staff epi curve. Need to change output destination.
  # Achieving dynamic naming (for most current week) of export image by placing within function that recalls maximum week. 
output_3 <- function(week) {
  ggsave(filename = paste0("Covid Cases Staff Epi curve Wk ", week,".jpeg"),           # This function pastes name plus current week number and file extension.
         plot = last_plot(), path = data_output,
         height = 5, width = 13, units = "in", dpi = 300, bg = "white")
}
output_3(max(all_data$week))


########################### Resident case rate facets by HHS Region#####################################
  
hhs_plot <- ggplot(
    hhs_data,
    aes(x = week_end,
        y = res_case_rate)) +
  theme_classic() +
  geom_line(color = "#F4B183", lwd=.6) +
  coord_cartesian(ylim = c(0, 20)) +
  ## Create facet plots stratified by fema_region
  facet_wrap( ~ HHS_Region, nrow = 2, scales = "free") +
  labs(x = '') +
  theme(
    plot.title = element_text(size = 12, face = "bold", hjust = 0.5),
    legend.position = "none",
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_blank(),
    axis.line = element_line(color = "gray40"),
    axis.title.x = element_text(size = 9, color = "gray40"),
    axis.title.y = element_text(size = 9, color = "gray40"),
    axis.text.y = element_text(size = 7, color = "gray40"),
    axis.text.x = element_text(size = 3, color = "gray40", angle = 35, hjust = 1),
    strip.background = element_blank(),
    strip.text.x = element_blank(),
    panel.spacing.x = unit(1, "lines"),
    panel.spacing.y = unit(0.5, "lines")) +
  scale_x_date(date_labels = "%m/%e",
               breaks = seq(min(hhs_data$week_end),
                            max(hhs_data$week_end),
                              by="week")) +
  scale_y_continuous(name = str_wrap("Resident Case-Rate Per 1,000 Resident-Weeks", width = 65),
                     breaks = seq(0,60, by = 20)) +
  ## Add labels for fema_region to each facet plot  
  geom_text(aes(label = HHS_Region,
                x = mean(week_end),
                y = max(hhs_data$res_case_rate)*1.08),
            size = 3.2)#) +
  ## Add grey shading to most recent MMWR week of data  
  annotate(geom = 'rect',
           xmin = "2022-08-21",
           xmax = "2023-09-10",
           ymin = 0,
           ymax = 30,
           fill = 'grey', alpha = 0.4)
  
  library(plotly)
  
## Adding Trace
  hhs_plot <- hhs_plot %>%
    add_trace(
      hoverinfo = hhs_plot$week_end,
      marker = list(color='green'),
      showlegend = F
    )
  
hhs_plot

## Save regional plot image as JPEG
ggsave(path = output.folder,
       filename = paste("covid_adm_regional_",
                        file.date, ".jpeg", sep=""),
       covid_adm_regional,
       width = 10,
       height = 4,
       units = "in")
