HotspotPlot <- function(CovidConfirmedCases, CovidDeaths, MAJCOMInput){
  #convert cases and death dataframes to long format. Also add in new_cases in last 1, 3, and 30 days
  tempCases = CovidConfirmedCases %>% select(-State, -stateFIPS) %>%
    reshape2::melt(id.var = c('CountyFIPS','County Name'), variable.name = 'date', value.name = "cumulative_cases") %>%
    mutate(date = as.Date(str_replace(date, "X",""), format = "%m/%d/%y"), `County Name` = as.character(`County Name`)) %>%
    distinct(CountyFIPS,date,.keep_all = TRUE)
  CasesGrowth <- tempCases %>% group_by(CountyFIPS) %>% arrange(CountyFIPS, date) %>%
    dplyr::mutate(new_cases_1 = cumulative_cases - lag(cumulative_cases, 1), 
                  new_cases_3_days = cumulative_cases - lag(cumulative_cases,3),
                  new_cases_30_days = lag(cumulative_cases,3) - lag(cumulative_cases, 30),
                  case_growth = ifelse(is.nan(new_cases_3_days/(new_cases_3_days + new_cases_30_days)), 0,
                                       (new_cases_3_days/(new_cases_3_days +new_cases_30_days))))
  tempDeaths = CovidDeaths %>% select(-State, -stateFIPS) %>%
    reshape2::melt(id.var = c('CountyFIPS','County Name'), variable.name = 'date', value.name = "cumulative_deaths") %>%
    mutate(date = as.Date(str_replace(date, "X",""), format = "%m/%d/%y"), `County Name` = as.character(`County Name`)) %>%
    distinct(CountyFIPS,date,.keep_all = TRUE) %>% mutate(cumulative_deaths = ifelse(is.na(cumulative_deaths), 0, cumulative_deaths))
  
  # join Cases and deaths dataframes
  Growth = CasesGrowth %>% left_join(tempDeaths, by = c("CountyFIPS" = "CountyFIPS", "date" = "date", "County Name" = "County Name"))
  # deaths_1_days = lag(cumulative_deaths,1),
  # deaths_3_days = cumulative_deaths - lag(cumulative_deaths,3), 
  # deaths_10_days = cumulative_deaths - lag(cumulative_deaths,10)) 
  
  #Join case data with county population data
  Growth = Growth %>% left_join(CountyInfo %>% select(FIPS, Population), by = c("CountyFIPS" = "FIPS")) %>% 
    mutate(Population = ifelse(is.na(Population), 0, Population),
           cumulative_deaths = ifelse(is.na(cumulative_deaths), 0, cumulative_deaths))
  
  # Function to fix 4-letter FIPS
  fix.fips <- function(column){
    column <- str_pad(column, width=5, side="left", pad="0")
    return(column)
  }
  # per capita calcs
  Growth = Growth %>% mutate(new_cases_1_pp = new_cases_1*100000/Population, new_cases_3_pp = new_cases_3_days*100000/Population,
                             new_cases_30_pp = new_cases_30_days*100000/Population, cases_pp = cumulative_cases*100000/Population,
                             deaths_pp = cumulative_deaths*100000/Population) %>% ungroup() %>%
    mutate(CountyFIPS = fix.fips(CountyFIPS))
  
  # Convert cimd dataframe to long format and filter to within 50 miles of base
  
  rownames(cimd) = CountyInfo[,3] 
  cimd_long <- cimd %>% rownames_to_column(var= "FIPS")
  cimd_long <- cimd_long %>% gather(-c(FIPS), key = base, value = DistanceMiles) 
  Bases50 <- cimd_long %>% filter(DistanceMiles <= 50) %>% mutate(FIPS = fix.fips(FIPS))
  
  #test code
  # Bases50 %>% filter(base == 'Pentagon') %>% left_join(Growth, by = c("FIPS" = "CountyFIPS")) %>%filter(date == current_date)
  
  #join base data with county growth data. aggregate county data to base-radius level. also add back in the MAJCOM column 
  bases_radius <- Bases50 %>% left_join(Growth, by = c("FIPS" = "CountyFIPS")) %>% dplyr::group_by(base, date) %>% 
    dplyr::summarise(cumulative_cases = sum(cumulative_cases), cases_pp = sum(cases_pp), 
                     new_cases_1_pp = sum(new_cases_1_pp), new_cases_3_pp = sum(new_cases_3_pp),
                     new_cases_30_pp = sum(new_cases_30_pp), cumulative_deaths = sum(cumulative_deaths), deaths_pp = sum(deaths_pp)) %>% 
    mutate(case_growth = new_cases_3_pp/(new_cases_30_pp+new_cases_3_pp))
  bases_radius = bases_radius %>% left_join(AFBaseLocations %>% select(Base, 'Major Command'), by = c("base" = "Base"))
  
  #this morning, cases were updated before deaths so I added in this code to pull the most current reported deaths date
  current_date = (bases_radius %>% ungroup() %>% filter(deaths_pp > 0) %>% filter(date ==max(date)) %>% select(date))$date[1]
  
  #ggrepel does not work with plotly , also I'm getting an error on the "aes(fill = deaths_pp)" when trying to convert to plotly. Any ideas why? 
  
  if (MAJCOMInput == "All"){
    
    bases_radius %>% mutate(cases_30_trunc = pmin(new_cases_30_pp, 10000)) %>%  # had to truncate cases at 10000 before since Mcguire was goin nuts 
      filter(new_cases_3_pp > 500, #filtering to show only bases with more than 200 cases per cap in last 3 days. gets cluttered if you include all
             date == current_date) %>% # just show AD AF bases
      ggplot(aes(size = cases_30_trunc, x = new_cases_3_pp, fill = deaths_pp , y = case_growth)) + 
      geom_point(alpha = 1, shape = 21, stroke = 1) + scale_size(range = c(0, 15), name="Cases (per 100,000) in\nLast 30 Days",
                                                                 breaks=c(2000,4000,6000,8000),
                                                                 labels=c("2000","4000","6000","8000+"),
                                                                 guide="legend") + 
      scale_alpha(range = c(1, 1)) + #this line might not be needed. didn't want the alpha values to change based off of color/fill
      scale_fill_distiller(palette = "RdBu", na.value = "#b2182b", "Deaths (per 100,000)") +
      scale_x_log10() + scale_y_continuous(labels = scales::percent) + expand_limits(y = 0) + 
      geom_hline(yintercept=1/9, linetype='dashed', col = 'black') + 
      # annotate("text", x = 3000, y = 1/9, label = 'Cases Shrinking', vjust = 1.5, color = 'blue') +  ##this code broke for some reason
      # annotate("text", x = 3000, y = 1/9, label = 'Cases Growing', vjust = -.5, color = 'red') +
      # geom_text(aes(label = base), size = 4, colour = "black", alpha = .6, check_overlap = TRUE, vjust = "top") + ##if you want text labels for plotly
      geom_label_repel(aes(new_cases_3_pp, case_growth, label = base),
                       fontface = 'bold', size = 3, fill = "white", color = "#00308f", box.padding = unit(0.75, "lines")) +
      ylab("Growth Rate (# Cases In 3 Days / # Cases in 30 Days)") + #ylim(0,.8) + #geom_line(y = 1/9) +
      xlab("New Cases (per 100,000) in Last 3 Days") + 
      ggtitle("COVID-19 Case Count Growth within 50 Miles of Installation", subtitle = paste0("Current as of ", current_date)) + 
      theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5))+ 
      theme_bw() +
      theme(plot.title = element_text(face = "bold", size = 15, family = "sans"),
            axis.title = element_text(face = "bold", size = 11, family = "sans"),
            axis.text.x = element_text(angle = 60, hjust = 1), 
            axis.line = element_line(color = "black"),
            legend.position = 'right',
            plot.background = element_blank(),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            panel.border = element_blank())
    # ggplotly(p)
  }
  else if (MAJCOMInput == "Active Duty"){
    
    bases_radius %>% mutate(cases_30_trunc = pmin(new_cases_30_pp, 10000)) %>%  # had to truncate cases at 10000 before since Mcguire was goin nuts 
      filter(new_cases_3_pp > 200, #filtering to show only bases with more than 200 cases per cap in last 3 days. gets cluttered if you include all
             date == current_date, 
             `Major Command` != "ANG" , `Major Command` != "AFRC") %>% # just show AD AF bases
      ggplot(aes(size = cases_30_trunc, x = new_cases_3_pp, fill = deaths_pp , y = case_growth)) + 
      geom_point(alpha = 1, shape = 21, stroke = 1) + scale_size(range = c(0, 15), name="Cases (per 100,000) in\nLast 30 Days",
                                                                 breaks=c(2000,4000,6000,8000),
                                                                 labels=c("2000","4000","6000","8000+"),
                                                                 guide="legend") + 
      scale_alpha(range = c(1, 1)) + #this line might not be needed. didn't want the alpha values to change based off of color/fill
      scale_fill_distiller(palette = "RdBu", na.value = "#b2182b", "Deaths (per 100,000)") +
      scale_x_log10() + scale_y_continuous(labels = scales::percent) + expand_limits(y = 0) + 
      geom_hline(yintercept=1/9, linetype='dashed', col = 'black') + 
      # annotate("text", x = 3000, y = 1/9, label = 'Cases Shrinking', vjust = 1.5, color = 'blue') +  ##this code broke for some reason
      # annotate("text", x = 3000, y = 1/9, label = 'Cases Growing', vjust = -.5, color = 'red') +
      # geom_text(aes(label = base), size = 4, colour = "black", alpha = .6, check_overlap = TRUE, vjust = "top") + ##if you want text labels for plotly
      geom_label_repel(aes(new_cases_3_pp, case_growth, label = base),
                       fontface = 'bold', size = 3, fill = "white", color = "#00308f", box.padding = unit(0.75, "lines")) +
      ylab("Growth Rate (# Cases In 3 Days / # Cases in 30 Days)") + #ylim(0,.8) + #geom_line(y = 1/9) +
      xlab("New Cases (per 100,000) in Last 3 Days") + 
      
      ggtitle("COVID-19 Case Count Growth within 50 Miles of Installation", subtitle = paste0("Current as of ", current_date)) + 
      theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5))+ 
      theme_bw() +
      theme(plot.title = element_text(face = "bold", size = 15, family = "sans"),
            axis.title = element_text(face = "bold", size = 11, family = "sans"),
            axis.text.x = element_text(angle = 60, hjust = 1), 
            axis.line = element_line(color = "black"),
            legend.position = 'right',
            plot.background = element_blank(),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            panel.border = element_blank())
    # ggplotly(p)
  }
  else if (MAJCOMInput == "ANG"){
    bases_radius %>% mutate(cases_30_trunc = pmin(new_cases_30_pp, 10000)) %>%  # had to truncate cases at 10000 before since Mcguire was goin nuts 
      filter(new_cases_3_pp > 400, #filtering to show only bases with more than 200 cases per cap in last 3 days. gets cluttered if you include all
             date == current_date, 
             `Major Command` == MAJCOMInput) %>% # just show AD AF bases
      ggplot(aes(size = cases_30_trunc, x = new_cases_3_pp, fill = deaths_pp , y = case_growth)) + 
      geom_point(alpha = 1, shape = 21, stroke = 1) + scale_size(range = c(0, 15), name="Cases (per 100,000) in\nLast 30 Days",
                                                                 breaks=c(2000,4000,6000,8000),
                                                                 labels=c("2000","4000","6000","8000+"),
                                                                 guide="legend") + 
      scale_alpha(range = c(1, 1)) + #this line might not be needed. didn't want the alpha values to change based off of color/fill
      scale_fill_distiller(palette = "RdBu", na.value = "#b2182b", "Deaths (per 100,000)") +
      scale_x_log10() + scale_y_continuous(labels = scales::percent) + expand_limits(y = 0) + 
      geom_hline(yintercept=1/9, linetype='dashed', col = 'black') + 
      # annotate("text", x = 3000, y = 1/9, label = 'Cases Shrinking', vjust = 1.5, color = 'blue') +  ##this code broke for some reason
      # annotate("text", x = 3000, y = 1/9, label = 'Cases Growing', vjust = -.5, color = 'red') +
      # geom_text(aes(label = base), size = 4, colour = "black", alpha = .6, check_overlap = TRUE, vjust = "top") + ##if you want text labels for plotly
      geom_label_repel(aes(new_cases_3_pp, case_growth, label = base),
                       fontface = 'bold', size = 3, fill = "white", color = "#00308f", box.padding = unit(0.75, "lines")) +
      ylab("Growth Rate (# Cases In 3 Days / # Cases in 30 Days)") + #ylim(0,.8) + #geom_line(y = 1/9) +
      xlab("New Cases (per 100,000) in Last 3 Days") + 
      
      ggtitle("COVID-19 Case Count Growth within 50 Miles of Installation", subtitle = paste0("Current as of ", current_date)) + 
      theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5))+ 
      theme_bw() +
      theme(plot.title = element_text(face = "bold", size = 15, family = "sans"),
            axis.title = element_text(face = "bold", size = 11, family = "sans"),
            axis.text.x = element_text(angle = 60, hjust = 1), 
            axis.line = element_line(color = "black"),
            legend.position = 'right',
            plot.background = element_blank(),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            panel.border = element_blank())
    # ggplotly(p)
  }
  else{
    bases_radius %>% mutate(cases_30_trunc = pmin(new_cases_30_pp, 10000)) %>%  # had to truncate cases at 10000 before since Mcguire was goin nuts 
      filter(new_cases_3_pp > 10, #filtering to show only bases with more than 200 cases per cap in last 3 days. gets cluttered if you include all
             date == current_date, 
             `Major Command` == MAJCOMInput) %>% # just show AD AF bases
      ggplot(aes(size = cases_30_trunc, x = new_cases_3_pp, fill = deaths_pp , y = case_growth)) + 
      geom_point(alpha = 1, shape = 21, stroke = 1) + scale_size(range = c(0, 15), name="Cases (per 100,000) in\nLast 30 Days",
                                                                 breaks=c(2000,4000,6000,8000),
                                                                 labels=c("2000","4000","6000","8000+"),
                                                                 guide="legend") + 
      scale_alpha(range = c(1, 1)) + #this line might not be needed. didn't want the alpha values to change based off of color/fill
      scale_fill_distiller(palette = "RdBu", na.value = "#b2182b", "Deaths (per 100,000)") +
      scale_x_log10() + scale_y_continuous(labels = scales::percent) + expand_limits(y = 0) + 
      geom_hline(yintercept=1/9, linetype='dashed', col = 'black') + 
      # annotate("text", x = 3000, y = 1/9, label = 'Cases Shrinking', vjust = 1.5, color = 'blue') +  ##this code broke for some reason
      # annotate("text", x = 3000, y = 1/9, label = 'Cases Growing', vjust = -.5, color = 'red') +
      # geom_text(aes(label = base), size = 4, colour = "black", alpha = .6, check_overlap = TRUE, vjust = "top") + ##if you want text labels for plotly
      geom_label_repel(aes(new_cases_3_pp, case_growth, label = base),
                       fontface = 'bold', size = 3, fill = "white", color = "#00308f", box.padding = unit(0.75, "lines")) +
      ylab("Growth Rate (# Cases In 3 Days / # Cases in 30 Days)") + #ylim(0,.8) + #geom_line(y = 1/9) +
      xlab("New Cases (per 100,000) in Last 3 Days") + 
      
      ggtitle("COVID-19 Case Count Growth within 50 Miles of Installation", subtitle = paste0("Current as of ", current_date)) + 
      theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5))+ 
      theme_bw() +
      theme(plot.title = element_text(face = "bold", size = 15, family = "sans"),
            axis.title = element_text(face = "bold", size = 11, family = "sans"),
            axis.text.x = element_text(angle = 60, hjust = 1), 
            axis.line = element_line(color = "black"),
            legend.position = 'right',
            plot.background = element_blank(),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            panel.border = element_blank())
    # ggplotly(p)
  }
  
  
}
