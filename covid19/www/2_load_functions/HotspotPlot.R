HotspotPlot <- function(CovidConfirmedCases, CovidDeaths, BranchSelect,OpsSelect,MAJNAFSelect,MAJCOMInput,NAFChoice,WingChoice,GroupChoice){
    #convert cases and death dataframes to long format. Also add in new_cases in last 1, 3, and 30 days
    tempCases = CovidConfirmedCases %>% select(-State, -stateFIPS) %>%
        reshape2::melt(id.var = c('CountyFIPS','County Name'), variable.name = 'date', value.name = "cumulative_cases") %>%
        mutate(date = as.Date(str_replace(date, "X",""), format = "%m/%d/%y"), `County Name` = as.character(`County Name`)) %>%
        distinct(CountyFIPS,date,.keep_all = TRUE)
    CasesGrowth <- tempCases %>% group_by(CountyFIPS) %>% arrange(CountyFIPS, date) %>%
        dplyr::mutate(new_cases_1 = cumulative_cases - lag(cumulative_cases, 1), 
                      new_cases_3_days = cumulative_cases - lag(cumulative_cases,3),
                      new_cases_30_days = cumulative_cases - lag(cumulative_cases, 30),
                      case_growth = ifelse(is.nan(new_cases_3_days/(new_cases_30_days)), 0,
                                           (new_cases_3_days/(new_cases_30_days))),
                      new_cases_7_days = cumulative_cases - lag(cumulative_cases,7),
                      new_cases_14_days = cumulative_cases - lag(cumulative_cases,14),
                      case_growth_week = ifelse(is.nan(new_cases_7_days/(new_cases_14_days - new_cases_7_days)), 0,
                                                (new_cases_7_days/(new_cases_14_days  - new_cases_7_days)))) %>%
                      replace(is.na(.),0)
    
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
    
    
    #######
    Growth = dplyr::filter(Growth, Population != 0)
    Growth <- Growth %>% mutate_if(is.numeric, function(x) ifelse(is.infinite(x), 0, x))
    
    #########
    
    # Function to fix 4-letter FIPS
    fix.fips <- function(column){
        column <- str_pad(column, width=5, side="left", pad="0")
        return(column)
    }
    # per capita calcs
    Growth = Growth %>% mutate(new_cases_1_pp = new_cases_1*100000/Population, new_cases_3_pp = new_cases_3_days*100000/Population,
                               new_cases_30_pp = new_cases_30_days*100000/Population, cases_pp = cumulative_cases*100000/Population,
                               new_cases_7_pp = new_cases_7_days*100000/Population, new_cases_14_pp = new_cases_14_days*100000/Population,
                               deaths_pp = cumulative_deaths*100000/Population) %>% ungroup() %>%
        mutate(CountyFIPS = fix.fips(CountyFIPS))
    
    # Convert cimd dataframe to long format and filter to within 50 miles of base
    rownames(cimd) = CountyInfo$FIPS
    cimd_long <- cimd %>% rownames_to_column(var= "FIPS")
    cimd_long <- cimd_long %>% gather(-c(FIPS), key = base, value = DistanceMiles) 
    Bases50 <- cimd_long %>% filter(DistanceMiles <= 50) %>% mutate(FIPS = fix.fips(FIPS))
    
    #test code
    # Bases50 %>% filter(base == 'Pentagon') %>% left_join(Growth, by = c("FIPS" = "CountyFIPS")) %>%filter(date == current_date)
    
    #join base data with county growth data. aggregate county data to base-radius level. also add back in the MAJCOM column 
    bases_radius <- Bases50 %>% left_join(Growth, by = c("FIPS" = "CountyFIPS")) %>% dplyr::group_by(base, date) %>% 
        dplyr::summarise(cumulative_cases = sum(cumulative_cases),  new_cases_1 = sum(new_cases_1),
                          new_cases_3_days = sum(new_cases_3_days),new_cases_7_days = sum(new_cases_7_days),
                          new_cases_14_days = sum(new_cases_14_days),new_cases_30_days = sum(new_cases_30_days),
                          cumulative_deaths = sum(cumulative_deaths), Population = sum(Population),
                          ) %>% 
         mutate(cases_pp = cumulative_cases/Population,
                new_cases_1_pp = new_cases_1/Population*100000, new_cases_3_pp = new_cases_3_days/Population*100000,
                new_cases_7_pp = new_cases_7_days/Population*100000, new_cases_14_pp = new_cases_14_days/Population*100000,
                new_cases_30_pp = new_cases_30_days/Population*100000, deaths_pp = cumulative_deaths/Population*100000,
                case_growth = new_cases_3_pp/(new_cases_30_pp),
                case_growth_week = ((new_cases_7_pp - (new_cases_14_pp-new_cases_7_pp)) / (new_cases_14_pp-new_cases_7_pp))
        )

    bases_radius = bases_radius %>% left_join(AFBaseLocations %>% select(Base,Branch,Operational,'Major Command'), by = c("base" = "Base"))
    #this morning, cases were updated before deaths so I added in this code to pull the most current reported deaths date
    current_date = (bases_radius %>% ungroup() %>% filter(deaths_pp > 0) %>% filter(date ==max(date)) %>% select(date))$date[1]
    #### Week % Change Chart ####
    
    
    ####Need to add filtering for branch/operational status/then majcom/NAF
    bases_radius<-dplyr::filter(bases_radius,Branch %in% BranchSelect) #"Air Force")#

    if (BranchSelect!="Air Force"){
        if (OpsSelect != "All"){ 
            bases_radius<-dplyr::filter(bases_radius,Operational %in% OpsSelect)
        }
        bases_radius <- bases_radius %>% 
            mutate(include = ifelse((new_cases_7_pp > 1800) & (date == current_date), TRUE, FALSE))
    } else if (BranchSelect=="Air Force"){
        if (OpsSelect != "All"){ 
            bases_radius<-dplyr::filter(bases_radius,Operational %in% OpsSelect)
        }
        if (MAJNAFSelect=="MAJCOM"){
            if (MAJCOMInput == "All"){ 
                bases_radius <- bases_radius %>% 
                    mutate(include = ifelse((new_cases_7_pp > 1800) & (date == current_date), TRUE, FALSE))
            }else if (MAJCOMInput == "Active Duty"){
                bases_radius <- bases_radius %>% 
                    mutate(include = ifelse((new_cases_7_pp > 500) & (date == current_date) & 
                                                (`Major Command` != "ANG") & (`Major Command` != "AFRC"), TRUE, FALSE))
            }else if ((MAJCOMInput == "ANG")){
                bases_radius <- bases_radius %>% 
                    mutate(include = ifelse((new_cases_7_pp > 1000) & (date == current_date) & 
                                                (`Major Command` == MAJCOMInput), TRUE, FALSE))
            }else{
                bases_radius <- bases_radius %>% mutate(include = ifelse((new_cases_7_pp > 0) & (date == current_date) & 
                                                                             (`Major Command` == MAJCOMInput), TRUE, FALSE))
            }
        } else if (MAJNAFSelect=="NAF"){
            AFWings<-dplyr::filter(AFNAFS,NAF %in% NAFChoice)
            if (WingChoice=="All") {
                forecastbaselist<-dplyr::filter(AFWings,Wing %in% WingList) 
                #forecastbaselist<-dplyr::filter(forecastbaselist,Group %in% GroupChoice)                 
                forecastbaselist<-sort(unique(forecastbaselist$Base), decreasing = FALSE) 
                bases_radius<-dplyr::filter(bases_radius,Base %in% forecastbaselist) 
                bases_radius <- bases_radius %>% 
                    mutate(include = ifelse((new_cases_7_pp > 1800) & (date == current_date), TRUE, FALSE))
            } else {
                forecastbaselist<-dplyr::filter(AFWings,Wing %in% WingChoice) 
                forecastbaselist<-dplyr::filter(forecastbaselist,Group %in% GroupChoice)                 
                forecastbaselist<-sort(unique(forecastbaselist$Base), decreasing = FALSE) 
                bases_radius<-dplyr::filter(bases_radius,Base %in% forecastbaselist)       
                bases_radius <- bases_radius %>% 
                    mutate(include = ifelse((new_cases_7_pp > 1800) & (date == current_date), TRUE, FALSE))
            }            
        }            
    } 
    
    
    min_new_14 <- min(bases_radius %>% filter(date == current_date) %>% .$new_cases_14_pp)
    max_new_14 <- pmin(max(bases_radius %>% filter(date == current_date) %>% .$new_cases_14_pp), 10000)
    min_week_rate <- pmax(min(bases_radius %>% filter(date== current_date) %>% .$case_growth_week), -2) 
    max_week_rate <- pmin(max(bases_radius %>% filter(date== current_date) %>% .$case_growth_week), 2)
    max_deaths <- max(bases_radius %>% filter(date== current_date) %>% .$deaths_pp)
                                   
    bases_radius %>%  # had to truncate cases at 10000 before since Mcguire was goin nuts 
        filter(include == TRUE) %>% # filter for "included" bases from IF statement above
        mutate(truncate_week_rate = pmin(case_growth_week, 2)) %>%
        ggplot(aes(size = new_cases_14_pp, x = new_cases_7_pp, fill = deaths_pp , y = truncate_week_rate)) + 
        geom_point(alpha = 1, shape = 21, stroke = 1) + scale_size_binned(range = c(0, 15), name="Cases (per 100,000) in Last 14 Days",
                                                                          # n.breaks=4,
                                                                          # nice.breaks = TRUE,
                                                                          limits = c(round(min_new_14, -3), max_new_14),
                                                                          #labels=c("2000","4000","6000","8000+"),
                                                                          guide=guide_legend(direction = "horizontal", title.position = "top")) + 
        scale_alpha(range = c(1, 1)) + #this line might not be needed. didn't want the alpha values to change based off of color/fill
        scale_fill_distiller(palette = "RdBu", na.value = "#b2182b", "Cumulative Deaths (per 100,000)", 
                             limits = c(min(bases_radius$deaths_pp), max(bases_radius$deaths_pp)), #,
                             guide=guide_colorsteps(title.position = "top")) +
        #scale_x_log10() + 
        scale_y_continuous(breaks = seq(min_week_rate,max_week_rate,.5),labels = scales::percent) + 
        expand_limits(y = c(min_week_rate,max_week_rate)) + 
        geom_hline(yintercept=0, linetype='dashed', col = 'black') + 
        # annotate("text", x = 3000, y = 1/9, label = 'Cases Shrinking', vjust = 1.5, color = 'blue') +  ##this code broke for some reason
        # annotate("text", x = 3000, y = 1/9, label = 'Cases Growing', vjust = -.5, color = 'red') +
        # geom_text(aes(label = base), size = 4, colour = "black", alpha = .6, check_overlap = TRUE, vjust = "top") + ##if you want text labels for plotly
        geom_label_repel(aes(new_cases_7_pp, truncate_week_rate, label = base),
                         fontface = 'bold', size = 3, fill = "white", color = "#00308f", box.padding = unit(0.75, "lines")) +
        # geom_rect(aes(xmin = 0, xmax = 10000, ymin = 0, ymax= 0), alpha = .3, color = "#00308f", show.legend = F) +
        # geom_ribbon(aes(ymin = -1, ymax = 0), fill = "blue", alpha = .5) + 
        ylab("Weekly Growth (Percent Change from Previous Week's Case Count)") + #ylim(0,.8) + #geom_line(y = 1/9) +
        xlab("New Cases (per 100,000) in Last 7 Days") + 
        ggtitle("COVID-19 Case Count Growth within 50 Miles of Installation", subtitle = paste0("Current as of ", current_date)) + 
        theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5))+ 
        theme_bw() +
        theme(plot.title = element_text(face = "bold", size = 15, family = "sans"),
              axis.title = element_text(face = "bold", size = 11, family = "sans"),
              axis.text.x = element_text(angle = 60, hjust = 1), 
              axis.line = element_line(color = "black"),
              legend.position = 'bottom',
              plot.background = element_blank(),
              panel.grid.major = element_blank(),
              panel.grid.minor = element_blank(),
              panel.border = element_blank(),
              legend.title.align = .5)
    
    #test
    # MAJCOMInput = "All"
    # MAJCOMInput = "ANG"
    
}

#### Old Code ####
#### Old Growth Chart ####  
# # Add "include" column to filter down to included bases based off of MAJCOMInput
# if (MAJCOMInput == "All"){ 
#     bases_radius <- bases_radius %>% 
#         mutate(include = ifelse((new_cases_3_pp > 500) & (date == current_date), TRUE, FALSE))
# }else if (MAJCOMInput == "Active Duty"){
#     bases_radius <- bases_radius %>% 
#         mutate(include = ifelse((new_cases_3_pp > 200) & (date == current_date) & 
#                                     (`Major Command` != "ANG") & (`Major Command` != "AFRC"), TRUE, FALSE))
# }else if ((MAJCOMInput == "ANG")){
#     bases_radius <- bases_radius %>% 
#         mutate(include = ifelse((new_cases_3_pp > 200) & (date == current_date) & 
#                                     (`Major Command` == MAJCOMInput), TRUE, FALSE))
# }else{
#     bases_radius <- bases_radius %>% mutate(include = ifelse((new_cases_3_pp > 10) & (date == current_date) & 
#                                                                  (`Major Command` == MAJCOMInput), TRUE, FALSE))
# }
# 
# min_new_30 <- min(bases_radius %>% filter(date == current_date) %>% .$new_cases_30_pp)
# max_new_30 <- pmin(max(bases_radius %>% filter(date == current_date) %>% .$new_cases_30_pp), 10000)
# max_rate <- max(bases_radius %>% filter(date== current_date) %>% .$case_growth) 
# max_deaths <- max(bases_radius %>% filter(date== current_date) %>% .$deaths_pp)
# 
# 
# bases_radius %>% mutate(cases_30_trunc = pmin(new_cases_30_pp, 10000)) %>%  # had to truncate cases at 10000 before since Mcguire was goin nuts 
#     filter(include == TRUE) %>% # just show AD AF bases
#     ggplot(aes(size = cases_30_trunc, x = new_cases_3_pp, fill = deaths_pp , y = case_growth)) + 
#     geom_point(alpha = 1, shape = 21, stroke = 1) + scale_size_binned(range = c(0, 15), name="Cases (per 100,000) in Last 30 Days",
#                                                                       breaks=seq(round(min_new_30, -3), max_new_30, round((max_new_30 - min_new_30)/3,-3)),
#                                                                       limits = c(round(min_new_30, -3), max_new_30),
#                                                                       #labels=c("2000","4000","6000","8000+"),
#                                                                       guide=guide_legend(direction = "horizontal", title.position = "top")) + 
#     scale_alpha(range = c(1, 1)) + #this line might not be needed. didn't want the alpha values to change based off of color/fill
#     scale_fill_distiller(palette = "RdBu", na.value = "#b2182b", "Deaths (per 100,000)", 
#                          limits = c(min(bases_radius$deaths_pp), max(bases_radius$deaths_pp)-100), #,
#                          guide=guide_colorsteps(title.position = "top")) +
#     scale_x_log10() + 
#     scale_y_continuous(breaks = seq(0,1,.2),labels = scales::percent) + 
#     expand_limits(y = c(0,max_rate)) + 
#     geom_hline(yintercept=1/9, linetype='dashed', col = 'black') + 
#     # annotate("text", x = 3000, y = 1/9, label = 'Cases Shrinking', vjust = 1.5, color = 'blue') +  ##this code broke for some reason
#     # annotate("text", x = 3000, y = 1/9, label = 'Cases Growing', vjust = -.5, color = 'red') +
#     # geom_text(aes(label = base), size = 4, colour = "black", alpha = .6, check_overlap = TRUE, vjust = "top") + ##if you want text labels for plotly
#     geom_label_repel(aes(new_cases_3_pp, case_growth, label = base),
#                      fontface = 'bold', size = 3, fill = "white", color = "#00308f", box.padding = unit(0.75, "lines")) +
#     ylab("Growth Rate (# Cases In 3 Days / # Cases in 30 Days)") + #ylim(0,.8) + #geom_line(y = 1/9) +
#     xlab("New Cases (per 100,000) in Last 3 Days") + 
#     ggtitle("COVID-19 Case Count Growth within 50 Miles of Installation", subtitle = paste0("Current as of ", current_date)) + 
#     theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5))+ 
#     theme_bw() +
#     theme(plot.title = element_text(face = "bold", size = 15, family = "sans"),
#           axis.title = element_text(face = "bold", size = 11, family = "sans"),
#           axis.text.x = element_text(angle = 60, hjust = 1), 
#           axis.line = element_line(color = "black"),
#           legend.position = 'bottom',
#           plot.background = element_blank(),
#           panel.grid.major = element_blank(),
#           panel.grid.minor = element_blank(),
#           panel.border = element_blank(),
#           legend.title.align = .5)
#
