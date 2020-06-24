HotspotPlot <- function(BranchSelect,OpsSelect,MAJNAFSelect,MAJCOMInput,NAFChoice,WingChoice,GroupChoice){
    
    #this morning, cases were updated before deaths so I added in this code to pull the most current reported deaths date
    current_date = (bases_radius %>% ungroup() %>% filter(deaths_pp > 0) %>% filter(date ==max(date)) %>% select(date))$date[1]
    #### Week % Change Chart ####
    
    
    # BranchSelect<-"Air Force"
    # OpsSelect <- "Guard"   
    # MAJNAFSelect<-"MAJCOM"
    # MAJCOMInput<-"ACC"

    
    ####Need to add filtering for branch/operational status/then majcom/NAF
    basesRadius<-dplyr::filter(bases_radius,Branch %in% BranchSelect) #

    if (BranchSelect!="Air Force"){
        if (OpsSelect != "All"){ 
            basesRadius<-dplyr::filter(basesRadius,Operational %in% OpsSelect)
        }
        basesRadius <- basesRadius %>% 
            mutate(include = ifelse((new_cases_7_pp > 100) & (date == current_date), TRUE, FALSE))
        
    } else if (BranchSelect=="Air Force"){
        if (MAJNAFSelect=="MAJCOM"){
            if (OpsSelect != "All"){ 
                basesRadius<-dplyr::filter(basesRadius,Operational %in% OpsSelect)
            }
            if (MAJCOMInput == "All"){ 
                basesRadius <- basesRadius %>% 
                    mutate(include = ifelse((new_cases_7_pp > 100) & (date == current_date), TRUE, FALSE))
            }
            # else if (MAJCOMInput == "Active Duty"){
            #     basesRadius <- basesRadius %>% 
            #         mutate(include = ifelse((new_cases_7_pp > 500) & (date == current_date) & 
            #                                     (`Major Command` != "ANG") & (`Major Command` != "AFRC"), TRUE, FALSE))
            # }else if ((MAJCOMInput == "ANG")){
            #     basesRadius <- basesRadius %>% 
            #         mutate(include = ifelse((new_cases_7_pp > 1000) & (date == current_date) & 
            #                                     (`Major Command` == MAJCOMInput), TRUE, FALSE))
            # }
            else{
                basesRadius <- basesRadius %>% mutate(include = ifelse((new_cases_7_pp > 0) & (date == current_date) & 
                                                                             (`Major Command` == MAJCOMInput), TRUE, FALSE))
            }
        } else if (MAJNAFSelect=="NAF"){
            AFWngs<-dplyr::filter(AFNAFS,NAF %in% NAFChoice)
            if (WingChoice=="All") {
                WingList2 <- sort(unique(AFWngs$Wing), decreasing = FALSE)
                forecastbaselist<-dplyr::filter(AFWngs,Wing %in% WingList2)
                if (GroupChoice!="All") {
                    forecastbaselist<-dplyr::filter(forecastbaselist,Group %in% GroupChoice)
                } else {
                    AFGrps<-dplyr::filter(AFWings,Wing %in% WingList)
                    GroupList <- sort(unique(AFGrps$Group), decreasing = FALSE)
                    forecastbaselist<-dplyr::filter(forecastbaselist,Group %in% GroupList)
                }
                forecastbaselist<-sort(unique(forecastbaselist$Base), decreasing = FALSE) 
                basesRadius<-dplyr::filter(basesRadius,base %in% forecastbaselist) 
                basesRadius <- basesRadius %>% 
                    mutate(include = ifelse((new_cases_7_pp > 10) & (date == current_date), TRUE, FALSE))
            } else {
                forecastbaselist<-dplyr::filter(AFWngs,Wing %in% WingChoice) 
                if (GroupChoice!="All") {
                    forecastbaselist<-dplyr::filter(forecastbaselist,Group %in% GroupChoice)
                } else {
                    AFGrps<-dplyr::filter(AFWings,Wing %in% WingChoice)
                    GroupList <- sort(unique(AFGrps$Group), decreasing = FALSE)
                    forecastbaselist<-dplyr::filter(forecastbaselist,Group %in% GroupList)
                }
                forecastbaselist<-sort(unique(forecastbaselist$Base), decreasing = FALSE) 
                basesRadius<-dplyr::filter(basesRadius,base %in% forecastbaselist)       
                basesRadius <- basesRadius %>% 
                    mutate(include = ifelse((new_cases_7_pp > 10) & (date == current_date), TRUE, FALSE))
            }            
        }            
    } 
    
    
    min_new_14 <- min(basesRadius %>% filter(date == current_date) %>% .$new_cases_14_pp)
    max_new_14 <- pmin(max(basesRadius %>% filter(date == current_date) %>% .$new_cases_14_pp), 10000)
    min_week_rate <- pmax(min(basesRadius %>% filter(date== current_date) %>% .$case_growth_week, na.rm = TRUE), -2) 
    max_week_rate <- pmin(max(basesRadius %>% filter(date== current_date) %>% .$case_growth_week, na.rm = TRUE), 2)
    max_deaths <- max(basesRadius %>% filter(date== current_date) %>% .$deaths_pp)
                                   
    basesRadius %>%  # had to truncate cases at 10000 before since Mcguire was goin nuts 
        filter(include == TRUE) %>% # filter for "included" bases from IF statement above
        mutate(truncate_week_rate = pmin(case_growth_week-1, 2)) %>%
        ggplot(aes(size = new_cases_14_pp, x = new_cases_7_pp, fill = deaths_pp , y = truncate_week_rate)) + 
        geom_point(alpha = 1, shape = 21, stroke = 1) + scale_size_binned(range = c(0, 15), name="Cases (per 100,000) in Last 14 Days",
                                                                          # n.breaks=4,
                                                                          # nice.breaks = TRUE,
                                                                          limits = c(round(min_new_14, -3), max_new_14),
                                                                          #labels=c("2000","4000","6000","8000+"),
                                                                          guide=guide_legend(direction = "horizontal", title.position = "top")) + 
        scale_alpha(range = c(1, 1)) + #this line might not be needed. didn't want the alpha values to change based off of color/fill
        scale_fill_distiller(palette = "RdBu", na.value = "#b2182b", "Cumulative Deaths (per 100,000)", 
                             limits = c(min(basesRadius$deaths_pp), max(basesRadius$deaths_pp)), #,
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
#     basesRadius <- basesRadius %>% 
#         mutate(include = ifelse((new_cases_3_pp > 500) & (date == current_date), TRUE, FALSE))
# }else if (MAJCOMInput == "Active Duty"){
#     basesRadius <- basesRadius %>% 
#         mutate(include = ifelse((new_cases_3_pp > 200) & (date == current_date) & 
#                                     (`Major Command` != "ANG") & (`Major Command` != "AFRC"), TRUE, FALSE))
# }else if ((MAJCOMInput == "ANG")){
#     basesRadius <- basesRadius %>% 
#         mutate(include = ifelse((new_cases_3_pp > 200) & (date == current_date) & 
#                                     (`Major Command` == MAJCOMInput), TRUE, FALSE))
# }else{
#     basesRadius <- basesRadius %>% mutate(include = ifelse((new_cases_3_pp > 10) & (date == current_date) & 
#                                                                  (`Major Command` == MAJCOMInput), TRUE, FALSE))
# }
# 
# min_new_30 <- min(basesRadius %>% filter(date == current_date) %>% .$new_cases_30_pp)
# max_new_30 <- pmin(max(basesRadius %>% filter(date == current_date) %>% .$new_cases_30_pp), 10000)
# max_rate <- max(basesRadius %>% filter(date== current_date) %>% .$case_growth) 
# max_deaths <- max(basesRadius %>% filter(date== current_date) %>% .$deaths_pp)
# 
# 
# basesRadius %>% mutate(cases_30_trunc = pmin(new_cases_30_pp, 10000)) %>%  # had to truncate cases at 10000 before since Mcguire was goin nuts 
#     filter(include == TRUE) %>% # just show AD AF bases
#     ggplot(aes(size = cases_30_trunc, x = new_cases_3_pp, fill = deaths_pp , y = case_growth)) + 
#     geom_point(alpha = 1, shape = 21, stroke = 1) + scale_size_binned(range = c(0, 15), name="Cases (per 100,000) in Last 30 Days",
#                                                                       breaks=seq(round(min_new_30, -3), max_new_30, round((max_new_30 - min_new_30)/3,-3)),
#                                                                       limits = c(round(min_new_30, -3), max_new_30),
#                                                                       #labels=c("2000","4000","6000","8000+"),
#                                                                       guide=guide_legend(direction = "horizontal", title.position = "top")) + 
#     scale_alpha(range = c(1, 1)) + #this line might not be needed. didn't want the alpha values to change based off of color/fill
#     scale_fill_distiller(palette = "RdBu", na.value = "#b2182b", "Deaths (per 100,000)", 
#                          limits = c(min(basesRadius$deaths_pp), max(basesRadius$deaths_pp)-100), #,
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
