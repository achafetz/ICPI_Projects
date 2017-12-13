##  MWI HTS_POS Reporting Issue 
##  A.Chafetz, USAID
##  Purpose: visualize FY17 reporting issue for HTS_POS
##  Created: 10/18/17 


pacman::p_load(tidyverse)

#import
  df_mer  <- read_tsv("~/ICPI/Data/ICPI_FactView_OU_IM_20171115_v1_1.txt") %>% rename_all(tolower)
  
  
#clean up
  df_mwi <- df_mer %>%
    filter(operatingunit == "Malawi", indicator %in% c("HTS_TST", "HTS_TST_POS"), standardizeddisaggregate == "Total Numerator") %>%
    select(-fy2015q2, -contains("targets"), -contains("apr")) %>% 
    mutate_at(vars(starts_with("fy2015")), funs(as.integer(.))) %>% 
    group_by(operatingunit, indicator) %>% 
    summarize_at(vars(fy2015q3:fy2017q4), funs(sum(., na.rm=TRUE))) %>%
    ungroup %>%
    gather(pd, values, fy2015q3:fy2017q4) %>%
    filter(values !=0) %>%
    separate(pd, c("fy", "qtr"), -3, remove = FALSE)

#visuals 
  
  #HTS + HTS_POS
  p1 <- df_mwi %>%
    ggplot()  + 
      geom_col(mapping = aes(x = pd, y = values, group = indicator)) +
      facet_grid(indicator ~ ., scales = "free_y") +
      scale_y_continuous(labels = scales::comma) + 
      scale_x_discrete(labels = c("FY15Q3", "Q4", "FY16Q1","Q2", "Q3", "Q4","FY17YQ1", "Q2", "Q3", "Q4")) + 
      labs(title = "PEPFAR Reported Testing in Malawi", x = "", y = "", caption = "Source: ICPI Fact View FY17Q4v1.1")
    
  #HTS_POS
  p2 <- df_mwi %>%
    filter(indicator== "HTS_TST_POS", fy!="fy2015") %>%
    ggplot()  + 
      geom_col(mapping = aes(x = qtr, y = values, group = fy)) +
      facet_wrap(~fy) +
      scale_y_continuous(labels = scales::comma) +
      labs(title = "Issue around FY17 HTS_TST_POS Reporting in Malawi", x = "Quarter", y = "HTS_TST_POS (Total Num)", 
        caption = "Source: ICPI Fact View FY17Q4v1.1")

  #HTS Positivity
  p3 <- df_mwi %>% 
    spread(indicator, values) %>%
    mutate(HTS_TST_YIELD = HTS_TST_POS/HTS_TST) %>%
    gather(indicator, values, HTS_TST, HTS_TST_POS,HTS_TST_YIELD) %>%
    filter(indicator== "HTS_TST_YIELD") %>%
    ggplot()  + 
      geom_line(mapping = aes(x = pd, y = values, group=1)) +
      geom_point(mapping = aes(x = pd, y = values)) +
      scale_y_continuous(labels = scales::percent) + 
      scale_x_discrete(labels = c("FY15Q3", "Q4", "FY16Q1","Q2", "Q3", "Q4","FY17YQ1", "Q2", "Q3", "Q4")) + 
      labs(title = "PEPFAR Testing Positivity in Malawi", x = "Period", y = "Positivity (HTS_POS/HTS_TST)", caption = "Source: ICPI Fact View FY17Q4v1.1")

  multiplot(p1, p3, cols=2)
  
  
  
  multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
    library(grid)
    
    # Make a list from the ... arguments and plotlist
    plots <- c(list(...), plotlist)
    
    numPlots = length(plots)
    
    # If layout is NULL, then use 'cols' to determine layout
    if (is.null(layout)) {
      # Make the panel
      # ncol: Number of columns of plots
      # nrow: Number of rows needed, calculated from # of cols
      layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                       ncol = cols, nrow = ceiling(numPlots/cols))
    }
    
    if (numPlots==1) {
      print(plots[[1]])
      
    } else {
      # Set up the page
      grid.newpage()
      pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
      
      # Make each plot, in the correct location
      for (i in 1:numPlots) {
        # Get the i,j matrix positions of the regions that contain this subplot
        matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
        
        print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                        layout.pos.col = matchidx$col))
      }
    }
  }  