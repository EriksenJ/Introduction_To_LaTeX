#### Introduction to LaTeX - Exporting tables and graphs 
## Author: Jesper Eriksen - eriksenjesp@gmail.com  
## Date: Aug-04-2019




#### Description 

    # Often we need to export graphs and tables to use for our LaTeX documents. 
    # Doing so "by hand" we risk making many small mistakes and spending hours 
    # updating tables when we change a few lines of code. 
    # This file introduces plot-saving features of the ggplot2 package a
    # and LaTeX exporting features of the stargazer package that can save you from 
    # unnecessary hours of work. 



#### Load packages 

    library(tidyverse)  # Data wrangling and plotting tools 
    library(stargazer)  # Package made to export regression and summary tables
    library(haven)      # And a package for loading different data types fast(er) 
    
    
    
    
#### Load some data that we want to test out 
    
    # It contains data collected by David Cardand Alan Krueger on 
    # fast food restaurants in New Jersey (NJ) and eastern 
    # Pennsylvania(PA) during two interview waves in March and 
    # November/December of 1992. On April1, 1992 New Jersey raised its 
    # minimum wage from $4.25 to $5.05. The minimum in Pennsylvania 
    # remained at the federal level of $4.25. We will use this data to
    # analyze the impactof the minimum wage increase in New Jersey on 
    # employment in the fast food industry.
    
    dat <- read_dta("http://masteringmetrics.com/wp-content/uploads/2015/02/minwage.dta")
    
    
    ## Checking variables 
    ## Variables with a "2" are measured after reform in Nov./Dec.
    glimpse(dat) 
    
    # state: 1 indicates New Jersey (treated), 0 Pennsylvania (not treated)
    # fte: Full-time equivalent employees - sum of full time (empft) and 1/2 times part time (emppt) employees 
    # dw: Change in starting wage of employees 
    # sample: Dummy indicating that company was present before and after reform 
    
    
    ## Subset to companies present before and after reform 
    dat1 <- dat %>% 
        filter(sample == 1)
    
    ## Recode state variables 
    dat1 <- dat1 %>% 
        mutate(state = case_when(state == 1 ~ "New Jersey", 
                                 state == 0 ~ "Pennsylvania"))
    
    
    
    
#### Data analysis 
    
    ## Let's get some summary statistics calculated for us by Stargazer 
    # Summary statistics on the dataset 
    dat1 %>% 
        as.data.frame() %>% 
        stargazer(., summary = T, type = "latex", 
                  out = "Tables/Summary_1.tex")
    
    # Table with a proper title and label 
    dat1 %>% 
        as.data.frame() %>% 
        stargazer(., summary = T, type = "latex", 
                  title = "Summary statistics for firms observed twice", 
                  label = "Tab:Summary_proper",
                  out = "Tables/Summary_2.tex")
    
    # Specific summary statistics 
    dat1 %>% 
        as.data.frame() %>% 
        stargazer(., summary = T, type = "latex", mean.sd = T, median = T,
                  out = "Summary_3.tex")
    
    
    
    ## What about specific tables not calculated by stargazer? 
    # Let's get the average wages and their difference 
    d <- dat1 %>% 
        group_by(state) %>% 
        summarise(mean_1 = mean(wage_st),
                mean_2 = mean(wage_st2))
    
    # Now add a column with time differences in average wages 
    d <- d %>% 
        mutate(time_diff = mean_2 - mean_1)
    
    # And a row with the difference between states 
    d[3, 2:4] <- d[1, 2:4] - d[2, 2:4]
    d[3, 1] <- "State_diff"
    
    # Then save the resulting table
    d %>% 
        as.matrix() %>% 
        stargazer(., summary = F, type = "latex",
                  title = "Difference in mean wages", 
                  label = "Tab:Diff_wages",
                  out = "Tables/Diff_wages_1.tex")
    
    # The numbers have some long decimals - let's round them and then save 
    d[, 2:4] <- d[, 2:4] %>% round(., digits = 2)
    d %>% 
        as.matrix() %>% 
        stargazer(., summary = F, type = "latex",
                  title = "Difference in mean wages (rounded)", 
                  label = "Tab:Diff_wages_rounded",
                  out = "Tables/Diff_wages_2.tex")
    
    
    
    ## Now let's try merging two tables for plotting 
    ## We already have the wage table - let's make one 
    ## for employment and put them together 
    d_2 <- dat1 %>% 
        group_by(state) %>% 
        summarise(mean_1 = mean(empft),
                  mean_2 = mean(empft2)) %>% 
        mutate(time_diff = mean_2 - mean_1)
    d_2[3, 2:4] <- d_2[1, 2:4] - d_2[2, 2:4]
    d_2[3, 1] <- "State_diff" 
    d_2[, 2:4] <- d_2[, 2:4] %>% round(., digits = 2)
    
    
    # Now merge together with a variable indicating which is which  
    d_tot <- d %>% 
        mutate(Variable = "") %>% 
        select(Variable, everything())
    
    d_tot[1, 1] <- "Wages"
    d_tot[1, 2:5] <- NA
    d_tot[2:4, 2:5] <- d 
    d_tot[5, 1] <- "FT Employees"
    d_tot[6:8, 2:5] <- d_2 
    
    d_tot <- d_tot %>% 
        as.matrix() %>% 
        replace_na(., "")
    
    
    d_tot %>% 
        stargazer(., summary = F, 
                  title = "Differences in mean wages and full-time employment",
                  label = "Tab:Diff_table",
                  out = "Tables/Diff_table.tex")
    
    
    dat1 %>% 
        as.data.frame() %>% 
        stargazer(., summary = T, type = "text")
    
    
    
    
    
#### Running a regression and get coefficients for different setups 
    
    ## Let's run regressions for differences in wages and ft employment 
    ## First without control, secondly with contol for being co-owned 
    dat1 <- dat1 %>% 
        mutate(dfte = empft - empft2,
               chain = factor(chain), 
               co_owned = factor(co_owned),
               Treated = ifelse(state == "New Jersey", 1, 0)) 

    
    # Run regressions for the variables 
    reg_w <- lm(dw ~ Treated, data = dat1)
    reg_fte <- lm(dfte ~ Treated, data = dat1)    
    reg_w_control <- lm(dw ~ Treated + chain + co_owned, data = dat1)
    reg_fte_control <- lm(dfte ~ Treated + chain + co_owned, data = dat1)

    regs <- list(reg_w, reg_fte, reg_w_control, reg_fte_control)
    
    stargazer(regs, type = "text")    
    stargazer(regs, type = "latex", 
              title = "Difference-in-Difference regressions for wages and employment", 
              label = "Tab:DiD",
              out = "Tables/DiD_estimates.tex")
    
    
    
    
#### Finally, plots! 
    
    ## Let's get a sense of the relation between wage and ftempl changes 
    ## We'll use the package ggplot2 to do so 
    
    # Histograms 
    dat1 %>% 
        ggplot(., aes(x = dw, fill = state)) + 
        geom_histogram() + 
        facet_wrap(facets = ~state, ncol = 1) +
        theme_classic() +
        theme(legend.position = "none")
    ggsave("Plots/Hist_dw.png", width = 5.5, height = 4.3)
    
    dat1 %>% 
        ggplot(., aes(x = dfte, fill = state)) + 
        geom_histogram() + 
        facet_wrap(facets = ~state, ncol = 1) +
        theme_bw() + 
        theme(legend.position = "none")
    ggsave("Plots/Hist_dfte.png", width = 5.5, height = 4.3)
    
    
    
    # Scatterplot of wages 
    dat1 %>% 
        ggplot(., aes(x = wage_st, y = wage_st2, color = state)) +
        geom_hline(yintercept = 5.03, color = "grey", linetype = 2) + 
        geom_point() +
        theme_classic() 
    ggsave("Plots/Scatter_wages.png", width = 5.5, height = 4.3)
    
    
    # Scatterplot of dw and dfte 
    dat1 %>% 
        ggplot(., aes(x = dw, y = dfte, 
                      color = state, shape = state)) +
        geom_point() + 
        theme_classic() 
    ggsave("Plots/Scatter_dw_dfte.png", width = 5.5, height = 4.3)
    
    
    
        
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
        