#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http:/shiny.rstudio.com/
#

library(shiny)

countries_apx <- read_excel("appendice.xlsx", sheet = "country_nm")
product_apx <- read_excel("appendice.xlsx", sheet = "prod")
language_apx <- read_excel("appendice.xlsx", sheet = "lang")
app_nm_apx <- read_excel("appendice.xlsx", sheet = "app_nm")



load("base_table.Rdata")

# Define server logic required to draw a histogram
shinyServer(function(input, output) {


################ TAB: Base Table###############  
    output$Basetable <- DT::renderDataTable({
        Basetable <- final_base_table
        DT::datatable(Basetable)
    })


    output$Product <- renderPlot({
        
        ###########################
        base_table_p <- final_base_table
        prod_counts = c(sum(base_table_p$procuct_1_cnt),sum(base_table_p$product_2_cnt),sum(base_table_p$product_4_cnt),sum(base_table_p$product_5_cnt),sum(base_table_p$product_6_cnt),sum(base_table_p$product_7_cnt),sum(base_table_p$product_8_cnt))
        prod_counts
        df_product_counts <- data.frame(
            products = as.factor(c('Prod_1','Prod_2','Prod_4','Prod_5','Prod_6','Prod_7','Prod_8')),
            counts = prod_counts)
        
        #create a bar chart displaying most popular products excluding poker 
        bar_plots_products <- ggplot(df_product_counts,aes(x=products,y=counts, fill=products)) +
            geom_col(position='dodge') +
            ggtitle('Product count (usage) against product type') + 
            theme_light(base_size=11) +
            theme(plot.title = element_text(hjust = 0.5)) +
            ylab('Count (times played)') + 
            scale_x_discrete('Product type',labels=c('Sports book fixed-odd','Sports book live-action','Casino BossMedia','Supertoto','Games VS','Games bwin','Casino Chartwell')) +
            theme(axis.text.x = element_text(angle = 90)) +
            scale_fill_discrete(labels=c('Sports book fixed-odd','Sports book live-action','Casino BossMedia','Supertoto','Games VS','Games bwin','Casino Chartwell'))
        
        bar_plots_products
        })
    
    ################demographics###########
    output$demographics <- renderPlotly({
        df_donut <- read.csv("df_donut.csv")
        
        plot_ly(labels = df_donut$country, values = df_donut$count)  %>% 
            plotly::add_pie(hole = 0.5) %>% 
            layout(title = "Top User Distribution by Country",  showlegend = T,
                   xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                   yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
        
    })
    
   
    
    output$demographics2 <- renderPlotly({
    df_country <- read.csv("df_country.csv")
    fig_cntry <- plot_ly(
        df_country,
        x = ~reorder(Country_Name, Male),
        y = ~Male,
        type = "bar",
        name = 'Male',
        textposition = 'auto')  %>%
        add_trace(y = ~Female, name = 'Females')  %>% 
        layout(yaxis = list(title = 'Count of Users'), barmode = 'stack')
    
    fig_cntry
    
    })
    
    
    output$demographics_de <- renderPlotly({
        df_country_de <- final_base_table 
        df_country_de <- dplyr::filter(df_country_de, Country_Name == "Germany")
        df_country_de$month <- factor(strftime(df_country_de$FirstPay, '%b'), levels = month.abb)
        df_country_de <- df_country_de %>% 
            group_by(month, Gender) %>% 
            summarise(avg_bets=mean(avg_bets)
            )
        
        
        df_country_de <- pivot_wider(df_country_de,names_from = Gender, values_from = avg_bets )
        df_country_de <- df_country_de %>% replace(is.na(.), 0)
        names(df_country_de)[names(df_country_de) == 1] <- 'Male'
        names(df_country_de)[names(df_country_de) == '0'] <- 'Female'
        df_country_de$total <- df_country_de$Male + df_country_de$Female
        df_country_de <- subset(df_country_de, total > 0)
        df_country_de
        
        
        
        fig_cntry <- plot_ly(
            df_country_de,
            x = ~month,
            y = ~Male,
            type = "bar",
            name = 'Male',
            textposition = 'auto')  %>%
            add_trace(y = ~Female, name = 'Females')  %>% 
            layout(yaxis = list(title = 'Average Bets'), barmode = 'stack')
        fig_cntry
        
    })
    
    
    output$demographics_de2 <- renderPlotly({
        df_country_de <- final_base_table 
        df_country_de <- dplyr::filter(df_country_de, Country_Name == "Germany")
        df_country_de$month <- factor(strftime(df_country_de$FirstPay, '%b'), levels = month.abb)
        df_country_de <- df_country_de %>% 
            group_by(month, Gender) %>% 
            summarise(avg_bets=mean(avg_buy)
            )
        
        
        df_country_de <- pivot_wider(df_country_de,names_from = Gender, values_from = avg_bets )
        df_country_de <- df_country_de %>% replace(is.na(.), 0)
        names(df_country_de)[names(df_country_de) == 1] <- 'Male'
        names(df_country_de)[names(df_country_de) == '0'] <- 'Female'
        df_country_de$total <- df_country_de$Male + df_country_de$Female
        df_country_de <- subset(df_country_de, total > 0)
        df_country_de
        
        
        
        fig_cntry <- plot_ly(
            df_country_de,
            x = ~month,
            y = ~Male,
            type = "bar",
            name = 'Male',
            textposition = 'auto')  %>%
            add_trace(y = ~Female, name = 'Females')  %>% 
            layout(yaxis = list(title = 'Average Buys'), barmode = 'stack')
        fig_cntry
        
    })
    
    output$demographics_de3 <- renderPlotly({
        #df_country_de3
        df_country_de3 <- final_base_table 
        df_country_de3 <- dplyr::filter(df_country_de3, Country_Name == "Germany")
        df_country_de3$month <- factor(strftime(df_country_de3$FirstPay, '%b'), levels = month.abb)
        df_country_de3 <- df_country_de3 %>% 
            dplyr::group_by(month, Gender) %>% 
            dplyr::summarise(avg_bets=mean(avg_sell)
            )
        
        
        df_country_de3 <- pivot_wider(df_country_de3,names_from = Gender, values_from = avg_bets )
        df_country_de3 <- df_country_de3 %>% replace(is.na(.), 0)
        names(df_country_de3)[names(df_country_de3) == 1] <- 'Male'
        names(df_country_de3)[names(df_country_de3) == '0'] <- 'Female'
        df_country_de3$total <- df_country_de3$Male + df_country_de3$Female
        df_country_de3 <- subset(df_country_de3, total > 0)
        #df_country_de3
        
        
        
        fig_cntry <- plot_ly(
            df_country_de3,
            x = ~month,
            y = ~Male,
            type = "bar",
            name = 'Male',
            textposition = 'auto')  %>%
            add_trace(y = ~Female, name = 'Females')  %>% 
            layout(yaxis = list(title = 'Average Sells'), barmode = 'stack')
        fig_cntry
        
    })
    
    
    output$demographics_de4 <- renderPlotly({
        df_country_de <- final_base_table 
        df_country_de <- dplyr::filter(df_country_de, Country_Name == "Germany")
        df_country_de$month <- factor(strftime(df_country_de$FirstPay, '%b'), levels = month.abb)
        df_country_de <- df_country_de %>% 
            group_by(month, Gender) %>% 
            summarise(avg_bets=mean(avg_stakes)
            )
        
        
        df_country_de <- pivot_wider(df_country_de,names_from = Gender, values_from = avg_bets )
        df_country_de <- df_country_de %>% replace(is.na(.), 0)
        names(df_country_de)[names(df_country_de) == 1] <- 'Male'
        names(df_country_de)[names(df_country_de) == '0'] <- 'Female'
        df_country_de$total <- df_country_de$Male + df_country_de$Female
        df_country_de <- subset(df_country_de, total > 0)
        df_country_de
        
        
        
        fig_cntry <- plot_ly(
            df_country_de,
            x = ~month,
            y = ~Male,
            type = "bar",
            name = 'Male',
            textposition = 'auto')  %>%
            add_trace(y = ~Female, name = 'Females')  %>% 
            layout(yaxis = list(title = 'Average Stakes'), barmode = 'stack')
        fig_cntry
        
    })
    
    output$demographics_de5 <- renderPlotly({
        df_country_de <- final_base_table 
        df_country_de <- dplyr::filter(df_country_de, Country_Name == "Germany")
        df_country_de$month <- factor(strftime(df_country_de$FirstPay, '%b'), levels = month.abb)
        df_country_de <- df_country_de %>% 
            group_by(month, Gender) %>% 
            summarise(avg_bets=mean(avg_wins)
            )
        
        
        df_country_de <- pivot_wider(df_country_de,names_from = Gender, values_from = avg_bets )
        df_country_de <- df_country_de %>% replace(is.na(.), 0)
        names(df_country_de)[names(df_country_de) == 1] <- 'Male'
        names(df_country_de)[names(df_country_de) == '0'] <- 'Female'
        df_country_de$total <- df_country_de$Male + df_country_de$Female
        df_country_de <- subset(df_country_de, total > 0)
        df_country_de
        
        
        
        fig_cntry <- plot_ly(
            df_country_de,
            x = ~month,
            y = ~Male,
            type = "bar",
            name = 'Male',
            textposition = 'auto')  %>%
            add_trace(y = ~Female, name = 'Females')  %>% 
            layout(yaxis = list(title = 'Average Wins'), barmode = 'stack')
        fig_cntry
        
    })
    
    output$demographics_de6 <- renderPlot({
        df_country_de <- final_base_table 
        df_country_de <- dplyr::filter(df_country_de, Country_Name == "Germany")
        df_country_de$month <- factor(strftime(df_country_de$FirstPay, '%b'), levels = month.abb)
        df_country_de <- df_country_de %>% 
            group_by(Gender) %>% 
            summarise(count_users=length(UserID)
            )
        
        
        df_country_de$Gender[df_country_de$Gender == 0] <- "Female"
        df_country_de$Gender[df_country_de$Gender == 1] <- "Male"
#        df_country_de
        # Compute percentages
        df_country_de$fraction = df_country_de$count_users / sum(df_country_de$count_users)
        # Compute the cumulative percentages (top of each rectangle)
        df_country_de$ymax = cumsum(df_country_de$fraction)
        # Compute the bottom of each rectangle
        df_country_de$ymin = c(0, head(df_country_de$ymax, n=-1))
        
        #Compute label position
        df_country_de$labelPosition <- (df_country_de$ymax + df_country_de$ymin) / 2
        
        # Compute a good label
        df_country_de$label <- paste0(df_country_de$Gender, "\n", paste0((round(df_country_de$fraction,3)*100),"%"))
        df_country_de
        ggplot(df_country_de, aes(ymax=ymax, ymin=ymin, xmax=4, xmin=3, fill=Gender)) +
            geom_rect() +
            geom_label( x=3.5, aes(y=labelPosition, label=label), size=6) +
            scale_fill_brewer(palette=3) +
            coord_polar(theta="y") +
            theme_void() +
            theme(legend.position = "none") +
            xlim(c(2, 4))
        
    })
    
    
    output$gambleing <- renderPlot({
        #create plot that visualizes monetary value (gambler' winnings), frequency, and recency
        scatter_plot_monetary_value <- ggplot(final_base_table, aes(x=frequency,y=monetary_value,color=recency)) + 
            ggtitle('Gambler total winnings against frequency') + 
            xlab('Frequency (days)') + 
            ylab('Total winnings (Euros)') + 
            #create annotation for most extreme outlier, displaying their ID
            annotate(geom='text',x = 175, y= 1070000, label='Gambler ID with highest winnings',color='blue')
        
        #code adapted and inspired from this forum thread: https:/stackoverflow.com/questions/1923273/counting-the-number-of-elements-with-the-values-of-x-in-a-vector
        scatter_plot_monetary_value_1 <- lapply(final_base_table[1], function(data) scatter_plot_monetary_value +
                                                    geom_jitter(alpha=0.5) + 
                                                    theme_light(base_size=11) + 
                                                    theme(plot.title = element_text(hjust = 0.5)) + 
                                                    geom_text(aes(label= ifelse(final_base_table$monetary_value > quantile(final_base_table$monetary_value, 0.9999999),as.character(final_base_table$UserID),'')),hjust=0,vjust=0))
        
        scatter_plot_monetary_value_1
        
    })
    
    output$distribution <- renderPlot({
        load("base_table.Rdata")
        xvar <- input$var
        hist(final_base_table[xvar])
    })
    
    
    output$playplots <- renderPlot({
        load("base_table.Rdata")
        varx <- input$var_x
        vary <- input$var_y
        ggplot(data=final_base_table, aes(x=varx, y=vary)) + geom_bar(stat="identity") 
        

    })
    
    output$map <- renderPlot({
        data <- final_base_table
        data(wrld_simpl)
        myCountries = wrld_simpl@data$NAME %in% data$Country_Name
        plot(wrld_simpl, col = c(gray(.45), "blue")[myCountries+1])
    })
    
    output$eu_map <- renderPlot({
        rgn_plt <- final_base_table %>% 
            count(Country) %>% 
            rename(region = Country)
        rgn_plt$Category <- ifelse(rgn_plt$n <= 100, 1, ifelse(rgn_plt$n > 1000 & rgn_plt$n<20000, 2, ifelse(rgn_plt$n > 20000, 4, 3)))
        
        rgn_plt <- rename(rgn_plt, Country = region)
        
        ctry_map_df <- read_excel("appendice.xlsx", sheet = "country_nm")
        
        # Get the world map
        worldMap <- getMap()
        
        # Member States of the European Union
        europeanUnion <- c("Austria","Belgium","Bulgaria","Croatia","Cyprus",
                           "Czech Rep.","Denmark","Estonia","Finland","France",
                           "Germany","Greece","Hungary","Ireland","Italy","Latvia",
                           "Lithuania","Luxembourg","Malta","Netherlands","Poland",
                           "Portugal","Romania","Slovakia","Slovenia","Spain",
                           "Sweden","United Kingdom")
        # Select only the index of states member of the E.U.
        indEU <- which(worldMap$NAME%in%europeanUnion)
        
        # Extract longitude and latitude border's coordinates of members states of E.U. 
        europeCoords <- lapply(indEU, function(i){
            df <- data.frame(worldMap@polygons[[i]]@Polygons[[1]]@coords)
            df$region =as.character(worldMap$NAME[i])
            colnames(df) <- list("long", "lat", "region")
            return(df)
        })
        europeCoords <- do.call("rbind", europeCoords)
        
        world_map <- map_data("world")
        rgn_plt_nm <- left_join(rgn_plt, ctry_map_df, by  = "Country")
        rgn_plt_2 <- inner_join(rgn_plt_nm, europeCoords, by = c("Country Name"="region"))
        
        # Plot map
        map_plot <- ggplot() + geom_polygon(data = rgn_plt_2, aes(x = long, y = lat, group = Country, fill = Category),
                                            colour = "black", size = 0.1) +
            coord_map(xlim = c(-13, 35),  ylim = c(32, 71))
        
        map_plot <- map_plot + scale_fill_gradient(name = "Users Distribution", low = "#FF0000FF", high = "#FFFF00FF", na.value = "grey50")
        map_plot <- map_plot + theme(
            axis.text.x = element_blank(),
            axis.text.y = element_blank(), axis.ticks.x = element_blank(),
            axis.ticks.y = element_blank(), axis.title = element_blank(),
            plot.margin = unit(0 * c(-1.5, -1.5, -1.5, -1.5), "lines"))
        
        map_plot
        
    })
    
    
    # Downloadable csv of selected dataset ----
    output$downloadData <- downloadHandler(
        filename = function() {
            paste('marketing_base_table', ".csv", sep = "")
        },
        content = function(file) {
            write.csv(final_base_table, file, row.names = FALSE)
        }
    )
    
    output$plt_clustering <- renderPlotly({
        
        base_cluster <- final_base_table
        nums <- unlist(lapply(base_cluster, is.numeric))  
        base_cluster <- base_cluster[ , nums]
        data <- base_cluster
        tmp <- cor(data)
        tmp[upper.tri(tmp)] <- 0
        diag(tmp) <- 0
        
        data.new <- 
            data[, !apply(tmp, 2, function(x) any(abs(x) > 0.99, na.rm = TRUE))]
        head(data.new)
        
        scaled_data <- scale(data.new)
        
        kmeans <- kmeans(scaled_data, 5, nstart=1,iter.max = 20 )
        
        
        scaled_data_final <- final_base_table %>%
            mutate(
                cluster = kmeans$cluster
            )
        
        scaled_data_final <- scaled_data_final %>%
            group_by(cluster)  %>%
            summarise(count = n())
        
        ###########Plot CLusters###########
        plot_ly(
            scaled_data_final,
            x = ~reorder(cluster,count),
            y = ~count,
            name = "Clusters",
            type = "bar"
        )
    })    
    
    
    ##########custom clusters#################
    
    output$plt_clustering_cstm <- renderPlotly({
        
        base_cluster <- final_base_table
        nums <- unlist(lapply(base_cluster, is.numeric))  
        base_cluster <- base_cluster[ , nums]
        data <- base_cluster
        tmp <- cor(data)
        tmp[upper.tri(tmp)] <- 0
        diag(tmp) <- 0
        
        data.new <- 
            data[, !apply(tmp, 2, function(x) any(abs(x) > 0.99, na.rm = TRUE))]
        head(data.new)
        
        scaled_data <- scale(data.new)
        
        kmeans <- kmeans(scaled_data, input$ncluster, nstart=1,iter.max = 20 )
        
        
        scaled_data_final <- final_base_table %>%
            mutate(
                cluster = kmeans$cluster
            )
        
        scaled_data_final <- scaled_data_final %>%
            group_by(cluster)  %>%
            summarise(count = n())
    
        
        ###########Plot CLusters###########
        plot_ly(
            scaled_data_final,
            x = ~reorder(cluster,count),
            y = ~count,
            name = "Clusters",
            type = "bar"
        )
    })    
    
    ##########################################
        ################Plot Loyalty##################
        output$plt_loyalty <- renderPlotly({
        loyal <- final_base_table %>%
            group_by(Loyality)  %>%
            summarise(count = n())
        
        plot_ly(
            loyal,
            x = ~reorder(Loyality,count),
            y = ~count,
            name = "Clusters",
            type = "bar"
        )
        
    })
    
    output$plt_language <- renderPlotly({
        #############Plot Languages#####################
        lang <- final_base_table %>%
            group_by(`Language Description`)  %>%
            summarise(count = n())
        
        plot_ly(
            lang,
            x = ~reorder(`Language Description`, count),
            y = ~count,
            name = "Languages",
            type = "bar"
        )
        
    })
    
    output$plt_top_apps <- renderPlotly({
        
        applications_plt <- final_base_table %>%
            group_by(`Application Description`)  %>%
            summarise(count_users = n())
        
        applications_plt_subst <-  head(arrange(applications_plt, desc(count_users)), n = input$top_apps)
        
        fig <- plot_ly(applications_plt_subst, labels = ~`Application Description`, values = ~count_users, type = 'pie')
        fig <- fig %>% layout(title = paste('Top', input$top_apps ,' performing applications'),
                              xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                              yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
        fig
        
    })
    
    
    output$plt_bottom_apps <- renderPlotly({
        
        applications_plt <- final_base_table %>%
            group_by(`Application Description`)  %>%
            summarise(count_users = n())
        
        applications_plt_subst <-  head(arrange(applications_plt, count_users), n = input$bottom_apps)
        
        fig <- plot_ly(applications_plt_subst, labels = ~`Application Description`, values = ~count_users, type = 'pie')
        fig <- fig %>% layout(title = paste('Bottom ', input$bottom_apps,' performing applications'),
                              xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                              yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
        fig
        
    })
    
    output$profit_box <- renderValueBox({
        profits <- final_base_table %>%
            summarise(profit = sum(profit))
        valueBox(
            subtitle ="Total Profits",
            paste(as.character(format_dollars(profits/1000000)), "M"),
            color = "purple"
        )
    })
    
    names(final_base_table)
    
    output$buy_box <- renderValueBox({
        profits <- final_base_table %>%
            summarise(profit = sum(total_buy))
        valueBox(
            subtitle ="Total Buys",
            paste(as.character(format_dollars(profits/1000000)), "M"),
            color = "purple"
        )
    })
    
    output$sell_box <- renderValueBox({
        profits <- final_base_table %>%
            summarise(profit = sum(total_sell))
        valueBox(
            subtitle ="Total Sold",
            paste(as.character(format_dollars(profits/1000000)), "M"),
            color = "purple"
        )
    })
    
    output$wins_box <- renderValueBox({
        profits <- final_base_table %>%
            summarise(profit = sum(total_wins))
        valueBox(
            subtitle ="Total Wins",
            paste(as.character(format_dollars(profits/1000000)), "M"),
            color = "purple"
        )
    })
    
    output$stakes_box <- renderValueBox({
        profits <- final_base_table %>%
            summarise(profit = sum(total_stakes))
        valueBox(
            subtitle ="Total Stakes",
            paste(as.character(format_dollars(profits/1000000)), "M"),
            color = "purple"
        )
    })
    
    output$bets_box <- renderValueBox({
        profits <- final_base_table %>%
            summarise(profit = sum(total_bets))
        valueBox(
            subtitle ="Total Bets",
            paste(as.character(format_dollars(profits/1000000)), "M"),
            color = "purple"
        )
    })
    
    
    }) #Server End
