##### FINAL APP SERVER #####

# Read in Data
wvs_colonies <- readRDS("wvs_colonies.rds")
plot_map <- readRDS("plot_map.rds")
colonies <- readRDS("colonies.rds")
gini <- readRDS("gini_long.rds")
gdp <- readRDS("gdp_colonies.rds")
density_count <- readRDS("density_count.rds")
rights <- readRDS("rights.rds")

# Plotting Preparation
# Defining my theme
my_theme <- function(){
  theme(
    # Plot background color 
    plot.background = element_rect(fill = NA),
    # Adding border 
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.5),
    # Panel background
    panel.background = element_rect(fill = NA),
    # Plot labels
    plot.title = element_text(color = "black", family = "serif", face = "bold"),
    plot.subtitle = element_text(color = "black", family = "serif"),
    plot.caption = element_text(color = "black", family = "serif", face = "italic"),
    # Customizing grid lines 
    panel.grid.major.y = element_line(color = "lightgrey", linetype = 1, linewidth = 0.5),
    panel.grid.minor.y = element_blank(),
    panel.grid.major.x = element_line(color = "lightgrey", linetype = 1, linewidth = 0.5),
    panel.grid.minor.x = element_blank(),
    # Customizing facets
    strip.background = element_rect(color = "black", fill = NA),
    strip.text = element_text(color = "black", hjust = 0.5, family = "serif"),
    # Customizing axes
    axis.text.y = element_text(color = "black", family = "serif", face = "bold.italic"),
    axis.text.x = element_text(color = "black", family = "serif", face = "bold.italic"),
    axis.title = element_text(color = "black", family = "serif", face = "bold"),
    axis.ticks = element_line(color = "black"),
    # Customizing legend
    legend.title = element_text(color = "black", family = "serif", face = "italic"), 
    legend.text = element_text(color = "black", family = "serif")
  )
}

# Define function to return mean scores for countries that have never been colonized in WVS scatter plot
never_mean <- function(data, filter_col, filter_value, target_col) {
  data %>%
    filter(!!sym(filter_col) == filter_value) %>% 
    ungroup() %>% 
    summarize(mean_value = mean(!!sym(target_col), na.rm = TRUE)) %>%  
    pull(mean_value)  
}

# Define function to return mean GDP for countries that have never ben colonized in GDP scatter plot
medgdp_noncolony <- gdp %>% 
  filter(length == 0) %>% 
  summarize(med2023 = median(YR2023, na.rm = TRUE)) %>% 
  pull(med2023)


# DEFINE SERVER
server <- function(input, output, session) {
  
  # World map
  output$map <- renderPlot({
    plot_map <- plot_map %>%
      mutate(fill_colonizer = ifelse(Colonizer %in% input$chosen_colonizer, as.character(Colonizer), "Unselected")) 
    
    ggplot(data = plot_map) +
      geom_sf(aes(fill = fill_colonizer), color = "black") + 
      scale_fill_manual(values = c("Unselected" = "white", "None" = "#828282", "France" = "#F2DD33", "Spain" = "#cb4b16", 
                                   "Britain" = "#2aa198", "Belgium" = "#AACD4C",
                                   "Italy" = "#56B356", "Netherlands" = "#F5A833",  
                                   "Germany" = "#6c71c4", "Portugal" = "#d33682")) +
      labs(fill = "Colonizer") +
      my_theme() +
      theme_void() +  
      theme(plot.background = element_rect(color = NA))
  }) 
  
  # display text for map
  output$map_text <- renderUI({
    HTML("<h1 style='font-size: 22px; font-weight: bold; text-align: center;'>Which countries are former colonies?</h1><br>
    Throughout history, many of the worlds nations have belonged to a handful of European states. Through colonization, or the subjugation and exploitation of indigenous populations, colonizers added land to their ever-growing empires. <br><br>  
    <b>Choose empires to see how much of the world they controlled throughout their histories.</b><br><br><br>")
  })
  
  # Page break
  output$page_break1 <- renderUI({
    HTML("<br><br>")
  })
  
  # Timeline plot
  output$timeline <- renderPlotly({
    time <- ggplot(density_count) + 
                   aes(x = years, y = colonies, color = colonizer) +
      geom_line(linewidth = 0.6) +
      scale_color_manual(values = c("France" = "#F2DD33", "Spain" = "#cb4b16", 
                                   "Britain" = "#2aa198", "Belgium" = "#AACD4C",
                                   "Italy" = "#56B356", "Netherlands" = "#F5A833",  
                                   "Germany" = "#6c71c4", "Portugal" = "#d33682")) +
      scale_x_continuous(breaks = seq(from = 1400, to = 2020, by = 100)) +
      scale_y_continuous(breaks = seq(from = 0, to = 60, by = 15)) +
      labs(title = "Empire Size Over Time",
           x = "Year", y = "Number of Active Colonies",
           fill = "Colonizer") +
      theme(plot.title = element_text(size = 16, face = "bold"), 
            axis.text = element_text(size = 12),
            axis.title = element_text(size = 14, face = "bold"), 
            legend.title = element_text(size = 12),
            legend.text = element_text(size = 12)) +
      my_theme()
    
    ggplotly(time) %>% 
      layout(legend = list(title = list(text = "Colonizer")))
  })
  
  # Timeline text
  output$timeline_text <- renderUI({
    HTML("As they say, \"Rome wasn’t built in a day.\"   In this plot, explore how European empires grew over time. <b>Double click an empire to isolate it, and hover over each line for more details.</b><br><br>
         <b>Important dates in the history of global colonization:</b><br>
         <b>1462:</b> Portugal colonizes Cape Verde, beginning their empire.<br>
         <b>1493:</b> Spain colonizes Hispaniola,  beginning their empire.<br>
         <b>1607:</b> Britain settles in Jamestown, beginning their empire.<br>
         <b>1608:</b> France settles in Nova Scotia, beginning their empire.<br>
         <b>1776-1820:</b> First wave of decolonization in the New World.<br>
         <b>1870-1914:</b> The Scramble for Africa. By 1914, 90% of the African Continent belongs to a European Empire.<br>
         <b>1945-1970:</b> After World War II, the 'Third World' is formally decolonized.<br><br>
         <i>What is decolonization?</i> The process by which colonies become independent of imperial powers. <br>
         <a href='https://www.britannica.com/topic/decolonization'>Click here to learn more about decolonization.</a>"
         )
  })
  

  # Page break
    output$page_break2 <- renderUI({
      HTML("<br>")
  })  
  
    # GDP scatter plot
    output$gdp_plot <- renderPlotly ({
      gdp <- ggplot(subset(gdp, length != 0)) +
        aes(x = length, y = YR2023, color = colonizer, text = paste("Country:", country)) +
        scale_y_log10(
          breaks = c(10^9, 10^11, 10^13),  
          labels = c("$1 billion", "$100 billion", "$10 trillion"),
          name = "GDP in 2023 (log10)") +
        scale_x_continuous(
          breaks = seq(from = 0, to = 500, by = 100),
          name = "Length of Time as Colony (Years)") +
        #geom_smooth(method = lm) +
        geom_hline(
          yintercept = medgdp_noncolony,
          color = "black",
          linewidth = 0.8) +
        geom_point(size = 1.5, alpha = 0.8) +
        scale_color_manual(values = c("France" = "#F2DD33", "Spain" = "#cb4b16", 
                                      "Britain" = "#2aa198", "Belgium" = "#AACD4C",
                                      "Italy" = "#56B356", "Netherlands" = "#F5A833",  
                                      "Germany" = "#6c71c4", "Portugal" = "#d33682")) +
        my_theme() +
        guides(color = guide_legend("Colonizer")) +
        labs(title = "Effect of Colonization on Present GDP")
      
      ggplotly(gdp)
    })
    
    # display text for gdp
    output$gdp_text <- renderUI ({
      HTML("<b>How might prior colonization affect present day GDP?</b><br><br>
      The institutions created by empires did not just disappear when countries gained independence, especially in countries that experienced colonization for longer. In particular, the economic institutions left behind have continued to affect the economies of former colonies, even to this day, in ways that vary by empire. GDP, or Gross Domestic Product, is one way to assess economies.<br><br>
    In this plot, compare the GDPs of colonies of particular empires to the median GDP of countries that have never been colonized. Explore how length of time spent as a colony may contribute to contemporary GDP. <b>Double click an empire to isolate it, and hover over each point for more details.</b><br> <a href='https://cepr.org/voxeu/columns/economic-impact-colonialism'>Click here to learn more about the economic effects of colonization </a><br><br>
    <i>What is GDP?</i> GDP, or Gross Domestic Product, is a measure of economic growth during a particular period. In general, the higher a country's GDP, the better or healthier its economy is considered to be. <a href='https://www.imf.org/en/Publications/fandd/issues/Series/Back-to-Basics/gross-domestic-product-GDP#:~:text=GDP%20is%20important%20because%20it,the%20economy%20is%20doing%20well.'>Click here to learn more about GDP.</a>")
    })  
  
  
  # Page break
  output$page_break3 <- renderUI({
    HTML("<br>")
  })
  
  # Gini over time plot
  output$gini_plot <- renderPlot({
    
    ggplot(
      gini %>% 
        filter(colonizer %in% input$gini_colonizer) %>% 
        filter(year >= input$gini_year[1] & year <= input$gini_year[2]),
      aes(x = year, y = avg_gini, color = colonizer)  
    ) +
      geom_line(linewidth = 1.2) +
      scale_color_manual(values = c("None" = "black", "France" = "#F2DD33", "Spain" = "#cb4b16", 
                                    "Britain" = "#2aa198", "Belgium" = "#AACD4C",
                                    "Italy" = "#56B356", "Netherlands" = "#F5A833",  
                                    "Germany" = "#6c71c4", "Portugal" = "#d33682")) +
      labs(
        x = "Year", y = "Gini (%)", color = "Colonizer",
        title = "Average Gini by Colonizer Over Time"
      ) +
      theme(plot.background = element_rect(color = NA, fill = NA),
            plot.title = element_text(size = 18, face = "bold"), 
            plot.subtitle = element_text(size = 14),
            axis.text = element_text(size = 14),
            axis.title = element_text(size = 16, face = "bold"), 
            legend.title = element_text(size = 14),
            legend.text = element_text(size = 12),
            plot.caption = element_text(size = 12)) +
      my_theme()  
    
  })
  
  
  # display text for gini
  output$gini_text <- renderUI ({
    HTML("<b>How might colonization have affected income inequality over time?</b><br><br>Colony-empire relationships were often explotative, and often entailed socio-economic ramifications (underclasses of indigenous populations relative to settlers and members of colonial powers). Though, non-colonized countries can also be hierarchical in similar ways.<br><br>
    In this plot, compare average Gini over time for colonies of particular empires to average Gini over time of countries that have never been colonized. <b>Choose empires and time periods to see how Gini has changed over time.</b><br><br>  
    <i>What is Gini?</i> The Gini index is a measure of income inequality. A Gini of 0 indicates perfect equality, while a Gini of 100 indicates perfect inequality. 
         <a href='https://databank.worldbank.org/metadataglossary/gender-statistics/series/SI.POV.GINI'>Click here to learn more about the Gini index</a>")
  })

  
  # WVS scatter plot
  output$wvs_plot <- renderPlotly({
    p <- ggplot(wvs_colonies, aes(x = length, y = .data[[input$wvs_value]], color = colonizer, text = paste("Country:", country))) +
      # Scatter plot of only former colonies
      geom_point(data = wvs_colonies %>% 
                   filter(colony == "Colonized"), 
                 aes(color = colonizer)) +
      # Custom colors
      scale_color_manual(values = c("France" = "#F2DD33", "Spain" = "#cb4b16", 
                                    "Britain" = "#2aa198", "Belgium" = "#AACD4C",
                                    "Italy" = "#56B356", "Netherlands" = "#F5A833",  
                                    "Germany" = "#6c71c4", "Portugal" = "#d33682")) +
      # Reference line; mean scores for countries that have never been colonized
      geom_hline(
        yintercept = never_mean(wvs_colonies, "colony", "Never Colonized", input$wvs_value),
        color = "black",
        linewidth = 0.7) +
      # Labels
      guides(color = guide_legend("Colonizer")) +
      scale_x_continuous(name = "Length of Time as Colony (Years)") +
      my_theme() + 
      theme(legend.position = "bottom")
    
    ggplotly(p)
  })
  
  # display text for WVS scatter plot
  output$wvs_text <- renderUI({
    HTML("<br><b>To what extent might a nation’s colonizer, and the length of time it spent as a colony, affect its contemporary values?</b><br><br>
    Colonial relationships lead to cultural mixing and transformation, whether through mere contact, colonial settling, or violent imposition. Economic factors and institutions may also impact values. Lastly, as countries become independent, political values are affected by the ghosts of colonial pasts and institutions.<br><br>
    The World Values Survey (WVS) is a large scale, cross-cultural survey of nations’ values, in a variety of social, political, and economic domains. Here, see how average (WVS) scores by nation compare to the average score of countries that have never been colonized (reference line). <b>Double click an empire to isolate it, and hover over each point for more details.</b>")
  })
  
  
  # display plot for constitutions
  output$cons_plot <- renderPlot({
    
    filtered_rights <- rights %>% 
      filter(years == input$cons_year)
    
    ggplot(filtered_rights, 
           aes(x = colonizer, y = value, fill = colonizer)) +
      geom_col(color = "black", linewidth = 0.5) +
      scale_fill_manual(values = c( 
                                   "France" = "#F2DD33", "Spain" = "#cb4b16", 
                                   "Britain" = "#2aa198", "Belgium" = "#AACD4C",
                                   "Italy" = "#56B356", "Netherlands" = "#F5A833",  
                                   "Germany" = "#6c71c4", "Portugal" = "#d33682")) +
      scale_y_continuous(limits = c(0, 65),
                         breaks = seq(0, 65, by = 10)) + 
      facet_wrap(~ metric) +
      labs(
        x = "Colonizer", y = "Count",
        title = "Mentions of Rights in Constitutions of Former Colonies vs. # Colonies, by Empire",
        fill = "Colonizer"
      ) +
      my_theme() +
      theme(plot.title = element_text(size = 16, face = "bold"), 
            plot.subtitle = element_text(size = 14),
            strip.text = element_text(size = 14),
            axis.text = element_text(size = 14, angle = 45, hjust = 1),
            axis.title = element_text(size = 16, face = "bold"), 
            legend.title = element_text(size = 14),
            legend.text = element_text(size = 12),
            plot.caption = element_text(size = 12))
    
  })
  
  # display text for constitutions
  output$cons_text <- renderUI({
    HTML("<b>How have governing documents, especially the inclusion of rights, changed over time? How might this relate to a country's former colonizer?</b><br><br>
    The United States of America was the first colony to decolonize*. In 1789, the Constitution was ratified, the first document of its kind. As colonies gained independence thereafter, more and more constitutions legitimized the governing powers of new nations. The ratification of such documents allowed for formerly colonized peoples to exercise <b>self determination</b>, or the right to decide one's own political destiny. Constitutions have thus become a requirement for legitimate statehood.<br<br>
    However, constitutions can demonstrate great influence from previous regimes as well as colonies' struggles for independence. One commonality amongst many post-colonial constitutions is a guarantee of rights in some way. <br><br>
    In this plot, explore how mentions of rights in the constitutions of former colonies have changed over time, in relation to their empire and its size over time. <b>Choose a year to see how the number of colonies in an empire compared to the number of constitutions with rights. Hit 'play' to watch how these numbers have changed over time.</b><br><br>
         (*Ironic, huh.)")
  })
  
  # End server code
}