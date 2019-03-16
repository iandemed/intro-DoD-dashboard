library(RSQLite)

#' Loads necessary data from our SQL database
#' 
#' @return list of data.frames that corresponds to tables and variables
#'   from our database that are relevant to our analysis
#'   
get_data_from_db <- function(){
  
  if(!file.exists("database.sqlite")){
    stop("database.sqlite not found in your local directory ")
  }
  
  conn <- dbConnect(drv = RSQLite::SQLite(), dbname = "database.sqlite")
  on.exit(dbDisconnect(conn))
  
  return(list(
    DeathRecords = dbGetQuery(conn, statement = paste("SELECT CurrentDataYear as Year, MannerOfDeath, ResidentStatus,",
                                                      "Education2003Revision as Education, MonthOfDeath, Icd10Code,", 
                                                      "Sex, AgeType, Age, RaceRecode5 as Race, MaritalStatus FROM DeathRecords")) %>%
      # Only include people with no missing values that have 
      # an age defined in years and are U.S. citizens
      filter(AgeType == 1, MaritalStatus != "U", MannerOfDeath != 5 & MannerOfDeath != 0, 
             ResidentStatus != 4, Education != 9, Race != 0) %>% select(-AgeType),
    Icd10Codes= dbGetQuery(conn, statement = "SELECT Code, Description FROM Icd10Code"),
    educ = dbGetQuery(conn, statement = "SELECT Code, Description FROM Education2003Revision"),
    race = dbGetQuery(conn, statement = "SELECT Code, Description FROM RaceRecode5"),
    marstat = dbGetQuery(conn, statement = "SELECT Code, Description FROM MaritalStatus"),
    MoD = dbGetQuery(conn, statement = "SELECT Code, Description FROM MannerOfDeath"),
    ResStatus = dbGetQuery(conn, statement = "SELECT Code, Description FROM ResidentStatus")
  ))
}

#' Removes unnecessary options from Plotly objects we create
#' 
#'  @param plotly_obj An existing plot_ly object
#'  
plotly_config <- function(plotly_obj){
  plotly::config(plotly_obj, displaylogo = FALSE, collaborate = FALSE,
                 modeBarButtonsToRemove = list("lasso2d", "SendDataToCloud", "autoScale2d", "select2d"))
}


configLandingPanel <- function(plot, xtitle=NULL, ytitle=NULL){
  if (!is.null(xtitle)){
    plot <- layout(plot, xaxis = list(title = xtitle,titlefont = list(size=12)))
  }
  if (!is.null(ytitle)){
    plot <- layout(plot, yaxis = list(title = ytitle,titlefont = list(size=12)))
  }
  
  layout(plot,legend=list(bgcolor="rgba(255,255,255,0.25)", font=list(size=10)))
}


#' Loads the data needed to create our plots
#' 
#'  @param group_name a character vector specifying how 
#'      someone has died
#'  @param Sex_chr a character vector specifying the sex of the
#'      observations being graphed
#'  @param death_lst a list object containing the necessary
#'      data.frames to create teh charts
#'  
#'  @return a list containing important statistics and data
#'      to create the plotly histogram charts
#'      
get_plot_data <- function(group_name, Sex_chr, death_lst){
  
  if (!is.list(death_lst)){
    stop("death_lst must be a list object")
  } else if (!is.data.frame(death_lst$DeathRecords) | !is.data.frame(death_lst$Icd10Codes)){
    stop("List does not contain necessary data.frames: DeathRecords, Icd10Codes")
  } else if (!str_detect(group_name, "Firearms|Poisoning") | 
             !str_detect(group_name, "Self-harm by Jumping") |
             !str_detect(group_name, "Suffocation or Drowning")){
    stop("Incorrect group_name. The following group_names are valid: Firearms, Poisoning, Self-harm by Jumping, Suffocation or Drowning")
  } else if (!str_detect(Sex_chr, "M|F")){
    stop("Sex_chr must either be 'M' or 'F'")
  }
  
  dat <- death_lst$DeathRecords %>% 
    left_join(death_lst$Icd10Code, by = c("Icd10Code" = "Code"))
    
  dat <- dat %>% 
    # Further restrict our analysis to exclude "garbage" codes as defined
    # by the Global Burden of Disease Study (Dwyer-Lindgren et al., 2016 &
    # Lozano et al., 2010)
    filter(Icd10Code >= "X60" & Icd10Code <= "X84" | Icd10Code == "Y870") %>%
    # Use ifelse() function to progressively eliminate options. For example,
    # the term "self-harm" covers every Icd10Code except the ones pertaining
    # to self-poisoning
    mutate(Description = str_to_lower(Description),
      group = ifelse(str_detect(Description, "discharge") == T, "Firearms",
                ifelse(str_detect(Description, "self-poisoning") == T, "Poisoning", 
                  ifelse(str_detect(Description, "jumping"),"Self-harm by Jumping",
                    ifelse(str_detect(Description, "hanging") | str_detect(Description, "drowning"), "Suffocation or Drowning",
                    "Other Self-Harm"
                    )
                  )
                )
              )
    )
  
  
  
  # Find the total counts for each group
  if (group_name == "Total") {
    
    # Calculate the mean for the specific sex and the total population
    total_avg_age <- mean(dat$Age)
    sex_avg_age <- mean((dat %>% filter(Sex == Sex_chr))$Age)
    
    
    dat <- dat %>% 
      group_by(Age, Sex) %>%
      summarise(count = n()) %>%
      dplyr::ungroup()
    
    # Calculate the maximum bound for our y-axis
    # so that both graphs can have a similar (and
    # thus more comparable) axis
    stats <- dat %>% group_by(Sex) %>% summarise(max = max(count), std = sd(count))
    
  } else {
    
    dat <- dat %>% filter(group == group_name)
    
    # Calculate the mean for the specific sex and the total population
    total_avg_age <- mean(dat$Age)
    sex_avg_age <- mean((dat %>% filter(Sex == Sex_chr))$Age)
    
    
    
    dat <- dat %>%
      group_by(Age, Sex, group) %>%
      summarise(count = n()) %>%
      dplyr::ungroup()
    
    # Calculate the maximum bound for our y-axis
    # so that both graphs can have a similar (and
    # thus more comparable) axis
    stats <- dat %>% group_by(Sex) %>% summarise(max = max(count))
  }
  
  
  
  return(list(
      Sex = Sex_chr,
      data = as.data.frame(dat %>% filter(Sex == Sex_chr)),
      grp_avg = round(total_avg_age,0), 
      sex_avg = round(sex_avg_age,0),
      ymax = round(0.05*max(stats$max) + max(stats$max))
    )
  )
  
}

#' Create a population period with males on the LHS and females on the RHS
#' 
#'  @param data data.frame used to create the chart
#'  @param Sex specify the sex used in the data so bars are given the proper color 
#'  @param binwidth numeric value to set  the binwidth of the charts
#'  
#'  @return a plotly object
#'  
update_histogram_plot <- function(plotly_obj = plotly_config(plot_ly()), plot_info, Sex, binwidth = 1){
  
  if (!is.list(plot_info)){
    stop("plot_info is not a list object")
  } (!is.data.frame(plot_info$data) | !is.numeric(plot_info$grp_avg) |
       !is.numeric(plot_info$sex_avg) | !is.numeric(plot_info$ymax)){
    stop("plot_info is missing one of the following elements: data, grp_avg, sex_avg, ymax")
  } else if (!str_detect(Sex_chr, "M|F")){
    stop("Sex_chr must either be 'M' or 'F'")
  } else if (!is.numeric(binwidth) | binwidth == 0){
    stop("binwidth must be a non-zero numeric value")
  }
  
  data = plot_info$data
  xmax <- max(data$Age)
  
  if (Sex == "M"){
    bar_color = "rgba(51, 153, 255, 0.5)"
    sex_label = "Male"
  } else if (Sex == "F"){
    bar_color = "rgba(255, 153, 255, 0.5)"
    sex_label = "Female"
  } else{
    stop("Sex must either be 'M' or 'F'")
  }
  
  
  plotly_obj <- plotly_obj %>%
    add_trace(x = data$Age,
             y = data$count,
             type = 'bar',
             width = binwidth,
             marker = list(color = bar_color,
                           line = list(color = bar_color, width = 1.5)),
             name = as.character(Sex))%>%
    add_segments(x = ~plot_info$grp_avg, xend = ~plot_info$grp_avg,
                 y = 0, yend = ~plot_info$ymax,
                 line = list(dash = "dash", width = 2),
                 name = paste("Mean (Total):", as.character(plot_info$grp_avg))) %>%
    add_segments(x = ~plot_info$sex_avg, xend = ~plot_info$sex_avg,
                 y = 0, yend = ~plot_info$ymax,
                 line = list(dash = "dot", width = 2),
                 name = paste0("Mean (", Sex, "): ", as.character(plot_info$sex_avg))) %>%
    layout(barmode = "Overlay",
           xaxis = list(title = "Age", range = c(0,xmax)),
           yaxis = list(title = "Count", range = c(0, ~plot_info$ymax)))
  
  return(plotly_obj)
}


#' Create a ggplot version of the histograms displayed in the app
#' 
#'  @param plot_info a list that contains two data frames necessary to contstruct
#'      the charts
#'  @param gph_title A character string used to create the title of the graphs
#'  @param barColors A character string used to specify the colors for each bar.
#'      Female is first, and Male is second 
#'  @param dashStyle A character vector specifying the style used for the mean
#'      lines. The order is as follows: Total, Female, Male
#'  @lineColors A character vecotr specifying the color used for the mean lines
#'      The order is as follows: Total, Female, Male    
#'  @param binwidth numeric value to set  the binwidth of the charts
#'  
#'  @return a ggplot object
#'  

create_histogram_chart <- function(plot_info, gph_title,
                                   barColors =  c("hotpink", "skyblue2"),
                                   dashStyle = c("solid", "dashed", "dotted"),
                                   lineColors = c("red", "black", "black"),
                                   binwidth = 1){
  
  # Give the sex variable a more meaningful label for display on
  # graphs
  dat = plot_info$dat %>% mutate(Sex = case_when( Sex == "M" ~ "Male",
                                                  Sex == "F" ~ "Female"))
  xmax <- max(dat$Age)
  
  
  p <- ggplot(data = dat) + geom_histogram(aes(x = Age, y = count, fill = Sex),
                                 stat = "identity",
                                 position = "identity", alpha = 0.5, binwidth = binwidth, width = 0.8) +
                  # Displays both the male and female versions 
                  facet_wrap(~Sex) +
                  geom_linerange(aes(x = value, y = NULL, ymin=0, ymax = plot_info$ymax,
                                     linetype = type, color = type),
                                 data = plot_info$stats) +
                  geom_hline(aes(yintercept = 0)) +
                  scale_fill_manual(name = NULL, values = barColors) + 
                  scale_x_continuous(limits = c(0, xmax)) + 
                  scale_color_manual(name = NULL, values = lineColors, labels = paste0("Mean (", plot_info$stats$type, ") = ", plot_info$stats$value)) +
                  scale_linetype_manual(name = NULL, values = dashStyle, labels = paste0("Mean (", plot_info$stats$type, ") = ", plot_info$stats$value)) +
                  theme_minimal() +
                  theme( # Remove unneccessary grid lines
                        panel.grid.major.x = element_blank(),
                        panel.grid.minor.x = element_blank(),
                        panel.grid.minor.y = element_blank(),
                         # Make changes to text and position of the legend
                        legend.margin = margin(0,0,0,0, unit = "pt"),
                        legend.position = c(0.125, 0.85),
                        legend.text = element_text(size = rel(0.6)),
                        # Make changes to the text of the titles and axes
                        plot.title = element_text(hjust = 0.5, lineheight = 1, size = rel(1.5)),
                        axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 0, unit = "pt")),
                        axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0, unit = "pt")),
                        strip.text = element_text(size = rel(1), face = "bold")) +
                  guides(fill = FALSE) + 
                  labs(title = gph_title, y = "Count", x = "Age" )
                  
  return(p)  
}

#'
#' Creates the specified histograms using `create_histogram_chart() `and saves them in a pdf
#' 
#' @param death_lst List of data from `get_data_from_db()`
#' @param file_name a character string specifiying the name the user
#'     wishes to save the file name as 
#' @param types a character vector of types that are used to contsruct
#'     the pdf file
#'     

create_histogram_pdf <- function(death_lst, file_name = "DoD_hist.pdf", 
                                 types = c("Total", "Firearms", "Poisoning", "Self-harm by Jumping", "Suffocation or Drowning")){
  
  # Initialize the list of figures
  hist_gphs <- list()
  
  for (i in 1:length(types)){
    
    male_dat <- get_plot_data(types[i], "M", death_lst)
    female_dat <- get_plot_data(types[i], "F", death_lst)
    
    # Create the list object needed in our function
    plot_info <- list(
      dat = bind_rows(male_dat$data, female_dat$data),
      # Combine the statistics in to a data.frame for easy referencing later
      stats = data.frame(type = c("Total","Male","Female"), 
                         value = c(male_dat$grp_avg, male_dat$sex_avg, female_dat$sex_avg)),
      ymax = male_dat$ymax
    )
    
    # Give the graph a more meaningful title
    gph_title <- paste0("Number of Deaths (", types[1], ")")
    
    # Creates our graphs and saves them in a list
    hist_gphs[[i]] <- create_histogram_chart(plot_info, gph_title)
    
  }
  
  # Print the graphs to a pdf
  pdf(file_name, height = 8.5, width = 11)
  for (i in 1:length(types)){
    plot(hist_gphs[[i]])
    gph <- recordPlot()
    replayPlot(gph)
  }
  dev.off()
  graphics.off()
  
}
  