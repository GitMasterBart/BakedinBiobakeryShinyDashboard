## Only run this example in interactive R sessions
library(RMySQL)
library(Maaslin2)
library(vegan)
library(tidyverse)

if(!require('shinyjs')) {
  install.packages('shinyjs')
  library('shinyjs')
}



proj = "BakedinBiobakeryDashboard"
path = "~/Desktop/BakedinBiobakeryDashboard/"



mydb = dbConnect(MySQL(),
                 user="root",
                 host="localhost",
                 password='rakker444',
                 dbname='biobakery',
                 port=3306
)




intials = dbSendQuery(mydb, "select * from biobakery_Users")

intials_table <- suppressWarnings(fetch(intials, n = Inf))

researches = dbSendQuery(mydb, "select * from biobakery_researches")

researches_table_table <- suppressWarnings(fetch(researches, n = Inf))

Records = dbSendQuery(mydb, "select * from biobakery_dumptable")
data.frame <- suppressWarnings(fetch(Records, n = Inf))
dbDisconnect(mydb)





if (interactive()){
  # table example
  
  
  
  # DataTables example
  shinyApp(
    ui = fluidPage(
      
      titlePanel("Uploading Files"),
      
      sidebarLayout(
        
        sidebarPanel(
          
          
          fileInput("file1", "Choose CSV File",
                    multiple = TRUE,
                    accept = c("text/csv",
                               "text/comma-separated-values,text/plain",
                               ".csv")),
          
          tags$hr(),
          
          
          selectInput("disp", "Users", intials_table$initials),
          conditionalPanel(condition = "output.nrows",
                           checkboxInput("headonly", "Only use first 1000 rows")),
        ),
        
        
        mainPanel(
          useShinyjs(),
          dataTableOutput("table"),
          actionButton("do", "Maaslin2"),
          plotOutput("boxplot"),
          actionButton("refresh", "Refresh")
          #tableOutput("boxplot"),
          
          
          
        ))),
    
    
    
    server = function(input, output) {
      
      
      
    
      observeEvent(input$refresh, {
        refresh()
      })
      
      
      mydb = dbConnect(MySQL(),
                       user="root",
                       host="localhost",
                       password='rakker444',
                       dbname='biobakery',
                       port=3306
      )
      
      
      
      
      intials = dbSendQuery(mydb, "select * from biobakery_Users")
      
      intials_table <- suppressWarnings(fetch(intials, n = Inf))
      
      researches = dbSendQuery(mydb, "select * from biobakery_researches")
      
      researches_table_table <- suppressWarnings(fetch(researches, n = Inf))
      
      Records = dbSendQuery(mydb, "select * from biobakery_dumptable")
      print(Records)
      data.frame <- suppressWarnings(fetch(Records, n = Inf))
      dbDisconnect(mydb)
      
      observeEvent(input$disp, {
        removeUI(
          selector = "#res"
        )
      })
      
      observeEvent(input$disp, {
        insertUI(
          selector = "#disp",
          where = "afterEnd",
          ui =  radioButtons("res", "research",
                             choices = researches_table_table$name[researches_table_table$user_id_id == intials_table$User_id[intials_table$initials == input$disp]],
                             selected = 0)
        )
      })
      
      observeEvent(input$do, {
        removeUI(
          selector = "#do"
        )
        
        insertUI(
          selector = "#res",
          where = "afterEnd",
          ui =   actionButton("plot", "plotten"),
        )
      })
      

      
      
    
      observeEvent(input$plot,{
        removeUI(
          selector = "#plot"
        )
        
        input_data = read.delim(paste0(path, "metadata2.csv"), header = T, sep = ",")
        
        project_name = researches_table_table$name[researches_table_table$name == input$res]
        
        results = read.delim(paste0(path,"output_data_", project_name, "/all_results.tsv"), header = T)
        
        treatment_res = results %>% filter(metadata == "treatment") %>% filter(qval <= 0.1)
        timepoint_res = results %>% filter(metadata == "timepoint") %>% filter(qval <= 0.1)
        interaction_res = results %>% filter(metadata == "interaction") %>% filter(qval <= 0.1)
        
        
        n_sample <- n_distinct(input_data$sample)
        results_sig=
          results %>% 
          arrange(qval) %>% 
          filter(qval <= 0.1)
        
        chunk_number <- nrow(results_sig)%/%(20 * n_sample) + 1
        
        insertUI(
          selector = "#res",
          where = "afterEnd",
          ui =  selectInput("sli", "slice",
                            choices = as.double(1:chunk_number),
                            # selected = 0
          )
        )
      }
      )
      
      
      
      output$table <- renderDataTable({
        subset_genefamilie <- data.frame %>%
          filter(family != "unclassified",
                 researches_id_id == researches_table_table$researches_id[researches_table_table$name == input$res],
                 user_id_id == intials_table$User_id[intials_table$initials == input$disp]
          ) %>%
          separate(sample, into=c("a","b","c", "d","e", "f" ,"sample", "output_type"), sep = "_") %>%
          mutate(sample=paste0(b , "_",  c, "_", d,"_", e,"_",f,"_",sample)) %>%
          select(-c(a,b,c, d, e, f,id, family, researches_id_id, user_id_id)) %>%
          #group_by(sample, gene) %>%
          filter(output_type == "Abundance-RPKs") %>%
          pivot_wider(names_from = "gene",
                      values_from = "result",
                      values_fill = 0) %>%
          select(-output_type)
        
        req(input$file1)
        # 
        df <- read.csv(input$file1$datapath,
                       header = T,
                       sep = ";", #input$sep,
                       quote = "" #input$quote
        )
        df <- as.data.frame(df, col = F)
        # 
        # 
        data_wide <- merge(df, subset_genefamilie, by="sample")
        # 
        write.csv(as.data.frame(data_wide), file="~/Desktop/BakedinBiobakeryDashboard/metadata2.csv")
        
        return(data_wide)
        
      } ,  options = list(
        pageLength = 5))
      
      observeEvent(input$do, {
        input_data = read.delim(paste0("~/Desktop/BakedinBiobakeryDashboard/metadata2.csv"), header = T, sep = ",")
        
        data =
          input_data %>%
          select(-ends_with("unclassified")) %>%
          select(-sample, -treatment, -run, -timepoint) %>%
          rename_all(., ~str_replace_all(., pattern = "_.*", "")) %>%
          as.data.frame(row.names = input_data$sample)
        
        
        metadata =
          input_data %>%
          as.data.frame(row.names = input_data$sample) %>%
          select(sample:timepoint) %>%
          mutate(interaction = as.factor(paste0(treatment, "_", timepoint)))
        # 
        
        withProgress(message = 'Making plot', value = 0.7, {
          # Number of times we'll go through the loop
          n <- 100
          project_name = researches_table_table$name[researches_table_table$name == input$res]
          print(project_name)
          if(!dir.exists(paste0(path,"output_data_",{project_name}))){dir.create(paste0(path,"output_data_",{project_name}))
            
            fit_data = Maaslin2(
              input_data = data,
              input_metadata = metadata,
              output = paste0(path, "output_data_", {project_name}),
              analysis_method = "LM",
              fixed_effects = c("treatment",
                                "timepoint",
                                
                                
                                "interaction"), # interaction dummy made for treatment (i=2) and timepoint (j=2)
              normalization = "none",
              max_significance = 0.1)}
          
          
          Sys.sleep(0.1)
          # }
          
          
        })
        
      })
      
      
      output$boxplot <- renderPlot({
        
        
        
        library(tidyverse)
        input_data = read.delim(paste0("~/Desktop/BakedinBiobakeryDashboard/metadata2.csv"), header = T, sep = ",")
        
        project_name = researches_table_table$name[researches_table_table$name == input$res]
        
        results = read.delim(paste0("~/Desktop/BakedinBiobakeryDashboard/","output_data_", project_name, "/all_results.tsv"), header = T)
        
        treatment_res = results %>% filter(metadata == "treatment") %>% filter(qval <= 0.1)
        timepoint_res = results %>% filter(metadata == "timepoint") %>% filter(qval <= 0.1)
        interaction_res = results %>% filter(metadata == "interaction") %>% filter(qval <= 0.1)
        
        
        
        results_sig=
          results %>% 
          arrange(qval) %>% 
          filter(qval <= 0.1)
        
        
        
        data =
          input_data %>%
          select(-ends_with("unclassified")) %>%
          select(-sample, -treatment, -run, -timepoint) %>%
          rename_all(., ~str_replace_all(., pattern = "_.*", "")) %>%
          as.data.frame(row.names = input_data$sample)
        
        
        totals = 
          data %>%
          as_tibble(rownames = "sample") %>% 
          pivot_longer(-sample, names_to = "feature", values_to = "expression_cpm") %>% 
          separate(sample, into = c("a" ,"treatment", "run", "timepoint", "b", "c"), sep = "_") %>%
          mutate(groups = as.factor(paste0(treatment, "_", timepoint))) %>% 
          select(treatment, run, timepoint, groups) %>% 
          distinct() %>% 
          group_by(groups) %>% 
          dplyr::summarise(n = n())
        
        results_sig = 
          results_sig %>% 
          arrange(-coef)
        
        n_sample <- n_distinct(input_data$sample)
        total <- nrow(results_sig)
        
        my_vec <- 1:total
        chunk_number <- nrow(results_sig)%/%(20 * n_sample) + 1
        print(chunk_number)
        new_list <- split(my_vec,            
                          cut(seq_along(my_vec),
                              chunk_number,
                              labels = FALSE))
        
        
        
        
        #"as.numeric(unlist(new_list[input$sli])"
        
        per_feature_plot = 
          data %>% 
          as_tibble(rownames = "sample") %>%
          pivot_longer(-sample, names_to = "feature", values_to = "expression_cpm") %>%
          separate(sample, into = c("a" ,"treatment", "run", "timepoint", "b", "c"), sep = "_") %>%
          mutate(feature = str_replace(feature, "-", ".")) %>%
          filter(feature %in% results_sig$feature) %>%
          mutate(groups = as.factor(paste0(treatment, "_", timepoint))) %>%
          # filter(feature == "AKBLIG.RXN" | feature == "ACETATEKIN.RXN" ) %>% 
          arrange(feature) %>% 
          slice(as.numeric(unlist(new_list[input$sli]))) %>% # take only top n
          ggplot(aes(x = groups, y = expression_cpm)) +
          geom_boxplot(aes(fill = groups),
                       color="black",
                       outlier.alpha = 0.0,
                       na.rm = TRUE,
                       alpha = 0.5,
                       show.legend = F,
                       width=0.5
          ) +
          geom_point(aes(fill = groups),
                     alpha = 0.75 ,
                     size = 1,
                     shape = 21,
                     stroke = 0.15,
                     color = 'black',
                     position = position_jitterdodge()
          ) +
          scale_fill_brewer(palette = "Spectral") +
          theme(panel.grid.major = element_blank(),
                panel.grid.minor = element_blank(),
                panel.background = element_blank(),
                axis.line = element_line(colour = "black"),
                legend.position = "none",
                axis.text.x = element_text(angle = 45, hjust = 1)) +
          ylab(paste0(glue::glue("Expression (cpm)")))+
          facet_wrap(~feature)  #+
        geom_text(data = head(results_sig),
                  x = Inf,
                  y = Inf,
                  # hjust = 1,
                  # vjust = 1.2,
                  aes(label = paste0("FDR q = ", round(qval, 3),
                                     "\nCoefficient (", value, ") = ", round(coef, 3))
                  ),
                  color = "gray50",
                  size = 2.5,
                  fontface = "italic")
        
        
        
        return(per_feature_plot)
        
        
        
      })
      
      
      
      
      
    })
}





#
# 


