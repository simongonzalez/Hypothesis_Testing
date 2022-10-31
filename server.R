shinyServer(function(input, output, session) {
  
  #Functions--------------------------------------------------------------------------------------------
  #-----------------------------------------------------------------------------------------------------
  #-----------------------------------------------------------------------------------------------------
  
  #calculate sds for each formant
  sd_classify = function(data, sd_value){
    #calculates acoustic frequencies sds for each sex and vowel--------------------------------------------------------------------------------------------
    frequencies_sum_f1 = as.data.frame(data %>% group_by(vowel, sex) %>% summarise(mean(F1, na.rm = TRUE), sd(F1, na.rm = TRUE), 
                                                                                   mean(F1, na.rm = TRUE)-(sd_value*sd(F1, na.rm = TRUE)), 
                                                                                   mean(F1, na.rm = TRUE)+(sd_value*sd(F1, na.rm = TRUE))))
    
    frequencies_sum_f2 = as.data.frame(data %>% group_by(vowel, sex) %>% summarise(mean(F2, na.rm = TRUE), sd(F2, na.rm = TRUE), 
                                                                                   mean(F2, na.rm = TRUE)-(sd_value*sd(F2, na.rm = TRUE)), 
                                                                                   mean(F2, na.rm = TRUE)+(sd_value*sd(F2, na.rm = TRUE))))
    
    names(frequencies_sum_f1) = c('vowel', 'sex', 'mean_f', 'sd_f', 'sdMINUS2', 'sdPLUS2')
    names(frequencies_sum_f2) = c('vowel', 'sex', 'mean_f', 'sd_f', 'sdMINUS2', 'sdPLUS2')
    
    data$f1_class = 'in'
    data$f2_class = 'in'
    
    for(i in 1:nrow(frequencies_sum_f1)){
      data[data$vowel == frequencies_sum_f1$vowel[i] & data$sex == frequencies_sum_f1$sex[i] & data$F1 < frequencies_sum_f1$sdMINUS2[i], c('f1_class')] = 'out'
      data[data$vowel == frequencies_sum_f1$vowel[i] & data$sex == frequencies_sum_f1$sex[i] & data$F1 > frequencies_sum_f1$sdPLUS2[i], c('f1_class')] = 'out'
      data[data$vowel == frequencies_sum_f2$vowel[i] & data$sex == frequencies_sum_f2$sex[i] & data$F2 < frequencies_sum_f2$sdMINUS2[i], c('f2_class')] = 'out'
      data[data$vowel == frequencies_sum_f2$vowel[i] & data$sex == frequencies_sum_f2$sex[i] & data$F2 > frequencies_sum_f2$sdPLUS2[i], c('f2_class')] = 'out'
    }
    
    data = data[data$f1_class == 'in',]
    data = data[data$f2_class == 'in',]
    
    return(data)
  }
  #-----------------------------------------------------------------------------------------------------
  #-----------------------------------------------------------------------------------------------------
  #-----------------------------------------------------------------------------------------------------
  #-----------------------------------------------------------------------------------------------------
  #-----------------------------------------------------------------------------------------------------
  
  d = read.csv('d.csv')
  
  output$frm <- renderText({
    response_str = unlist(strsplit(input$response_variable, " "))
    
    fixed_str = paste(unlist(strsplit(input$fixed_variable, " ")), collapse = ' + ')
    
    random_str = paste0('(', input$random_intercept, ')')
    
    frm = paste(response_str[1], ' ~ ', fixed_str, ' + ', random_str)
    
    print(frm)
  })
  
  
  
  
  prepare_data = reactive({
    
    df = d
    
    if(input$binary_in != ""){
      #get binary-------------------------------------------------------------
      binary_column = gsub(" ", "", unlist(strsplit(input$binary_in, " ")))[1]
      binary_value = as.numeric(gsub(" ", "", unlist(strsplit(input$binary_in, " ")))[2])
      
      #subset data
      df = df[df[[binary_column]] == binary_value,]
    }
    
    if(input$categorical_in != ""){
      #get categorical-------------------------------------------------------------
      categorical_column = gsub(" ", "", unlist(strsplit(input$categorical_in, " ")))[1]
      categorical_value = as.numeric(gsub(" ", "", unlist(strsplit(input$categorical_in, " ")))[2])
      
      #subset data
      df = df[df[[categorical_column]] == categorical_value,]
    }
    
    if(input$discrete_in != ""){
      #get discrete-------------------------------------------------------------
      discrete_column = gsub(" ", "", unlist(strsplit(input$discrete_in, " ")))[1]
      discrete_value = as.numeric(gsub(" ", "", unlist(strsplit(input$discrete_in, " ")))[2])
      
      #subset data
      df = df[df[[discrete_column]] <= discrete_value,]
    }
    
    if(input$continuous_in != ""){
      #get continuous-------------------------------------------------------------
      #find the two components of the formula
      formula_in = gsub("\\(", "", unlist(strsplit(input$continuous_in, " "))[1])
      sd_in = as.numeric(gsub("\\)", "", unlist(strsplit(input$continuous_in, " "))[2]))
      
      #exclude values beyond the sd
      df = sd_classify(df,sd_in)
    }
    
    
  })
  
  
  hypothesis_testing = reactive({
    
    withProgress(message = 'Calculating', value = 0, {
      df = prepare_data()
      
      if(input$response_variable != ""){
        response_str = unlist(strsplit(input$response_variable, " "))
      }else{
        return()
      }
      
      if(input$random_intercept != ""){
        random_str = paste0('(', input$random_intercept, ')')
      }else{
        return()
      }
      
      
      if(input$sig_factor_test != ""){
        sig_factor_str = unlist(strsplit(input$sig_factor_test, " "))
      }else{
        return()
      }
      
      
      sig_df = c()
      sig_cntr = 1
      plot_values_lst = list()
      

      
      for(response_i in response_str){
        
        if(input$fixed_variable != ""){
          fixed_str = paste(unlist(strsplit(input$fixed_variable, " ")), collapse = ' + ')
        }else{
          return()
        }
        
        
        frm = as.formula(paste(response_i, ' ~ ', fixed_str, ' + ', random_str))
        
        #loop though split parameters
        for(splt_param_i in unique(df[[input$split_parameter]])){
          tmp_df = df[df[[input$split_parameter]] == splt_param_i,]

          mdl = lmer(frm, data = tmp_df)
          
          for(sig_factor_i in sig_factor_str){
            
            if(input$fixed_variable != ""){
              
              fixed_split = unlist(strsplit(input$fixed_variable, " "))
              fixed2 = fixed_split
              
              for(str_splt_i in 1:length(fixed_split)){
                if(identical(fixed2[str_splt_i], sig_factor_i))
                  fixed2 = fixed2[-str_splt_i]
              }
              fixed2 = paste(fixed2, collapse = ' ')
              
              fixed_str2 = paste(unlist(strsplit(fixed2, " ")), collapse = ' + ')
            }else{
              return()
            }
            
            
            
            
            frm2 = as.formula(paste(response_i, ' ~ ', fixed_str2, ' + ', random_str))
            
            mdl2 = lmer(frm2, data = tmp_df)
            
            
            
            
            mdl_test = anova(mdl, mdl2)
            
            
            
            chisqMdl = mdl_test$`Pr(>Chisq)`[2]
            
            
            
            if(chisqMdl < 0.051){
 
              sig_df_tmp = data.frame(response_i, splt_param_i, sig_factor_i, sig_cntr)
              
              sig_cntr = sig_cntr + 1
              sig_df = rbind(sig_df, sig_df_tmp)
            }

            incProgress(1/unique(df[[input$split_parameter]]))
          }
        }
      }
      
      return(sig_df)
      
    })
  })

  output$sum_table_num = DT::renderDataTable(
    data.frame(unclass(summary(select_if(d, is.numeric))), check.names = FALSE, stringsAsFactors = FALSE), options = list(lengthChange = T, dom = 'tip'),
    rownames = FALSE, style = "bootstrap"
  )
  
  output$sum_table_nonnum = DT::renderDataTable(
    data.frame(unclass(summary(d[,!sapply(d, is.numeric)])), check.names = FALSE, stringsAsFactors = FALSE), options = list(lengthChange = T, dom = 'tip'),
    rownames = FALSE, style = "bootstrap"
  )
  
  output$results_table = DT::renderDataTable(
    hypothesis_testing(), options = list(lengthChange = T, dom = 'tip'),
    caption = 'Statistical Analysis Results', rownames = FALSE, colnames = c('Response Varibale', 'Split Parameter', 'Plot Number'),
    style = "bootstrap"
  )
  
  
  
  #--------------
  output$plt_nmber_rslts = renderUI({
    if(is.null(hypothesis_testing()))
      return()
    
    d_in = hypothesis_testing()
    print(d_in)
    selectInput("plot_number_result", label = h5("Plot number"), 
                choices = c(1:nrow(d_in)), 
                selected = 1)
  })
  
  
  
  output$unfiltered_dt = renderUI({
    if(length(input$plot_type) > 1)
      return(NULL)
    
    checkboxInput('unfiltered_data', label = 'Compare with unfiltered data', value = F)
    
  })
  
  
  output$vowel_space_results = renderPlotly({
    
    if(is.null(hypothesis_testing()))
      return()
    
    withProgress(message = 'Plotting', value = 0, {
      d_in = prepare_data()
      
      table_results = hypothesis_testing()
      print('-------')
      print(table_results)
      print('=======')
      d_in_subset = d_in[d_in[[input$split_parameter]] == as.character(table_results[table_results$sig_cntr == input$plot_number_result, c('splt_param_i')]),]
      
      #if to compare with all data
      if(length(input$plot_type) == 1 & input$unfiltered_data == T){
        d_ttl = d[d[[input$split_parameter]] == as.character(table_results[table_results$sig_cntr == input$plot_number_result, c('splt_param_i')]),]
      }
      
      get_interaction = as.character(table_results[table_results$sig_cntr == input$plot_number_result, c('sig_factor_i')])
      interaction_label = paste(unlist(strsplit(get_interaction, ":")))
      plot_interaction = paste0('interaction(', gsub(':', ',', get_interaction), ')')
      
      #calculate means----------------------------------------
      if(length(which(2 %in% input$plot_type))){
        
        d_means_f1 = as.data.frame(d_in_subset %>% group_by_(.dots=interaction_label) %>% summarise(mean(F1, na.rm = TRUE)))
        d_means_f2 = as.data.frame(d_in_subset %>% group_by_(.dots=interaction_label) %>% summarise(mean(F2, na.rm = TRUE)))
        d_means = merge(d_means_f1, d_means_f2)
        names(d_means) = c(interaction_label, 'F1', 'F2')
        
        if(length(input$plot_type) == 1 & input$unfiltered_data == T){
          d_means_f1_unfiltered = as.data.frame(d_ttl %>% group_by_(.dots=interaction_label) %>% summarise(mean(F1, na.rm = TRUE)))
          d_means_f2_unfiltered = as.data.frame(d_ttl %>% group_by_(.dots=interaction_label) %>% summarise(mean(F2, na.rm = TRUE)))
          d_means_unfiltered = merge(d_means_f1_unfiltered, d_means_f2_unfiltered)
          names(d_means_unfiltered) = c(interaction_label, 'F1', 'F2')
        }
      }
      
      x = 'F2'
      y = 'F1'
      
      incProgress(10)
      
      p = ggplot(d_in_subset, aes_string(x = x, y = y, color = plot_interaction, fill = plot_interaction)) + 
        scale_x_reverse( lim=c(2000,250)) +
        scale_y_reverse( lim=c(800,200))
      #incProgress(20)
      
      #----------------------
      #add tokens
      if(length(which(1 %in% input$plot_type))){
        
        #compare with total data
        if(length(input$plot_type) == 1 & input$unfiltered_data == T){
          p = p + geom_point(data = d_ttl, col = 'black', alpha = 0.1, size = 2)
        }
        
        p = p + geom_point(size = 2, alpha = 0.5) 
      }
      incProgress(30)
      #----------------------
      #add means
      if(length(which(2 %in% input$plot_type))){
        
        #compare with total data
        if(length(input$plot_type) == 1 & input$unfiltered_data == T){
          p = p + geom_point(data = d_means_unfiltered, col = 'black', alpha = 0.1, size = 7)
        }
        
        p = p + geom_point(data = d_means, size = 7)
      }
      incProgress(40)
      #----------------------
      #add ellipses
      if(length(which(3 %in% input$plot_type))){
        
        #compare with total data
        if(length(input$plot_type) == 1 & input$unfiltered_data == T){
          p = p + stat_ellipse(data = d_ttl, geom = "polygon", col = 'black', level = 0.95, alpha = 0.1)
        }
        
        p = p + stat_ellipse(geom = "polygon", level = 0.95, alpha = 0.5)
      }
      incProgress(50)
      #----------------------
      #add densities
      if(length(which(4 %in% input$plot_type))){
        
        #compare with total data
        if(length(input$plot_type) == 1 & input$unfiltered_data == T){
          p = p + geom_density2d(data = d_ttl, col = 'black', alpha = 0.1)
        }
        
        p = p + geom_density2d()
      }
      
      p = p + annotate("text", x = 1800, y = 800, label = paste0('n = ', nrow(d_in_subset)), hjust = 0.5)
      
      response_title = table_results[table_results$sig_cntr == input$plot_number_result, c('response_i')]
      parameter_title = table_results[table_results$sig_cntr == input$plot_number_result, c('splt_param_i')]
      factor_title = table_results[table_results$sig_cntr == input$plot_number_result, c('sig_factor_i')]
      p = p + labs(title = paste(response_title, parameter_title, factor_title, sep = ' - '))
      
      p = p + theme(legend.title = element_blank())
      
      incProgress(100)
      
      ggplotly(p)
      
      
    })
  })
  
  output$no_sig = renderUI({
    
    if(!is.null(hypothesis_testing()))
      return()
    
    verbatimTextOutput("no_significant")
  })
  
  output$no_significant <- renderText({ 
    if(!is.null(hypothesis_testing()))
      return()
    
    'No significant differences with these settings' 
  })
  
  output$qrreader = output$qrreader2 = renderPlot({
    par(mar=c(0,0,0,0))
    image(qrencode_raster("https://phoneapps.shinyapps.io/hypothesis_automation/"), 
          asp=1, col=c("white", "black"), axes=FALSE, 
          xlab="", ylab="")
  })
  
  
  output$text1 <- renderUI({ 
    
    
  text_out = p("Hypothesis testing is a crucial and time-consuming process in any sociophonetic study. 
                                                            One major reason for this is the number of plausible variables that need to be tested 
       which could account for phonetic variation in a given speech community. Thus, time is 
       spent carrying out multiple calculations with different parameters on different datasets, 
       as well as on sifting through, filtering, visualising and statistically analysing the data. 
       At any of these stages, researchers are faced with multiple decisions regarding how to treat 
       the data, each of which may affect the way the data is processed at a later stage in the analysis. 
       A change to any one decision may require changes at multiple other points in the data analysis chain. 
       However, many of these process can be automated, potentially reducing time spent preparing the 
       data for analysis. In this poster, we propose a system for automating hypothesis testing using 
       acoustic data from Horvath (1985) for the Sydney Speaks project (Travis 2016). In the proposed 
       system, the user interacts with an interface which has the option of inputting all the variables 
       of interest, and then partitioning that data out along criteria determined by the user. 
       The interface then allows the user to both visualise and analyse the data by changing the relevant 
       parameters without having to reprocess the data. The interface further corroborates visual trends 
       using statistical models, isolates and reports on whether these differences are significant, and 
       takes the user to the relevant settings, both visualising and analysing these differences. An 
       approach like this increases efficiency and analysis accuracy by reducing human error and ensuring 
       that no “steps are skipped” in the process. Finally, automation of this kind may improve the ability 
       of sociophonetic studies to be easily reproducible.", align = 'justify')
  
  HTML(paste(text_out, sep = '<br/>'))
  })

  
  output$title1 <- renderUI({ 
    text_out = 'Hypothesis testing automation in the sociophonetic analysis of vowel variation'
  HTML(paste(text_out, sep = '<br/>'))
  })
 
})