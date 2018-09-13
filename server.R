#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinyjs)
colors<-c("#363636","#242F40","#CCA43B","#E5B0B0")
color <- c( 'rgb(90, 221, 147,1)',
            'rgba(89, 199, 182,1)', 'rgb(94, 156, 147)',
            'rgb(37, 94, 101)','rgba(0,0,0,1)')

# Define server logic required to draw a histogram
shinyServer(function(input, output,session) {
  Excel_data<-reactiveVal({NULL})
  Excel_data_success_allocation<-reactiveVal({NULL})
  
  observeEvent(input$year,{
      current_department=isolate({input$department})
      department_result<-getDepartments(academic_year=input$year,semester=input$semester)
      
      if(current_department %in% unlist(department_result))
        updateSelectInput(session,inputId = "department",label=NULL,
                          choices=department_result, selected = current_department)
      else{
        updateSelectInput(session,inputId = "department",label=NULL,
                          choices=department_result)
      }
  })
  
  observeEvent(input$department,{
    updateSelectInput(session,inputId = "discipline",label=NULL,
                      choices=getDisciplines(academic_year=input$year,semester=input$semester,
                                             department=isolate(input$department)))
  })
  
  #------------------------------WHEN CLICKED SUBMIT Base and Supplemental Allocation---------------------------------------------
  observeEvent(input$submit,{
    
    
    output$value_box_sem_year_base<-renderUI({
      infoBox(title="Year:", value=isolate(input$year),subtitle=paste0("Semester: ",input$semester),width='100%',color='black', icon=icon("calendar"))
    })
    
    output$value_box_dep_dis<-renderUI({
      infoBox(title="Department: ", value=isolate(input$department),subtitle=paste0("Discipline:",isolate(input$discipline)),width='50%',color='black',icon=icon("building"))
    })
    
    
    ftes_data<-isolate({get_ftes(input$semester,input$year,input$department,input$discipline)})
    
    if(!is.null(ftes_data)){
      ftes_data$FTES<-round( ftes_data$FTES,digits=1)
      ftes_data$Credit<-round( ftes_data$Credit,digits=1)
      ftes_data$Noncredit<-round( ftes_data$Noncredit,digits=1)
      ftes_data$Concurrent<-round( ftes_data$Concurrent,digits=1)
      
      
      #change names for excel
      temp_excel<-ftes_data
     
      if(isolate(input$department)!="All Departments" && isolate(input$discipline)!="All Disciplines"){#chose a department and discipline
        colnames(temp_excel)<-c("Academic_Year","Department","Discipline","Total_FTES","Credit_FTES","Noncredit_FTES","AdultEd_FTES","Concurent_FTES")
      }else if(isolate(input$discipline)=="All Disciplines" && isolate(input$department)!="All Departments"){#did not chose a discipline
        colnames(temp_excel)<-c("Academic_Year","Department","Total_FTES","Credit_FTES","Noncredit_FTES","AdultEd_FTES","Concurent_FTES")
      }else if(isolate(input$department)=="All Departments"){
        colnames(temp_excel)<-c("Academic_Year","FTES_Total","Total_FTES","Noncredit_FTES","AdultEd_FTES","Concurent_FTES")
      }
      
      Excel_data(temp_excel)
      
      #---table-----
      table_ftes<-temp_excel
      colnames(table_ftes)<-str_replace_all(colnames(table_ftes),"_"," ")
      
      output$table_ftes_glance<-renderDataTable({table_ftes})
      output$table_ftes_overview<-renderDataTable({table_ftes})
      #-----------------------------FTES Overview GRAPH--------------------------------------------
      p<-plot_ly(ftes_data, y=ftes_data$FTES, x=ftes_data$Academic_Year,type = 'scatter', mode = 'lines+markers',line = list(color = color[1], width = 4),
               hoverinfo="text",
               text= ~paste0("Academic Year:",Academic_Year,
                             "\n FTES:",FTES)) %>%
        layout(title = "FTES Overview",
               xaxis = list(title = "Academic Year"),
               yaxis = list (title = "FTES"),
               margin=list(r=80))%>%
        config(plot_ly(), displaylogo = FALSE,collaborate =FALSE,
               modeBarButtonsToRemove = list('sendDataToCloud','lasso2d','zoom2d','select2d',
                                             'hoverClosestCartesian','hoverCompareCartesian'))
      output$FTES_glance<-renderPlotly({p})
      
      #-----------------------------FTES Breakdown GRAPH--------------------------------------------
      p_trace<-plot_ly(ftes_data, x=~Academic_Year, y=~Credit, name="Credit", type = 'scatter', mode = 'lines+markers',line = list(color = color[1], width = 4)) %>%
         add_trace(y=~Noncredit, name="Noncredit", mode = 'lines+markers',line = list(color = color[2], width = 4))%>%
         add_trace(y=~AdultEd, name="AdultEd", mode = 'lines+markers',line = list(color = color[3], width = 4))%>%
          add_trace(y=~Concurrent, name="Concurrent", mode = 'lines+markers',line = list(color = color[4], width = 4)) %>%
        layout(title = "FTES Breakdown",
               xaxis = list(title = "Academic Year"),
               yaxis = list (title = "FTES"),
               margin=list(r=80))%>%
        config(plot_ly(), displaylogo = FALSE,collaborate =FALSE,
               modeBarButtonsToRemove = list('sendDataToCloud','lasso2d','zoom2d','select2d',
                                             'hoverClosestCartesian','hoverCompareCartesian'))
     
       output$FTES_overview<-renderPlotly({p_trace})
    }
    
    supplemental_data<-isolate({get_supplemental(input$semester,input$year,input$department,input$discipline)})
    
    #----------------------------Need-based Graph --------------------------------------------------------
    if(!is.null(supplemental_data)){
      #table-------
      table_supp<-supplemental_data
      colnames(table_supp)<-str_replace_all(colnames(table_supp),"_"," ")
      output$table_supp<-renderDataTable({table_supp})
      
      
      s_trace<-plot_ly(supplemental_data, x=~Academic_Year, y=~AB_540, name="AB 540", type = 'scatter', mode = 'lines+markers',line = list(color = color[1], width = 4),
                       hoverinfo='text',text=~paste0("->AB 540 \nAcademic Year: ",Academic_Year,"\n Count: ",AB_540)) %>%
        add_trace(y=~BOGG, name="California Promise Grant", mode = 'lines+markers',line = list(color = color[2], width = 4),
                  hoverinfo='text',text=~paste0("->California Promise Grant \nAcademic Year: ",Academic_Year,"\n Count: ",BOGG))%>%
        add_trace(y=~Pell_Grant, name="Pell Grant", mode = 'lines+markers',line = list(color = color[3], width = 4),
                  hoverinfo='text',text=~paste0("->Pell Grant \nAcademic Year: ",Academic_Year,"\n Count: ",Pell_Grant))%>%
        layout(title = "Need-Based",
               xaxis = list(title = "Academic Year"),
               yaxis = list (title = "Head Count"),
               margin=list(r=80))%>%
        config(plot_ly(), displaylogo = FALSE,collaborate =FALSE,
               modeBarButtonsToRemove = list('sendDataToCloud','lasso2d','zoom2d','select2d',
                                             'hoverClosestCartesian','hoverCompareCartesian'))
      
      output$supp_overview<-renderPlotly({s_trace})
    #excel------
      if(isolate(input$department)!="All Departments" && isolate(input$discipline)!="All Disciplines")
        merge_excels<-merge(supplemental_data,temp_excel, by=c("Academic_Year","Department","Discipline"), all=TRUE)
      else if(isolate(input$department)!="All Departments" && isolate(input$discipline)=="All Disciplines")
        merge_excels<-merge(supplemental_data,temp_excel, by=c("Academic_Year","Department"), all=TRUE)
      else
        merge_excels<-merge(supplemental_data,temp_excel, by=c("Academic_Year"), all=TRUE)
      
      if(!is.null(merge_excels[is.na(merge_excels)]))
        merge_excels[is.na(merge_excels)]<-0
      Excel_data(merge_excels)
      
    }
    
  })
  
  
  observeEvent(input$submit_success,{
    
    output$value_box_sem_year_success<-renderUI({
      infoBox(title="Year:", value=isolate(input$year),subtitle=paste0("Semester: ",isolate(input$semester)),width='100%',color='black', icon=icon("calendar"))
    })
    
    output$value_box_prog<-renderUI({
      infoBox(title="Program: ", value=isolate(input$program),width='50%',color='black',icon=icon("graduation-cap"))
    })
    
    success_data<-isolate ({get_success_allocation(input$semester,input$year,input$program)})
  
    #----------------graph--------------------
    p_success<-plot_ly(success_data, x=~Academic_Year, y=~AA_AS, name="AA/AS", type = 'scatter', mode = 'lines+markers',line = list(color = color[1], width = 4)) %>%
      add_trace(y=~ADT, name="ADT", mode = 'lines+markers',line = list(color = color[2], width = 4))%>%
      add_trace(y=~Certificates_Over_18, name="Certificates Over 18 Units", mode = 'lines+markers',line = list(color = color[3], width = 4))%>%
      layout(title = "Success Allocation",
             xaxis = list(title = "Academic Year"),
             yaxis = list (title = "Count"),
             margin=list(r=80))%>%
      config(plot_ly(), displaylogo = FALSE,collaborate =FALSE,
             modeBarButtonsToRemove = list('sendDataToCloud','lasso2d','zoom2d','select2d',
                                           'hoverClosestCartesian','hoverCompareCartesian'))
    
    output$success_allocation_graph<-renderPlotly({p_success})
    
    #fixing for excel purposes
    excell_success<-success_data
    
    #Add program
    excell_success$Program<-isolate(input$program)
    if(!is.null(isolate(input$semester))){
        excell_success$Semester<-(isolate(input$semester))
        excell_success<-excell_success[c(1,6:5,2:4)]
    }else{
      excell_success<-excell_success[c(1,5,2:4)]
    }
    Excel_data_success_allocation(excell_success)
    
    #table-----
    table_suc<-excell_success
    colnames(table_suc)<-str_replace_all(colnames(table_suc),"_"," ")
    output$table_success<-renderDataTable({table_suc})
    })
  
  #-----------------------------Download Excel File----------------------------------------------
  # Downloadable csv of ----
  output$downloadExcel <- downloadHandler(
    filename = function() {
      paste("Base&SuccessAllocation-",input$year,"-",input$department,"-",input$discipline, ".csv", sep = "")
    },
    content = function(file) {
      write.csv(Excel_data(), file, row.names = FALSE)
    }
  )
  output$downloadExcel_success <- downloadHandler(
    filename = function() {
      paste("SuccessAllocation-",input$year,"-",input$program, ".csv", sep = "")
    },
    content = function(file) {
      write.csv(Excel_data_success_allocation(), file, row.names = FALSE)
    }
  )
  
  
 
  
  observe(if(!is.null(input$semester) && length(input$semester)==1){
    choices=c("Summer","Fall","Winter","Spring")
    subElement <- paste0("#semester .checkbox:nth-child(",  match(setdiff(choices,input$semester),choices),") label")

    shinyjs::disable(selector=subElement)
    
  }
  else{
    shinyjs::enable("semester")
  })
  
  observeEvent(input$departments,{
    chosen_department(input$departments)
  })
  
})
