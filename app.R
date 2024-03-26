library(shiny)
library(shinythemes)
library(ggplot2)
library(scales)
library(pracma)
library(shinyFeedback)
library(readxl)
library(vroom)
library(DT)
library(ggiraph)
library(tidyverse)

"
Author: LuisM18
License: Creative Commons Legal Code 1.0 Universal
"
theme_set(theme_bw())

ui <- navbarPage(title = "Optimización de portafolios", #Config page
                 theme = shinytheme("sandstone"),
                 tabPanel("Datos", 
                          sidebarPanel(
                            fileInput("archivo", "Seleccicone archivo CSV o Excel",
                                      multiple = FALSE,
                                      accept = c(".csv",".xlsx",".xls")),
                            tags$hr(),
                            checkboxInput("header", "Header", TRUE),
                            radioButtons("sep", "Separador",
                                         choices = c(Coma = ",",
                                                     Punto_y_coma = ";",
                                                     Tab = "\t"),
                                         selected = ",")
                          ),
                          mainPanel(
                            dataTableOutput("datos"))),
                 tabPanel("Dominancia",
                          fluidRow(
                            column(4,offset = 3,
                                   numericInput("ndom","Dominantes",5,
                                                2, 10, 1),
                                   h2(strong("Perfil individual")),   
                                   dataTableOutput("domt")
                            ),br(),
                            column(4,
                                   plotOutput("domg",width = "100%",click = 'dom_click',hover = 'dom_hover')
                            )),
                            plotOutput("gactivo",width = '100%')),
                 tabPanel("Frontera eficiente",
                          titlePanel("Frontera eficiente"),
                          tabsetPanel(
                            tabPanel("Grafica",plotOutput("gfront",width = "100%")),
                            tabPanel("Datos",dataTableOutput("nfront"))
                          ),
                          tags$hr(),
                          fluidRow(
                            column(4,
                                   sliderInput("nport","Número de portafolios", min=1000, max=10000,
                                               2000),
                                   numericInput("rf","Tasa libre de riesgo (%)",5,min=1,max=20,step=0.01)
                                   
                            ),
                            column(4,
                                   tableOutput("tan")
                            )
                          )))



server <- function(input, output) {
  
  matriz_csv <- function(datos){ 
    copia = matrix(nrow=nrow(datos),ncol=ncol(datos))
    for(i in 1:ncol(datos)){
      copia[,i]= as.double(as.character(unlist(datos[i]))) 
    }
    colnames(copia)= colnames(datos)
    return(copia)
  }
  
  dominantes <- function(datos,n){
    rh = diff(log(matriz_csv(datos)))
    dom = matrix(nrow=ncol(rh),ncol=4)
    colnames(dom) = c("Activo","Rentabilidad","Riesgo","Sharpe")
    
    
    for(i in 1:nrow(dom)){
      dom[i,2]= ((1+mean(rh[,i]))^365)-1
    }
    for(i in 1:nrow(dom)){
      dom[i,3]= sd(rh[,i])*sqrt(252)
    }
    for(i in 1:nrow(dom)){
      dom[i,4]= (dom[i,2]/dom[i,3])
    }
    dom = data.frame(dom)
    dom$Activo  = colnames(datos)
    dom = dom[order(-dom$Sharpe),]
    dominantes = dom[1:n,c(1,2,3,4)]
    return(dominantes)
  }
  
  frontera <- function(datos,ndom,nport){
    
    dom = dominantes(datos,ndom)
    re = dom[,2]
    rp = array(dim=nport)
    vp = array(dim=nport)
    sp = array(dim=nport)
    rh = diff(log(matriz_csv(datos[,dom$Activo])))
    cov = cov(rh)
    
    wi = rand(nport,ndom)
    var = matrix(nrow=ndom,ncol = ndom)
    
    for(i in 1:nrow(wi)){
      s = sum(wi[i,])
      for(j in 1:ncol(wi)){
        wi[i,j] = wi[i,j]/s
      }
      vp[i] = sqrt(dot(t(wi[i,]),dot(cov,wi[i,])))
      rp[i] = dot(re,wi[i,])
    }
    for(i in 1:length(vp)){
      sp[i]= (rp[i]/vp[i])
    }
    colnames(wi)= dom[,1]
    port = data.frame(rp,vp,sp,wi)
    colnames(port)=c("Rentabilidad","Riesgo","Sharpe",colnames(wi))
    return(port)
  }
  
  optimo <- function(datos,rf,ndom,nport){
    f = frontera(datos,ndom,nport)
    sharpe = max(f[,3])
    for(i in 1:nrow(f)){
      if(sharpe==f[i,3]){
        rc = f[i,1]
        vc = f[i,2]
      }
    }
    optimo = c(paste(round(rc*100,2),'%'),paste(round(vc*100,2),'%'))
    return(optimo)
  }
  

  data <- reactive({
    req(input$archivo)
    file = input$archivo
    ext = tools::file_ext(input$archivo$name)
    switch(ext,
           csv = vroom(file$datapath,
                       col_names = input$header,
                       delim = input$sep,
                       show_col_types = F),
           xls = read_xls(file$datapath,
                          col_names = input$header),
           xlsx = read_xlsx(file$datapath,1,
                            col_names = input$header),
           validate('Formato invalido: Por favor cargue un archivo csv o Excel(.xls,.xsls)')
    )
  })
  
  output$datos <- renderDataTable({
    datos = data()
    
    # Si Date existe sera rownames, sino seq
    
    return(datatable(datos) %>% formatCurrency(colnames(datos),'$'))
    
    })
  
  output$domt = renderDataTable({
    datos <- datatable(dominantes(data(),input$ndom),
                       options = list(dom = "t",
                                      ordering = FALSE,
                                      paging = FALSE,
                                      searching = FALSE),
                       selection = 'none',
                       rownames = FALSE,
                       class = 'row-border') %>% formatPercentage(c('Rentabilidad','Riesgo'),2) %>% formatCurrency('Sharpe','')  
    return(datos)
    })
  
  output$domg = renderPlot({
    datos <- data()
    d = dominantes(datos,input$ndom)

    ggplot(d, aes(x = Riesgo, y = Rentabilidad, color = Activo)) +
      geom_point_interactive(size = 10) +
      theme_bw() +
      xlab("Riesgo") + ylab("Rentabilidad esperada") +
      scale_y_continuous(labels=percent , limits = c(0, max(d[,2])+0.001)) +
      scale_x_continuous(labels =percent , limits = c(0, max(d[,3])+0.001)) 
    
  })
  
    output$gactivo = renderPlot({
      req(input$dom_click)
      points = nearPoints(dominantes(data(),input$ndom),input$dom_click,xvar = "Riesgo", yvar = "Rentabilidad" ,maxpoints = 1)
      
      datos = data()
        ggplot(datos) + 
        aes(x= linspace(0,100,dim(datos)[1])  ,y =datos[[points$Activo]],group = 1,color) +
        geom_line(linewidth= 2) +
        xlab("Fecha") + ylab(points$Activo)

  })

  output$nfront = renderDataTable({
    datos <- frontera(data(),input$ndom,input$nport)
    port = datatable(datos) %>% formatPercentage(colnames(datos)[!colnames(datos) == 'Sharpe'],2) %>% formatCurrency('Sharpe','')
    return(port)
  })
  
  output$gfront = renderPlot({
    datos <- data()
    port = frontera(datos,input$ndom,input$nport)
    
    ggplot(port, aes(x = Riesgo, y = Rentabilidad)) +
      geom_point(aes(colour = Sharpe)) +
      xlab("Riesgo") + ylab("Rentabilidad esperada") +
      scale_y_continuous(labels=percent) +
      scale_x_continuous(labels =percent)+
      scale_colour_gradient2(midpoint = mean(port[,3]), low = 'red',mid = 'orange',high = 'green',guide = FALSE)
  })
  
  output$tan = renderTable({
    datos <- data()
    opt = optimo(datos,input$rf/100,input$ndom,input$nport)
    tabla = matrix(nrow=2,ncol=2)
    tabla[,2]= opt 
    tabla[,1]= c("Rentabilidad Esperada","Riesgo")
    colnames(tabla)= c("Portafolio Optimo"," ")
    return(tabla)
  },digits = 4,spacing= 's')
}

shinyApp(ui = ui, server = server)
