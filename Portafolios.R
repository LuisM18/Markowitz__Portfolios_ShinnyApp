library(shiny)
library(shinythemes)
library(ggplot2)
library(scales)
library(pracma)
library(shinyFeedback)
library(readxl)
library(vroom)

"
Author: LuisM18
License: 
"

porcentaje = function(valor){
  return(paste( round( valor, 4 ) * 100 , "%"))
}

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
                            tableOutput("datos"))),
                 tabPanel("Dominancia",
                          plotOutput("domg",width = "100%"),
                          
                          fluidRow(
                            column(4,
                                   h2(strong("Dominancia")),
                                   numericInput("ndom","Dominantes",5,
                                                2, 10, 1)
                            ),br(),
                            column(4,
                                   h2(strong("Perfil individual")),   
                                   tableOutput("domt")
                                   
                            ))),
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
    dom = apply(dom[,c(2,3)],2,porcentaje)  
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
    colnames(port)=c("Rent","Riesgo","Sharpe",colnames(wi))
    return(port)
  }
  
  optimo <- function(datos,rf,ndom,nport){
    f = frontera(datos,ndom,nport)
    sharpe = max(f[,3])
    for(i in 1:nrow(f)){
      if(sharpe==f[i,3]){
        rc = f[i,1]
        vc = f[i,2]
        wc = f[i,3:3+ndom]
      }
    }
    tan = (rc-rf)/vc
    optimo = c(tan,rc,vc)
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
  
  output$datos <- renderTable(data(),digits = 3)
  
  output$domt = renderTable({
    datos <- data()
    return(dominantes(datos,input$ndom))
    
  },digits = 5)
  
  output$domg = renderPlot({
    datos <- data()
    d = dominantes(datos,input$ndom)
    
    ggplot(d, aes(x = Riesgo, y = Rentabilidad, color = Activo)) +
      geom_point(size = 7) +
      theme_bw() +
      xlab("Riesgo") + ylab("Rentabilidad esperada") +
      scale_y_continuous(labels=percent , limits = c(0, max(d[,2])+0.001)) +
      scale_x_continuous(labels =percent , limits = c(0, max(d[,3])+0.001))
    
  })
  
  output$nfront = renderDataTable({
    datos <- data()
    port = frontera(datos,input$ndom,input$nport)
    return(port)
  })
  
  output$gfront = renderPlot({
    datos <- data()
    port = frontera(datos,input$ndom,input$nport)
    
    ggplot(port, aes(x = Riesgo, y = Rent)) +
      geom_point(size = 1) +
      theme_bw() +
      xlab("Riesgo") + ylab("Rentabilidad esperada") +
      scale_y_continuous(labels=percent , limits = c(0, max(port[,1]))) +
      scale_x_continuous(labels =percent , limits = c(0, max(port[,2])))
    
  })
  
  output$tan = renderTable({
    datos <- data()
    opt = optimo(datos,input$rf/100,input$ndom,input$nport)
    tabla = matrix(nrow=3,ncol=2)
    tabla[,2]= opt 
    tabla[,1]= c("Tan","Rentabilidad","Riesgo")
    colnames(tabla)= c("Portafolio Optimo"," ")
    return(tabla)
  },digits = 4)
}

shinyApp(ui = ui, server = server)
