#install.packages("shinyjs")
#install.packages("sodium")
#install.packages("foreign")
#install.packages("reshape2")
#install.packages("plyr")
#install.packages("dplyr")
#install.packages("ggplot2")
#install.packages("lubridate")
#install.packages("scales")
#install.packages("stringr")
#install.packages("plotly")
#install.packages('DT')
#install.packages("gridExtra")
#install.packages("htmltools")

library(shiny)
library(shinydashboard)
library(DT)
library(shinyjs)
library(sodium)
library(htmltools)
library("reshape2") ## AYUDA A CREAR TABLAS DINAMICAS DCAST
library("foreign") ## PERMITE LEER DBF
library("plyr") ## ME PERMITE HACER TABLAS DINAMICAS DDPLY
library("dplyr") ##Sirve para usar el case
library("ggplot2") ## sirve para las graficas
library(lubridate) ## manipular fechas
library("scales") ##modificar axes
library("stringr") ##MAnipular caracteres
library("plotly") ##MAnipular caracteres
library("DT")
library("gridExtra")
library(shinyjs)##login
library(sodium)##login

###########################################################################################################
###########################################################################################################
##################################        BASES          ##################################################
###########################################################################################################
###########################################################################################################

setwd("D:/AFFICENT/Clientes SAI/Dimefet/Dashboard/")


###Clientes#####
##SEGMENTACION DE CLIENTES##
segfreccte=read.dbf("Data/Cliente/segfreccte.dbf",as.is=FALSE) 
ctesnvos=read.dbf("Data/Cliente/ctesnvos.dbf",as.is=FALSE)
detctefac=read.dbf("Data/Cliente/detctesfac.dbf",as.is=FALSE)

##RESUMEN CLIENTES##  
vtasctes=read.dbf("Data/Cliente/vtascte.dbf",as.is=FALSE) 
catcte=read.dbf("Data/cliente/catcte.dbf",as.is=FALSE)
cteprod=read.dbf("Data/cliente/cteprod.dbf",as.is=FALSE)


##NOTAS DE VENTA##
nvtamost=read.dbf("Data/Cliente/nvtamost.dbf",as.is=FALSE) 
horamost=read.dbf("Data/Cliente/horamost.dbf",as.is=FALSE)

###Flujos#####
fe=read.dbf("Data/flujo/flujoef.dbf",as.is=FALSE) 
cxp=read.dbf("Data/flujo/cxp.dbf",as.is=FALSE) 
cxc=read.dbf("Data/flujo/cxc.dbf",as.is=FALSE) 

###uTILIDAD#####
uttotpres=read.dbf("Data/Utilidad/uttotpres.dbf",as.is=FALSE)
uttotcomp=read.dbf("Data/Utilidad/uttotcomp.dbf",as.is=FALSE)
ingcomp=read.dbf("Data/Utilidad/ingreso.dbf",as.is=FALSE)
#ingfac=read.dbf("Data/Utilidad/ingresofac.dbf",as.is=FALSE)
gascomp=read.dbf("Data/Utilidad/gasto.dbf",as.is=FALSE)
catgto=read.dbf("Data/Utilidad/catgto.dbf",as.is=FALSE)

###PRODUCTO#####
vtasprodm=read.dbf("Data/Producto/Producto.dbf",as.is=FALSE)
catprod=read.dbf("Data/Producto/catprod.dbf",as.is=FALSE)
#existencia=read.dbf("Data/Producto/existencia.dbf",as.is=FALSE)
#prodalma=read.dbf("Data/Producto/prodalma.dbf",as.is=FALSE)

###AGENTES#####
agevtahist=read.dbf("Data/Agentes/agevtahist.dbf",as.is=FALSE) #nvtamost
agehora=read.dbf("Data/Agentes/agehora.dbf",as.is=FALSE) #horamost
ageprod=read.dbf("Data/Agentes/ageprod.dbf",as.is=FALSE) #nvtaprod
catage=read.dbf("Data/Agentes/catage.dbf",as.is=FALSE) #nvtaprod

###COBRANZA#####
basecob=read.dbf("Data/Cobranza/basecob.dbf",as.is=FALSE) 
factpencob=read.dbf("Data/Cobranza/factpencob.dbf",as.is=FALSE) 
evcob=read.dbf("Data/Cobranza/evcob.dbf",as.is=FALSE) 

### cicloadq #####
#cicloadq=read.dbf("Data/cicloadq/cicloadq.dbf",as.is=FALSE) 
#diasavg=read.dbf("Data/cicloadq/diasavg.dbf",as.is=FALSE) 
#docpend=read.dbf("Data/cicloadq/docpend.dbf",as.is=FALSE) 



###########################################################################################################
###########################################################################################################
####################################### Main login screen #################################################
###########################################################################################################
###########################################################################################################

loginpage <- div(id = "loginpage", style = "width: 500px; max-width: 100%; margin: 0 auto; padding: 20px;",
                 wellPanel(
                   tags$h2("LOG IN", class = "text-center", style = "padding-top: 0;color:#333; font-weight:600;"),
                   textInput("userName", placeholder="Username", label = tagList(icon("user"), "Username")),
                   passwordInput("passwd", placeholder="Password", label = tagList(icon("unlock-alt"), "Password")),
                   br(),
                   div(
                     style = "text-align: center;",
                     actionButton("login", "SIGN IN", style = "color: white; background-color:#3c8dbc;
                                 padding: 10px 15px; width: 150px; cursor: pointer;
                                 font-size: 18px; font-weight: 600;"),
                     shinyjs::hidden(
                       div(id = "nomatch",
                           tags$p("Usuario o Password incorrecto!",
                                  style = "color: red; font-weight: 600; 
                                            padding-top: 5px;font-size:16px;", 
                                  class = "text-center"))),
                     br(),
                     br()
                   ))
)

credentials = data.frame(
  username_id = c("user", "admin"),
  passod   = sapply(c("user", "admin"),password_store),
  permission  = c("basic", "advanced"), 
  stringsAsFactors = F
)

###########################################################################################################
###########################################################################################################
##########################################      UI      ###################################################
###########################################################################################################
###########################################################################################################


header <- dashboardHeader( title = "DIMEFET", uiOutput("logoutbtn"))

sidebar <- dashboardSidebar(uiOutput("sidebarpanel")) 
body <- dashboardBody(shinyjs::useShinyjs(), uiOutput("body"))
ui<-dashboardPage(header, sidebar, body, skin = "blue")

###########################################################################################################
###########################################################################################################
######################################   SERVER - UI   ####################################################
###########################################################################################################
###########################################################################################################


server <- function(input, output, session) {
  
  login = FALSE
  USER <- reactiveValues(login = login)
  
  observe({ 
    if (USER$login == FALSE) {
      if (!is.null(input$login)) {
        if (input$login > 0) {
          Username <- isolate(input$userName)
          Password <- isolate(input$passwd)
          if(length(which(credentials$username_id==Username))==1) { 
            pasmatch  <- credentials["passod"][which(credentials$username_id==Username),]
            pasverify <- password_verify(pasmatch, Password)
            if(pasverify) {
              USER$login <- TRUE
            } else {
              shinyjs::toggle(id = "nomatch", anim = TRUE, time = 1, animType = "fade")
              shinyjs::delay(3000, shinyjs::toggle(id = "nomatch", anim = TRUE, time = 1, animType = "fade"))
            }
          } else {
            shinyjs::toggle(id = "nomatch", anim = TRUE, time = 1, animType = "fade")
            shinyjs::delay(3000, shinyjs::toggle(id = "nomatch", anim = TRUE, time = 1, animType = "fade"))
          }
        } 
      }
    }    
  })
  
  output$logoutbtn <- renderUI({
    req(USER$login)
    tags$li(a(icon("fa fa-sign-out"), "Logout", 
              href="javascript:window.location.reload(true)"),
            class = "dropdown", 
            style = "background-color: #eee !important; border: 0;
                    font-weight: bold; margin:5px; padding: 10px;")
  })
  
  output$sidebarpanel <- renderUI({
    if (USER$login == TRUE ){ 
      if (credentials[,"permission"][which(credentials$username_id==input$userName)]=="advanced") {
          sidebarMenu(
     
       ####Text Box para ingresar las fechas que van a controlar el tablero##########
            
            dateRangeInput("fec", "Fechas:",start = as.Date(paste0(year(today()),"-01","-01")),end   = today()),
            
            actionButton("actualizar","Actualizar"),
       
       ####Opciones del Menu y submenu izquierdo#######
       
             menuItem("Utilidad",
                menuSubItem("Resumen Utilidad",tabName="utcomp"),     
                menuSubItem("Resumen Gastos",tabName="gas"),     
                menuSubItem("Resumen Ingresos",tabName="ing")),
             menuItem("Clientes",
                menuSubItem("Resumen de Clientes",tabName="vtasctes"),     
                menuSubItem("Segmentacion de Clientes",tabName="segclien"),
                menuSubItem("Clientes Mostrador",tabName="vtasmost")),       
             menuItem("Flujo de Efectivo",
                menuSubItem("Resumen",tabName="resfe"),      
                menuSubItem("Cuentas por Pagar",tabName="cxp"),
                menuSubItem("Cuentas por Cobrar",tabName="cxc")),
             menuItem("Productos",
                menuSubItem("Resumen de Productos",tabName="men_Prod"),
                menuSubItem("Productos por Cliente",tabName="men_prodcte"),
                menuSubItem("Existencia vs Ventas",tabName="men_prodexist"),
                menuSubItem("Productos por Agentes",tabName="men_prodage")),
             menuItem("Metricas de Agentes",tabName="men_Agentes"), 
             menuItem("Cobranza",
                menuSubItem("Resumen de Cobranza",tabName="men_rescob"),
                menuSubItem("Cobranza por Cliente",tabName="men_cobcte"),
                menuSubItem("Evaluacion de Cobranza",tabName="men_evcob")),
             menuItem("Ciclo Adq Compras",
                menuSubItem("Resumen",tabName="men_ciccomp"))
       
          )
      }
      else{
        sidebarMenu(
          
          ####Opciones del Menu y submenu izquierdo#######
          
          menuItem("Cobranza",
                   menuSubItem("Resumen de Cobranza",tabName="men_rescob"),
                   menuSubItem("Cobranza por Cliente",tabName="men_cobcte"),
                   menuSubItem("Evaluacion de Cobranza",tabName="men_evcob"))
          
          
        )
          
      }
      
    }
  })
  
  output$body <- renderUI({
    if (USER$login == TRUE ) {
      tabItems(  #esta funcion indica que vamos a trabajar con Tabitem
        #y que cada click que demos en la barra izquierda cambie el cuerpo del dashboard
        
        tabItem(tabName="vtasmost",
                
                fluidRow(
                  ##br(),
                  box(numericInput("ANIO","Elige el Anio que quieras graficar:", value=year(today()),width="300px"), 
                      plotOutput("hrmost"),title="Ventas Mostrado por Hora",solidHeader = TRUE,status="primary"),
                  box(plotOutput("mesmost"),title="Ventas Mostrado Mensual",solidHeader = TRUE,status="primary"))
                
        ),
        
        tabItem(tabName="vtasctes",
                fluidRow(box(uiOutput("Box2"),width=3),
                         box(uiOutput("Box3"),width=3)),
                box(plotlyOutput("vcmg"),title="Historico por producto",solidHeader = TRUE,status="danger",width=5),
                box(numericInput("Participa","Clientes con participacion Mayor que:", value=".01",width="300px"), 
                    plotlyOutput("vcgp"),title="Participacion en Ventas",solidHeader = TRUE,status="danger"),
                fluidRow(box(numericInput("Participactep","Producto con participacion Mayor que:", value=".01"), 
                             plotlyOutput("vpctegp"),title="Participacion por producto",solidHeader = TRUE,status="danger",width=5),
                         box(numericInput("ANIOCTEHIST","Elige el Anio que quieras graficar:", value=year(today())), 
                             plotlyOutput("vpctegl"),title="Historico por producto",solidHeader = TRUE,status="danger",width=5),
                         box(uiOutput("Boxcte11"),title="Filtros",solidHeader = TRUE,status="danger",width=2)),  
                DT::dataTableOutput("tablevtascte")
                
        ),
        
        tabItem(tabName="segclien",
                fluidRow(
                  box("Filtra Grafica 1,2,3",
                      uiOutput("canalcte"),
                      br(),
                      "Filtra Grafica 1",
                      numericInput("ANION","Elige el Anio que quieras graficar:", value=year(today()),width="300px"),
                      selectInput("vargraf","Elige la variable que quieras Graficar:",c("Numero de compras" = "CNT_FAC","Monto de la Ventas" = "VENT_NET"),width="300px"),title="Filtros",solidHeader = TRUE,status="primary",width = 2),
                  box(plotOutput("histctesnvos"),title="1.- Ventas por Tipo de Clientes",solidHeader = TRUE,status="primary",width = 5),
                  box(tabBox(id="grafnvos",
                             tabPanel("2.- Participacion por segmento",plotlyOutput("partseg")),
                             tabPanel("3.- Grafica captacion Ctes Nvos",plotOutput("capctenvo")),width = 12),title="",solidHeader = TRUE,status="primary",width = 5),
                  box(tabBox(id="tabsegcte",
                             tabPanel("Resumen por Clientes",DT::dataTableOutput("rescte")),
                             tabPanel("Detalle por Mes",DT::dataTableOutput("detctemes")),
                             tabPanel("Detalle por Factura",DT::dataTableOutput("detctefac")),width = 12),title="Segmentacion de clientes por Frecuencia de Compra",solidHeader = TRUE,status="primary",width = 12)
                )
                
        ),
        
        tabItem(tabName="resfe",
                fluidRow(box(textInput("fecflujo","(aaaa-mm-dd)", value=today()+15,width="250px"),
                             h4("Flujo de Efectivo"),
                             textOutput("fecxc1"),
                             textOutput("fecxp1"),
                             strong(textOutput("subtfe1")),
                             br(),
                             textOutput("fecxc2"),
                             textOutput("fecxp2"),
                             strong(textOutput("subtfe2")),
                             br(),
                             strong(textOutput("totfe")),
                             br(),
                             textOutput("fecxcvncred"),
                             textOutput("fecxpcncred"),
                             textOutput("fecxcvncargo"),
                             textOutput("fecxpcncargo"),
                             title="Elige la fecha para estimar el flujo:",solidHeader = TRUE,status="primary",width = 2),
                box(plotlyOutput("fehist"),title="Flujo de Efectivo Acumulado",solidHeader = TRUE,status="primary",width = 10)),    
                box(DT::dataTableOutput('fetabla'),title="Flujo de Efectivo Detalle por Factura",solidHeader = TRUE,status="primary",width = 12),
                "_____________________________________________"
        ),
        
        tabItem(tabName="cxp",
                box(plotOutput("barcxp"),title="1.- Monto por Vencer",solidHeader = TRUE,status="primary",width=5),
                box(plotlyOutput("partcxp"),title="2.- Participacion por Proveedor",solidHeader = TRUE,status="primary",width = 5),
                box(selectInput("VENCI","Elige el plazo de Vencimiento:",c("VENCIDA" = "VENCIDA","SIETE" = "SIETE","QUINCE" = "QUINCE","TREINTA" = "TREINTA","CUARENTA Y CINCO" = "CUATCINCO","SESENTA" = "SESENTA","MAS" = "MAS","NOTAS DE CREDITO" = "N_CRED","NOTAS DE CARGO" = "N_CARG"),width="300px"),
                    numericInput("Particxp","Proveedores con participacion Mayor que:", value=".03",width="300px"),title="Filtros para Grafica 2",solidHeader = TRUE,status="primary",width = 2),
                box(DT::dataTableOutput('dowcxp'),title="Facturas de Compra por Vencer",solidHeader = TRUE,status="primary",width = 12),
                "_____________________________________________"
                
        ),
        
        tabItem(tabName="cxc",
                box(plotOutput("barcxc"),title="1.- Monto por Vencer",solidHeader = TRUE,status="primary",width=5),
                box(plotlyOutput("partcxc"),title="2.- Participacion por Clientes",solidHeader = TRUE,status="primary",width = 5),
                box(selectInput("VENCIcxc","Elige el plazo de Vencimiento:",c("VENCIDA" = "VENCIDA","SIETE" = "SIETE","QUINCE" = "QUINCE","TREINTA" = "TREINTA","CUARENTA Y CINCO" = "CUATCINCO","SESENTA" = "SESENTA","MAS" = "MAS","NOTAS DE CREDITO" = "N_CRED","NOTAS DE CARGO" = "N_CARG"),width="300px"),
                    numericInput("Particxc","Clientes con participacion Mayor que:", value=".05",width="300px"),title="Filtros para Grafica 2",solidHeader = TRUE,status="primary",width = 2),
                box(DT::dataTableOutput('dowcxc'),title="Facturas de Compra por Vencer",solidHeader = TRUE,status="primary",width = 12),
                "_______________________________________________"
        ),
        
        tabItem(tabName="utcomp",
                
                fluidRow(box(uiOutput("Boxut11"),width=3),
                         infoBoxOutput("ut",width=3),
                         infoBoxOutput("ing",width=3),
                         infoBoxOutput("gas",width=3)),
                box(plotOutput("pres_ut",height="400px"),title="Comparativo Real - Periodo Anterior",solidHeader = TRUE,status="danger",width=4,height="500px"),
                box(tabBox(id="tabutilidadcomp",
                           tabPanel("Historico por Utilidad", plotOutput("utcompg")),
                           tabPanel("Ingreso y Gasto ",numericInput("anioinggas","Elige el Anio que quieras graficar:", value=year(today()),width="300px"),
                                    plotOutput("inggasg",height="300px")),width = 12),width = 8)
                
        ),
        #
        
        tabItem(tabName="gas",
                fluidRow(box(tabBox(id="gastotab",
                                    tabPanel("Participacion por Gatos",numericInput("Participagsp","Gastos con participacion Mayor que:", value=".01"), 
                                             plotlyOutput("gpgp")),
                                    tabPanel("Participacion por Almacen",numericInput("Participagsp2","Almacen con participacion Mayor que:", value=".01"), 
                                             plotlyOutput("gpgp2")),width=12),title="Graficas de Gastos",solidHeader = TRUE,status="danger",width=10),
                         box(uiOutput("Boxgs11"),
                             uiOutput("Boxgs12"),
                             uiOutput("Boxgs13"),
                             uiOutput("Boxgs14"),
                             uiOutput("Boxgs15"),title="Filtros",solidHeader = TRUE,status="danger",width=2)),  
                box(DT::dataTableOutput("tablegascomp"),width=20,title="Tipo de Gatos",solidHeader = TRUE,status="danger",collapsed="TRUE")
        ),
        
        tabItem(tabName="ing",
                fluidRow(box(uiOutput("Boxing11"),solidHeader = FALSE,width=2),
                         infoBoxOutput("venting",width=3),
                         infoBoxOutput("facing",width=3)),  
                "--------------------------------------------------------------------------------------------",
                fluidRow(box(plotlyOutput("ingfacagp",height=250),title="Graficas de Facturas por Documento",solidHeader = TRUE,status="success",width=6,height = 310),
                         box(plotlyOutput("ingfacfantgp",height=250),title="Graficas de Facturas por Mes de venta",solidHeader = TRUE,status="success",width=6,height=310),height=310),
                fluidRow(box(plotlyOutput("ingfaccvfacgp",height=250),title="Graficas de Facturas por Clave de Factura",solidHeader = TRUE,status="success",width=6,height=310),
                         box(plotlyOutput("ingfacstatfgp",height=250),title="Graficas de Facturas por Estatus Factura",solidHeader = TRUE,status="success",width=6,height = 310),height=310),
                "--------------------------------------------------------------------------------------------",
                fluidRow(box(plotlyOutput("ingvtagp",height=250),title="Graficas de Ventas por Documento",solidHeader = TRUE,status="info",width=4,height = 310),height=310)
        ),
        
        tabItem(tabName="men_Prod",
                
                fluidRow(box(selectInput("variableprod","Elige la variable que quieras Graficar:",c("Cantidad" = "CANT_VEN","Ventas" = "IMP_TOT","Utilidad" = "UTILIDAD","Costo" = "COST_PROM"),width="300px"),width=3),
                         box(uiOutput("Box11"),width=2),
                         box(uiOutput("Box22"),width=2),
                         box(uiOutput("Box33"),width=2),
                         box(uiOutput("Box44"),width=2)),
                fluidRow(infoBoxOutput("utprod",width=3),
                         infoBoxOutput("vtaprod",width=3),
                         infoBoxOutput("costprod",width=3),
                         infoBoxOutput("cantprod",width=3)),
                fluidRow(box(plotOutput("vppmgi"),title="Historico por Importe",solidHeader = TRUE,status="primary",width=6),
                         box(numericInput("Participap","Productos con participacion Mayor que:", value=".01",width="300px"), 
                             plotlyOutput("vpgp"),title="Participacion en Ventas",solidHeader = TRUE,status="danger",width=6)),
                fluidRow(box(DT::dataTableOutput("vsptb"),title="Tabla comparativa por anios",solidHeader = TRUE,status="primary",width=12))
                
                #DT::dataTableOutput("tabvtasprod")
                
        ),
        
        
        tabItem(tabName="men_prodcte",
                
                fluidRow(box(selectInput("variableprodcte","Elige la variable que quieras Graficar:",c("Cantidad" = "CANT_VEN","Ventas" = "IMP_TOT"),width="300px"),width=3),
                         box(uiOutput("Boxcp11"),width=2),
                         box(uiOutput("Boxcp22"),width=2),
                         box(uiOutput("Boxcp33"),width=2),
                         box(uiOutput("Boxcp44"),width=2)),
                fluidRow(box(numericInput("Participapc","Clientes con participacion Mayor que:", value=".01",width="300px"), 
                             plotlyOutput("vcpgp"),title="Participacion en Ventas",solidHeader = TRUE,status="danger",width=8),
                         box(numericInput("ventmayor","Clientes con Ventas Mayores que:", value="1000000",width="300px"),
                             uiOutput("numerocte"),
                             plotlyOutput("vcphgp"),title="Histograma de dias transcurridos de Req a OC",solidHeader = TRUE,status="primary",width=4)),
                DT::dataTableOutput("vcptp")
        ),
        
        
        tabItem(tabName="men_prodexist",
                
                fluidRow(box(uiOutput("Boxexiste1"),width=2),
                         box(textInput("descprodexist","Clave del Producto a graficar:", value="TOREXP",width="300px"),width=4),
                         box(textInput("atrprodexist","Clave del Atributo del Producto a graficar:", value="Todos",width="300px"),width=4),),
                box(plotOutput("vpemg"),title="Historico mensual por cantidad",solidHeader = TRUE,status="primary",width=12),
                DT::dataTableOutput("vspextb")
        ),
        
        
        tabItem(tabName="men_Agentes",
                fluidRow(box(uiOutput("Boxage1"),width=4),
                         box(uiOutput("Boxage2"),width=4),
                         box(uiOutput("Boxage3"),width=4)
                ),
                fluidRow(
                  ##br(),
                  box(numericInput("ANIOAGE","Elige el Anio que quieras graficar:", value=year(today()),width="300px"), 
                      plotOutput("agehr"),title="Ventas por Hora",solidHeader = TRUE,status="primary"),
                  box(plotOutput("agemes"),title="Historico de Mensual",solidHeader = TRUE,status="primary")),
                fluidRow(box(numericInput("Participaage","Agentes con participacion Mayor que:", value=".01",width="300px"), 
                             plotlyOutput("vagp"),title="Participacion en Ventas por Agentes",solidHeader = TRUE,status="danger",width=12)),
                fluidRow(box(tabBox(id="agentetab",
                                    tabPanel("Participacion por producto",numericInput("Participanvp","Agentes con participacion Mayor que:", value=".01"), 
                                             plotlyOutput("vpnvgp")),
                                    tabPanel("Historico por producto",numericInput("ANIONVHIST","Elige el Anio que quieras graficar:", value=year(today())), 
                                             plotlyOutput("vpnvgl")),width=12),title="Productos",solidHeader = TRUE,status="danger",width=10),
                         box(uiOutput("Boxnv11"),
                             uiOutput("Boxnv22"),
                             uiOutput("Boxnv33"),title="Filtros",solidHeader = TRUE,status="danger",width=2))                
                
        ),
        

        tabItem(tabName="men_rescob",
                fluidRow(box(DT::dataTableOutput("tabprmf"),title="Porcentaje de Monto Facturado con pago retrasado",solidHeader = TRUE,status="primary",width=6),
                         box(DT::dataTableOutput("tabprnf"),title="Porcentaje de Numero de Facturas con pago retrasado",solidHeader = TRUE,status="danger",width=6)),
                fluidRow(box(DT::dataTableOutput("tabp5mf"),title="Porcentaje de Monto Facturado con pago menor o igual a 5 dias",solidHeader = TRUE,status="primary",width=6),
                         box(DT::dataTableOutput("tabp5nf"),title="Porcentaje de Numero de Facturas con pago menor o igual a 5 dias",solidHeader = TRUE,status="danger",width=6))
                
                ),
        
        
                
        tabItem(tabName="men_cobcte",
                fluidRow(uiOutput("ctecob")),
                fluidRow(box(uiOutput("vendcob"),width=3),
                         box(uiOutput("diacre"),width=3),
                         box(uiOutput("avgdp"),width=3),
                         box(uiOutput("mtsf"),width=3)),
                fluidRow(box(plotOutput("barcob"),title="Total de Facturas Pagadas a Tiempo o con Retraso",solidHeader = TRUE,status="primary",width=5),
                         box("----Facturas con Retraso-------------------",
                             br(),
                             DT::dataTableOutput('tabdpr'),
                             br(),
                             "----Facturas pagadas En Tiempo -------------------",
                             br(),
                             DT::dataTableOutput('tabdpat'),
                             title="Promedio de Dias de pago",solidHeader = TRUE,status="primary",width = 6)),
                fluidRow(box(DT::dataTableOutput("tabfpc"),title="Facturas pendientes de cobro",solidHeader = TRUE,status="danger",width=5),
                         box(DT::dataTableOutput("tabdc"), title="Promedio de dias que se han dado de credito",solidHeader = TRUE,status="danger",width=6)),
                fluidRow(box(DT::dataTableOutput("tablarescob"),title="Tabla de cobranza",solidHeader = TRUE,status="primary",width=12))
                ),
        
        tabItem(tabName="men_evcob",
                fluidRow(box(uiOutput("agecob"),width=3),
                         box(uiOutput("mescob"),width=2),
                         box(uiOutput("aniocob"),width=2)),
                fluidRow(box(h3("----------------------Detalles de Facturas a Credito ------------------"),
                             br(),
                             tableOutput('sumacob'),
                             br(),
                             h3("----------------------Detalles de Facturas de Contado ---------------"),
                             br(),
                             tableOutput('sumacob_credcero'),width=5),
                         box(plotlyOutput("porfnocob", height = "50%"),
                             br(),
                             plotlyOutput("pormnocob"),width=7))
                ),

        
        tabItem(tabName="men_ciccomp",
                fluidRow(box(uiOutput("reqpen"),width=3),
                         box(uiOutput("ocpen"),width=3),
                         box(uiOutput("rempen"),width=3),
                         box(numericInput("anioclc","Anio a evaluar:", value=2020,width="300px"),
                             numericInput("mesclc","Mes a evaluar:", value=month(today()),width="300px"),title="Filtros para Histogramas 1, 2 y 3",solidHeader = TRUE,status="primary",width=3)),
                fluidRow(box(DT::dataTableOutput('diasreq'),title="Promedio mensual de dias transcurridos de REQ a OC",solidHeader = TRUE,status="warning"),
                         box(uiOutput("numeroreq"),
                             plotlyOutput("diasreqhgp",height=200),title="1.- Histograma de dias transcurridos de REQ a OC",solidHeader = TRUE,status="warning",width=4,height=300)),
                fluidRow(box(DT::dataTableOutput('diasoc'),title="Promedio mensual de dias transcurridos de OC a REM",solidHeader = TRUE,status="success"),
                         box(uiOutput("numerooc"),
                             plotlyOutput("diasochgp",height=200),title="2.- Histograma de dias transcurridos de OC a REM",solidHeader = TRUE,status="success",width=4,height=300)),
                fluidRow(box(DT::dataTableOutput('diastot'),title="Promedio mensual de dias transcurridos de REQ a REM",solidHeader = TRUE,status="primary"),
                         box(uiOutput("numerotot"),
                             plotlyOutput("diastothgp",height=200),title="3.-Histograma de dias transcurridos de REQ a REM",solidHeader = TRUE,status="primary",width=4,height=300)),
                fluidRow(box(tabBox(id="tabclo",
                             tabPanel("Detalle completo por producto",DT::dataTableOutput("tabclodoc")),
                             tabPanel("Detalle de dias",DT::dataTableOutput("tabdiasavg")),
                             tabPanel("Detalle de docuemtos pendientes",DT::dataTableOutput("tabdocpend")),width = 12),title="Detalle de documentos en Tablas",solidHeader = TRUE,status="danger",width = 12)),
                fluidRow(box(h3("-------------------------------------------------------------------------------------")))
        ##EDGAR                                
)





      ) # cierra TABS del menu
    }
    else {
      loginpage
    }
  })
  
###########################################################################################################
###########################################################################################################
######################################   SERVER - SERVER   ################################################
###########################################################################################################
###########################################################################################################
  
  fecha_ini<-eventReactive(input$actualizar,{
    as.Date(paste0(year(input$fec[1]),"-",str_pad(month(input$fec[1]), 2, side = "left", pad = "0"),"-01"))
  } , ignoreNULL = FALSE)
  
  fecha_fin<-eventReactive(input$actualizar,{
    as.Date(paste0(year(input$fec[2]),"-",str_pad(month(input$fec[2]), 2, side = "left", pad = "0"),"-01"))
  } , ignoreNULL = FALSE)
  
  #######Fecha para calculo de tablas mensuales########
  
  fecha_ini2<-eventReactive(input$actualizar,{
    as.Date(paste0(year(input$fec[1])-1,"-",str_pad(month(input$fec[1]), 2, side = "left", pad = "0"),"-01"))
  } , ignoreNULL = FALSE)
  
  fecha_fin2<-eventReactive(input$actualizar,{
    as.Date(paste0(year(input$fec[2])-1,"-",str_pad(month(input$fec[2]), 2, side = "left", pad = "0"),"-01"))
  } , ignoreNULL = FALSE)
  
  #######Fecha para calculo anio anterior########
  
  fecha_ini3<-eventReactive(input$actualizar,{
    as.Date(paste0(year(input$fec[1])-1,"-01-01"))
  } , ignoreNULL = FALSE)
  
  fecha_fin3<-eventReactive(input$actualizar,{
    as.Date(paste0(year(input$fec[2])-1,"-12-01"))
  } , ignoreNULL = FALSE)
  
  
  ###########################################################
  ####################  CLIENTES  ###########################
  ###########################################################
  
  #################Clientes mostrador############
  
  ###############  POR HORA  ####################
  agehr<-reactive({ #Ventas por mes grafica importe Todos
    agehr=agehora[which(agehora$FECHA>="2017-01-01" & agehora$FECHA<=today()),]
    agehr=agehr[which(agehr$HORA>=8 & agehr$HORA<=18),]
    agehr= agehr[which(agehr$ANIO==input$ANIO),]
    agehr=ddply(agehr[],.(MES,HORA),summarize,VENTA=sum(VENTA))
  })
  
  output$agehr<- renderPlot({  ###Historico Importe
    ggplot(agehr() ,aes(x=HORA,y=VENTA, group=factor(MES), shape=factor(MES), colour=factor(MES)))+geom_line(size=2)+ scale_y_continuous(labels = comma) + scale_x_discrete(limits = c(8:18)) + xlab("HORA") + ylab("NUMERO DE NOTAS DE VENTA") +geom_text(stat="identity",aes(check_overlap = TRUE,label=prettyNum(round(VENTA,0),big.mark=",",scientific=FALSE)),size=3,color="black")
  })
  
  ###############  POR MES  ####################
  agemes<-reactive({ #Ventas por mes grafica importe Todos
    agemes=agevtahist[which(agevtahist$FECHA>="2017-01-01" & agevtahist$FECHA<=today()),]
    agemes=ddply(agemes[],.(ANIO,MES),summarize,VENTA=sum(VENTA))
  })
  
  output$agemes<- renderPlot({  ###Historico Importe
    ggplot(agemes() ,aes(x=MES,y=VENTA, group=factor(ANIO), shape=factor(ANIO), colour=factor(ANIO)))+geom_line(size=2)+ scale_y_continuous(labels = comma) + scale_x_discrete(limits = c(1:12)) + xlab("MES") + ylab("NUMERO DE NOTAS DE VENTA") +geom_text(stat="identity",aes(check_overlap = TRUE,label=prettyNum(round(VENTA,0),big.mark=",",scientific=FALSE)),size=3,color="black")
  })
  
  
  #################Resumen de Clientes############
  
  ############### Catalogo de cientes  ####################
  
  #output$Box4 = renderUI(selectInput("vend","Elige el Vendedor:",unique(catcte$NOM_AGE),"Todos"))
  
  #output$Box1 = renderUI(selectInput("suc","Elige la Zona:",if (input$vend=="Todos") unique(catcte$NOM_ZON) else catcte$NOM_ZON[which(catcte$NOM_AGE == input$vend)],"Todos"))
  
  output$Box2 = renderUI(selectInput("tipcte","Elige un Tipo de Cliente:",unique(catcte$TIPO),"Todos"))
  
  output$Box3 = renderUI(selectInput("cte","Elige un Cliente:",if (input$tipcte=="Todos") unique(catcte$NOMCTE) else catcte$NOMCTE[which(catcte$TIPO == input$tipcte)],"Todos"))
  
  ############### Grafica POR MES  ####################
  
  vscmg<-reactive({ #Ventas por mes grafica importe Todos
    vscmg=vtasctes[which(vtasctes$FECHA>="2018-01-01" & vtasctes$FECHA<=today()),]
    vtcmg= if (input$tipcte=="Todos") vscmg else vscmg[which(vscmg$NOM_CAN==input$tipcte),]
    vcmg= if (input$cte=="Todos") vtcmg else vtcmg[which(vtcmg$NOM_CTE==input$cte),] 
    vscmg=select(vcmg,ANIO,MES,MTOVTA)
    vscmg=ddply(vscmg[],.(ANIO,MES),summarize,MTOVTA=sum(MTOVTA))
  })
  
  #output$vcmg<- renderPlot({  ###Historico Importe
  #  ggplot(vscmg() ,aes(x=MES,y=MTOVTA, group=factor(ANIO), shape=factor(ANIO), colour=factor(ANIO)))+geom_line(size=2)+ scale_y_continuous(labels = comma) + scale_x_discrete(limits = c(1:12)) + xlab("MES") + ylab("IMPORTE") +geom_text(hjust="center",stat="identity",check_overlap = TRUE,aes(label=prettyNum(round(MTOVTA,0),big.mark=",",scientific=FALSE)),size=3,color="black")
  #})
  
  output$vcmg<- renderPlotly({  ###Historico Importe
    plot_ly(vscmg(), x = ~MES, y = ~MTOVTA, type = 'scatter', mode = 'lines', color= ~factor(ANIO), text = ~paste("Mes: ", MES),  hovertemplate = paste(
      "<b>%{text}</b><br>",
      "%{yaxis.title.text}: %{y:$,.0f}<br>"),
      line = list(width = 4)) %>%
      layout(showlegend = TRUE,xaxis = list(range= c(1:12),title = "Meses"),
             yaxis = list(title = "Importe de Ventas"),
             legend = list(orientation = 'v',font = list(size = 9)))
  })
  
  
  ############### Grafica de PASTEL  ####################
  
  vscgp<-reactive({ #Grafica de pastel
    vscgp=vtasctes[which(vtasctes$FECHA>=fecha_ini() & vtasctes$FECHA<=fecha_fin()),]   
    vtcgp= if (input$tipcte=="Todos") vscgp else vscgp[which(vscgp$NOM_CAN==input$tipcte),]
    #vcgp= if (input$cte=="Todos") vtcgp else vtcgp[which(vtcgp$NOM_CTE==input$cte),] 
    vscgp=select(vtcgp,NOM_CAN,NOM_CTE,MTOVTA)
    
    vscgp= if (input$tipcte=="Todos") ddply(vscgp[],.(NOM_CAN),summarize,MTOVTA=sum(MTOVTA)) else vscgp
    vscgp= if (input$tipcte!="Todos") ddply(vscgp[],.(NOM_CTE),summarize,MTOVTA=sum(MTOVTA)) else vscgp
    a= colSums(vscgp[,2,drop=FALSE])
    vscgp=ddply(vscgp[],.(vscgp[,1]),summarize,MTOVTA=sum(MTOVTA),Part=MTOVTA/a)
    vscgp=as.data.frame(vscgp)
    colnames(vscgp)=c("nombre","MTOVTA","Part")
    otro=vscgp[which(vscgp$Part<=input$Participa),]
    a=colSums(otro[,2,drop=FALSE])
    b=colSums(otro[,3,drop=FALSE])
    otros=list(nombre="Otros", MTOVTA=a, Part=b)
    vscgp=rbind(vscgp,otros)
    vscgp=vscgp[which(vscgp$Part>input$Participa),]
    vscgp=as.data.frame(vscgp)
  })
  
  output$vcgp<- renderPlotly({  ###Participacion en ventas
    plot_ly(vscgp(), labels = ~vscgp()[,1], values = ~vscgp()[,2], type = 'pie')%>%layout(legend = list(orientation = 'r',font = list(size = 9)))
  })
  
  ###############  Participacion por producto  ####################
  prodcte<-reactive({ #catalogo de productos subgrupo
    prodcte= if (input$clasecte=="Todos") catprod else catprod[which(catprod$CSE_PROD==input$clasecte),]
    prodcte= if (input$grupocte=="Todos") prodcte else prodcte[which(prodcte$SUB_CSE==input$grupocte),]
  })
  
  output$Boxcte11 = renderUI(selectInput("clasecte","Elige la Clase de Producto:",unique(catprod$CSE_PROD),"Todos"))
  output$Boxcte22 = renderUI(selectInput("grupocte","Elige el Grupo:",if (input$clasecte=="Todos") unique(catprod$SUB_CSE) else catprod$SUB_CSE[which(catprod$CSE_PROD == input$clasecte)],"Todos"))
  output$Boxcte33 = renderUI(selectInput("subgrupocte","Elige el SubGrupo:",unique(prodcte()$SUB_SUBCSE),"Todos"))
  
  
  vpctegp<-reactive({ #Grafica de pastel
    vpctegp=cteprod[which(cteprod$FECHA>=fecha_ini() & cteprod$FECHA<=fecha_fin()),]
    vpctegp= if (input$tipcte=="Todos") vpctegp else vpctegp[which(vpctegp$NOM_CAN==input$tipcte),]
    vpctegp= if (input$cte=="Todos") vpctegp else vpctegp[which(vpctegp$NOM_CTE==input$cte),]
    vpctegp= if (input$clasecte=="Todos") vpctegp else vpctegp[which(vpctegp$CSE_PROD==input$clasecte),]
    vpctegp= if (input$grupocte=="Todos") vpctegp else vpctegp[which(vpctegp$SUB_CSE==input$grupocte),]
    vpctegp= if (input$subgrupocte=="Todos") vpctegp else vpctegp[which(vpctegp$SUB_SUBCSE==input$subgrupocte),]
    vpctegp=select(vpctegp,CSE_PROD,SUB_CSE,SUB_SUBCSE,DESC_PROD,CANT_VEN)
    
    vpctegp= if (input$clasecte=="Todos") ddply(vpctegp[],.(CSE_PROD),summarize,CANT_VEN=sum(CANT_VEN)) else vpctegp
    vpctegp= if (input$clasecte!="Todos" & input$grupocte=="Todos") ddply(vpctegp[],.(SUB_CSE),summarize,CANT_VEN=sum(CANT_VEN)) else vpctegp
    vpctegp= if (input$clasecte!="Todos" & input$grupocte!="Todos" & input$subgrupocte=="Todos") ddply(vpctegp[],.(SUB_SUBCSE),summarize,CANT_VEN=sum(CANT_VEN)) else vpctegp
    vpctegp= if (input$clasecte!="Todos" & input$grupocte!="Todos" & input$subgrupocte!="Todos") ddply(vpctegp[],.(DESC_PROD),summarize,CANT_VEN=sum(CANT_VEN)) else vpctegp
    a= colSums(vpctegp[,2,drop=FALSE])
    vpctegp=ddply(vpctegp[],.(vpctegp[,1]),summarize,CANT_VEN=sum(CANT_VEN),Part=CANT_VEN/a)
    vpctegp=as.data.frame(vpctegp)
    colnames(vpctegp)=c("nombre","MTOVTA","Part")
    otro=vpctegp[which(vpctegp$Part<=input$Participactep),]
    a=colSums(otro[,2,drop=FALSE])
    b=colSums(otro[,3,drop=FALSE])
    otros=list(nombre="Otros", MTOVTA=a, Part=b)
    vpctegp=rbind(vpctegp,otros)
    vpctegp=vpctegp[which(vpctegp$Part>input$Participactep),]
    vpctegp=as.data.frame(vpctegp)
  })
  output$vpctegp<- renderPlotly({  ###Participacion en ventas
    plot_ly(vpctegp(), labels = ~vpctegp()[,1], values = ~vpctegp()[,2], type = 'pie')%>%layout(legend = list(orientation = 'h',font = list(size = 11)))
  })
  
  
  ###############  Historico por producto  ####################
  vpctegl<-reactive({ #Historico por producto
    vpctegl=cteprod[which(cteprod$ANIO==input$ANIOCTEHIST),]
    vpctegl= if (input$tipcte=="Todos") vpctegl else vpctegl[which(vpctegl$NOM_CAN==input$tipcte),]
    vpctegl= if (input$cte=="Todos") vpctegl else vpctegl[which(vpctegl$NOM_CTE==input$cte),]
    vpctegl= if (input$clasecte=="Todos") vpctegl else vpctegl[which(vpctegl$CSE_PROD==input$clasecte),]
    vpctegl= if (input$grupocte=="Todos") vpctegl else vpctegl[which(vpctegl$SUB_CSE==input$grupocte),]
    vpctegl= if (input$subgrupocte=="Todos") vpctegl else vpctegl[which(vpctegl$SUB_SUBCSE==input$subgrupocte),]
    vpctegl=select(vpctegl,MES,CSE_PROD,SUB_CSE,SUB_SUBCSE,DESC_PROD,CANT_VEN)
    
    vpctegl= if (input$clasecte=="Todos") ddply(vpctegl[],.(CSE_PROD,MES),summarize,CANT_VEN=sum(CANT_VEN)) else vpctegl
    vpctegl= if (input$clasecte!="Todos" & input$grupocte=="Todos") ddply(vpctegl[],.(SUB_CSE,MES),summarize,CANT_VEN=sum(CANT_VEN)) else vpctegl
    vpctegl= if (input$clasecte!="Todos" & input$grupocte!="Todos" & input$subgrupocte=="Todos") ddply(vpctegl[],.(SUB_SUBCSE,MES),summarize,CANT_VEN=sum(CANT_VEN)) else vpctegl
    vpctegl= if (input$clasecte!="Todos" & input$grupocte!="Todos" & input$subgrupocte!="Todos") ddply(vpctegl[],.(DESC_PROD,MES),summarize,CANT_VEN=sum(CANT_VEN)) else vpctegl
    colnames(vpctegl)=c("CLAS","MES","VENTA")
    vpctegl=as.data.frame(vpctegl)
  })
  
  output$vpctegl<- renderPlotly({  ###Participacion en ventas
    plot_ly(vpctegl(), x = ~MES, y = ~VENTA, type = 'scatter', mode = 'lines',color= ~CLAS,
            line = list(width = 4)) %>%
      layout(showlegend = FALSE,xaxis = list(range= c(1, 12)),
             yaxis = list (title = "Numero de Ventas"),
             legend = list(orientation = 'h',font = list(size = 9)))
  })
  
  
  
  
  
  ############### Tabla de Ventas por cliente  ####################
  
  vscm<-eventReactive(input$actualizar,{ #Tabla ventas por cliente 
    vcm=vtasctes[which(vtasctes$FECHA>=fecha_ini() & vtasctes$FECHA<=fecha_fin()),]
    vcm=select(vcm,-ANIO,-MES,-FECHA)
    vcm=ddply(vcm[],.(NOM_CAN,NOM_CTE),summarize,MTOVTA=sum(MTOVTA))
    
    vpmytd=vtasctes[which(vtasctes$FECHA>=fecha_ini2() & vtasctes$FECHA<=fecha_fin2()),]
    vpmytd=select(vpmytd,-ANIO,-MES,-FECHA)
    vpmytd=ddply(vpmytd[],.(NOM_CAN,NOM_CTE),summarize,MTOVTA_ACUM_AA=sum(MTOVTA))
    
    vpmtan=vtasctes[which(vtasctes$FECHA>=fecha_ini3() & vtasctes$FECHA<=fecha_fin3()),]
    vpmtan=select(vpmtan,-ANIO,-MES,-FECHA)
    vpmtan=ddply(vpmtan[],.(NOM_CAN,NOM_CTE),summarize,MTOVTA_TOT_AA=sum(MTOVTA))
    
    if (fecha_ini()>as.Date("2018-07-31")) vcm=merge(x=vcm,vpmytd,all.x=TRUE) else vcm
    if (fecha_ini()>as.Date("2018-07-31")) vcm=merge(x=vcm,vpmtan,all.x=TRUE) else vcm
  } , ignoreNULL = FALSE)
  
  output$tablevtascte <- DT::renderDataTable(DT::datatable(vscm(),
                                                           filter = 'top',
                                                           extensions = 'Buttons', 
                                                           options = list(lengthMenu = list(c(10,20,50,100, -1), list('10', '20', '50', '100', 'All')),pageLength = 10, autoWidth = TRUE,scrollX = TRUE,dom = 'lBfrtip',
                                                                          buttons = c('copy', 'excel')))%>%formatStyle(columns =1:2, fontSize = '80%')%>%formatCurrency(columns=3:6, currency = "$  ", interval = 3, mark = ",", digits = 0)
  )
  
  
  #output$tablevtascte <- DT::renderDataTable(DT::datatable(vscm(), filter = 'top', options = list(pageLength = 10, autoWidth = TRUE))%>%formatCurrency(columns=4:6, currency = "$  ", interval = 3, mark = ",", digits = 0))
  
  
  #################Segmentacion de clientes por frecuencia de compra ############
  
  ###############  PARTICIPACION POR SEGMENTO  ####################
  partseg<-reactive({ #Grafica de pastel
    partseg= if (input$canal=="Todos") segfreccte else segfreccte[which(segfreccte$NOM_CAN==input$canal),]
    partseg= ddply(partseg[],.(ETIQUETA),summarize,NUMCTE=length(ETIQUETA))
    a= colSums(partseg[,2,drop=FALSE])
    partseg=ddply(partseg[],.(partseg[,1]),summarize,NUMCTE=sum(NUMCTE),Part=NUMCTE/a)
    partseg=as.data.frame(partseg)
  })
  
  output$partseg<- renderPlotly({  ###Participacion en ventas
    #plot_ly(partseg(), labels = ~partseg()[,1], values = ~partseg()[,2], type = 'pie')%>%layout(legend = list(orientation = 'h'))
    plot_ly(partseg(), labels = ~partseg()[,1], values = ~partseg()[,2], type = 'pie')%>%layout(legend = list(orientation = 'h'))
  })
  
  ###############  HISTORICOS CAPTACION DE CTES NVOS  ####################
  capctenvo<-reactive({ #captacion de clientes por mes
    capctenvo= if (input$canal=="Todos") segfreccte else segfreccte[which(segfreccte$NOM_CAN==input$canal),]
    capctenvo=ddply(capctenvo[],.(ANIO=substr(ALTACTE,1,4),MES=substr(ALTACTE,6,7)),summarize,CONTCTES=length(CVE_CTE))
    capctenvo=select(capctenvo,ANIO,MES,CONTCTES)
    capctenvo=capctenvo[which(capctenvo$ANIO>=2017),]
  })
  
  output$capctenvo<- renderPlot({  ###Historico Importe
    ggplot(capctenvo() ,aes(x=MES,y=CONTCTES, group=factor(ANIO), shape=factor(ANIO), colour=factor(ANIO)))+geom_line(size=2)+ scale_y_continuous(labels = comma) + scale_x_discrete(limits = c(1:12)) + xlab("MES") + ylab("CLIENTES NUEVOS REGISTRADOS") +geom_text(stat="identity",aes(check_overlap = TRUE,label=prettyNum(round(CONTCTES,0),big.mark=",",scientific=FALSE)),size=4,color="black")
  })
  
  ###############  HISTORICOS DE VTAS CTES NVOS  ####################
  histctesnvos<-reactive({ #Ventas por mes grafica importe Todos
    histctesnvos=ctesnvos[which(ctesnvos$FECHA>="2017-01-01" & ctesnvos$FECHA<=today()),]
    histctesnvos= histctesnvos[which(histctesnvos$ANIO==input$ANION),]
    histctesnvos= if (input$canal=="Todos") histctesnvos else histctesnvos[which(histctesnvos$NOM_CAN==input$canal),]
    histctesnvos=select(histctesnvos,NUEV_RET,MES,"IMPORTE"= input$vargraf)
    histctesnvos=ddply(histctesnvos[],.(NUEV_RET,MES),summarize,IMPORTE=sum(IMPORTE))
  })
  
  output$histctesnvos<- renderPlot({  ###Historico Importe
    ggplot(histctesnvos() ,aes(x=MES,y=IMPORTE, group=factor(NUEV_RET), shape=factor(NUEV_RET), colour=factor(NUEV_RET))) +geom_line(size=2)+ scale_y_continuous(labels = comma) + scale_x_discrete(limits = c(1:12)) + xlab("MES") +geom_text(stat="identity",aes(check_overlap = TRUE,label=prettyNum(round(IMPORTE,0),big.mark=",",scientific=FALSE)),size=4,color="black")
  })
  
  output$canalcte = renderUI(selectInput("canal","Elige el Canal de Ventas:",c(as.character(segfreccte$NOM_CAN),"Todos"),"Todos"))
  
  ###############  TABLA Resumen por Cliente  ####################
  
  output$rescte <- DT::renderDataTable(DT::datatable(segfreccte[,1:12], filter = 'top', options = list(pageLength = 10, autoWidth = TRUE))%>%formatCurrency(columns=7, currency = "$  ", interval = 3, mark = ",", digits = 0)%>%formatStyle(columns = c(3,10,12), fontSize = '70%')%>%formatCurrency(columns=8, currency = "", interval = 3, mark = ",", digits = 0))
  
  ###############  TABLA Resumen por Mes  ####################
  detctemes=ctesnvos[which(ctesnvos$FECHA>="2017-01-01" & ctesnvos$FECHA<=today()),]
  detctemes<-select(detctemes,-ANIO,-MES,-ETIQ)
  detctemes<-detctemes[,1:11]
  
  output$detctemes <- DT::renderDataTable(DT::datatable(detctemes, filter = 'top', options = list(pageLength = 10, autoWidth = TRUE))%>%formatCurrency(columns=8, currency = "$  ", interval = 3, mark = ",", digits = 0)%>%formatCurrency(columns=9, currency = "", interval = 3, mark = ",", digits = 0)
  )
  
  ###############  TABLA Resumen por Factura  ####################
  
  output$detctefac <- DT::renderDataTable(DT::datatable(detctefac[,1:9], filter = 'top', options = list(pageLength = 10, autoWidth = TRUE))%>%formatCurrency(columns=6, currency = "$  ", interval = 3, mark = ",", digits = 0))
  
  
  
  ###########################################################
  #################### FLUJOS DE EFECTIVO  ###########################
  ###########################################################
  
  
  #################  Resumen  ##################
  
  flujoef<-reactive({ #Ventas por mes grafica importe Todos
    flujoef=fe[which(fe$F_VENCI<=input$fecflujo),]
  })
  
  
  ###############  Info boxes de cxc y cxp ####################
  
  ####Cuentas x cobrar por vencer
  
  vfecxc<-reactive({ #Ventas por mes grafica importe Todos
    vfecxc=flujoef()[which(flujoef()$STATUSFAC=="xvencer"),]
    vfecxc=vfecxc[which(vfecxc$C_P_C=="cxc"),]
    vfecxc = colSums(vfecxc[,9,drop=FALSE])
  })
  
  output$fecxc1<-renderText({
    vfecxc = vfecxc()
    vfecxc= prettyNum(round(vfecxc,0),big.mark=",",scientific=FALSE)
    paste0("+ CXC por vencer ",vfecxc)
  })
  
  ####Cuentas x pagar por vencer
  
  vfecxp<-reactive({ #Ventas por mes grafica importe Todos
    vfecxp=flujoef()[which(flujoef()$STATUSFAC=="xvencer"),]
    vfecxp=vfecxp[which(vfecxp$C_P_C=="cxp"),]
    vfecxp = colSums(vfecxp[,9,drop=FALSE])
  })
 
  output$fecxp1<-renderText({
    vfecxp = vfecxp()
    vfecxp= prettyNum(round(vfecxp,0),big.mark=",",scientific=FALSE)
    paste0("- CXP por vencer ",vfecxp)
  })
  
  ####Flujo Total no considera Vencidas
  
  output$subtfe1<-renderText({
    fexv = vfecxc()-vfecxp()
    fexv= prettyNum(round(fexv,0),big.mark=",",scientific=FALSE,style={"font-size:150%;font-family:cambria;text-align:center"})
    paste0("  ","= Total por Vence "," "," "," ",fexv)
  })
  
  ####Cuentas x cobrar vencidas
  vfecxcv<-reactive({ 
    vfecxcv=flujoef()[which(flujoef()$STATUSFAC=="vencido"),]
    vfecxcv=vfecxcv[which(vfecxcv$C_P_C=="cxc"),]
    vfecxcv = colSums(vfecxcv[,9,drop=FALSE])
    
  })
  output$fecxc2<-renderText({
    vfecxcv = vfecxcv()
    vfecxcv= prettyNum(round(vfecxcv,0),big.mark=",",scientific=FALSE)
    paste0("+ CXC Vencidas ",vfecxcv)
  })
  
  
  ####Cuentas x pagar vencidas
  
  vfecxpv<-reactive({ #Ventas por mes grafica importe Todos
    vfecxpv=flujoef()[which(flujoef()$STATUSFAC=="vencido"),]
    vfecxpv=vfecxpv[which(vfecxpv$C_P_C=="cxp"),]
    vfecxpv = colSums(vfecxpv[,9,drop=FALSE])
  })
  
  output$fecxp2<-renderText({
    vfecxpv = vfecxpv()
    vfecxpv= prettyNum(round(vfecxpv,0),big.mark=",",scientific=FALSE)
    paste0("- CXP Vencidas   ",vfecxpv)
  })
    
  ####Flujo Total Vencidas
  
  output$subtfe2<-renderText({
    fexv = vfecxcv()-vfecxpv()
    fexv= prettyNum(round(fexv,0),big.mark=",",scientific=FALSE)
    paste0("= Total Vencidas ",fexv)
  })
  
  ####Flujo Total
  
  output$totfe<-renderText({
    fexv = vfecxc()-vfecxp()+vfecxcv()-vfecxpv()
    fexv= prettyNum(round(fexv,0),big.mark=",",scientific=FALSE)
    paste0("=Flujo Total  ",fexv)
  })
  
  ####Notas de Credito Cxc
  
  fecxcvncred<-reactive({ 
    fecxcvncred=fe[which(fe$C_P_C=="cxc"),]
    fecxcvncred = colSums(na.omit(fecxcvncred[,12,drop=FALSE]))
  })
  
  output$fecxcvncred<-renderText({
    fexv = fecxcvncred()
    fexv= prettyNum(round(fexv,0),big.mark=",",scientific=FALSE)
    paste0("- Notas de Credito CXC  ",fexv)
  })
  
  ####Notas de Credito Cxp
  
  fecxpcncred<-reactive({ 
    fecxpcncred=fe[which(fe$C_P_C=="cxp"),]
    fecxpcncred = colSums(na.omit(fecxpcncred[,12,drop=FALSE]))
  })
  
  output$fecxpcncred<-renderText({
    fexv = fecxpcncred()
    fexv= prettyNum(round(fexv,0),big.mark=",",scientific=FALSE)
    paste0("+ Notas de Credito CXP  ",fexv)
  })
  
  ####Notas de Cargo Cxc
  
  fecxcvncargo<-reactive({ 
    fecxcvncargo=fe[which(fe$C_P_C=="cxc"),]
    fecxcvncargo = colSums(na.omit(fecxcvncargo[,13,drop=FALSE]))
  })
  
  output$fecxcvncargo<-renderText({
    fexv = fecxcvncargo()
    fexv= prettyNum(round(fexv,0),big.mark=",",scientific=FALSE)
    paste0("+ Notas de Cargo CXC  ",as.character(fexv))
  })
  
  ####Notas de Cargo Cxp
  
  fecxpcncargo<-reactive({ 
    fecxpcncargo=fe[which(fe$C_P_C=="cxp"),]
    fecxpcncargo = colSums(na.omit(fecxpcncargo[,13,drop=FALSE]))
  })
  
  output$fecxpcncargo<-renderText({
    fexv = fecxpcncargo()
    fexv= prettyNum(round(fexv,0),big.mark=",",scientific=FALSE)
    fexv= as.character(paste0('- Notas de Cargo CXP ',as.character(fexv)))
    
  })
  

  
  ###############  Grafica de comportamiento cxc y cxp por vencer ####################          
  
  fehist<-reactive({ #Ventas por mes grafica importe Todos
    fehist=fe[which(fe$DIAS_VENC>=0),]
    fehist=fehist[which(fehist$F_VENC<=as.Date(paste0(year(today()),"-12","-31"))),]
    fehist=ddply(fehist[],.(FAC,F_VENCI,C_P_C),summarize,MONTO=sum(MONTO_ACUM))
    fehist=fehist[,-1]
    fehist=fehist[order(fehist$F_VENCI),]
  })
  
  
  #   output$fehist<- renderPlot({  ###Historico Importe
  #   ggplot(fehist() ,aes(x=F_VENCI,y=MONTO, group=factor(C_P_C), shape=factor(C_P_C), colour=factor(C_P_C))) +geom_line(size=2)+ scale_y_continuous(labels = comma) + xlab("Vencimiento")# +geom_text(stat="identity",aes(check_overlap = TRUE,label=prettyNum(round(MONTO,0),big.mark=",",scientific=FALSE)),size=3,color="black")
  # })
  
  output$fehist<- renderPlotly({  ###
    plot_ly(fehist(), x =fehist()$F_VENCI, y = ~MONTO, type = 'scatter', mode = 'lines',color= ~factor(C_P_C),
            line = list(width = 4)) %>%
      layout(xaxis = list (title = "Vencimiento"),
             yaxis = list (title = "Monto"),
             legend = list(orientation = 'R',font = list(size = 8)))
  })
  
  

  
  
  ###############  Tabla del FE por Facturas ###########################
  
  fetabla<-reactive({ #Ventas por mes grafica importe Todos
    fehist=fe[,1:13]
    fehist=select(fehist,-MONTO_ACUM,-LUGAR)
  })
  
  output$fetabla <- DT::renderDataTable(DT::datatable(fetabla(), filter = 'top', options = list(pageLength = 10, autoWidth = TRUE))%>%formatCurrency(columns=(9:11), currency = "$  ", interval = 3, mark = ",", digits = 0)%>%formatCurrency(columns=7:8, currency = "", interval = 3, mark = ",", digits = 0))
  
  
  
  
  
  
  #################Cuentas por Pagar############
  
  ###############  Facturas de compra por Vencer ####################
  datacxp <- reactive({
    tcxp=cxp[,1:19]
    tcxp=tcxp[,-17]
    names(tcxp)[14] = "CUARENTA Y CINCO"
    tcxp=as.data.frame(tcxp)
    
    })
  
  
  
  output$dowcxp <- DT::renderDataTable(DT::datatable(datacxp(),
                                                     filter = 'top',
                                                     extensions = 'Buttons', 
                                                     options = list(lengthMenu = list(c(10,20,50,100, -1), list('10', '20', '50', '100', 'All')),pageLength = 10, autoWidth = TRUE,scrollX = TRUE,dom = 'lBfrtip',
                                                                    buttons = c('copy', 'excel')))%>%formatStyle(columns =1:18, fontSize = '80%')%>%formatCurrency(columns=10:18, currency = "$  ", interval = 3, mark = ",", digits = 0)%>%formatCurrency(columns=8:9, currency = "", interval = 3, mark = ",", digits = 0)
  )
  
  
  ###############  Grafica de barras facturas de compra por vencer ####################
  
  grafbarcxp<-reactive({
    VENCIDA = round(colSums(na.omit(cxp[,10,drop=FALSE])),digits = 0)
    SIETE = round(colSums(na.omit(cxp[,11,drop=FALSE])),digits = 0)
    QUINCE = round(colSums(na.omit(cxp[,12,drop=FALSE])),digits = 0)
    TREINTA = round(colSums(na.omit(cxp[,13,drop=FALSE])),digits = 0)
    CUATCINCO = round(colSums(na.omit(cxp[,14,drop=FALSE])),digits = 0)
    SESENTA = round(colSums(na.omit(cxp[,15,drop=FALSE])),digits = 0)
    MAS = round(colSums(na.omit(cxp[,16,drop=FALSE])),digits = 0)
    N_CRED = round(colSums(na.omit(cxp[,18,drop=FALSE])),digits = 0)
    N_CARG = round(colSums(na.omit(cxp[,19,drop=FALSE])),digits = 0)
    MONTO=c(VENCIDA,SIETE,QUINCE,TREINTA,CUATCINCO,SESENTA,MAS,N_CRED,N_CARG)
    VENCIMIENTO=c("VENCIDA","SIETE","QUINCE","TREINTA","CUARENTA y CINCO","SESENTA","MAS","N_CRED","N_CARG")
    grafbarcxp=data.frame(VENCIMIENTO,MONTO)
  })
  VENCIMIENTO=c("VENCIDA","SIETE","QUINCE","TREINTA","CUARENTA y CINCO","SESENTA","MAS","N_CRED","N_CARG")
  
  output$barcxp<- renderPlot({  ###Grafica vs Presupuesto
    
    ggplot(grafbarcxp(),aes(VENCIMIENTO,MONTO,label=rownames(grafbarcxp())))+geom_bar(stat="identity") +                        
      xlab("") + ylab("Monto")+geom_text(stat="identity",aes(label=prettyNum(MONTO,big.mark=",",scientific=FALSE),vjust=-0.5),size=5,color="black")+ scale_y_continuous(labels = comma)+ scale_x_discrete(limits = VENCIMIENTO)
  })
  
  
  ###############  PARTICIPACION POR Proveedor  ####################
  partcxp<-reactive({ #Grafica de pastel
    partcxp=select(cxp,NOM_PROV,"MONTO"= input$VENCI)
    partcxp= ddply(partcxp[],.(NOM_PROV),summarize,MONTO=sum(na.omit(MONTO)))
    a= colSums(partcxp[,2,drop=FALSE])
    partcxp=ddply(partcxp[],.(partcxp[,1]),summarize,MONTO=sum(MONTO),Part=MONTO/a)
    partcxp=as.data.frame(partcxp)
    colnames(partcxp)=c("nombre","MONTO","Part")
    otro=partcxp[which(partcxp$Part<=input$Particxp),]
    a=colSums(otro[,2,drop=FALSE])
    b=colSums(otro[,3,drop=FALSE])
    otros=list(nombre="Otros", MONTO=a, Part=b)
    partcxp=rbind(partcxp,otros)
    partcxp=partcxp[which(partcxp$Part>input$Particxp),]
    partcxp=as.data.frame(partcxp)
  })
  
  
  output$partcxp<- renderPlotly({  ###Participacion por proveedor
    plot_ly(partcxp(), labels = ~partcxp()[,1], values = ~partcxp()[,2], type = 'pie')%>%layout(legend = list(orientation = 'h',font = list(size = 9)))
  })
  
  
  #################Cuentas por Cobrar############
  
  ###############  Facturas de venta por Vencer ####################
  datacxc <- reactive({
    tcxc=cxc[,1:19]
    tcxc=select(tcxc,-LUGAR,-CVE_AGE,-NOM_AGE)
    names(tcxc)[12] = "CUARENTA Y CINCO"
    tcxc=as.data.frame(tcxc)            })
  
  
  output$dowcxc <- DT::renderDataTable(DT::datatable(datacxc(),
                                                     filter = 'top',
                                                     extensions = 'Buttons', 
                                                     options = list(lengthMenu = list(c(10,20,50,100, -1), list('10', '20', '50', '100', 'All')),pageLength = 10, autoWidth = TRUE,scrollX = TRUE,dom = 'lBfrtip',
                                                                    buttons = c('copy', 'excel')))%>%formatStyle(columns =1:16, fontSize = '80%')%>%formatCurrency(columns=8:16, currency = "$  ", interval = 3, mark = ",", digits = 0)%>%formatCurrency(columns=6:7, currency = "", interval = 3, mark = ",", digits = 0)
  )
  
  
  ###############  Grafica de barras facturas de venta por vencer ####################
  
  grafbarcxc<-reactive({
    VENCIDA = round(colSums(na.omit(cxc[,8,drop=FALSE])),digits = 0)
    SIETE = round(colSums(na.omit(cxc[,9,drop=FALSE])),digits = 0)
    QUINCE = round(colSums(na.omit(cxc[,10,drop=FALSE])),digits = 0)
    TREINTA = round(colSums(na.omit(cxc[,11,drop=FALSE])),digits = 0)
    CUATCINCO = round(colSums(na.omit(cxc[,12,drop=FALSE])),digits = 0)
    SESENTA = round(colSums(na.omit(cxc[,13,drop=FALSE])),digits = 0)
    MAS = round(colSums(na.omit(cxc[,14,drop=FALSE])),digits = 0)
    N_CRED = round(colSums(na.omit(cxc[,18,drop=FALSE])),digits = 0)
    N_CARG = round(colSums(na.omit(cxc[,19,drop=FALSE])),digits = 0)
    MONTO=c(VENCIDA,SIETE,QUINCE,TREINTA,CUATCINCO,SESENTA,MAS,N_CRED,N_CARG)
    VENCIMIENTO=c("VENCIDA","SIETE","QUINCE","TREINTA","CUARENTA y CINCO","SESENTA","MAS","N_CRED","N_CARG")
    grafbarcxc=data.frame(VENCIMIENTO,MONTO)
  })
  VENCIMIENTO=c("VENCIDA","SIETE","QUINCE","TREINTA","CUARENTA y CINCO","SESENTA","MAS","N_CRED","N_CARG")
  
  output$barcxc<- renderPlot({  
    
    ggplot(grafbarcxc(),aes(VENCIMIENTO,MONTO,label=rownames(grafbarcxc())))+geom_bar(stat="identity") +                        
      xlab("") + ylab("Monto")+geom_text(stat="identity",aes(label=prettyNum(MONTO,big.mark=",",scientific=FALSE),vjust=-0.5),size=5,color="black")+ scale_y_continuous(labels = comma)+ scale_x_discrete(limits = VENCIMIENTO)
  })
  
  
  ###############  PARTICIPACION POR Cliente  ####################
  partcxc<-reactive({ #Grafica de pastel
    partcxc=select(cxc,NOM_CTE,"MONTO"= input$VENCIcxc)
    partcxc= ddply(partcxc[],.(NOM_CTE),summarize,MONTO=sum(na.omit(MONTO)))
    a= colSums(partcxc[,2,drop=FALSE])
    partcxc=ddply(partcxc[],.(partcxc[,1]),summarize,MONTO=sum(MONTO),Part=MONTO/a)
    partcxc=as.data.frame(partcxc)
    colnames(partcxc)=c("nombre","MONTO","Part")
    otro=partcxc[which(partcxc$Part<=input$Particxc),]
    a=colSums(otro[,2,drop=FALSE])
    b=colSums(otro[,3,drop=FALSE])
    otros=list(nombre="Otros", MONTO=a, Part=b)
    partcxc=rbind(partcxc,otros)
    partcxc=partcxc[which(partcxc$Part>input$Particxc),]
    partcxc=as.data.frame(partcxc)
  })
  
  
  
  
  output$partcxc<- renderPlotly({  ###Participacion por cliente
    plot_ly(partcxc(), labels = ~partcxc()[,1], values = ~partcxc()[,2], type = 'pie')%>%layout(legend = list(orientation = 'h'))
  }) 
  
  
  
  
  
  
  ###########################################################
  ####################  UTILIDAD  ###########################
  ###########################################################
  
  ################# REsumen Utilidad ############      
  
  ################# GRAFICA MENSUAL UTILIDAD ############
  output$Boxut11 = renderUI(selectInput("ualma","Elige un almacen:",unique(catgto$LUGAR),"Todos"))
  
  utcompg<-reactive({ #Ventas por mes grafica importe Todos
    utcompg=uttotcomp[which(uttotcomp$FECHA>="2017-10-01" & uttotcomp$FECHA<=today()),]
    utcompg=if (input$ualma=="Todos") utcompg else utcompg[which(utcompg$LUGAR==input$ualma),]
    utcompg=select(utcompg,ANIO,MES,UTILIDAD,INGRESO,GASTO)
    utcompg=ddply(utcompg[],.(ANIO,MES),summarize,UTILIDAD=sum(UTILIDAD),INGRESO=sum(INGRESO),GASTO=sum(GASTO))
  })
  
  
  output$utcompg<- renderPlot({  ###Grafica Historico
    
    p1 <- ggplot(utcompg(),aes(x=MES,y=UTILIDAD, group=factor(ANIO), shape=factor(ANIO), colour=factor(ANIO)))+geom_line(size=2)+ scale_y_continuous(labels = comma) + scale_x_discrete(limits = c(1:12)) + xlab("MES") + ylab("UTILIDAD")+geom_text(stat="identity",aes(check_overlap = TRUE,label=prettyNum(round(UTILIDAD,0),big.mark=",",scientific=FALSE)),size=3,color="black")
    p2 <- ggplot(utcompg(),aes(x=MES,y=INGRESO, group=factor(ANIO), shape=factor(ANIO), colour=factor(ANIO)))+geom_line(size=2)+ scale_y_continuous(labels = comma) + scale_x_discrete(limits = c(1:12)) + xlab("MES") + ylab("INGRESO")+geom_text(stat="identity",aes(check_overlap = TRUE,label=prettyNum(round(INGRESO,0),big.mark=",",scientific=FALSE)),size=3,color="black")
    p3 <- ggplot(utcompg(),aes(x=MES,y=GASTO, group=factor(ANIO), shape=factor(ANIO), colour=factor(ANIO)))+geom_line(size=2)+ scale_y_continuous(labels = comma) + scale_x_discrete(limits = c(1:12)) + xlab("MES") + ylab("GASTO")+geom_text(stat="identity",aes(check_overlap = TRUE,label=prettyNum(round(GASTO,0),big.mark=",",scientific=FALSE)),size=3,color="black")
    grid.arrange(p1, p2,p3)
    
  })
  
  ##################GRAFICA INGRSO Y GASTOS JUNTOS ####################3
  
  inggascompg<-reactive({ #Ventas por mes grafica importe Todos
    inggascompg=uttotcomp[which(uttotcomp$ANIO==input$anioinggas),]
    inggascompg=if (input$ualma=="Todos") inggascompg else inggascompg[which(inggascompg$LUGAR==input$ualma),]
    inggascompg1=select(inggascompg,ANIO,MES,INGRESO)
    inggascompg1=cbind(inggascompg1,"I")
    colnames(inggascompg1)<-c("ANIO","MES","IMPORTE","DOC")
    inggascompg2=select(inggascompg,ANIO,MES,GASTO)
    inggascompg2=cbind(inggascompg2,"G")
    colnames(inggascompg2)<-c("ANIO","MES","IMPORTE","DOC")
    inggascompg=rbind(inggascompg1,inggascompg2)
    inggascompg=ddply(inggascompg[],.(MES,DOC),summarize,IMPORTE=sum(IMPORTE))
  })
  
  
  output$inggasg<- renderPlot({  ###Grafica Historico
    ggplot(inggascompg(),aes(x=MES,y=IMPORTE, group=factor(DOC), shape=factor(DOC), colour=factor(DOC)))+geom_line(size=2)+ scale_y_continuous(labels = comma) + scale_x_discrete(limits = c(1:12)) + xlab("MES") + ylab("IMPORTE")+geom_text(stat="identity",aes(check_overlap = TRUE,label=prettyNum(round(IMPORTE,0),big.mark=",",scientific=FALSE)),size=3,color="black")
  })
  
  
  
  
  
  
  ################# INFOBOX INGRESO,GASTO, UTILIDAD ############
  
  butcomp<-reactive({ #Ventas por mes grafica importe Todos
    butcomp=if (input$ualma=="Todos") utcomp() else utcomp()[which(utcomp()$LUGAR==input$ualma),]
  })
  
  utcomp<-eventReactive(input$actualizar,{ #UTilidad Real
    utcomp=uttotcomp[which(uttotcomp$FECHA>=fecha_ini() & uttotcomp$FECHA<=fecha_fin()),] 
  } , ignoreNULL = FALSE)
  
  
  output$ut<-renderInfoBox({
    totalut = colSums(butcomp()[,6,drop=FALSE])
    totalut= prettyNum(totalut,big.mark=",",scientific=FALSE)
    if(totalut>=0) ico_ut="thumbs-up" else
      ico_ut="thumbs-down"
    infoBox("Utilidad",totalut,icon=icon(ico_ut))
  })
  
  output$ing<-renderInfoBox({
    totaling = colSums(butcomp()[,4,drop=FALSE])
    totaling= prettyNum(totaling,big.mark=",",scientific=FALSE)
    infoBox("Ingresos",totaling,icon=icon("plus-sign", lib = "glyphicon"),color = "green")
  })
  
  output$gas<-renderInfoBox({
    totalgas = colSums(butcomp()[,5,drop=FALSE])
    totalgas= prettyNum(totalgas,big.mark=",",scientific=FALSE)
    infoBox("Gastos",totalgas,icon=icon("minus-sign", lib = "glyphicon"),color = "red")
  })
  
  ################# GRAFICA DE BARRAS ############
  butcompaa<-reactive({ 
    butcompaa=if (input$ualma=="Todos") utcompaa() else utcompaa()[which(utcompaa()$LUGAR==input$ualma),]
  })
  
  utcompaa<-eventReactive(input$actualizar,{ #utilidad anioo anterior
    utcompaa=uttotcomp[which(uttotcomp$FECHA>=(fecha_ini()+dyears(-1)) & uttotcomp$FECHA<=(fecha_fin()+ dyears(-1))),] 
  } , ignoreNULL = FALSE)
  
  utcomppres<-eventReactive(input$actualizar,{ #utilidad presupuesto
    uttotpres[which(uttotpres$ANIO>=year(fecha_ini()) & uttotpres$MES>=month(fecha_ini()) & uttotpres$ANIO<=year(fecha_fin()) & uttotpres$MES<=month(fecha_fin())),] 
  } , ignoreNULL = FALSE)
  
  grafbarutl<-reactive({
    #presupuesto = round(colSums(utcomppres()[,4,drop=FALSE]),digits=0)
    #MONTO=c(real,presupuesto,aa)
    #MOVIMIENTO=c("Real","Presupuesto","AA")
    real = round(colSums(butcomp()[,6,drop=FALSE]),digits = 0)
    aa=round(colSums(butcompaa()[,6,drop=FALSE]),digits=0)
    MONTO=c(real,aa)
    MOVIMIENTO=c("Real","Anio Anterior")
    grafbarutl=data.frame(MOVIMIENTO,MONTO)
  })
  
  grafbargas<-reactive({
    #presupuesto = round(colSums(utcomppres()[,4,drop=FALSE]),digits=0)
    #MONTO=c(real,presupuesto,aa)
    #MOVIMIENTO=c("Real","Presupuesto","AA")
    real = round(colSums(butcomp()[,5,drop=FALSE]),digits = 0)
    aa=round(colSums(butcompaa()[,5,drop=FALSE]),digits=0)
    MONTO=c(real,aa)
    MOVIMIENTO=c("Real","Anio Anterior")
    grafbarutl=data.frame(MOVIMIENTO,MONTO)
  })
  
  grafbaring<-reactive({
    #presupuesto = round(colSums(utcomppres()[,4,drop=FALSE]),digits=0)
    #MONTO=c(real,presupuesto,aa)
    #MOVIMIENTO=c("Real","Presupuesto","AA")
    real = round(colSums(butcomp()[,4,drop=FALSE]),digits = 0)
    aa=round(colSums(butcompaa()[,4,drop=FALSE]),digits=0)
    MONTO=c(real,aa)
    MOVIMIENTO=c("Real","Anio Anterior")
    grafbarutl=data.frame(MOVIMIENTO,MONTO)
  })
  
  output$pres_ut<- renderPlot({  ###Grafica vs Presupuesto
    
    p1 <- ggplot(grafbarutl(),aes(MOVIMIENTO,MONTO,label=rownames(grafbarutl())))+geom_bar(stat="identity") +                        
      xlab("") + ylab("UTILIDAD")+geom_text(stat="identity",aes(label=prettyNum(MONTO,big.mark=",",scientific=FALSE),vjust=MONTO>0),size=5,color="White")+ scale_y_continuous(labels = comma)
    p2 <- ggplot(grafbaring(),aes(MOVIMIENTO,MONTO,label=rownames(grafbaring())))+geom_bar(stat="identity") +                        
      xlab("") + ylab("INGRESO")+geom_text(stat="identity",aes(label=prettyNum(MONTO,big.mark=",",scientific=FALSE),vjust=MONTO>0),size=5,color="White")+ scale_y_continuous(labels = comma)
    p3 <- ggplot(grafbargas(),aes(MOVIMIENTO,MONTO,label=rownames(grafbargas())))+geom_bar(stat="identity") +                        
      xlab("") + ylab("GASTO")+geom_text(stat="identity",aes(label=prettyNum(MONTO,big.mark=",",scientific=FALSE),vjust=MONTO>0),size=5,color="White")+ scale_y_continuous(labels = comma)
    grid.arrange(p1, p2,p3)
    
  })
  
  ################# Gastos ############ 
  
  csegs<-reactive({ #catalogo de productos subgrupo
    csegs= if (input$galma=="Todos") catgto else catgto[which(catgto$LUGAR==input$galma),]
    csegs= if (input$tipogs=="Todos") csegs else csegs[which(csegs$DES_TIAL==input$tipogs),]
  })
  
  gpogs<-reactive({ #catalogo de productos subgrupo
    gpogs= if (input$galma=="Todos") catgto else catgto[which(catgto$LUGAR==input$galma),]
    gpogs= if (input$tipogs=="Todos") gpogs else gpogs[which(gpogs$DES_TIAL==input$tipogs),]
    gpogs= if (input$clasegs=="Todos") gpogs else gpogs[which(gpogs$CSE_PROD==input$clasegs),]
  })
  
  subgpogs<-reactive({ #catalogo de productos subgrupo
    subgpogs= if (input$galma=="Todos") catgto else catgto[which(catgto$LUGAR==input$galma),]
    subgpogs= if (input$tipogs=="Todos") subgpogs else subgpogs[which(subgpogs$DES_TIAL==input$tipogs),]
    subgpogs= if (input$clasegs=="Todos") subgpogs else subgpogs[which(subgpogs$CSE_PROD==input$clasegs),]
    subgpogs= if (input$grupogs=="Todos") subgpogs else subgpogs[which(subgpogs$SUB_CSE==input$grupogs),]
  })
  
  output$Boxgs11 = renderUI(selectInput("galma","Elige un almacen:",unique(catgto$LUGAR),"Todos"))
  output$Boxgs12 = renderUI(selectInput("tipogs","Elige el Tipo:",if (input$galma=="Todos") unique(catgto$DES_TIAL) else catgto$DES_TIAL[which(catgto$LUGAR == input$galma)],"Todos"))
  output$Boxgs13 = renderUI(selectInput("clasegs","Elige la Clase:",unique(csegs()$CSE_PROD),"Todos"))
  output$Boxgs14 = renderUI(selectInput("grupogs","Elige el Grupo:",unique(gpogs()$SUB_CSE),"Todos"))
  output$Boxgs15 = renderUI(selectInput("subgrupogs","Elige el SubGrupo:",unique(subgpogs()$SUB_SUBCSE),"Todos"))
  
  
  ###############  Participacion por GAsto  ####################
  
  gpgp<-reactive({ #Grafica de pastel
    gpgp=gascomp[which(gascomp$FECHA>=fecha_ini() & gascomp$FECHA<=fecha_fin()),]
    gpgp= if (input$galma=="Todos") gpgp else gpgp[which(gpgp$LUGAR==input$galma),]
    gpgp= if (input$tipogs=="Todos") gpgp else gpgp[which(gpgp$DES_TIAL==input$tipogs),]
    gpgp= if (input$clasegs=="Todos") gpgp else gpgp[which(gpgp$CSE_PROD==input$clasegs),]
    gpgp= if (input$grupogs=="Todos") gpgp else gpgp[which(gpgp$SUB_CSE==input$grupogs),]
    gpgp= if (input$subgrupogs=="Todos") gpgp else gpgp[which(gpgp$SUB_SUBCSE==input$subgrupogs),]
    gpgp=select(gpgp,LUGAR,DES_TIAL,CSE_PROD,SUB_CSE,SUB_SUBCSE,DESC_PROD,TOTAL_FAC)
    
    
    gpgp= if (input$galma=="Todos") ddply(gpgp[],.(LUGAR),summarize,GASTOS=sum(TOTAL_FAC)) else gpgp
    gpgp= if (input$galma!="Todos" & input$tipogs=="Todos") ddply(gpgp[],.(DES_TIAL),summarize,GASTOS=sum(TOTAL_FAC)) else gpgp
    gpgp= if (input$galma!="Todos" & input$tipogs!="Todos" & input$clasegs=="Todos") ddply(gpgp[],.(CSE_PROD),summarize,GASTOS=sum(TOTAL_FAC)) else gpgp
    gpgp= if (input$galma!="Todos" & input$tipogs!="Todos" & input$clasegs!="Todos" & input$grupogs=="Todos") ddply(gpgp[],.(SUB_CSE),summarize,GASTOS=sum(TOTAL_FAC)) else gpgp
    gpgp= if (input$galma!="Todos" & input$tipogs!="Todos" & input$clasegs!="Todos" & input$grupogs!="Todos" & input$subgrupogs=="Todos") ddply(gpgp[],.(SUB_SUBCSE),summarize,GASTOS=sum(TOTAL_FAC)) else gpgp
    gpgp= if (input$galma!="Todos" & input$tipogs!="Todos" & input$clasegs!="Todos" & input$grupogs!="Todos" & input$subgrupogs!="Todos") ddply(gpgp[],.(DESC_PROD),summarize,GASTOS=sum(TOTAL_FAC)) else gpgp
    
    a= colSums(gpgp[,2,drop=FALSE])
    gpgp=ddply(gpgp[],.(gpgp[,1]),summarize,GASTOS=sum(GASTOS),Part=GASTOS/a)
    gpgp=as.data.frame(gpgp)
    colnames(gpgp)=c("nombre","MTOGAS","Part")
    otro=gpgp[which(gpgp$Part<=input$Participansp),]
    a=colSums(otro[,2,drop=FALSE])
    b=colSums(otro[,3,drop=FALSE])
    otros=list(nombre="Otros", MTOGAS=a, Part=b)
    gpgp=rbind(gpgp,otros)
    gpgp=gpgp[which(gpgp$Part>input$Participagsp),]
    gpgp=as.data.frame(gpgp)
  })
  output$gpgp<- renderPlotly({  ###Participacion en GASTOSs
    plot_ly(gpgp(), labels = ~gpgp()[,1], values = ~gpgp()[,2], type = 'pie')%>%layout(legend = list(orientation = 'r',font = list(size = 11)))
  })
  
  gpgp2<-reactive({ #Grafica de pastel
    gpgp2=gascomp[which(gascomp$FECHA>=fecha_ini() & gascomp$FECHA<=fecha_fin()),]
    gpgp2= ddply(gpgp2[],.(ALMACEN),summarize,GASTOS=sum(TOTAL_FAC))
    
    a= colSums(gpgp2[,2,drop=FALSE])
    gpgp2=ddply(gpgp2[],.(gpgp2[,1]),summarize,GASTOS=sum(GASTOS),Part=GASTOS/a)
    gpgp2=as.data.frame(gpgp2)
    colnames(gpgp2)=c("nombre","MTOGAS","Part")
    otro=gpgp2[which(gpgp2$Part<=input$Participansp),]
    a=colSums(otro[,2,drop=FALSE])
    b=colSums(otro[,3,drop=FALSE])
    otros=list(nombre="Otros", MTOGAS=a, Part=b)
    gpgp2=rbind(gpgp2,otros)
    gpgp2=gpgp2[which(gpgp2$Part>input$Participagsp),]
    gpgp2=as.data.frame(gpgp2)
  })
  
  output$gpgp2<- renderPlotly({  ###Participacion en GASTOSs
    plot_ly(gpgp2(), labels = ~gpgp2()[,1], values = ~gpgp2()[,2], type = 'pie')%>%layout(legend = list(orientation = 'r',font = list(size = 11)))
  })
  
  ################# Tabla de Gastos ############
  
  gas_cse<-eventReactive(input$actualizar,{ #UTilidad Real
    gas_cse=gascomp[which(gascomp$FECHA>=fecha_ini() & gascomp$FECHA<=fecha_fin()),]
    gas_cse=gas_cse[,1:12]
    gas_cse=ddply(gas_cse[],.(LUGAR,DES_TIAL,CSE_PROD,SUB_CSE,SUB_SUBCSE,DESC_PROD),summarize,TOTAL_FAC=sum(TOTAL_FAC))
    
    gas_cseytd=gascomp[which(gascomp$FECHA>=fecha_ini2() & gascomp$FECHA<=fecha_fin2()),]
    gas_cseytd=gas_cseytd[,1:12]
    gas_cseytd=ddply(gas_cseytd[],.(LUGAR,DES_TIAL,CSE_PROD,SUB_CSE,SUB_SUBCSE,DESC_PROD),summarize,TOTAL_FAC_AA=sum(TOTAL_FAC))
    
    gas_csetan=gascomp[which(gascomp$FECHA>=fecha_ini3() & gascomp$FECHA<=fecha_fin3()),]
    gas_csetan=gas_csetan[,1:12]
    gas_csetan=ddply(gas_csetan[],.(LUGAR,DES_TIAL,CSE_PROD,SUB_CSE,SUB_SUBCSE,DESC_PROD),summarize,TOTAL_FAC_TotAA=sum(TOTAL_FAC))
    
    if (fecha_ini()>as.Date("2019-01-31")) gas_cse=merge(x=gas_cse,gas_cseytd, all.x=TRUE) else gas_cse
    if (fecha_ini()>as.Date("2019-01-31")) gas_cse=merge(x=gas_cse,gas_csetan, all.x=TRUE) else gas_cse
  } , ignoreNULL = FALSE)
  
  
  output$tablegascomp <- DT::renderDataTable(DT::datatable(gas_cse(),filter = 'top',extensions = 'Buttons', 
                                                           options = list(lengthMenu = list(c(10,20,50,100, -1), list('10', '20', '50', '100', 'All')),pageLength = 10, autoWidth = TRUE,scrollX = TRUE,dom = 'lBfrtip',
                                                                          buttons = c('copy', 'excel')))%>%formatStyle(columns =1:14, fontSize = '80%')%>%formatCurrency(columns=7:9, currency = "$  ", interval = 3, mark = ",", digits = 0))
  
  ################# Ingresos ############      
  
  output$Boxing11 = renderUI(selectInput("ialma","Elige un almacen:",unique(catgto$LUGAR),"Todos"))
  
  ################# Grafica de Pastel Ventas por REM FAC o NVTA #####################
  
  ingvtagp<-reactive({ #Grafica de pastel
    ingvtagp=ingcomp[which(ingcomp$FECHA>=fecha_ini() & ingcomp$FECHA<=fecha_fin()),]
    ingvtagp= if (input$ialma=="Todos") ingvtagp else ingvtagp[which(ingvtagp$LUGAR==input$ialma),]
    ingvtagp=select(ingvtagp,DOCUMENTO,TOTAL_FAC)
    
    a= colSums(ingvtagp[,2,drop=FALSE])
    ingvtagp=ddply(ingvtagp[],.(ingvtagp[,1]),summarize,TOTAL_FAC=sum(TOTAL_FAC),Part=TOTAL_FAC/a)
    ingvtagp=as.data.frame(ingvtagp)
    colnames(ingvtagp)=c("nombre","MTOING","Part")
    ingvtagp=as.data.frame(ingvtagp)
  })
  output$ingvtagp<- renderPlotly({  ###Participacion en GASTOSs
    plot_ly(ingvtagp(), labels = ~ingvtagp()[,1], values = ~ingvtagp()[,2], type = 'pie')%>%layout(legend = list(orientation = 'r',font = list(size = 11)))
  })
  
  ################# Grafica de Pastel Facturado por Rem Dir #####################
  
  ingfacagp<-reactive({ #Grafica de pastel
    ingfacagp=ingfac[which(ingfac$FECHA>=fecha_ini() & ingfac$FECHA<=fecha_fin()),]
    ingfacagp= if (input$ialma=="Todos") ingfacagp else ingfacagp[which(ingfacagp$LUGAR==input$ialma),]
    ingfacagp=select(ingfacagp,DOCUMENTO,TOTAL_FAC)
    
    a= colSums(ingfacagp[,2,drop=FALSE])
    ingfacagp=ddply(ingfacagp[],.(ingfacagp[,1]),summarize,TOTAL_FAC=sum(TOTAL_FAC),Part=TOTAL_FAC/a)
    ingfacagp=as.data.frame(ingfacagp)
    colnames(ingfacagp)=c("nombre","MTOING","Part")
    ingfacagp=as.data.frame(ingfacagp)
  })
  output$ingfacagp<- renderPlotly({  ###Participacion en GASTOSs
    plot_ly(ingfacagp(), labels = ~ingfacagp()[,1], values = ~ingfacagp()[,2], type = 'pie')%>%layout(legend = list(orientation = 'r',font = list(size = 11)))
  })
  
  ################# Grafica de Pastel Facturado por Mes Origen #####################
  
  ingfacfantgp<-reactive({ #Grafica de pastel
    ingfacfantgp=ingfac[which(ingfac$FECHA>=fecha_ini() & ingfac$FECHA<=fecha_fin()),]
    ingfacfantgp= if (input$ialma=="Todos") ingfacfantgp else ingfacfantgp[which(ingfacfantgp$LUGAR==input$ialma),]
    ingfacfantgp=select(ingfacfantgp,FECANTTXT,TOTAL_FAC)
    
    a= colSums(ingfacfantgp[,2,drop=FALSE])
    ingfacfantgp=ddply(ingfacfantgp[],.(ingfacfantgp[,1]),summarize,TOTAL_FAC=sum(TOTAL_FAC),Part=TOTAL_FAC/a)
    ingfacfantgp=as.data.frame(ingfacfantgp)
    colnames(ingfacfantgp)=c("nombre","MTOING","Part")
    ingfacfantgp=as.data.frame(ingfacfantgp)
  })
  output$ingfacfantgp<- renderPlotly({  ###Participacion en GASTOSs
    plot_ly(ingfacfantgp(), labels = ~ingfacfantgp()[,1], values = ~ingfacfantgp()[,2], type = 'pie')%>%layout(legend = list(orientation = 'r',font = list(size = 11)))
  })
  
  ################# Grafica de Pastel Facturado por Fiscal NoFiscal #####################
  
  ingfaccvfacgp<-reactive({ #Grafica de pastel
    ingfaccvfacgp=ingfac[which(ingfac$FECHA>=fecha_ini() & ingfac$FECHA<=fecha_fin()),]
    ingfaccvfacgp= if (input$ialma=="Todos") ingfaccvfacgp else ingfaccvfacgp[which(ingfaccvfacgp$LUGAR==input$ialma),]
    ingfaccvfacgp=select(ingfaccvfacgp,CVE_FACTU,TOTAL_FAC)
    
    a= colSums(ingfaccvfacgp[,2,drop=FALSE])
    ingfaccvfacgp=ddply(ingfaccvfacgp[],.(ingfaccvfacgp[,1]),summarize,TOTAL_FAC=sum(TOTAL_FAC),Part=TOTAL_FAC/a)
    ingfaccvfacgp=as.data.frame(ingfaccvfacgp)
    colnames(ingfaccvfacgp)=c("nombre","MTOING","Part")
    ingfaccvfacgp=as.data.frame(ingfaccvfacgp)
  })
  output$ingfaccvfacgp<- renderPlotly({  ###Participacion en GASTOSs
    plot_ly(ingfaccvfacgp(), labels = ~ingfaccvfacgp()[,1], values = ~ingfaccvfacgp()[,2], type = 'pie')%>%
      layout(legend = list(orientation = 'r',font = list(size = 11)))
  })
  
  ################# Grafica de Pastel Facturado por Estatus Factura #####################
  
  ingfacstatfgp<-reactive({ #Grafica de pastel
    ingfacstatfgp=ingfac[which(ingfac$FECHA>=fecha_ini() & ingfac$FECHA<=fecha_fin()),]
    ingfacstatfgp= if (input$ialma=="Todos") ingfacstatfgp else ingfacstatfgp[which(ingfacstatfgp$LUGAR==input$ialma),]
    ingfacstatfgp=select(ingfacstatfgp,STATUS_FAC,TOTAL_FAC)
    
    a= colSums(ingfacstatfgp[,2,drop=FALSE])
    ingfacstatfgp=ddply(ingfacstatfgp[],.(ingfacstatfgp[,1]),summarize,TOTAL_FAC=sum(TOTAL_FAC),Part=TOTAL_FAC/a)
    ingfacstatfgp=as.data.frame(ingfacstatfgp)
    colnames(ingfacstatfgp)=c("nombre","MTOING","Part")
    ingfacstatfgp=as.data.frame(ingfacstatfgp)
  })
  
  output$ingfacstatfgp<- renderPlotly({  ###Estatus Factura
    plot_ly(ingfacstatfgp(), labels = ~ingfacstatfgp()[,1], values = ~ingfacstatfgp()[,2], type = 'pie')%>%
      layout(legend = list(orientation = 'r',font = list(size = 11)))
  })
  
  
  output$suma<-renderText({
    a= colSums(ingfacstatfgp()[,2,drop=FALSE]) 
    
  })
  
  ################### Info Boxes ######################################
  
  output$venting<-renderInfoBox({
    venting = colSums(ingvtagp()[,2,drop=FALSE])
    venting= prettyNum(venting,big.mark=",",scientific=FALSE)
    infoBox("Ventas",venting,icon = icon("chart-line"))
  })
  
  output$facing<-renderInfoBox({
    facing = colSums(ingfacstatfgp()[,2,drop=FALSE])
    facing= prettyNum(facing,big.mark=",",scientific=FALSE)
    infoBox("FACTURADO",facing,icon = icon("chart-line"),color="green")
  })
  
  ###########################################################
  ####################  PRODUCTO  ###########################
  ###########################################################
  
  ################# REsumen Producto ############      
  
  output$Box11 = renderUI(selectInput("clase","Elige la Clase:",unique(catprod$CSE_PROD),"Todos"))
  
  output$Box22 = renderUI(selectInput("grupo","Elige el Grupo:",if (input$clase=="Todos") unique(catprod$SUB_CSE) else catprod$SUB_CSE[which(catprod$CSE_PROD == input$clase)],"Todos"))
  
  output$Box33 = renderUI(selectInput("subgrupo","Elige el SubGrupo:",unique(prodc1()$SUB_SUBCSE),"Todos"))
  
  output$Box44 = renderUI(selectInput("descprod","Elige el Producto:",unique(prodc2()$DESC_PROD),"Todos"))
  
  
  ################# CATALOGO DE Producto ############
  
  prodc1<-reactive({ #catalogo de productos subgrupo
    prodc1= if (input$clase=="Todos") catprod else catprod[which(catprod$CSE_PROD==input$clase),]
    prodc1= if (input$grupo=="Todos") prodc1 else prodc1[which(prodc1$SUB_CSE==input$grupo),]
  })
  
  prodc2<-reactive({ #catalogo de productos desc_prod
    prodc2= if (input$clase=="Todos") catprod else catprod[which(catprod$CSE_PROD==input$clase),]
    prodc2= if (input$grupo=="Todos") prodc2 else prodc2[which(prodc2$SUB_CSE==input$grupo),]
    prodc2= if (input$subgrupo=="Todos") prodc2 else prodc2[which(prodc2$SUB_SUBCSE==input$subgrupo),]
  })
  
  
  ################# GRAFICA MENSUAL Producto ############
  
  vppmgi<-reactive({ #Ventas por producto por mes grafica importe Todos
    vppmgi=vtasprodm[which(vtasprodm$FECHA>="2018-01-01" & vtasprodm$FECHA<=today()),]
    vppmgi= if (input$clase=="Todos") vppmgi else vppmgi[which(vppmgi$CSE_PROD==input$clase),]
    vgpmg= if (input$grupo=="Todos") vppmgi else vppmgi[which(vppmgi$SUB_CSE==input$grupo),]
    vsgcmg= if (input$subgrupo=="Todos") vgpmg else vgpmg[which(vgpmg$SUB_SUBCSE==input$subgrupo),] 
    vdgcmg= if (input$descprod=="Todos") vsgcmg else vsgcmg[which(vsgcmg$DESC_PROD==input$descprod),]
    vppmgi=select(vdgcmg,ANIO,MES,"IMPORTE"= input$variableprod)
    vppmgi=as.data.frame(vppmgi)
    vppmgi=ddply(vppmgi[],.(ANIO,MES),summarize,IMPORTE=sum(IMPORTE))
  })
  
  output$vppmgc<- renderPlot({  ###historico Cantidad
    ggplot(vppmgc(),aes(x=MES,y=CANTIDAD, group=factor(ANIO),shape=factor(ANIO), colour=factor(ANIO)))+geom_line(size=2) + scale_x_discrete(limits = c(1:12)) + xlab("MES") + ylab("CANTIDAD")
  })
  
  output$vppmgi<- renderPlot({  ###Historico Importe
    ggplot(vppmgi(),aes(x=MES,y=IMPORTE, group=factor(ANIO), shape=factor(ANIO), colour=factor(ANIO)))+geom_line(size=2)+ scale_y_continuous(labels = comma) + scale_x_discrete(limits = c(1:12)) + xlab("MES") + ylab("IMPORTE")+geom_text(stat="identity",aes(check_overlap = TRUE,label=prettyNum(round(IMPORTE,0),big.mark=",",scientific=FALSE)),size=3,color="black")
    
  })
  ################# GRAFICA de PASTEL Producto ############
  
  vspgp<-reactive({ #Grafica de pastel
    vspgp=vtasprodm[which(vtasprodm$FECHA>=fecha_ini() & vtasprodm$FECHA<=fecha_fin()),]
    vspgp= if (input$clase=="Todos") vspgp else vspgp[which(vspgp$CSE_PROD==input$clase),]
    vtpgp= if (input$grupo=="Todos") vspgp else vspgp[which(vspgp$SUB_CSE==input$grupo),]
    vpgp= if (input$subgrupo=="Todos") vtpgp else vtpgp[which(vtpgp$SUB_SUBCSE==input$subgrupo),]
    #vpgp= if (input$descprod=="Todos") vpgp else vpgp[which(vpgp$DESC_PROD==input$descprod),]
    vspgp=select(vpgp,CSE_PROD,SUB_CSE,SUB_SUBCSE,DESC_PROD,"IMPORTE"= input$variableprod)
    
    
    vspgp= if (input$clase=="Todos") ddply(vspgp[],.(CSE_PROD),summarize,IMPORTE=sum(IMPORTE)) else vspgp
    vspgp= if (input$clase!="Todos" & input$grupo=="Todos") ddply(vspgp[],.(SUB_CSE),summarize,IMPORTE=sum(IMPORTE)) else vspgp
    vspgp= if (input$clase!="Todos" & input$grupo!="Todos" & input$subgrupo=="Todos") ddply(vspgp[],.(SUB_SUBCSE),summarize,IMPORTE=sum(IMPORTE)) else vspgp
    vspgp= if (input$clase!="Todos" & input$grupo!="Todos" & input$subgrupo!="Todos") ddply(vspgp[],.(DESC_PROD),summarize,IMPORTE=sum(IMPORTE)) else vspgp
    a= colSums(vspgp[,2,drop=FALSE])
    vspgp=ddply(vspgp[],.(vspgp[,1]),summarize,IMPORTE=sum(IMPORTE),Part=IMPORTE/a)
    vspgp=as.data.frame(vspgp)
    colnames(vspgp)=c("nombre","MTOVTA","Part")
    otro=vspgp[which(vspgp$Part<=input$Participap),]
    a=colSums(otro[,2,drop=FALSE])
    b=colSums(otro[,3,drop=FALSE])
    otros=list(nombre="Otros", MTOVTA=a, Part=b)
    vspgp=rbind(vspgp,otros)
    vspgp=vspgp[which(vspgp$Part>input$Participap),]
    vspgp=as.data.frame(vspgp)
  })
  
  output$vpgp<- renderPlotly({  ###Participacion en ventas
    plot_ly(vspgp(), labels = ~vspgp()[,1], values = ~vspgp()[,2], type = 'pie')%>%layout(legend = list(orientation = 'h',font = list(size = 11)))
  })
  
  
  
  ################# TABLA COMPARA ANIOS Producto ############
  
  vsptb<-reactive({ #Grafica de pastel
    vsptb=vtasprodm[which(vtasprodm$MES>=month(fecha_ini()) & vtasprodm$MES<=month(fecha_fin())),]
    vsptb= if (input$clase=="Todos") vsptb else vsptb[which(vsptb$CSE_PROD==input$clase),]
    vsptb= if (input$grupo=="Todos") vsptb else vsptb[which(vsptb$SUB_CSE==input$grupo),]
    vsptb= if (input$subgrupo=="Todos") vsptb else vsptb[which(vsptb$SUB_SUBCSE==input$subgrupo),]
    #vpgp= if (input$descprod=="Todos") vpgp else vpgp[which(vpgp$DESC_PROD==input$descprod),]
    vsptb=select(vsptb,ANIO,CSE_PROD,SUB_CSE,SUB_SUBCSE,DESC_PROD,"IMPORTE"= input$variableprod)
    
    vsptb= if (input$clase=="Todos") ddply(vsptb[],.(ANIO,CSE_PROD),summarize,IMPORTE=sum(IMPORTE)) else vsptb
    vsptb= if (input$clase!="Todos" & input$grupo=="Todos") ddply(vsptb[],.(ANIO,SUB_CSE),summarize,IMPORTE=sum(IMPORTE)) else vsptb
    vsptb= if (input$clase!="Todos" & input$grupo!="Todos" & input$subgrupo=="Todos") ddply(vsptb[],.(ANIO,SUB_SUBCSE),summarize,IMPORTE=sum(IMPORTE)) else vsptb
    vsptb= if (input$clase!="Todos" & input$grupo!="Todos" & input$subgrupo!="Todos") ddply(vsptb[],.(ANIO,DESC_PROD),summarize,IMPORTE=sum(IMPORTE)) else vsptb
    nombcol=colnames(vsptb[2])
    a=vsptb[which(vsptb$ANIO=="2017"),]
    a=select(a,-ANIO)
    colnames(a)=c(nombcol,"2017")
    b=vsptb[which(vsptb$ANIO=="2018"),]
    b=select(b,-ANIO)
    colnames(b)=c(nombcol,"2018")
    c=vsptb[which(vsptb$ANIO=="2019"),]
    c=select(c,-ANIO)
    colnames(c)=c(nombcol,"2019")            
    
    vsptb=merge(x=c,b,all.x=TRUE)
    vsptb=merge(x=vsptb,a,all.x=TRUE)
    vsptb=as.data.frame(vsptb)
    vsptb = vsptb [ , c(1,4,3,2)]
  })
  
  sketch = reactive({
    htmltools::withTags(table(
      class = "display",
      style = "bootstrap",
      tableFooter(c("","Totales",0,0,0)),
      tableHeader(c("",colnames(vsptb())))))
  })
  
  output$vsptb <- DT::renderDataTable(DT::datatable(vsptb(),container = sketch(),filter = 'top', options = list(pageLength = 10, autoWidth = TRUE,
                                                                                                                footerCallback = JS(
                                                                                                                  "function( tfoot, data, start, end, display ) {",
                                                                                                                  "var api = this.api(), data;",
                                                                                                                  "var numFormat = $.fn.dataTable.render.number( ',', '.', 0, '' ).display;",
                                                                                                                  "total1 = api.column( 2, { page: 'current'} ).data().reduce( function ( a, b ) {return a + b;} )",
                                                                                                                  "total2 = api.column( 3, { page: 'current'} ).data().reduce( function ( a, b ) {return a + b;} )",
                                                                                                                  "total3 = api.column( 4, { page: 'current'} ).data().reduce( function ( a, b ) {return a + b;} )",
                                                                                                                  "$( api.column( 2 ).footer() ).html(numFormat(total1));
                                                                    $( api.column( 3 ).footer() ).html(numFormat(total2));
                                                                    $( api.column( 4 ).footer() ).html(numFormat(total3));",
                                                                                                                  "}")))%>%formatCurrency(columns=2:4, currency = "", interval = 3, mark = ",", digits = 0))  
  
  
  
  ################# INDICADORES INFOBOX ############
  
  vppmgc<-reactive({ 
    vppmgc=vtasprodm[which(vtasprodm$FECHA>=fecha_ini() & vtasprodm$FECHA<=fecha_fin()),]
    vppmgc= if (input$clase=="Todos") vppmgc else vppmgc[which(vppmgc$CSE_PROD==input$clase),]
    vgpmgc= if (input$grupo=="Todos") vppmgc else vppmgc[which(vppmgc$SUB_CSE==input$grupo),]
    vsgcmgc= if (input$subgrupo=="Todos") vgpmgc else vgpmgc[which(vgpmgc$SUB_SUBCSE==input$subgrupo),] 
    vdgcmgc= if (input$descprod=="Todos") vsgcmgc else vsgcmgc[which(vsgcmgc$DESC_PROD==input$descprod),]
    vppmgc=select(vdgcmgc,ANIO,MES,UTILIDAD,COST_PROM,IMP_TOT,CANT_VEN)
  })
  
  
  output$utprod<-renderInfoBox({
    totalutprod = colSums(vppmgc()[,3,drop=FALSE])
    totalutprod= prettyNum(totalutprod,big.mark=",",scientific=FALSE)
    if(totalutprod>=0) ico_ut="thumbs-up" else
      ico_ut="thumbs-down"
    infoBox("Utilidad",totalutprod,icon=icon(ico_ut))
  })
  
  output$vtaprod<-renderInfoBox({
    totalvtaprod = colSums(vppmgc()[,5,drop=FALSE])
    totalvtaprod= prettyNum(totalvtaprod,big.mark=",",scientific=FALSE)
    infoBox("Ventas",totalvtaprod,icon=icon("plus-sign", lib = "glyphicon"),color = "green")
  })
  
  output$costprod<-renderInfoBox({
    totalcostprod = colSums(vppmgc()[,4,drop=FALSE])
    totalcostprod= prettyNum(totalcostprod,big.mark=",",scientific=FALSE)
    infoBox("Costo",totalcostprod,icon=icon("minus-sign", lib = "glyphicon"),color = "red")
  })
  
  output$cantprod<-renderInfoBox({
    totalcantprod = colSums(vppmgc()[,6,drop=FALSE])
    totalcantprod= prettyNum(totalcantprod,big.mark=",",scientific=FALSE)
    infoBox("Unidades Vendidas",totalcantprod,icon=icon("dashboard", lib = "glyphicon"),color = "red")
  })
  
  
  ################# TABLA PRODUCTOS ############
  
  vppm<-eventReactive(input$actualizar,{ #UTilidad Real
    vppm=vtasprodm[which(vtasprodm$FECHA>=fecha_ini() & vtasprodm$FECHA<=fecha_fin()),]
    vppm=select(vppm,-ANIO,-MES,-FECHA)
    vppm=ddply(vppm[],.(CSE_PROD,SUB_CSE,SUB_SUBCSE,DESC_PROD,UNI_MED,ULT_MES),summarize,CANTIDAD=sum(CANT_VEN),IMPORTE=sum(IMP_TOT),COSTO=sum(COST_PROM),UTILIDAD=sum(UTILIDAD),UT_PROM=sum(UT_PROM))
    vppm <- vppm %>% mutate(UT_PROM = vppm$UTILIDAD/vppm$CANTIDAD)
  } , ignoreNULL = FALSE)
  
  output$tabvtasprod <- DT::renderDataTable(DT::datatable(vppm(), filter = 'top', options = list(pageLength = 10, autoWidth = TRUE))%>%formatCurrency(columns=7:11, currency = "", interval = 3, mark = ",", digits = 0)%>%formatStyle(columns = c(4), fontSize = '70%'))
  
  ###########################################################
  #################  Productos por Clientes #################
  
  output$Boxcp11 = renderUI(selectInput("clasecp","Elige la Clase:",unique(catprod$CSE_PROD),"Todos"))
  
  output$Boxcp22 = renderUI(selectInput("grupocp","Elige el Grupo:",if (input$clasecp=="Todos") unique(catprod$SUB_CSE) else catprod$SUB_CSE[which(catprod$CSE_PROD == input$clasecp)],"Todos"))
  
  output$Boxcp33 = renderUI(selectInput("subgrupocp","Elige el SubGrupo:",unique(prodcp1()$SUB_SUBCSE),"Todos"))
  
  output$Boxcp44 = renderUI(selectInput("descprodcp","Elige el Producto:",unique(prodcp2()$DESC_PROD),"Todos"))
  
  
  ################# CATALOGO DE Producto ############
  
  prodcp1<-reactive({ #catalogo de productos subgrupo
    prodcp1= if (input$clasecp=="Todos") catprod else catprod[which(catprod$CSE_PROD==input$clasecp),]
    prodcp1= if (input$grupocp=="Todos") prodcp1 else prodcp1[which(prodcp1$SUB_CSE==input$grupocp),]
  })
  
  prodcp2<-reactive({ #catalogo de productos desc_prod
    prodcp2= if (input$clasecp=="Todos") catprod else catprod[which(catprod$CSE_PROD==input$clasecp),]
    prodcp2= if (input$grupocp=="Todos") prodcp2 else prodcp2[which(prodcp2$SUB_CSE==input$grupocp),]
    prodcp2= if (input$subgrupocp=="Todos") prodcp2 else prodcp2[which(prodcp2$SUB_SUBCSE==input$subgrupocp),]
  })
  
  ################# GRAFICA de PASTEL Prod por cliente ############
  
  vcpgp<-reactive({ #Grafica de pastel
    vcpgp=cteprod[which(cteprod$FECHA>=fecha_ini() & cteprod$FECHA<=fecha_fin()),]
    vcpgp= if (input$clasecp=="Todos") vcpgp else vcpgp[which(vcpgp$CSE_PROD==input$clasecp),]
    vcpgp= if (input$grupocp=="Todos") vcpgp else vcpgp[which(vcpgp$SUB_CSE==input$grupocp),]
    vcpgp= if (input$subgrupocp=="Todos") vcpgp else vcpgp[which(vcpgp$SUB_SUBCSE==input$subgrupocp),]
    vcpgp= if (input$descprodcp=="Todos") vcpgp else vcpgp[which(vcpgp$DESC_PROD==input$descprodcp),]
    vcpgp=select(vcpgp,NOM_CTE,CSE_PROD,SUB_CSE,SUB_SUBCSE,DESC_PROD,"IMPORTE"= input$variableprodcte)
    
    
    vcpgp= ddply(vcpgp[],.(NOM_CTE),summarize,IMPORTE=sum(IMPORTE))
    a= colSums(vcpgp[,2,drop=FALSE])
    vcpgp=ddply(vcpgp[],.(vcpgp[,1]),summarize,IMPORTE=sum(IMPORTE),Part=IMPORTE/a)
    vcpgp=as.data.frame(vcpgp)
    colnames(vcpgp)=c("nombre","MTOVTA","Part")
    otro=vcpgp[which(vcpgp$Part<=input$Participapc),]
    a=colSums(otro[,2,drop=FALSE])
    b=colSums(otro[,3,drop=FALSE])
    otros=list(nombre="Otros", MTOVTA=a, Part=b)
    vcpgp=rbind(vcpgp,otros)
    vcpgp=vcpgp[which(vcpgp$Part>input$Participapc),]
    vcpgp=as.data.frame(vcpgp)
  })
  
  output$vcpgp<- renderPlotly({  ###Participacion en ventas
    plot_ly(vcpgp(), labels = ~vcpgp()[,1], values = ~vcpgp()[,2], type = 'pie')%>%layout(legend = list(orientation = 'r',font = list(size = 7)))
  })
  
  
  ################# histograma por producto y cliente ############
  
  vcphgp<-reactive({ #Grafica de pastel
    vcphgp=cteprod[which(cteprod$FECHA>=fecha_ini() & cteprod$FECHA<=fecha_fin()),]
    vcphgp= if (input$clasecp=="Todos") vcphgp else vcphgp[which(vcphgp$CSE_PROD==input$clasecp),]
    vcphgp= if (input$grupocp=="Todos") vcphgp else vcphgp[which(vcphgp$SUB_CSE==input$grupocp),]
    vcphgp= if (input$subgrupocp=="Todos") vcphgp else vcphgp[which(vcphgp$SUB_SUBCSE==input$subgrupocp),]
    vcphgp= if (input$descprodcp=="Todos") vcphgp else vcphgp[which(vcphgp$DESC_PROD==input$descprodcp),]
    vcphgp=select(vcphgp,NOM_CTE,CSE_PROD,SUB_CSE,SUB_SUBCSE,DESC_PROD,"IMPORTE"= input$variableprodcte)
    
    vcphgp= ddply(vcphgp[],.(NOM_CTE),summarize,IMPORTE=sum(IMPORTE))
    vcphgp=vcphgp[which(vcphgp$IMPORTE>0 & vcphgp$IMPORTE<input$ventmayor),]
    vcphgp=vcphgp[order(vcphgp$IMPORTE),]
  })
  
  output$numerocte<-renderText({
    ctefil<-nrow(vcphgp())
    ctefil<-paste0("Numero total de clientes: ",as.character(ctefil))
  })
  
  
  output$vcphgp<- renderPlotly({  ###Participacion en ventas
    #hist(vcphgp()$IMPORTE,breaks=6)
    plot_ly(x = ~vcphgp()[,2], type = "histogram") %>% 
      layout(barmode = "overlay",
             xaxis = list(title = "Unidades Vendidas",
                          zeroline = FALSE),
             yaxis = list(title = "Numero de Clientes",
                          zeroline = FALSE))
    #plot_ly(x= vcphgp()[,2], type = "histogram")%>%layout(legend = list(orientation = 'r',font = list(size = 11)))
  })
  
  
  ################# tabla frecuencia acumulada por producto y cliente ############
  
  vcptp<-reactive({ #Grafica de pastel
    vcptp=cteprod[which(cteprod$FECHA>=fecha_ini() & cteprod$FECHA<=fecha_fin()),]
    vcptp= if (input$clasecp=="Todos") vcptp else vcptp[which(vcptp$CSE_PROD==input$clasecp),]
    vcptp= if (input$grupocp=="Todos") vcptp else vcptp[which(vcptp$SUB_CSE==input$grupocp),]
    vcptp= if (input$subgrupocp=="Todos") vcptp else vcptp[which(vcptp$SUB_SUBCSE==input$subgrupocp),]
    vcptp= if (input$descprodcp=="Todos") vcptp else vcptp[which(vcptp$DESC_PROD==input$descprodcp),]
    vcptp=select(vcptp,NOM_CTE,CSE_PROD,SUB_CSE,SUB_SUBCSE,DESC_PROD,"IMPORTE"= input$variableprodcte)
    
    
    
    vcptp= ddply(vcptp[],.(NOM_CTE),summarize,IMPORTE=sum(IMPORTE),Acum=999999999999,part=9.000,partacum=9.000)
    z= colSums(vcptp[,2,drop=FALSE])
    y=0
    vcptp=vcptp[order(vcptp$IMPORTE, decreasing = TRUE),]
    
    for(i in 1:nrow(vcptp)) {
      # i-th element of `u1` squared into `i`-th position of `usq`
      y <- vcptp$IMPORTE[i]+y
      vcptp$Acum[i]<-y
      vcptp$part[i]<-vcptp$IMPORT[i]/z
      vcptp$partacum[i]<-y/z
      
    }
    colnames(vcptp)=c("Cliente",as.character(input$variableprodcte),"Acumulado","Participacion","Part-Acum")              
    vcptp=as.data.frame(vcptp)
    
  })
  
  
  output$vcptp <- DT::renderDataTable(DT::datatable(vcptp(), filter = 'top', options = list(pageLength = 10, autoWidth = TRUE))%>%formatCurrency(columns=2:3, currency = "", interval = 3, mark = ",", digits = 0)%>%formatPercentage(columns=4:5,2))
  
  ############################################################
  #################  Existencia por producto #################
  
  output$Boxexiste1 = renderUI(selectInput("sucexist","Elige la Sucursal:",unique(catcte$LUGAR),"Todos"))            
  
  ################# GRAFICA MENSUAL Producto ############
  
  vpemg<-reactive({ #Ventas por producto por mes grafica importe Todos
    vpemg=prodalma[which(prodalma$FECHA>="2018-01-01" & prodalma$FECHA<=today() & prodalma$CVE_PROD==input$descprodexist),]
    vpemg= if (input$sucexist=="Todos") vpemg else vpemg[which(vpemg$LUGAR==input$sucexist),]
    vpemg= if (input$atrprodexist=="Todos") vpemg else vpemg[which(vpemg$NEW_MED==input$atrprodexist),]
    vpemg=ddply(vpemg[],.(ANIO,MES),summarize,CANTIDAD= sum(CANT_VEN))
    vpemg=as.data.frame(vpemg)
  })
  
  output$vpemg<- renderPlot({  ###historico Cantidad
    ggplot(vpemg(),aes(x=MES,y=CANTIDAD, group=factor(ANIO),shape=factor(ANIO), colour=factor(ANIO)))+geom_line(size=2) + scale_x_discrete(limits = c(1:12)) + xlab("MES") + ylab("CANTIDAD")+geom_text(stat="identity",aes(check_overlap = TRUE,label=prettyNum(round(CANTIDAD,0),big.mark=",",scientific=FALSE)),size=3,color="black")
  })
  
  
  ################# TABLA COMPARA ANIOS Producto ############
  
  vspextb<-reactive({ #Grafica de pastel
    vspextb=prodalma[which(prodalma$MES>=month(fecha_ini()) & prodalma$MES<=month(fecha_fin())),]
    vspextb=ddply(vspextb[],.(ANIO,LUGAR,CSE_PROD,CVE_PROD,DESC_PROD,NEW_MED,DESC_MED),summarize,IMPORTE= sum(CANT_VEN))
    
    vspextb= if (input$sucexist=="Todos") vspextb else vspextb[which(vspextb$LUGAR==input$sucexist),]
    vspextb= ddply(vspextb[],.(ANIO,CSE_PROD,CVE_PROD,DESC_PROD,NEW_MED,DESC_MED),summarize,IMPORTE=sum(IMPORTE))
    nombcol=colnames(vspextb[2:6])
    a=vspextb[which(vspextb$ANIO=="2017"),]
    a=select(a,-ANIO)
    colnames(a)=c(nombcol,"2017")
    b=vspextb[which(vspextb$ANIO=="2018"),]
    b=select(b,-ANIO)
    colnames(b)=c(nombcol,"2018")
    c=vspextb[which(vspextb$ANIO=="2019"),]
    c=select(c,-ANIO)
    colnames(c)=c(nombcol,"2019")            
    
    vspextb=merge(x=c,y=b,all.x=TRUE,all.y=TRUE)
    vspextb=merge(x=vspextb,y=a,all.x=TRUE,all.y=TRUE)
    vspextb=as.data.frame(vspextb)
    vspextb = vspextb [ , c(1,2,3,4,5,8,7,6)]
    
    exist=select(existencia,LUGAR,CSE_PROD,CVE_PROD,NEW_MED,"IMPORTE"= EXISTENCIA)
    exist= if (input$sucexist=="Todos") exist else exist[which(exist$LUGAR==input$sucexist),]
    exist= ddply(exist[],.(CSE_PROD,CVE_PROD,NEW_MED),summarize,EXISTENCIA=sum(IMPORTE))
    
    vspextb$CONTADOR1=0
    vspextb$CONTADOR2=0
    vspextb$CONTADOR3=0
    vspextb$CONTADOR1[!is.na(vspextb$'2017')] <- 1
    vspextb$CONTADOR2[!is.na(vspextb$'2018')] <- 1
    vspextb$CONTADOR3[!is.na(vspextb$'2019')] <- 1
    
    vspextb$'2017'[is.na(vspextb$'2017')] <- 0
    vspextb$'2018'[is.na(vspextb$'2018')] <- 0
    vspextb$'2019'[is.na(vspextb$'2019')] <- 0
    
    vspextb=merge(x=vspextb,exist,all.x=TRUE)
    vspextb=as.data.frame(vspextb)
    vspextb$CONTADOR4=vspextb$CONTADOR1+vspextb$CONTADOR2+vspextb$CONTADOR3
    vspextb$PROMEDIO=(vspextb$'2017'+vspextb$'2018'+vspextb$'2019')/vspextb$CONTADOR4
    vspextb$'EXIST-2019'=(vspextb$EXISTENCIA-vspextb$'2019')
    vspextb$'EXIST-PROM'=(vspextb$EXISTENCIA-vspextb$PROMEDIO)
    vspextb=select(vspextb,-CONTADOR1,-CONTADOR2,-CONTADOR3,-CONTADOR4)
    vspextb = vspextb [ , c(1,2,4,3,5,6,7,8,10,9,11,12)]
    vspextb=vspextb[order(-vspextb$'EXIST-2019'),]
    vspextb
  })
  
  output$vspextb <- DT::renderDataTable(DT::datatable(vspextb(),filter = 'top',extensions = 'Buttons', 
                                                      options = list(lengthMenu = list(c(10,20,50,100, -1), list('10', '20', '50', '100', 'All')),pageLength = 10, autoWidth = TRUE,scrollX = TRUE,dom = 'lBfrtip',
                                                                     buttons = c('copy', 'excel')))%>%formatStyle(columns =1:5, fontSize = '80%')%>%formatCurrency(columns=6:12, currency = "", interval = 3, mark = ",", digits = 0))
  
  
  
  
  ##
  
  ###########################################################
  ####################  Agentes  ###########################
  ###########################################################
  
  ctage<-reactive({ #Catalogo de clientes
    ctage= if (input$sucage=="Todos") catage else catage[which(catage$LUGAR==input$sucage),]
    ctage= if (input$canage=="Todos") ctage else ctage[which(ctage$TIPO==input$canage),]
  })
  
  output$Boxage1 = renderUI(selectInput("sucage","Elige la Sucursal:",unique(catage$LUGAR),"Todos"))
  output$Boxage2 = renderUI(selectInput("canage","Elige un Canal de Ventas:",if (input$sucage=="Todos") unique(catage$TIPO) else catage$TIPO[which(catage$LUGAR == input$sucage)],"Todos"))
  output$Boxage3 = renderUI(selectInput("agentenv","Elige un Agente:",unique(ctage()$NOM_AGE),"Todos"))
  
  #################Ventas mostrador############
  
  ###############  POR HORA  ####################
  agehr<-reactive({ #Ventas por mes grafica importe Todos
    agehr=agehora[which(agehora$FECHA>="2017-01-01" & agehora$FECHA<=today()),]
    agehr= if (input$sucage=="Todos") agehr else agehr[which(agehr$LUGAR==input$sucage),]
    agehr= if (input$canage=="Todos") agehr else agehr[which(agehr$NOM_CAN==input$canage),]
    agehr= if (input$agentenv=="Todos") agehr else agehr[which(agehr$NOM_AGE==input$agentenv),]
    agehr=agehr[which(agehr$HORA>=8 & agehr$HORA<=18),]
    agehr= agehr[which(agehr$ANIO==input$ANIOAGE),]
    agehr=ddply(agehr[],.(MES,HORA),summarize,VENTA=sum(CNT_FAC))
  })
  
  output$agehr<- renderPlot({  ###Historico Importe
    ggplot(agehr() ,aes(x=HORA,y=VENTA, group=factor(MES), shape=factor(MES), colour=factor(MES)))+geom_line(size=2)+ scale_y_continuous(labels = comma) + scale_x_discrete(limits = c(8:18)) + xlab("HORA") + ylab("NUMERO DE DOCUMENTOS VENDIDOS") +geom_text(stat="identity",aes(check_overlap = TRUE,label=prettyNum(round(VENTA,0),big.mark=",",scientific=FALSE)),size=3,color="black")
  })
  
  ###############  POR MES  ####################
  agemes<-reactive({ #Ventas por mes grafica importe Todos
    agemes=agevtahist[which(agevtahist$FECHA>="2017-01-01" & agevtahist$FECHA<=today()),]
    agemes= if (input$sucage=="Todos") agemes else agemes[which(agemes$LUGAR==input$sucage),]
    agemes= if (input$canage=="Todos") agemes else agemes[which(agemes$NOM_CAN==input$canage),]
    agemes= if (input$agentenv=="Todos") agemes else agemes[which(agemes$NOM_AGE==input$agentenv),]
    agemes=ddply(agemes[],.(ANIO,MES),summarize,VENTA=sum(IMP_TOT))
  })
  
  output$agemes<- renderPlot({  ###Historico Importe
    ggplot(agemes() ,aes(x=MES,y=VENTA, group=factor(ANIO), shape=factor(ANIO), colour=factor(ANIO)))+geom_line(size=2)+ scale_y_continuous(labels = comma) + scale_x_discrete(limits = c(1:12)) + xlab("MES") + ylab("IMPORTE") +geom_text(stat="identity",aes(check_overlap = TRUE,label=prettyNum(round(VENTA,0),big.mark=",",scientific=FALSE)),size=3,color="black")
  })
  
  ###############  Participacion por agente  ####################
  
  vagp<-reactive({ #Grafica de pastel
    vagp=agevtahist[which(agevtahist$FECHA>=fecha_ini() & agevtahist$FECHA<=fecha_fin()),]
    vagp= if (input$sucage=="Todos") vagp else vagp[which(vagp$LUGAR==input$sucage),]
    vagp= if (input$canage=="Todos") vagp else vagp[which(vagp$NOM_CAN==input$canage),]
    vagp=select(vagp,NOM_AGE,IMP_TOT)
    
    vagp=ddply(vagp[],.(NOM_AGE),summarize,VENTA=sum(IMP_TOT))
    a= colSums(vagp[,2,drop=FALSE])
    vagp=ddply(vagp[],.(vagp[,1]),summarize,VENTA=sum(VENTA),Part=VENTA/a)
    vagp=as.data.frame(vagp)
    colnames(vagp)=c("nombre","MTOVTA","Part")
    otro=vagp[which(vagp$Part<=input$Participaage),]
    a=colSums(otro[,2,drop=FALSE])
    b=colSums(otro[,3,drop=FALSE])
    otros=list(nombre="Otros", MTOVTA=a, Part=b)
    vagp=rbind(vagp,otros)
    vagp=vagp[which(vagp$Part>input$Participaage),]
    vagp=as.data.frame(vagp)
  })
  
  output$vagp<- renderPlotly({  ###Participacion en ventas
    plot_ly(vagp(), labels = ~vagp()[,1], values = ~vagp()[,2], type = 'pie')%>%layout(legend = list(orientation = 'r',font = list(size = 9)))
  })
  
  ###############  Participacion por producto  ####################
  prodnv<-reactive({ #catalogo de productos subgrupo
    prodnv= if (input$clasenv=="Todos") catprod else catprod[which(catprod$CSE_PROD==input$clasenv),]
    prodnv= if (input$gruponv=="Todos") prodnv else prodnv[which(prodnv$SUB_CSE==input$gruponv),]
  })
  
  output$Boxnv11 = renderUI(selectInput("clasenv","Elige la Clase de Producto:",unique(catprod$CSE_PROD),"Todos"))
  output$Boxnv22 = renderUI(selectInput("gruponv","Elige el Grupo:",if (input$clasenv=="Todos") unique(catprod$SUB_CSE) else catprod$SUB_CSE[which(catprod$CSE_PROD == input$clasenv)],"Todos"))
  output$Boxnv33 = renderUI(selectInput("subgruponv","Elige el SubGrupo:",unique(prodnv()$SUB_SUBCSE),"Todos"))
  
  vpnvgp<-reactive({ #Grafica de pastel
    vpnvgp=ageprod[which(ageprod$FECHA>=fecha_ini() & ageprod$FECHA<=fecha_fin()),]
    vpnvgp= if (input$sucage=="Todos") vpnvgp else vpnvgp[which(vpnvgp$LUGAR==input$sucage),]
    vpnvgp= if (input$canage=="Todos") vpnvgp else vpnvgp[which(vpnvgp$NOM_CAN==input$canage),]
    vpnvgp= if (input$agentenv=="Todos") vpnvgp else vpnvgp[which(vpnvgp$NOM_AGE==input$agentenv),]
    vpnvgp= if (input$clasenv=="Todos") vpnvgp else vpnvgp[which(vpnvgp$CSE_PROD==input$clasenv),]
    vpnvgp= if (input$gruponv=="Todos") vpnvgp else vpnvgp[which(vpnvgp$SUB_CSE==input$gruponv),]
    vpnvgp= if (input$subgruponv=="Todos") vpnvgp else vpnvgp[which(vpnvgp$SUB_SUBCSE==input$subgruponv),]
    vpnvgp=select(vpnvgp,CSE_PROD,SUB_CSE,SUB_SUBCSE,DESC_PROD,IMP_TOT)
    
    
    vpnvgp= if (input$clasenv=="Todos") ddply(vpnvgp[],.(CSE_PROD),summarize,VENTA=sum(IMP_TOT)) else vpnvgp
    vpnvgp= if (input$clasenv!="Todos" & input$gruponv=="Todos") ddply(vpnvgp[],.(SUB_CSE),summarize,VENTA=sum(IMP_TOT)) else vpnvgp
    vpnvgp= if (input$clasenv!="Todos" & input$gruponv!="Todos" & input$subgruponv=="Todos") ddply(vpnvgp[],.(SUB_SUBCSE),summarize,VENTA=sum(IMP_TOT)) else vpnvgp
    vpnvgp= if (input$clasenv!="Todos" & input$gruponv!="Todos" & input$subgruponv!="Todos") ddply(vpnvgp[],.(DESC_PROD),summarize,VENTA=sum(IMP_TOT)) else vpnvgp
    a= colSums(vpnvgp[,2,drop=FALSE])
    vpnvgp=ddply(vpnvgp[],.(vpnvgp[,1]),summarize,VENTA=sum(VENTA),Part=VENTA/a)
    vpnvgp=as.data.frame(vpnvgp)
    colnames(vpnvgp)=c("nombre","MTOVTA","Part")
    otro=vpnvgp[which(vpnvgp$Part<=input$Participanvp),]
    a=colSums(otro[,2,drop=FALSE])
    b=colSums(otro[,3,drop=FALSE])
    otros=list(nombre="Otros", MTOVTA=a, Part=b)
    vpnvgp=rbind(vpnvgp,otros)
    vpnvgp=vpnvgp[which(vpnvgp$Part>input$Participanvp),]
    vpnvgp=as.data.frame(vpnvgp)
  })
  output$vpnvgp<- renderPlotly({  ###Participacion en ventas
    plot_ly(vpnvgp(), labels = ~vpnvgp()[,1], values = ~vpnvgp()[,2], type = 'pie')%>%layout(legend = list(orientation = 'r',font = list(size = 11)))
  })
  
  
  ###############  Historico por producto  ####################
  vpnvgl<-reactive({ #Historico por producto
    vpnvgl=ageprod[which(ageprod$ANIO==input$ANIONVHIST),]
    vpnvgl= if (input$sucage=="Todos") vpnvgl else vpnvgl[which(vpnvgl$LUGAR==input$sucage),]
    vpnvgl= if (input$canage=="Todos") vpnvgl else vpnvgl[which(vpnvgl$NOM_CAN==input$canage),]
    vpnvgl= if (input$agentenv=="Todos") vpnvgl else vpnvgl[which(vpnvgl$NOM_AGE==input$agentenv),]
    vpnvgl= if (input$clasenv=="Todos") vpnvgl else vpnvgl[which(vpnvgl$CSE_PROD==input$clasenv),]
    vpnvgl= if (input$gruponv=="Todos") vpnvgl else vpnvgl[which(vpnvgl$SUB_CSE==input$gruponv),]
    vpnvgl= if (input$subgruponv=="Todos") vpnvgl else vpnvgl[which(vpnvgl$SUB_SUBCSE==input$subgruponv),]
    vpnvgl=select(vpnvgl,MES,CSE_PROD,SUB_CSE,SUB_SUBCSE,DESC_PROD,IMP_TOT)
    
    
    vpnvgl= if (input$clasenv=="Todos") ddply(vpnvgl[],.(CSE_PROD,MES),summarize,VENTA=sum(IMP_TOT)) else vpnvgl
    vpnvgl= if (input$clasenv!="Todos" & input$gruponv=="Todos") ddply(vpnvgl[],.(SUB_CSE,MES),summarize,VENTA=sum(IMP_TOT)) else vpnvgl
    vpnvgl= if (input$clasenv!="Todos" & input$gruponv!="Todos" & input$subgruponv=="Todos") ddply(vpnvgl[],.(SUB_SUBCSE,MES),summarize,VENTA=sum(IMP_TOT)) else vpnvgl
    vpnvgl= if (input$clasenv!="Todos" & input$gruponv!="Todos" & input$subgruponv!="Todos") ddply(vpnvgl[],.(DESC_PROD,MES),summarize,VENTA=sum(IMP_TOT)) else vpnvgl
    colnames(vpnvgl)=c("CLAS","MES","VENTA")
    vpnvgl=as.data.frame(vpnvgl)
  })
  
  
  output$vpnvgl<- renderPlotly({  ###
    plot_ly(vpnvgl(), x = ~MES, y = ~VENTA, type = 'scatter', mode = 'lines',color= ~CLAS,
            line = list(width = 4)) %>%
      layout(showlegend=TRUE,xaxis = list(range= c(1, 12)),
             yaxis = list (title = "Numero de Ventas"),
             legend = list(orientation = 'R',font = list(size = 8))) %>%
      add_annotations(x = vpnvgl()$MES,
                      y = vpnvgl()$VENTA,
                      text = rownames(vpnvgl()),
                      xref = "x",
                      yref = "y",
                      showarrow = FALSE,
                      font=list(size=9))
  })
  

  ###########################################################
  ####################  Cobranza  ###########################
  ###########################################################
  
  
      #################### COBRANZA POR CLIENTE ##################
                
                ###### Limpio la base
                
                basecobfin=basecob[,(1:25)]
                basecobfin=basecobfin[which(basecobfin$FALTA_FAC>="2018-01-01" & basecobfin$PAGOCOMP==1),]
                
              
                
                ###### Filtro del cliente  
                
                output$ctecob = renderUI(selectInput("ctecob","Elige un Cliente:",unique(basecobfin$NOM_CTE),"ADRIAN ENRIQUE LEDESMA HERNANDEZ"))  
              
                ###### Filtro la base de acuerdo al cliente elegido #####
                
                basecobranza<-reactive({ #Historico por producto
                  basecobranza=basecobfin[which(basecobfin$NOM_CTE==input$ctecob),]
                })
                
                #### Info Boxes #####
                  
                output$vendcob<-renderInfoBox({
                  a=nrow(basecobranza())
                  vendedor=basecobranza()[a,7]
                  infoBox("Vendedor",vendedor,width=3,icon=icon("address-card"))
                })
                
                output$diacre<-renderInfoBox({
                  diacre=basecobranza()[1,13]
                  infoBox("Dias de credito asignados",diacre,width=3,icon=icon("handshake"))
                })
                
                output$avgdp<-renderInfoBox({
                  avgdp=round(mean(basecobranza()$DIAS_PAG),0)
                  infoBox("Promedio de dias de Pago",avgdp,width=3,icon=icon("calendar-alt"))
                })
                
                output$mtsf<-renderInfoBox({
                  factpencob=factpencob[which(factpencob$NOM_CTE==input$ctecob),]
                  #factpencob=ddply(factpencob[],.(factpencob[,1]),summarize,MTOVTA=sum(MTOVTA),Part=MTOVTA/a)
                  a= colSums(factpencob[,7,drop=FALSE])
                  a=round(a,0)
                  a= prettyNum(a,big.mark=",",scientific=FALSE)
                  infoBox("Monto total de Facturas Pendientes de Cobro",a,width=3,icon=icon("dollar-sign"))
                })
                
                
                ###### Grafica de Barras
                
                output$barcob<- renderPlot({  ###Grafica vs Presupuesto
                  basecobfin=ddply(basecobranza(),.(ANIO,MES,RETRASO),summarize,FACTURAS=sum(PAGOCOMP))    
                  cob1 =basecobfin[which(basecobfin$ANIO==year(today())),]
                  cob2 =basecobfin[which(basecobfin$ANIO==year(today())-1),]
                  cob3 =basecobfin[which(basecobfin$ANIO==year(today())-2),]
                  
                  MES<-c(1:12)
                  FACTURAS<-c(0,0,0,0,0,0,0,0,0,0,0,0)
                  RETRASO<-c("A Tiempo","A Tiempo","Retraso","A Tiempo","A Tiempo","A Tiempo","A Tiempo","A Tiempo","A Tiempo","A Tiempo","A Tiempo","A Tiempo")
                  error<-data.frame(MES,FACTURAS,RETRASO)
              
              
                  if(nrow(cob1) == 0){
                    pcob1 <- ggplot(error,aes(x = MES,FACTURAS,fill=RETRASO))+geom_bar(position = "dodge",stat="identity") +                        
                      xlab("") + ylab("")+ scale_x_discrete(breaks= c(1:12), labels= c(1:12),limits = c(1:12)) + ggtitle(as.character(year(today()))) +
                      theme(plot.title = element_text(size=14, face="bold.italic",hjust = 0.5))
                  } else {
                  pcob1 <- ggplot(cob1,aes(x = MES,FACTURAS,fill=RETRASO))+geom_bar(position = "dodge",stat="identity") +                        
                    xlab("") + ylab("")+ scale_x_discrete(breaks= c(1:12), labels= c(1:12),limits = c(1:12)) + ggtitle(cob1$ANIO) +
                    theme(plot.title = element_text(size=14, face="bold.italic",hjust = 0.5))
                  }
                  
                  
                  if(nrow(cob2) == 0){
                    pcob2 <- ggplot(error,aes(x = MES,FACTURAS,fill=RETRASO))+geom_bar(position = "dodge",stat="identity") +                        
                      xlab("") + ylab("")+ scale_x_discrete(breaks= c(1:12), labels= c(1:12),limits = c(1:12)) + ggtitle(as.character(year(today())-1)) +
                      theme(plot.title = element_text(size=14, face="bold.italic",hjust = 0.5))
                  } else {
                  pcob2 <- ggplot(cob2,aes(x = MES,FACTURAS,fill=RETRASO))+geom_bar(position = "dodge",stat="identity") +                        
                    xlab("") + ylab("Numero de Facturas") + scale_x_discrete(limits = c(1:12)) + ggtitle(cob2$ANIO) +
                    theme(plot.title = element_text(size=14, face="bold.italic",hjust = 0.5))
                  }
                  
                  
                  if(nrow(cob3) == 0){
                    pcob3 <- ggplot(error,aes(x = MES,FACTURAS,fill=RETRASO))+geom_bar(position = "dodge",stat="identity") +                        
                      xlab("") + ylab("")+ scale_x_discrete(breaks= c(1:12), labels= c(1:12),limits = c(1:12)) + ggtitle(as.character(year(today())-2)) +
                      theme(plot.title = element_text(size=14, face="bold.italic",hjust = 0.5))
                  } else {
                  pcob3 <- ggplot(cob3,aes(x = MES,FACTURAS,fill=RETRASO))+geom_bar(position = "dodge",stat="identity") +                        
                    xlab("Meses") + ylab("")+ scale_x_discrete(limits = c(1:12)) + ggtitle(cob3$ANIO) +
                    theme(plot.title = element_text(size=14, face="bold.italic",hjust = 0.5))
                  }
                  
                  grid.arrange(pcob1, pcob2, pcob3)
                  
                })  
                
                
                ##### Tabla de Dias De pago con retraso y  a tiempo
                
                output$tabdpr<- DT::renderDataTable({
                  basecobfin<-basecobranza()[which(basecobranza()$RETRASO=="Retraso"),]
                  basecobfin<-dcast(basecobfin,ANIO~MES,mean,value.var="DIAS_PAG")
                  basecobfin<-round(basecobfin,0)
                  basecobfin<- replace(basecobfin, is.na(basecobfin), 0)
                  DT::datatable(basecobfin,
                                          extensions = 'Buttons', 
                                          options = list(lengthMenu = list(c(10,20,50,100, -1), list('10', '20', '50', '100', 'All')),pageLength = 10, autoWidth = FALSE,scrollX = TRUE,dom = 't'))
                  }) 
                
                
                output$tabdpat<- DT::renderDataTable({
                  basecobfin<-basecobranza()[which(basecobranza()$RETRASO=="A Tiempo"),]
                  basecobfin<-dcast(basecobfin,ANIO~MES,mean,value.var="DIAS_PAG")
                  basecobfin<-round(basecobfin,0)
                  basecobfin<- replace(basecobfin, is.na(basecobfin), 0)
                  DT::datatable(basecobfin,
                                extensions = 'Buttons', 
                                options = list(lengthMenu = list(c(10,20,50,100, -1), list('10', '20', '50', '100', 'All')),pageLength = 10, autoWidth = FALSE,scrollX = TRUE,dom = 't'))
                }) 
                
              
                ##### Tabla de Dias De Credito
                  
                output$tabdc<- DT::renderDataTable({
                  basecobfin<-dcast(basecobranza(),ANIO~MES,mean,value.var="DIAS_CRED")
                  basecobfin<-round(basecobfin,0)
                  basecobfin<- replace(basecobfin, is.na(basecobfin), 0)
                  DT::datatable(basecobfin,
                                extensions = 'Buttons', 
                                options = list(lengthMenu = list(c(10,20,50,100, -1), list('10', '20', '50', '100', 'All')),pageLength = 10, autoWidth = FALSE,scrollX = TRUE,dom = 't'))
                }) 
                
                ##### Tabla de de las facturas pendientes
                
                output$tabfpc<- DT::renderDataTable({
                  factpencob=factpencob[which(factpencob$NOM_CTE==input$ctecob),]
                  factpencob=select(factpencob,-CVE_CTE,-NOM_CTE)
                  factpencob=factpencob[,1:6]
                  DT::datatable(factpencob,
                                extensions = 'Buttons', 
                                options = list(lengthMenu = list(c(10,20,50,100, -1), list('10', '20', '50', '100', 'All')),pageLength = 10, autoWidth = FALSE,scrollX = TRUE,dom = 'lt'))%>%formatStyle(columns =1:6, fontSize = '90%')%>%formatCurrency(columns=c(4,5), currency = "$  ", interval = 3, mark = ",", digits = 0)
                }) 
                
              ##factpencob
                
                
                
                output$tablarescob <- DT::renderDataTable({
                  basecobfin1<-select(basecobranza(),-NOM_CTE,-IMPORTE,-CANTIDAD,-NOM_CAN,-FALTA_CTE,-NOM_ZON,-NOM_SUB,-LUGAR,-NOM_AGE,-CVE_MON,-DIA_CRE)
                  DT::datatable(basecobfin1,
                                                                         filter = 'top',
                                                                         extensions = 'Buttons', 
                                                                         options = list(lengthMenu = list(c(10,20,50,100, -1), list('10', '20', '50', '100', 'All')),pageLength = 10, autoWidth = FALSE,scrollX = TRUE,dom = 'lBfrtip',
                                                                                        buttons = c('copy', 'excel')))%>%formatStyle(columns =10, fontSize = '100%')%>%formatCurrency(columns=c(4), currency = "$  ", interval = 3, mark = ",", digits = 0)
                })
                
      ####################  RESUMEN DE COBRANZA  ##################
                
                
                ##Porcentaje del monto facturado de facturas con retraso
                
                output$tabprmf<- DT::renderDataTable({
                  basecobfin1<-dcast(basecobfin,MES~ANIO,sum,value.var="TOT_FAC")
                  
                  basecobfin2<-basecobfin[which(basecobfin$RETRASO=="Retraso"),]
                  basecobfin2<-dcast(basecobfin2,MES~ANIO,sum,value.var="TOT_FAC")
                  basecobfin<-basecobfin2/basecobfin1
                  basecobfin[,1]<-c(1:12)
                  basecobfin<-round(basecobfin,4)
                  basecobfin<- replace(basecobfin, is.na(basecobfin), 0)
                  DT::datatable(basecobfin,
                                extensions = 'Buttons', 
                                options = list(lengthMenu = list(c(12), list('12')),pageLength = 12, autoWidth = FALSE,scrollX = TRUE,dom = 't'))%>%formatPercentage(columns=c(2:13),2)
                })   
                
                ##Porcentaje del número de facturas con retraso
                
                output$tabprnf<- DT::renderDataTable({
                  basecobfin1<-dcast(basecobfin,MES~ANIO,sum,value.var="PAGOCOMP")
                  
                  basecobfin2<-basecobfin[which(basecobfin$RETRASO=="Retraso"),]
                  basecobfin2<-dcast(basecobfin2,MES~ANIO,sum,value.var="PAGOCOMP")
                  basecobfin<-basecobfin2/basecobfin1
                  basecobfin[,1]<-c(1:12)
                  basecobfin<-round(basecobfin,4)
                  basecobfin<- replace(basecobfin, is.na(basecobfin), 0)
                  DT::datatable(basecobfin,
                                extensions = 'Buttons', 
                                options = list(lengthMenu = list(c(12), list('12')),pageLength = 12, autoWidth = FALSE,scrollX = TRUE,dom = 't'))%>%formatPercentage(columns=c(2:13),2)
                })   
  

                
                ##Porcentaje del monto facturado de facturas pagadas en 5 o menos dias
                
                output$tabp5mf<- DT::renderDataTable({
                  basecobfin1<-dcast(basecobfin,MES~ANIO,sum,value.var="TOT_FAC")
                  
                  basecobfin2<-basecobfin[which(basecobfin$DIAS_PAG<=5),]
                  basecobfin2<-dcast(basecobfin2,MES~ANIO,sum,value.var="TOT_FAC")
                  basecobfin<-basecobfin2/basecobfin1
                  basecobfin[,1]<-c(1:12)
                  basecobfin<-round(basecobfin,4)
                  basecobfin<- replace(basecobfin, is.na(basecobfin), 0)
                  DT::datatable(basecobfin,
                                extensions = 'Buttons', 
                                options = list(lengthMenu = list(c(12), list('12')),pageLength = 12, autoWidth = FALSE,scrollX = TRUE,dom = 't'))%>%formatPercentage(columns=c(2:13),2)
                })   
                
                ##Porcentaje del número de facturas pagadas en 5 o menos dias
                
                output$tabp5nf<- DT::renderDataTable({
                  basecobfin1<-dcast(basecobfin,MES~ANIO,sum,value.var="PAGOCOMP")
                  
                  basecobfin2<-basecobfin[which(basecobfin$DIAS_PAG<=5),]
                  basecobfin2<-dcast(basecobfin2,MES~ANIO,sum,value.var="PAGOCOMP")
                  basecobfin<-basecobfin2/basecobfin1
                  basecobfin[,1]<-c(1:12)
                  basecobfin<-round(basecobfin,4)
                  basecobfin<- replace(basecobfin, is.na(basecobfin), 0)
                  DT::datatable(basecobfin,
                                extensions = 'Buttons', 
                                options = list(lengthMenu = list(c(12), list('12')),pageLength = 12, autoWidth = FALSE,scrollX = TRUE,dom = 't'))%>%formatPercentage(columns=c(2:13),2)
                })
                
      ####################  EVALUACION DE COBRANZA  ##################
                
                ###### Limpio la base
                
                #evcobbase=evcob[which(evcob$FALTA_FAC>="2016-01-01" & evcob$PAGOCOMP==1),]
                evcobbase=evcob
                evcobbase$COBTOT[is.na(evcobbase$COBTOT)] <- 0
                evcobbase$COBREAL[is.na(evcobbase$COBREAL)] <- 0
                evcobbase$CANTIDAD[is.na(evcobbase$CANTIDAD)] <- 0
                
                
                ###### Filtros  
                
                output$agecob = renderUI(selectInput("agecob","Elige un Agente:",c(as.vector(unique(evcobbase$NOM_AGE)),"Todos"),"Todos"))
                output$mescob = renderUI(selectInput("mescob","Elige un Mes:",c(1:12),month(today())))  
                output$aniocob = renderUI(selectInput("aniocob","Elige un Anio:",c(year(today())-2,year(today())-1,year(today())),year(today())))  
                
                ###### Filtro la base de acuerdo al agente anio y  mes elegido #####
                
                evacobage<-reactive({ #Historico por producto
                  if (input$agecob=="Todos") evcobbase else (evaluacioncob=evcobbase[which(evcobbase$NOM_AGE==input$agecob),])
                })
                
                evacobfin<-reactive({ #Historico por producto
                  evaluacioncob=evacobage()[which(evacobage()$ANOPROMPAG==input$aniocob & evacobage()$MESPROMPAG==input$mescob),]
                })
                
                ###### Creacion de indicadores #####
                
                #### Facturas Recuperadas###
                recuperadoscob<-reactive({ #Historico por producto
                  recuperados=evacobage()[which(evacobage()$ANIOPAG==as.numeric(input$aniocob) & evacobage()$MESPAG==as.numeric(input$mescob) & evacobage()$ANOPROMPAG==as.numeric(input$aniocob) & evacobage()$MESPROMPAG<as.numeric(input$mescob) & evacobage()$DIAS_CRED>0),]
                  a= colSums(recuperados[,33,drop=FALSE])
                  recuperados2=evacobage()[which(evacobage()$ANIOPAG==as.numeric(input$aniocob) & evacobage()$MESPAG==as.numeric(input$mescob) & evacobage()$ANOPROMPAG<as.numeric(input$aniocob) & evacobage()$DIAS_CRED>0),]
                  b= colSums(recuperados2[,33,drop=FALSE])
                  c=a+b
                   })
                
                m_recuperadoscob<-reactive({ #Historico por producto
                  recuperados=evacobage()[which(evacobage()$ANIOPAG==as.numeric(input$aniocob) & evacobage()$MESPAG==as.numeric(input$mescob) & evacobage()$ANOPROMPAG==as.numeric(input$aniocob) & evacobage()$MESPROMPAG<as.numeric(input$mescob) & evacobage()$DIAS_CRED>0),]
                  a= colSums(recuperados[,18,drop=FALSE])
                  recuperados2=evacobage()[which(evacobage()$ANIOPAG==as.numeric(input$aniocob) & evacobage()$MESPAG==as.numeric(input$mescob) & evacobage()$ANOPROMPAG<as.numeric(input$aniocob) & evacobage()$DIAS_CRED>0),]
                  b= colSums(recuperados2[,18,drop=FALSE])
                  c=a+b
                })
                
                #### Cobranza del mes###
                cobranzadelmes<-reactive({ #Historico por producto
                  recuperados=evacobage()[which(evacobage()$ANOPROMPAG==as.numeric(input$aniocob) & evacobage()$MESPROMPAG==as.numeric(input$mescob) & evacobage()$DIAS_CRED>0),]
                  a= colSums(recuperados[,32,drop=FALSE])
                })
                
                m_cobranzadelmes<-reactive({ #Historico por producto
                  recuperados=evacobage()[which(evacobage()$ANOPROMPAG==as.numeric(input$aniocob) & evacobage()$MESPROMPAG==as.numeric(input$mescob) & evacobage()$DIAS_CRED>0 & evacobage()$COBREAL==1),]
                  a= colSums(recuperados[,18,drop=FALSE])
                })
                

                #### Pagos anticipados###
                anticiposcob<-reactive({ #Historico por producto
                  recuperados=evacobage()[which(evacobage()$ANIOPAG==as.numeric(input$aniocob) & evacobage()$MESPAG==as.numeric(input$mescob) & evacobage()$ANOPROMPAG==as.numeric(input$aniocob) & evacobage()$MESPROMPAG>as.numeric(input$mescob) & evacobage()$DIAS_CRED>0),]
                  a= colSums(recuperados[,33,drop=FALSE])
                  recuperados2=evacobage()[which(evacobage()$ANIOPAG==as.numeric(input$aniocob) & evacobage()$MESPAG==as.numeric(input$mescob) & evacobage()$ANOPROMPAG>as.numeric(input$aniocob) & evacobage()$DIAS_CRED>0),]
                  b= colSums(recuperados2[,33,drop=FALSE])
                  c=a+b
                })
                
                m_anticiposcob<-reactive({ #Historico por producto
                  recuperados=evacobage()[which(evacobage()$ANIOPAG==as.numeric(input$aniocob) & evacobage()$MESPAG==as.numeric(input$mescob) & evacobage()$ANOPROMPAG==as.numeric(input$aniocob) & evacobage()$MESPROMPAG>as.numeric(input$mescob) & evacobage()$DIAS_CRED>0),]
                  a= colSums(recuperados[,18,drop=FALSE])
                  recuperados2=evacobage()[which(evacobage()$ANIOPAG==as.numeric(input$aniocob) & evacobage()$MESPAG==as.numeric(input$mescob) & evacobage()$ANOPROMPAG>as.numeric(input$aniocob) & evacobage()$DIAS_CRED>0),]
                  b= colSums(recuperados2[,18,drop=FALSE])
                  c=a+b
                })
                
                #### Total de facturas a Cobrar del mes###
                totcobranzadelmes<-reactive({ #Historico por producto
                  recuperados=evacobage()[which(evacobage()$ANOPROMPAG==as.numeric(input$aniocob) & evacobage()$MESPROMPAG==as.numeric(input$mescob) & evacobage()$DIAS_CRED>0),]
                  a= colSums(recuperados[,31,drop=FALSE])
                })
                

                m_totcobranzames<-reactive({ #Historico por producto
                  recuperados=evacobage()[which(evacobage()$ANOPROMPAG==as.numeric(input$aniocob) & evacobage()$MESPROMPAG==as.numeric(input$mescob) & evacobage()$DIAS_CRED>0 & evacobage()$COBTOT==1),]
                  a= colSums(recuperados[,18,drop=FALSE])
                })
                
                
                output$sumacob<-renderTable({
                  conceptos=c('Recuperados','Cobranza del mes','Pago anticipado','_______________________','Pendiente de Cobrar','Pendiente de Cobrar (%)','_______________________','Total a cobrar')
                  No_facs=c(recuperadoscob(),cobranzadelmes(),anticiposcob(),'_______________________',(totcobranzadelmes()-cobranzadelmes()),paste0(round(((totcobranzadelmes()-cobranzadelmes())/totcobranzadelmes())*100,2)," %"),'_______________________',totcobranzadelmes())
                  montos=c(prettyNum(round(m_recuperadoscob(),0),big.mark=",",scientific=FALSE),prettyNum(round(m_cobranzadelmes()),big.mark=",",scientific=FALSE),prettyNum(round(m_anticiposcob()),big.mark=",",scientific=FALSE),'_______________________',prettyNum(round((m_totcobranzames()-m_cobranzadelmes()),0),big.mark=",",scientific=FALSE),paste0(round(((m_totcobranzames()-m_cobranzadelmes())/m_totcobranzames())*100,2)," %"),'_______________________',prettyNum(round(m_totcobranzames(),0),big.mark=",",scientific=FALSE))
                  tabla=data.frame("Conceptos"=conceptos,"Monto de Facturas"=montos,"Numero de Facturas"=No_facs)
                  
                }, align = 'r')              
                
                #### Facturas Dias de Credito Cero Pagadas###
                diascredceropag<-reactive({ #Historico por producto
                  recuperados=evacobage()[which(evacobage()$ANOPROMPAG==as.numeric(input$aniocob) & evacobage()$MESPROMPAG==as.numeric(input$mescob) & evacobage()$DIAS_CRED==0& evacobage()$PAGOCOMP==1),]
                  a= colSums(recuperados[,33,drop=FALSE])
                })
                
                m_diascredceropag<-reactive({ #Historico por producto
                  recuperados=evacobage()[which(evacobage()$ANOPROMPAG==as.numeric(input$aniocob) & evacobage()$MESPROMPAG==as.numeric(input$mescob) & evacobage()$DIAS_CRED==0& evacobage()$PAGOCOMP==1),]
                  a= colSums(recuperados[,18,drop=FALSE])
                })
                
                #### Facturas Dias de Credito Cero No Pagadas###
                diascredceronopag<-reactive({ #Historico por producto
                  recuperados=evacobage()[which(evacobage()$ANOPROMPAG==as.numeric(input$aniocob) & evacobage()$MESPROMPAG==as.numeric(input$mescob) & evacobage()$DIAS_CRED==0& evacobage()$PAGOCOMP==0),]
                  a= colSums(recuperados[,33,drop=FALSE])
                })
                
                m_diascredceronopag<-reactive({ #Historico por producto
                  recuperados=evacobage()[which(evacobage()$ANOPROMPAG==as.numeric(input$aniocob) & evacobage()$MESPROMPAG==as.numeric(input$mescob) & evacobage()$DIAS_CRED==0& evacobage()$PAGOCOMP==0),]
                  a= colSums(recuperados[,18,drop=FALSE])
                })
                
                
                output$sumacob_credcero<-renderTable({
                  conceptos=c('Cobradas en el mes','Pendientes de Cobrar','Total')
                  No_facs=c(diascredceropag(),diascredceronopag(),diascredceropag()+diascredceronopag())
                  montos=c(prettyNum(round(m_diascredceropag(),0),big.mark=",",scientific=FALSE),prettyNum(round(m_diascredceronopag()),big.mark=",",scientific=FALSE),prettyNum(round(m_diascredceropag()+m_diascredceronopag()),big.mark=",",scientific=FALSE))
                  tabla=data.frame("Conceptos"=conceptos,"Monto de Facturas"=montos,"Numero de Facturas"=No_facs)
                }, align = 'r')  
                
                  
                ###### Gráficas de Cobranza #####    

                output$porfnocob<- renderPlotly({  ###Historico Importe
                  plot_ly(porfacnocob(), x = ~MES, y = ~PORCENTAJE, type = 'scatter', mode = 'lines', color= ~factor(ANIO)) %>%
                    layout(title = "PORCENTAJE DE NUMERO DE FACTURAS PENDIENTE DE COBRAR", showlegend = TRUE,xaxis = list(range= c(1:12),title = "Meses"),
                           yaxis = list(title = "%"),
                           legend = list(orientation = 'v',font = list(size = 9)))
                })
                  
                
                porfacnocob<- reactive({
                  recuperados1=evacobage()[which(evacobage()$FALTA_FAC>="2018-01-01" & evacobage()$DIAS_CRED>0 & evacobage()$COBTOT==1),]
                  basecobfin1=ddply(recuperados1[],.(MES=MESPROMPAG,ANIO=ANOPROMPAG),summarize,COBTOT=sum(COBTOT))
                  
                  recuperados2=evacobage()[which(evacobage()$FALTA_FAC>="2018-01-01" & evacobage()$DIAS_CRED>0 & evacobage()$COBREAL==1),]
                  basecobfin2=ddply(recuperados2[],.(MES=MESPROMPAG,ANIO=ANOPROMPAG),summarize,COBREAL=sum(COBREAL))
                  basecobfin=merge(x=basecobfin2,basecobfin1,all.x=TRUE)
                  basecobfin['PORCENTAJE']=round(((basecobfin['COBTOT']-basecobfin['COBREAL'])/basecobfin['COBTOT'])*100,2)
                  basecobfin=basecobfin[order(basecobfin$ANIO,basecobfin$MES),]
                  basecobfin
                  })
                
                output$pormnocob<- renderPlotly({  ###Historico Importe
                  plot_ly(m_porfacnocob(), x = ~MES, y = ~PORCENTAJE, type = 'scatter', mode = 'lines', color= ~factor(ANIO)) %>%
                    layout(title = "PORCENTAJE DE MONTO PENDIENTE DE COBRAR", showlegend = TRUE,xaxis = list(range= c(1:12),title = "Meses"),
                           yaxis = list(title = "%"),
                           legend = list(orientation = 'v',font = list(size = 9)))
                  
                })

                m_porfacnocob<- reactive({
                  recuperados1=evacobage()[which(evacobage()$FALTA_FAC>="2018-01-01" & evacobage()$DIAS_CRED>0 & evacobage()$COBTOT==1),]
                  basecobfin1=ddply(recuperados1[],.(MES=MESPROMPAG,ANIO=ANOPROMPAG),summarize,COBTOT=sum(CANTIDAD))
                  
                  recuperados2=evacobage()[which(evacobage()$FALTA_FAC>="2018-01-01" & evacobage()$DIAS_CRED>0 & evacobage()$COBREAL==1),]
                  basecobfin2=ddply(recuperados2[],.(MES=MESPROMPAG,ANIO=ANOPROMPAG),summarize,COBREAL=sum(CANTIDAD))
                  basecobfin=merge(x=basecobfin2,basecobfin1,all.x=TRUE)
                  basecobfin['PORCENTAJE']=round(((basecobfin['COBTOT']-basecobfin['COBREAL'])/basecobfin['COBTOT'])*100,2)
                  basecobfin=basecobfin[order(basecobfin$ANIO,basecobfin$MES),]
                  basecobfin
                })
                
                ###########################################################
                ####################  Ciclo de Compra  ###########################
                ###########################################################
                
                
                #################### Resumen siclo de compra ##################
                
          
                #### Info Boxes #####
                
                output$reqpen<-renderInfoBox({
                  a=docpend[which(docpend$TIPO_DOC=="REQ"),]
                  requipend = colSums(a[,3,drop=FALSE])
                  requipend = prettyNum(requipend,big.mark=",",scientific=FALSE)
                  infoBox("REQ Pendiente",requipend,width=3,color="yellow")
                })

                output$ocpen<-renderInfoBox({
                  a=docpend[which(docpend$TIPO_DOC=="OC"),]
                  ordcomppend = colSums(a[,3,drop=FALSE])
                  ordcomppend = prettyNum(ordcomppend,big.mark=",",scientific=FALSE)
                  infoBox("OC Pendiente",ordcomppend,width=3,color="green")
                })

                output$rempen<-renderInfoBox({
                  a=docpend[which(docpend$TIPO_DOC=="REM"),]
                  remipend = colSums(a[,3,drop=FALSE])
                  remipend = prettyNum(remipend,big.mark=",",scientific=FALSE)
                  infoBox("Requisicion Pendiente",remipend,width=3,color="light-blue")
                })
                

                
                ################# histograma dias trans de Req a OC ############

                reqclc<-reactive({ #Grafica de pastel
                  reqclc=diasavg[which(diasavg$TIPO_DOC=="REQ" & diasavg$ANIODOC==input$anioclc & diasavg$MESDOC==input$mesclc),]
                  reqclc=reqclc[order(reqclc$DIAS_RQ_OC),]
                })
                
                output$numeroreq<-renderText({
                  a=diasavg[which(diasavg$TIPO_DOC=="REQ" & diasavg$ANIODOC==input$anioclc & diasavg$MESDOC==input$mesclc),]
                  reqnum = nrow(a)
                  reqnum = prettyNum(reqnum,big.mark=",",scientific=FALSE)
                  reqnum<-paste0("Numero total de Requisiciones: ",as.character(reqnum))
                })
                                
                output$diasreqhgp<- renderPlotly({  ###Participacion en ventas
                  #hist(vcphgp()$IMPORTE,breaks=6)
                  plot_ly(x = ~reqclc()[,15], type = "histogram") %>% 
                    layout(barmode = "overlay",
                           xaxis = list(title = "Dias de Req a OC",
                                        zeroline = FALSE),
                           yaxis = list(title = "Numero de Rquisiciones",
                                        zeroline = FALSE))
                  #plot_ly(x= vcphgp()[,2], type = "histogram")%>%layout(legend = list(orientation = 'r',font = list(size = 11)))
                })
                  

                ##### Tabla de Dias de conversión de documento REQ a OC  ##########
                
                output$diasreq<- DT::renderDataTable({
                  diasreq=diasavg[which(diasavg$TIPO_DOC=="REQ"),]
                  diasreq<-dcast(diasreq,ANIODOC~MESDOC,mean,value.var="DIAS_RQ_OC")
                  diasreq<-round(diasreq,0)
                  diasreq<- replace(diasreq, is.na(diasreq), 0)
                  DT::datatable(diasreq,
                                extensions = 'Buttons', 
                                options = list(lengthMenu = list(c(10,20,50,100, -1), list('10', '20', '50', '100', 'All')),pageLength = 10, autoWidth = FALSE,scrollX = TRUE,dom = 't'))
                }) 
                
                
                ################# histograma dias trans de OC a REM ############
                
                occlc<-reactive({ #Grafica de pastel
                  occlc=diasavg[which(diasavg$TIPO_DOC=="OC" & diasavg$ANIODOC==input$anioclc & diasavg$MESDOC==input$mesclc),]
                  occlc=occlc[order(occlc$DIAS_OC_RF),]
                })
                
                output$numerooc<-renderText({
                  a=diasavg[which(diasavg$TIPO_DOC=="OC" & diasavg$ANIODOC==input$anioclc & diasavg$MESDOC==input$mesclc),]
                  ocnum = nrow(a)
                  ocnum = prettyNum(ocnum,big.mark=",",scientific=FALSE)
                  ocnum<-paste0("Numero total de Oredenes de compra: ",as.character(ocnum))
                })
                
                output$diasochgp<- renderPlotly({  ###Participacion en ventas
                  #hist(vcphgp()$IMPORTE,breaks=6)
                  plot_ly(x = ~occlc()[,3], type = "histogram") %>% 
                    layout(barmode = "overlay",
                           xaxis = list(title = "Dias de Req a OC",
                                        zeroline = FALSE),
                           yaxis = list(title = "Numero de Rquisiciones",
                                        zeroline = FALSE))
                  #plot_ly(x= vcphgp()[,2], type = "histogram")%>%layout(legend = list(orientation = 'r',font = list(size = 11)))
                })
                
                
                ##### Tabla de Dias de conversión de documento REQ a OC  ##########
                
                output$diasoc<- DT::renderDataTable({
                  diasoc=diasavg[which(diasavg$TIPO_DOC=="OC"),]
                  diasoc<-dcast(diasoc,ANIODOC~MESDOC,mean,value.var="DIAS_OC_RF")
                  diasoc<-round(diasoc,0)
                  diasoc<- replace(diasoc, is.na(diasoc), 0)
                  DT::datatable(diasoc,
                                extensions = 'Buttons', 
                                options = list(lengthMenu = list(c(10,20,50,100, -1), list('10', '20', '50', '100', 'All')),pageLength = 10, autoWidth = FALSE,scrollX = TRUE,dom = 't'))
                }) 

                ################# histograma dias trans de REQ a REM ############
                
                totclc<-reactive({ #Grafica de pastel
                  totclc=diasavg[which(diasavg$TIPO_DOC=="COM" & diasavg$ANIODOC==input$anioclc & diasavg$MESDOC==input$mesclc),]
                  totclc=totclc[order(totclc$DIAS_OC_RF),]
                })
                
                output$numerotot<-renderText({
                  a=diasavg[which(diasavg$TIPO_DOC=="COM" & diasavg$ANIODOC==input$anioclc & diasavg$MESDOC==input$mesclc),]
                  totnum = nrow(a)
                  totnum = prettyNum(totnum,big.mark=",",scientific=FALSE)
                  totnum<-paste0("Numero total de Oredenes de compra: ",as.character(totnum))
                })
                
                output$diastothgp<- renderPlotly({  ###Participacion en ventas
                  #hist(vcphgp()$IMPORTE,breaks=6)
                  plot_ly(x = ~totclc()[,14], type = "histogram") %>% 
                    layout(barmode = "overlay",
                           xaxis = list(title = "Dias de Req a OC",
                                        zeroline = FALSE),
                           yaxis = list(title = "Numero de Rquisiciones",
                                        zeroline = FALSE))
                  #plot_ly(x= vcphgp()[,2], type = "histogram")%>%layout(legend = list(orientation = 'r',font = list(size = 11)))
                })
                
                ##### Tabla de Dias de conversión de documento REQ a OC  ##########
                
                output$diastot<- DT::renderDataTable({
                  diasreq=diasavg[which(diasavg$TIPO_DOC=="COM"),]
                  diasreq<-dcast(diasreq,ANIODOC~MESDOC,mean,value.var="DIAS_COMP")
                  diasreq<-round(diasreq,0)
                  diasreq<- replace(diasreq, is.na(diasreq), 0)
                  DT::datatable(diasreq,
                                extensions = 'Buttons', 
                                options = list(lengthMenu = list(c(10,20,50,100, -1), list('10', '20', '50', '100', 'All')),pageLength = 10, autoWidth = FALSE,scrollX = TRUE,dom = 't'))
                }) 
                
                ##### Tabla detalle de documentos  ##########
                
                output$tabclodoc<- DT::renderDataTable({
                  a=cicloadq[,1:41]
                  DT::datatable(a,
                                filter = 'top',
                                extensions = 'Buttons', 
                                options = list(lengthMenu = list(c(10,20,50,100, -1), list('10', '20', '50', '100', 'All')),pageLength = 10, autoWidth = FALSE,scrollX = TRUE,dom = 'lBfrtip',buttons = c('copy', 'excel')))
                })
                
                ##### Tabla detalle de documentos  ##########
                
                output$tabdiasavg<- DT::renderDataTable({
                  a=diasavg[,1:16]
                  DT::datatable(a,
                                filter = 'top',
                                extensions = 'Buttons', 
                                options = list(lengthMenu = list(c(10,20,50,100, -1), list('10', '20', '50', '100', 'All')),pageLength = 10, autoWidth = FALSE,scrollX = TRUE,dom = 'lBfrtip',buttons = c('copy', 'excel')))
                })
                
                ##### Tabla detalle de documentos  ##########
                
                output$tabdocpend<- DT::renderDataTable({
                  a=docpend[,1:5]
                  DT::datatable(a,
                                filter = 'top',
                                extensions = 'Buttons', 
                                options = list(lengthMenu = list(c(10,20,50,100, -1), list('10', '20', '50', '100', 'All')),pageLength = 10, autoWidth = FALSE,scrollX = TRUE,dom = 'lBfrtip',buttons = c('copy', 'excel')))
                }) 
  
                
                                              
}

#runApp(list(ui = ui, server = server), launch.browser = TRUE,host="192.168.16.4", port = 7776)
runApp(list(ui = ui, server = server), launch.browser = TRUE, port = 7775)
