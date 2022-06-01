#### Load libraries ----
library(shiny)

rm(list = ls())
library(igraph)
library(tidyr)
library(dplyr)
library(ggplot2)
library(plotly)
library(ggtext)
library(data.table)

####
# initialize global variable to record selected (clicked) rows
options(shiny.maxRequestSize = 1000 * 1024 ^ 2)
columns = c("x", "y", "marker", "order")
selected_points = data.frame(matrix(nrow = 0, ncol = length(columns)))
colnames(selected_points) = columns
GLOBAL_SEED = 12345
set.seed(GLOBAL_SEED)

rad2deg <- function(rad) {
  (rad * 180) / (pi)
}

ui = shinyUI(fluidPage(
  titlePanel(title = div(img(src = "hex-IntestLine.png", height = 100, width = 100),
                         "IntestLine - Digitally unroll your intestine images")),
  
  tags$head(tags$style(
    HTML("hr {border-top: 1px solid #000000;}")
  )),
  sidebarLayout(
    sidebarPanel(
      tags$head(
        tags$style(
          type = "text/css",
          "
             #loadmessage {
               position: fixed;
               top: 0px;
               left: 0px;
               width: 100%;
               padding: 5px 0px 5px 0px;
               text-align: center;
               font-weight: bold;
               font-size: 100%;
               color: #000000;
               background-color: #CCFF66;
               z-index: 105;
             }
          "
        )
      ),
      # Colors: #CCFF66
      helpText(HTML(
        #### User manual ----
        paste(
          tags$em(tags$b(
            span("User manual:", style = "color:#990099")
          )),
          tags$b(
            span("1. Define Area of interest.", style = "color:#00cc99")
          ),
          tags$b(
            span("1.1. Choose CODEX file for your analysis.", style = "color:#00cc99;margin-left:1em;")
          ),
          tags$b(
            span("1.2. Wait for the CODEX file to be uploaded.", style = "color:#00cc99;margin-left:1em;")
          ),
          tags$b(
            span('1.3. Choose the seperator for your CSV file.', style = "color:#00cc99;margin-left:1em;")
          ),
          tags$b(
            span(
              '1.4. Choose X0, Y0, A and B parameters to define region of interest',
              style = "color:#00cc99;margin-left:1em;"
            ),
            HTML(paste0(
              span("(in red)", style = "color:#ff6666"),
              span(".", style = "color:#00cc99")
            ))
          ),
          tags$b(
            span("1.5. Press on the <Select Parameters> button.", style = "color:#00cc99;margin-left:1em;")
          ),
          tags$b(
            span("1.6. Press on the <Select Area> button.", style = "color:#00cc99;margin-left:1em;")
          ),
          tags$b(
            span("1.7. Wait for the plot to show on right.", style = "color:#00cc99;margin-left:1em;")
          ),
          tags$b(
            span("1.8. Adjust parameters and follow steps 1.4-1.7.", style = "color:#00cc99;margin-left:1em;")
          ),
          tags$b(
            span("1.9. Now, press on the <Analysis> button.", style = "color:#6699ff;margin-left:1em;")
          ),
          tags$b(
            span('2. Select Backbone (Outline) points based on Olfm4 or other desirible marker.', style = "color:#6699ff")
          ),
          tags$b(
            span('2.1. Wait while the "Choose marker" field loads.', style = "color:#6699ff;margin-left:1em;")
          ),
          tags$b(
            span(
              "2.2. Choose your desired Marker, Gate value and % Cell numbers to analyse and press <Choose>.",
              style = "color:#6699ff;margin-left:1em;"
            )
          ),
          tags$b(
            span("2.3. Select points for the Backbone on the plot with the mouse. It will take some time, so please be patient.", style = "color:#6699ff;margin-left:1em;")
          ),
          tags$b(
            span("2.4. Click on outlined", style = "color:#6699ff;margin-left:1em;"),
            HTML(paste0(
              span("(red) ", style = "color:#ff6666"),
              span("point again to remove it.", style = "color:#6699ff")
            ))
          ),
          tags$b(
            span(
              "2.5. You can download the outline points as a CSV file with the <Download> button.",
              style = "color:#6699ff;margin-left:1em;"
            )
          ),
          tags$b(
            span(
              "2.6. You can check your outline with the <Quality Control> button. A good outline will create a spiral going outward. ",
              
              style = "color:#6699ff;margin-left:1em;"
            )
          ),
          
          tags$b(
            span("3. Press the <Unroll> button to initiate analysis.", style = "color:#cc6699")
          ),
          tags$b(
            span(
              "3.1. You can vary the Unrolling angle according to your needs.",
              
              style = "color:#cc6699;margin-left:1em;"
            ), span("Larger angles will include more wrongly unrolled cells!", style = "color:#ff3333") 
          ),
          tags$b(
            span(
              "3.2. Select another Marker (and Outline data) and press <Choose> to reset.",
              style = "color:#cc6699;margin-left:1em;"
            )
          ),
          ###### Variants ----
          tags$b(
            span(
              "3.3. You can upload the points from previous Outline session and press <Choose>.",
              style = "color:#cc6699;margin-left:1em;"
            )
          ),
          tags$b(span("3.4. Then follow steps 2.2-3.", style = "color:#cc6699;margin-left:1em;")),
          tags$b(span("4. Vary your parameters to explore your data in depth.", style = "color:#00ccff")),
          
          sep = "<br/>"
          
        )
      )),
      #### UI ----
      hr(),
      conditionalPanel(condition = "$('html').hasClass('shiny-busy')",
                       tags$div("Loading...", id = "loadmessage")),
      fileInput(
        'codex_upload',
        'Choose CODEX file to upload (max. 1Gb)',
        accept = c('text/csv',
                   'text/comma-separated-values,text/plain',
                   '.csv')
      ),
      radioButtons(
        "separator",
        "Separator: ",
        choices = c(",", ";"),
        selected = ",",
        inline = TRUE
      ),
      
      fluidRow(
        column(
          width = 6,
          numericInput("chooseX0",
                       "Input X0:",
                       min = 0,
                       value = 15975,)
        ),
        column(
          width = 6,
          numericInput("chooseY0",
                       "Input Y0:",
                       min = 0,
                       value = 12710,)
        )
      ),
      
      fluidRow(
        column(width = 6,
               numericInput(
                 "chooseA",
                 "Input A:",
                 min = 1,
                 value = 8400
               )),
        column(width = 6,
               numericInput(
                 "chooseB",
                 "Input B:",
                 min = 1,
                 value = 8850
               ))
      ),
      
      
      fluidRow(
        column(
          width = 6,
          actionButton(
            "selectParam",
            "0. Select Parameters",
            icon = icon("sort-numeric-down"),
            style = "background:#cc66ff;color:white;"
          )
        ),
        column(width = 6,
               actionButton(
                 "selectArea", "1. Select Area", icon = icon("chart-area"),
                 style = "background:#ff6666;color:white;"
               ))
      ),
      hr(),
      
      fluidRow(column(
        width = 12,
        offset = 4,
        actionButton(
          "uploadData",
          "2. Analysis",
          icon = icon("hubspot"),
          style = "background:#00cc99;color:white;"
        )
      )),
      hr(),
      #selectizeInput("selectMarker", "Choose marker", choices = "Upload Data!"),
      sliderInput(
        "chooseGate",
        "Select a Marker strength value more than:",
        min = 0,
        max = 4,
        value = 0.4,
        step = 0.1
      ),
      
      sliderInput(
        "percentCells",
        "Select % Cells to visualize:",
        min = 10,
        max = 100,
        value = 50,
        step = 10
      ),
      
      fluidRow(
        column(width = 6,
               selectizeInput("selectMarker", "Choose marker", choices = "Upload Data!")),
        column(width = 6,
               numericInput("selectMaxAngle", "Unrolling angle:", value = 7, min = 0, max = 50))
      ),
      fluidRow(column(
        width = 12,
        offset = 4,
        actionButton(
          "chooseMarker",
          "3. Choose",
          icon = icon("hand-point-up"),
          style = "background:#6699ff;color:white;"
        )
      )),
      
      hr(),
      fileInput(
        'outline_upload',
        'Choose Outline file to upload (max. 1Gb)',
        accept = c('text/csv',
                   'text/comma-separated-values',
                   '.csv')
      )
      
    ),
    #### Main panel ----
    mainPanel(
      hr(),
      # hr(),
      # h5(HTML(
      #   paste(
      #     "Credit: Dr. Jiangyan Yu and Altay Yuzeir",
      #     'Department of Quantitative Systems biology at LIMES Institute in Bonn, Germany',
      #     "Under the leadership of Prof. Dr. Andreas Schlitzer, April 2022",
      #     sep = "<br/>"
      # 
      #   )
      # ), align = "center"),
      # hr(),
      h5(HTML(
        paste(
          tags$b(span("Please start your Backbone (Outline) point selection starting from the center and go outward.", style = "color:red;font-size:18;")),
          #'A minimum number of outline points must be selected for effective analysis.',
          sep = "<br/>"
          
        )
      ), align = "center"),
      hr(),
      plotOutput("plot", click = "clicked"),
      fluidRow(
        column(
          width = 4,
          offset = 2,
          actionButton("qualityControl", "3.5. Quality Control",  icon = icon("clipboard-check"),
                       style = "background:#ff9966;color:white;")
        ),
        column(
          width = 4,
          offset = 2,
          downloadButton('downloadData', 'Download Backbone',
                         style = "background:#cccc00;color:white;")
          
        )
      ),
      hr(),
      
      
      plotlyOutput("quality_plot"),
      fluidRow(column(
        width = 12,
        offset = 5,
        actionButton(
          "unrollData",
          "4. Unroll",
          icon = icon("digital-ocean"),
          style = "background:#cc6699;color:white;"
        )
      )),
      hr(),
      h5(HTML(
        paste(
          "All Markers are visualized according to their strength, overlayed over all (100%) of DAPI2+ (Marker>0.0) cells.",
          sep = "<br/>"
          
        )
      ), align = "center"),
      plotOutput("line_plot"),
      fluidRow(column(
        width = 12,
        offset = 5,
        downloadButton(
          "downloadStretch",
          "5. Download Stretch",
          style = "background:#999966;color:white;"
        ))
      ),
      hr(),
      plotOutput("lost_cells"),
      #htmlOutput("lost_cells_number"),
      hr(),
      h5(HTML(
        paste(
          "Developed by: Dr. Jiangyan Yu and Altay Yuzeir",
          'Department of Quantitative Systems biology at LIMES Institute in Bonn, Germany',
          "Under the leadership of Prof. Dr. Andreas Schlitzer, April 2022",
          sep = "<br/>"
          
        )
      ), align = "center"),
      hr()
    )
  )
))
#### Server ----
server = shinyServer(function(input, output, session) {
  
  data_upload <- reactive({
    inFile <- input$codex_upload
    if (is.null(inFile))
      return(NULL)
    df <-
      fread(
        file = inFile$datapath,
        header = TRUE,
        sep = input$separator,
        data.table = F,
        check.names = T
      )
    return(df)
  })
  
  outline_upload <- reactive({
    inFile <- input$outline_upload
    if (is.null(inFile))
      return(NULL)
    df <-
      fread(
        file = inFile$datapath,
        header = TRUE,
        sep = ",",
        data.table = F,
        check.names = T
      )
    return(df)
  })
  
  #### Upload main data ----
  
  observeEvent(input$selectParam  , {
    input_image1 = data_upload()
    input_image = input_image1 
    input_x0 = input$chooseX0
    input_y0 = input$chooseY0
    
    if (isTruthy(input_x0))
    {
      x0 = input_x0
    } else {
      x0 = 0.5 * (min(input_image$x) + max(input_image$x))
    }
    
    if (isTruthy(input_y0))
    {
      y0 = input_y0
    } else{
      y0 = 0.5 * (min(input_image$y) + max(input_image$y))
    }
    
    input_A = input$chooseA
    input_B = input$chooseB
    
    if (isTruthy(input_A))
    {
      a = input_A
    } else{
      a = 1
    }
    
    if (isTruthy(input_B))
    {
      b = input_B
    } else{
      b = 1
    }
    
    output$plot = renderPlot({
      plot.new()
    })
    
    # output$quality_plot = renderPlotly({
    #   plot_ly()
    # })
    
    output$line_plot = renderPlot({
      plot.new()
    })
    
    output$lost_cells = renderPlot({
      plot.new()
    })
    
    observeEvent(input$selectArea,
                 {
                   output$plot = renderPlot({
                     plot(
                       input_image$x,
                       input_image$y,
                       cex = 0.1,
                       ylab = "y",
                       xlab = "x"
                     )
                     plotrix::draw.ellipse(
                       x = input_x0,
                       y = input_y0,
                       a = input_A,
                       b = input_B,
                       col = "red"
                     )
                     points(input_image$x,input_image$y,cex=0.1)
                     
                     
                   })
                 },
                 ignoreInit = T,
                 priority = -1)
    
  })
  
  observeEvent(input$uploadData, {
    input_image = data_upload()
    input_image1 = input_image
    selected_distance = 1
    
    DAPI_bg = data.frame()
    input_x0 = input$chooseX0
    input_y0 = input$chooseY0
    
    if (isTruthy(input_x0))
    {
      x0 = input_x0
    } else {
      x0 = 0.5 * (min(input_image$x) + max(input_image$x))
    }
    
    if (isTruthy(input_y0))
    {
      y0 = input_y0
    } else{
      y0 = 0.5 * (min(input_image$y) + max(input_image$y))
    }
    
    input_A = input$chooseA
    input_B = input$chooseB
    
    if (isTruthy(input_A))
    {
      a = input_A
    } else{
      a = 1
    }
    
    if (isTruthy(input_B))
    {
      b = input_B
    } else{
      b = 1
    }
    
    input_image$distance2center = (input_image$x - x0) ^ 2 / a ^ 2 + (input_image$y -
                                                                        y0) ^ 2 / b ^ 2
    
    input_image = filter(input_image, distance2center < selected_distance)
    
    img_marker = input_image[,c(10:(ncol(input_image)-1))]
    
    
    img_marker = select(img_marker,-any_of("Real.cells"))
    
    
    # scale columns
    img_marker_s = img_marker %>% scale() %>% as.data.frame()
    
    img_meta = input_image[, 1:9]
    
    marker_names = colnames(img_marker[, 1:ncol(img_marker)])
    
    img_meta$distance2center = (img_meta$x - x0) ^ 2 / a ^ 2 + (img_meta$y -
                                                                  y0) ^ 2 / b ^ 2
    
    all_image = cbind(img_meta, img_marker_s)
    all_image$pos = paste0(all_image$x,"_",all_image$y)
    
    
    updateSelectizeInput(session,
                         "selectMarker",
                         choices = marker_names,
                         server = T)
    
    output$plot <- renderPlot({
      plot.new()
    })
    
    output$line_plot <- renderPlot({
      plot.new()
    })
    
    # output$quality_plot = renderPlotly({
    #   plot_ly()
    # })
    
    output$lost_cells <- renderPlot({
      plot.new()
    })
    
    #### Marker ----
    observeEvent(input$chooseMarker, {
      
      
      selected_marker = input$selectMarker
      aa = outline_upload()
      
      selected_points = selected_points[0, ]
      
      if (isTruthy(aa)) {
        selected_points = aa
      } else{
        selected_points = selected_points[0, ]
      }
      
      
      gate_value = input$chooseGate
      percent_cell = input$percentCells
      
      
      test1 = cbind(x = img_meta$x,
                    y = img_meta$y,
                    marker = img_marker_s[, selected_marker]) %>% as.data.frame()
      
      test = subset(test1, marker > gate_value)
      set.seed(GLOBAL_SEED)
      test = sample_n(test, size = round(percent_cell / 100 * nrow(test)))
      test$order = c(1:nrow(test))
      
      selected <- reactive({
        # add clicked
        selected_points <<-
          rbind(
            selected_points,
            nearPoints(
              test,
              input$clicked,
              threshold = 10,
              maxpoints = 1
            )
          )
        # remove _all_ duplicates if any (toggle mode)
        # http://stackoverflow.com/a/13763299/3817004
        selected_points <<-
          selected_points[!(duplicated(selected_points) |
                              duplicated(selected_points, fromLast = TRUE)),]
        
        #str(selected_points)
        return(selected_points)
      })
      
      
      output$downloadData <- downloadHandler(
        filename = function() {
          paste(
            'backbone_outline-',
            selected_marker,
            "-",
            format(Sys.time(), "%d-%b-%Y_%Hh%Mmin"),
            '.csv',
            sep = ''
          )
        },
        content = function(file) {
          write.table(x = selected(),
                      file,
                      sep = ",",
                      row.names = F)
        }
      )
      
      output$plot <- renderPlot({
        plot.new()
      })
      
      output$line_plot <- renderPlot({
        plot.new()
      })
      
      output$lost_cells <- renderPlot({
        plot.new()
      })
      
      output$plot <- renderPlot({
        ggplot(test, aes(x = x, y = y)) +
          geom_point() +
          geom_point(data = selected(),
                     colour = "red",
                     size = 0.5)
        
      })
      
      
      #uploaded_points = selected()
      #### Quality control ----
      observeEvent(input$qualityControl,
                   {
                     output$quality_plot <- renderPlotly({
                       
                       base_points1 = selected()
                       
                       base_points1$base_order = c(1:nrow(base_points1))
                       base_points1$Outline_points = base_points1$base_order
                       
                       fig = plot_ly(
                         base_points1,
                         x = ~ x,
                         y = ~ y,
                         z = ~ Outline_points,
                         marker = list(size = 3)
                       )
                       fig <-
                         fig %>% add_markers(hovertemplate = 'x=%{x}\ny=%{y}\nz=%{z}<extra></extra>')
                       fig
                       
                       
                     })
                   },
                   ignoreInit = T,
                   priority = -2) 
      
      #### Start unrolling ----
      observeEvent(input$unrollData,
                   {
                     chosen_angle = input$selectMaxAngle #max angle
                     selected_marker = input$selectMarker
                     gate_value = input$chooseGate
                     percent_cell = input$percentCells
                     
                     output$line_plot = renderPlot({
                       
                       base_points1 = selected()
                       base_points1$base_order = c(1:nrow(base_points1))
                       
                       f = vector()
                       
                       sum = 0
                       for(i in 2:nrow(base_points1)){
                         sum = sum + sqrt((base_points1$x[i]-base_points1$x[i-1])^2 + (base_points1$y[i]-base_points1$y[i-1])^2)
                         f[i] = sum 
                       }
                       f[1] = 0
                       
                       base_points1$base_order = f
                       
                       base_points1$pos = paste0(base_points1$x, "_", base_points1$y)
                       base_points1$distance2center = (base_points1$x - x0) ^ 2 / a ^
                         2 + (base_points1$y - y0) ^ 2 / b ^ 2
                       
                       #### DAPI2 marker -----
                       
                       if(nrow(DAPI_bg) == 0){
                         
                         base_points = base_points1
                         
                         test1_DAPI = cbind(x = img_meta$x, y = img_meta$y, marker = img_marker_s[,"DAPI2"]) %>% as.data.frame()
                         
                         test_DAPI = subset(test1_DAPI, marker > 0)
                         test_DAPI$order = c(1:nrow(test_DAPI))
                         
                         test2_DAPI = cbind(img_meta, img_marker_s["DAPI2"])
                         # tmp_meta = test2
                         
                         tmp_meta_DAPI = subset(test2_DAPI,img_marker_s["DAPI2"] > 0)
                         
                         
                         tmp_meta_DAPI$pos = paste0(tmp_meta_DAPI$x,"_",tmp_meta_DAPI$y)
                         
                         # rmv base points from query list
                         query_points_DAPI = tmp_meta_DAPI[!tmp_meta_DAPI$pos %in% base_points$pos,]
                         query_points_DAPI = subset(query_points_DAPI,distance2center > min(base_points$distance2center))
                         
                         output_DAPI = query_points_DAPI[,c("x","y","z","distance2center","DAPI2")]
                         output_DAPI$nn_index = 0
                         output_DAPI$nn_dist = 0
                         output_DAPI$nearest_Row = 0
                         output_DAPI$nearest_Column = 0
                         output_DAPI$shortest_path_order = 0
                         
                         i=1
                         for(i in c(1:nrow(query_points_DAPI))){
                           f1 = base_points %>% subset(., distance2center >= query_points_DAPI[i,"distance2center"])
                           if(nrow(f1) == 0) next
                           else{
                             kd_closest_DAPI = RANN::nn2(f1[,c('x','y')],query_points_DAPI[i,c('x','y')],k=1,searchtype = "radius",radius = 5000)
                             if(kd_closest_DAPI$nn.idx == 0) next
                             else{
                               output_DAPI[i,c(6)] = kd_closest_DAPI$nn.idx
                               output_DAPI[i,c(7)] = kd_closest_DAPI$nn.dists
                               output_DAPI[i,c(8,9,10)] = f1[kd_closest_DAPI$nn.idx,c('x','y','base_order')]
                             }
                           }
                         }
                         output_DAPI = subset(output_DAPI, shortest_path_order>0)
                         
                         output_DAPI = output_DAPI[order(output_DAPI$shortest_path_order),]
                         output_DAPI_full = output_DAPI
                         output1_DAPI = output_DAPI_full
                         
                         jjj <<- output_DAPI_full
                         
                         dd1_DAPI = output_DAPI_full
                         dd1_DAPI = rename(dd1_DAPI,nearest_Row_x = nearest_Row)
                         dd1_DAPI = rename(dd1_DAPI,nearest_Column_y = nearest_Column)
                         gg1_DAPI = vector()
                         kk1_DAPI = vector()
                         for(i in 1:nrow(dd1_DAPI)){
                           dist_or_pi1_DAPI = sqrt( (x0-dd1_DAPI$x[i])^2 + (y0-dd1_DAPI$y[i])^2 )
                           
                           dist_pi_bb1_DAPI = sqrt( (dd1_DAPI$x[i]-dd1_DAPI$nearest_Row_x[i])^2 + (dd1_DAPI$y[i]-dd1_DAPI$nearest_Column_y[i])^2 )
                           
                           dist_or_bb1_DAPI = sqrt( (x0-dd1_DAPI$nearest_Row_x[i])^2 + (y0-dd1_DAPI$nearest_Column_y[i])^2 )
                           
                           cos_pi_OR_bb1_DAPI = (-dist_pi_bb1_DAPI^2 + dist_or_pi1_DAPI^2 + dist_or_bb1_DAPI^2)/(2*dist_or_pi1_DAPI*dist_or_bb1_DAPI)
                           
                           gg1_DAPI[i] = rad2deg(acos(cos_pi_OR_bb1_DAPI))
                           
                         }
                         
                         mm1_DAPI = cbind(dd1_DAPI,gg1_DAPI)
                         
                         rr1_DAPI = as.data.frame(subset(mm1_DAPI, gg1_DAPI<chosen_angle))
                         rr12_DAPI = as.data.frame(subset(mm1_DAPI, gg1_DAPI>chosen_angle))
                         
                         output1_DAPI = rr1_DAPI
                         
                         Q1_DAPI <- quantile(output1_DAPI$nn_dist, .25)
                         Q3_DAPI <- quantile(output1_DAPI$nn_dist, .75)
                         IQR_DAPI <- IQR(output1_DAPI$nn_dist)
                         
                         no_outliers_DAPI <- subset(output1_DAPI, output1_DAPI$nn_dist > (Q1_DAPI - 1.5*IQR_DAPI) & output1_DAPI$nn_dist < (Q3_DAPI + 1.5*IQR_DAPI))
                         only_outliers_DAPI <- subset(output1_DAPI, output1_DAPI$nn_dist < (Q1_DAPI - 1.5*IQR_DAPI) | output1_DAPI$nn_dist > (Q3_DAPI + 1.5*IQR_DAPI))
                         output1_DAPI = no_outliers_DAPI
                         output1_DAPI$pos = paste0(output1_DAPI$x,"_",output1_DAPI$y)
                         cc <<- merge(output1_DAPI, all_image, by = "pos", no.dups = T, suffixes = c("","1"))
                         
                         
                         vv_DAPI <<- rr12_DAPI
                         pp_DAPI <<- output_DAPI_full
                         ss_DAPI <<- only_outliers_DAPI
                         DAPI_bg <<- cc
                         DAPI_bg_down <<- select(DAPI_bg, -any_of(c("DAPI21", "x1", "y1", "z1", "distance2center1")))
                         DAPI_bg_down <<- rename(DAPI_bg_down, angle_origin = gg1_DAPI)
                         
                       }else{
                         #DAPI_bg = jjj
                         
                         dd1_DAPI = jjj
                         dd1_DAPI = rename(dd1_DAPI,nearest_Row_x = nearest_Row)
                         dd1_DAPI = rename(dd1_DAPI,nearest_Column_y = nearest_Column)
                         gg1_DAPI = vector()
                         kk1_DAPI = vector()
                         for(i in 1:nrow(dd1_DAPI)){
                           dist_or_pi1_DAPI = sqrt( (x0-dd1_DAPI$x[i])^2 + (y0-dd1_DAPI$y[i])^2 )
                           
                           dist_pi_bb1_DAPI = sqrt( (dd1_DAPI$x[i]-dd1_DAPI$nearest_Row_x[i])^2 + (dd1_DAPI$y[i]-dd1_DAPI$nearest_Column_y[i])^2 )
                           
                           dist_or_bb1_DAPI = sqrt( (x0-dd1_DAPI$nearest_Row_x[i])^2 + (y0-dd1_DAPI$nearest_Column_y[i])^2 )
                           
                           cos_pi_OR_bb1_DAPI = (-dist_pi_bb1_DAPI^2 + dist_or_pi1_DAPI^2 + dist_or_bb1_DAPI^2)/(2*dist_or_pi1_DAPI*dist_or_bb1_DAPI)
                           
                           gg1_DAPI[i] = rad2deg(acos(cos_pi_OR_bb1_DAPI))
                           
                         }
                         
                         mm1_DAPI = cbind(dd1_DAPI,gg1_DAPI)
                         
                         rr1_DAPI = as.data.frame(subset(mm1_DAPI, gg1_DAPI<chosen_angle))
                         rr12_DAPI = as.data.frame(subset(mm1_DAPI, gg1_DAPI>chosen_angle))
                         
                         output1_DAPI = rr1_DAPI
                         
                         Q1_DAPI <- quantile(output1_DAPI$nn_dist, .25)
                         Q3_DAPI <- quantile(output1_DAPI$nn_dist, .75)
                         IQR_DAPI <- IQR(output1_DAPI$nn_dist)
                         
                         no_outliers_DAPI <- subset(output1_DAPI, output1_DAPI$nn_dist > (Q1_DAPI - 1.5*IQR_DAPI) & output1_DAPI$nn_dist < (Q3_DAPI + 1.5*IQR_DAPI))
                         only_outliers_DAPI <- subset(output1_DAPI, output1_DAPI$nn_dist < (Q1_DAPI - 1.5*IQR_DAPI) | output1_DAPI$nn_dist > (Q3_DAPI + 1.5*IQR_DAPI))
                         output1_DAPI = no_outliers_DAPI
                         output1_DAPI$pos = paste0(output1_DAPI$x,"_",output1_DAPI$y)
                         cc <<- merge(output1_DAPI, all_image, by = "pos", no.dups = T, suffixes = c("","1"))
                         
                         vv_DAPI <<- rr12_DAPI
                         pp_DAPI <<- jjj
                         ss_DAPI <<- only_outliers_DAPI
                         DAPI_bg <<- cc
                         DAPI_bg_down <<- select(DAPI_bg, -any_of(c("DAPI21", "x1", "y1", "z1", "distance2center1")))
                         DAPI_bg_down <<- rename(DAPI_bg_down, angle_origin = gg1_DAPI)
                       }
                       
                       
                       #### Selected marker -----
                       
                       min_scale = round(min(cc[, selected_marker]), 2)
                       median_scale = round(median(cc[, selected_marker]), 2)
                       max_scale = round(max(cc[, selected_marker]), 2)-1
                       
                       ggplot(NULL)+
                         
                         geom_point(data = cc, aes(
                           x = shortest_path_order,
                           y = nn_dist,
                           color = eval(parse(text = selected_marker))),
                           size = 0.5
                         ) +
                         labs(color = selected_marker) +
                         
                         scale_colour_gradientn(colors = c( "pink", "red", "yellow"),
                                                breaks = c(
                                                  #min_scale, 
                                                  median_scale, max_scale
                                                ))+
                         
                         ylab("Thickness (serosa-luminal axis)")+xlab("Outline length (proxima-distal axis)")+
                         labs(title = paste0("Digitally unrolled for ", selected_marker," marker"))+
                         theme(plot.title = element_text(hjust = 0.5))+
                         theme(
                           axis.line = element_line(color = 'black'),
                           panel.background = element_rect(fill = 'white', color = 'black'),
                           panel.grid.major = element_blank(),
                           panel.grid.minor = element_blank(),
                           panel.border = element_blank()
                         ) 
                       
                     })
                     
                     output$lost_cells = renderPlot({
                       
                       jj = paste0("You have lost <b><span style=\"color:red\">",
                                   nrow(vv_DAPI),
                                   "</span></b> out of <b>",
                                   nrow(pp_DAPI),
                                   " total DAPI2 cells due to Unrolling angle.")
                       
                       jj2 = paste0("Additionally, you have lost <b><span style=\"color:orange\">",
                                    nrow(ss_DAPI),
                                    "</span></b> out of <b>",
                                    nrow(pp_DAPI),
                                    "</b> total DAPI2 cells due to Outlier removal.")
                       
                       ggplot(pp_DAPI, aes(x = x, y = y)) +
                         labs(title = jj, subtitle = jj2) +
                         theme(plot.title = element_markdown(hjust = 0.5),
                               plot.subtitle = element_markdown(hjust = 0.5)) +
                         theme(
                           axis.line = element_line(color = 'black'),
                           panel.background = element_rect(fill = 'white', color = 'black'),
                           panel.grid.major = element_blank(),
                           panel.grid.minor = element_blank(),
                           panel.border = element_blank()
                         ) +
                         geom_point() +
                         geom_point(data = vv_DAPI,
                                    colour = "red",
                                    size = 0.5) +
                         geom_point(data = ss_DAPI,
                                    colour = "orange",
                                    size = 0.5) +
                         ylab("CODEX coordinate y") + 
                         xlab("CODEX coordinate x")
                       
                     })
                     
                     output$downloadStretch <- downloadHandler(
                       filename = function() {
                         paste(
                           'stretch-',
                           "DAPI2_based",
                           "-",
                           chosen_angle,
                           "_degree_angle-",
                           format(Sys.time(), "%d-%b-%Y_%Hh%Mmin"),
                           '.csv',
                           sep = ''
                         )
                       },
                       content = function(file) {
                         write.table(x = DAPI_bg_down,
                                     file,
                                     sep = ",",
                                     row.names = F)
                       }
                     )
                     
                     
                   },
                   ignoreInit = T,
                   priority = -1)
      
    })
    
  })
  
})



shinyApp(ui, server)