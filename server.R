
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
# 
# http://www.rstudio.com/shiny/
#
## try http:// if https:// URLs are not supported
#source("https://bioconductor.org/biocLite.R")
#biocLite("tkWidgets")
#shiny::runApp(port = 7775,host = '192.168.1.24',display.mode = 'showcase')
library(shiny)
library(cluster)
library(ggplot2)
library(Cairo) 
library(GGally)
library(Hmisc)
library(pastecs)
library(psych)
library(corrgram)
library(corrplot)
library(caret)
library(rpart.plot)
library(e1071)
library(kernlab)
library(pROC)
library(gdata)

#library(rattle)
#install.packages("rattle")


#library(tkWidgets)
#options(shiny.trace=FALSE)
# By default, the file size limit is 5MB. It can be changed by
# setting this option. Here we'll raise limit to 9MB.
options(shiny.maxRequestSize = 100*1024^2)


shinyServer(
  function(input, output, session) {
    par(bg=NA)
    
    ##################################################
    #                 INICIO GRAFICOS               #
    #################################################
    ranges <- reactiveValues(x = NULL, y = NULL)
    
    output$scatter_plot <- renderPlot({
      ggplot(readfile(), aes_string(input$x_col, input$y_col)) +
        geom_point(aes_string(colour = input$color_scatter)) +
        coord_cartesian(xlim = ranges$x, ylim = ranges$y)+ 
        ggtitle(input$scatter_titulo)+
        xlab(input$scatter_xlab) +
        ylab(input$scatter_ylab) 
    })
    
    # When a double-click happens, check if there's a brush on the plot.
    # If so, zoom to the brush bounds; if not, reset the zoom.
    observeEvent(input$scatter_plot_dblclick, {
      brush <- input$scatter_plot_brush
      if (!is.null(brush)) {
        ranges$x <- c(brush$xmin, brush$xmax)
        ranges$y <- c(brush$ymin, brush$ymax)
        
      } else {
        ranges$x <- NULL
        ranges$y <- NULL
      }
    })
   
      
    
    output$x_col <- renderUI({
      #if(is.null(input$variables)){ 
      colnames <- varlist(readfile(),type="numeric")
      #}else{
      # colnames <-input$variables
      #}
      selectInput('x_col', 'Variable del eje X', colnames,colnames[1])
     
      
    })
    output$y_col <- renderUI({
      #if(is.null(input$variables)){ 
      colnames <- varlist(readfile(),type="numeric")
      #}else{
      # colnames <-input$variables
      #}
      
      selectInput('y_col', 'Variable del eje Y', colnames,colnames[2])
      
    })
    
    output$scatter_xlab <- renderUI({
      textInput("scatter_xlab", "Etiqueta para el eje X", input$x_col)
    })
    output$scatter_ylab <- renderUI({
      textInput("scatter_ylab", "Etiqueta para el eje Y", input$y_col)
    })
    output$color_scatter <- renderUI({
      #if(is.null(input$variables)){ 
      colnames <- varlist(readfile(),type="factor")
      #}else{
      # colnames <-input$variables
      #}
      
      selectInput('color_scatter', 'Seleccione una variable para el color', colnames)
      
    })
    ##scatter matrix
    
    
    
    output$matrix_menu <- renderUI({
      colnames <- names(readfile())
    selectizeInput("matrix_vars", "Seleccione las variables para visualizar:",
                   colnames,multiple = TRUE,options = list(maxItems = 3))
    })
    
    output$color_matrix <- renderUI({
      #if(is.null(input$variables)){ 
      colnames <- varlist(readfile(),type="factor")
      #}else{
      # colnames <-input$variables
      #}
      
      selectInput('color_matrix', 'Seleccione una variable para el color', colnames)
      
    })
    
    output$matrix_plot <- renderPlot({
      
      data<-readfile()[input$matrix_vars]
        
      
      
      #color=as.name(input$color) 
      #data[,color]<-as.factor(data[,color])
      #
      if(length(data)>0){
        ###OPCIONAL
        if(!input$color_matrix  %in%  input$matrix_vars){
          data<-cbind(data,readfile()[input$color_matrix])
        }
        ###opcional
        data[,input$color]<-as.factor(data[,input$color_matrix])
        #
        if(length(data)>1){
          # list(continuous = wrap("density", alpha = 0.5), combo = "box")
          if(input$matrix_advance==2){
        
                if(input$show_lower == 1){
                    lower = list( continuous = input$cont_lower, combo = input$combo_lower,discrete=input$discrete_lower)
                }else{
                  lower = "blank"
                }
                  
                if(input$show_upper == 1){
                    upper = list( continuous = input$cont_upper, combo = input$combo_upper,discrete=input$discrete_upper)
                }else{
                  upper = "blank"
                }
                if(input$show_diag == 1){
                    diag = list( continuous = input$cont_diag,discrete=input$discrete_diag)
                }else{
                  diag = "blank"
                }
                
                ggpairs(data, 
                        lower = lower,
                        upper = upper,
                        diag = diag,
                        mapping = ggplot2::aes_string(color = input$color_matrix))#color='Species', alpha=0.4)
          }else{
                ggpairs(data, 
                    mapping = ggplot2::aes_string(color = input$color_matrix))#color='Species', alpha=0.4)
                
          }
        }
      }
      
    })
   
    
 
    
    ##################################################
    #                 FIN GRAFICOS               #
    #################################################
  
  readfile<-eventReactive(input$leerArchivo,{
    inFile<-input$file1
    
    if (is.null(inFile))
      return(NULL)
    
    if(input$auto_reading==TRUE){
    
        separador=input$sep
        
        if(separador=='otro'){
          separador=input$otro_sep
        }
       
         data<-read.csv(inFile$datapath, header = input$header,
                 sep = separador, quote = input$quote,dec = input$decimal,stringsAsFactors=input$string_as_factors)
         
    }else{
      metadata = getFileMetaData(inFile$datapath, numLine = 200, isFile = TRUE)
      sepa = metadata$separator
      
      #print(metadata)
      headera = metadata$header
      print(sepa)
      print(headera)
      data<-read.csv(inFile$datapath, 
               header = headera,
               sep = sepa,
               quote = '"',
               dec = '.',
               stringsAsFactors=TRUE)
      trim(data)
      
      }
   }
    
    
    )
  
  output$contents <- renderDataTable({
    # input$file1 will be NULL initially. After the user selects
    # and uploads a file, it will be a data frame with 'name',
    # 'size', 'type', and 'datapath' columns. The 'datapath'
    # column will contain the local filenames where the data can
    # be found.
    
     data<- readfile() 
     if(!(is.null(readfile()))){
       #if(input$vars_edit==TRUE){
       if(!is.null(input$variables)){
         data<-data[input$variables]
         colnames <- names(data)
         colname_alter = c()
         
         for(i in 1:ncol(data)){
           nam = paste0("ren-",colnames[i])
           type<-paste0("tipo-",colnames[i])
           if(!(is.null(nam)|| nam=='ren-')){
           colname_alter<-c(colname_alter,input[[nam]])
           }
         }
         
         if(length(colname_alter)==length(colnames)){
          print(colname_alter)
          names(data)<-colname_alter
         }
         
         #readfile()<-data
       }
       #}
     }
     
     head(data, 20)
   #head(data, 20)
  }, options = list(scrollX = TRUE , pageLength = 10))
  
  output$VarControls <- renderUI({
      #if(is.null(input$variables)){ 
    colnames <- names(readfile())
    #}else{
     # colnames <-input$variables
    #}
    
    cbg<-checkboxGroupInput("variables", "", choices = colnames,selected = colnames,inline = FALSE,width = NULL)
    #print (cbg)
    cbg
    
  })
  output$VarControls_rename <- renderUI({
    colnames <- names(readfile())
    #cbg<-checkboxGroupInput("variablesXXX", "", choices = colnames,selected = colnames,inline = FALSE,width = NULL)
    #print(list(colnames))
    textIn <- list()
    #print (colnames[1])
  for(i in 1:ncol(readfile())){
    #print (colnames[i])
    ti<-textInput(inputId = paste0("ren-",colnames[i]),label=colnames[i],value = colnames[i],width = NULL)
    var_type<-ifelse(is.numeric(readfile()[,colnames[i]]),'numeric','factor')
    si<-selectizeInput(inputId = paste0("tipo-",colnames[i]), label=NULL, choices=c('Tipo Numérico'='numeric','Tipo Categórico'='factor'), selected = var_type, multiple = FALSE,options = NULL)
    #cbi<-checkboxInput(inputId = paste0("incluir-",colnames[i]), label=colnames[i], value = TRUE, width = NULL)
    #print(ti)
    #print(si)
    #print((typeof()))
    #textIn <- list(textIn, list(cbi))
     
    textIn <- list(textIn, list(ti))
    textIn <- list(textIn, list(si))
    textIn <- list(textIn, list(tags$hr()))
    
   
  }
    tagList(
    textIn
    )
  
  })
  
  output$groupby <- renderUI({
    colnames <- input$variables
    checkboxGroupInput("groupby", "Campos de Agrupación", choices = input$variables)
    
  })
  
  output$aggregationColumns <- renderUI({
    
    checkboxGroupInput("aggregationColumns", "Columnas a Calcular", choices = input$variables)
    
  })
  
  output$aggMethods <- renderUI({
    colnames <- input$variables
    checkboxGroupInput("aggMethods", "Selecciona la funcion de Agregación", choices = c('Sum','Mean','SD','Count'))
    
  })
  
  output$contents_pre<-renderDataTable({
    data<- readfile()
    data<-data[input$variables]
    head(data, 20)
    
  }, options = list(scrollX = TRUE, pageLength = 10))
  
  output$summary_vars<-renderDataTable({
   if(!(is.null(readfile()))){
     if(input$summary_method=='basico'){
       data_render<-readfile()
       #flag = c(paste(sep = "",'<img src="', 'shared/tempImages/',basename(generateImage(data_render[2])),'" height="200"></img>'),
       #        '<img src="http://upload.wikimedia.org/wikipedia/commons/thumb/f/fa/Flag_of_the_People%27s_Republic_of_China.svg/200px-Flag_of_the_People%27s_Republic_of_China.svg.png" height="52"></img>',
       #         '<img src="http://upload.wikimedia.org/wikipedia/commons/thumb/f/fa/Flag_of_the_People%27s_Republic_of_China.svg/200px-Flag_of_the_People%27s_Republic_of_China.svg.png" height="52"></img>'
      # )
       summ<-summary(readfile())
       #cbind(summ,flag)
     }else{
       #if(input$summary_method=='hmisc'){
         #Hmisc::describe(readfile())
      # }else{
       
         if(input$summary_method=='pastecs'){
           summ <- t(stat.desc(readfile())) 
           summ <- cbind(Variable = rownames(summ), summ)
           
         }else{
           if(input$summary_method=='psych'){
             summ<-psych::describe(readfile())
             summ <- cbind(Variable = rownames(summ), summ)
             summ
             }
       #  }
       }
     }
    }
  }, options = list(scrollX = TRUE,pageLength = 10,bFilter=0, bSort=0, bProcessing=0, bPaginate=0, bInfo=0),escape = FALSE)
  
  
  correlations_chart<-eventReactive(input$do_correlation,{
    data<-readfile()
    par(bg=NA)
    par(c(5.1, 4.1, 6.1, 2.1))
    #corrgram(data, order=TRUE, lower.panel=panel.shade,
     #        upper.panel=panel.pie, text.panel=panel.txt,
             #main="Correlaciones")
    colnames <- varlist(readfile(),type="numeric")
    M<-cor(data[colnames])
    col <- colorRampPalette(c("#BB4444", "#EE9988", "#FFFFFF", "#77AADD", "#4477AA"))
    corrplot(M, method="color", col=col(200),  
             type="upper", order="hclust", 
             addCoef.col = "black", # Add coefficient of correlation
             tl.col="black", tl.srt=45, #Text label color and rotation
             # Combine with significance
             #p.mat = p.mat, sig.level = 0.01, insig = "blank", 
             # hide correlation coefficient on the principal diagonal
            tl.pos = "lt",
             diag=FALSE ,
             bg = "transparent"
             
            # title="Correlaciones de Variables"
    )
    
  })
  output$out_plot_correlations <- renderPlot({
    correlations_chart()
  },bg="transparent")
  
  
  na_fix<-eventReactive(input$do_na_fix,{
    string_fix = input$string_na
    numeric_fix = input$numeric_na
    data <- readfile()
    
    if(string_fix != 'nothing' | numeric_fix != 'nothing'){
        
        colnames_string <- varlist(data,type="factor")
        colnames_char <- varlist(data,type="character")
        colnames_numeric <- varlist(data,type="numeric")
        
        if(string_fix == 'erase_row'){
          remove_nas<-which(complete.cases(data[colnames_string]))
          data<-data[remove_nas,]
          remove_nas<-which(complete.cases(data[colnames_char]))
          data<-data[remove_nas,]
        }
        
        if(numeric_fix == 'erase_row'){
          remove_nas<-which(complete.cases(data[colnames_numeric]))
          data<-data[remove_nas,]
        }
    }
    data
  })
  
  output$na_fix_table<-renderDataTable({
    na_fix()
  }, options = list(scrollX = TRUE, pageLength = 10))
  
    
  
    
  
  
  
  
  output$summary<-renderDataTable({
    applyAggreg()
  }, options = list(scrollX = TRUE, pageLength = 10))
  
  applyAggreg<-eventReactive(input$ApplyAgg,{
    sALL(readfile(),input)
  })
  
  output$ModelVarControls <- renderUI({
    if(is.null(input$aggMethods)){ 
        colnames <-input$variables
      }else{
        colnames <-names(applyAggreg())
      }
    checkboxGroupInput("model_variables", "Seleccione las variables que serán utilizadas en el Modelo a entrenar", choices = colnames,selected = colnames)
  })
  
  transform_model_data<-eventReactive(input$do_model_var_transform,{
    data=''
    if(is.null(input$aggMethods)){ 
      data<- readfile()
    }else{
      data <- applyAggreg()
    }
    data = data[input$model_variables]
    
    if(input$nulos==TRUE){  
      data<-data[complete.cases(data),]      
    }
    if(input$scale_on==TRUE){  
       data<-scale(data)        
    }
    as.data.frame(data)
  })
  
  output$modified_final_vars<-renderDataTable({
    head(transform_model_data(),5)
  }, options = list(scrollX = TRUE , pageLength = 10))
  
  
  cluster_test<-eventReactive(input$do_cluster_test,{
    set.seed(-1785)
    number_clusters_validation_I <- input$number_clusters_validation[1]
    number_clusters_validation_F <- input$number_clusters_validation[2]
    data <-transform_model_data()
    #print(data)
    subset<-data[input$model_variables]
   # print("Entrando a cluster")
    #print(subset)
    wss <- (nrow(subset)-1)*sum(apply(subset,2,var))
    for (i in number_clusters_validation_I:number_clusters_validation_F) wss[i] <- sum(kmeans(subset, 
                                                                 centers=i)$withinss)
    plot(1:number_clusters_validation_F, wss, type="b", xlab="Number of Clusters",
         ylab="Within Groups Sum of Squares")
    #print("saliendo de cluster")
  })
  
  output$numberOfClusters<-renderPlot({
    cluster_test()
  })
  
  cluster_generate<-eventReactive(input$do_cluster,{
    set.seed(-1785) 
    subset<-transform_model_data()
    fit <- kmeans(subset, input$number_centers)
   })
  
  #plot cluster fitted
  
  output$centers <- renderDataTable({
    fit<-cluster_generate()
    fit$centers
  }, options = list(scrollX = TRUE,pageLength = 10))
    #Plot Cluster distribution
  output$clusters<-renderPlot({
    fit<-cluster_generate()
    par(mar = c(5, 4, 6, 2))
    height <- table(fit$cluster)
    mp <- barplot(height, main = "Clusters Distribution",xlab="Cluster",ylab="Number of Rows")
    text(mp, height, labels = format(height, 2), pos = 1, cex = 0.9)
  })
    #Creating fitted cluster plot
  output$fitCluster<-renderPlot({
    subset<-transform_model_data()
    fit<-cluster_generate()
    clusplot(subset, fit$cluster, color=TRUE, shade=TRUE, 
             labels=4, lines=0)
  })
  
  output$out_class_var <- renderUI({
    #if(is.null(input$variables)){ 
    colnames <- varlist(readfile(),type="factor")
    #}else{
    # colnames <-input$variableslibrary
    #}
    
    selectInput('class_var_out', 'Seleccione la variable Objetivo del modelo', colnames,multiple = FALSE)
    
  })
  ##Variable clasificacion arbol
  
  output$choose_columns_classify <- renderUI({
    # If missing input, return to avoid error later in function
    if(is.null(input$file1))
      return()
    
    # Get the data set with the appropriate name
    dat <- readfile()
    colnames <- names(dat)
    
    # Create the checkboxes and select them all by default
    checkboxGroupInput("predict_columns", "Seleccione las variables predictoras", 
                       choices  = colnames,
                       selected = colnames)
  })
  
  #predict_columns
  #class_var_out
  
  classify_models<-eventReactive(input$do_classify,{
  #fitControl <- trainControl(method = "repeatedcv",
   #                          number = 3,
  #                          repeats = 2, returnResamp="all",classProbs = TRUE)
  ctr <- trainControl(method='cv',
                      number=input$train_cv, 
                      classProbs=TRUE
                      #summaryFunction=twoClassSummary 
  )
  
  set.seed(3456)
  data = readfile()
  trainIndex <- createDataPartition(data[,input$class_var_out], p = (input$train_percent/100),
                                    list = FALSE,
                                    times = 1)
  data_train = readfile()[trainIndex,]
  
  
  # Choose the features and classes
  #library(mlbench)
  #data(PimaIndiansDiabetes2)
  #x <- PimaIndiansDiabetes2[c("age","glucose","insulin","mass","pedigree","pregnant","pressure","triceps")]
  #y <- PimaIndiansDiabetes2$diabetes
  x<-subset(data_train, select = input$predict_columns)
  print(input$predict_columns)
  y<-as.factor(unlist( subset(data_train,select=input$class_var_out)))
 # y<-as.factor(y)
  # print(typeof(y))
  #  print(input$class_var_out)
  # print("Numero x y y")
  #  print(nrow(x))
  #  print(nrow(y))
  #grid <- expand.grid( .winnow = c(TRUE,FALSE), .trials=c(1,5,10,15,20), .model="tree" )
  print("modelando")
  print(nrow(data_train))
  
 # mdl<- train(x=x,y = data_train$Species,trControl=fitControl,method="rpart",verbose=FALSE)
  #mdl = train (x=x,
       #        y = data_train$Species,
    #             #method = "rpart",
         #        tuneLength=20,
           #      metric="ROC",
            #     trControl = fitControl)
 # data(iris)
  #formula <- as.formula(Species ~.)
  f <- as.formula(paste(input$class_var_out, "~", paste(input$predict_columns, collapse=" + ")))
  #print(f)
  set.seed(178509)
  withProgress(message = 'Generando Modelo De Arbol', value = 0, {
    # Number of times we'll go through the loop
    incProgress(1, detail = "...")
    
    mdl <- train(f,data_train,method = "rpart",cp=0.02,maxdepth=20,trControl = ctr)
    
  })
  
  mdl
  
  # visualize the resample distributions
  #xyplot(mdl,type = c("g", "p", "smooth"))
  
  })
  classify_models_svm<-eventReactive(input$do_classify,{
    set.seed(3456)
    data = readfile()
    trainIndex <- createDataPartition(data[,input$class_var_out], p = (input$train_percent/100),
                                      list = FALSE,
                                      times = 1)
    data_train = readfile()[trainIndex,]

  
  
  # Choose the features and classes
  #library(mlbench)
  #data(PimaIndiansDiabetes2)
  #x <- PimaIndiansDiabetes2[c("age","glucose","insulin","mass","pedigree","pregnant","pressure","triceps")]
  #y <- PimaIndiansDiabetes2$diabetes
  #x<-subset(data_train, select = input$predict_columns)
 # print(input$predict_columns)
    #y<-as.factor(unlist( subset(data_train,select=input$class_var_out)))
  # y<-as.factor(y)
    # print(typeof(y))
    # print(input$class_var_out)
    #print("Numero x y y")
    #print(nrow(x))
    #print(nrow(y))
  #grid <- expand.grid( .winnow = c(TRUE,FALSE), .trials=c(1,5,10,15,20), .model="tree" )
  print("modelando")
  print(nrow(data_train))
  # mdl<- train(x=x,y = data_train$Species,trControl=fitControl,method="rpart",verbose=FALSE)
  #mdl = train (x=x,
  #        y = data_train$Species,
  #             #method = "rpart",
  #        tuneLength=20,
  #      metric="ROC",
  #     trControl = fitControl)
  # data(iris)
  #formula <- as.formula(Species ~.)
  ctr <- trainControl(method='cv',
                      number=input$train_cv, 
                      classProbs=TRUE
                      #summaryFunction=twoClassSummary 
  )
 
  f <- as.formula(paste(input$class_var_out, "~", paste(input$predict_columns, collapse=" + ")))
  #print(f)
  set.seed(178509)
  svp.c<-''
  withProgress(message = 'Generando Modelo de SVM', value = 0, {
    # Number of times we'll go through the loop
    incProgress(1, detail = "...")
    
    #mdl <- train(f,data_train,method = "rpart",cp=0.002,maxdepth=20)
    #mod <- train(f, data=data_train, method = "svmLinear", trControl = ctrl)
    data_train[input$predict_columns]<-trim(data_train[input$predict_columns])
    svp.c <- train(f, data=data_train,  method = "svmLinear",  
                   trControl = ctr, 
                   preProcess = c('center', 'scale'),
                   metric = "ROC")
    
  })
  
  svp.c
  
  })
  
  output$out_classify_chart<-renderPlot({
    #library(rattle)
    
    print(classify_models())
    #plot(classify_models()$finalModel)
    draw_tree(classify_models()$finalModel,sub="-")
  },bg="transparent")
  output$out_classify_chart2<-renderPlot({
    #library(rattle)
    
    print(classify_models_svm())
    #plot(classify_models()$finalModel)
    #kernlab::plot(classify_models_svm()$finalModel,data = readfile())
    results <- resamples(list(TREE=classify_models(), SVM=classify_models_svm()))
    # summarize the distributions
    print(summary(results))
    # boxplots of results
    bwplot(results)
  },bg="transparent")
  
  output$trained_tree<-renderPlot({
    if(is.null(classify_models()))
        plot(classify_models())
  },bg="transparent")
  
confusionMatrixTree<-eventReactive(input$do_classify,{
    set.seed(3456)
    data = readfile()
    trainIndex <- createDataPartition(data[,input$class_var_out], p = (input$train_percent/100),
                                      list = FALSE,
                                      times = 1)
    data_test = readfile()[-trainIndex,]
    
    tree<-classify_models()
    #svm<-classify_models_svm()
    
    predictedTest_tree<-predict(tree,newdata = data_test)
    #predictedTest_svm<-predict(svm,newdata = data_test)
    
    confusionMatrix(predictedTest_tree,data_test[,input$class_var_out])
    #confusionMatrix(predictedTest_svm,data_test[,input$class_var_out])$table
    
    
  })
  
  output$confusionMatrixTree_table<-renderTable({
    confusionMatrixTree()$table
  })
  
  output$confusionMatrixTree_byClass<-renderTable({
    t(t(confusionMatrixTree()$overall))
  })
  
  
  
  confusionMatrixSVM<-eventReactive(input$do_classify,{
    set.seed(3456)
    data = readfile()
    trainIndex <- createDataPartition(data[,input$class_var_out], p = (input$train_percent/100),
                                      list = FALSE,
                                      times = 1)
    data_test = readfile()[-trainIndex,]
    
    #tree<-classify_models()
    svm<-classify_models_svm()
    
    #predictedTest_tree<-predict(tree,newdata = data_test)
    predictedTest_svm<-predict(svm,newdata = data_test)
    
    #confusionMatrix(predictedTest_tree,data_test[,input$class_var_out])
    confusionMatrix(predictedTest_svm,data_test[,input$class_var_out])
    
    
  })
  
  output$confusionMatrixSVM_table<-renderTable({
    confusionMatrixSVM()$table
  })
  
  output$confusionMatrixSVM_byClass<-renderTable({
    t(t(confusionMatrixSVM()$overall))
  })
    
  
  
  
  
  })#FINFIN
    
  
  
sALL<-function(data,input){
  
  library(doBy)
  
  if(length(input$aggMethods)  == 1){
    
    if(is.element('Sum',input$aggMethods)){
      return (summaryBy(list(input$aggregationColumns,input$groupby), data = data, 
                FUN = function(x) { c(sum = sum(x)) } ))
      
    }
    if(is.element('Mean',input$aggMethods)){
      return (summaryBy(list(input$aggregationColumns,input$groupby), data = data, 
                FUN = function(x) { c(mean = mean(x,na.rm = TRUE)) } ))
    }
    if(is.element('SD',input$aggMethods)){
      return (summaryBy(list(input$aggregationColumns,input$groupby), data = data, 
                FUN = function(x) { c(sd = sd(x,na.rm = TRUE)) } ))
    }
    if(is.element('Count',input$aggMethods)){
      return (summaryBy(list(input$aggregationColumns,input$groupby), data = data, 
                FUN = function(x) { c(count = length(x)) } ))
    }
  }else
  {
    if(length(input$aggMethods)==2 ){
      
      
      if(is.element('Sum',input$aggMethods)& is.element('Count',input$aggMethods)){
        return (summaryBy(list(input$aggregationColumns,input$groupby), data = data, 
                          FUN = function(x) { c(sum = sum(x),count = length(x)) } ))
      }
      
      if(is.element('Sum',input$aggMethods)& is.element('Mean',input$aggMethods)){
        return (summaryBy(list(input$aggregationColumns,input$groupby), data = data, 
                          FUN = function(x) { c(sum = sum(x),mean = mean(x,na.rm = TRUE)) } ))
      }
      
      if(is.element('Sum',input$aggMethods)& is.element('SD',input$aggMethods)){
        return (summaryBy(list(input$aggregationColumns,input$groupby), data = data, 
                          FUN = function(x) { c(sum = sum(x),sd = sd(x,na.rm = TRUE)) } ))
      }
      
      if(is.element('Mean',input$aggMethods)& is.element('Count',input$aggMethods)){
        return (summaryBy(list(input$aggregationColumns,input$groupby), data = data, 
                          FUN = function(x) { c(mean = mean(x,na.rm = TRUE),count = length(x)) } ))
      }
      
      if(is.element('SD',input$aggMethods)& is.element('Count',input$aggMethods)){
        return (summaryBy(list(input$aggregationColumns,input$groupby), data = data, 
                          FUN = function(x) { c(sd = sd(x,na.rm = TRUE),count = length(x)) } ))
      }
      
      if(is.element('Mean',input$aggMethods)& is.element('SD',input$aggMethods)){
        return (summaryBy(list(input$aggregationColumns,input$groupby), data = data, 
                          FUN = function(x) { c(mean = mean(x,na.rm = TRUE),sd = sd(x,na.rm = TRUE)) } ))
      }
    }else{
      if(length(input$aggMethods)==3){
        
        if(is.element('Sum',input$aggMethods) & is.element('Count',input$aggMethods) & is.element('Mean',input$aggMethods)){
          return (summaryBy(list(input$aggregationColumns,input$groupby), data = data, 
                            FUN = function(x) { c(sum = sum(x),count = length(x),mean = mean(x,na.rm = TRUE)) } ))
        }
        
        if(is.element('Sum',input$aggMethods) & is.element('Count',input$aggMethods) & is.element('SD',input$aggMethods)){
          return (summaryBy(list(input$aggregationColumns,input$groupby), data = data, 
                            FUN = function(x) { c(sum = sum(x),count = length(x),sd = sd(x,na.rm = TRUE)) } ))
        }
        
        if(is.element('Mean',input$aggMethods) & is.element('Count',input$aggMethods) & is.element('SD',input$aggMethods)){
          return (summaryBy(list(input$aggregationColumns,input$groupby), data = data, 
                            FUN = function(x) { c(mean = mean(x,na.rm = TRUE),count = length(x),sd = sd(x,na.rm = TRUE)) } ))
        }
       
        if(is.element('Mean',input$aggMethods) & is.element('Sum',input$aggMethods) & is.element('SD',input$aggMethods)){
          return (summaryBy(list(input$aggregationColumns,input$groupby), data = data, 
                            FUN = function(x) { c(sum = sum(x),mean = mean(x,na.rm = TRUE),sd = sd(x,na.rm = TRUE)) } ))
        }
      }else{
        if(length(input$aggMethods)==4){
          
          if(is.element('Sum',input$aggMethods) & is.element('Count',input$aggMethods) & is.element('Mean',input$aggMethods) & is.element('SD',input$aggMethods)){
            return (summaryBy(list(input$aggregationColumns,input$groupby), data = data, 
                              FUN = function(x) { c(sum = sum(x),count = length(x),mean = mean(x,na.rm = TRUE),sd = sd(x,na.rm = TRUE)) } ))
          }
        }else{
          
        }
      }
    }
  }
}
varlist <- function (df=NULL,type=c("numeric","factor","character"), pattern="", exclude=NULL) {
  vars <- character(0)
  if (any(type %in% "numeric")) {
    vars <- c(vars,names(df)[sapply(df,is.numeric)])
  }
  if (any(type %in% "factor")) {
    vars <- c(vars,names(df)[sapply(df,is.factor)])
  }  
  if (any(type %in% "character")) {
    vars <- c(vars,names(df)[sapply(df,is.character)])
  }  
  vars[(!vars %in% exclude) & grepl(vars,pattern=pattern)]
}

getFileMetaData <- function(file.name, numLine = 100, seps, isFile = TRUE){
  
  separator <- ""
  header <- FALSE
  sep <- NULL
  
  if(missing(seps)){
    seps <- c(",", ";", "\t"," ") #"\\t", " ")
  }#else{
  #    seps <- c(seps, ",", ";", "\t", " ")
  #}
  
  if(isFile){
    conn <- safeFileOpen(file.name)
    if(inherits(conn, "connection")){
      toCheck <- readLines(conn, n = numLine)
      close(conn)
    }else{
      stop(paste("Can't read file because", conn))
    }
  }else{
    toCheck <- file.name
  }
  
  good <- function(x) all(x==x[1]) && x[1] > 1
  
  for(i in seps){
    w <- strsplit(toCheck[2:length(toCheck)], i)
    v <- sapply(w, length)
    if(good(v)){
      sep <- i
      break
    }
  }
  if(!is.null(sep)){
    separator <- sep
    if(length(unlist(strsplit(toCheck[1], separator)))
       == length(unlist(strsplit(toCheck[2], separator))) - 1){
      header <- TRUE
      colNames <- gsub("\"", "",
                       unlist(strsplit(toCheck[1], separator)))
      skip <- 0
      rowNames <- getRowNames(file.name, separator, header, skip)
    }else{
      headerNSkip <- guess.header(toCheck[1:2], separator)
      header <- headerNSkip[["header"]]
      skip <- headerNSkip[["skip"]] 
      colNames <- headerNSkip[["colNames"]]
      rowNames <- getRowNames(file.name, separator, header, skip)
    }
    type <- find.type(file.name, separator, header, numLine)
    return(list(header = header, separator = separator, skip = skip,
                col.names = colNames, row.names = rowNames, type = type))
  }else{
    # New line is always the separator
    return(list(header = FALSE, separator = "\n", col.names = NA,
                row.names = NA, skip = 0,
                type = find.type(file.name, sep = "\n", header = FALSE)))
  }
}
getRowNames <- function(file.name, sep, header, skip){
  #data <- read.table(file.name, sep = sep, nrow = 3, header = header,
            #         skip = skip)
  data <- read.csv(file.name, sep = sep, nrow = 3, header = header,
                              skip = skip)
  return(rownames(data))
}
# A wraper of the file function that checks to see if a given file
# name does exist.

safeFileOpen <- function(fileName) {
  
  if (file.exists(fileName)){
    info <- file.info(fileName)
    if(info$isdir){
      return(paste(fileName, "is a directory"))
    }
    if(file.access(fileName, mode = 4) != 0){
      return(paste("You don't have read permission to file", fileName))
    }
    if(regexpr("\\.rda$|\\.Rda$", fileName) > 0){
      if(readLines(fileName, n = 1) == "RDX2"){
        return(paste(fileName, "is a binary file"))
      }else{
        return(file(fileName))
      }
    }
    if(regexpr("\\.gz", fileName) > 0){
      return(gzfile(fileName))
    }else if(regexpr("\\.zip", fileName) > 0){
      return(unz(fileName))
    }else if(regexpr("\\.bz2", fileName) > 0 ){
      return(bzfile(fileName))
    }else{
      return(file(fileName))
    }
  }else
    return(paste(fileName,"doest not exist"))
}
guess.header <- function(twoLines, sep){
  
  on.exit(options(warn = 1))
  
  if(!is.null(sep)){
    firstLine <- unlist(strsplit(twoLines[1], sep))
    scndLine <- unlist(strsplit(twoLines[2], sep))
  }else{
    firstLine <- twoLines[1]
    scndLine <- twoLines[2]
  }
  
  options(warn = -1)
  firstLine <- as.numeric(firstLine)
  scndLine <- as.numeric(scndLine)
  options(warn = 1)
  
  firstLine[!is.na(firstLine)] <- "num"
  scndLine[!is.na(scndLine)] <- "num"
  
  if(!setequal(firstLine, scndLine)){
    return(list(header = TRUE, skip = 1,
                colNames = gsub("\"", "",  unlist(strsplit(twoLines[1], sep)))))
  }else{
    #if(any(!is.na(firstLine))){
    #    return(list(header = FALSE, skip = 0))
    #}
    return(list(header = FALSE, skip = 0, colNames = NA))
  }
}

find.type <- function(file.name, sep, header = FALSE, numLine = 5,
                      isFile = TRUE){
  if(isFile){
    line <- as.matrix(read.csv(file.name, sep = sep, header = header,
                                 nrows = numLine, as.is = TRUE))
  }else{
    line <- as.matrix(file.name)
  }
  
  types <- NULL
  for(i in 1:nrow(line)){
    types <- rbind(types, charOrNum(line[i,]))
  }
  if(nrow(unique(types)) == 1){
    return(types[1,])
  }else{
    return(rep("Character", ncol(types)))
  }
}

charOrNum <- function(vect){
  options(warn = -1)
  temp <- as.numeric(vect)
  options(warn = 1)
  
  temp[is.na(temp)] <- "Character"
  temp[!is.na(temp) & temp != "Character"] <- "Numeric"
  return(temp)
}

##########################################################
#
# Limpieza de datos most frequent
##########################################################

statistic_clean <- function(x,method) {
  
  if(method == 'most_frecuent'){
    
    
  }
  
  if(method == 'min'){
    
    
  }
  
  if(method == 'max'){
    
    
  }
  
  if(method == 'replace_with'){
    
    
  }
  
  if (is.numeric(x)) {
    x[is.na(x)] <- mean(x, na.rm = TRUE)
    x
  } else {
    x[is.na(x)] <- names(which.max(table(x)))
    x
  }
}




##########################################################
#
# GEneracion de imagenes de sumarizacion
##########################################################

generateImage<-function(data){
  outfile <- tempfile(tmpdir = "D:\\data\\R\\win-library\\3.2\\shiny\\www\\shared\\tempImages",fileext='.png')
  
  # Generate the PNG
  png(outfile, width=400, height=300)
  hist(data, main="Generated in renderImage()")
  dev.off()
  outfile
  
  
}

draw_tree<-function (model, main = "", sub, palettes, ...) 
{
  if (missing(sub)) 
    sub <- paste("Rattle", format(Sys.time(), "%Y-%b-%d %H:%M:%S"), 
                 Sys.info()["user"])
  num.classes <- length(attr(model, "ylevels"))
  default.palettes <- c("Greens", "Blues", "Oranges", "Purples", 
                        "Reds", "Greys")
  if (missing(palettes)) 
    palettes <- default.palettes
  missed <- setdiff(1:6, seq(length(palettes)))
  palettes <- c(palettes, default.palettes[missed])
  numpals <- 6
  palsize <- 5
  pals <- c(RColorBrewer::brewer.pal(9, palettes[1])[1:5], 
            RColorBrewer::brewer.pal(9, palettes[2])[1:5], RColorBrewer::brewer.pal(9, 
                                                                                    palettes[3])[1:5], RColorBrewer::brewer.pal(9, palettes[4])[1:5], 
            RColorBrewer::brewer.pal(9, palettes[5])[1:5], RColorBrewer::brewer.pal(9, 
                                                                                    palettes[6])[1:5])
  if (model$method == "class") {
    yval2per <- -(1:num.classes) - 1
    per <- apply(model$frame$yval2[, yval2per], 1, function(x) x[1 + 
                                                                   x[1]])
  }
  else {
    per <- model$frame$yval/max(model$frame$yval)
  }
  per <- as.numeric(per)
  if (model$method == "class") 
    col.index <- ((palsize * (model$frame$yval - 1) + trunc(pmin(1 + 
                                                                   (per * palsize), palsize)))%%(numpals * palsize))
  else col.index <- round(per * (palsize - 1)) + 1
  col.index <- abs(col.index)
  if (model$method == "class") 
    extra <- 104
  else extra <- 101
  rpart.plot::prp(model, type = 2, extra = extra, box.col = pals[col.index], 
                  nn = TRUE, varlen = 0, faclen = 0, shadow.col = "grey", 
                  fallen.leaves = TRUE, branch.lty = 3, ...)
  title(main = main, sub = sub)
}