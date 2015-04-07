source("libs_install.R")

Repo_Str_HTML <- function (data_str, rutaOutput) 
{
  ## Function for processing string or factor variables giving HTML output
  colnam_str=colnames(data_str)
  
  for(i in 1:length(colnam_str))
  {
    col_param=colnam_str[[i]]
    print(colnam_str[[i]])
    Reporting_Str(data_str, colnam_str[[i]])
    knit("resumen_str.Rmd",encoding="UTF-8") 
    markdownToHTML("resumen_str.md",sprintf("%s/%s.html", rutaOutput, colnam_str[[i]]))
  }
  
  
}


Repo_Num_HTML <- function (data_num, rutaOutput)
{
  ## Function for processing numerical variables giving HTML output
  colnam_num=colnames(data_num)
  
  for(i in 1:length(colnam_num))
  {
    col_param=colnam_num[[i]]
    print(col_param)
    Reporting_Num(data_num, colnam_num[[i]])
    knit("resumen_num.Rmd",encoding="UTF-8")
    markdownToHTML("resumen_num.md",sprintf("%s/%s.html", rutaOutput, colnam_num[[i]]))
  }
}





getDataByType <- function (data, df_meta, type_col) 
{
  ## Getting the variables to process according to their type
  cols_type=subset(df_meta, df_meta[,"type"]==type_col & df_meta[,"process"]==1)
  
  ## Separated by if because data frames of 1 column dont have name
  if(length(cols_type[,"variable"])==1) {
    
    data_by_type=data.frame(data[,names(data) %in% cols_type[,"variable"]])
    
    ## Change variable name
    col_name=as.character(cols_type[,"variable"])
    names(data_by_type)[[1]]=col_name
    
  } else {
    data_by_type=data[,names(data) %in% cols_type[,"variable"]]
  }
  
  return(data_by_type)
}

load_meta <- function (path_proj, data) 
{
  ## Load configuration file containing type of variable and if it must be processed. It creates a new configuration file if dont exists infering their types.
  
  if(file.exists(sprintf("%s/config_repomatic.xlsx",path_proj))) 
  {
    df_meta=read.xlsx(sprintf("%s/config_repomatic.xlsx",path_proj), 1, sheetName="variables",stringsAsFactor= TRUE,as.data.frame=TRUE, header=TRUE, colClasses=NA, keepFormulas=FALSE, encoding="UTF-8", strip.white=TRUE)
    
    
    ## Delete unuseful columns
    df_meta=df_meta[,!(names(df_meta) %in% c("stringsAsFactor","strip.white"))]
    
  } else {
    
    
    df_meta=data.frame(variable=colnames(data), process=1, type=get_type(data))
    write.xlsx(x = df_meta, file = sprintf("%s/config_repomatic.xlsx",path_proj), sheetName = "variables", row.names = FALSE, append=TRUE)
    
    
    
  }
  
  return(df_meta)
}




get_type <- function (data)
{
  ## Get data type for all column
  a=sapply(data, function(x) if(is.numeric(x)) "num" else "str")
  
  ## unname deletes names from vector
  return(unname(a))
}


getGrafNA <- function (dataNA_Table) 
{
  ## Function getting the plot of empty or NA value
  d=subset(dataNA_Table, dataNA_Table$Status != "Total")
  d=droplevels(d)
  d$Percentage=as.numeric(as.character(sub("," , ".", d$Percentage)))
  graf=ggplot(d, aes(x = factor(Status), y =Percentage,fill=factor(Status)), environment= environment()) + 
    geom_bar(stat = "identity") +scale_fill_manual(values=c("#66CC99", "#CC6666"))+
    theme(legend.position = "none")+labs(x=NULL)+ coord_cartesian(ylim = c(0, 100)) 
  
  return(graf)
}


getEstadoVar <- function(var)
{
  ## Function that returns the state of the variable
  ## NA processing
  cantNA=sum(is.na(var))
  cantNotNA=sum(!is.na(var))
  porcNA=round(cantNA/(cantNotNA+cantNA)*100,2)
  val=c(porcNA, 100-porcNA)
  dataNA_Table<<-data.frame(Status=c("NA.","Complete", "Total"), Percentage=c(round(val[[1]],2),round(val[[2]],2), "100"),Quantity=c(cantNA,cantNotNA,cantNA+cantNotNA))  
  
  return(dataNA_Table)
}

Reporting_Num <- function (dataNum, col_param) 
{
  # col_param="var"
  ## Descriptive Statistic 
  var=dataNum[[col_param]]  
  varNotNA=data.frame(var=subset(var,!is.na(var)))
  lblMinMax <<- sprintf("%.2f - %.2f", min(varNotNA[[1]]), max(varNotNA[[1]]))
  lblRango98 <<- sprintf("%.2f - %.2f", quantile(varNotNA[[1]],.01)[[1]], quantile(varNotNA[[1]],.99)[[1]])
  lblRango80 <<- sprintf("%.2f - %.2f", quantile(varNotNA[[1]],.2)[[1]], quantile(varNotNA[[1]],.80)[[1]])
  
  dfDesc<<-data.frame(Statistic=c("Unique",
                                    "Average",
                                    "Stand. Dev.",
                                    "Min-Max",
                                    "Range of 98%",
                                    "Range of 80%"),
                      Value=c(length(unique(varNotNA[[1]])),
                              round(mean(varNotNA[[1]]),2),
                              round(sd(varNotNA[[1]]),2),
                              lblMinMax,
                              lblRango98,
                              lblRango80))
  
  
  dfQuantiles <<- data.frame("q5"=quantile(varNotNA[[1]],.05)[[1]],
                             "q10"=quantile(varNotNA[[1]],.10)[[1]],
                             "q25"=quantile(varNotNA[[1]],.25)[[1]],
                             "q50"=quantile(varNotNA[[1]],.50)[[1]],
                             "q75"=quantile(varNotNA[[1]],.75)[[1]],
                             "q90"=quantile(varNotNA[[1]],.90)[[1]],
                             "q95"=quantile(varNotNA[[1]],.95)[[1]]
  )
  
  ## Plots generation
  grafHistograma<<-ggplot(as.data.frame(varNotNA[[1]]), aes(varNotNA[[1]]), environment=environment())+
    geom_histogram(aes(fill=..count..),color="blue",binwidth = diff(range(varNotNA[[1]]))/30) + 
    xlab(col_param) + ylab("Quantity")+labs(fill="")+scale_fill_gradient(low="skyblue",high="blue")+
    scale_x_continuous(breaks=pretty(n=10,x=varNotNA[[1]]))
  
  grafDensidad<<-ggplot(varNotNA, aes(varNotNA[[1]]), environment=environment()) +
    geom_density(alpha = 0.2,fill="blue",adjust=0.4) + xlab(col_param)+
    scale_x_continuous(breaks=pretty(n=10,x=varNotNA[[1]])) 
  
  grafBoxPlot<<-qplot(factor(0),fill=factor(0), varNotNA[[1]], data=varNotNA, geom="boxplot",ylab="", xlab=col_param) +   
    scale_x_discrete(breaks=NULL)+stat_summary(fun.y=mean, geom="point", shape=5, size=4) + guides(fill=FALSE) +
    scale_fill_manual(values = c("#56B4E9", "#0000FF"))+scale_y_continuous(breaks=pretty(n=10,x=varNotNA[[1]]))
  
  
  ## NA processing
  dataNA_Table<<-getEstadoVar(var)  
  grafDataNA <<- getGrafNA(dataNA_Table)
  
}


Reporting_Str <- function (dataCat, col_param) 
{
  # col_param="var_debug"
  
  dataCount=count(dataCat, vars = col_param)
  
  # Rename column
  colnames(dataCount)=c(col_param,"Frequency")
  
  ## Sort data descending
  dataSorted=dataCount[with(dataCount, order(-Frequency)), ]
  
  lblCasosUnicos=sprintf("Unique cases: %d",length(dataSorted))
  
  # Keep all values except NA
  dataSortedNotNA=subset(dataSorted,!is.na(dataSorted[,1])) 
  
  # Sum only Frequency column
  totalNotNA=sum(dataSortedNotNA[,2])
  
  # Get percentages without NA
  dataSortedNotNA$Percentage=lapply(dataSortedNotNA$Frequency, function(x) {round(100*x/totalNotNA,2)})
  
  dataSortedNotNA_NoFactor <<- data.frame(lapply(dataSortedNotNA, as.character), stringsAsFactors=FALSE)
  
  dataSortedNotNA[[col_param]] = factor(dataSortedNotNA[[col_param]], levels=unique(as.character(dataSortedNotNA[[col_param]])) )
  
  ## Unique cases
  cantDistintos<<-length(dataSortedNotNA[[col_param]])
  lblCasosUnicos <<- print(sprintf("Unique cases: %.f ", cantDistintos))
  
  ## Distribution plot
  grafDist <<- ggplot(dataSortedNotNA, 
                      aes(x=factor(dataSortedNotNA[[col_param]]),
                          y=unlist(dataSortedNotNA$Percentage),
                          fill=factor(dataSortedNotNA[[col_param]])), environment= environment()) +
    geom_bar(stat='identity') + coord_flip()  + theme(legend.position = "none") +
    ylab("% Count") +  xlab("")
  
  ## NA processing
  dataNA_Table <<-  getEstadoVar(dataCat[[col_param]])
  grafDataNA <<- getGrafNA(dataNA_Table)
  
}

repomatic <- function(file_name)
{
  #########################################
  #### Begin main program 
  #########################################
  

  ## Try to read from command line
  if(length(args)!=0) 
  {
    options(echo=TRUE) # if you want see commands in output file
    args = commandArgs(trailingOnly = TRUE)
    
    dir_name=dirname(args[1])
    base_file_name=basename(args[1])
    
    print(args[1])
    setwd(args[1])
    
  } else {
    dir_name=dirname(file_name)
    base_file_name=basename(file_name)
  }
  
  
  
  ## File name reading
  data=read.delim(sprintf("%s/%s", dir_name, base_file_name, header=TRUE))
  
  ##  Load metada (creates it at first run)
  df_meta=load_meta(dir_name, data)
  
  ## Splitting by variable type
  data_num=getDataByType(data, df_meta, "num")
  data_str=getDataByType(data, df_meta, "str")
  
  ## Directory results creation
  rutaOutput=sprintf("%s/Repomatic Results", dir_name)
  dir.create(file.path(rutaOutput), showWarnings = FALSE)
  
  ## Creation reports for variable of type numeric 
  if(ncol(data_num)>0)
    Repo_Num_HTML(data_num, rutaOutput)
  
  ## Creation reports for variable of type factor 
  if(ncol(data_str)>0)
    Repo_Str_HTML(data_str, rutaOutput)
 

  
  
}