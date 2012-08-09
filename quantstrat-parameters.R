
# getParameterTable<-function (strategy) #,staticSwitch)
# {
#   
#   tmp_paramTable<-list()
#   nofi=0
#   indexnum=0
#   for (indicator in strategy$indicators ){ 
#     #   .formals  <- formals(fun) #yc here get the prameters needed for that function.
#     #   print(.formals)
#     nofi=nofi+1 
#     indexnum=indexnum+1
#     #
#     fun<-match.fun(indicator$name) #yc here get the function of the indicator
#     tmp_paramTable[[nofi]]<-list()
#     #tmp_paramTable[[nofi]]<-formals(fun)       
#     tmp_paramTable[[nofi]]$paramType<-'indicator'
#     tmp_paramTable[[nofi]]$paramEnabled<-indicator$enabled
#     tmp_paramTable[[nofi]]$indexnum=indexnum
#     tmp_paramTable[[nofi]]$label<-indicator$label
#     tmp_paramTable[[nofi]]$args<-formals(fun)
#     
#   }
#   
#   indexnum=0
#   for (signal in strategy$signals ){ 
#     
#     
#     nofi=nofi+1
#     indexnum=indexnum+1
#     
#     fun<-match.fun(signal$name)
#     tmp_paramTable[[nofi]]<-list()
#     
#     tmp_paramTable[[nofi]]$paramType<-'signal'
#     tmp_paramTable[[nofi]]$paramEnabled<-signal$enabled
#     tmp_paramTable[[nofi]]$indexnum=indexnum
#     tmp_paramTable[[nofi]]$label<-signal$label
#     tmp_paramTable[[nofi]]$args<-formals(fun)
#     
#   }
#   
#   for (rule in strategy$rules ){
#     indexnum=0
#     for (trule in rule){
#       
#       
#       
#       nofi=nofi+1
#       indexnum=indexnum+1
#       
#       fun<-match.fun(trule$name) 
#       tmp_paramTable[[nofi]]<-list()
#       
#       tmp_paramTable[[nofi]]$paramType<-trule$type
#       tmp_paramTable[[nofi]]$paramEnabled<-trule$enabled
#       tmp_paramTable[[nofi]]$indexnum=indexnum
#       tmp_paramTable[[nofi]]$label<-trule$label
#       tmp_paramTable[[nofi]]$args<-formals(fun)
#       tmp_paramTable[[nofi]]$timespan<-trule$timespan
#       
#     }
#     
#   }
#   #data.frame(c(paramStructure[[6]][1:4],param.name.=names(paramStructure[[6]]$args)))
#   paramPack<-list()
#   for (i in 1:length(tmp_paramTable)){
#     
#     paramPack$paramNameList[[i]]<-data.frame(c(tmp_paramTable[[i]][1:4],param.=names(tmp_paramTable[[i]]$args)))
#     
#   }
#   
#   #tmp_paramTable$strategyName<-strategy$name
#   paramPack$strategyName<-strategy$name
#   paramPack$structure<-tmp_paramTable
#   
#   return(paramPack)
#   
# }

# setParameterDistribution<-function(paramDist=NULL,type=NULL,indexnum=0,distribution=NULL,weight,label,psindex=NULL){#All is needed, set to illegal values
#   
#   if(!hasArg(paramDist)||!exists(as.character(substitute(paramDist))) ){
#     paramDist<-list()
#     print('Object for parameter distribution initialized...')
#   }
#   #   else{
#   
#   if (!is.list(distribution)|length(distribution)!=1) stop("distribution must be passed as a named list of length 1")
#   if (!type %in% c("indicator","signal","enter","exit","order")) stop("Type must be a string in: indicator, signal, enter, exit, order")
#   
#   tmp_paramDist<-list()
#   tmp_paramDist$type<-type
#   tmp_paramDist$indexnum<-indexnum
#   tmp_paramDist$distribution<-distribution
#   
#   if (missing(label)) {
#     tmp_paramDist$label<-paste('Param',type,indexnum,names(distribution),sep='.')
#   }
#   else {tmp_paramDist$label<-label}
#   
#   
#   if(!hasArg(weight)) weight<-sample(1/length(distribution[[1]]),length(distribution[[1]]),replace=TRUE)
#   
#   tmp_paramDist$weight<-weight
#   
#   if(!hasArg(psindex) | (hasArg(psindex) & is.null(psindex))) psindex = length(paramDist)+1
#   #class(tmp_paramDist)<-'parameter_distribution'
#   
#   #TODO put an check to see if the type/indexnum exist already.
#   paramDist[[psindex]]<-tmp_paramDist
#   #   }
#   return(paramDist)
# }


applyParameter<-function(strategy,portfolios,parameterPool,parameterConstraints,method,sampleSize,verbose=FALSE,...){
  #need to create combination of distribution values in each slot of the parameterPool
  
  initialPortf<-getPortfolio(portfolios)
  symbols<-names(initialPortf$symbols)
  initDate<-time(first(initialPortf$symbols[[1]]$posPL))
  
  limits<-list()
  for(symbol in names(initialPortf$symbols))
    limits[[symbol]]<-initialPortf$symbols[[symbol]]$PosLimit
  
  tmp_strategy<-strategy
  
  results<-list()
  results$stats<-NULL
  
  if (!is.strategy(tmp_strategy)) {
    tmp_strategy<-try(getStrategy(tmp_strategy))
    if(inherits(tmp_strategy,"try-error"))
      stop ("You must supply an object of type 'strategy'.")
  } 
  
  out<-list()
  paramdist<-list()
  paramweight<-list()
  paramLabel<-list()
  lvmatch<-list()
  
  for (i in 1:length(parameterPool)){
    
    distr<-parameterPool[[i]]
    #paramdist[[i]]<-distr$distribution[[1]]
    paramdist[[paste('Param',distr$type,distr$indexnum,names(distr$distribution),sep='.')]]<-distr$distribution[[1]]
    paramweight[[paste('ParamWt',distr$type,distr$indexnum,names(distr$distribution),sep='.')]]<-distr$weight
    #paramdist[[paste(i)]]<-distr$distribution[[1]]
    
    #Build label<->var name match.
    lvmatch$label[i]<-distr$label
    lvmatch$varName[i]<-paste('Param',distr$type,distr$indexnum,names(distr$distribution),sep='.')
    
  }
  
  paramLabel<-data.frame(lvmatch,stringsAsFactors=FALSE)

  #TODO make it take sample size etc.
  
  if (method=='expand') 
  {
    paramTable<-expand.grid(paramdist, stringsAsFactors=FALSE)
  }
  else if (method=='random')
  {
    if (missing(sampleSize)) {stop ("sampleSize is needed")} 
    #paramTable<-data.frame()
    
    #genSample update the paramTable with more sample rows.
    genSample<-function(iparamTable,paramdist,tsampleSize,remainSize)
    {
      if (missing(remainSize) ) remainSize=tsampleSize
      
      tparamTable<-data.frame()
      
      for( i in 1:length(paramdist))
      {
        ireplace<-(length(paramdist[i])<tsampleSize)
        
        if (nrow(tparamTable)==0)
        {
          tparamTable<-data.frame(sample(paramdist[[i]],remainSize,prob=paramweight[[i]],replace=ireplace),stringsAsFactors=FALSE)
          
        }   
        else{
          tparamTable<-cbind(tparamTable,data.frame(sample(paramdist[[i]],remainSize,prob=paramweight[[i]],replace=ireplace),stringsAsFactors=FALSE))
        }                                       
      }
      
      names(tparamTable)<-names(paramdist)
      
      # put constraint test on tparamTable, before rbind
      for (k in 1:length(parameterConstraints))
      {
        constrintfill<-paramConstraint(label=parameterConstraints[[k]]$constraintLabel,
                                       data=tparamTable,
                                       columns=merge(paramLabel,data.frame(parameterConstraints[[k]]$paramList),by="label")$varName, #has to keep the order.
                                       relationship=parameterConstraints[[k]]$relationship)                
        
        
        #only keep the samples fulfill the constraints.
        tparamTable<-tparamTable[which(constrintfill==TRUE),]
      }
      
      
      iparamTable<-rbind(iparamTable,tparamTable)
      
      iparamTable<-unique(iparamTable)
      
      #           if(verbose >=1) print("nnnnnnnnnnnnnnnnnnnnnnn")
      #           if(verbose >=1) print(nrow(iparamTable))
      
      if (nrow(iparamTable)<tsampleSize)
      {
        iparamTable<-genSample(iparamTable,paramdist,tsampleSize,remainSize=tsampleSize-nrow(iparamTable))          
      }
      
      names(iparamTable)<-names(paramdist)
      return(iparamTable)
    } #end define function
    
    paramTable<-NULL
    paramTable<-genSample(paramTable,paramdist,sampleSize)      
    
  }
  
  strategyList<-list()
  if(verbose >=1) print("ParamTable generated")
  
  instruments<-as.list(FinancialInstrument:::.instrument)
  getSymbols<-as.list(.getSymbols)
  blotter<-as.list(.blotter)
  
  #Pack all symbols downloaded in .GlobalEnv
  symbols<-names(.getSymbols)
  
  testPackListPRL<-foreach (i = 1:nrow(paramTable), .export=c('instruments',symbols,'getSymbols','blotter','tmp_strategy'),.verbose=TRUE,...=...) %dopar% 
    
{
  #if(verbose)
  print(paste('===> now starting parameter test', i))
  
  require(quantstrat, quietly=TRUE)
  
  # loops must be run with an empty .blotter environment each, or .blotter appears to accumulate portfolios and accounts
  # and passes them from one loop to the next on each CPU - JH July 2012
  if (getDoParRegistered() && getDoParWorkers()>1)
  {
    rm(list=ls(pos=.blotter), pos=.blotter)
    gc(verbose=verbose)
  }
  
  testPack<-list()
  
  #Pass environments needed.
  loadInstruments(instruments)
  .getSymbols<-as.environment(getSymbols)
  
  #Unpack symbols to worker. change later.
  #seems need to go through assign, rather than just .export the names...
  
  for (sym in symbols) {
    assign(sym, eval(as.name(sym)), .GlobalEnv)
  }
  
  #Create a copy of strategy object, so not to lock up on the sameone.
  PLtmp_strategy<-tmp_strategy
  
  #Extract parameter from table and construct PLtmp_strategy.
  for (j in 1:ncol(paramTable))
  {
    set.param.values <- function(param.list, new.values)
    {
      pnamepos<-pmatch(names(param.list),names(new.values),nomatch=0L)
      
      if( any(pnamepos>0))
      {
        #FIXME: any matching args will be set to 1st param
        param.list[which(pnamepos>0)]<-new.values[1]
      }
      else
      {
        param.list<-append(param.list, new.values)
      }
      param.list
    }
    
    tmp_arg<-parameterPool[[j]]$distribution[1] #Just get the list form with name
    tmp_arg[[1]]<-paramTable[i,j]
    
    tmp_index<-parameterPool[[j]]$indexnum
    
    switch(parameterPool[[j]]$type,
           'indicator'=
{
  PLtmp_strategy$indicators[[tmp_index]] = set.param.values(PLtmp_strategy$indicators[[tmp_index]], tmp_arg)
  PLtmp_strategy$indicators[[tmp_index]]$arguments = set.param.values(PLtmp_strategy$indicators[[tmp_index]]$arguments, tmp_arg)
},
           'signal'=
{
  PLtmp_strategy$signals[[tmp_index]] = set.param.values(PLtmp_strategy$signals[[tmp_index]], tmp_arg)
  PLtmp_strategy$signals[[tmp_index]]$arguments = set.param.values(PLtmp_strategy$signals[[tmp_index]]$arguments, tmp_arg)
},
           'order'=
{
  PLtmp_strategy$rules$order[[tmp_index]] = set.param.values(PLtmp_strategy$rules$order[[tmp_index]], tmp_arg)
  PLtmp_strategy$rules$order[[tmp_index]]$arguments = set.param.values(PLtmp_strategy$rules$order[[tmp_index]]$arguments, tmp_arg)
},
           'enter'=
{
  PLtmp_strategy$rules$enter[[tmp_index]] = set.param.values(PLtmp_strategy$rules$enter[[tmp_index]], tmp_arg)
  PLtmp_strategy$rules$enter[[tmp_index]]$arguments = set.param.values(PLtmp_strategy$rules$enter[[tmp_index]]$arguments, tmp_arg)
},
           'exit'=
{
  PLtmp_strategy$rules$exit[[tmp_index]] = set.param.values(PLtmp_strategy$rules$exit[[tmp_index]], tmp_arg)
  PLtmp_strategy$rules$exit[[tmp_index]]$arguments = set.param.values(PLtmp_strategy$rules$exit[[tmp_index]]$arguments, tmp_arg)
}
    )
  } #loop j
  
  #Initial portfolio for each test        
  #######################################################################################
  
  testPack$portfolio.st<-paste(portfolios,'p',i,sep='.')
  testPack$account.st<-paste(portfolios,'p',i,sep='.')
  
  rmpstr<-paste('portfolio',testPack$portfolio.st,sep=".")
  rmastr<-paste('account',testPack$account.st,sep=".")
  
  try(rm(list = rmpstr, pos = .blotter),silent=FALSE)
  try(rm(list = rmastr, pos = .blotter),silent=FALSE)
  try(rm(list=paste("order_book",testPack$account.st,sep="."),pos=.strategy),silent=FALSE)
  
  if(verbose >=1) print('Initial portf')
  
  #               Decide not to remove the main obj from .blotter, incase of non-parallel run.
  #               try(rm(list=paste("order_book",portfolios,sep='.'),pos=.strategy),silent=TRUE)
  ##              try(rm(paste("account",portfolio.st,sep='.'),paste("portfolio",portfolio.st,sep='.'),pos=.blotter),silent=TRUE)
  #               try(rm(list=paste("account",portfolios,sep='.'),pos=.blotter))
  #               try(rm(list=paste("portfolio",portfolios,sep='.'),pos=.blotter))
  
  try({initPortf(testPack$portfolio.st,symbols=symbols, initDate=initDate)})
  try({initAcct(testPack$account.st,testPack$portfolio.st, initDate=initDate)})
  try({initOrders(portfolio=testPack$portfolio.st,initDate=initDate)})
  
  for(symbol in names(limits))
    addPosLimit(portfolio=testPack$portfolio.st, symbol=symbol, timestamp=initDate, maxpos=limits[[symbol]]$MaxPos[[1]])
  
  # Apply strategy ######################################################################################
  if(verbose >=1) print("Apply strategy...")
  
  try(rm("PLtmp_strategy",pos=.strategy),silent=TRUE)
  
  if(verbose >=1) print(PLtmp_strategy$signals[[2]])
  
  assign("PLtmp_strategy1",PLtmp_strategy,envir=as.environment(.strategy))
  
  testPack$out<-try(applyStrategy(strategy=PLtmp_strategy , portfolios=testPack$portfolio.st ),...=...)
  testPack$strategy<-PLtmp_strategy
  
  #   Update portfolio ######################################################################################
  
  #out<-try(applyStrategy(strategy=stratBBands , portfolios=portfolios ))
  #       try({
  #                   updatePortf(testPack$portfolio.st,Date=initDate)
  #                   updateAcct(testPack$account.st,Date=initDate)
  #                   updateOrders(portfolio=testPack$portfolio.st)
  #               })
  
  
  #try(updatePortf(Portfolio=testPack$portfolio.st,Dates=paste('::',as.Date(Sys.time()),sep='')))
  updatePortf(Portfolio=testPack$portfolio.st,Dates=paste('::',as.Date(Sys.time()),sep=''))
  
  #no need to update account.
  #updateAcct(account.st,Dates=paste(startDate,endDate,sep="::")) 
  #updateEndEq(account.st,Dates=paste(startDate,endDate,sep="::"))
  #getEndEq(account.st,Sys.time())
  
  testPack$parameters<-paramTable[i,]
  
  testPack$stats<-tradeStats(Portfolios=testPack$portfolio.st)
  testPack$blotterl<-as.list(.blotter)
  #               testPack$blotter<-as.environment(.blotter)
  #               testPack$blotterr<-.blotter
  
  return(testPack)
  
}   # Loop i
  gc(verbose=verbose)
  
  for (k in 1: nrow(paramTable)){
    results$statsTable<-rbind(results$stats,cbind(testPackListPRL[[k]]$parameters,testPackListPRL[[k]]$stats))
    if(verbose >=1) print(names(testPackListPRL[[k]]$blotterl))
    
    for(nn in 1:length(testPackListPRL[[k]]$blotterl)){
      #           if(verbose >=1) print(paste(names(testPackListPRL[[k]]$blotterl)[nn],'nnp',nn,sep='.'))
      assign(names(testPackListPRL[[k]]$blotterl[nn]),testPackListPRL[[k]]$blotterl[[nn]],envir=as.environment(.blotter))
    }
    names(testPackListPRL)[k]<-testPackListPRL[[k]]$portfolio.st
  }
  
  results$eachRun<-testPackListPRL
  results$paramTable<-paramTable
  results$paramConstrainTable<-data.frame(parameterConstraints)
  
  results$parameterDistribution<-parameterPool
  results$parameterConstraints<-parameterConstraints
  
  return(results)
  
}

assignInNamespace("applyParameter", applyParameter, ns="quantstrat")

paramConstraint <- function(label,data=mktdata, columns, relationship=c("gt","lt","eq","gte","lte")) {
  relationship=relationship[1] #only use the first one
  #   if(verbose >=1) print(columns)
  if (length(columns)==2){
    ret_sig=NULL
    if (relationship=='op'){
      # (How) can this support "Close"? --jmu
      if(columns[1] %in% c("Close","Cl","close"))
        stop("Close not supported with relationship=='op'")
      switch(columns[1],
             Low =, 
             low =, 
             bid = { relationship = 'lt' },
             Hi  =,
             High=,
             high=,
             ask = {relationship = 'gt'}
      )
    }
    
    colNums <- match.names(columns,colnames(data))
    
    opr <- switch( relationship,
                   gt = , '>' = '>', 
                   lt =, '<' = '<', 
                   eq =, "==" =, "=" = "==",
                   gte =, gteq =, ge =, ">=" = ">=",
                   lte =, lteq =, le =, "<=" = "<="
    )
    
    ret_sig$tname <- do.call( opr, list(data[,colNums[1]], data[,colNums[2]]))
    
  } else {
    stop("comparison of more than two columns not supported, see sigFormula")
  }
  names(ret_sig)<-label
  return(data.frame(ret_sig))
}

assignInNamespace("paramConstraint", paramConstraint, ns="quantstrat")
