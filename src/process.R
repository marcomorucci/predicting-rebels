library(forecast)
library(vars)
library(R.utils)

fillnas = function(d){
  chars = as.character(d$MonthYear)
  nd = d
  for (y in 1990:2014){
    for (m in 1:12){
      if (m <= 9)
        monthYear = paste(c(as.character(y), "0", as.character(m)), collapse="")
      else
        monthYear = paste(c(as.character(y), as.character(m)), collapse="")
      if (monthYear %in% chars)
        next
      nd = rbind(nd, rep(x=NA, ncol(d)))
      nd$MonthYear[nrow(nd)] = as.integer(monthYear)
      nd$Code = Mode(d$Code)
    }
  }
  nd[order(nd$MonthYear),]
}

Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

get_sum_forecast = function(data, model="arfima"){
  d = ts(data$Sum, frequency=12)

  if (model=="arfima")
    fit = arfima(d, estim="mle") 
  else if (model=="nnet")
    fit = nnetar(data$Sum)
  else
    fit = auto.arima(d)
  pred = forecast(fit, h=1)$mean
  nm = names(pred)
  pred = as.numeric(pred)
  names(pred) = nm
  pred
}

get_var_forecast = function(data, ratios=FALSE, include.c=FALSE ,...){
  if (!include.c){
    if (ratios)
        d = ts(data[,c("VC", "MC", "VV", "MV")] / (data$Sum+0.0001), frequency=12)
      else
        d = ts(data[,c("VC", "MC", "VV", "MV")], frequency=12)  
  }else{
      if (ratios)
        d = ts(data[,c("VC", "MC", "VV", "MV", "c.VC", "c.MC", "c.VV", "c.MV")] / (data$Sum+0.0001), frequency=12)
      else
        d = ts(data[,c("VC", "MC", "VV", "MV", "c.VC", "c.MC", "c.VV", "c.MV")], frequency=12)   
  }
  fit = VAR(d, ...)
  pred = forecast(fit, h=1)$mean
  ns = names(pred)
  pred = as.numeric(pred)
  names(pred) = ns
  pred[c("VC", "MC", "VV", "MV")]
}

predict_all = function(train, test, var.ratios=FALSE, sum.model="arfima",
                       ind.forecast=FALSE, include.c=FALSE, ...){
  res = data.frame()
  pdata = train
  for (y in 1:nrow(test)){
    if(ind.forecast)
      v = get_ind_forecasts(pdata, model=sum.model, ratios=var.ratios, ...)
    else
      v = get_var_forecast(pdata, ratios=var.ratios, include.c=include.c, ...)
    if (sum.model=="var")
      s = sum(v)
    else
      s = get_sum_forecast(pdata, model=sum.model)
    if (var.ratios)
      v = v*s
    res = rbind(res, c(v, s))
    pdata = rbind(pdata, test[y,])
  }
  names(res) = c("VC","MC","VV","MV","Sum")
  res
}

compute_error = function(pred, test, type="abs"){
  out = data.frame(row.names=1:nrow(pred))
  for (c in colnames(pred)){
    if (!(c %in% colnames(test)))
      next
    
    if (type=="abs")
      err = abs(pred[,c] - test[,c])
    else if (type=="raw")
      err = pred[,c] - test[,c]
    else if (type=="sqr")
      err = (pred[,c] - test[,c])**2
    else if (type=="rat")
      err = abs(pred[, c] - test[, c])/test[, c]
    out = cbind(out, err)
    names(out)[length(names(out))] = paste(c(c, "err"), collapse=".")
  }
  if ("VarSum" %in% colnames(out))
    out$VarSum.err = abs(out$VarSum - test$Sum)
  out
}

get_ind_forecasts = function(data, model="arfima", ratios=FALSE, ...){
  if (ratios)
    d = data[,c("VC", "MC", "VV", "MV")] / (data$Sum+0.0001)
  else
    d = data[,c("VC", "MC", "VV", "MV")]
  
  row = numeric()
  for (c in colnames(d)){
    if (model=="arfima")
      fit = arfima(ts(d[,c], frequency=12), ...)
    if (model=="nnet")
      fit = nnetar(d[, c], ...)
    else
      fit = auto.arima(ts(d[,c], frequency=12), ...)
    pred = as.numeric(forecast(fit, h=1)$mean)
    row = c(row, pred)
    names(row)[length(row)] = c
  }
  
  row
}


run_analysis = function(train, test, var.ratios=FALSE, sum.model="arfima", ...){
  out = data.frame()
  print("Building model 1...")
  m1 = build.m1(train, test, sum.model)
  print("Building model 2...")
  m2 = build.m2(train, test, sum.model)
  print("Building model 3...")
  m3 = build.m3(train, test, sum.model)
  print("Building model 4...")
  m4 = build.m4(train, test, sum.model)
  print("Building model 5...")
  m5 = build.m5(train, test, sum.model)
  print("Building naive benchmark...")
  naive = build.naive(train, test)
  out = data.frame(m1, m2, m3, m4, m5, naive)
  out
}

summarize = function(x, cols, include.std=TRUE){
  cmean = colwise(mean)
  cstd = colwise(sd)
  if(include.std)
    res = rbind(mean=cmean(x[, cols]), std=cstd(x[, cols]))
  else
    res = cmean(x[, cols])
  res
}

summarize.errors = function(m, include.std=TRUE){
  cols = paste(deparse(substitute(m)), c("VC.err", "MC.err", "VV.err", "MV.err", "Sum.err"), sep=".")
  code = paste(deparse(substitute(m)), "Code", sep=".")
  cntry = ddply(m$err, .variables=code, .fun=summarize, cols, include.std)
  t = cbind(m$err, Time=rep(1:23, times=length(levels(m$err[,code]))))
  time = ddply(t, "Time", summarize, cols, include.std)
  event = summarize(m$err, cols, include.std)
  s = list()
  s$month = time[,-1]
  s$country = cntry
  s$event = event
  s
}

generate.tables = function(data){
  vc = data.frame()
  mc = data.frame()
  vv = data.frame()
  mv = data.frame()
  for (m in data){
    err = summarize.errors(m, include.std=FALSE)
    vc = cbind(vc, err[, 1])
    mc = cbind(mc, err[, 2])
    vv = cbind(vv, err[, 3])
    mv = cbind(mv, err[, 4])
  }
  tab = list()
  tab$VC = vc
  tab$MC = mc
  tab$VV = vv
  tab$MV = mv
  tab
}

build.m1 = function(train, test, mode="arfima", include.codes=TRUE, include.c=TRUE, ...){
  out = data.frame()
  vals = data.frame()
  m1 = list()
  pb <- txtProgressBar(min = 0, max = length(levels(train$Code)), style = 3)
  cnt = 0
  for(cntry in levels(train$Code)){
    pre = predict_all(train[train$Code==cntry, ], test[test$Code==cntry, ], 
                      sum.model="var", include.c=include.c, ...)
    err = compute_error(pre, test[test$Code==cntry, colnames(pre)])
    if (include.codes){
      out = rbind(out, cbind(err, Code=rep(cntry, nrow(err))))
      vals = rbind(vals, cbind(pre, Code=rep(cntry, nrow(pre))))
    }else{
      out = rbind(out, err)
      vals = rbind(vals, pre)
    }
    setTxtProgressBar(pb, cnt)
    cnt = cnt +1
  }
  names(out) = as.character(sapply(names(out), FUN=function(x){paste("m1",x, sep=".")}))
  names(vals) = as.character(sapply(names(out), FUN=function(x){paste("m1",x, sep=".")}))
  m1$pred = vals
  m1$err = out
  close(pb)
  m1
}

build.m2 = function(train, test, mode="arfima", include.codes=TRUE, include.c=TRUE, ...){
  m2 = list()
  out = data.frame()
  vals = data.frame()
  pb <- txtProgressBar(min = 0, max = length(levels(train$Code)), style = 3)
  cnt = 0
  for(cntry in levels(train$Code)){
    pre = predict_all(train[train$Code==cntry, ], test[test$Code==cntry, ], sum.model=mode, 
                      include.c=include.c, ...)
    err = compute_error(pre, test[test$Code==cntry, colnames(pre)])
    if (include.codes){
      out = rbind(out, cbind(err, Code=rep(cntry, nrow(err))))
      vals = rbind(vals, cbind(pre, Code=rep(cntry, nrow(pre))))
    }else{
      out = rbind(out, err)
      vals = rbind(vals, pre)
    }
    
    setTxtProgressBar(pb, cnt)
    cnt = cnt +1
  }
  names(out) = as.character(sapply(names(out), FUN=function(x){paste("m2",x, sep=".")}))
  names(vals) = as.character(sapply(names(vals), FUN=function(x){paste("m2",x, sep=".")}))
  m2$pred = vals
  m2$err = out
  close(pb)
  m2
}

build.m3 = function(train, test, mode="arfima", include.codes=TRUE, 
                    include.c=TRUE, ...){
  out = data.frame()
  vals = data.frame()
  m3 = list()
  pb <- txtProgressBar(min = 0, max = length(levels(train$Code)), style = 3)
  cnt = 0
  for(cntry in levels(train$Code)){
    pre = predict_all(train[train$Code==cntry, ], test[test$Code==cntry, ], var.ratios=TRUE, 
                      sum.model=mode, include.c=include.c, ...)
    err = compute_error(pre, test[test$Code==cntry, colnames(pre)])
    if (include.codes){
      out = rbind(out, cbind(err, Code=rep(cntry, nrow(err))))
      vals = rbind(vals, cbind(pre, Code=rep(cntry, nrow(err))))
    }else{
      out = rbind(out, err)
      vals = rbind(vals, pre)
    }
    setTxtProgressBar(pb, cnt)
    cnt = cnt +1
  }
  names(out) = as.character(sapply(names(out), FUN=function(x){paste("m3",x, sep=".")}))
  names(vals) = as.character(sapply(names(vals), FUN=function(x){paste("m3",x, sep=".")}))
  m3$pred = vals
  m3$err = out
  close(pb)
  m3
}

build.m4 = function(train,test, mode="arfima", include.codes=TRUE, include.c=TRUE, ...){
  out = data.frame()
  vals = data.frame()
  m4 = list()
  pb <- txtProgressBar(min = 0, max = length(levels(train$Code)), style = 3)
  cnt = 0
  for(cntry in levels(train$Code)){
    pre = predict_all(train[train$Code==cntry, ], test[test$Code==cntry, ], 
                      var.ratios=FALSE, sum.model="var", ind.forecast=TRUE, 
                      include.c = include.c, ...)
   
    err = compute_error(pre, test[test$Code==cntry, colnames(pre)])
    if (include.codes){
      out = rbind(out, cbind(err, Code=rep(cntry, nrow(err))))
      vals = rbind(vals, cbind(pre, Code=rep(cntry, nrow(pre))))
    }
    else{
      out = rbind(out, err)
      vals = rbind(vals, err)
    }
    setTxtProgressBar(pb, cnt)
    cnt = cnt +1
  }
  names(out) = as.character(sapply(names(out), FUN=function(x){paste("m4",x, sep=".")}))
  names(vals) = as.character(sapply(names(vals), FUN=function(x){paste("m4",x, sep=".")}))
  m4$pred = vals
  m4$err = out
  close(pb)
  m4
}


build.m5 = function(train,test, mode="arfima", include.codes=TRUE, include.c=TRUE, ...){
  out = data.frame()
  m5 = list()
  vals = data.frame()
  pb <- txtProgressBar(min = 0, max = length(levels(train$Code)), style = 3)
  cnt = 0
  for(cntry in levels(train$Code)){
    pre = predict_all(train[train$Code==cntry, ], test[test$Code==cntry, ], 
                      var.ratios=TRUE, sum.model=mode, ind.forecast=TRUE, 
                      include.c=include.c, ...)
    err = compute_error(pre, test[test$Code==cntry, colnames(pre)])
    if (include.codes){
      out = rbind(out, cbind(err, Code=rep(cntry, nrow(err))))
      vals = rbind(vals, cbind(pre, Code=rep(cntry, nrow(pre))))
    }else{
      out = rbind(out, err)
      vals = rbind(vals, pre)
    }
    setTxtProgressBar(pb, cnt)
    cnt = cnt +1
  }
  names(out) = as.character(sapply(names(out), FUN=function(x){paste("m5",x, sep=".")}))
  names(vals) = as.character(sapply(names(vals), FUN=function(x){paste("m5",x, sep=".")}))
  m5$pred = vals
  m5$err = out
  close(pb)
  m5
}

multi_arfima = function(train, test, ...){
  pdata = train
  for(y in 1:nrow(test)){
    fit = arfima::arfima(pdata, ...)
    pred = predict(fit, n.ahead=1)
  }
}

multi_var = function(train, test, ...){
  pdata = train
  out = data.frame()
  for(y in 1:nrow(test)){
    fit = VAR(y=ts(pdata, frequency=12), ...)
    pred = forecast(fit, h=1)$mean
    out = rbind(out, pred)
    pdata = rbind(pdata, test[y, ])
  }
  out
}

naive.pred = function(train, test){
  p = lag(ts(test[,c("VC", "MC", "VV","MV", "Sum")]), )
  rbind(train[nrow(train), c("VC", "MC", "VV", "MV", "Sum")], p)[1:nrow(test), ]
}

build.naive = function(train, test, include.codes=TRUE){
  out = data.frame()
  vals = data.frame()
  naive = list()
  for (cntry in levels(train$Code)){
    pred = naive.pred(train[train$Code==cntry, ], test[test$Code==cntry, ])
    err = compute_error(pred, test[test$Code==cntry, ])
    if (include.codes){
      err = cbind(err, Code=rep(cntry, nrow(err)))
      pred = cbind(pred, Code=rep(cntry, nrow(pred)))
    }
    vals = rbind(vals, pred)
    out = rbind(out, err)
  }
  names(out) = as.character(sapply(names(out), FUN=function(x){paste("naive",x, sep=".")}))
  names(vals) = as.character(sapply(names(vals), FUN=function(x){paste("naive",x, sep=".")}))
  naive$pred = vals
  naive$err = out
  naive
}