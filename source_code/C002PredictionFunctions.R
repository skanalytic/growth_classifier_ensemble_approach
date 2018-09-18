FZhireadalldata <- function(dirname="Datafiles"){
	fulllistname <- FZhilistdata(dirname=dirname)
	listname <- sapply(fulllistname,FUN=function(x){
				substr(x,1,nchar(x)-4)
			})
	listname <- gsub("^.*?_","",listname)
	listname <- sapply(listname,FUN=function(x){
				subfront <- substr(x,1,nchar(x)-3)
				subback <- substr(x,nchar(x)-2,nchar(x))
				if(subback %in% c("ANN","LTM")){
					paste(subfront,"_",subback,sep="")
				}else{x}
			})

	for(nthdt in 1:length(fulllistname)){
		tempname <- fulllistname[nthdt]
		tempname <- substr(tempname,1,nchar(tempname)-4)
		tempdt <- FZreaddata(dataobj=tempname, dirname = dirname)
		tempcolnames <- colnames(tempdt)
		temprownames <- rownames(tempdt)
		tempsubback <- substr(tempname,nchar(tempname)-2,nchar(tempname))
		if(length(which(!is.na(tempdt))) == 0 | tempsubback == "ANN"){
			ANNname <- paste(substr(tempname,1,nchar(tempname)-3),"ANN",sep="")
			tempdt1 <- FZreaddata(dataobj=ANNname, dirname = dirname)
			tempdt <- FZgetmonthlyfromannual(ANNdata=tempdt1, LTMdata=tempdt, shiftper=3)
		}
		assign(toupper(listname[nthdt]),tempdt,pos = .GlobalEnv)
	}

	return(toupper(listname))
}


FZhibacktestnonode <- function(trainingmx=NULL,predictionmx=NULL, class_method1=NULL, class_method2=NULL, train_method=NULL, useweights=FALSE,binary=FALSE,maxmtry = 35, node=NULL){
	if(is.null(trainingmx) | is.null(predictionmx)){stop("FZhibacktest: Input is NULL")}

  require(e1071)
	require(caret)
	set.seed(82)
	uniquenames <- unique(predictionmx[,2])

	outmx <- matrix(NA,ncol=length(unique(predictionmx[,2])),nrow=length(unique(predictionmx[,1])))
	rownames(outmx) <- unique(predictionmx[,1])
	colnames(outmx) <- unique(predictionmx[,2])
	training_portion <- 0.9
	rownamesid <- rownames(outmx)

	if(binary){
		#Growth Only
		factorlist <- trainingmx[,1]
		trainingmx[,1] <- ifelse(!is.na(factorlist) & factorlist != "GROWTH","MATURE","GROWTH")
		input <- FZremovena(trainingmx)
		intrain  <- createDataPartition(y = input[,1], p = training_portion, list = FALSE)
		training <- input[intrain,]
		training[,1] <- as.factor(training[,1])
		testing <- input[-intrain,]
		growthstocklength <- length(which(!is.na(training[,1]) & training[,1] == "GROWTH"))
		maturestocklength <- max(table(training[,1]))
		weightsgrowth <- maturestocklength/growthstocklength
		weightsvec <- ifelse(!is.na(training[,1]) & training[,1] == "GROWTH", weightsgrowth,1)
		weightsvec <- weightsvec/sum(weightsvec)
		trctrl <- trainControl(method = train_method, number = 10, repeats = 3)
		tunegrid <- expand.grid(.mtry=c(1:maxmtry))

		if(!is.null(useweights) & useweights==TRUE){
			if(class_method1 %in% c("rf","ranger")){
				if(!is.null(node)){
					fit <- train(training[,-1], training[,1], method = class_method1, trControl=trctrl,weights=weightsvec,tuneGrid=tunegrid)
				}else{
					fit <- train(training[,-1], training[,1], method = class_method1, trControl=trctrl,weights=weightsvec,tuneGrid=tunegrid)
				}
			}else{
				fit <- train(training[,-1], training[,1], method = class_method1, trControl=trctrl,weights=weightsvec)
			}
		} else {
			if(class_method1 %in% c("rf","ranger")){
				if(!is.null(node)){
					fit <- train(training[,-1], training[,1], method = class_method1, trControl=trctrl,tuneGrid=tunegrid)
				}else{
					fit <- train(training[,-1], training[,1], method = class_method1, trControl=trctrl,tuneGrid=tunegrid)
				}
			}else{
				fit <- train(training[,-1], training[,1], method = class_method1, trControl=trctrl)
			}
		}

		for(upos in uniquenames){
			predict_indep_mat <- FZremovena(predictionmx[predictionmx[,2] == upos,])
			if(nrow(predict_indep_mat) != 0){
				colnames(predict_indep_mat)[-c(1,2)] <- colnames(testing[,-1]) 
				Prediction <- predict(fit, newdata = predict_indep_mat[,-c(1,2)]) 
				names(Prediction) <- predict_indep_mat[,1]
				subpos <- rownames(outmx)
				stockselect <- as.character(predict_indep_mat[,1])[as.character(predict_indep_mat[,1]) %in% subpos]
				outmx[stockselect,upos] <- as.character(Prediction[stockselect])
			}
		}

		#The Rest
		outmx[!is.na(outmx) & outmx!="GROWTH"] <- NA

		trainingmx[,1] <- ifelse(!is.na(factorlist) & factorlist == "MATURECYCLICAL","MATURECYCLICAL",ifelse(!is.na(factorlist) & factorlist == "MATURESTABLE","MATURESTABLE",NA))
		input <- FZremovena(trainingmx)
		intrain  <- createDataPartition(y = input[,1], p = training_portion, list = FALSE)
		training <- input[intrain,]
		training[,1] <- as.factor(training[,1])
		testing <- input[-intrain,]
		trctrl <- trainControl(method = train_method, number = 20, repeats = 10)		
		tunegrid <- expand.grid(.mtry=c(1:maxmtry))

		if(class_method2 %in% c("ranger","rf")){
			if(!is.null(node)){
				fit <- train(training[,-1], training[,1], method = class_method2, trControl=trctrl,tuneGrid=tunegrid)
			}else{
				fit <- train(training[,-1], training[,1], method = class_method2, trControl=trctrl,tuneGrid=tunegrid)
			}
		}else{
			fit <- train(training[,-1], training[,1], method = class_method2, trControl=trctrl) 
		}

		for(upos in uniquenames){
			predict_indep_mat <- FZremovena(predictionmx[predictionmx[,2] == upos,])
			if(nrow(predict_indep_mat) != 0){
				colnames(predict_indep_mat)[-c(1,2)] <- colnames(testing[,-1]) 
				Prediction <- predict(fit, newdata = predict_indep_mat[,-c(1,2)]) 
				names(Prediction) <- predict_indep_mat[,1]
				subpos <- rownames(outmx)[which(is.na(outmx[,upos]) | outmx[,upos] != "GROWTH")]
				stockselect <- as.character(predict_indep_mat[,1])[as.character(predict_indep_mat[,1]) %in% subpos]
				outmx[stockselect,upos] <- as.character(Prediction[stockselect])
			}
		}
	}else{
		input <- FZremovena(trainingmx)
		intrain  <- createDataPartition(y = input[,1], p = training_portion, list = FALSE)
		training <- input[intrain,]
		training[,1] <- as.factor(training[,1])
		testing <- input[-intrain,]
		growthstocklength <- length(which(!is.na(training[,1]) & training[,1] == "GROWTH"))
		maturestocklength <- max(table(training[,1]))
		weightsgrowth <- maturestocklength/growthstocklength
		weightsvec <- ifelse(!is.na(training[,1]) & training[,1] == "GROWTH", weightsgrowth,1)
		weightsvec <- weightsvec/sum(weightsvec)
		trctrl <- trainControl(method = train_method, number = 20, repeats = 10)
		tunegrid <- expand.grid(.mtry=c(1:maxmtry))

		if(!is.null(useweights) & useweights==TRUE){
			if(class_method1 %in% c("rf","ranger")){
				if(!is.null(node)){
					fit <- train(training[,-1], training[,1], method = class_method1, trControl=trctrl,weights=weightsvec,tuneGrid=tunegrid)
				}else{
					fit <- train(training[,-1], training[,1], method = class_method1, trControl=trctrl,weights=weightsvec,tuneGrid=tunegrid)
				}
			}else{
				fit <- train(training[,-1], training[,1], method = class_method1, trControl=trctrl,weights=weightsvec)
			}
		} else {
			if(class_method1 %in% c("rf","ranger")){
				if(!is.null(node)){
					fit <- train(training[,-1], training[,1], method = class_method1, trControl=trctrl,tuneGrid=tunegrid)
				}else{
					fit <- train(training[,-1], training[,1], method = class_method1, trControl=trctrl,tuneGrid=tunegrid)
				}
			}else{
				fit <- train(training[,-1], training[,1], method = class_method1, trControl=trctrl)
			}
		}

		for(upos in uniquenames){
			predict_indep_mat <- FZremovena(predictionmx[predictionmx[,2] == upos,])
			if(nrow(predict_indep_mat) != 0){
				colnames(predict_indep_mat)[-c(1,2)] <- colnames(testing[,-1]) 
				Prediction <- predict(fit, newdata = predict_indep_mat[,-c(1,2)]) 
				outmx[predict_indep_mat[,1],upos] <- as.character(Prediction)
			}
		}
	}
	return(outmx)
}






FZhiClassPerf <- function(training=NULL,prediction=NULL,factors=NULL,title=NULL,Growthper=0){
	if(is.null(training) | is.null(prediction) | is.null(factors)){stop("FZhiperformance: Input is NULL")}
	uniqueclass <- unique(c(training)[!is.na(c(training))])

	if(Growthper != 0){
		incprediction <- ifelse(prediction == "GROWTH",1,0)
		incprediction <- FZmovingavg(incprediction,maper = Growthper)
		prediction <- ifelse(incprediction != 0, "GROWTH",prediction)
	}

	#Confusion Table
	outmx <- sapply(1:length(uniqueclass),FUN=function(nthclass){
		tempoutmx <- rep(0,length(uniqueclass))
		names(tempoutmx) <- paste(uniqueclass,"(Pred)")
		classid <- ifelse(!is.na(training) & training == uniqueclass[nthclass],prediction,NA)
		tempcount <- table(c(classid))
		tempoutmx[paste(names(tempcount),"(Pred)")] <- tempcount
		tempoutmx
	})
	colnames(outmx) <- paste(uniqueclass,"(Train)")

	#Performance for Training Set
	dev.new()
	par(mfrow=c(length(factors),1))
	for(nthfactor in 1:length(factors)){
		trainfactor <- matrix(NA,nrow=length(uniqueclass),ncol=ncol(prediction))
		rownames(trainfactor) <- uniqueclass
		colnames(trainfactor) <- colnames(prediction)

		trainfactor <- sapply(1:length(uniqueclass),FUN=function(nthclass){
		sapply(1:ncol(training),FUN=function(nthcol){
			median(factors[[nthfactor]][training[,nthcol] == uniqueclass[nthclass],nthcol],na.rm=TRUE)})
		})
		matplot(trainfactor, type = "l",col = rainbow(ncol(trainfactor)),ylab = paste(names(factors)[nthfactor],"(Train)",sep=""))
		if(nthfactor == 1){
			title(main = paste(title,"Training Set"),xlab = "X axis", ylab = "Y axis",font.main= 4,col.main= "black",cex.sub = 0.75)
			legend("topleft", legend = uniqueclass, ncol = 2,cex = 0.75,pch=1,col=rainbow(ncol(trainfactor)))
		}
	}

	#Performance for Prediction Set
	dev.new()
	par(mfrow=c(length(factors),1))
	for(nthfactor in 1:length(factors)){
		predfactor <- matrix(NA,nrow=length(uniqueclass),ncol=ncol(prediction))
		rownames(predfactor) <- uniqueclass
		colnames(predfactor) <- colnames(prediction)


		predfactor <- sapply(1:length(uniqueclass),FUN=function(nthclass){
			sapply(1:ncol(training),FUN=function(nthcol){
				median(factors[[nthfactor]][prediction[,nthcol] == uniqueclass[nthclass],nthcol],na.rm=TRUE)})
		})
		matplot(predfactor, type = "l",col = rainbow(ncol(predfactor)),ylab = paste(names(factors)[nthfactor],"(Prediction)",sep=""))
		if(nthfactor == 1){
			title(main = paste(title,"Prediction Set"),xlab = "X axis", ylab = "Y axis",font.main= 4,col.main= "black",cex.sub = 0.75)
			legend("topleft", legend = uniqueclass, ncol = 2,cex = 0.75,pch=1,col=rainbow(ncol(predfactor)))
		}
	}

	Sensitivity <- sapply(1:length(uniqueclass),FUN=function(nthclass){
		outmx[nthclass,nthclass]/sum(outmx[,nthclass],na.rm=TRUE)
	})
	names(Sensitivity) <- uniqueclass

	Specificity <- sapply(1:length(uniqueclass),FUN=function(nthclass){
		(sum(outmx[,-nthclass],na.rm=TRUE) - sum(diag(outmx)[-nthclass]))/sum(outmx[,-nthclass],na.rm=TRUE)
	})
	names(Specificity) <- uniqueclass

	par(mfrow=c(1,1))
	return(list(Condusion = outmx,Sensitivity = Sensitivity,Specificity = Specificity))
}




FZfundamentalvolatility <- function(input=NULL, volper=3, fundamentalper=1){
	if(is.null(input) | is.null(volper) | is.null(fundamentalper)){stop("FZfundamentalvolatility: Some Para's are NULL")}
	outmx <- sapply(volper:ncol(input),function(x){apply(input[,(x-volper+1):x],1,sd,na.rm=TRUE)*sqrt(fundamentalper)})
	outmx <- cbind(matrix(NA,nrow=nrow(input), ncol=(volper-1)),outmx)
	dimnames(outmx) <- dimnames(input)
	outmx
}





FZEVAadjustforpara <- function(input=NULL,adjustbasemx=NULL){
	if(is.null(input)| is.null(adjustbasemx)){stop("FZEVAadjustforpara: Some Para's are NULL")}
	inputAdjMedian <- apply(input,2,function(y){
					mdval <- ave(y,adjustbasemx,FUN=function(z){median(z,na.rm=TRUE)})
					mdval
				})
	inputMedian <- apply(input,2,median,na.rm=TRUE)
	inputAdj <- t(inputMedian-t(inputAdjMedian))*0.5
	output <- input+inputAdj
	output
}



FZEVAbetacalc <- function(rets=NULL, country=NULL, mcapdata=NULL, betaper=104){
	if(is.null(rets)| is.null(country)|is.null(betaper)|is.null(mcapdata)){stop("FZEVAbetacalc: Some Para's are NULL")}
	
	MCAPdaily <- matrix(NA,nrow=nrow(rets),ncol=ncol(rets))
	multiply <- ncol(rets)/ncol(mcapdata)
	for (col in 0:(ncol(mcapdata)-1)){
		MCAPdaily[,col*multiply+1] <- mcapdata[,col+1]
	}
	MCAPdaily <- FZfillwithlast(input=MCAPdaily)
	incl <- ifelse(!is.na(rets)&!is.na(MCAPdaily),1,NA)
	mcapincl <- MCAPdaily*incl
	retsincl <- rets*incl
	weights <- t(t(mcapincl)/colSums(mcapincl,na.rm=TRUE))

	indexseries <- colSums(weights*retsincl,na.rm=TRUE)

	new <- c(NA,indexseries[1:(length(indexseries)-1)])
	change <- (indexseries/new)-1
	betacalcoutmx <- matrix(NA, nrow = nrow(rets), ncol = ncol(rets))
	for(colpos in betaper:ncol(rets)){
		curmx <- rets[,(colpos-betaper+1):colpos]
		tempindexseries <- change[(colpos-betaper+1):colpos]
		covarmx <- apply(curmx,1,function(x){cov(x,tempindexseries,use="pairwise.complete.obs")/var(tempindexseries,na.rm=TRUE)})
		betacalcoutmx[,colpos] <- covarmx
	}

	Totalmedian <- apply(betacalcoutmx,2,median,na.rm=TRUE)
	Countrymedian <- apply(betacalcoutmx,2,function(y){
				val <- ave(y,country,FUN=function(z){median(z,na.rm=TRUE)})
				val
				})

	adjustments <- (Totalmedian-Countrymedian)*0.5    # Adjfac=0.5
	betacalcoutmxnew <- betacalcoutmx-adjustments
	for (colpos in betaper:ncol(betacalcoutmxnew)){
		standardoutput <- (betacalcoutmxnew[,colpos]-mean(betacalcoutmxnew[,colpos],na.rm=TRUE))/sd(betacalcoutmxnew[,colpos],na.rm=TRUE)
		if ( abs(standardoutput) > 2){
			betacalcoutmx[,colpos] <- mean(betacalcoutmxnew[,colpos],na.rm=TRUE)+2*sd(betacalcoutmxnew[,colpos],na.rm=TRUE)*sign(betacalcoutmxnew[,colpos])
		}
	}

	dimnames(betacalcoutmxnew) <- dimnames(rets)
	#betacalcoutmxnew[!is.na(betacalcoutmxnew) & betacalcoutmxnew < 0] <- NA
	#betacalcoutmxnew <- FZfillwithlast(input=betacalcoutmxnew)
	betacalcoutmxnew
}



FZEVAstockfundamentals <- function(stockid=NULL, mcapmx=NULL, salesmx=NULL, netincmx=NULL){

	stockmcap <- mcapmx[stockid,]
	stocksales <- salesmx[stockid,]
	stocknetinc <- netincmx[stockid,]
	
	mcapall <- colSums(mcapmx, na.rm=TRUE)
	salesall <- colSums(salesmx,na.rm=TRUE)
	netincall <- colSums(netincmx,na.rm=TRUE)
	
	rbindall <- rbind(stockmcap,stocksales,stocknetinc,mcapall,salesall,netincall)
	rbindallabs <- abs(rbindall)
	outfile <- (rbindallabs/FZshiftmx(input=rbindallabs,shiftcol=-1))*sign(rbindall) # to deal with negative netinc
	#outfile <- rbindall/FZshiftmx(input=rbindall,shiftcol=-1)

	mcaprel <- outfile[1,]/outfile[4,]
	salesrel <- outfile[2,]/outfile[5,]
	netincrel <- outfile[3,]/outfile[6,]
	outfile <- rbind(outfile,mcaprel,salesrel,netincrel)
	outfile[is.na(outfile)] <- 1
	outfile <- apply(outfile,1,cumprod)
	outfile
}

FZEvasum <- function(input1=NULL,input2=NULL){
	if(is.null(input1) | is.null(input2)){stop("FZEvasum: Some Para's are NULL")}
	if(is.na(input1) | is.na(input2)){stop("FZEvasum: Some Para's are NA")}
	sum <- (input1+input2)
	sum
}

FZEvastanvarbypara <- function(inputvar=NULL, paramx=NULL, Adjfac=0.5){
	if(is.null(inputvar)| is.null(paramx)){stop("FZEvastanvarbypara: Some Para's are NULL")}
	#cnames <- colnames(inputvar)
	#paralist <- unique(cnames)
	
	SDinput <- apply(inputvar,2,function(x){
		outval <- (x-mean(x,na.rm=TRUE))/sd(x,na.rm=TRUE)
		outval
	})

	MDinput <- apply(SDinput,2,function(y){
			mdval <- ave(y,paramx,FUN=function(z){median(z,na.rm=TRUE)})
			mdval
	})

	sdinputcolmed <- apply(SDinput,2,median,na.rm=TRUE)
	adjmd <- t(t(MDinput)-sdinputcolmed)
	adjmd <- adjmd*Adjfac
	outmx <- SDinput-adjmd
	outmx
}

FZEvaMovAvg <- function(input=NULL,maper=NULL){
	if(is.null(input)|is.null(maper)){stop("FZEvaMovAvg: Some Para's are NULL")}
	input[is.na(input)] <- 0
	if(is.vector(input)){
		cumsuminput <- cumsum(input)
		cumsuminputshift <- c(rep(0,maper),cumsuminput[1:(length(input)-maper)])
		countvec <- ifelse(!is.na(input) & input != 0, 1, 0)
		countvec <- cumsum(countvec)
		countvec[countvec > maper] <- maper
		mapersum <- (cumsuminput-cumsuminputshift)/countvec
		mapersum[!is.finite(mapersum) | is.nan(mapersum) | is.na(mapersum)] <- NA
	} 
	if(is.matrix(input)){
		cumsuminput <- t(apply(input,1,cumsum))
		cumsuminputshift <- FZshiftmx(input=cumsuminput, shiftcol=-maper)
		countvec <- ifelse(!is.na(input) & input != 0, 1, 0)
		countvec <- t(apply(countvec,1,cumsum))
		countvec[countvec > maper] <- maper
		mapersum <- (cumsuminput-cumsuminputshift)/countvec
		mapersum[!is.finite(mapersum) | is.nan(mapersum) | is.na(mapersum)] <- NA
	}
	
	mapersum
}


FZEVArange <- function(input=NULL,rangeper=NULL,shift=FALSE){
	if(is.null(input)){stop("FZrange: Input is NULL")}
	input <- FZzerona(input=input)
	dummymx <- matrix(NA,nrow=nrow(input),ncol=(rangeper-1))
	expinput <- cbind(dummymx,input,dummymx)
	FZrangeoutputmx <- matrix(NA,nrow(expinput),ncol(expinput))
	for (colpos in rangeper:(ncol(input)+rangeper-1)){
		ifelse (!is.null(shiftF) & shiftF==TRUE,curmx <- expinput[,colpos:(colpos+rangeper-1)],curmx <- expinput[,(colpos-rangeper+1):colpos])	
		maxvals <- apply(curmx,1,max,na.rm=TRUE)
		minvals <- apply(curmx,1,min,na.rm=TRUE)
		FZrangeoutputmx[,colpos] <- maxvals-minvals	
	}
	outputmx <- FZrangeoutputmx[,rangeper:(ncol(input)+rangeper-1)]
	ifelse(!is.null(shiftF) & shiftF==TRUE,outputmx[,(ncol(outputmx)-rangeper+2):ncol(outputmx)]<-NA,outputmx[,1:(rangeper-1)]<-NA)
	outputmx
}



FZEVAcapplitemBSitemNET <- function(input=input, capper = 3){
	if(is.null(input)|is.null(capper)){stop("FZcapplitemBSitem: Some para's are NULL")}
	inputnona <- FZzerona(input=input)
	BSGROSS <- FZmovingavg(input=inputnona,maper=capper)*capper
	BSGROSS[,1:capper] <- FZcummx(input=inputnona)[,1:capper]
	PLcharge <- BSGROSS/3
	cumdepn <- FZmovingavg(input=PLcharge,maper=capper)*capper
	cumdepn[,1:capper] <- FZcummx(input=PLcharge)[,1:capper]
	cumdepn[,(capper+1):ncol(cumdepn)] <- cumdepn[,(capper+1):ncol(cumdepn)]-inputnona[,1:(ncol(cumdepn)-capper)]
	BSnet <- BSGROSS-cumdepn	
	BSnet
}



FZEVAcapplitemBSitemGROSS <- function(input=input, capper = 3){
	if(is.null(input)|is.null(capper)){stop("FZcapplitemBSitem: Some para's are NULL")}
	inputnona <- FZzerona(input=input)
	BSGROSS <- FZmovingavg(input=inputnona,maper=capper)*capper
	BSGROSS[,1:capper] <- FZcummx(input=inputnona)[,1:capper]
	BSGROSS
}





FZEVAcapplitemPLitem <- function(input=input, capper = 3){
	if(is.null(input)|is.null(capper)){stop("FZcapplitemBSitem: Some para's are NULL")}
	inputnona <- FZzerona(input=input)
	BSGROSS <- FZmovingavg(input=inputnona,maper=capper)*capper
	BSGROSS[,1:capper] <- FZcummx(input=inputnona)[,1:capper]
	PLcharge <- BSGROSS/3
	PLcharge

}



FZEVAcompareinclusionmx <- function(inputold=NULL, inputnew=NULL, targetvar=NULL){
	if(is.null(inputold)| is.null(inputnew)|is.null(targetvar)){stop("FZEvacompareinclusionmx: Some Para's are NULL")}
	outlist <- list()
	listOLD <- list()
	listNEW <- list()
	for (checkcol in 1:ncol(inputold)){
		old <- inputold[,checkcol]
		new <- inputnew[,checkcol]
		onlyold <- new[is.na(new) & old == 1]
		onlynew <- old[is.na(old) & new == 1]
		ttnames <- unique(c(names(onlyold),names(onlynew)))
		listA <- ttnames[ttnames%in%names(onlyold)]
		listB <- ttnames[ttnames%in%names(onlynew)]
		output <- list(listA[!is.na(listA)],listB[!is.na(listB)])
		outlist[[colnames(targetvar)[checkcol]]] <- output
		listOLD[[colnames(targetvar)[checkcol]]] <- listA[!is.na(listA)]
		listNEW[[colnames(targetvar)[checkcol]]] <- listB[!is.na(listB)]
	}

	outmed <- rbind(apply(inputold*targetvar,2,median,na.rm=TRUE),apply(inputnew*targetvar,2,median,na.rm=TRUE))
	outmean <- rbind(apply(inputold*targetvar,2,mean,na.rm=TRUE),apply(inputnew*targetvar,2,mean,na.rm=TRUE))
	outmin <- rbind(apply(inputold*targetvar,2,min,na.rm=TRUE),apply(inputnew*targetvar,2,min,na.rm=TRUE))
	outmax <- rbind(apply(inputold*targetvar,2,max,na.rm=TRUE),apply(inputnew*targetvar,2,max,na.rm=TRUE))
	outcolSums <- rbind(colSums(inputold,na.rm=TRUE),colSums(inputnew,na.rm=TRUE))

	outfull <- list(outlist,listOLD,listNEW,outmed,outmean,outmin,outmax,outcolSums)
	outfull
}




FZEVAinflAdjGrossPPE <- function(GrossPPEmx=NULL,AccmuDepnmx=NULL,Depnmx=NULL,Inflationmx=NULL,Countrymx=NULL){
	if(is.null(GrossPPEmx)|is.null(AccmuDepnmx)|is.null(Depnmx)|is.null(Inflationmx)|is.null(Countrymx)){stop("FZEVAGrossPPEadj: Some Para's are NULL")}
	#Depnmxnona <-  FZzerona(input=Depnmx)
	MAvgDepnmx <- FZmovingavg(input = Depnmx, maper=3)
	MAvgDepnmx[,1] <- Depnmx[,1]
	MAvgDepnmx[,2] <- (Depnmx[,1]+ Depnmx[,2])/2
	MAvgDepnmx[,3] <- (Depnmx[,1]+ Depnmx[,2]+Depnmx[,3])/3

	mxinf <- matrix(NA, nrow=nrow(Inflationmx), ncol=ncol(Inflationmx), dimnames = dimnames(Inflationmx))
		tempinfmx <- sapply(1:ncol(mxinf),function(x){
		tempinf <- Inflationmx[match(names(Country),rownames(Inflationmx)),x]
		tempinf 
	})
	inflation <- (tempinfmx/FZshiftmx(input=tempinfmx,shiftcol=-1))-1
	rownames(inflation) <- rownames(Country)

	adj <- AccmuDepnmx/MAvgDepnmx
	factor <- (1+inflation)^adj
	InflAdjGrossPPE <- GrossPPEmx*factor
	InflAdjGrossPPE
}


FZEVAinflAdjDepn <- function(GrossPPEmx=NULL,AccmuDepnmx=NULL,Depnmx=NULL,Inflationmx=NULL,Countrymx=NULL){
	if(is.null(GrossPPEmx)|is.null(AccmuDepnmx)|is.null(Depnmx)|is.null(Inflationmx)|is.null(Countrymx)){stop("FZEVAGrossPPEadj: Some Para's are NULL")}
	Depnmxnona <-  FZzerona(input=Depnmx)
	Depnfactor <- GrossPPEmx/Depnmx
	InflAdjGrossPPE <- FZEVAinflAdjGrossPPE(GrossPPEmx=GrossPPEmx,AccmuDepnmx=AccmuDepnmx,Depnmx=Depnmx,Inflationmx=Inflationmx,Countrymx=Countrymx)
	InflAdjDepn <- InflAdjGrossPPE/(GrossPPEmx/Depnmx)
	InflAdjDepn
}





FZEVAdatastandardize <- function(input=NULL){
	if(is.null(input)){stop("FZEVAdatastandardize: INPUT is NULL")}
	output <- apply(input,2,function(x){
			outval <- (x-mean(x,na.rm=TRUE))/sd(x,na.rm=TRUE)
			outval
			})
output
}




FZEVAtestvariable <- function(inputvar=NULL, factortest=NULL,bands=5,chartlabel="Chart1"){
	if(is.null(inputvar)|is.null(factortest)|is.null(bands)){stop("FZbandret: Some Para's are NULL")}
	bandret <- FZbandret(inputfactor=inputvar,bands=bands,returns=factortest)
	bandretmean <- apply(bandret,2,mean,na.rm=TRUE)
	bandretmax <- max(apply(bandret,1,median,na.rm=TRUE))
	bandretmin <- min(apply(bandret,1,median,na.rm=TRUE))
	bandretspread <- bandretmax-bandretmin
	bandretrescale <- bandret-matrix(rep(bandretmean,bands),nrow=bands,byrow=TRUE)
	fit <- lm(as.vector(factortest) ~ as.vector(inputvar),na.action=na.exclude)
	dev.new()
	par(mfrow=c(1,2))
	barplot(apply(bandret,1,median,na.rm=TRUE),main=paste(chartlabel,round(summary(fit)$adj.r.squared, digits=8),sep=": "),xlab=paste("bands",bandretspread),ylab="factor")
	barplot(apply(bandretrescale,1,median,na.rm=TRUE),xlab="bands",ylab="rescaled factor")
	fit
}




FZgetbands <- function(input=NULL,bands=NULL){
	if(is.null(input)| is.null(bands)){stop("FZgetbands: some para's are null")}
	if(is.matrix(input)){
	perc <- apply(input,2,function(x){
		percrank <- floor(rank(x,na.last="keep")/(length(x[!is.na(x)])+1)*bands)+1
		percrank
		})
	}else{
		perc <- floor(rank(input,na.last=NA)/(length(input[!is.na(input)])+1)*bands)+1
	}
	perc
}




FZbandret <- function(inputfactor = NULL, bands=NULL, returns=NULL){
	if(is.null(inputfactor)|is.null(bands)|is.null(returns)){stop("FZbendret: Some Para's are NULL")}
	bandmx <- FZgetbands(input=inputfactor,bands=bands)
	bandvals <- c(1:bands)
	bandret <- matrix(NA,nrow=length(bandvals),ncol=ncol(inputfactor),dimnames=list(bandvals,colnames(inputfactor)))
	for (i in 1:bands){
		tempmx <- bandmx
		tempmx[!is.na(tempmx)&tempmx!=i] <- NA
		tempmx[tempmx==i] <- 1
		avgbandret <- apply(tempmx*returns,2,median,na.rm=TRUE)
		bandret[i,] <- avgbandret
		
	}
	bandret
}




FZbandchange <- function(input=NULL,shiftcol=NULL, bands=NULL){
	if(is.null(input)|is.null(shiftcol)|is.null(bands)){stop("FZbandchange: some para's are null")}
	input <- input
	shiftinput <- FZshiftmx(input = input, shiftcol = shiftcol)	
	inputchange <- shiftinput/input
	bandchange <- FZgetbands(input=inputchange,bands=bands)
	bandchange
}




FZEVAcreateDFforanalysisweighted <- function(manualclassmx=NULL,balanceclass="GROWTH",...){
	argument <- list(...)
	set.seed(82)
	if(length(argument)==0|is.null(manualclassmx)| is.null(balanceclass)){stop("FZEVAcreateDFforanalysis: Some Para's are NULL")}
	unlistargument <- unlist(argument)
	argumentmx <- matrix(unlistargument, ncol=length(argument),byrow=FALSE)
	fulldf <- data.frame(as.vector(manualclassmx),argumentmx)
	names <- outer(rownames(data.frame(manualclassmx)),colnames(data.frame(manualclassmx)), function(x,y) paste(x,y,sep="_"))
	rownames(fulldf) <- as.vector(names)
	fulldf <- FZremovena(fulldf)
	fulldf
}




#############################################FINISHED FUNCTIONS BELOW##################################################

FZadjustvarbypara <- function(inputvar=NULL, paramx=NULL, Adjfac=0.5){
	if(is.null(inputvar)| is.null(paramx)){stop("FZEvastanvarbypara: Some Para's are NULL")}
	MDinput <- apply(inputvar,2,function(y){
			mdval <- ave(y,paramx,FUN=function(z){median(z,na.rm=TRUE)})
			mdval
	})

	inputcolmed <- apply(inputvar,2,median,na.rm=TRUE)
	adjmd <- t(t(MDinput)-inputcolmed)
	adjmd <- adjmd*Adjfac
	outmx <- inputvar+adjmd
	outmx
}






FZstandardisevarbycol <- function(input=NULL){
	if(is.null(input)){stop("FZstandardisevarbycol: Some Para's are NULL")}
	inputout <- apply(input,2,function(x){
				outval <- (x-mean(x,na.rm=TRUE))/sd(x,na.rm=TRUE)
				outval
			})
	inputout

}





FZrange <- function(input=NULL,rangeper=NULL){
	if(is.null(input)|is.null(rangeper)){stop("FZrange: Input is NULL")}
	rangepermx <- sapply(rangeper:ncol(input), function(x){
				tempmx <- input[,(x-rangeper+1):x]
				maxvals <- apply(tempmx,1,max,na.rm=TRUE)
				minvals <- apply(tempmx,1,min,na.rm=TRUE)
				meanvals <- apply(tempmx,1,median,na.rm=TRUE)
				outvals  <- (maxvals-minvals)/meanvals
				outvals
			  })
	rangepermx[is.na(rangepermx) | is.nan(rangepermx) | !is.finite(rangepermx)] <- NA
	rangepermx <- cbind(matrix(NA,nrow=nrow(input),ncol=rangeper-1),rangepermx)
	dimnames(rangepermx) <- dimnames(input)
	rangepermx
	
}




FZpriceVolatilitywithper <- function(input=NULL, volper=52){
	if(is.null(input)){stop("FZpriceVolatility: Input is NULL")}
	fulloutmx <- matrix(NA, nrow=nrow(input), ncol=ncol(input))
	for (ecolpos in volper:(ncol(input))){
		scolpos <- ecolpos-volper+1
		tempmx <- input[,scolpos:ecolpos]
		fulloutmx[,ecolpos] <- apply(tempmx,1,sd,na.rm=TRUE)*sqrt(volper)
	}

	dimnames(fulloutmx) <- dimnames(input)
	eoymx <- FZgetyearends (input=fulloutmx)
	eoyvec <- colSums(eoymx ,na.rm=TRUE)
	eoyonly <- fulloutmx[,c(names(eoyvec[eoyvec!=0]))]
	eoyonly
}





FZgetyearends <- function(input=NULL){
	require(lubridate)
	#Will create a matrix of 1 & 0's, with 1 representing the last day of the month
	if(is.null(input)){stop("FZgetmonthends: Inputis NULL")}
	cnames <- colnames(input)
	cnamestemp <- sapply(cnames,function(x){gsub("X","",x)})

	ttalldates <- as.Date(substring(cnamestemp ,(nchar(cnamestemp[1])-7),nchar(cnamestemp [1])),format="%Y%m%d")
	ttnextdate <- c(ttalldates[2:length(ttalldates)],NA)
	eomvec <- ifelse(!is.na(ttalldates) & !is.na(ttnextdate) & year(ttnextdate)>year(ttalldates)| year(ttnextdate)> year(ttalldates),1,0)
	eommx <- matrix(eomvec,nrow=nrow(input), ncol=ncol(input),byrow=TRUE)
	dimnames(eommx) <- dimnames(input)
	eommx
}





FZmovingSD <- function(input=NULL, sdper=NULL){

	if(is.null(input)|is.null(sdper)){stop("FZmovingSD: Some Para's are NULL")}
	outmx <- matrix(NA,nrow=nrow(input),ncol=ncol(input))
	for (colpos in sdper:ncol(input)){
		curmx <- input[,(colpos-sdper+1):colpos]
		outvals <- apply(curmx,1,sd)# na.rm=TRUE??????
		outmx[,colpos] <- outvals
	}
	dimnames(outmx) <- dimnames(input)
	outmx
}





FZtrimvals <- function(input=NULL,trimval=2,naval=TRUE){

	if(is.null(input)|is.null(trimval)){stop("FZscalemx: Some Para's are NULL")}
	outmx <- apply(input,2,function(curvals){
		meaninput <- mean(curvals,na.rm=TRUE)
		sdinput <- sd(curvals,na.rm=TRUE)
		upperlvl <- meaninput + (trimval*sdinput)
		lowerlvl <- meaninput - (trimval*sdinput)
		curvals[!is.na(curvals) & curvals > upperlvl] <- NA
		curvals[!is.na(curvals) & curvals < lowerlvl] <- NA
		curvals		
	})
	outmx 

}





FZtrailingmax <- function(input=NULL){
	if(is.null(input)){stop("FZtrailingmax: Input is NULL")}
	outputmx <- apply(input,1,function(inputvec){
		invec <- sapply(2:length(inputvec),function(xpos){
			ifelse(!is.na(inputvec[xpos]), trailingmax <- max(inputvec[1:xpos],na.rm=TRUE),trailingmax <- NA)
			trailingmax
		})
	})
	outputmx <- cbind(input[,1],t(outputmx))
	dimnames(outputmx) <- dimnames(input)
	outputmx
}






FZtrailingmin <- function(input=NULL){
	if(is.null(input)){stop("FZtrailingmin: Input is NULL")}
	outputmx <- apply(input,1,function(inputvec){
		invec <- sapply(2:length(inputvec),function(xpos){
			ifelse(!is.na(inputvec[xpos]), trailingmin <- min(inputvec[1:xpos],na.rm=TRUE),trailingmin <- NA)
			trailingmin
			})
		})
	outputmx <- cbind(input[,1],t(outputmx))
	outputmx
}






FZcapplitemBSitemNET <- function(input=input, capper = 3){
	if(is.null(input)|is.null(capper)){stop("FZcapplitemBSitem: Some para's are NULL")}
	inputnona <- FZzerona(input=input)
	BSGROSS <- FZcapplitemBSitemGROSS (input=inputnona, capper = capper)
	PLcharge <- FZcapplitemPLitem(input=inputnona, capper = capper)
	cumdepn <- FZmovingavg(input=PLcharge,maper=capper)*capper
	cumdepn[,1:capper] <- FZcummx(input=PLcharge)[,1:capper]
	cumdepn[,(capper+1):ncol(cumdepn)] <- cumdepn[,(capper+1):ncol(cumdepn)]-inputnona[,1:(ncol(cumdepn)-capper)]
	BSnet <- BSGROSS-cumdepn	
	BSnet
}






FZtestvariable <- function(inputvar=NULL, factortest=NULL,bands=5,chartlabel="Chart1"){
	if(is.null(inputvar)|is.null(factortest)|is.null(bands)){stop("FZbandret: Some Para's are NULL")}
	bandret <- FZbandret(inputfactor=inputvar,bands=bands,returns=factortest)
	bandretmean <- apply(bandret,2,mean,na.rm=TRUE)
	bandretmax <- max(apply(bandret,1,median,na.rm=TRUE))
	bandretmin <- min(apply(bandret,1,median,na.rm=TRUE))
	bandretspread <- bandretmax-bandretmin
	bandretrescale <- bandret-matrix(rep(bandretmean,bands),nrow=bands,byrow=TRUE)
	fit <- lm(as.vector(factortest) ~ as.vector(inputvar),na.action=na.exclude)
	dev.new()
	par(mfrow=c(1,2))
	barplot(apply(bandret,1,median,na.rm=TRUE),main=paste(chartlabel,round(summary(fit)$adj.r.squared, digits=8),sep=": "),xlab=paste("bands",bandretspread),ylab="factor")
	barplot(apply(bandretrescale,1,median,na.rm=TRUE),xlab="bands",ylab="rescaled factor")
	fit
}





FZclassificationwithpredweighted <- function(input=NULL, predictmat=NULL, training_portion = 0.7, class_method = "ranger", train_method = "repeatedcv", rownamesid = rownames(PLSALES)){
	if(is.null(input)|is.null(predictmat)){stop("FS_classification: Input is NULL")}
	require(caret)
	set.seed(82)
	### Remove NA values
	input   <- FZremovena(input)
	predict_indep_mat <- FZremovena(predictmat)

	intrain  <- createDataPartition(y = input[,1], p = training_portion, list = FALSE)
	training <- input[ intrain,]
	testing <- input[-intrain,]
	growthstocklength <- length(which(!is.na(training[,1]) & training[,1] == "GROWTH"))
	maturestocklentgh <- max(table(inputtraingrowthEVAtemp[,1]))
	weightsgrowth <- maturestocklentgh/growthstocklength
	weightsvec <- ifelse(!is.na(training[,1]) & training[,1] == "GROWTH", weightsgrowth,1)
	weightsvec <- weightsvec/sum(weightsvec)
	trctrl <- trainControl(method = train_method, number = 10, repeats = 3)
	fit <- train(training[,-1], training[,1], method = class_method, trControl=trctrl,weights=weightsvec)

	test_pred <- predict(fit, newdata = testing[,-1])
	Confusion_mat <- confusionMatrix(test_pred, testing[,1])
	
	#### prediction 
	colnames(predict_indep_mat) <- colnames(testing[,-1]) 
	Prediction <- predict(fit, newdata = predict_indep_mat) ### predict the future values
	rowposid <- as.numeric(c(rownames(predict_indep_mat)))
	rownamestouse <- rownamesid[rowposid]
	Prediction_table <- data.frame(rownames(predict_indep_mat), Prediction,rownamestouse)
	#rownames(Prediction_table) <- rownamestouse
	list(Prediction_Classes = Prediction_table, confusion_Matrix = Confusion_mat)
}





FZclassificationwithpred <- function(input=NULL, predictmat=NULL, training_portion = 0.7, class_method = "ranger", train_method = "repeatedcv", rownamesid = rownames(PLSALES)){
	if(is.null(input)|is.null(predictmat)){stop("FS_classification: Input is NULL")}
	require(caret)
	set.seed(82)
	### Remove NA values
	input   <- FZremovena(input)
	predict_indep_mat <- FZremovena(predictmat)

	intrain  <- createDataPartition(y = input[,1], p = training_portion, list = FALSE)
	training <- input[ intrain,]
	testing <- input[-intrain,]
	trctrl <- trainControl(method = train_method, number = 10, repeats = 3)
	fit <- train(training[,-1], training[,1], method = class_method, trControl=trctrl)
	test_pred <- predict(fit, newdata = testing[,-1])
	Confusion_mat <- confusionMatrix(test_pred, testing[,1])
	#### prediction 
	colnames(predict_indep_mat) <- colnames(testing[,-1]) 
	Prediction <- predict(fit, newdata = predict_indep_mat) ### predict the future values
	rowposid <- as.numeric(c(rownames(predict_indep_mat)))
	rownamestouse <- rownamesid[rowposid]
	Prediction_table <- data.frame(rownames(predict_indep_mat), Prediction,rownamestouse)
	#rownames(Prediction_table) <- rownamestouse
	list(Prediction_Classes = Prediction_table, confusion_Matrix = Confusion_mat)
}






FZrandomforestclassificationweighted <- function(input=NULL,WRFclasswt=NULL){
	# Weighted Random Forest function
	if(is.null(input)|is.null(WRFclasswt)){stop("FZrandomforestclassificationweighted: Some Para's are NULL")}
	require(VSURF)
	set.seed(42)  

	input <- FZremovena(input)
	response <- factor(input[,1])
	independant <- as.matrix(input[,-1])
	colnames(independant) <- colnames(input[2:ncol(input)])
  
	vsurfmodel <- VSURF(x=independant, y=response, parallel = TRUE, classwt= WRFclasswt,ntrees =2000)
	FZunregister()
	novartotal <- ncol(independant)
	VImatrix <- matrix(rep(0,2*novartotal), ncol = 2)
	VImatrix[,1] <- vsurfmodel$imp.mean.dec
	VImatrix[,2] <- vsurfmodel$imp.mean.dec.ind
	varselinterp <- vsurfmodel$varselect.interp
	varselpred <- vsurfmodel$varselect.pred
	outvals <- varselpred
	outmx <- data.frame(matrix(NA, nrow=length(outvals),ncol=2))
	outmx[,1] <- outvals
	outmx[,2] <- colnames(input[2:ncol(input)])[outvals]
	outmx
}





FZ_classificationweighted <- function(input=NULL, training_portion = 0.7, class_method = "ranger", train_method = "repeatedcv", ...){
### This function does classification in different classification algorithm with repeated cross validation 
## K nearest neighbour: "knn"
## Weighted Subspace Random Forests: "wsrf"
## Support vector machine: "svmLinear"
## Bagging : "AdaBag"
## Boosting : "adaboost"
## Random forest : "rf"

	set.seed(42)
	if(is.null(input)){stop("FS_classification: Input is NULL")}
	require(caret)
	### Remove NA values
	input   <- FZremovena(input)
  
	intrain  <- createDataPartition(y = input[,1], p = training_portion, list = FALSE)
	training <- input[ intrain,]
	testing  <- input[-intrain,]
	growthstocklength <- length(which(!is.na(training[,1]) & training[,1] == "GROWTH"))
	maturestocklentgh <- max(table(inputtraingrowthEVAtemp[,1]))
	weightsgrowth <- maturestocklentgh/growthstocklength
	weightsvec <- ifelse(!is.na(training[,1]) & training[,1] == "GROWTH", weightsgrowth,1)
	weightsvec <- weightsvec/sum(weightsvec)
	train_method <- ifelse(!is.na(class_method) & class_method=="rf", train_method <- "oob", train_method <- train_method) 
	trctrl   <- trainControl(method = train_method, number = 10, repeats = 3)
	fit      <- train(training[,-1], training[,1], method = class_method, trControl=trctrl,weights=weightsvec)
	test_pred     <- predict(fit, newdata = testing[,-1])
	Confusion_mat <- confusionMatrix(test_pred, testing[,1])
	Confusion_mat 
}






FZEVAMAvgLTM <- function(input=NULL,maper=NULL,chgper=NULL){
	if(is.null(input)|is.null(maper)| is.null(chgper)){stop("FZEVAMAvgMONDATA: Some Para's are NULL")}
	curmx <- input
	for (mapos in 1:(maper-1)){
		curmx <- curmx + FZshiftmx(input=input,shiftcol=-(mapos*chgper))
	}
	output <- curmx/maper
	output
}






FZEVAcapplitemBSitemGROSSLTM <- function(input=input,capper=4,chgper=3){
	if(is.null(input)|is.null(capper)){stop("FZEVAcapplitemBSitemGROSSMONDATA: Some para's are NULL")}
	inputnona <- FZzerona(input=input)
	BSGROSS <- FZEVAMAvgMONDATA(input=inputnona,maper=capper,chgper=chgper)*capper
	BSGROSS[,1:capper] <- FZcummx(input=inputnona)[,1:capper]
	BSGROSS
}







FZEVAreaddataMONDATA <- function(dataobjMON=NULL, dataobjANN=NULL, dirname="Datafiles"){
	if(is.null(dataobjMON)|is.null(dataobjANN)|is.null(dirname)){stop("FZreaddata: Some para's are null")}
	tt <- as.matrix(read.csv(paste(getwd(),"\\",(dirname=dirname),"\\",dataobjMON,".csv",sep=""),sep=",",header=TRUE,row.names=1,na.strings = "#N/A",stringsAsFactors = FALSE ))
	if (length(which(!is.na(tt)))==0){
		ttANN <- as.matrix(read.csv(paste(getwd(),"\\",(dirname=dirname),"\\",dataobjANN,".csv",sep=""),sep=",",header=TRUE,row.names=1,na.strings = "#N/A",stringsAsFactors = FALSE))
		tt <- FZgetmonthlyfromannual(ANNdata=ttANN, LTMdata=tt, shiftper=3)
	}
	tt
}







FZEVApriceVolatilitywithperMONDATA <- function(input=NULL, volper=12){
	if(is.null(input)){stop("FZpriceVolatility: Input is NULL")}
	fulloutmx <- matrix(NA, nrow=nrow(input), ncol=ncol(input))
	for (ecolpos in volper:(ncol(input))){
		scolpos <- ecolpos-volper+1
		tempmx <- input[,scolpos:ecolpos]
		fulloutmx[,ecolpos] <- apply(tempmx,1,sd,na.rm=TRUE)*sqrt(volper)
	}

	dimnames(fulloutmx) <- dimnames(input)
	#eoymx <- FZgetyearends (input=fulloutmx)
	#eoyvec <- colSums(eoymx ,na.rm=TRUE)
	#eoyonly <- fulloutmx[,c(names(eoyvec[eoyvec!=0]))]
	#eoyonly
	fulloutmx
}






FZreaddataLTM <- function(dataobj=NULL, dirname="Datafiles",shiftcol=NULL){
	if(is.null(dataobj)|is.null(dirname)|is.null(shiftcol)){stop("FZreaddataLTM: Some para's are null")}
	tt <- as.matrix(read.csv(paste(getwd(),"\\",(dirname=dirname),"\\",dataobj,".csv",sep=""),sep=",",header=TRUE,row.names=1,na.strings = "#N/A",stringsAsFactors = FALSE ))
	#if(!is.na(setnumeric)& setnumeric==TRUE){tt[,1:ncol(tt)]<-as.numeric(tt[,1:ncol(tt)])}
	if (length(which(!is.na(tt)))==0){
	ANNname <- paste(substr(dataobj,1,nchar(dataobj)-3),"ANN",sep="")
	tt1 <- as.matrix(read.csv(paste(getwd(),"\\",(dirname=dirname),"\\",ANNname,".csv",sep=""),sep=",",header=TRUE,row.names=1,na.strings = "#N/A",stringsAsFactors = FALSE ))
	tt <- tt1 %x% t(rep(1, 12))
	colnames(tt) <- rep(colnames(tt1),each=12)
	tt <- FZshiftmx(input=tt,shiftcol=-shiftcol)
	}
	tt
}





FZcummx <- function(input=NULL){
	if(is.null(input)){stop("FZcummx: Input is NULL")}
	input <- FZzerona(input=input)
	cummx <- t(apply(input,1,cumsum))
	cummx
}





FZhicummxLTM <- function(input=NULL,per=NULL){
	if(is.null(input)|is.null(per)){stop("FZhipersum: Input is NULL")}
	input <- FZzerona(input)

	sapply(1:ncol(input),FUN=function(nthcol){
		selectcol <- rev(-seq(-nthcol,-1,per))
		if(length(selectcol) > 1){
			apply(input[,selectcol],1,FUN=function(x){sum(x)})
		}else{
			input[,selectcol]
		}
	})
}





FZcapplitemBSitem <- function(input=input, capper = 3){
	if(is.null(input)|is.null(capper)){stop("FZcapplitemBSitem: Some para's are NULL")}
	inputnona <- FZzerona(input=input)
	BSGross <- (FZcummx(input=inputnona)-FZshiftmx(input=FZcummx(input=inputnona),shiftcol=-capper))
	Cumsum <- FZcummx(input=FZcummx(input=inputnona/capper))
	Accumdepn <- (Cumsum - FZshiftmx(input=BSGross,shiftcol=-capper))
	Netdepn <- (Cumsum - Accumdepn)
	Netdepn
}





FZcapplitemBSitemLTM <- function(input=input, capper = 3,shiftper=12){
	if(is.null(input)|is.null(capper)){stop("FZcapplitemBSitemLTM: Some para's are NULL")}
	inputnona <- FZzerona(input=input)
	BSGross <- (FZhicummxLTM(input=inputnona,per=shiftper)-FZshiftmx(input=FZhicummxLTM(input=inputnona,per=shiftper),shiftcol=-capper*shiftper))
	Cumsum <- FZhicummxLTM(input=FZhicummxLTM(input=inputnona/capper,per=shiftper),per=shiftper)
	Accumdepn <- (Cumsum - FZshiftmx(input=BSGross,shiftcol=-capper*shiftper))
	Netdepn <- (Cumsum - Accumdepn)
	Netdepn
}






FZcapplitemBSitemGROSSLTM <- function(input=input, capper = 3,shiftper=12){
	if(is.null(input)|is.null(capper)|is.null(shiftper)){stop("FZcapplitemBSitemLTM: Some para's are NULL")}
	inputnona <- FZzerona(input=input)
	BSGROSS <- FZhicummxLTM(input=inputnona,per=shiftper) - FZshiftmx(input=FZhicummxLTM(input=inputnona,per=shiftper),shiftcol=-capper*shiftper)
	BSGROSS[,1:(capper*shiftper)] <- FZhicummxLTM(input=inputnona,per=shiftper)[,1:(capper*shiftper)]
	BSGROSS
}





FZcapplitemPLitemLTM <- function(input=input, capper = 3,shiftper=12){
	if(is.null(input)|is.null(capper)){stop("FZcapplitemPLitemLTM: Some para's are NULL")}
	inputnona <- FZzerona(input=input)
	PLcharge <- FZcapplitemBSitemGROSSLTM(input=inputnona, capper = capper,shiftper=shiftper)/capper
	PLcharge
}






FZEVAinflAdjGrossPPELTM <- function(GrossPPEmx=NULL,AccmuDepnmx=NULL,Depnmx=NULL,Inflationmx=NULL,Countrymx=NULL){
	if(is.null(GrossPPEmx)|is.null(AccmuDepnmx)|is.null(Depnmx)|is.null(Inflationmx)|is.null(Countrymx)){stop("FZEVAinflAdjGrossPPELTM: Some Para's are NULL")}
	#Depnmxnona <-  FZzerona(input=Depnmx)
	MAvgDepnmx <- FZEVAMAvgLTM(input = Depnmx, maper=3,chgper=12)
	MAvgDepnmx[,1:12] <- Depnmx[,1:12]

	mxinf <- matrix(NA, nrow=nrow(Inflationmx), ncol=ncol(Inflationmx), dimnames = dimnames(Inflationmx))
	tempinfmx <- sapply(1:ncol(mxinf),function(x){
				tempinf <- Inflationmx[match(Country,gsub("[.]", " ",rownames(Inflationmx))),x]
				#tempinf <- Inflationmx[match(names(Country),rownames(Inflationmx)),x]
				tempinf
				})
	inflation <- (tempinfmx/FZshiftmx(input=tempinfmx,shiftcol=-12))-1
	rownames(inflation) <- rownames(Country)

	adj <- AccmuDepnmx/MAvgDepnmx
	factor <- (1+inflation)^adj
	InflAdjGrossPPE <- GrossPPEmx*factor
	InflAdjGrossPPE
}





FZEVAMAvgSKIP <- function(input=NULL,maper=NULL,chgper=NULL){
	if(is.null(input)|is.null(maper)| is.null(chgper)){stop("FZEVAMAvgMONDATA: Some Para's are NULL")}
	curmx <- input
	for (mapos in 1:(maper-1)){
		curmx <- curmx + FZshiftmx(input=input,shiftcol=-(mapos*chgper))
	}
	output <- curmx/maper
	output
}


FZhiSDSKIP <- function(input=NULL,maper=NULL,chgper=NULL){
	if(is.null(input)|is.null(maper)| is.null(chgper)){stop("FZhiSDSKIP: Some Para's are NULL")}

	input <- FZzerona(input)
	outmx <- matrix(NA,ncol = ncol(input),nrow=nrow(input))

	for (mapos in ((maper-1)*chgper+1):ncol(input)){
		outmx[,mapos] <- t(apply(input[,seq(mapos - (maper-1)*chgper,mapos,chgper)],1,sd,na.rm=TRUE))
	}
	dimnames(outmx) <- dimnames(input)
	outmx <- FZzerona(outmx)
	return(outmx)
}

FZgetmonthlyfromannual <- function(ANNdata=NULL, LTMdata=NULL, shiftper=3){
	if(is.null(ANNdata)|is.null(LTMdata)){stop("FZgetmonthlyfromannual: some para's are null")}
	outmx <- matrix(NA, nrow=nrow(LTMdata),ncol=ncol(LTMdata))#
	dimnames(outmx) <- dimnames(LTMdata)
	outmx[,colnames(ANNdata)] <- ANNdata[,colnames(ANNdata)]
	outmx <- FZfillwithlast(input=outmx)
	outmx <- FZnazero(input=outmx)
	outmx <- FZshiftmx(input=outmx, shiftcol=-(shiftper))
	dimnames(outmx) <- dimnames(LTMdata)
	outmx
}


FZ_logit_pvalue <- function(input=NULL){
	if(is.null(input)){stop("FS_logit_pvalue: Input is NULL")}
	coltouse <- ncol(input)
	#tab <- matrix(0, nrow = coltouse-1, ncol = 1)
	outmx <- apply(input[,2:ncol(input)],2,function(curvar){
		alldata <- FZremovena(data.frame(input[,1], curvar))
		model1 <- glm(as.factor(alldata[,1])~alldata[,2], family=binomial(link='logit'))
		summodel <- summary(model1)
		outval <- coef(summodel)[2,4]
	})
	outmx
}


FZEVAcapplitemBSitem <- function(input=input, capper = 3){
	if(is.null(input)|is.null(capper)){stop("FZcapplitemBSitem: Some para's are NULL")}
	inputnona <- FZzerona(input=input)
	BSGROSS <- FZmovingavg(input=inputnona,maper=capper)*capper
	BSGROSS[,1:capper] <- FZcummx(input=inputnona)[,1:capper]
	PLcharge <- BSGROSS/3
	cumdepn <- FZmovingavg(input=PLcharge,maper=capper)*capper
	cumdepn[,1:capper] <- FZcummx(input=PLcharge)[,1:capper]
	cumdepn[,(capper+1):ncol(cumdepn)] <- cumdepn[,(capper+1):ncol(cumdepn)]-inputnona[,1:(ncol(cumdepn)-capper)]
	BSnet <- BSGROSS-cumdepn
	BSnet
}

FZcumprodmx <- function(input=NULL){
	if(is.null(input)){stop("FZcummx: Input is NULL")}
	input <- FZzerona(input=input)
	cummx <- t(apply(1+input,1,cumprod))
	cummx
}




FZridgeregVec <- function(stockdatavec=NULL, indicatordata=NULL,maxvar=1,minvar=0){
# Calculates Lasso Regression for each stock
	if(is.null(stockdatavec) | is.null(indicatordata)){stop("FZlassoreg: Some Para's are NULL")}
	set.seed(82)
	indicatordata <- FZfillwithlast(input=indicatordata)
	minobs <- ceiling(nrow(indicatordata)*minvar)
	maxobs <- floor(nrow(indicatordata)*maxvar)
	templasso <- FZlimitdata(dependant = stockdatavec, indicatordata=indicatordata)
	lassocv <- cv.glmnet(x=t(templasso[2:nrow(templasso),]), y=templasso[1,], alpha = 0)
	lambda.fin <- lassocv$lambda.1se
	ttlassoresults <- glmnet(x=t(templasso[2:nrow(templasso),]), y=templasso[1,],  alpha = 0, type.gaussian="covariance", lambda=lambda.fin)
	templassooutput <- as.vector(ttlassoresults$beta)
	templassooutput
}


FZlassoregvec <- function(stockdatavec=NULL, indicatordata=NULL,maxvar=NULL,minvar=NULL){
# Calculates Lasso Regression for each stock
	if(is.null(stockdatavec) | is.null(indicatordata)){stop("FZlassoreg: Some Para's are NULL")}
	set.seed(82)
	indicatordata <- FZfillwithlast(input=indicatordata)
	minobs <- ceiling(nrow(indicatordata)*minvar)
	maxobs <- floor(nrow(indicatordata)*maxvar)
	templasso <- FZlimitdata(dependant = stockdatavec, indicatordata=indicatordata)
	lassocv <- cv.glmnet(x=t(templasso[2:nrow(templasso),]), y=templasso[1,])
	lambda.fin <- lassocv$lambda.1se
	ttlassoresults <- glmnet(x=t(templasso[2:nrow(templasso),]), y=templasso[1,], lambda=lambda.fin)
	templassooutput <- as.vector(ttlassoresults$beta)
	templassooutput
}



FZlimitdata <- function(dependant = NULL, indicatordata=NULL){
# Will chop a matrix to ensure it matches length of vector
# output will be a new matrix, first row is the dependant, others are the factor data
# assumes all data in indicaotrs are complete
	if(is.null(dependant) | is.null(indicatordata)){stop("FZlimitdata: Some Para's are NULL")}
	inputvec <- ifelse(is.na(dependant)|is.nan(dependant)|!is.finite(dependant),NA,1)
	dependant <- dependant*inputvec 
	indicatordata <- FZfillwithlast(input=indicatordata)
	indicatordata <- t(apply(indicatordata,1,function(x){x*inputvec}))
	tempmx <- rbind(dependant,indicatordata)
	tempmx <- tempmx[,!is.na(colSums(tempmx))]
	tempmx
}

FZbandretmedian <- function(inputfactor = NULL, bands=NULL, returns=NULL, shiftrets=NULL){
	if(is.null(inputfactor)|is.null(bands)|is.null(returns)|is.null(shiftrets)){stop("FZbendret: Some Para's are NULL")}
	bandmx <- FZgetbands(input=inputfactor,bands=bands)
	bandvals <- c(1:bands)
	bandret <- matrix(NA,nrow=length(bandvals),ncol=ncol(inputfactor),dimnames=list(bandvals,colnames(inputfactor)))
	for (i in 1:bands){
		tempmx <- bandmx
		tempmx[!is.na(tempmx)&tempmx!=i] <- NA
		tempmx[tempmx==i] <- 1
		avgbandret <- apply(tempmx*returns,2,median,na.rm=TRUE)
		bandret[i,] <- avgbandret
	}
	bandret
}

FZbandretmean <- function(inputfactor = NULL, bands=NULL, returns=NULL, shiftrets=NULL){
	if(is.null(inputfactor)|is.null(bands)|is.null(returns)|is.null(shiftrets)){stop("FZbendret: Some Para's are NULL")}
	bandmx <- FZgetbands(input=inputfactor,bands=bands)
	bandvals <- c(1:bands)
	bandret <- matrix(NA,nrow=length(bandvals),ncol=ncol(inputfactor),dimnames=list(bandvals,colnames(inputfactor)))
	for (i in 1:bands){
		tempmx <- bandmx
		tempmx[!is.na(tempmx)&tempmx!=i] <- NA
		tempmx[tempmx==i] <- 1
		avgbandret <- apply(tempmx*returns,2,mean,na.rm=TRUE)
		bandret[i,] <- avgbandret
	}
	bandret
}



FZcapplitemPLitem <- function(input=input, capper = 3){
	if(is.null(input)|is.null(capper)){stop("FZcapplitemBSitem: Some para's are NULL")}
	inputnona <- FZzerona(input=input)
	PLcharge <- FZcapplitemBSitemGROSS(input=inputnona, capper = 3)/3
	PLcharge
}



FZcapplitemBSitemGROSS <- function(input=input, capper = 3){
	if(is.null(input)|is.null(capper)){stop("FZcapplitemBSitem: Some para's are NULL")}
	inputnona <- FZzerona(input=input)
	BSGROSS <- FZcummx(input=inputnona)-FZshiftmx(input=FZcummx(input=inputnona),shiftcol=-capper)
	BSGROSS[,1:capper] <- FZcummx(input=inputnona)[,1:capper]
	BSGROSS
}

