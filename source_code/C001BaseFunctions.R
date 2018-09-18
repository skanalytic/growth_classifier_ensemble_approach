FZappenddata <- function(oldfile=NULL, newfile=NULL, dirname="Datafiles"){
	if(is.null(oldfile)|is.null(newfile)){stop("FZappenddata: Some Para's are NULL")}
	ofile <- FZfreaddata(dataobj=oldfile,dirname=dirname,numeric=TRUE)
	nfile <- FZfreaddata(dataobj=newfile,dirname=dirname,numeric=TRUE)
	
	interrownames <- intersect(rownames(ofile),rownames(nfile))
	intercolnames <- intersect(colnames(ofile),colnames(nfile))

	if(colnames(ofile)[ncol(ofile)] != colnames(nfile)[ncol(nfile)]){
		checkold <- ofile[interrownames,intercolnames[(length(intercolnames) - 2):length(intercolnames)]]
		checknew <- nfile[interrownames,intercolnames[(length(intercolnames) - 2):length(intercolnames)]]
		check <- checkold - checknew

		ContinueTest <- FALSE
		if(length(c(check[!is.na(check) & check != 0])) > 0.1*length(c(check[!is.na(check)]))){
			require(installr)
			ContinueTest <- ask.user.yn.question("FZappenddata: There are more than 10% historical values are different. Do you want to continue?")
			if(!ContinueTest){
				stop("FZappenddata: Please check data consistency.")
			}
		}

		if(ContinueTest == TRUE){
			ofile <- ofile[interrownames,]
			replace <- which(colnames(nfile) == intercolnames[length(intercolnames) - 2]):ncol(nfile)
			newdata <- nfile[interrownames,replace]
			for(i in 1:length(replace)){
				outmx <- cbind(ofile,newdata[,i])
				colnames(outmx)[ncol(outmx)] <- colnames(nfile)[replace[i]]
			}
			rownames(outmx) <- interrownames
		}else{
			ofile <- ofile[interrownames,]
			replace <- (which(colnames(nfile) == intercolnames[length(intercolnames)]) + 1):ncol(nfile)
			newdata <- nfile[interrownames,replace]
			if(length(replace) > 1){
				for(i in 1:length(replace)){
					outmx <- cbind(ofile,newdata[,i])
					colnames(outmx)[ncol(outmx)] <- colnames(nfile)[replace[i]]
				}
			}else{
				outmx <- cbind(ofile,newdata)
				colnames(outmx)[ncol(outmx)] <- colnames(nfile)[replace]

			}
			rownames(outmx) <- interrownames
	}
	

	FZsavedata(dataobj=outmx, dirname=dirname,fname=oldfile)
	gc()
	NULL
	}
	#return(outmx)
}


FZhiDataUpdate <- function(dirname = "Datafiles",code=NULL){
	NewFileList <- list.files(path = paste(getwd(),dirname,sep="/"),pattern=paste("D_NEW",code,sep=""))
	OldFileList <- gsub(paste("NEW",code,sep=""),"",NewFileList)

	NewFileList <- gsub(".csv","",NewFileList)
	OldFileList <- gsub(".csv","",OldFileList)

	for(i in 1:length(NewFileList)){
		FZappenddata(oldfile=OldFileList[i], newfile=NewFileList[i], dirname="Datafiles")
	}
"Complete"
}



FZfreaddata <- function(dataobj=NULL, dirname="Datafiles",numeric=FALSE){
	if(is.null(dataobj)|is.null(dirname)){stop("FZfreaddata: Some para's are null")}
	require(data.table)
	fname <- paste(getwd(),"\\",dirname,"\\",dataobj,".csv",sep="")
	outmx <- fread(fname, sep=",",header=TRUE,na.strings = "#N/A",stringsAsFactors=FALSE)
	outrnames <- as.vector(unlist(list(outmx[,1])))

	outmx <- as.matrix(outmx[,-1])
	
	if(substr(colnames(outmx)[1],1,1) != "X"){
		colnames(outmx) <- paste("X",colnames(outmx),sep="")
	}
	if(numeric){
		outmx <- apply(outmx,2,as.numeric)
	}
	rownames(outmx) <- outrnames
	outmx
}

FZreaddata <- function(dataobj=NULL, dirname="Datafiles"){
	if(is.null(dataobj)|is.null(dirname)){stop("FZreaddata: Some para's are null")}
	tt <- as.matrix(read.csv(paste(getwd(),"\\",(dirname=dirname),"\\",dataobj,".csv",sep=""),sep=",",header=TRUE,row.names=1,na.strings = c("#N/A"),stringsAsFactors = FALSE ))
	tt
}

FZsavedata <- function(dataobj=NULL, dirname=NULL,fname=NULL){
	if(is.null(dataobj)|is.null(dirname)|is.null(fname)){stop("FZsavedata: some para's are null")}
	dataobj <- as.matrix(dataobj)
	#write.table(dataobj,paste(getwd(),"\\",dirname,"\\",fname,".csv",sep=""),sep=",")
	write.csv(dataobj,paste(getwd(),"\\",dirname,"\\",fname,".csv",sep=""))
}

FZlistdata <- function(dirname=NULL){
	if(is.null(dirname)){stop("FZlistdata:Dirname is NULL")}
	list.files(path = paste(getwd(),dirname,sep="/"),pattern = ".csv$", recursive = TRUE)
}

FZzerona <- function(input=NULL){
	if(is.null(input)){stop("FZzerona: Input is NULL")}
	input[is.na(input)|is.nan(input)|!is.finite(input)] <-0
	input
}

FZnazero <- function(input=NULL){
	if(is.null(input)){stop("FZzerona: Input is NULL")}
	input[input==0] <- NA
	input
}

FZshiftmx <- function(input=NULL, shiftcol=NULL){
	if(is.null(input)|is.null(shiftcol)){stop("FZshiftmx: Some Para's are NULL")}
	if(!is.matrix(input)){stop("FZshiftmx: Input is not a matrix")}
	if(shiftcol>0){
		shiftmx <- input[,(shiftcol+1):ncol(input)]
		namx <- matrix(NA, nrow=nrow(input),ncol=shiftcol)
		shiftmx <- cbind(shiftmx,namx)
	} else {
		shiftmx <- input[,1:(ncol(input)+shiftcol)]
		namx <- matrix(NA, nrow=nrow(input),ncol=abs(shiftcol))
		shiftmx <- cbind(namx,shiftmx)		
	}
	colnames(shiftmx) <- colnames(input)
	rownames(shiftmx)<- rownames(input)
	shiftmx
}

FZcummx <- function(input=NULL){
	if(is.null(input)){stop("FZcummx: Input is NULL")}
	input <- FZzerona(input=input)
	cummx <- t(apply(input,1,cumsum))
	cummx
}

FZrunningavgmx <- function(input=NULL){
	if(is.null(input)){stop("FZrunningavgmx: Input is NULL")}
	input <- FZzerona(input=input)
	runningavg <- matrix(NA, nrow=nrow(input),ncol=ncol(input), dimnames=list(dimnames(input)))
	runningavg[,1] <- input[,1]
	for (colpos in 2:ncol(input)){
		runningavg[,colpos] <- rowMeans(input[,1:colpos],na.rm=TRUE)
	}
	runningavg
}

FZfillwithlast <- function(input=NULL){
	if(is.null(input)){stop("FZfillwithlast: input is NULL")}
	if(!is.matrix(input)){stop("FZfillwithlast: input is not a matrix")}
	input <- FZzerona(input=input)
	adjmx <- matrix(NA, nrow=nrow(input), ncol=ncol(input))#,dimnames=list(dimnames(input)))
	adjmx[,1] <- input[,1]
	for (colpos in 2:ncol(adjmx)){
		inputcol <- input[,colpos]
		adjcol <- adjmx[,(colpos-1)]
		newcol <- ifelse(inputcol==0 & adjcol!=0,adjcol,inputcol)
		adjmx[,colpos] <- newcol 
	}
	dimnames(adjmx) <- dimnames(input)
	adjmx
}

FZcountmx <- function(input=NULL, nazero = TRUE){
# will return a matrix of the same dimensions as the input, with a 1 for each value that is present
# if nazeroisset to true, then will return 0 if value is missing
	if(is.null(input)){stop("FZcountmx: input is NULL")}
	countmx <- input
	countmx[!is.na(countmx) & countmx!=0] <- 1
	if(!is.null(nazero)& nazero==TRUE){countmx[countmx !=1| is.na(countmx)] <- 0}
	if(!is.null(nazero)& nazero==FALSE){countmx[countmx !=1| is.na(countmx)] <- NA}
	countmx
}

FZmatchmx <- function(stockrets=NULL, targetmx=NULL){
	if(is.null(stockrets)|is.null(targetmx)){stop("FZmatchmx: Some Para's are NULL")}
	coltouse <- colnames(stockrets)
	outputmx <- targetmx[,coltouse]
	outputmx
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

FZmovingavg <- function(input=NULL, maper = NULL){
	if(is.null(input) |is.null(maper)){stop("FZmovingavg: Some Para's are NULL")}
	macummx <- FZcummx(input=input)
	mashiftcummx <- FZshiftmx(input=macummx, shiftcol=-maper)
	mamx <- (macummx-mashiftcummx)/maper
	mamx
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

FZremovena <- function(input=NULL){
	if(is.null(input)){stop("FZremovena: Input is NULL")}
	input[complete.cases(input), ]
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
	outputmx
}

FZEvastanvarbypara <- function(inputvar=NULL, paramx=NULL, Adjfac=0.5){
	if(is.null(inputvar)| is.null(paramx)){stop("FZEvastanvarbypara: Some Para's are NULL")}
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

FZstandardisevarbycol <- function(input=NULL){
	if(is.null(input)){stop("FZstandardisevarbycol: Some Para's are NULL")}
	inputout <- apply(input,2,function(x){
		outval <- (x-mean(x,na.rm=TRUE))/sd(x,na.rm=TRUE)
		outval
	})
	inputout
}

FZpriceVolatility <- function(input=NULL){
	if(is.null(input)){stop("FZpriceVolatility: Input is NULL")}
	cnames <- colnames(input)
	cnamestemp <- sapply(cnames,function(x){gsub("X","",x)})
	cnamestemp <- substring(cnamestemp,1,4)
	workinglist <- unique(cnamestemp)
	outmx <- matrix(NA, nrow=nrow(input),ncol=length(workinglist))
	colnames(outmx) <- workinglist
	rownames(outmx) <- rownames(input)
	for (yearval in workinglist){
		tempvec <- ifelse(cnamestemp==yearval,1,NA)
		tempmx <- t(t(input)*tempvec)
		histvol <- apply(tempmx,1,sd,na.rm=TRUE)*sqrt(52)
		outmx[,yearval] <- histvol
	}
	outmx
}