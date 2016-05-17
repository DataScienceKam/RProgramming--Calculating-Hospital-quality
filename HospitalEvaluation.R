# setwd("D:/Users/k3560/Desktop/air/rprog-data-ProgAssignment3-data")
# getwd()
 
# data.outcome <- read.csv("outcome-of-care-measures.csv", header=TRUE, sep=',', colClasses = "character")
# data.hospital <- read.csv("hospital-data.csv", header=TRUE, sep=',' check.names=FALSE)
# data <- merge(data.outcome, data.hospital)
# ncol(data)
# nrow(data)
 
#looking at the data
# ------------------------------------------------
# head(data) : first few rows and column values
# tail(data) : last few rows and column values
# class(data) : types of objects
# dim(data) : how many rows and columns
# summary(data) : min, max, mean, median, 1qt, 2qt, 3qt for numbers, total rows for characters
# str(data)
# nrow(data)
# ncol(data)
# colnames(data) : column names
# names(data) : colnames
# table(data$Hospital.Name) displays row values
# sapply(data, typeof)
# sapply(data, class)
# data$Provider.Number
 
# Subset data with following columns: 7, 11, 35, and 44 columns
#------------------------------------------------------------------
# all rows
# sdata <- data.outcome[ ,c(2, 7, 11, 35, 43)]
# names(sdata) <- c("Hospital.Name", "State", "Heart.Attack", "Heart.Failure", "Penumonia")
# head(sdata)
 
 
# sState <- subset(sdata, grepl(state, sdata$State))
# sCol <- sState[!(sState$Heart.Attack == "Not Available"), 1:3]
# convert from character to numeric
# sCol$Heart.Attack <- as.numeric(sCol$Heart.Attack)
# str(sCol)
# dim(sState) how many rows and columns does the dataframe have
# sOrdered <- sCol[order(sCol$Heart.Attack), ]
# sTop10 <- head(sOrdered, 10)
# sHos <- sTop10[order(sTop10$Hospital.Name), ]
# head(sHos, 10)

#-------------------------------------------------------------------------------------------
# Plotting 30 day mortality rate
#-------------------------------------------------------------------------------------------
Mortalitymean <- function(df, outcome){
	s <- subset(df, select = outcome)	
	sna <- na.omit(s)
	m <- mean(as.matrix(sna))
	#lapply(sna, mean, na.rm = TRUE)
	#sapply(sna, mean, na.rm = TRUE)
	round(m, digits = 3)	
}

IndividualStateCal <- function(df, type, fun){
	sHA <- na.omit(df[, c(1, 4)])
	sHF <- na.omit(df[, c(2, 4)])
	sP <- na.omit(df[, c(3, 4)])
	# aggregate mean based on Row value : State
	if(type == "heart attack"){
		if(fun == "mean"){
			mHA <- aggregate(sHA[, 1], by=list(sHA$State), FUN=mean)
		}
		else if (fun == "std"){
			mHA <- aggregate(sHA[, 1], by=list(sHA$State), FUN=std)
		}
		else if(fun == "stderr"){
			mHA <- aggregate(sHA[, 1], by=list(sHA$State), FUN=stderr)
		}
		mHA <- cbind(mHA, type = "Heart Attack Mortality Rate")
		return(mHA)
	}
	else if(type == "heart failure")
	{
		if(fun == "mean"){
			mHF <- aggregate(sHF[, 1], by=list(sHF$State), FUN=mean)
		}
		else if (fun == "std"){
			mHF <- aggregate(sHF[, 1], by=list(sHF$State), FUN=std)
		}
		else if(fun == "stderr"){
			mHF <- aggregate(sHF[, 1], by=list(sHF$State), FUN=stderr)
		}
		mHF <- cbind(mHF, type = "Heart Failure Mortality Rate")
		return(mHF)
	}
	else if(type == "pnemonia")
	{
		if(fun == "mean"){
			mP <- aggregate(sP[, 1], by=list(sP$State), FUN=mean)
		}
		else if (fun == "std"){
			mP <- aggregate(sP[, 1], by=list(sP$State), FUN=std)
		}
		else if(fun == "stderr"){
			mP <- aggregate(sP[, 1], by=list(sP$State), FUN=stderr)
		}
		mP <- cbind(mP, type = "Pnemonia Mortality Rate")
		return(mP)
	}
}


plotHeartAttackMortalityRate{
		setwd("C:/Users/Kam/Desktop/DataScientist/rprog-data-ProgAssignment3-data")
		data.outcome <- read.csv("outcome-of-care-measures.csv", header=TRUE, sep=',', colClasses = "character")
               
        # Subset data with following columns: 11, 17, and 23 columns
        data <- data.outcome[ ,c(11, 17, 23)]
		names(data) <- c("HeartAttack", "HeartFailure", "Penumonia")
		data$HeartAttack <- as.numeric(data$HeartAttack)
		data$HeartFailure <- as.numeric(data$HeartFailure)
		data$Penumonia <- as.numeric(data$Penumonia)
		hist(data[, 1])
		hist(data[, 2])
		hist(data[, 3])
		
		# mean, standard deviation, standard error of mean, confidence interval by state
		statdata <- cbind(data, State = data.outcome[, 7])
		
		# Mean for individual states
		HAttack <- IndividualStateCal(statdata, "heart attack", "mean")
		names(HAttack) <- c("State", "Mean", "For")
		x <- HAttack$State
		y <- HAttack$Mean
		# plot
		plot(x = x, y = y, ylab = "Mortality Rate Mean", main = "Heart Attack Mean", sub = "By State", col = ifelse(y > 15, 'red', 'green'))
		dotchart(x = y, xlab = "Mortality Rate Mean", main = "Heart Attack Mortality", sub = "Mean by State", col = ifelse(y > 15, 'red', 'black'))
		barplot(y, ylab = "Mortality Rate Mean", main = " Heart Attack Mortality", sub = "Mean by State", col = ifelse(y > 15, 'red', 'green'), names.arg = HAttack$State)
		barplot(y, ylab = "Mortality Rate Mean", main = "Mortality Rate Mean", sub = "By State", col = ifelse(y > 15, 'red', 'green'), axes = TRUE, horiz = TRUE)
		hist(y, xlab = "Mortality Rate Mean", main = "Heart Attack Mortality", sub = "Mean by State", breaks= 2)
		scatterplot(x = x, y = y, ylab = "Mortality Rate Mean", main = "Mortality Rate Mean", sub = "By State", col = ifelse(y > 15, 'red', 'green'))
		
		HFailure <- IndividualStateCal(statdata, "heart failure", "mean")
		names(HFailure) <- c("State", "Mean", "For")
		Penumonia <- IndividualStateCal(statdata, "pnemonia", "mean")
		names(Penumonia) <- c("State", "Mean", "For")
		
		# Standard Deviation by state
		HAttack <- IndividualStateCal(statdata, "heart attack", "std")
		names(HAttack) <- c("State", "Mean", "For")
		HFailure <- IndividualStateCal(statdata, "heart failure", "std")
		names(HFailure) <- c("State", "Mean", "For")
		Penumonia <- IndividualStateCal(statdata, "pnemonia", "std")
		names(Penumonia) <- c("State", "Mean", "For")
		
		# Standard Error by state
		HAttack <- IndividualStateCal(statdata, "heart attack", "stderr")
		names(HAttack) <- c("State", "Mean", "For")
		HFailure <- IndividualStateCal(statdata, "heart failure", "stderr")
		names(HFailure) <- c("State", "Mean", "For")
		Penumonia <- IndividualStateCal(statdata, "pnemonia", "stderr")
		names(Penumonia) <- c("State", "Mean", "For")
		
		# Confidence interval by state
		help("predict.lm")
		
		# Plot graphs for the mean
		mplot = do.call("rbind", list(mHA, mHF, mP))
		y <- mplot$Mean
		boxplot(y, ylab = "Mortality Rate for all data", main = "Mortality Rate Mean", sub = "By State", col = ifelse(y > 15, 'red', 'green'))
		# Frequency of distribution
		
		
}
#-------------------------------------------------------------------------------------------
# Best hospital in state:
# heart attack, Pnemonia, Heart Failure death: hospitals with the least mortality rate 
#-------------------------------------------------------------------------------------------
bestordered <- function(outcome, sState){
                                if(outcome == "heart attack"){
                                                sCol <- sState[!(sState$Heart.Attack == "Not Available"), 1:3]    
                                                sCol$Heart.Attack <- as.numeric(sCol$Heart.Attack)
                                                # check to see if the value has been converted to as.numeric
                                                # str(sCol)
                                                sOrdered <- sCol[order(sCol$Heart.Attack), ]    
                                }
                                else if(outcome == "heart failure"){
                                                sCol <- sState[!(sState$Heart.Failure == "Not Available"), c(1, 2, 4)]
                                                sCol$Heart.Failure <- as.numeric(sCol$Heart.Failure)
                                                # check to see if the value has been converted to as.numeric
                                                # str(sCol)
                                                sOrdered <- sCol[order(sCol$Heart.Failure), ]
                                }
                                else{
                                                sCol <- sState[!(sState$Penumonia == "Not Available"), c(1, 2, 5)]
                                                sCol$Penumonia <- as.numeric(sCol$Penumonia)
                                                # check to see if the value has been converted to as.numeric
                                                # str(sCol)
                                                sOrdered <- sCol[order(sCol$Penumonia), ]
                                }
                                return(sOrdered)
}
 
match <- function(outcome, sOrdered, sFVal){
                if(outcome == "heart attack"){
                                sMatch <- sOrdered[(sOrdered$Heart.Attack == sFVal), 1:3]
                }
                else if(outcome == "heart failure"){
                                sMatch <- sOrdered[(sOrdered$Heart.Failure == sFVal), 1:3]
                }
                else{
                                sMatch <- sOrdered[(sOrdered$Penumonia== sFVal), 1:3]
                }
                return(sMatch)
}
 
best <- function(state, outcome){
                setwd("C:/Users/Kam/Desktop/DataScientist/rprog-data-ProgAssignment3-data")
                getwd()
               
                # read the data into a dataframe object
                data.outcome <- read.csv("outcome-of-care-measures.csv", header=TRUE, sep=',', colClasses = "character")
               
                # Subset data with following columns: 2, 7, 11, 17, and 23 columns
                data <- data.outcome[ ,c(2, 7, 11, 17, 23)]
                sState <- subset(data, grepl(state, data$State))
                # stop function to throw an error
                if(nrow(sState) == 0){
                                stop(paste("Error in best(", state, ",", outcome, ") : invalid state"))
                }
                else if (!(outcome == "heart attack" || outcome == "heart failure" || outcome == "pneumonia")){
                                stop(paste("Error in best(", state, ",", outcome, ") : invalid outcome"))
                }
                # change the column names of the chosen dataset
                names(sState) <- c("Hospital.Name", "State", "Heart.Attack", "Heart.Failure", "Penumonia")
                # check if the names have been changed
                #head(sState)
               
                # get ordered values: Function call
                sOrdered <- bestordered(outcome, sState)
                #Select first row value
                print(head(sOrdered, 10))
                sFVal <- sOrdered[1,3]
                print (sFVal)
               
                #select all rows that match first row value: Function call
                sMatch <- match(outcome, sOrdered, sFVal)
               
                # select top 10
                sHos <- sMatch[order(sMatch$Hospital.Name), ]
                #print(sHos)
                return(head(sHos, 1))
}

library("dplyr")
#-------------------------------------------------------------------------------------------
# Ranking hospitals by mortality rate for a given state and a given mortality rate rank
#-------------------------------------------------------------------------------------------
rankRate <- function(outcome, sState){
                                if(outcome == "heart attack"){
                                                sCol <- sState[!(sState$Heart.Attack == "Not Available"), 1:3]   
                                                sCol$Heart.Attack <- as.numeric(sCol$Heart.Attack)
                                                # check to see if the value has been converted to as.numeric
                                                # str(sCol)
                                                sOrdered <- sCol[order(sCol$Heart.Attack), ]    
                                }
                                else if(outcome == "heart failure"){
                                                sCol <- sState[!(sState$Heart.Failure == "Not Available"), c(1, 2, 4)]
                                                sCol$Heart.Failure <- as.numeric(sCol$Heart.Failure)
                                                # check to see if the value has been converted to as.numeric
                                                # str(sCol)
                                                sOrdered <- sCol[order(sCol$Heart.Failure), ]
                                }
                                else{
                                                sCol <- sState[!(sState$Penumonia == "Not Available"), c(1, 2, 5)]
                                                sCol$Penumonia <- as.numeric(sCol$Penumonia)
                                                # check to see if the value has been converted to as.numeric
                                                # str(sCol)
                                                sOrdered <- sCol[order(sCol$Penumonia), ]
                                }
				# Add a dense rank col based on the rates so that we can rank the hospital and then order the rank on Hospital to the get the correct result
                                
				# Dense rank on the dataset so that we can provide a rank number for each of the values based on the data subset
				# required library dplyr
				if(outcome == "heart attack"){
					deathRank <- sOrdered %>% mutate(rank = dense_rank(Heart.Attack))
				}
				else if (outcome == "heart failure"){
					deathRank <- sOrdered %>% mutate(rank = dense_rank(Heart.Failure))
				}
				else{
					deathRank <- sOrdered %>% mutate(rank = dense_rank(Penumonia))
				}
				rankHosOrd <- arrange(deathRank, rank, Hospital.Name)
				seqsrankHosOrd  <- cbind(rankHosOrd, RankNum = seq.int(nrow(rankHosOrd)))
				return(seqsrankHosOrd)
}
 
rankhospital <- function(state, outcome, num = "best"){
                setwd("C:/Users/Kam/Desktop/DataScientist/rprog-data-ProgAssignment3-data")
                getwd()
               
                # read the data into a dataframe object
                data.outcome <- read.csv("outcome-of-care-measures.csv", header=TRUE, sep=',', colClasses = "character")
               
                # Subset data with following columns: 2, 7, 11, 17, and 23 columns
                data <- data.outcome[ ,c(2, 7, 11, 17, 23)]
                sState <- subset(data, grepl(state, data$State))
                # stop function to throw an error
                if(nrow(sState) == 0){
                                stop(paste("Error in best(", state, ",", outcome, ") : invalid state"))
                }
                else if (!(outcome == "heart attack" || outcome == "heart failure" || outcome == "pneumonia")){
                                stop(paste("Error in best(", state, ",", outcome, ") : invalid outcome"))
                }
                # change the column names of the chosen dataset
                names(sState) <- c("Hospital.Name", "State", "Heart.Attack", "Heart.Failure", "Penumonia")
                # check if the names have been changed
		print(colnames(sState))
                #head(sState)
               
                # get ordered values: Function call
                sOrdered <- rankRate(outcome, sState)
				# Order first by rank and then by hospital
				if(num == "best"){
					return(head(sOrdered, 1))
				}
				else if(num == "worst"){
					return(tail(sOrdered, 1))
				}
				else{
					# return the rank
					datafiltered <- filter(sOrdered, RankNum == num)
					return(head(datafiltered, 1))
				}
}

#-------------------------------------------------------------------------------------------
# Ranking all hospitals by mortality rate for a given mortality rate rank
#-------------------------------------------------------------------------------------------
rankall <- function(outcome, num = "best"){
                setwd("C:/Users/Kam/Desktop/DataScientist/rprog-data-ProgAssignment3-data")
                getwd()
               
                # read the data into a dataframe object
                data.outcome <- read.csv("outcome-of-care-measures.csv", header=TRUE, sep=',', colClasses = "character")
               
                # Subset data with following columns: 2, 7, 11, 17, and 23 columns
                data <- data.outcome[ ,c(2, 7, 11, 17, 23)]
                if (!(outcome == "heart attack" || outcome == "heart failure" || outcome == "pneumonia")){
                                stop(paste("Error in best(", state, ",", outcome, ") : invalid outcome"))
                }
                # change the column names of the chosen dataset
                names(data) <- c("Hospital.Name", "State", "Heart.Attack", "Heart.Failure", "Penumonia")
                # check if the names have been changed
                #head(data)
               
                # get ordered values: Function call
                sOrdered <- rankRate(outcome, data)
		# Order first by rank and then by hospital
		if(num == "best"){
			datafiltered <- filter(sOrdered, rank == 1)
			return(datafiltered)
		}
		else if(num == "worst"){
			# find the worst rank
			final <- sapply(sOrdered, max)
			datafiltered <- filter(sOrdered, rank == final)
			return(datafiltered)
		}
		else{
			# return the rank
			datafiltered <- filter(sOrdered, rank == num + 1)
			return(datafiltered)
		}
}


 

 
# Partial search for rows
# sub <- subset(data.outcome, grepl("^METHO", data.outcome$Hospital.Name), ignore.case=TRUE)
 
 
# clear the workspace
> rm(list=ls())


state <- "NY"
outcome <- "heart failure"
data <- data.outcome[ ,c(2, 7, 11, 17, 23)]
sState <- subset(data, grepl(state, data$State))
names(sState) <- c("Hospital.Name", "State", "Heart.Attack", "Heart.Failure", "Penumonia")
sCol <- sState[!(sState$Heart.Attack == "Not Available"), 1:3]   
sCol$Heart.Attack <- as.numeric(sCol$Heart.Attack)
sOrdered <- sCol[order(sCol$Heart.Attack), ]  
sOrdered





rankhospital <- function(state, outcome, num = "best") {
        ## Read outcome data
	setwd("C:/Users/Kam/Desktop/DataScientist/rprog-data-ProgAssignment3-data")
       
        data <- read.csv("outcome-of-care-measures.csv")

        ## Check that state and outcome are valid
        states <- levels(data[, 7])[data[, 7]]
        state_flag <- FALSE
        for (i in 1:length(states)) {
                if (state == states[i]) {
                        state_flag <- TRUE
                }
        }
        if (!state_flag) {
                stop ("invalid state")
        } 
        if (!((outcome == "heart attack") | (outcome == "heart failure")
              | (outcome == "pneumonia"))) {
                stop ("invalid outcome")
        }

        ## Return hospital name in that state with the given rank 30-day death 
        ## rate
        col <- if (outcome == "heart attack") {
                11
        } else if (outcome == "heart failure") {
                17
        } else {
                23
        }
        
        data[, col] <- suppressWarnings(as.numeric(levels(data[, col])[data[, col]]))
        data[, 2] <- as.character(data[, 2])
        statedata <- data[grep(state, data$State), ]
        orderdata <- statedata[order(statedata[, col], statedata[, 2], na.last = NA), ]
        if(num == "best") {
                orderdata[1, 2]
        } else if(num == "worst") {
                orderdata[nrow(orderdata), 2]
        } else{
                orderdata[num, 2]
        }
}



best <- function(state, outcome) {
        ## Read outcome data
	setwd("C:/Users/Kam/Desktop/DataScientist/rprog-data-ProgAssignment3-data")
        data <- read.csv("outcome-of-care-measures.csv")
        
        ## Check that state and outcome are valid
        states <- levels(data[, 7])[data[, 7]]
        state_flag <- FALSE
        for (i in 1:length(states)) {
                if (state == states[i]) {
                        state_flag <- TRUE
                }
        }
        if (!state_flag) {
                stop ("invalid state")
        } 
        if (!((outcome == "heart attack") | (outcome == "heart failure")
            | (outcome == "pneumonia"))) {
                stop ("invalid outcome")
        }
        
        ## Return hospital name in that state with lowest 30-day death rate
        col <- if (outcome == "heart attack") {
                11
        } else if (outcome == "heart failure") {
                17
        } else {
                23
        }
        
        data[, col] <- suppressWarnings(as.numeric(levels(data[, col])[data[, col]]))
        data[, 2] <- as.character(data[, 2])
        statedata <- data[grep(state, data$State), ]
        orderdata <- statedata[order(statedata[, col], statedata[, 2], na.last = NA), ]
        orderdata[1, 2]
}




##Part 2: best.R:

best <- function(state, outcome) {
        ## Read outcome data
        data <- read.csv("outcome-of-care-measures.csv")
        
        ## Check that state and outcome are valid
        states <- levels(data[, 7])[data[, 7]]
        state_flag <- FALSE
        for (i in 1:length(states)) {
                if (state == states[i]) {
                        state_flag <- TRUE
                }
        }
        if (!state_flag) {
                stop ("invalid state")
        } 
        if (!((outcome == "heart attack") | (outcome == "heart failure")
            | (outcome == "pneumonia"))) {
                stop ("invalid outcome")
        }
        
        ## Return hospital name in that state with lowest 30-day death rate
        col <- if (outcome == "heart attack") {
                11
        } else if (outcome == "heart failure") {
                17
        } else {
                23
        }
        
        data[, col] <- suppressWarnings(as.numeric(levels(data[, col])[data[, col]]))
        data[, 2] <- as.character(data[, 2])
        statedata <- data[grep(state, data$State), ]
        orderdata <- statedata[order(statedata[, col], statedata[, 2], na.last = NA), ]
        orderdata[1, 2]
}

##Part 3: rankhospital.R:

rankhospital <- function(state, outcome, num = "best") {
        ## Read outcome data
        data <- read.csv("outcome-of-care-measures.csv")

        ## Check that state and outcome are valid
        states <- levels(data[, 7])[data[, 7]]
        state_flag <- FALSE
        for (i in 1:length(states)) {
                if (state == states[i]) {
                        state_flag <- TRUE
                }
        }
        if (!state_flag) {
                stop ("invalid state")
        } 
        if (!((outcome == "heart attack") | (outcome == "heart failure")
              | (outcome == "pneumonia"))) {
                stop ("invalid outcome")
        }

        ## Return hospital name in that state with the given rank 30-day death 
        ## rate
        col <- if (outcome == "heart attack") {
                11
        } else if (outcome == "heart failure") {
                17
        } else {
                23
        }
        
        data[, col] <- suppressWarnings(as.numeric(levels(data[, col])[data[, col]]))
        data[, 2] <- as.character(data[, 2])
        statedata <- data[grep(state, data$State), ]
        orderdata <- statedata[order(statedata[, col], statedata[, 2], na.last = NA), ]
        if(num == "best") {
                orderdata[1, 2]
        } else if(num == "worst") {
                orderdata[nrow(orderdata), 2]
        } else{
                orderdata[num, 2]
        }
}

##Part 2: best.R:

best <- function(state, outcome) {
        ## Read outcome data
        data <- read.csv("outcome-of-care-measures.csv")
        
        ## Check that state and outcome are valid
        states <- levels(data[, 7])[data[, 7]]
        state_flag <- FALSE
        for (i in 1:length(states)) {
                if (state == states[i]) {
                        state_flag <- TRUE
                }
        }
        if (!state_flag) {
                stop ("invalid state")
        } 
        if (!((outcome == "heart attack") | (outcome == "heart failure")
            | (outcome == "pneumonia"))) {
                stop ("invalid outcome")
        }
        
        ## Return hospital name in that state with lowest 30-day death rate
        col <- if (outcome == "heart attack") {
                11
        } else if (outcome == "heart failure") {
                17
        } else {
                23
        }
        
        data[, col] <- suppressWarnings(as.numeric(levels(data[, col])[data[, col]]))
        data[, 2] <- as.character(data[, 2])
        statedata <- data[grep(state, data$State), ]
        orderdata <- statedata[order(statedata[, col], statedata[, 2], na.last = NA), ]
        orderdata[1, 2]
}

##Part 3: rankhospital.R:

rankhospital <- function(state, outcome, num = "best") {
        ## Read outcome data
        data <- read.csv("outcome-of-care-measures.csv")

        ## Check that state and outcome are valid
        states <- levels(data[, 7])[data[, 7]]
        state_flag <- FALSE
        for (i in 1:length(states)) {
                if (state == states[i]) {
                        state_flag <- TRUE
                }
        }
        if (!state_flag) {
                stop ("invalid state")
        } 
        if (!((outcome == "heart attack") | (outcome == "heart failure")
              | (outcome == "pneumonia"))) {
                stop ("invalid outcome")
        }

        ## Return hospital name in that state with the given rank 30-day death 
        ## rate
        col <- if (outcome == "heart attack") {
                11
        } else if (outcome == "heart failure") {
                17
        } else {
                23
        }
        
        data[, col] <- suppressWarnings(as.numeric(levels(data[, col])[data[, col]]))
        data[, 2] <- as.character(data[, 2])
        statedata <- data[grep(state, data$State), ]
        orderdata <- statedata[order(statedata[, col], statedata[, 2], na.last = NA), ]
        if(num == "best") {
                orderdata[1, 2]
        } else if(num == "worst") {
                orderdata[nrow(orderdata), 2]
        } else{
                orderdata[num, 2]
        }
}

##Part 4: rankall.R:

rankall <- function(outcome, num = "best") {
        ## Read outcome data
	setwd("C:/Users/Kam/Desktop/DataScientist/rprog-data-ProgAssignment3-data")
        data <- read.csv("outcome-of-care-measures.csv")
        
        ## Check that outcome is valid
        if (!((outcome == "heart attack") | (outcome == "heart failure")
              | (outcome == "pneumonia"))) {
                stop ("invalid outcome")
        }

        ## For each state, find the hospital of the given rank
        col <- if (outcome == "heart attack") {
                11
        } else if (outcome == "heart failure") {
                17
        } else {
                23
        }
        
        data[, col] <- suppressWarnings(as.numeric(levels(data[, col])[data[, col]]))
        data[, 2] <- as.character(data[, 2])
        
        # Generate an empty vector that will be filled later, row by row, to 
        # generate the final output.
        output <- vector()
        
        states <- levels(data[, 7])
        
        for(i in 1:length(states)) {
                statedata <- data[grep(states[i], data$State), ]
                orderdata <- statedata[order(statedata[, col], statedata[, 2], 
                                             na.last = NA), ]
                hospital <- if(num == "best") {
                        orderdata[1, 2]
                } else if(num == "worst") {
                        orderdata[nrow(orderdata), 2]
                } else{
                        orderdata[num, 2]
                }
                output <- append(output, c(hospital, states[i]))
        }

        ## Return a data frame with the hospital names and the (abbreviated) 
        ## state name
        output <- as.data.frame(matrix(output, length(states), 2, byrow = TRUE))
        colnames(output) <- c("hospital", "state")
        rownames(output) <- states
        
        output
}


