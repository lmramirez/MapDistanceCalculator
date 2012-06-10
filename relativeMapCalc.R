mydist <- function(mapfile){
	#add pixmap package location to libPaths and import pixmap
	.libPaths("/home/matloff/Pub/R32/") #need this on CSIF 32-bit machines, else comment out
	library(pixmap)

	#read in and plot map file
	userMap <- read.pnm(mapfile)
	plot(userMap)

	#global Variable to keep track of previous maps
	prevMaps <<- list(recordPlot())
	distances<<- vector()
	myscale <<- 1	

	#print Menu
	cat("\nMenu: \n", "as - add segment\n", "dls - delete last segment\n", "gs - get scale\n", "q - quit\n")

	#begin prompting for user input
	usrInput()
}


usrInput <- function(){
	#usrInput prompts the user for input and then
	#calls helper functions and recursively calls itself
	#until the user enters "q"

	#prompt user for command and scan one line of input
	cat("\nEnter Command: ")
	usrCommand <- scan(what="raw",nlines=1,quiet=TRUE)

	#carry out user's input command
	if (usrCommand=="as") {	#add segment to current map
		addSegment()
		usrInput()
	} else if (usrCommand=="dls") { #restore previous map
		delSegment()
		usrInput()
	} else if (usrCommand=="gs") { #use given scale to calculate distances
		getScale()
		usrInput()
	} else if (usrCommand=="q") { #quit program
		return("bye")
	} else {
	    	cat("Error: Bad Input")
		usrInput()
	}
}

addSegment <- function(){
	#uses locator() and lines() to print out a new segment chosen
	#by user to the current plot and adds the new plot to a list
	#of plots so that it may be restored later if needed
	
	#prompt user to click coordinates
	cat("Please click on two spots in the map to draw a new segment","\n")

	#get position from user, print red line, and record plot in list of previous plots
	pos<-locator(2)
	lines(pos$x,pos$y,col="red")
	prevMaps[[length(prevMaps)+1]] <<- recordPlot()
	
	#update and print total distance
	distances <<- c(distances, (getDistance(pos)/myscale))
	cat("Total Distance: ", sum(distances), "\n")
}

delSegment <- function(){
	#deletes the last segment drawn by user by 
	#restoring the previous map that was displayed	

	#if there is only one map in prevMaps, do nothing
	if(length(prevMaps)==1){
		return("Error: No previous maps to delete.")
	}

	#delete last element of prevMaps list then restore previous map
	prevMaps <<- prevMaps[-length(prevMaps)]
	replayPlot(prevMaps[[length(prevMaps)]])

	#update distance vector and display distances
	distances <<- distances[-length(distances)]
	cat("Total Distance: ", sum(distances), "\n")
}

getScale <- function(){
	#creates a new scale for determining distances on a map by taking
	#a ratio of "map" distance and desired distance and dividing all 
	#distances in a vector of recorded distances by this ratio	
	#also prints a temporary map showing just the line for the new scale

	#reset to original scale
	distances <<- distances*myscale

	#prompt user for locations
	cat("Please click on two points in the map to set scale.\n")
	pos <- locator(2)
	
	#show original map with scale on it
	replayPlot(prevMaps[[1]])
	lines(pos$x,pos$y, col="black")

	#prompt user for scale distance and update scale variable
	cat("Please enter distance for entered segment.\n")	
        usrCommand <- scan(what="numeric",nlines=1,quiet=TRUE)	

	#calculate scale
	myscale <<- getDistance(pos)/as.numeric(usrCommand)	

	#update distance vector
	distances <<- distances/myscale

	#restore regular map
	replayPlot(prevMaps[[length(prevMaps)]])

	#display new total distance
	cat("Updated Total Distance: ", sum(distances), "\n")
}



getDistance <- function(xy){
	#takes the output of locator() as input so the input data is 
	#a list representing two points
	#with xy with $x vector of x values and $y vector of y values
	#and uses distance formula to calculate distance

	return(sqrt(((xy$y[2]-xy$y[1])^2 + (xy$x[2]-xy$x[1])^2)))
}

