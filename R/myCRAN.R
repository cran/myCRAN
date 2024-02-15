#' myCRAN
#'
#' @import cranlogs
#' @import grDevices
#' @import graphics
#' @import pkgsearch
#'
#'
#' @description wrapper for cran_downloads() to plot counts and cumulative counts
#'	of downloads of CRAN packages
#'
#' @param packages character vector of names of packages
#' @param author character string of name of target author
#' @param from date parameter for cran_downloads()
#' @param when date parameter for cran_downloads()
#' @param plotNew Boolean if TRUE start a new plot window
#' @param plotWidth integer width parameter for dev.new()
#'
#' @details Use either packages or author as the input parameter.
#' When author is given, the function advanced_search() queries CRAN
#' to return a list of the names of all packages by that author.
#'
#' @examples
#' \dontrun{
#' 	packages<-c("timeLineGraphics","textBoxPlacement","SherlockHolmes","myCRAN")
#' 	author = "Zeeberg"
#' 	from<-"2023-01-01"
#' 	myCRAN(author=author,from=from,plotNew=TRUE)
#' 	myCRAN(packages=packages,when="last-week",plotNew=FALSE)
#' 	}
#'
#' @return returns no values, but has side effect of generating plots
#'   of daily and cumulative number of downloads of your packages
#'
#' @export
myCRAN<-
	function(packages=NULL,author=NULL,from,when,plotNew=TRUE,plotWidth=12) {
		l<-list()
		s<-vector("integer",length(packages))
		if(!is.null(author))
		  packages<-pkgsearch::advanced_search(Author = author, size = 100)$package

		for(pack in packages) {
			l[[pack]]<-cran_downloads(pack,from=from,when=when)
		}
		max<-0
		for(pack in names(l)) {
			max<-max(max,l[[pack]][,"count"])
			s[pack]<-sum(l[[pack]][,"count"])
		}

		ylim<-c(0,max)

		# next list is the original list that I used until around February 14, 2024
		# colors<-c("green","blue","red","orange","magenta","black")
		# next list comes from https://sashamaps.net/docs/resources/20-colors/
		# I omit white from the original list

		# colors<-c("red","green","blue","orange","magenta","pink","teal",
  	#	    "brown","beige","maroon","mint","olive","apricot","navy","grey","black","lavender","yellow","cyan","lime","purple")

		colors<-c("red","green","blue","orange","magenta","pink","#469990",
		          "brown","beige","maroon","#aaffc3","#808000","#ffd8b1","navy","grey","black","lavender","yellow","cyan","#bfef45","purple")

		xlabel<-"date"
		ylabel<-"counts"
		title<-"My R Packages Downloads"

		if(plotNew)
		  dev.new(width=plotWidth,unit="in")
		plot(x=l[[packages[1]]][,"date"],y=l[[packages[1]]][,"count"],ylim=ylim,type="l",
			xlab=xlabel,ylab=ylabel,main=title)
		i<-1
		for(pack in names(l)) {
			lines(x=l[[packages[1]]][,"date"],y=l[[pack]][,"count"],col=colors[i])
			i<-i+1
		}
		leg<-sprintf("%s %d",names(l),s[names(l)])
		legend("topleft",legend=leg,fill=colors)

		# cumulative distributions
		lc<-list()

		max<-0
		for(pack in names(l))
			max<-max(max,sum(l[[pack]][,"count"]))

		ylim<-c(0,max)

		for(pack in names(l)) {
			lc[[pack]]$cum<-vector("integer",nrow(l[[pack]]))
			lc[[pack]]$cum[1]<-l[[pack]][1,"count"]
			for(i in 2:nrow(l[[pack]]))
				lc[[pack]]$cum[i]<-lc[[pack]]$cum[i-1]+l[[pack]][i,"count"]
		}
		ylabel<-"cumulative counts"
		#plot(x=l[[packages[1]]][,"date"],lc[["SherlockHolmes"]]$cum,ylim=ylim,type="l",
		#	xlab=xlabel,ylab=ylabel,main=title)
		plot(x=l[[packages[1]]][,"date"],lc[[names(l)[1]]]$cum,ylim=ylim,type="l",
		     xlab=xlabel,ylab=ylabel,main=title)
		i<-1
		for(pack in names(l)) {
			lines(x=l[[packages[1]]][,"date"],y=lc[[pack]]$cum,col=colors[i])
			i<-i+1
		}
		legend("topleft",legend=leg,fill=colors)
	}
