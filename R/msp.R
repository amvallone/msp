# @import stringi
#' @import stringdist
#' @name msp
#' @rdname msp

#' 
#' @title Fix misspelling and different in similar character data.
#'

#' @param x a character to search
#' @param y the vector containing x and order character
#' @param r level of similarity between element, by default is 0.8
#' @param type an character indicating the replacement procedure. Se details for more information
#' @param ... additional arguments are passed on to \link{stringsim} function.
#' @description Look for similarities between a single character entry and a character vector to detect similarities dues to misspeling problems and fix it.
#'
#' @return a same object than \code{x} without misspelling
#' @details Type define the way of the rigth option to replace is choose,  |code{ask} generate a interactive replacement process, \code{first} use the fisrt element of the list as correct element to repalce and \code{min} use the smallest element as correct to replace. The default value is \code{ask}

#' @examples
#' \dontrun{
#' 	set.seed(1)
#' 	z <- stringi::stri_rand_strings(20, 5)
#' 	a <- z[1]
#' 	a1 <- c("GNZuC","GNZuC ", "GNZu.C"," GNZuC", "GNZ uC", "gNZuC")
#' 	z <- c(z,sample(z),sample(z,10),a1)
#' 	msp(a,z)
#' }
#' @export
 
msp <- function(x,y,r=0.8,type = "ask",...){
		change <- NULL
		goodone <- NULL
		add <- NULL
		skips <- NULL
		#checks
		if(length(a)>1) stop("Please provide a single element to search")
		if(is.character(x)!=TRUE) stop(" You must provide a character")
		if(!is.character(z)) stop("The target vector must be a character vector")
		#type definitions	
		type.ask<-function(search){
			display<-c(search, "None")
			cat(display, fill=max(nchar(search)) ,labels= paste0("[",seq_along(display),"]"))
			goodone <<- readline("Please select the correct option to replace: ")
			if ((length(search)+1)==as.numeric(goodone)){
				add <<- readline("Please introduce the correct option to replace: ")
				goodone <<- length(search)
			}
			skip <- readline("Do you want to skip any opcion? Yes (Y) No (N): ")
			if (skip =="Y"){
				skips <- readline("Please indicate the registers to skip: ")
				skips <<- as.integer(unlist(strsplit(skips," ")))			
			} 
		}

		type.first<-function(search){
			goodone <<- 1L
		}

		type.min<-function(search){
			long <- sapply(search,nchar)
			goodone <<- which(long==min(long))[1L]	
		}	
	
		#global function
		clean.y <- y
		unique.y <- unique(y)
		#search <- agrep(x,unique.y, max.distance = r,value=TRUE)
		sim <- stringdist::stringsim(x,unique.y,...)
		target <- which(sim >= r)
		search <- unique.y[target]
		change <- seq_along(search)
		if(length(search)==0) stop("No mactching, nothing to replace")
		if (length(search)==1) {
			 type.first(search)
		} else if (type =="ask"){ 
			 type.ask(search) 
		} else if (type=="first"){ 
			 type.first(search) 
		} else  { 
			 type.min(search) 
		}
		if(!is.null(add)){
			replace<-add
		} else {
			replace <- search[as.numeric(goodone)]
		}
		for ( j in change){
			if(j %in% skips) next
			clean.y[which(y==search[change[j]])] <- replace
		}
	return(clean.y)
}
