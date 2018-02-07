#' import stringi
#' @name msp
#' @rdname msp

#' 
#' @title Fix misspelling and different in similar character data.
#'

#' @param x a character to search
#' @param y the vector containing x and order character
#' @param r the desire level of adjustmen of the search by default 0.5
#' @param type an charactaer indicating the remplacement procudure. Se details for mor information
#' @description Look for similatiries between a caharacter and a vector character to detect misspeling problems and change it.
#'
#' @return a same object than \code{x} without misspelling
#' @details Type define the way of the rigth option to replace is choose,  |code{ask} generate a interactive replacement process, \code{first} use the fisrt element of the list as correct element to repalce and \code{long} use the longer element as correct to replace. The default value is \code{ask}

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
 
msp <- function(x,y,r=0.5,type = "ask"){
		change <- NULL
		goodone <- NULL
		add <- NULL
		#checks
		if(is.character(x)!=TRUE) stop(" You must provide a character")
		
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
				skips<-readline("Please indicate the registers to skip: ")
				skips<-as.integer(unlist(strsplit(skips," ")))			
				n.search <- seq_along(search)
				change <<- n.search[-skips]
			} else {
				change <<- seq_along(search)
			}
		}

		type.first<-function(search){
			goodone <<- 1L
			change <<- seq_along(search)
		}

		type.long<-function(search){
			long <- sapply(search,nchar)
			goodone <<- which(long==max(long))[1L]
			change <<- seq_along(search)	
		}	
	
		#global function
		clean.y <- y
		unique.y <- unique(y)
		search <- agrep(x,unique.y, max.distance = r,value=TRUE)
		if(length(search)==0) stop("No mactching, nothing to replace")
		if (length(search)==1) {
			 type.first(search)
		} else if (type =="ask"){ 
			 type.ask(search) 
		} else if (type=="first"){ 
			 type.first(search) 
		} else  { 
			 type.long(search) 
		}
		if (exists("add")==TRUE){
			replace<-add
		} else {
			replace <- search[as.numeric(goodone)]
		}
		for ( j in change){
			clean.y[which(y==search[change[j]])] <- replace
		}
	return(clean.y)
}
