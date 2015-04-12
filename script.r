# Knitr Handle

## ---- P1

# Script for R html table output

# set work dir
setwd("D:/Workspace/R/HTMLtable")

# load libraries
library(knitr)

# generate functions for table output

# first the general function. It sets the table and the general output settings
tabelize <- function(tab, class_table="table", class_row="table_row", class_cell="table_cell", class_colname="table_colname", class_rowname="table_rowname", cell_sep="\n", prefix=""){
	if(!is.matrix(tab) & !is.data.frame(tab)){
		stop("Passed data is not a matrix or data.frame. Exiting function.")
	}
	tabel <- "Tabelize object"
	
	attr(tabel, "data") <- tab # assign data
	#The following are the class names attributes
	attr(tabel, "class") <- "tabelizer" 
	attr(tabel, "class_table") <- class_table
	attr(tabel, "class_row") <- class_row
	attr(tabel, "class_cell") <- class_cell
	attr(tabel, "class_colname") <- class_colname
	attr(tabel, "class_rowname") <- class_rowname
	
	attr(tabel, "prefix") <- prefix # this is pasted in front of every class spec, if user wants different styles for different tables. Default is no prefix
	# Some layout attributes
	attr(tabel, "cell_sep") <- cell_sep
	
	# Return the table object, It can be edited further (perhaps in future) or printed to html by the print_tab command
	return(tabel)
}

print_tab <- function(tabel){
	if(!("tabelizer" %in% attributes(tabel))) {
		stop("Passed object is not a tabelizer object can not print anything")
	}
	
	if(attr(tabel, "class") != "tabelizer") {
		stop("Passed object is not a tabelizer object, can not print anything")
	}
	tab = attr(tabel, "data") # set the data
	
	# First print the table declaration
	cat(paste("<div class=\"", attr(tabel, "prefix"), attr(tabel, "class_table"), "\">\n", sep=""))
	
	# Print the column names
	# start row
	cat(paste("\t<div class=\"", attr(tabel, "prefix"), attr(tabel, "class_row"), "\">\n", sep=""))
	# Print empty cell for rownames column (empty here)
	cat(paste("\t\t<div class=\"", attr(tabel, "prefix"), attr(tabel, "class_colname"), "\"> ", "</div>", attr(tabel, "cell_sep"), sep=""))
	#Then print the colnames
	for(i in c(1:length(colnames(tab)))) {
		cat(paste("\t\t<div class=\"", attr(tabel, "prefix"), attr(tabel, "class_colname"), "\"> ", colnames(tab)[i], "</div>", attr(tabel, "cell_sep"), sep=""))
	}	
	#close the row
	cat("\t</div>\n")
	
	# loop through rows
	for(i in c(1:dim(tab)[1])){
		#start the row
		cat(paste("\t<div class=\"", attr(tabel, "prefix"), attr(tabel, "class_row"), "\">\n", sep=""))
		# print the rownames column
		cat(paste("\t\t<div class=\"", attr(tabel, "prefix"), attr(tabel, "class_rowname"), "\"> ", rownames(tab)[i], "</div>", attr(tabel, "cell_sep"), sep=""))
		 # Loop through the data columns
		for( j in c(1:dim(tab)[2])){
			#print the data
			cat(paste("\t\t<div class=\"", attr(tabel, "prefix"), attr(tabel, "class_cell"), "\"> ", tab[i, j], "</div>", attr(tabel, "cell_sep"), sep=""))
		}
		#stop row
		cat("\t</div>\n")
	}
	
	#Close the table declaration
	cat("</div>\n")
}

## ---- P2
# TESTING TESTING
x <- matrix(floor(runif(10, min = 1, max = 5)), 5, 2)
colnames(x) <- c("Column 1", "Column 2")
rownames(x) <- c("Row 1", "Row 2", "Row 3", "Row 4", "Row 5")
x1 <- tabelize(x)
print_tab(x1)

## ---- P3
f <- data.frame(c("D", "a", "v", "i", "d"), c(1:5), rep("test", times = 5))
colnames(f) <- c("Column 1", "Column 2", "Column 3")
rownames(f) <- c("Row 1", "Row 2", "Row 3", "Row 4", "Row 5")
f1 = tabelize(f)
print_tab(f1)