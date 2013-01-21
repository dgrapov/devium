DEVIUM
======

<b>D</b>ynamic multivariat<b>E</b> data analysis And <b>VI</b>s<b>U</b>alization Platfor<b>M</b>

A GUI for interactive plotting and analysis of multivariate data.
 
 Including: 
 
  - GUI using RGtk2 toolkit implemented with gWidgests 
 
  - Dynamic plotting - base and ggplot2 
 
  - Linked brushing of multiple plots -  iplots
 
  - Network Visualizations - RCytoscape
 
  - Analyses - univariate and multivariate
 
  - Automated report generation - rsweave
  
  - Data import and linking with MS Excel and Google Spreadsheets
  
  - Successor to <a href="https://sourceforge.net/projects/imdev/">imDEV</a>, including improved interface and capabilities

Installation
======
<p>Eventually devium will be installed from R as a package and from alternative repositories where it can be bundled with GTK+ components.
For now it can be installed by using the source code in the devium/R directory.
Copy and paste the following code in to R to source the necessary files.</p>

Run the following code below (copy and paste into the R console), which includes the`function` `source.dir` which is used to get files from github (depends on `package` `Rcurl`for https access).
```R
#function to get files from github, needs package Rcurl to acces https
source.dir<-function(type="file",dir=getwd(),
 file.list=
  c("https://raw.github.com/dgrapov/devium/master/R/Devium%20GUI%20elements.r",
				"https://raw.github.com/dgrapov/devium/master/R/Devium%20Plotting%20Functions.r",
				"https://raw.github.com/dgrapov/devium/master/R/Devium%20common%20functions.R",
				"https://raw.github.com/dgrapov/devium/master/R/Devium%20network%20functions.r"))
	{
		#check to see the type of source
		switch(type,
		"file" = .local<-function()
					{
						o.dir<-getwd()
						setwd(dir)
						obj<-dir()
						sapply(1:length(obj),function(i)
							{
								tryCatch(source(obj[i]),error=function(e){print(paste("can't load:",obj[i]))})
							})
						setwd(o.dir)	
					},
		"https" = .local<-function(file.list)	
					{
						if(require(Rcurl)==FALSE){install.packages("Rcurl");library(RCurl)} else { library(RCurl)}
						if(is.null(file.list)){return()}else{obj<-file.list}
						sapply(1:length(obj),function(i)
						{
							tryCatch( eval( expr = parse( text = getURL(obj[i],
							   ssl.verifypeer=FALSE) ),envir=.GlobalEnv),error=function(e){print(paste("can't load:",obj[i]))})
						})
					}
				)
			.local(file.list=file.list)
	}
 
#now install devium
source.dir(type="https") # github
devium.gui()
 
#load a data set for a demo
data<-iris
#end
```

