DEVIUM
======

<b>D</b>ynamic Multivariat<b>E</b> Data Analysis and <b>VI</b>s<b>U</b>alization Platfor<b>M</b>

## Source all devium R functions from github
```r
source("http://pastebin.com/raw.php?i=UyDBTA57")
```

[DeviumWeb] (https://github.com/dgrapov/DeviumWeb)
======
[Browser-based GUI for devium](https://github.com/dgrapov/DeviumWeb) implemented in [R](http://cran.us.r-project.org/) and [Shiny](http://www.rstudio.com/shiny/) (under development).

Examples 
======
Check out some prototypes and examples of the analyses implemented in ```Devium``` on the demo server. 
- [Heirarchical Cluster Analysis](http://spark.rstudio.com/dgrapov/Heatmap/)
- [Basic Plotting] (http://spark.rstudio.com/dgrapov/Plotting/)
- [Principal Components Analysis Diagnostics] (http://spark.rstudio.com/dgrapov/PCA/)
- [Metabolomic Network Generation](http://spark.rstudio.com/dgrapov/MetaMapR)

TODO
======
 - remove GTK GUI code (see below)

[GTK] (http://www.gtk.org/) 
======
(obsolete see [DeviumWeb](https://github.com/dgrapov/DeviumWeb))
  - GUI using RGtk2 toolkit implemented with gWidgests 
 
  - Dynamic plotting - base and ggplot2 
 
  - Linked brushing of multiple plots -  iplots
 
  - Network Visualizations - igraph (static, interactive and 3D), Cytoscape (RCytoscape)
 
  - Analyses - univariate and multivariate
 
  - Automated report generation - rsweave
  
  - Data import and linking with MS Excel and Google Spreadsheets
  
  - Successor to <a href="https://sourceforge.net/projects/imdev/">imDEV</a>, including improved interface and capabilities


Installation of GTK GUI
======
<p>Eventually devium will be installed from R as a package and from alternative repositories where it can be bundled with GTK+ components.
For now it can be installed by using the source code in the devium/R directory.
Copy and paste the following code in to R to source the necessary files.</p>

 ```r
 #load package from github repo
source("http://pastebin.com/raw.php?i=JVyTrYRD")

#check out some of the functions (accesory objects will be cleaned up)
objects()

#launch GUI (watch R console for messages regarding the downloading of dependancies)
devium.gui()

 ```
 Note if you don't yet have the GTK+ toolkit installed you need to specify to "Install GTK+" when prompted from the R command line.
 


