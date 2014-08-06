# Copyright (C) Mark van der Loo and Tal Galili
#
# This file is part of dendextend.
#
# dendextend is free software: you can redistribute it and/or modify it
# under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 2 of the License, or
# (at your option) any later version.
#
# dendextend is distributed in the hope that it will be useful, but
# WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
#  A copy of the GNU General Public License is available at
#  http://www.r-project.org/Licenses/
#


#' Plot dendrogram to webpage and view. 
#'
#' @param d            a dendrogram object
#' @param height       pixels, height of the plot.  
#' @param width        pixels, width of the plot
#' @param rightmargin  pixels to reserve on the right side for leaf labels.
#' @param open         open the graphic in a browser? (see details).
#' @param ...          Graphical options, see \code{\link{d3dendro_options}}.
#'
#' @details
#' If \code{open=TRUE}, the graphic is opened using the function defined by \code{getOption("viewer")}.
#' If no \code{viewer} option is specified, \code{utils::browseURL} is opened. Specifically, in RStudio
#' this means that the viewer is used.
#'
#' @section Tips:
#' Dendrogram plots with many end-nodes tend to get overlapping labels or (end)nodes. If the number of endnodes
#' is not too large, there are several ways to overcome this.
#' \itemize{
#' \item{Decrease font size of the nodes (\code{node_font}, see \code{\link{d3dendro_options}} on how to specify)} 
#' \item{Decrease size of node radius (\code{node_radius}, \emph{idem})}
#' \item{Increase the height of the plot. Use about (2 times font height) times count_terminal_nodes(d).}
#' \item{Plot sub-dendrograms in stead of the whole dendrogram. There's only so much 
#'   detail a screen can display and your eye can observe. Consider plotting subdendrograms or other summaries 
#'   when dendrograms have many, many nodes.}
#' }
#'
#'
#' @return
#' If \code{open=TRUE}, a character string containing the html is invisibly retured.
#' If \code{open=TRUE}, a character string containing the html is returned.
#' 
d3dendrogram <- function(d,height=500,width=700,rightmargin=200, open=TRUE,...){
  # set default options.
  e <- d3dendro_options(...,copy=TRUE)
  
  # compute json rep of dendrogram
  e$json_dendrogram <- as.json.dendrogram(d)
  e$height <- height
  e$width <- width
  e$rightmargin <- rightmargin
  
  # render webpage.
  html <- whisker::whisker.render(d3dendro_template(),data=e)

  if (open){ # open in (customized) viewer
     tmpfile <- tempfile(fileext = ".html")
     write(html,file=tmpfile)
     v <- getOption('viewer')
     view <- if ( !is.null(v) ) v else function(x) utils::browseURL(x)
     view(tmpfile)
     return(invisible(html))
  }
  return(html)
}

as.json.dendrogram <- function(d){
  # internal helper function
  add_json <- function(x){
    v <- attributes(x)
    lab <- ifelse(is.null(v$label),"",v$label)
    json <<- paste(json,sprintf('{ "name" : "%s", "y" : %s',lab,v$height))
    if ( is.leaf(x) ){
      json <<- paste(json,"}\n")
    } else {  
      json <<- paste(json,',\n "children" : [' )
      for ( i in seq_along(x) ){ 
        add_json(x[[i]])
        s <- ifelse(i<length(x),",","")
        json <<- paste(json,s)
      }
      json <<- paste(json," ]}")
    }
  }
  json <- ""
  add_json(d)
  json
}

#' Get or set current defaults for d3dendrogram
#' 
#' @param ... \code{<option>=<value>} pairs (see below). If no option/value pair is given, a list with 
#'    current settings is returned. If the single input \code{"reset"} is given, options
#'    are restored to factory settings.
#' @param copy only when options are set: set global options or return a modified copy of the global
#'    option environment invisibly?
#'    
#' @section Options:
#' Option values are exported as-is to the generated javascript/webpage. 
#' 
#' \tabular{ll}{
#'   \code{node_fill (char,"#fff")}             \tab Node color (fill) \cr
#'   \code{node_stroke (char;"steelblue")}      \tab Node color (circumference)\cr
#'   \code{node_stroke_width (char;"1.5px")}    \tab Width of node circumference \cr
#'   \code{node_radius (num;4.5)}               \tab Size of node (pixels) \cr
#'   \code{node_font (char;"14px sans-serif")}  \tab Font used to print labels. Size and type spec\cr
#'   \code{link_fill (char;"none")}             \tab link fill color (you probably do not want to set this)\cr
#'   \code{link_stroke (char;"#ccc")}           \tab link line color (that's the one you want)\cr
#'   \code{link_stroke_width (char;"1.5px")}    \tab link line width (pixels) \cr
#'   \code{axis_stroke (char;"black")}          \tab Color of axis line\cr
#'   \code{axis_width (char; "2px")}            \tab Width of axis line\cr
#'   \code{axis_ntick (num; 5)}                 \tab Approximate nr of tick marks\cr
#'   \code{axis_ticklength_in (num;5)}          \tab Length of tickmarks into the graph (pixels)  \cr
#'   \code{axis_ticklength_out (num;5)}         \tab Length of tickmarks out of the graph (pixels)\cr
#'   \code{axis_tickmark_offset (num;8)}        \tab Distance between axis and tick labels (pixels)
#' }
#'
#' @export 
d3dendro_options <- function(...,copy=FALSE){
   e <- if (!copy) D3DENDRODEFAULTS else as.environment(as.list(D3DENDRODEFAULTS))
   opts <- list(...)
   if (length(opts) == 0)
      return( as.list(e) )
   if ( length(opts) == 1 && opts[[1]] == "reset" )
      d3dendro_defaults(e)
   else
      for ( n in names(opts) ) assign(n, opts[[n]], e)
   return(invisible(e))
}

# global variable holding d3 dendro options. Initialized .onLoad()
D3DENDRODEFAULTS <- new.env()
# function setting defaults for d3 dendrogram plotting
# e: an environment
d3dendro_defaults <- function(e){
   e$node_fill <- "#fff"
   e$node_stroke <- "steelblue"
   e$node_stroke_width <- "1.5px"
   e$node_radius <- 4.5
   e$node_font <- "14px sans-serif"
   e$link_fill <- "none"
   e$link_stroke <- "#ccc"
   e$link_stroke_width <- "1.5px"
   e$axis_stroke <- "black"
   e$axis_width <- "2px"
   e$axis_ntick <- 5
   e$axis_ticklength_in <- 5
   e$axis_ticklength_out <- 5
   e$axis_tickmark_offset <- 8
   e
}




d3dendro_template <- function(){
'<!doctype html>
<html><head>
<style>
  .node circle {           
    fill: {{{node_fill}}};
    stroke: {{{node_stroke}}};           
    stroke-width: {{{node_stroke_width}}};   
  }   
  .node {
    font: {{{node_font}}};   
  }   
  .link {           
    fill: {{{link_fill}}};           
    stroke: {{{link_stroke}}};          
    stroke-width: {{{link_stroke_width}}};   
  }   
  line {           
    stroke: {{{axis_stroke}}};
    stroke-width: {{{axis_width}}};
  }
</style>
<script type="text/javascript" src="http://d3js.org/d3.v3.min.js"></script>
</head>
  <body>
  <script type="text/javascript">
    var width = {{{width}}};   
    var height = {{{height}}};  
    var cluster = d3.layout.cluster()           
      .size([height, width-200]);   
    var diagonal = d3.svg.diagonal()           
      .projection (function(d) { return [x(d.y), y(d.x)];});   
    var svg = d3.select("body").append("svg")           
      .attr("width",width)           
      .attr("height",height)           
      .append("g")          
      .attr("transform","translate(100,0)");   
    var xs = [];   
    var ys = [];   
    function getXYfromJSONTree(node){           
      xs.push(node.x);          
      ys.push(node.y);           
      if(typeof node.children != "undefined"){                   
        for ( j in node.children){                           
          getXYfromJSONTree(node.children[j]);                   
        }           
      }   
    }   
    var ymax = Number.MIN_VALUE;  
    var ymin = Number.MAX_VALUE;   
    var xmax = Number.MIN_VALUE;  
    var xmin = Number.MAX_VALUE;   

    var json =  {{{json_dendrogram}}}

    getXYfromJSONTree(json);          
    var nodes = cluster.nodes(json);           
    var links = cluster.links(nodes);           

    nodes.forEach( function(d,i){                   
      if(typeof xs[i] != "undefined"){                           
        d.x = xs[i];                   
      }                   
      if(typeof ys[i] != "undefined"){                           
        d.y = ys[i];                   
      }           
    });           
    nodes.forEach( function(d){                   
      if(d.y > ymax)
        ymax = d.y;
      if(d.y < ymin)                           
        ymin = d.y;           
    });           
    nodes.forEach( function(d){                   
      if(d.x > xmax)
        xmax = d.x;
      if(d.x < xmin)                           
        xmin = d.x;           
    });
    xinv = d3.scale.linear().domain([ymin, ymax]).range([0, width-{{{rightmargin}}}]);           
    x = d3.scale.linear().domain([ymax, ymin]).range([0, width-{{{rightmargin}}}]);
    y = d3.scale.linear().domain([xmin, xmax]).range([60,height-50]);
    var link = svg.selectAll(".link")                  
      .data(links)                   
      .enter().append("path")                   
      .attr("class","link")                   
      .attr("d", diagonal);           
    var node = svg.selectAll(".node")                  
      .data(nodes)                   
      .enter().append("g")                   
      .attr("class","node")                   
      .attr("transform", function(d) {                     
    return "translate(" + x(d.y) + "," + y(d.x) + ")";               
    });           
    node.append("circle")                   
      .attr("r", {{{node_radius}}});           
    node.append("text")                   
      .attr("dx", function(d) { return d.children ? -8 : 8; })                   
      .attr("dy", 3)                  
      .style("text-anchor", function(d) { return d.children ? "end" : "start"; })           
      .text( function(d){ return d.name;});       
    var g = d3.select("svg").append("g")            
      .attr("transform","translate(100,40)");       
    g.append("line")            
      .attr("x1",x(ymin))           
      .attr("y1",0)            
      .attr("x2",x(ymax))            
      .attr("y2",0);       
    g.selectAll(".ticks")            
      .data(x.ticks({{{axis_ntick}}}))           
      .enter().append("line")            
      .attr("class","ticks")            
      .attr("x1", function(d) { return xinv(d); })           
      .attr("y1", -{{{axis_ticklength_out}}})            
      .attr("x2", function(d) {return xinv(d); })            
      .attr("y2", {{{axis_ticklength_in}}});
    g.selectAll(".label")            
      .data(x.ticks({{{axis_ntick}}}))            
      .enter().append("text")            
      .attr("class","label")            
      .text(String)            
      .attr("x", function(d) {return xinv(d); })            
      .attr("y", -{{{axis_tickmark_offset}}})           
      .attr("text-anchor","middle");
  </script>
  </body>
</html>'
}
