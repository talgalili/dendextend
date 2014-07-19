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




# require(dendextend)
# require(whisker)



# plot dendrogram to html string. 
#
# d a dendrogram object
# height/widht : pixels, height/widht of the plot
# rightmargin  : pixels to reserve on the right side for leaf labels.
#
d3dendrogram <- function(d,height=500,width=700,rightmargin=200){
  e <- new.env()
  e$json_dendrogram <- as.json.dendrogram(d)
  e$height <- height
  e$width <- width
  e$rightmargin <- rightmargin
  whisker.render(d3dendro_template(),data=e)
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







d3dendro_template <- function(){
'<!doctype html>
<html><head>
<style>
  .node circle {           
    fill: #fff;           
    stroke: steelblue;           
    stroke-width: 1.5px;   
  }   
  .node {           
    font: 14px sans-serif;   
  }   
  .link {           
    fill: none;           
    stroke: #ccc;          
    stroke-width: 1.5px;   
  }   
  line {           
    stroke: black;   
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
      .attr("r", 4.5);           
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
      .data(x.ticks(5))           
      .enter().append("line")            
      .attr("class","ticks")            
      .attr("x1", function(d) { return xinv(d); })           
      .attr("y1", -5)            
      .attr("x2", function(d) {return xinv(d); })            
      .attr("y2", 5);       
    g.selectAll(".label")            
      .data(x.ticks(5))            
      .enter().append("text")            
      .attr("class","label")            
      .text(String)            
      .attr("x", function(d) {return xinv(d); })            
      .attr("y", -5)           
      .attr("text-anchor","middle");
  </script>
  </body>
</html>'
}