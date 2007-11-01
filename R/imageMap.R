## We import the generic from geneplotter, which we depend on


checkTags <- function(x, tagname, nn)
{
    if(is.null(names(x))) {
        if(length(x)==length(nn)) {
            names(x)=nn
        } else {
            stop(paste("'tags$", tagname, "' must have names if ",
                       "it is not the same length as the number ",
                       "of nodes in 'object'.", sep=""))
        }
    } else {
        if(!all(names(x) %in% nn))
            stop(paste("'names(tags$", tagname, ")' must match ",
                       "the names of the nodes in 'object'",
                       sep=""))
    }
    return(x)
}


setMethod("imageMap",
  signature=signature(object="Ragraph", con="connection", tags="list",
    imgname="character"),
  definition=function(object, con, tags, imgname, width, height, usr = par("usr")) {

  .Deprecated("toFile", "Rgraphviz")

  if(any(par("mai")!=0))
    warning("If par('mai') are not all 0, the result of this function (imageMap) may not be useful.")

  nn = sapply(AgNode(object), function(x) x@name)
  for(i in seq(along=tags))
    tags[[i]] = checkTags(tags[[i]], names(tags)[i], nn)

  if( !is.numeric(width) ||  length(width)!=1 )
    stop("'width' must be numeric of length 1.")
  if( !is.numeric(height) || length(height)!=1 )
    stop("'height' must be numeric of length 1.")
  if( !is.numeric(usr) || length(usr)!=4 )
    stop("'usr' must be numeric of length 4.")
  
  ## transform user to pixel coordinates
  x.u2p = function(x) { rx=(x-usr[1])/diff(usr[1:2]); stopifnot(all(rx>=0&rx<=1)); return(rx*width)  }
  y.u2p = function(y) { ry=(usr[4]-y)/diff(usr[3:4]); stopifnot(all(ry>=0&ry<=1)); return(ry*height) }

  nxy = getNodeXY(object)
  nh  = getNodeHeight(object)/2
  xl  =   floor(x.u2p( nxy$x - getNodeLW(object) ))
  xr  = ceiling(x.u2p( nxy$x + getNodeRW(object) ))
  yu  =   floor(y.u2p( nxy$y - nh ))
  yl  = ceiling(y.u2p( nxy$y + nh ))
  names(xl) = names(xr) = names(yu) = names(yl) = nn

  mapname <- paste("map", gsub(" |/|#|:", "_", imgname), sep="_")
  base::writeLines(paste("<IMG SRC=\"", imgname, "\" USEMAP=#", mapname, " BORDER=0>",
                   "<MAP NAME=\"", mapname, "\">", sep=""), con)
  for(nd in unique(unlist(lapply(tags, names)))) {
    out = paste("<AREA SHAPE=\"rect\" COORDS=\"", xl[nd], ",", yl[nd], ",",
                xr[nd], ",", yu[nd], "\"", sep="")
    for(i in seq(along=tags))
      if(nd %in% names(tags[[i]]))
        out = paste(out, " ", names(tags)[i], "=\"", tags[[i]][nd], "\"", sep="")
    out = paste(out, ">", sep="")
    base::writeLines(out, con)
  }
  base::writeLines("</MAP>", con)
} ## end of definition
) ## end of setMethod




## FH 10/31/07: Since there was a major overhaul of the Rgraphviz
## package this method is now much simpler for graph objects.
## Note that calling renderGraph will return a graph object with the
## coordinates of the nodes attached in the renderInfo slot;
## graphRenderInfo(foo, "nativeCoords") should get
## the coordinates in native caling [0,1], graphRenderInfo(foo, "figDim")
## gives the dimensions of the device the graph was rendered on.
## The user can still give width and height of the image file here
## in which case we will ignore the values in the graph and
## compute pixel coordinated from the native coordinates. 
## Margins should be handled ok now already in renderGraph.
setMethod("imageMap",
          signature=signature(object="graph", con="connection",
          tags="list",imgname="character"),
          definition=function(object, con, tags, imgname,
          width, height)
      {
          missW <- missing(width)
          missH <- missing(height)
          if(xor(missW, missH))
             stop("Need both width and height of the image to compute ",
                  "coordinates", call.=FALSE)

          ## check for valid tags 
          nn <- nodes(object)  
          for(i in seq(along=tags))
              tags[[i]] = checkTags(tags[[i]], names(tags)[i], nn)

          
          ## get pixel coordinates from the graph object
          coords <- graphRenderInfo(object, "nativeCoords")
          if(is.null(coords))
              stop("You first need to render this graph object.\n",
                   "    e.g. foo <- renderGraph(foo)", call.=FALSE)
          if(missW){
              width <- graphRenderInfo(object, "figDim")[1]
              height <- graphRenderInfo(object, "figDim")[2]
          }else{
              if( !is.numeric(width) ||  length(width)!=1 )
                  stop("'width' must be numeric of length 1.")
              if( !is.numeric(height) || length(height)!=1 )
                  stop("'height' must be numeric of length 1.")
          }
          coords[,c(1,3)] <- apply(coords[,c(1,3)],2,function(x) x*width)
          coords[,c(2,4)] <- apply(coords[,c(2,4)],2,function(x) x*height)
          
          ## build imageMap
          mapname <- paste("map", gsub(" |/|#|:", "_", imgname), sep="_")
          base::writeLines(paste("<IMG SRC=\"", imgname, "\" USEMAP=#",
                                 mapname, " BORDER=0><MAP NAME=\"", mapname,
                                 "\">", sep=""), con)
          for(nd in unique(unlist(lapply(tags, names)))) {
              out <- paste("<AREA SHAPE=\"rect\" COORDS=\"", coords[nd,1], ",",
              coords[nd,4], ",", coords[nd,3], ",", coords[nd,2], "\"",
                           sep="")
              for(i in seq(along=tags))
                  if(nd %in% names(tags[[i]]))
                      out = paste(out, " ", names(tags)[i], "=\"",
                      tags[[i]][nd], "\"", sep="")
              out <- paste(out, ">", sep="")
              base::writeLines(out, con)
          }
          base::writeLines("</MAP>", con)
      })

