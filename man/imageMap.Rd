\name{imageMap-methods}
\docType{methods}

\alias{imageMap}
\alias{imageMap-methods}
\alias{imageMap,Ragraph-method}
\alias{imageMap,graph-method}
\alias{imageMap,Ragraph,connection,list,character-method}
\alias{imageMap,graph,connection,list,character-method}

\title{Write an HTML IMG tag together with a MAP image map.}
\description{Write an HTML IMG tag together with a MAP image map.}
\usage{
  \S4method{imageMap}{Ragraph,connection,list,character}(object, con, tags, imgname, width, height, usr = par("usr"))
\S4method{imageMap}{graph,connection,list,character}(object, con, tags, imgname, width, height)
}
\arguments{
  \item{object}{The graph layout for which we want to create an image map.}
  \item{con}{Connection to which the image map is written.}
  \item{tags}{Named list whose elements are named character vectors.
    Names must correspond to node names in \code{object}. See details.}
  \item{imgname}{Character. Name of the image file (for example PNG
    file) that contains the plot.}
  \item{width}{Width of the image.}
  \item{height}{Height of the image.}
  \item{usr}{Numeric vector of length 4. The user coordinates in the
    plot window that \code{object} was plotted into.}
}

\details{The most important tags are \code{TITLE}, \code{HREF},
  and \code{TARGET}. If the list \code{tags} contains an element
  with name \code{TITLE}, then this must be a named character vector
  containing the tooltips that are to be displayed when the mouse moves
  over a node. The names of the nodes are specified in the \code{names}
  attribute  of the character vector and must match those of
  \code{object}.
  
  Similarly, \code{HREF} may be used to specify hyperlinks that the
  browser can follow when the mouse clicks on a node, and \code{TARGET}
  to specify the target browser window.

  Currently, only rectangular regions are implemented; the actual
  shape of the nodes as specified in \code{object} is ignored.
  Also, tags for edges of the graph are currently not supported.

  This function is typically used with the following sequence
  of steps:
  \enumerate{
    \item Create a graph layout with \code{\link[Rgraphviz:agopen]{agopen}}
    \item Plot it into a bitmap device, e.g. \code{\link{jpeg}}
    or \code{\link{png}}. 
    \item Write HTML header.
    \item Call the \code{\link{imageMap}} function.
    \item Optionally, write further text into the HTML connection.
    \item Close HTML file.
  }

  The new API for plotting of graphs now also allows for this
  alternative procedure:
  \enumerate{
    \item Lay out the graph object \code{foo} using
    \code{\link[Rgraphviz]{layoutGraph}}
    \item render the graph on a bitmap device using
    \code{\link[Rgraphviz]{renderGraph}} like this:
    \code{foo <- renderGraph(foo)}
    \item Write HTML header.
    \item Call the \code{\link{imageMap}} on the graph object \code{foo}.
    \item Optionally, write further text into the HTML connection.
    \item Close HTML file.
  }
}

\value{The function is called for its side effect, which is writing text into
the connection \code{con}.}

\seealso{\code{\link[Rgraphviz]{agopen}} 
  %-does not exist \code{\link[prada:openHTMLpage]{openHTMLpage}}
}

\author{Wolfgang Huber \url{http://www.ebi.ac.uk/huber}}
\keyword{iplot}

\examples{
fhtml = paste(tempfile(), ".html", sep="")
fpng  =paste(tempfile(), ".png", sep="")

if(capabilities()["png"] && interactive()) {

  ## Create a random graph, make tooltips and hyperlinks
  set.seed(123)
  g  = randomEGraph(letters[14:22], 0.2)

  tooltip = paste("This is node", nodes(g))
  url = paste("This could be a link for node", nodes(g))
  names(url) = names(tooltip) = nodes(g)

  ## Open plot device
  width = height = 512
  png(fpng, width=width, height=height)
  par(mai=rep(0,4))

  ## Layout and render
  lg = agopen(g, name="My layout")
  plot(lg)

  ## Write an HTML file with the image map
  con = file(fhtml, open="wt")
  writeLines("<html><head><title>Click Me</title></head><body>\n", con)

  imageMap(lg, con, fpng, tags=list(HREF=url, TITLE=tooltip), width=width, height=height)

  writeLines("</body></html>", con)
  close(con)
  dev.off()

  cat("Now have a look at file", fhtml, "with your browser.\n")
  browseURL(fhtml)
}
}


