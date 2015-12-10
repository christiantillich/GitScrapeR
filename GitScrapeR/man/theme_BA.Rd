\name{theme_BA}
\alias{theme_BA}
\docType{data}
\title{
%%   ~~ data name/kind ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of the dataset. ~~
}
\usage{data("theme_BA")}
\format{
  The format is:
List of 51
 $ line                :List of 4
  ..$ colour  : chr "black"
  ..$ size    : num 0.5
  ..$ linetype: num 1
  ..$ lineend : chr "butt"
  ..- attr(*, "class")= chr [1:2] "element_line" "element"
 $ rect                :List of 4
  ..$ fill    : chr "white"
  ..$ colour  : chr "black"
  ..$ size    : num 0.5
  ..$ linetype: num 1
  ..- attr(*, "class")= chr [1:2] "element_rect" "element"
 $ text                :List of 8
  ..$ family    : chr "sans"
  ..$ face      : chr "plain"
  ..$ colour    : chr "black"
  ..$ size      : num 12
  ..$ hjust     : num 0.5
  ..$ vjust     : num 0.5
  ..$ angle     : num 0
  ..$ lineheight: num 0.9
  ..- attr(*, "class")= chr [1:2] "element_text" "element"
 $ axis.text           :List of 8
  ..$ family    : NULL
  ..$ face      : NULL
  ..$ colour    : NULL
  ..$ size      : NULL
  ..$ hjust     : NULL
  ..$ vjust     : NULL
  ..$ angle     : NULL
  ..$ lineheight: NULL
  ..- attr(*, "class")= chr [1:2] "element_text" "element"
 $ strip.text          :List of 8
  ..$ family    : NULL
  ..$ face      : NULL
  ..$ colour    : NULL
  ..$ size      : NULL
  ..$ hjust     : NULL
  ..$ vjust     : NULL
  ..$ angle     : NULL
  ..$ lineheight: NULL
  ..- attr(*, "class")= chr [1:2] "element_text" "element"
 $ axis.line           :List of 4
  ..$ colour  : chr "black"
  ..$ size    : NULL
  ..$ linetype: NULL
  ..$ lineend : NULL
  ..- attr(*, "class")= chr [1:2] "element_line" "element"
 $ axis.title          :List of 8
  ..$ family    : NULL
  ..$ face      : chr "italic"
  ..$ colour    : NULL
  ..$ size      : NULL
  ..$ hjust     : NULL
  ..$ vjust     : NULL
  ..$ angle     : NULL
  ..$ lineheight: NULL
  ..- attr(*, "class")= chr [1:2] "element_text" "element"
 $ axis.title.x        :List of 8
  ..$ family    : NULL
  ..$ face      : NULL
  ..$ colour    : NULL
  ..$ size      : NULL
  ..$ hjust     : NULL
  ..$ vjust     : num -1
  ..$ angle     : NULL
  ..$ lineheight: NULL
  ..- attr(*, "class")= chr [1:2] "element_text" "element"
 $ axis.title.y        :List of 8
  ..$ family    : NULL
  ..$ face      : NULL
  ..$ colour    : NULL
  ..$ size      : NULL
  ..$ hjust     : NULL
  ..$ vjust     : num 2.5
  ..$ angle     : num 90
  ..$ lineheight: NULL
  ..- attr(*, "class")= chr [1:2] "element_text" "element"
 $ axis.text           :List of 8
  ..$ family    : NULL
  ..$ face      : NULL
  ..$ colour    : NULL
  ..$ size      : NULL
  ..$ hjust     : NULL
  ..$ vjust     : NULL
  ..$ angle     : NULL
  ..$ lineheight: NULL
  ..- attr(*, "class")= chr [1:2] "element_text" "element"
 $ axis.text.x         :List of 8
  ..$ family    : NULL
  ..$ face      : NULL
  ..$ colour    : NULL
  ..$ size      : NULL
  ..$ hjust     : NULL
  ..$ vjust     : NULL
  ..$ angle     : num 45
  ..$ lineheight: NULL
  ..- attr(*, "class")= chr [1:2] "element_text" "element"
 $ axis.text.y         :List of 8
  ..$ family    : NULL
  ..$ face      : NULL
  ..$ colour    : NULL
  ..$ size      : NULL
  ..$ hjust     : NULL
  ..$ vjust     : NULL
  ..$ angle     : NULL
  ..$ lineheight: NULL
  ..- attr(*, "class")= chr [1:2] "element_text" "element"
 $ axis.ticks          : list()
  ..- attr(*, "class")= chr [1:2] "element_blank" "element"
 $ axis.ticks.x        :List of 4
  ..$ colour  : NULL
  ..$ size    : NULL
  ..$ linetype: NULL
  ..$ lineend : NULL
  ..- attr(*, "class")= chr [1:2] "element_line" "element"
 $ axis.ticks.y        :List of 4
  ..$ colour  : NULL
  ..$ size    : NULL
  ..$ linetype: NULL
  ..$ lineend : NULL
  ..- attr(*, "class")= chr [1:2] "element_line" "element"
 $ axis.ticks.length   :Class 'unit'  atomic [1:1] 0.15
  .. ..- attr(*, "unit")= chr "cm"
  .. ..- attr(*, "valid.unit")= int 1
 $ axis.ticks.margin   :Class 'unit'  atomic [1:1] 0.1
  .. ..- attr(*, "unit")= chr "cm"
  .. ..- attr(*, "valid.unit")= int 1
 $ axis.line           :List of 4
  ..$ colour  : NULL
  ..$ size    : NULL
  ..$ linetype: NULL
  ..$ lineend : NULL
  ..- attr(*, "class")= chr [1:2] "element_line" "element"
 $ axis.line.x         :List of 4
  ..$ colour  : NULL
  ..$ size    : NULL
  ..$ linetype: NULL
  ..$ lineend : NULL
  ..- attr(*, "class")= chr [1:2] "element_line" "element"
 $ axis.line.y         :List of 4
  ..$ colour  : NULL
  ..$ size    : NULL
  ..$ linetype: NULL
  ..$ lineend : NULL
  ..- attr(*, "class")= chr [1:2] "element_line" "element"
 $ legend.background   :List of 4
  ..$ fill    : NULL
  ..$ colour  : logi NA
  ..$ size    : NULL
  ..$ linetype: NULL
  ..- attr(*, "class")= chr [1:2] "element_rect" "element"
 $ legend.margin       :Class 'unit'  atomic [1:1] 0.2
  .. ..- attr(*, "unit")= chr "cm"
  .. ..- attr(*, "valid.unit")= int 1
 $ legend.key          :List of 4
  ..$ fill    : NULL
  ..$ colour  : logi NA
  ..$ size    : NULL
  ..$ linetype: NULL
  ..- attr(*, "class")= chr [1:2] "element_rect" "element"
 $ legend.key.size     :Class 'unit'  atomic [1:1] 1.2
  .. ..- attr(*, "unit")= chr "lines"
  .. ..- attr(*, "valid.unit")= int 3
 $ legend.key.height   : NULL
 $ legend.key.width    : NULL
 $ legend.text         :List of 8
  ..$ family    : NULL
  ..$ face      : NULL
  ..$ colour    : NULL
  ..$ size      : NULL
  ..$ hjust     : NULL
  ..$ vjust     : NULL
  ..$ angle     : NULL
  ..$ lineheight: NULL
  ..- attr(*, "class")= chr [1:2] "element_text" "element"
 $ legend.text.align   : NULL
 $ legend.title        :List of 8
  ..$ family    : NULL
  ..$ face      : NULL
  ..$ colour    : NULL
  ..$ size      : NULL
  ..$ hjust     : NULL
  ..$ vjust     : NULL
  ..$ angle     : NULL
  ..$ lineheight: NULL
  ..- attr(*, "class")= chr [1:2] "element_text" "element"
 $ legend.title.align  : NULL
 $ legend.position     : chr "right"
 $ legend.direction    : chr "vertical"
 $ legend.justification: chr "center"
 $ legend.box          : NULL
 $ panel.background    :List of 4
  ..$ fill    : NULL
  ..$ colour  : logi NA
  ..$ size    : NULL
  ..$ linetype: NULL
  ..- attr(*, "class")= chr [1:2] "element_rect" "element"
 $ panel.border        :List of 4
  ..$ fill    : logi NA
  ..$ colour  : logi NA
  ..$ size    : NULL
  ..$ linetype: NULL
  ..- attr(*, "class")= chr [1:2] "element_rect" "element"
 $ panel.margin        :Class 'unit'  atomic [1:1] 0.25
  .. ..- attr(*, "unit")= chr "lines"
  .. ..- attr(*, "valid.unit")= int 3
 $ panel.grid          :List of 4
  ..$ colour  : NULL
  ..$ size    : NULL
  ..$ linetype: NULL
  ..$ lineend : NULL
  ..- attr(*, "class")= chr [1:2] "element_line" "element"
 $ panel.grid.major    :List of 4
  ..$ colour  : chr "#CCCCCC"
  ..$ size    : NULL
  ..$ linetype: NULL
  ..$ lineend : NULL
  ..- attr(*, "class")= chr [1:2] "element_line" "element"
 $ panel.grid.major.x  :List of 4
  ..$ colour  : NULL
  ..$ size    : NULL
  ..$ linetype: NULL
  ..$ lineend : NULL
  ..- attr(*, "class")= chr [1:2] "element_line" "element"
 $ panel.grid.major.y  :List of 4
  ..$ colour  : NULL
  ..$ size    : NULL
  ..$ linetype: NULL
  ..$ lineend : NULL
  ..- attr(*, "class")= chr [1:2] "element_line" "element"
 $ panel.grid.minor    : list()
  ..- attr(*, "class")= chr [1:2] "element_blank" "element"
 $ panel.grid.minor.x  :List of 4
  ..$ colour  : NULL
  ..$ size    : NULL
  ..$ linetype: NULL
  ..$ lineend : NULL
  ..- attr(*, "class")= chr [1:2] "element_line" "element"
 $ panel.grid.minor.y  :List of 4
  ..$ colour  : NULL
  ..$ size    : NULL
  ..$ linetype: NULL
  ..$ lineend : NULL
  ..- attr(*, "class")= chr [1:2] "element_line" "element"
 $ plot.background     :List of 4
  ..$ fill    : NULL
  ..$ colour  : NULL
  ..$ size    : NULL
  ..$ linetype: NULL
  ..- attr(*, "class")= chr [1:2] "element_rect" "element"
 $ plot.title          :List of 8
  ..$ family    : NULL
  ..$ face      : chr "bold"
  ..$ colour    : NULL
  ..$ size      : num 25
  ..$ hjust     : num 0.5
  ..$ vjust     : num 4
  ..$ angle     : NULL
  ..$ lineheight: NULL
  ..- attr(*, "class")= chr [1:2] "element_text" "element"
 $ plot.margin         :Class 'unit'  atomic [1:4] 0.1 0.1 0.1 0.1
  .. ..- attr(*, "unit")= chr "npc"
  .. ..- attr(*, "valid.unit")= int 0
 $ strip.background    :List of 4
  ..$ fill    : NULL
  ..$ colour  : NULL
  ..$ size    : NULL
  ..$ linetype: NULL
  ..- attr(*, "class")= chr [1:2] "element_rect" "element"
 $ strip.text          :List of 8
  ..$ family    : NULL
  ..$ face      : NULL
  ..$ colour    : NULL
  ..$ size      : NULL
  ..$ hjust     : NULL
  ..$ vjust     : NULL
  ..$ angle     : NULL
  ..$ lineheight: NULL
  ..- attr(*, "class")= chr [1:2] "element_text" "element"
 $ strip.text.x        :List of 8
  ..$ family    : NULL
  ..$ face      : NULL
  ..$ colour    : NULL
  ..$ size      : NULL
  ..$ hjust     : NULL
  ..$ vjust     : NULL
  ..$ angle     : NULL
  ..$ lineheight: NULL
  ..- attr(*, "class")= chr [1:2] "element_text" "element"
 $ strip.text.y        :List of 8
  ..$ family    : NULL
  ..$ face      : NULL
  ..$ colour    : NULL
  ..$ size      : NULL
  ..$ hjust     : NULL
  ..$ vjust     : NULL
  ..$ angle     : num -90
  ..$ lineheight: NULL
  ..- attr(*, "class")= chr [1:2] "element_text" "element"
 - attr(*, "class")= chr [1:2] "theme" "gg"
 - attr(*, "complete")= logi TRUE
}
\details{
%%  ~~ If necessary, more details than the __description__ above ~~
}
\source{
%%  ~~ reference to a publication or URL from which the data were obtained ~~
}
\references{
%%  ~~ possibly secondary sources and usages ~~
}
\examples{
data(theme_BA)
## maybe str(theme_BA) ; plot(theme_BA) ...
}
\keyword{datasets}
