#' Self generating STARD diagram
#'
#' Create STARD diagram from study data.
#' 
#' @param data Data set with study and diagnostic information for each participant, including those that were not ultimately included
#' @param orders A named vector or a list, names as the variable in the dataset
#' and values as labels in the box. The order of the diagram will be based on this.
#' @param side_box Variable vector, appeared as side box in the diagram. The next
#'  box will be the subset of the missing values of these variables.
#' @param side_note Variable vector, appeared as side box in the diagram. The difference to the
#' box is that the following boxes will not be subset.
#' @param index_test Name of the index_test variable. The
#'  diagram will split into branches on this variables forward.
#' @param labels Named vector, names is the location of the terminal node. The
#' position location should plus 1 after the allocation variables if the allocation
#' is defined.
#' @param cex Multiplier applied to font size, Default is 0.8#'
#' @param col Colour for labels (blue by default)
#' @param fill Fill for labels (white by default)
#' @param fontfam Font family for labels (Arial by default)
#' @param type Final assigned diagnosis, to be included in the bottom box of the STARD diagram
#' @param text_width a positive integer giving the target column for wrapping
#' lines in the output. String will not be wrapped if not defined (default).
#' The \code{\link[stringi]{stri_wrap}} function will be used if \code{stringi}
#' package installed, otherwise \code{\link[base]{strwrap}} will be used.
#'
#' @details
#' The calculation of numbers is as in an analogous to Kirchhoff's Laws of
#' electricity. The numbers in terminal nodes must sum to those in the ancestor
#'  nodes. All the drop outs will be populated as a side box. Which was different
#'  from the official STARD diagram template, which has dropout inside a
#'   vertical node.
#'
#' @return A \code{STARD} object.
#'
#' @export
#'
#' @seealso \code{\link{add_side_box}},\code{\link{add_split}},
#' \code{\link{add_side_box}} \code{\link{textbox}}
#' @example inst/examples/stard-plot-example.R
#' @import grid
#' @importFrom stats na.omit
#'
stard_plot <- function(data,
                       orders,
                       side_box,
                       index_test,
                       type = NULL,
                       side_note = NULL,
                       labels = NULL,
                       cex = 0.8,
                       text_width = NULL,
                       col = "#4F81BD",
                       fill = "white",
                       fontfam = 'Helvetica') {
  options(txt_gp = gpar(cex = cex,
                        fontfamily = fontfam))
  on.exit(options(txt_gp = gpar()))
  
  if (is.list(orders)) {
    orders <- unlist(orders)
  }
  
  # If all defined variables included in the orders
  if (!all(c(side_box, index_test, type, side_note) %in% names(orders))) {
    not_in <- which(!c(side_box, index_test, side_note, type) %in% names(orders))
    not_in <- c(side_box, index_test, side_note, type)[not_in]
    stop("Variable ", paste(not_in, collapse = ", "), " not included in the `orders`")
  }
  
  # If all the orders variables included in the dataset
  if (!all(names(orders) %in% names(data))) {
    not_in <- which(!names(orders) %in% names(data))
    not_in <- names(orders)[not_in]
    stop("Variable ", paste(not_in, collapse = ", "), " can not be found in the data")
  }
  
  if (!is.null(index_test) & length(index_test) > 1) {
    stop("Only one index_test supported")
  }
  
  if (!is.null(index_test)) {
    if (length(unique(stats::na.omit(data[[index_test]]))) < 2) {
      warning("Single values in the index_test, will be ignored")
      index_test <- NULL
    } else {
      pos_arm <- which(index_test == names(orders))
      
      if(pos_arm == length(orders)){
        orders <- c(orders[c(1:pos_arm)])
      } else {
        orders <- c(orders[c(1:pos_arm)],
                    "split_data_variable" = "Group",
                    orders[(pos_arm + 1):length(orders)])
      }
      
      
      data$split_data_variable <- data[[index_test]]
    }
  }
  
  # gp_list <- vector(mode = "list", length = length(orders))
  for (indx in seq_along(orders)) {
    i <- names(orders)[indx]
    
    if (indx == 1) { # Potentially eligible population
      txt <- paste0(orders[indx], " (n=", pret_num(sum(!is.na(data[[i]]))), ")")
      gp_list <- add_box(txt = txt, text_width = text_width)
      data <- data[!is.na(data[[i]]), ]
    } else {
      
      if (is.data.frame(data)) {
        val <- data[[i]]
      } else {
        val <- sapply(data, function(x) x[[i]], simplify = FALSE)
      }
      
      if ((i %in% side_box) | (i %in% side_note)) {
        txt <- gen_text(x = val, label = orders[indx], bullet = TRUE)
        
        gp_list <- add_side_box(gp_list,
                                txt = txt,
                                text_width = text_width
        )
        
        if(i %in% side_box){
        data <- sub_data(data, i)
        }
        
      } else if (i %in% type){
        txt <- gen_text(x = val, label = orders[indx], bullet = TRUE)
        
        gp_list <- add_box(gp_list,
                             txt = txt,
                             text_width = text_width,
                           just = 'left'
        )
        
      } else if (i == "split_data_variable") {
        txt <- gen_text(data[[i]])
        gp_list <- add_split(gp_list,
                             txt = txt,
                             text_width = text_width
        )
        
        data <- data[!is.na(data[[i]]), ]
        data <- split(data, as.factor(data[[i]]))
      } else {
        txt <- gen_text(x = val, label = orders[indx], bullet = FALSE)
        
        gp_list <- add_box(gp_list,
                           txt = txt,
                           text_width = text_width
        )
      }
    }
  }
  
  if (!is.null(labels)) {
    if (any(is.na(as.numeric(names(labels))))) {
      stop("Labels must be a named vector with names as the position of
           the node excluding side node.")
    }
    
    gp_list <- add_label_box(gp_list, txt = labels,
                             only_terminal = TRUE,
                             col = col,
                             fill = fill,
                             fontfam = fontfam)
  }
  
  return(gp_list)
}



# Subset missing data for the next step
#' @keywords internal
sub_data <- function(data, var) {
  if (!is.data.frame(data)) {
    sapply(data, function(x) x[is.na(x[[var]]), ],
           simplify = FALSE
    )
  } else {
    data[is.na(data[[var]]), ]
  }
}
