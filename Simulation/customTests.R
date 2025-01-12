match_call <- function(correct_call = NULL) {
  e <- get("e", parent.frame())
  # Trivial case
  if(is.null(correct_call)) return(TRUE)
  # Get full correct call
  full_correct_call <- expand_call(correct_call)  
  # Expand user's expression
  expr <- deparse(e$expr)
  full_user_expr <- expand_call(expr)
  # Compare function calls with full arg names
  identical(full_correct_call, full_user_expr)
}

# Utility function for match_call answer test
# Fills out a function call with full argument names
expand_call <- function(call_string) {
  # Quote expression
  qcall <- parse(text=call_string)[[1]]
  # If expression is not greater than length 1...
  if(length(qcall) <= 1) return(qcall)
  # See if it's an assignment
  is_assign <- is(qcall, "<-")
  # If assignment, process righthandside
  if(is_assign) {
    # Get righthand side
    rhs <- qcall[[3]]
    # If righthand side is not a call, can't use match.fun()
    if(!is.call(rhs)) return(qcall)
    # Get function from function name
    fun <- match.fun(rhs[[1]])
    # match.call() does not support primitive functions
    if(is.primitive(fun)) return(qcall)
    # Get expanded call
    full_rhs <- match.call(fun, rhs)
    # Full call
    qcall[[3]] <- full_rhs
  } else { # If not assignment, process whole thing
    # Get function from function name
    fun <- match.fun(qcall[[1]])
    # match.call() does not support primitive functions
    if(is.primitive(fun)) return(qcall)
    # Full call
    qcall <- match.call(fun, qcall)
  } 
  # Return expanded function call
  qcall
}

notify <- function() {
  e <- get("e", parent.frame())
  if(e$val == "No") return(TRUE)
  
  good <- FALSE
  while(!good) {
    # Get info
    message("\nMake sure you're connected to the internet\n")
    
    ID <- readline_clean("What is your ID? ")
    
    # Repeat back to them
    message("\nDoes everything look good?\n")
    message("Your ID: ", ID, "\n")
    
    yn <- select.list(c("Yes", "No"), graphics = FALSE)
    if(yn == "Yes") good <- TRUE
  }
  
  # Get course and lesson names
  course_name <- gsub(" ","+", attr(e$les, "course_name"))
  lesson_name <- gsub(" ","+", attr(e$les, "lesson_name"))
  
  library(httr)
  baseLink <- "https://docs.google.com/forms/d/e/1FAIpQLScou0ZvvEqj5KHjtuF8dIWqUDC6Pw7wZ1whcI2X8d8ta0vsIw/formResponse?&submit=Submit?usp=pp_url"

  idAttr <- "&entry.2112451087="
  courseNameAttr <- "&entry.527410779="
  lessonNameAttr <- "&entry.2005159868="

  toPost <- paste0(baseLink, idAttr, ID, courseNameAttr, course_name, lessonNameAttr, lesson_name)

  response <- POST(toPost) 
  if (response['status_code'] == 200) {
    message("\n Successfully Submited")
  } else {
    message("\n Please screen shot and contact your instructor")
  }

  TRUE
}

readline_clean <- function(prompt = "") {
  wrapped <- strwrap(prompt, width = getOption("width") - 2)
  mes <- stringr::str_c("| ", wrapped, collapse = "\n")
  message(mes)
  readline()
}

hrule <- function() {
  message("\n", paste0(rep("#", getOption("width") - 2), collapse = ""), "\n")
}