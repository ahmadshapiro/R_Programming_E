 # Returns TRUE if the user has calculated a value equal to that calculated by the given expression.
 calculates_same_value <- function(expr){
   e <- get("e", parent.frame())
   # Calculate what the user should have done.
   eSnap <- cleanEnv(e$snapshot)
   val <- eval(parse(text=expr), eSnap)
   passed <- isTRUE(all.equal(val, e$val))
   if(!passed)e$delta <- list()
   return(passed)
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