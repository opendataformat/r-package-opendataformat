def make(title, css):   
  head = (
    "<html> \n" +
    "<head> \n" +
    "<meta charset='utf-8'> \n" +
    "<link rel='preconnect' href='https://fonts.googleapis.com'> \n" +
    "<link rel='preconnect' href='https://fonts.gstatic.com' crossorigin> \n" +
    "<link href='https://fonts.googleapis.com/css2?family=Roboto:wght@300;400&display=swap' rel='stylesheet'> \n" +
    "<style> \n" + 
    css + "\n" + 
    "</style> \n" +
    "<title>" + title + "</title> \n" +
    "</head> \n" 
  )
  return head

