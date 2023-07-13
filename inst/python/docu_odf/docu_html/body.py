import docu_odf.docu_html.dataset as dataset
import docu_odf.docu_html.variables as variables

def make(temp_dir):
  body=(
    "<body> \n" + 
    "<h1>Documentation</h1> \n" + 
    dataset.make(temp_dir) + "\n" +
    variables.make(temp_dir) + "\n" +
    "</body>")
  return(body)
  
  
