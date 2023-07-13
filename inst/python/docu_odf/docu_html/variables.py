import os.path
import pandas as pd
import docu_odf.docu_html.categories as categories

def make(temp_dir):
    if (os.path.isfile(temp_dir + "/variables.csv") == True) :
        variables = ("<h2> Variables </h2> \n")
        variables_df = pd.read_csv(temp_dir + "/variables.csv", encoding='utf8')
        variables_df = variables_df.fillna("")
        all_vars = variables_df['variable']
        for var in range(len(all_vars)) :
          variables = variables + "<div id = 'block'> \n"    
          variables = variables + "<ul> \n"
          for i in list(variables_df.columns):
              if (i != "url") :
                  variables = variables + ("<li><b>" + i + ": </b>" + 
                  str(variables_df[i].loc[var]) + "</li> \n")
              if (i == "url") :   
                  variables = variables + ("<li><b>" + i + ": </b>" + 
                  "<a href='" + str(variables_df[i].iloc[var]) + 
                  "' target = '_blank'>" +
                  str(variables_df[i].iloc[var]) + "</a> </li> \n")
          # categories
          variables = variables + categories.make(temp_dir, 
          variable = variables_df["variable"].loc[var])
          variables = variables + "</ul> \n"  
          variables = variables + "</div> \n"    
          
    else:
        variables = ("")
    return(variables)
