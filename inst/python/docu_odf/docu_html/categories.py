import os.path
import pandas as pd

def make(temp_dir, variable):
    if (os.path.isfile(temp_dir + "/categories.csv") == True) :
        categories_df = pd.read_csv(temp_dir + "/categories.csv", encoding='utf8')
        categories_df = categories_df.fillna("")
        # check if variable has categories
        if variable in categories_df['variable'].values :
          categories = "<li><b>categories:</b></li> \n" + "<table> \n"
          categories = categories + "<tr> \n"
          for col in categories_df.columns :
              if col != "variable" :
                  categories = categories + "<th>" + col + "</th> \n"
          categories = categories + "</tr> \n"
          for cat in range(len(categories_df.loc[
            categories_df['variable'] == variable])) :
              categories = categories + "<tr>"
              for col in categories_df.columns:
                  if col != "variable" :
                      categories = categories + "<td>"
                      categories = categories + str(categories_df.loc[
                        categories_df['variable'] == variable][col].iloc[cat]) 
                      categories = categories + "</td>"
              categories = categories + "</tr>"
          categories = categories + "</table> \n"    
        else: 
          categories = ("")
    else:
        categories = ("")
    return(categories)
