import os.path
import pandas as pd

def make(temp_dir, color, my_var):
    if (os.path.isfile(temp_dir + "/categories.csv") == True) :
      # get data
      categories_df = pd.read_csv(temp_dir + "/categories.csv", encoding='utf8')
      categories_df = categories_df.fillna("")
      # check if variable has categories
      if my_var in categories_df['variable'].values :
        # print header
        print(color.BOLD + "categories:" + color.END)
        # print categories information
        print(categories_df[
          categories_df.variable == my_var].loc[
            : ,categories_df.columns != "variable"].to_string(index=False))
      else:
        print("")
    else:
        print("")
    
