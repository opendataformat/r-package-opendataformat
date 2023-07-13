import os.path
import pandas as pd
import docu_odf.docu_print.categories as categories

def make(temp_dir, color):
    if (os.path.isfile(temp_dir + "/variables.csv") == True) :
        # print header
        print(color.UNDERLINE + color.BOLD + "Variables" + color.END + "\n")
        # get data
        variables_df = pd.read_csv(temp_dir + "/variables.csv", encoding='utf8')
        variables_df = variables_df.fillna("")
        # print variables information
        for var in range(variables_df.shape[0]):
            for col in list(variables_df.columns):
                col_name = str(col)
                var_info = str(variables_df[col].loc[var])
                print(color.BOLD + col_name + ": " + color.END + var_info)
                # print("\n")
            my_var =  variables_df["variable"].loc[var]   
            categories.make(temp_dir, color, my_var)
            print("\n" + "\n")
    else:
        print("")
    
