import os.path
import pandas as pd

def make(temp_dir):
    if (os.path.isfile(temp_dir + "/dataset.csv") == True) :
        dataset = ("<h2> Dataset </h2> \n <div id = 'block'> \n <ul>")
        dataset_df = pd.read_csv(temp_dir + "/dataset.csv", encoding='utf8')
        dataset_df = dataset_df.fillna("")
        for i in list(dataset_df.columns):
            if (i != "url") :
                dataset = dataset + ("<li><b>" + i + ": </b>" + str(dataset_df[i].iloc[0]) + "</li> \n")
            if (i == "url") :   
                dataset = dataset + ("<li><b>" + i + ": </b>" + 
                "<a href='" + str(dataset_df[i].iloc[0]) + "' target = '_blank'>" +
                str(dataset_df[i].iloc[0]) + "</a> </li> \n")
        if (os.path.isfile(temp_dir + "/variables.csv") == True) :
          variables_df =  pd.read_csv(temp_dir + "/variables.csv", encoding='utf8', usecols = ['variable'])
          variables_list = variables_df['variable'].values.tolist()
          variables = ', '.join([str(item) for item in variables_list])
          dataset = dataset + ("<li><b>variables: </b>" + variables + "</li> \n")
        dataset = dataset + "</ul> </div>"
    else:
        dataset = ("")
    return(dataset)
