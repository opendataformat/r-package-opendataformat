# Modules
import csv
import os
import shutil
import xml.etree.ElementTree as ET

# formatting xml output
def pretty_print(current, parent=None, index=-1, depth=0):
  for i, node in enumerate(current):
    pretty_print(node, current, i, depth + 1)
  if parent is not None:
    if index == 0:
      parent.text = '\n' + ('\t' * depth)
    else:
      parent[index - 1].tail = '\n' + ('\t' * depth)
    if index == len(parent) - 1:
      current.tail = '\n' + ('\t' * (depth - 1))

# get head of csv file              
def header(input_dir,csv_file):
  with open(input_dir+"/"+csv_file,"r") as file:
    dict_reader=csv.DictReader(file)
    header = dict_reader.fieldnames      
  return header

# get language codes of attribute  
def get_lang(input_dir,csv_file, attribute):
  lang = []
  for i in header(input_dir,csv_file):
    if i.startswith(attribute+'_') == True:
      lang_code = i.split('_')
      lang.append(lang_code[1])
  return lang  

# make output directory  
def make_output_dir(output_dir):
  if not os.path.exists(output_dir+'/'):        
    os.makedirs(output_dir+'/')

# copy data.csv from input dir to output dir
def copy_data_csv(input_dir, output_dir, export_data):
  if export_data == "yes" :
    shutil.copy(
      input_dir+'/data.csv',
      output_dir)

# make xml file from csv files          
def csv2xml(input_dir, output_dir):
  schema_name=ET.QName("http://www.w3.org/2001/XMLSchema-instance","schemaLocation")
  root = ET.Element("codeBook",
  {schema_name:"ddi:codebook:2_5 http://www.ddialliance.org/Specification/DDI-Codebook/2.5/XMLSchema/codebook.xsd",
  'xmlns':"ddi:codebook:2_5",},version='2.5',)
  # dataset
  # check if dataset.csv exists
  if os.path.exists(input_dir+"/dataset.csv") == True :
    fileDscr=ET.SubElement(root,'fileDscr')
    fileTxt=ET.SubElement(fileDscr,'fileTxt')
    with open(input_dir+"/dataset.csv",mode="r",encoding="utf-8") as file:
      dict_reader=csv.DictReader(file)
      for row in dict_reader:
        
        # dataset name
        fileName=ET.SubElement(fileTxt,'fileName')
        fileName.text=row['dataset']
        # get keys
        list_keys = row.keys()
      # dataset description
      if 'description' in list_keys:
        fileCont=ET.SubElement(fileTxt,'fileCont')
        fileCont.text=row['description']
      if any(item.startswith('description_') for item in list_keys) == True:
        for lang in get_lang(input_dir,"dataset.csv","description"):
          fileCont=ET.SubElement(fileTxt,'fileCont')
          fileCont.attrib['{http://www.w3.org/XML/1998/namespace}lang'] = lang
          fileCont.text=row['description_'+lang]
      # dataset label
      fileCitation=ET.SubElement(fileTxt,'fileCitation')
      titlStmt=ET.SubElement(fileCitation,'titlStmt')
      if 'label' in list_keys:
        titl=ET.SubElement(titlStmt,'titl')
        titl.text=row['label']
      if any(item.startswith('label_') for item in list_keys):
        for lang in get_lang(input_dir,"dataset.csv","label"):
          titl=ET.SubElement(titlStmt,'titl')
          titl.attrib['{http://www.w3.org/XML/1998/namespace}lang'] = lang
          titl.text=row['label_'+lang]
      # url
      if 'url' in list_keys:
        notes=ET.SubElement(fileDscr,'notes')
        extlink=ET.SubElement(notes,'ExtLink')
        extlink.attrib['URI']=row['url']
  if variables_arg == "yes" :
    
    if os.path.exists(input_dir+"/categories.csv") == True :
        list_categories=[]
        categories=open(input_dir+"/categories.csv", mode="r", encoding="utf-8")
        dict_categories = csv.DictReader(categories)
        for i in dict_categories:
          list_categories.append(i)
    
        if os.path.exists(input_dir+"/variables.csv") == True :
          # variables
          dataDscr=ET.SubElement(root,'dataDscr')
          with open(input_dir+"/variables.csv", mode="r", encoding="utf-8") as file:
            dict_reader=csv.DictReader(file)
            for row in dict_reader:
              var = ET.SubElement(dataDscr, 'var')
              var.attrib['name'] = row['variable']
              # get keys
              list_keys = row.keys()
              # variable label
              if 'label' in list_keys:
                labl = ET.SubElement(var, 'labl')
                labl.text = row['label']
              if any(item.startswith('label_') for item in list_keys):
                for lang in get_lang(input_dir,"variables.csv","label"):
                  labl = ET.SubElement(var, 'labl')
                  labl.attrib['{http://www.w3.org/XML/1998/namespace}lang'] = lang
                  labl.text = row['label_'+lang]
              # variable description    
              if 'description' in list_keys:
                txt = ET.SubElement(var, 'txt')
                txt.text = row['description']        
              if any(item.startswith('description_') for item in list_keys):  
                for lang in get_lang(input_dir,"variables.csv","description"):
                  txt = ET.SubElement(var, 'txt')
                  txt.attrib['{http://www.w3.org/XML/1998/namespace}lang'] = lang
                  txt.text = row['description_'+lang]
              if 'url' in list_keys:    
                if not row['url'] == '':
                  notes = ET.SubElement(var, 'notes')
                  ExtLink = ET.SubElement(notes, 'ExtLink')
                  ExtLink.attrib['URI'] = row['url']
              if 'type' in list_keys:
                if not row['type'] == '':
                  varFormat = ET.SubElement(var, 'varFormat')
                  varFormat.attrib['type'] = row['type']

              # variable categories        
              varname = row['variable']
              for line in list_categories:
                # get keys
                list_keys = line.keys()
                if line['variable'] == varname:
                  catgry = ET.SubElement(var, 'catgry')
                  # value
                  catValu = ET.SubElement(catgry, 'catValu')
                  catValu.text = line['value']
                  # value label
                  if 'label' in list_keys:
                    labl = ET.SubElement(catgry, 'labl')
                    labl.text = line['label']
                  if any(item.startswith('label_') for item in list_keys):  
                    for lang in get_lang(input_dir,"categories.csv","label"):
                      labl = ET.SubElement(catgry, 'labl')
                      labl.attrib['{http://www.w3.org/XML/1998/namespace}lang'] = lang
                      labl.text = line['label_'+lang]
  # write xml
  make_output_dir(output_dir)
  pretty_print(root)
  tree = ET.ElementTree(root)
  tree.write(output_dir+"/metadata.xml", xml_declaration=True, encoding="utf-8")
  copy_data_csv(input_dir, output_dir, export_data)
  shutil.make_archive(output_dir, "zip", output_dir)

if __name__ == '__main__':
  csv2xml(input_dir, output_dir)
