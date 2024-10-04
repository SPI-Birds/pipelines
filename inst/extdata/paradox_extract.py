from pypxlib import Table
#from pandas import DataFrame

def extract_paradox(path, file_name):
  paradox_db = Table(path + "/" + file_name)
  col_names = list(paradox_db.fields.keys())
  output_list = []
  all_rows = list(range(0, len(paradox_db)))
  for row_num in all_rows:
    row = paradox_db[row_num]
    empty_dict = {}
    for col in col_names:
      empty_dict[col] = row[col]
    output_list.append(empty_dict)
  return output_list

#Alternative, may be quicker. Leave for now.
# def extract_paradox(path, file_name):
#   paradox_db = Table(path + "/" + file_name)
#   col_names = list(paradox_db.fields.keys())
#   output_dict = {}
#   
#   #Build keys for each column each with an empty lis
#   for col in col_names:
#     output_dict[col] = []
#   
#   all_rows = list(range(0, len(paradox_db)))
#   for row_num in all_rows:
#     row = paradox_db[row_num]
#     for col in col_names:
#       output_dict[col].append(row[col])
#   return output_dict

