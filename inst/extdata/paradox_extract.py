from pypxlib import Table
from pandas import DataFrame

paradox_db = Table("C:/Users/Liam/Downloads/Paikat.DB")

col_names = list(paradox_db.fields.keys())

output_list = []

all_rows = list(range(0, len(paradox_db)))

for row_num in all_rows:
  
  row = paradox_db[row_num]
  
  empty_dict = {}
  
  for col in col_names:
    
    empty_dict[col] = row[col]

  output_list.append(empty_dict)

# for row_num in all_rows:
#   
#   row = paradox_db[row_num]
#   
#   empty_list = []
#   
#   for col in col_names:
#     
#     empty_list.append(row[col])
#     
#   output_dict[row_num] = empty_list

# for row in paradox_db:
#   
#   empty_list = []
#   
#   for col in col_names:
#     
#     empty_list.append(row[col])
#     
#   output_list.append(tuple(empty_list))
