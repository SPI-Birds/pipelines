import os
import csv
import re
from pathlib import Path
from datetime import date, datetime, time
from access_parser import AccessParser


def export_access_db(dsn, table, output_dir, header=True, delim=",", quote='"'):
    """
    Export selected tables from an Access database to .csv files through the Python library AccessParser
    
    Parameters:
    -----------
    dsn : str
        File path to the MS Access database.
    table : str or list
        Names of tables to export. Can be single or multiple.
    output_dir : str
        Where the exported .csv files will be saved.
    header : bool, optional
        Column names in header? Default: True.
    delim : str, optional
        Column delimiter. Default: ','.
    quote : str, optional
        Quote character. Default: '"'.
        
    Returns:
    --------
    None
        Creates .csv files of selected tables from the Access database
    """
    
    # Check if the output directory is valid
    if not isinstance(output_dir, str) or len(output_dir.strip()) == 0:
        raise ValueError("`output_dir` should be a non-empty string")
    
    # Convert single table name to list
    if isinstance(table, str):
        table = [table]
    
    # Create directory where new files will be saved
    Path(output_dir).mkdir(parents=True, exist_ok=True)
    
    # Initialize AccessParser object to interact with MS Access database
    db = AccessParser(dsn)
    
    # Process each table
    for table_name in table:
        # Create output file path
        out_file = os.path.join(output_dir, f"{table_name}.csv")
        
        try:
            # Parse the table data
            table_data = db.parse_table(table_name)
            
            # Write to CSV file
            with open(out_file, 'w', newline='', encoding='utf-8') as csvfile:
                # Get column names and store them in a list
                columns = list(table_data.keys())
                
                # Run the csv_writer function with the input parameters
                if quote:
                    csv_writer = csv.writer(csvfile, delimiter=delim, quotechar=quote, 
                                          quoting=csv.QUOTE_MINIMAL)
                else:
                    csv_writer = csv.writer(csvfile, delimiter=delim, quoting=csv.QUOTE_NONE)
                
                # Write header if specified
                if header and columns:
                    csv_writer.writerow(columns)
                
                # Write data rows
                if columns: # Run the following logic on all columns
                    # We run on all columns to prevent manually specifying what columns we should transform
                    # It should only transform columns with datetime, date, time or in string format that matches datetime patterns
                    # Might need changes later

                    # Get the number of rows
                    num_rows = len(table_data[columns[0]]) if columns else 0
                    
                    for row_index in range(num_rows):
                        # Convert to the correct date format
                        row = []
                        for col in columns:
                            val = table_data[col][row_index]

                            # Format datetime to be consistent with rJackcess format
                            # Constantly validate in what format the datetime is stored
                            # Otherwise, errors with Dplyr and Lubridate in the R code might pop up

                            if isinstance(val, (datetime, date, time)):
                                if isinstance(val, datetime):
                                    # Format as YYYY-MM-DDTHH:MM to match rJackcess
                                    val = val.strftime('%Y-%m-%dT%H:%M')
                                elif isinstance(val, date):
                                    # Format as YYYY-MM-DD to match rJackcess
                                    val = val.strftime('%Y-%m-%d')
                                elif isinstance(val, time):
                                    # Format as HH:MM to match rJackcess
                                    val = val.strftime('%H:%M')

                            # If the value is a string, we can check if it with a regrex pattern
                            elif isinstance(val, str):
                                # Convert datetime strings to consistent format
                                # Check for full datetime pattern with optional seconds
                                if re.match(r"^\d{4}-\d{2}-\d{2} \d{2}:\d{2}(:\d{2})?$", val):
                                    # Convert to YYYY-MM-DDTHH:MM format
                                    dt = datetime.strptime(val.split('.')[0], '%Y-%m-%d %H:%M' if len(val) <= 16 else '%Y-%m-%d %H:%M:%S')
                                    val = dt.strftime('%Y-%m-%dT%H:%M')

                            row.append(val)

                        csv_writer.writerow(row)
                        
            print(f"Successfully exported table '{table_name}' to {out_file}")
            
        except Exception as e:
            print(f"Error exporting table '{table_name}': {str(e)}")
    
    