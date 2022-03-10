1) preliminary city-specific edits
2) master raw spreadsheets
3) sheets to clean
		run code file fix_data_misspellings.ipynb
4) data misspellings corrected
		run code taxize_name_corrections.R
5) taxize name corrections
		run code common_name_corrections.R
6) common name corrections
		run code BONAP_native_check.R
7) BONAP native assignments (delete old files)
		run code add_coordinates.R
8) convert addresses to lat-long coordinates for files that need that	\
		run code get_coordinates.ipynb (this happened in the background)
9) add coordinates (completed by Weilin; so just merge certain files with their completed form)
		run code select_columns.R
10) final column selection (delete old files)
---- 
