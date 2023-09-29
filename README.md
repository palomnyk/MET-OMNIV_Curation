# beef_biomarkers

# Harmonizing data
## harmonization\p1_build_ESHA_csv.R
- Script for building tables from ESHA output - making columns from indention
- Meant to give a starting point to our harmonization dictionaries
- Combines ESHA from multiple projects into the same table
    - currently combines the MAP and MED studies
- Has unit test: harmonization\unit_tests\test_build_ESHA_csv.R
## harmonization\p2_add_FPED_data.R
- Script for adding HEI/FPED categories to ESHA output
- Creates an HEI conversion table and adds HEI variables to the combined ESHA file.
- Has unit test: harmonization\unit_tests\test_add_FPED_data.R
## harmonization/p3_automatic_label_proteins.R
### Script for labeling the protein sources based off of rudimentary text mining and adding HEI values
#### rudimentary text mining
- Looks for key words in Item_name based off "dictoraries"
- Identifies protein sources and whether it is processed or not
- In script unit testing based on Lauren's hand processed script
#### Used the labeled proteins and the HEI conversion table to add HEI equivalants to combined ESHA reports
