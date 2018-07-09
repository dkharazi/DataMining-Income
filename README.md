## Overview

Where to put input data (income dataset):
After opening the provided R code, the user should change the directory or the input data, which can be done by modifying the “read_csv” command on line 19. If the user changed the input data from a .csv format, he or she should change it back in order for the command to function properly. Additionally, the user should ensure the the “ggplot” and “read” packages are installed properly, in order to open read the input data properly.

Where to find output data:
If each code block is run properly and orderly, two csv-formatted output files should be written to the user’s documents folder. The first file, named “CosineSimilarityOutput.csv,” is the output containing the top-k cosine similarities for each entry. The second file, named “JaccardSimilarityOutput.csv,” is the output containing the top-k jaccard similarities for each entry. If these files aren’t found in the documents folder, the user’s options most likely follow a different routine. In this case, simply search for the file names in the search directory to locate each file.

How to interpret the output:
According to the cosine similarity output, each entry includes its “ID,” a “k” amount of the greatest cosine proximities for its corresponding entry, and a “k” amount of indices that identify each corresponding proximity value. The first variable is labeled as “ID,” which is the given “ID” entries from the income dataset. The remaining columns are assigned to the “k” amount of index and proximity variables. The indices with the greatest cosine proximities are the leftmost variables, and the indices with decreasing cosine proximities are the rightmost variables.

How to adjust the “k” variable:
An adjustable k is provided, but the user will need to adjust it by changing "k" on lines 310 and 373. These lines are properly labeled, so the user shouldn't have an issue finding the adjustable "k" variable. The default for the "k" variable is 5.

The code should take between 5-10 minutes to compile. Please be patient during code compilation.
