# Stats-154


This is Stats 154 Project 2

On Piazza, you will find a zip archive with three files: image1.txt, image2.txt, image3.txt. Each contains one picture from the satellite. Each of these files contains several
rows each with 11 columns described in the Table below. All five radiance angles are raw
features, while NDAI, SD, and CORR are features that are computed based on subject matter
knowledge. More information about the features is in the article yu2008.pdf. The sensor
data is multi-angle and recorded in the red-band. For more information about MISR, see
http://www-misr.jpl.nasa.gov/.
01 y coordinate
02 x coordinate
03 expert label (+1 = cloud, -1 = not cloud, 0 unlabeled)
04 NDAI
05 SD
06 CORR
07 Radiance angle DF
08 Radiance angle CF
09 Radiance angle BF
10 Radiance angle AF
11 Radiance angle AN

We split the project in both python and R.The project can be reproduced as follows:

For the EDA of the three images and summary, just follow the code from Project 2.ipynb

Data preparation of Question 2.(a) - 2.(c) is from project2.R. The split of the data has been saved as csv and are used in Project 2.ipynb

Question 3 and 4 are all in Project 2.ipynb. The questions has been commented with the meaning of the steps