# CityTreesProject
We generated a new dataset of N=5,660,237 trees from 63 of the largest US cities with detailed information on location, health, nativity status, and species.  We analyzed (i) biodiversity, (ii) spatial structure, (iii) abundance of native trees, and (iv) tree health. 

## Abstract
Sustainable cities depend on urban forests. City trees -- a pillar of urban forests -- improve our health, clean the air, store CO2, and cool local temperatures. Comparatively less is known about urban forests as ecosystems, particularly their spatial composition, nativity statuses, biodiversity, and tree health. Here, we assembled and standardized a new dataset of N=5,660,237 trees from 63 of the largest US cities. The data comes from tree inventories conducted at the level of cities and/or neighborhoods. Each data sheet includes detailed information on tree location, species, nativity status (whether a tree species is naturally occurring or introduced), health, size, whether it is in a park or urban area, and more (comprising 28 standardized columns per datasheet). This dataset could be analyzed in combination with citizen-science datasets on bird, insect, or plant biodiversity; social and demographic data; or data on the physical environment. Urban forests offer a rare opportunity to intentionally design biodiverse, heterogenous, rich ecosystems.

## Methods
See eLife manuscript for full details. Below, we provide a summary of how the dataset was collected and processed.

 

## Data Acquisition

We limited our search to the 150 largest cities in the USA (by census population). 

To acquire raw data on street tree communities, we used a search protocol on both Google and Google Datasets Search (https://datasetsearch.research.google.com/). We first searched the city name plus each of the following: street trees, city trees, tree inventory, urban forest, and urban canopy (all combinations totaled 20 searches per city, 10 each in Google and Google Datasets Search). We then read the first page of google results and the top 20 results from Google Datasets Search. If the same named city in the wrong state appeared in the results, we redid the 20 searches adding the state name. If no data were found, we contacted a relevant state official via email or phone with an inquiry about their street tree inventory. Datasheets were received and transformed to .csv format (if they were not already in that format). We received data on street trees from 64 cities. One city, El Paso, had data only in summary format and was therefore excluded from analyses.


 

## Data Cleaning
All code used is in the zipped folder Data S5 in the eLife publication. Before cleaning the data, we ensured that all reported trees for each city were located within the greater metropolitan area of the city (for certain inventories, many suburbs were reported - some within the greater metropolitan area, others not). 

First, we renamed all columns in the received .csv sheets, referring to the metadata and according to our standardized definitions (Table S4). To harmonize tree health and condition data across different cities, we inspected metadata from the tree inventories and converted all numeric scores to a descriptive scale including “excellent,” “good”, “fair”, “poor”, “dead”, and “dead/dying”. Some cities included only three points on this scale (e.g., “good”, “poor”, “dead/dying”) while others included five (e.g., “excellent,” “good”, “fair”, “poor”, “dead”).

Second, we used pandas in Python (W. McKinney & Others, 2011) to correct typos, non-ASCII characters, variable spellings, date format, units used (we converted all units to metric), address issues, and common name format. In some cases, units were not specified for tree diameter at breast height (DBH) and tree height; we determined the units based on typical sizes for trees of a particular species. Wherever diameter was reported, we assumed it was DBH. We standardized health and condition data across cities, preserving the highest granularity available for each city. For our analysis, we converted this variable to a binary (see section Condition and Health). We created a column called “location_type” to label whether a given tree was growing in the built environment or in green space. All of the changes we made, and decision points, are preserved in Data S9. 

Third, we checked the scientific names reported using gnr_resolve in the R library taxize (Chamberlain & Szöcs, 2013), with the option Best_match_only set to TRUE (Data S9). Through an iterative process, we manually checked the results and corrected typos in the scientific names until all names were either a perfect match (n=1771 species) or partial match with threshold greater than 0.75 (n=453 species). BGS manually reviewed all partial matches to ensure that they were the correct species name, and then we programmatically corrected these partial matches (for example, Magnolia grandifolia-- which is not a species name of a known tree-- was corrected to Magnolia grandiflora, and Pheonix canariensus was corrected to its proper spelling of Phoenix canariensis). Because many of these tree inventories were crowd-sourced or generated in part through citizen science, such typos and misspellings are to be expected. 

Some tree inventories reported species by common names only. Therefore, our fourth step in data cleaning was to convert common names to scientific names. We generated a lookup table by summarizing all pairings of common and scientific names in the inventories for which both were reported. We manually reviewed the common to scientific name pairings, confirming that all were correct. Then we programmatically assigned scientific names to all common names (Data S9). 

Fifth, we assigned native status to each tree through reference to the Biota of North America Project (Kartesz, 2018), which has collected data on all native and non-native species occurrences throughout the US states. Specifically, we determined whether each tree species in a given city was native to that state, not native to that state, or that we did not have enough information to determine nativity (for cases where only the genus was known).

Sixth, some cities reported only the street address but not latitude and longitude. For these cities, we used the OpenCageGeocoder (https://opencagedata.com/) to convert addresses to latitude and longitude coordinates (Data S9). OpenCageGeocoder leverages open data and is used by many academic institutions (see https://opencagedata.com/solutions/academia).

Seventh, we trimmed each city dataset to include only the standardized columns we identified in Table S4.

After each stage of data cleaning, we performed manual spot checking to identify any issues.

# Usage Notes
Column definitions are in Column_Headers_Dryad.csv .

All Methods and descriptions are in eLife manuscript.
