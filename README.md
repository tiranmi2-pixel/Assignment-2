# Market Reaction to Layoffs in R&D Firms

This project looks at how the stock market reacts when companies announce layoffs. It specifically compares the reaction for firms that invest a lot in research and development (high R&D) versus those that don't (low R&D). The full analysis is available in the `Report.docx` file.

### Key Findings

The study finds that the stock market treats these two types of firms very differently.

Investors tend to penalize high R&D firms with negative stock returns after a layoff announcement. But they often reward low R&D firms with positive returns for the same action. This suggests that layoffs at innovative firms are seen as damaging to future growth, while layoffs at other firms are seen as a smart financial move.

### Data Sources

The layoff announcement data comes from a public database compiled from Worker Adjustment and Retraining Notification (WARN) notices. This data was downloaded directly by the `01_Data_Extract.R` script.

The firm financial data and stock market data come from the Compustat and CRSP databases. These were accessed through Wharton Research Data Services (WRDS).

### Author Information

Tiran Minosh Ediriweera (tiranm11@outlook.com)
Author grants full rights for any individual to test,modify and utilize this replication package for any academic purpose. The credits must link to original repository.


### Repository Structure

The project is organized into several folders to keep the data, scripts, and outputs clean.
```
/Project
|
|-- Code/
|   |-- Calculate CAR & Consolidating Data.R
|   |-- Data Cleaning.R
|   |-- Data Extract.R
|   |-- Final Analysis.R
|   |-- Main Script.R
|   `-- Merge Dataframes (CAT & RD scores).R
|
|-- Raw Data/
|   `-- warn_data_2015_2024.csv
|
|-- Processed Data/
|   |-- Processed-warn.csv
|   |-- Matched-WARN-Compustat.csv
|   |-- Final_CAR_File.csv
|   `-- /Static Data/
|       |-- layoff_events_upload.txt
|       `-- (WRDS output CSVs, e.g., 1 day window.csv)
|
|-- Output/
|   |-- Final_Cleaned_Dataset.csv
|   |-- Table_1_Summary_Statistics_by_RD_Classification.png
|   |-- Table_2_Summary_Statistics_for_CARs_by_RD_Group.png
|   |-- Table_3_Mean_CARs_by_RD_Classification_and_Event_Window.png
|   |-- Table_4_T-Test_for_Difference_in_Mean_CARs.png
|   `-- Figure_1_Mean_CAR_by_RD_Group_and_Event_Window.png
|
|-- Reports/
|   `-- Report.docx
```


### How to replicate

Follow these steps to replicate the analysis.

#### Prerequisites

1.  **Install Packages**: Before you begin, run the `Master Replication Script.R`. This script will check for all the R packages needed for the project and automatically install any that are missing.
2.  **Connect to WRDS**: The analysis requires a connection to the Wharton Research Data Services database. Make sure you have valid credentials and have logged in before proceeding to the main scripts.


 The connection to wrds is require from Step three onwards. The "Main.Script.R" contain a code block to login to wrds as shown in the below image.Uncomment code block and use your own username if you choose not login to wrds before code execution.
 
<img width="793" height="301" alt="image" src="https://github.com/user-attachments/assets/bceccd62-222c-4189-846b-c2e67567fe51" />


#### Replication Steps
The R scripts in the `/Code/` folder should be run in numerical order as stated below.

1.  **Extract Raw Data**
    * Run `01_Data_Extract.R`. This script downloads the raw layoff data.
    * **Output:** Saves `warn_data_2015_2024.csv` to the `/Raw Data/` folder.

2.  **Clean Data**
    * Run `02_Data_Cleaning.R`. This filters the raw data for permanent layoff events.
    * **Output:** Saves `Processed-warn.csv` to the `/Processed Data/` folder.

3.  **Match, Link, and Calculate Variables**
    * Run `03_Main_Script.R`. This is the core data processing script. It matches layoff announcements to firms in Compustat, links them to CRSP, and calculates R&D metrics.
    * **Output:** Saves `layoff_events_upload.txt` to the `/Processed Data/Static Data/` folder.

4.  **Obtain Event Study Data (Choose One Option)**
    * **Option A (Use Existing Data):** The repository already includes the necessary event study output files in the `/Processed Data/Static Data/` folder. If you choose this option, you can proceed directly to the next step.
    * **Option B (Re-run the Event Study):**
        * Manually log in to the WRDS web portal and upload the `layoff_events_upload.txt` file to the Event Study tool.
        * Run the event study for each required window.
        * Download the results and **replace** the existing files in `/Processed Data/Static Data/`. You must use the exact same filenames (e.g., `1 day window.csv`, `3 day window.csv`, etc.) for the next script to work.

5.  **Calculate Cumulative Abnormal Returns (CARs)**
    * Run `04_Calculate_CAR.R`. This script reads the CSV files from the previous step and calculates the final CAR values.
    * **Output:** Saves `Final_CAR_File.csv` to the `/Processed Data/` folder.

6.  **Final Merge**
    * Run `06_Merge_Dataframes.R`. This script combines the R&D data with the CAR data.
    * **Output:** Saves the final, analysis-ready `Final_Cleaned_Dataset.csv` to the `/Output/` folder.

7.  **Generate Report Outputs**
    * Run `07_Final_Analysis.R`. This script uses the final clean dataset to generate every table and figure included in the report.
    * **Output:** All final `.png` files for tables and figures are saved in the `/Output/` folder.









