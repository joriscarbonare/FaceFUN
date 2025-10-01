# **FaceFUN Repository**

## **Overview**  

The **FaceFUN** project investigates how plant-associated fungi influence vegetation dynamics and ecosystem processes under global change factors, with a specific focus on **phosphorus-limited dry forest-grassland systems**. Research is conducted at **EucFACE (Eucalyptus Free-Air CO₂ Enrichment)**, where vegetation shifts in response to **elevated CO₂ (eCO₂)**are examined through a multi-scale approach combining **field-based sampling, controlled growth chamber experiments, environmental DNA (eDNA) and RNA (eRNA) analyses, and ecosystem process modeling (MODEX)** to assess plant-soil-microbe interactions under elevated CO₂

This repository serves as a workspace for storing datasets, developing and refining analysis scripts, visualizing results through figures and graphs, and maintaining version history throughout the project.

---

## **Repository Structure**  

### 📂 **DATA**  

Each data subfolder follows a strict organization where raw data is stored in a `brut_data` directory. These raw data files must never be modified to ensure data integrity. Any data processing or modifications should be done through scripts that read a copy from `brut_data` and save results in appropriate output directories.

#### 📁 **plant_data/**
- **`EucFACE_vegsurvey_consolidated_data.csv`** – Vegetation survey dataset from EucFACE, providing species presence/absence data across rings, plots, quadrant and sampling cells.
- **`Photosynthetic_Pathways_of_Plants_TERN_v2_19092024.csv`** – Dataset from TERN (Terrestrial Ecosystem Research Network), containing species-level classifications of photosynthetic pathways (C3/C4) along with other ecological traits.
- **`TRY_Categorical_Traits_Lookup_Table_2012_03_17_TestRelease.xlsx`** – Extract from the TRY Plant Trait Database, providing a wide range of plant functional traits, including growth form (herb, graminoid, fern).
- **`veg_survey_lastdate.RData`** – Last survey only subset of the EucFACE vegetation survey dataset.

#### 📁 **PO4/**
##### 📁 **brut_data/**
Raw phosphorus measurement data files that must not be modified:
- **`JORIS-BRAYP-*.csv`** – Bray-P measurements for different sample sets
- **`PO4_sample_ID.xlsx`** – Sample identification and metadata

#### 📁 **NH4-NO3/**
##### 📁 **brut_data/**
Raw nitrogen measurement data that must not be modified:
- NH4 and NO3 measurements across different sampling lines (L1-L9)
- a coming file about sample ID association

#### 📁 **Respiration/**
##### 📁 **brut_data/**
Raw respiration measurement data that must not be modified:
- **`RES_FACEFUN_brut.xlsx`** – Raw respiration measurements
- **`wet_dry_repiration_facefun.xlsx`** – Wet/dry condition respiration data, and time of incubation

### 📂 **SCRIPTS**  
This folder originally held analysis scripts. Some example/early-analysis files (including a cell selection script and an example figure) were removed to keep the repository focused on core datasets. 

---

## **Project Members**  

- **Jason Hoeksema** – PI, University of Mississippi  
- **Nicole Hynson** – PI, University of Hawaiʻi at Mānoa  
- **Jennifer Bhatnagar** – PI, Boston University  
- **Edward Brzostek** – PI, West Virginia University  
- **Joris Carbonare** -PhD student, University of Mississippi 
- **Jensen Nipko** -PhD student, West Virginia University
- **Corinne Vietorisz** -PhD student, Boston University
- **Asha Thapa** -PhD student, Boston University

_(Additional collaborators to be completed)_

---
