# transboundary-data

March 19, 2024

## Overview

This repo contains raw data, code, and output datasets used for population indicators for the Transboundary Region in the [Pacific Salmon Explorer](https://www.salmonexplorer.ca). 

The general workflow involves manual extraction of relevant data from appendices of the Pacific Salmon Commission [Transboundary Technical Committee (TTC) Reports](https://www.psc.org/publications/technical-reports/technical-committee-reports/transboundary/) into raw data files, combining and cross-checking these data against spawner surveys in DFO's [New Salmon Escapement Database (NuSEDS)](https://open.canada.ca/data/en/dataset/c48669a3-045b-400d-b730-48aafe8c5ee6), assigning spawner surveys and TTC records to Conservation Units (CUs), and outputting datasets by CU for visualization in the Pacific Salmon Explorer.

## Files

* `data/0_lookup-files` contains decoders that dictate which NuSEDS `POP_ID` and/or TTC Appendix fields are used to population spawner surveys (`TBR_PSF_CU_Site_LookupFile.csv`) and CU-level spawner abundances (`TBR_PSF_CU_Abundance_LookupFile.csv`) shown in the PSE.

* `data/1_raw-data` contains raw files including the NuSEDS query for Transboundary Region (large; gitignored but available on request and the associated NuSEDS SQL query is in `NuSEDS_query.txt`), TTC extract tables by watershed and species, and time series from Stonehouse Creek and the Nahlin River Sonar.

* `data/2_clean-data` contains cleaned datasets for NuSEDS and TTC that address known errors or issued flagged by PSF's Transboundary Technical Working Group.

* `code` contains R scripts that read in raw data, apply any corrections, use lookup files to assign surveys to CUs and output datasets in a format that is consistent with the population indicators in the PSE.

* `output` contains the output datasets for integration into SWP datasets and the PSE. These files are formatted somewhat consistent with the outputs in the SWP [Data Library](https://data.salmonwatersheds.ca/).

## Acknowledgements

This work was funded by the BC Salmon Restoration and Innovation Fund. Initial data extraction and code base was developed by Gottfried Pestal (SOLV Consulting) as a contract to the Pacific Salmon Foundation. We are grateful to Vesta Mather and members of our Transboundary Technical Working Group for input and advise on Transboundary datasets and to Aaron Foos (DFO) for providing the TTC Appendices in table format.

## More information

Contact: Steph Peacock (speacock at psf dot ca)

