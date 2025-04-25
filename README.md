This repository contains the data and code for our paper:

> Magneville et al, (202.). *…*. … link to the paper once it’s out

Our pre-print is online here: (soon)

### How to cite

Please cite this compendium as:

> Magneville et al, (2025). *Compendium of R code and data for Past
> Climate Stability Shapes Diversity*. Accessed … Online at
> <https://doi.org/xxx/xxx>

#### Long-Term Climate Stability Shapes FD and PD In Euro-Mediterranean Forests

The goal of this R project is to reproduce all analyses, figures, and
tables of drivers of FD and PD of Mediterranean forests (multidiversity
and muti taxon approach)🌳 Past climate stability, Present habitat
characteristics, Disturbances and Past and Present human impact.

## Content

The directory contains:

-   [:file\_folder: R](/R): ⚙ Folder containing all functions created
    for the analyses. These functions can be loaded using
    **devtools::load\_all()**. They are called in the scripts coded in
    the analysis folder.

-   [:file\_folder: analysis](/analysis): 🍳 Folder containing all
    script to prepare data and analyse it. It uses the functions coded
    in the R folder.

-   [:file\_folder: integradiv\_db](/integradiv_db): 📦 Folder
    containing the data from the INTEGRADIV database. It more
    specifically contains:

    -   traits data with all traits gathered in the INTEGRADIV database
        (only a subset were used in this paper - cf first analyses)
        *version from September 2024*

    -   phylogenies (some of them being pruned in this paper - cf first
        analyses) *version from September 2024*

    -   occurrences in 50\*50km grid cells *version from September 2024*

    -   spatial grid of the studied area

    -   land mask of the studied area

-   [:file\_folder: transformed\_data](/transformed_data): Folder
    containing all intermediate dataframe/list/vector created during the
    analyses, and used to get final outputs. Data has been transformed
    from the raw data (INTEGRADIV database).

-   [:file\_folder: outputs](/outputs): Folder containing all the
    outputs of the project, created in the analyses scripts. It also
    includes specific folders [:file\_folder:
    residual\_maps](/outputs/residual_maps) which contains maps the
    residuals from each random forest model, to visualize whether the
    model was better at predicting specific areas or not ;
    [:file\_folder: drivers\_maps](/outputs/drivers_maps) which contains
    maps of specific individual drivers ; [:file\_folder:
    directionality](/outputs/directionality) which contains plots
    (biplots and mosaic plots) representing the directionality of the
    effect of individual drivers on each dimension of diversity and for
    each facet. Only the individual drivers having a relative importance
    &gt; 75% were studied here.

-   make.R File: Script to run the whole project. Be careful, some steps
    might be time consuming, and have been run on a server.
