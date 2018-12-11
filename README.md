xMAPr app - hyper-dynamic-range (HDR) xMAP® data analysis
================

## Introduction

Measuring the antibody response to multiple antigens in biofluids can be
achieved using the xMAP® technology supplied by Luminex®. For the
multiplexing assay all antigens are covalently bound to small
paramagnetic particles (MagPlex™) of unique fluorescence dye combination
making it possible to distinguish between the mixed spheres in the full
assay.

The immune response to some antigens can be extremely strong and
extremely weak to others. Thus, the quantitative dynamic range of
analysis extends over several orders of magnitude. In a standard
measurement, the patient’s plasma/serum is diluted 1000-fold and / or
10000-fold. Since this commonly used approach carries the risk of
generating quantitative data close to the detection limit when including
multiple antigens with a wide variance in the expected antibody
responses. This results in error prone data sets making the
interpretation of the data very difficult. An advanced approach has been
developed to overcome this issue including a serial dilution based
measurement of samples granting results derived from the linear range of
each antigen.

![serological assay workflow using the xMAP®
technology](www/xMAPr_manual/figures/serological_assay_workflow.png)

## Download xMAPr from GitHub and run locally

### Requirements

  - R (min. version 3.5.1) <https://cran.r-project.org>
  - RStudio Desktop (min. version 1.1.414)
    <https://www.rstudio.com/products/rstudio-desktop/>
  - **Windows user MUST install RTools**
    <https://cran.r-project.org/bin/windows/Rtools/> on **C:** drive
    (final path: C:\\RTools)

other demanding R packages will be installed on the first startup of the
xMAPr app (this might take a while)

### Running the app locally

1.  download the app from github <https://github.com/stemicha/xMAPr_app>
    by using the **clone or download** button
2.  after extracting the zip file please open the **ui.R** or
    **server.R** in RStudio
3.  select **Run external**
4.  press **Run App** to start the app (first run will take a while, due
    to package
installations)

<img src="www/xMAPr_manual/figures/run_app_locally.png" style="display: block; margin: auto;" />
