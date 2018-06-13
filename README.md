# BalticBOOST
This is the BalticBOOST Biodiversity Assessment Tool (BEAT 3.0)<br>
The tool was developed under the HELCOM BalticBOOST project and was used to perform the integrated biodiversity assessment for HOLAS II - <b>The HELCOM State of the Baltic Sea - Holistic Assessment</b><br><br>
<a href="https://doi.org/10.5281/zenodo.1288315"><img src="https://zenodo.org/badge/DOI/10.5281/zenodo.1288315.svg" alt="DOI"></a>

BEAT 3.0 was coded as an R-script in order to provide a freely accessible and open tool. The script can be downloaded from: https://github.com/NIVA-Denmark/BalticBOOST.
  
The structure of the assessment is defined in input tables to the tool. The default spatial structure follows the HELCOM Monitoring and Assessment Strategy, whereas the ecosystem component structure follows the updated European Commission Decision<sup>[1](#references)</sup> on GES criteria. As a first step BEAT 3.0 reads the input tables, normalizes the indicators and assigns weight to them, as well as calculates the indicator confidence. The following step is, following the defined structures, integrating the assessment results to the different ecosystem component and spatial assessment unit levels. In the last step, the results are summarized and exported as output tables, separately for the biodiversity assessment and the confidence assessment.

## Input tables
Input tables to the tool are:
* Spatial assessment units ([SAU.txt](input/SAU.txt) file) – a hierarchical list of the assessment units with four levels (according to the HELCOM spatial assessment unit levels 1-4). The area (km2) of all spatial assessment units are specified here if applying the area-weighted spatial aggregation option.
* Ecosystem components ([EcosystemComponents.txt](input/SAU.txt) file) – a hierarchical list of the ecosystem components (birds, fish, mammals, pelagic habitats, benthic habitats) with four levels (1=Biodiversity, 2=Ecosystem component, 3=Species group/broad habitat type, 4=Species/habitat element). Each component is linked to the relevant higher level ecosystem component.
* Descriptors ([descriptors.txt](input/descriptors.txt) file) – a list of the MSFD descriptors
* Criteria ([criteria.txt](input/criteria.txt) file) – a list of the MSFD criteria. This list is updated to follow the revised European Commission Decision on GES criteria.
* Indicator catalogue ([IndicatorCatalogue.txt](input/IndicatorCatalogue.txt) file) – a list where the indicators are assigned to relevant ecosystem component and MSFD criterion.
* Indicators ([indicators.txt](input/indicators.txt) file) – table of observed value, minimum and maximum values, threshold value and confidence evaluations for the indicators linked to the spatial assessment unit. One row is added for each assessment unit the indicator is used in. Instructions on how to define minimum and maximum values for the indicators can be found in the request sent to the indicator Lead and co-Lead country representatives (HOLAS II 5-2016 Document 4-1 Annex 1<sup>[2](#references)</sup>).
* Indicator group ([ooao.txt](input/ooao.txt) file) – a list grouping indicators/parameters used in conditional indicators and indicators to be treated with the OOAO approach, i.e. using the parameter with poorest status classification in further integration steps.

Of these input tables, the spatial assessment units, ecosystem components, descriptors and criteria can be used as they are in the HELCOM and MSFD context, but if new indicators are added to the tool one needs to follow the steps and update the input files as outlined in the steps describe below.

## Steps to run BEAT 3.0
1) Download the R script from https://github.com/NIVA-Denmark/BalticBOOST. Input files are found in the …/input folder.
2) Add the indicator to the indicator catalogue. Make sure to link the indicator with the correct ecosystem component ID and MSFD criteria. The ecosystem component ID is found in the EcosystemComponents.txt file
3) Insert the indicator results to the Indicators file. Add one row for each spatial unit the indicator has results for. If assessment is to be carried out on lower spatial assessment level than the indicator is assessed at, the information needs to be downscaled. This is done by adding the (same) indicator result to all relevant spatial assessment units at that level.
4) Specify the spatial assessment unit (ID is found in the [SAU.txt](input/SAU.txt) file) and indicator ID (found in the [IndicatorCatalogue.txt](input/IndicatorCatalogue.txt) file).
5) Specify the indicator type (1: indicator value increasing/decreasing with improved/worsened environmental status, 2: indicator with an optimal range/interval).
6) Insert the minimum and maximum values of the indicator. Instructions on how to set the minimum and maximum values are found in HOLAS II 5-2016 Document 4-1 Annex 1<sup>[2](#references)</sup>. Make sure the minimum and maximum values are inserted correctly into the Bad and High columns, depending on if increasing value mean improved status (Minimum = Bad, Maximum =High) or decreasing value means improved status (Minimum = High, Maximum = Bad).
7) Insert the threshold value (ModGood column)
8) For type 2 indicators the optimal value is inserted in the High column. Minimum value is inserted in the Bad column, lower threshold value in the ModGood column, higher threshold value in ModGood2 column and maximum value in Bad2 column.
9) Insert the indicator result (obs column).
10) Insert the standard error of the indicator result (if available)
11) Define and insert the confidence scores (High = 1, Intermediate = 0.5, Low = 0) for each of the four categories: confidence of classification (ConfA), temporal coverage (ConfT), spatial representation (ConfS) and methodological confidence (ConfM). The confidence can be inserted in numerical or text format. Instructions on how to assess the confidence in the different categories are found in HOLAS 5-2016 Document 4-1 Annex 1. If standard error has been provided ConfA can be left empty.
12) If the indicator uses a conditional approach, i.e. several parameters with threshold values, all parameters and their results are inserted as separate indicators following the instructions above. The parameters are grouped in the [ooao.txt](input/ooao.txt) file, where the indicator ID’s of the parameters used in the indicator are given the same group ID.
13) Run the R script [BOOSTbiodiv.R](BOOSTbiodiv.R) (make sure to specify the work directory location of the [include.R](include.R) file).
14) Result files are found in the […/results](results) folder.

## References<a name="references"></a>
1) [Commission Decision (EU) 2017/848 of 17 May 2017](http://eur-lex.europa.eu/legal-content/EN/TXT/?uri=CELEX%3A32017D0848)
2) [Nygård H, Murray C, Torn K, Fleming-Lehtinen V, Martin G, Andersen J, Korpinen S (2017):  WP 1.1 Deliverable 1: Development of a biodiversity assessment tool, BalticBOOST Final report 14 February 2017, Appendix 1](https://portal.helcom.fi/meetings/HOLAS%20II%205-2016-347/MeetingDocuments/4-1%20Developement%20of%20a%20biodiversity%20assessment%20tool.pdf)

