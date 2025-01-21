# Co-creation and application of a framework for the de-prioritization of communities during insecticide-treated bed net mass campaigns for malaria prevention and control in Kwara, Nigeria  <a href="https://zenodo.org/doi/10.5281/zenodo.14714372"><img src="https://zenodo.org/badge/718266745.svg" alt="DOI"></a>
## Overview
This repository contains the code and documentation for the algorithm used to guide the targeted de-prioritization of insecticide-treated nets (ITNs) in urban areas of Ilorin, Nigeria. This initiative was a collaboration between the Loyola University Chicago, the University of Ibadan, the World Health Organization (WHO) and Nigeria's National Malaria Elimination Programme (NMEP). Due to funding limitations, the NMEP aimed to identify the least vulnerable communities for malaria and exclude them during the 2023 Bednet mass campign.

## Background
A mixed-methods approach was employed to identify communities in Ilorin that would be excluded. This approach integrates quantitative analysis with local insights and field evaluations, ensuring a strategic framework for ITN de-prioritization and a judicious allocation of interventions.

## Repository Structure
- `/scripts`: contains code for the secondary data analysis used to score and rank wards by malaria risk are numbered beginning from 00 - 02. Code for visualizing the themes from the multistakeholder dialogue is in the script beginning in 03. 04 contains code for generating maps used in the Multistakeholder Dialogue (MSD).
- `/datafiles`: themes from the MSD sessions and all aggregate data used in the scoring models.
- `/Shiny_app`: code for the shiny app developed by Laurette Mhlanga, Postdoctoral Fellow at the Urban Malaria Project Team to support the NMEP in developing new deprioritization frameworks for other Nigerian cities.

## Methodology
1. **Rank wards by level of malaria risk**: The algorithm starts with an analysis of secondary data to score and rank wards according to their malaria risk.
2. **Validate and codesign with the community**: The MSD validates the findings from #1 and collaboratively identifies potential wards for ITN de-prioritization.
3. **Classify communities and select deprioritized areas**: A comprehensive field assessment in selected wards, focusing on classifying communities into formal settlements, informal settlements and slums to inform selection of those to excluded during the campaign.

## Key Findings
The MSD corroborated the algorithmic findings, highlighting Are 2 and Akanbi 4 as potential wards for ITN de-prioritization.
Field visits to 188 settlements across both wards led to the decision to focus on Are 2. Out of 73 settlements assessed here, thirteen were earmarked for ITN scale-back.

## Impact
This mixed-methods approach provides a strategic framework for optimizing ITN de-prioritization in urban settings. It sets a precedent for addressing complex health issues through collaborative, evidence-based approaches.

## Contact
- For any inquiries or contributions, please contact: Ifeoma Ozodiegwu, Assistant Professor @ Loyola University and Principal Investigator at the Urban Malaria Project via iozodiegwu@luc.edu  
- Specific inquires regarding the Shiny App should be addresssed to Laurette Mhlanga, Postdoctoral Fellow @ the Urban Malaria Project via lmhlanga@luc.edu

## Acknowledgments
Special thanks to the WHO, NMEP, the Kwara State Malaria Eliminiation Program and Ministry of Health, and all stakeholders involved in the Multistakeholder-Dialogue and field assessments.
