# Coproduction of a mixed-method framework for the deprioritization of communities during insecticide-treated bed net mass campaigns in Kwara Nigeria  <a href="https://zenodo.org/doi/10.5281/zenodo.10854716"><img src="https://zenodo.org/badge/718266745.svg" alt="DOI"></a>
## Overview
This repository contains the code and documentation for the algorithm used to guide the targeted de-prioritization of insecticide-treated nets (ITNs) in urban areas of Ilorin, Nigeria. This initiative was a collaboration between the Loyola University Chicago, the University of Ibadan, the World Health Organization and Nigeria's National Malaria Elimination Programme (NMEP). Due to funding limitations, the NMEP aimed to identify the least vulnerable communities for malaria and exclude them during the 2023 Bednet mass campign.

## Background
A mixed-methods approach was employed to identify communities in Ilorin where ITN distribution could be scaled back. This approach integrates quantitative analysis with local insights and field evaluations, ensuring a strategic framework for ITN de-prioritization and a judicious allocation of interventions.

## Repository Structure
- code for the secondary data analysis used to score and rank wards by malaria risk are numbered beginning from 00 - 02.
- code for visualizing the themes from the multistakeholder dialogue is in the script beginning in 03. 
- 04 contains code for generating maps used in the Multistakeholder Dialogue (MSD).
- `/datafiles`: themes from the MSD sessions and all aggregate data used in the scoring models.
- `/Shiny_app`: code for the shiny app developed by Laurette Mhlanga, Postdoctoral Fellow at the Urban Malaria Project Team to support the NMEP in developing new deprioritization frameworks for other Nigerian cities.

## Methodology
1. **Secondary Data Analysis**: The algorithm starts with an analysis of secondary data to score and rank wards according to their malaria risk.
2. **Multistakeholder Dialogue (MSD)**: The MSD validates the algorithmic findings and collaboratively identifies potential wards for ITN de-prioritization.
3. **Field Assessment**: A comprehensive field assessment in selected wards, focusing on settlement type heterogeneities to inform final decisions.

## Key Findings
The MSD corroborated the algorithmic findings, highlighting Are 2 and Akanbi 4 as potential wards for ITN de-prioritization.
Field visits to 188 settlements across both wards led to the decision to focus on Are 2. Out of 73 settlements assessed here, thirteen were earmarked for ITN scale-back.

## Impact
This mixed-methods approach provides a strategic framework for optimizing ITN de-prioritization in urban settings. It sets a precedent for addressing complex health issues through collaborative, evidence-based strategies.

## Contributing
Contributions to this repository are welcome. Please read our `CONTRIBUTING.md` file for guidelines on how to contribute.

## License
This project is licensed under the `MIT License`. See the `LICENSE` file for more details.

## Contact
For any inquiries or contributions, please contact: laurette@aims.ac.tz

## Acknowledgments
Special thanks to the WHO, NMEP, and all stakeholders involved in the Multistakeholder-Dialogue and field assessments.
