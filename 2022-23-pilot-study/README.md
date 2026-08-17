<p align="left" style="margin-bottom: 30px;">
  <a href="https://reefrevival.org/" target="_blank">
    <img src="https://reefrevival.org/wp-content/uploads/2024/05/ReefRevival-Logo-1024x447.png"
         width="200">
  </a>
</p>

# Pilot Coral Nursery Study
## About this repositoy

Data, code, figures, tables, and supporting material for the first assessment of coral growth, survival, and condition in a pilot *Pocillopora* nursery in the Galápagos Islands.

This repository supports the study:

> Dávalos, N., Velasco-Cedeño, D., & Brandt, M. (2026). First assessment of the physiological performance of nursery corals in the Galápagos Islands: seasonal impacts on *Pocillopora* (Scleractinia: Pocilloporidae) growth. *Revista de Biología Tropical, 74*(S1), e202610168. https://doi.org/10.15517/0xznfj40

**Repository structure**

The repository can be organized as follows:

```text
galapagos-reef-revival/
├── README.md                         # General overview of Galápagos Reef Revival
│
└── 2022-23-pilot-study/
    ├── README.md                     # information and reproducibility for this paper
    │
    ├── Code/                         # Data processing and analysis scripts
    │
    ├── Data/
    │   ├── Raw/                      # Original datasets
    │   └── Processed/                # Cleaned and analysis-ready datasets
    │
    └── Results/                      # Figures and main and supplementary tables generated from the analyses

```

- A brief guide to the code used in this research can be found <a href="https://davelascoc.github.io/galapagos-reef-revival/2022-23-pilot-study/Code/Corals_Pilot.html">here</a>.

## Our study

Coral communities in the Galápagos Islands experience strong seasonal and interannual environmental variability. This study evaluated how seasonal thermal conditions influenced the performance of nursery-grown *Pocillopora* corals in Puerto Villamil Bay, Isabela Island, Galápagos.

The study combined:

- a baseline survey of coral communities in Puerto Villamil Bay;
- cultivation of 12 *Pocillopora* morphotypes in two rope nurseries;
- photographic measurements of coral fragment growth;
- monthly assessments of mortality and coral condition;
- high-frequency in situ seawater temperature records; and
- El Niño 1+2 sea-surface temperature anomalies from NOAA.

The pilot nursery was established in January 2022 with 191 coral fragments. Fragment size was evaluated in January 2022, June 2022, and January 2023, allowing growth to be compared between the warm (January-May 2022) and cold (June-December 2022) seasons. Coral condition monitoring included the percentage of dead tissue, bleached tissue, and algal cover.

**Main findings**

- *Pocillopora* represented 73.6% of the 3,569 hermatypic coral colonies recorded during the baseline survey.
- Nursery temperatures ranged from 18.71 to 30.15 °C during persistent La Niña conditions.
- Mean coral planar area increased from 6.40 ± 2.51 cm² in January 2022 to 27.75 ± 15.85 cm² in January 2023.
- Overall survival in the nursery was 80%.
- Relative and specific growth rates were higher during the warm season, whereas absolute planar-area growth was slightly higher during the cold season.
- The cold season was associated with higher mortality, tissue loss, and algal overgrowth, with substantial variation among morphotypes.
- These results indicate that coral gardening is feasible in Galápagos, but nursery design and restoration planning should explicitly account for seasonality and differences among donor colonies/morphotypes.

### Data

The repository is intended to contain the datasets used to reproduce the analyses reported in the paper.

| Dataset | Coverage | Description |
| --- | --- | --- |
| Coral baseline survey | Nov-Dec 2021 | Location, identity, size, and health condition of coral colonies surveyed in Puerto Villamil Bay |
| Nursery growth | Jan 2022-Jan 2023 | Fragment identity, morphotype, planar area, maximum length, and derived growth metrics |
| Coral condition | May-Dec 2022 | Mortality and estimates of dead tissue, bleaching, and algal cover |
| In situ temperature | Jan-Dec 2022 | HOBO TidbiT temperature records collected at the nursery site at 10-min intervals |
| ENSO conditions | 2022-early 2023 | Weekly El Niño 1+2 SST anomalies obtained from the NOAA Climate Prediction Center |

The coral-condition percentages are derived from categorical field scores following Goergen et al. (2020), using the upper limit of each percentage interval as described in the paper.

### Funding and acknowledgements

The baseline survey, pilot nursery, and monitoring activities were supported by a Rufford Small Grant and Fundación de Conservación Jocotoco funding to Nicolás Dávalos, and by COCIBA and Galápagos Grants from Universidad San Francisco de Quito USFQ to Margarita Brandt.

We acknowledge José L. Barrios Ponce and Cristopher Gómez, coral gardeners from the Galápagos Reef Revival team, for their contributions to the baseline survey, nursery assembly and maintenance, coral health monitoring, photographic sampling, and other field activities.

### External data source

Weekly El Niño 1+2 SST anomalies were obtained from the NOAA Climate Prediction Center:

https://www.cpc.ncep.noaa.gov/data/indices/wksst9120.for

### License & Citation

The code in this repository is licensed under the MIT License.
The data are licensed under the Creative Commons Attribution 4.0 International (CC BY 4.0) License.

If you use material from this repository, please cite the associated publication:

```text
Dávalos, N., Velasco-Cedeño, D., & Brandt, M. (2026). First assessment of the physiological performance of nursery corals in the Galápagos Islands: seasonal impacts on Pocillopora (Scleractinia: Pocilloporidae) growth. Revista de Biología Tropical, 74(S1), e202610168. https://doi.org/10.15517/0xznfj40
```

