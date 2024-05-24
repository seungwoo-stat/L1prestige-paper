# L1prestige-paper

This repository contains code to reproduce figures and analysis from the paper "*L*<sub>1</sub> Prominence Measures for Directed Graphs" (Kang and Oh, 2024)

- `seoul-weekday-code` reproduces Figure 1(a), Figure 2, and analysis procedures outlined in Section 4.1 of the paper.

	- `seoul-move-weekday.rds` is an `igraph` object used in Section 4.1. It contains the weekday moring flow network of Seoul in December, 2023.

- `seoul-weekend-code` reproduces Figure 1(b), Figure 3 and analysis procedures outlined in Section 4.2 of the paper.

	- `seoul-move-weekend.rds` is an `igraph` object used in Section 4.2. It contains the weekend midday flow network of Seoul in December, 2023.

	- `seoulmap.rds` is a `SpatialPolygonDataFrame` object used for plotting the map of the Seoul city in Section 4.2.


### Reference

-   Seungwoo Kang and Hee-Seok Oh. (2024) *L*<sub>1</sub> Prominence Measures for Directed Graphs. Manuscript.
