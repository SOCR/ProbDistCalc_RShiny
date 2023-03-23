# ProbDistCalc_RShiny

**SOCR Interactive (RShiny) Probability Distribution Calculators**

<a href="http://www.distributome.org/V3/calc/index.html"><img align="middle" src="https://raw.githubusercontent.com/SOCR/ProbDistCalc_RShiny/master/images/SOCR_ProbDistCalc_Figure.png"></a>

Table of contents
=================

<!--ts-->
   * [Table of contents](#table-of-contents)
   * [Overview](#overview)
   * [Current Deployment](#current-deployment)
   * [Code](#code)
   * [Team](#team)
   * [Acknowledgments](#acknowledgments)
   * [References](#references)
<!--te-->


Overview
========

The [SOCR RShiny probability distribution calculators](https://shiny.med.umich.edu/apps/dinov/SOCR_DistribCalc_RShiny_App/) provide interactive vizualizations of probability densities, mass functions, and cumulative distributions, e.g., [bivariate normal distribution](https://socr.umich.edu/HTML5/BivariateNormal/).

Current Deployment
========

The most current version is V0.7, deployed at https://shiny.med.umich.edu/apps/dinov/SOCR_DistribCalc_RShiny_App/. The release can be find [here](https://github.com/SOCR/ProbDistCalc_RShiny/releases/tag/Ver0.7).

Code
====

The pure HTML5/JavaScript code is available in the [code folder](https://github.com/SOCR/ProbDistCalc_RShiny/tree/master/code). This is implemented as an RShiny app. R version 3.4.3 or above is required for this application.

Team
====

[SOCR Team](http://www.socr.umich.edu/people/) including [Ivo D. Dinov](http://umich.edu/~dinov), Jared Chai, and others.

Acknowledgments
===============

This work is supported in part by NIH grants [P20 NR015331](www.socr.umich.edu/CSCD), [UL1TR002240](https://projectreporter.nih.gov/project_info_description.cfm?aid=9491961&icde=39078316), [P30 DK089503](http://mmoc.med.umich.edu/), [UL1TR002240](https://www.michr.umich.edu), and NSF grants [1916425](http://midas.umich.edu/), [1734853](http://brain-life.org/), [1636840](http://neurosciencenetwork.org/), [1416953](http://distributome.org), [0716055](http://socr.umich.edu) and [1023115](http://distributome.org). Students, trainees, scholars, and researchers from SOCR, BDDS, MIDAS, MICHR, and the broad R-statistical computing community have contributed ideas, code, and support.

References
==========

* [Probability Distributome Calculators](http://www.distributome.org/V3/calc/index.html), the [Distributome Navigator](http://distributome.org/V3/), [XML distribution metadata](http://www.distributome.org/js/Distributome.xml), and [XML DB validator](http://www.distributome.org/V3/Distributome.xml.html).
* [Deployed RShiny Webapp](https://shiny.med.umich.edu/apps/dinov/SOCR_DistribCalc_RShiny_App/).
* [Bivariate Normal Distribution Calculator](https://github.com/SOCR/SOCR_Bivariate_Distributions) and the [SOCR Bivariate Normal Distribution Activity](http://wiki.stat.ucla.edu/socr/index.php/SOCR_BivariateNormal_JS_Activity).
* [SOCR Distributions](http://www.socr.ucla.edu/htmls/dist/) and [distribution activities](http://wiki.socr.umich.edu/index.php/SOCR_EduMaterials_DistributionsActivities).
# TODO

- Change `distributions` from a list to a hashmap.
  > Downsides of using a list: It would be disastrous to decide to add or remove one distribution later, since the index is used everywhere to identify distribution and we also want to keep the lexicographic order. Also, it’s very hard to find the index based on the name of some distribution.
- Refactor `plotly` functions into separate files.
- Implement all missing distributions.
- Make the slider self-adjust to the graph range.
