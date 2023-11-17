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
- [x]  The title of app should indicate it can be used as either probability calculator or modeler.
- [ ]  There should be histogram indicating the original dataset along with the distribution curve plot.
- [x]  Use tabs to switch between calculator and modeler
- [x]  Should somehow allow user to upload their own datasets besides using the predefined datasets.
- [x]  Should add labels to the plot, indicating the meaning of x and y axis.
- [x]  Should display current parameters (like $\mu$ and $\sigma$) along with the plot.
- [x]  Should round parameters.
- [x]  User should be able to hide/display the distribution information (currently always shown in the left-bottom corner).
- [x]  Extend modeling functionality to other distributions (currently only supporting Normal Distribution).
- [x]  Newline between "Fitted mean: “ and "Fitted standard deviation: “
- [x]  better UI design.
- [ ]  Center the graph at mean automatically
- [ ]  BUG: probability density of Normal Distribution looks wrong, especially at mean.
- [ ]  Deploy to SOCR server
