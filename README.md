# Are.na Spider
Crawl connected Are.na channels and visualize the resulting network.

<img src="https://rawgit.com/hxrts/spider/master/world_map.png" width="800px">

## Setup

Working prototype, but there are a couple setup steps for first time use:

- install [R](https://www.r-project.org/)
- clone the app repo
- cd into repo directory and run ```> R```
- from R, install the pacman package manager ```> install.packages('pacman')```
- install shiny ```> install.packages('shiny')```
- invoke shiny library ```> library(shiny)```
- import remaining code ```> source('_daemon')```
- run app (should launch browser) ```> Spider()```