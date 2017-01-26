# Are.na Spider
Crawl connected Are.na channels and visualize the resulting network.

<img src="https://rawgit.com/hxrts/spider/master/world_map.png" width="800px">

## Setup

Working prototype, but there are a couple setup steps for first time use:

- install [R](https://www.r-project.org/)
- clone the app repo
```> git clone git@github.com:hxrts/spider.git```
- cd into repo directory and run R
```> cd spider && R```
- from R, install the pacman package manager
```> install.packages('pacman')```
- install shiny
```> install.packages('shiny')```
- invoke shiny library
```> library(shiny)```
- import remaining code (will install remaining libraries as needed)
```> source('_daemon')```
- run app (will launch browser)
```> Spider()```

## Usage

Pick an origin channel and choose crawling parameters as desired. Clear the window after your search or add a new query to the existing graph.

Note: The scraping is rather slow, you can watch the progrss in the command prompt. Please be patient.

## Stop App

- ```ctrl + c``` returns to R prompt
- ```q()``` exits R prompt

## Run After Installation

- from spider directory run R
```> R```
- ```library(shiny)```
- ```source('_daemon')```
- ```Spider()```