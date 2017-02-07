# Are.na Spider
Crawl connected Are.na channels and visualize the resulting network.

<img src="https://rawgit.com/hxrts/spider/master/world_map.png" width="800px">

## Setup

Working prototype, but there are a couple setup steps for first time use:

- install [R](https://www.r-project.org/)
- clone the app repo and set permissions for run script
```> git clone git@github.com:hxrts/spider.git && chmod +x spider/run.sh```
- cd into repo directory and run R
```> cd spider && R```
- from R, install the pacman package manager
```> install.packages('pacman')```
- import remaining code (will install remaining libraries as needed)
```> source('_daemon')```
- run app (will launch browser)
```> Spider()```

## Usage

Pick an origin channel and choose crawling parameters as desired. Clear the window after your search or add a new query to the existing graph.

Note: The scraping is rather slow, you can watch the progrss in the command prompt. Please be patient.

## Stop App

- ```ctrl + c``` returns to R prompt
- ```q()``` exits R prompt if open

## Run After Installation

- [optional] specify local port in config.txt file
- from spider directory run startup script
```> ./run.sh```
- point your browser to the address displayed

## Note

- Add interesting results in the channel 'World Maps' with some information about the parameters used to generate the plot.