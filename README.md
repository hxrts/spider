# Are.na Spider
Crawl connected Are.na channels and visualize the resulting network.

<img src="https://rawgit.com/hxrts/spider/master/world_map.png" width="900px">

## Setup

Working prototype, but there are a couple setup steps for first time use:

- install [R](https://www.r-project.org/)
- clone the repo, move into app directory, set permissions for run script, and start R environment

```> git clone git@github.com:hxrts/spider.git && cd spider && chmod +x run.sh && R```

- from R, install the pacman package manager and import remaining code to install libraries as needed, exit upon completion

```> install.packages('pacman') ; source('_daemon.R') ; q(save = 'no')```


## Run

- [optional] specify local port in config.txt file
- from spider directory run startup script ```> sh run.sh```
- point your browser to the address displayed

## Usage

Pick an origin channel and choose crawling parameters as desired. Clear the window after your search or add a new query to the existing graph.

Note: The scraping is rather slow, especially graphs of high degree, you can watch crawling progrss in the command prompt. Please be patient.


## Stop

- ```ctrl + c``` halts app and returns command prompt

## World Maps

Add interesting results to the channel [World Maps](https://www.are.na/sam-hart/world-maps) with some information about the parameters used to generate the plot.

## Known Bugs

I've run into issues building graphs with overlapping edges without clearing the previous build, especially working from the initial graph. If the app encounters an error the screen will becomed greyed out, refreshing the window should re-initialize.