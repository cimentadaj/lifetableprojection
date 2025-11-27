
<!-- README.md is generated from README.Rmd. Please edit that file -->

# lifetableprojection

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
<!-- badges: end -->

This package aims to create a shiny webapp to make lifetable projections
available to users which are not familiar with R. The web application
allows to upload data, validate it and conduct lifetable projections.

## Installation

You can install the development version of lifetableprojection from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("cimentadaj/lifetableprojection")
```

## How to Launch the App

### Option 1: With R Installed

``` r
library(lifetableprojection)
run_app()
```

### Option 2: With Docker (No R Required)

Docker runs the app in a container - perfect if you don’t have R
installed.

#### Step 1: Install Docker Desktop

| Platform    | Instructions                                                                                                                   |
| ----------- | ------------------------------------------------------------------------------------------------------------------------------ |
| **Windows** | Download [Docker Desktop](https://www.docker.com/products/docker-desktop). Enable **WSL 2** when prompted during installation. |
| **Mac**     | Download [Docker Desktop](https://www.docker.com/products/docker-desktop) and follow the installer.                            |

#### Step 2: Download the Dockerfile

Download the
[Dockerfile](https://raw.githubusercontent.com/cimentadaj/lifetableprojection/main/Dockerfile)
and save it to a folder (e.g., your Desktop).

#### Step 3: Build and Run

Open a terminal in the folder where you saved the Dockerfile:

  - **Windows**: Right-click the folder → “Open in Terminal”
  - **Mac**: Right-click the folder → “New Terminal at Folder”

Then copy-paste these commands:

``` bash
docker build -t lifetable-app .
docker run --rm -p 8180:8180 lifetable-app
```

#### Step 4: Open the App

Go to **<http://localhost:8180>** in your browser.

#### Step 5: Stop the App

Press `Ctrl+C` in the terminal window.
