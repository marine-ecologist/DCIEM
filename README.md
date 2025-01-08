# DCIEM

<img src="https://marine-ecologist.github.io/DCIEM/images/dciemhex.png" style="float: right;" width="181"/>

`DCIEM` is a "development" package that functions as a diveplanner and dive table calculator using the [DCIEM diving algorithm](https://www.divegearexpress.com/amfile/file/download/file/143/product/8927/). The package composes of a series of functions to calculate air decompression bottom times, surface intervals, and repetitive diving, and includes adjusted calculations for Nitrox up to EANx 40. The diveplanner allows for dynamic calculations of repetitive groups based on datetime of dives and acts as a multi-day logbook.

The `DCIEM` algorithm is used under the Australian / New Zealand Diving Standards for occupational diving. In the past no dive computers operated the DCIEM algorithm, and calculations were performed by manual tables, which are prone to mistakes when calculated in the field. [Shearwater](<https://www.shearwater.com/support/dciem/> have in the past year implemented the DCIEM algorithm on their dive computers. An advantage of using a computer over tables is that the they constantly track the diver's tissue compartments even between dives, but they do not produce [repetitive groups or repetitive factors](https://www.shearwater.com/products/dciem/#:~:text=Shearwater%27s%20DCIEM%20implementation%20does%20not,tissue%20compartments%20even%20between%20dives), which makes them limited in use for current commercial dive standards.

The `DCIEM` package was written to fill this gap and make dive planning more straight forward with less mistakes calculating tables on the fly. The Shiny app will be expanded at a future version to allow saving and multi-diver dive logs for use in field situations, and a stand alone app developed for use on phones/tablets to accurately plan and log dives.

![](https://raw.githubusercontent.com/marine-ecologist/DCIEM/refs/heads/main/rmarkdown/images/app.png)

### Installation

`DCIEM` can be installed as a package from `GitHub` via `remotes`:

```{r eval=FALSE, include=TRUE, class.source = "fold-show"}

install.packages("remotes")
remotes::install_github("marine-ecologist/dciem")


```

To run the app direct without installing the package, use `runGitHub`:

```{r eval=FALSE, include=TRUE, class.source = "fold-show"}

runGitHub("dciem", "marine-ecologist")

```

### Warning

The package and functions are currently in development and need further testing.

Do NOT rely on this package or any automated calculator for planning dives. Please feel free to email corrections and improved/suggestions to

### Contact:

George Roff ([george.roff\@csiro.au](%22mailto:george.roff@csiro.au%22))
