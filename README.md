# Variogram

MoveApps

GitHub repository: https://github.com/movestore/ctmm_variogram

## Description
Calculates and plots variograms of movement tracks to visualize
autocorrelation structure before fitting a continuous-time movement model
(CTMM).

## Documentation
A variogram visualizes the autocorrelation structure present in a movement
track. It shows the semi-variance between pairs of locations as a function
of the time lag between them. That is, if locations separated by a given time
lag tend to be close together, the semi-variance at that lag is low, and
vice versa.

Generating and investigating a variogram is an initial step when fitting
a continuous-time movement model, as it helps identify whether
tracking data exhibit range resident behavior as well as plausible model
structures to use when fitting a CTMM to the data.

The App uses `ctmm::variogram()` to calculate one variogram per selected
animal and displays them in a grid. It can also be toggled to show a single
population variogram, which averages the variograms of all the selected animals.

The **Zoom** slider allows you to zoom in or out to view the autocorrelation
structure at shorter or longer time lags. By default, the first half of the 
variogram is shown; data beyond these lags are often noisy and of limited utility.

See the [ctmm vignette on variograms][1] for more information on interpreting
the variograms.

[1]: https://cran.r-project.org/web/packages/ctmm/vignettes/variogram.html

### Application scope
#### Generality of App usability
This App was developed for any taxonomic group.

#### Required data properties
The App should work for any location data in ctmm format.

Variograms are easiest to interpret when computed from regularly sampled data.
Variograms for irregularly sampled data may appear noisy. Similarly, for the
population variogram, animals should have similar sampling schedules and
movement behavior.

### Input type
`ctmm::telemetry.list`

### Output type
`ctmm::telemetry.list`

### Artefacts
None

### Settings
**Zoom**: The range of time lags to show on the x-axis of the variogram
plots. Moving the slider to the left shows shorter time lags and vice versa.
The zoom slider is on a logarithmic scale. By default, half of the longest
time lag in the data is shown, as longer lags can be highly noisy and of limited
utility. The **Reset to default** button returns the zoom slider to this value.

**Max columns**: The maximum number of columns to use in the grid of plots. The
grid uses fewer columns if fewer animals are selected, or if the window
is too narrow for the panels to stay readable. Defaults to 8 columns.

**Same axes for all animals**: If selected, all panels use the same x- and
y-axis. Only applies when **View** is set to "Per individual".

**View**: "Per individual" shows one variogram per animal. "Population variogram"
shows a single variogram averaged over the selected animals. Defaults to
"Per individual".

**Animals**: The animals to include in the variogram plot grid. By default,
all animals are shown. Note that all animals' data are passed to the next
App, regardless of whether they are selected for visualization or not.

**Store settings**: Click to store the current settings of the App for future
Workflow runs.

**Restore default settings**: Click to restore the App's default settings.

### Changes in output data
The input data remain unchanged.

### Errors and null handling
**Animals selection:** If no animals are selected, no plot is shown.

**Performance:** The variograms are calculated each time the App's window is
opened and each time the selected animals or the **View** setting change. For
large data sets (long tracks with frequent locations) this can take a while.
Drawing the plots also takes longer the more time lags are shown, so the plots
redraw faster when zoomed in on shorter time lags.

### Technical details
Variograms are calculated with `ctmm::variogram()` separately for each animal.
The function's default settings are used. This means that time lags are binned
by each animal's median sampling interval and that location error is
not accounted for.

Plots are drawn with ctmm's `plot()` method for variograms.

When **View** is set to "Population variogram", the displayed variogram is
the number-weighted average of all of the selected animals' variograms,
calculated with ctmm's `mean()` method for variograms.

The variogram method is described in Fleming, C. H., Calabrese, J. M.,
Mueller, T., Olson, K. A., Leimgruber, P., & Fagan, W. F. (2014). From
fine-scale foraging to home ranges: a semivariance approach to identifying
movement modes across spatiotemporal scales. *The American Naturalist*,
183(5), E154–E167.
