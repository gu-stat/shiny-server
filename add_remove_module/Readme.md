This Shiny application uses modules to add and remove an indefinite (unknown) number of variables and their values in a reactive manner and saves the output into a dataframe.

We started with two variables, but the code can easily be changed so that we start with only one. To do that, delete everything related to *Call Module 2* (server side), and *Module UI for Variable 2* (ui side), and change the value of `btn` from `reactiveValues(value = 2)` to `reactiveValues(value = 1)`.

This app was based on [this response on StackExchange](https://stackoverflow.com/a/46531264/6513180), and [this response on Shiny's github repo](https://github.com/rstudio/shiny/issues/2092). 