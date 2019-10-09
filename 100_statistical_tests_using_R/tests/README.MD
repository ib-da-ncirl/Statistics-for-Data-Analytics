# Unit testing
## Running tests
The following is the procedure to run unit tests for this project.

* Install the [testthat](https://cran.r-project.org/web/packages/testthat/index.html) library; use `install.packages("testthat")` from the R console, or via the `Tools->Install Packages` option in RStudio
* All the test files are stored in the `tests` folder
* To run an individual unit test from the R console; use `testthat::test_file('\<path to test file>')`

    For example, to run the tests for `utils.R` from the project root, use:
    `testthat::test_file('./tests/test_utils.R')`

* To run all the unit tests in the `tests` folder, from the R console, use:

    `testthat::test_dir('tests')`
 
## List of tests
| Script  | Test script | Description |
| ------- | ----------- | ----------- |
| 001_z-test_for_population_mean_variance_known.R  | test_001_z-test.R  | z-test for population mean where variance known  |
| 003_z-test_for_two_population_mean_variance_known.R  | test_003_z-test.R  | z-test for population mean where variance known  |
| 007_t-test_for_population_mean_variance_unknown.R  | test_007_t-test.R  | z-test for two population means (variances known and unequal)  |
| z_utils.R | test_z_utils.R  | z-value related functions  |
| t_utils.R | test_t_utils.R  | t-value related functions  |
| utils.R   |                 | Various utility functions  |
