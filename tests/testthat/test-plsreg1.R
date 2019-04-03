context("test-plsreg1")

test_that("plsreg1 works", {
    data(vehicles)

    # apply plsreg1 extracting 2 components (no cross-validation)
    expect_silent(pls1_one <- plsreg1(vehicles[,1:12], vehicles[,13,drop=FALSE], comps=2, crosval=FALSE))
    # apply plsreg1 with selection of components by cross-validation
    expect_silent(pls1_two <- plsreg1(vehicles[,1:12], vehicles[,13,drop=FALSE], comps=NULL, crosval=TRUE))

    expect_equal(colnames(pls1_two$Q2)[1], "RMSE")
    expect_equal(ncol(pls1_two$Q2)[1],6)
})
