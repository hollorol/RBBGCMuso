context("Post processing")
library(testthat)
library(RBBGCMuso)
setwd(system.file("examples/hhs","",package = "RBBGCMuso"))

test_that("Post processing string",{
    testMatrix1 <- data.frame(first = rep(1,5), second = rep(2,5), third = rep(3,5))
    testMatrix1c <- testMatrix1 
    testMatrix1c[,"newCol"] <- testMatrix1c[,2] + 3 * testMatrix1c[,3] 
    expect_equal(postProcMuso(testMatrix1,"newCol <- @2 + 3*@3"),testMatrix1c)
})

test_that("calibMuso with postprocessing",{
    model <- calibMuso(skipSpinup = FALSE, silent = TRUE)
    modelc<- model
    newCol <- modelc[,1]
    modelc<- cbind.data.frame(modelc,newCol)
    modelc[,"newCol"]<- model[,5]+3*model[,7]
    expect_equal(calibMuso(skipSpinup = FALSE,silent = TRUE, postProcString = "newCol <- @5 + 3* @7"), modelc)
})

