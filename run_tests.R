# run tests

library('RUnit')

testsuite.S4_Intro <- defineTestSuite(name = "S4_Intro",
                                         dirs = file.path("tests"),
                                         testFileRegexp = "^runit.+\\.[rR]$",
                                         testFuncRegexp = "^test.+",
                                         rngKind = "Mersenne-Twister",
                                         rngNormalKind = "Inversion"
)


source('classTrajectories.R')
testResult <- runTestSuite(testsuite.S4_Intro)
printTextProtocol(testResult)
