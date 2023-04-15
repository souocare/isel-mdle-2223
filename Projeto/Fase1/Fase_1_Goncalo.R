library(smotefamily)

data_example = sample_generator(5000,ratio = 0.80)
genData = smotefamily::BLSMOTE(data_example[,-3],data_example[,3])
genData_2 = smotefamily::BLSMOTE(data_example[,-3],data_example[,3],K=7, C=5, method = "type2")
