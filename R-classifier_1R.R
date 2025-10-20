#install.packages("ISLR")
library(ISLR)

quantilize <- function(this_col){
  col_num = which(colnames(auto_data) == this_col)
  my_qu = quantile(auto_data[, col_num])
  
  auto_data[, col_num][auto_data[, col_num] < my_qu[2]] = 1 #25%
  auto_data[, col_num][auto_data[, col_num] >= my_qu[2] & auto_data[, col_num] < my_qu[3]] = 2 #50%
  auto_data[, col_num][auto_data[, col_num] >= my_qu[3] & auto_data[, col_num] < my_qu[4]] = 3 #75%
  auto_data[, col_num][auto_data[, col_num] >= my_qu[4]] = 4 #100%
  
  return(auto_data[, col_num])
}


do_loops = 1 #--> how many times the classification is to be repeated
train_extent = 0.7 #--> define percentage of train data

auto_data = NULL
auto_data_train = NULL
auto_data_test = NULL
success_rate_mean = seq(0,0, length.out = do_loops)

itz = 0
while (itz <= do_loops) {
  
  
  #--------------------#
  #--- 1. read data ---#
  #--------------------#
  
  require(ISLR)
  data("Auto")
  auto_data = Auto
  
  
  #-----------------------#
  #--- 2. prepare data ---#
  #-----------------------#
  
  auto_data = auto_data[auto_data$origin != 2,] #-- remove lowest quantity (2 = Europe) to make it binomial (US/Japan)
  
  #--> prepare result_df which will contain the error rate of each column
  md_names = head(names(auto_data), -2) #--> remove origin and names
  result_df = as.data.frame(cbind(Attribute = md_names, Gesamtfehler = NA, Fehlerquote = NA), stringsAsFactors = FALSE)
  prognos_list = list() #--> will be filled within the loop
  
  #--> replace values by its quantiles
  auto_data$mpg = quantilize("mpg")
  auto_data$displacement = quantilize("displacement")
  auto_data$horsepower = quantilize("horsepower")
  auto_data$weight = quantilize("weight")
  auto_data$acceleration = quantilize("acceleration")
  
  
  #-----------------------#
  #--- 3. balance data ---#
  #-----------------------#
  
  l_01 = length(auto_data$origin[auto_data$origin == 1]) #--> US
  l_02 = length(auto_data$origin[auto_data$origin == 3]) #--> non-US (Japan)
  
  #----- downsampling -----#
  if(l_01 != l_02){
    if(l_01 > l_02){
      l_cut = sample(1:l_01, l_01 - l_02, replace = FALSE)
      auto_data_01 = auto_data[auto_data$origin == 1,]
      auto_data_01 = auto_data_01[-l_cut,]
      auto_data_02 = auto_data[auto_data$origin == 3,]
    }else if(l_01 < l_02){
      l_cut = sample(1:l_02, l_02 - l_01, replace = FALSE)
      auto_data_01 = auto_data[auto_data$origin == 1,]
      auto_data_02 = auto_data[auto_data$origin == 3,]
      auto_data_02 = auto_data_02[-l_cut,]
    }
  }
  
  
  #----------------------------------------#
  #--- 4. divide data in train and test ---#
  #----------------------------------------#
  
  l_01 = length(auto_data_01$origin[auto_data_01$origin == 1])
  l_02 = length(auto_data_02$origin[auto_data_02$origin == 3])
  
  #--------------------------------------------------
  if(l_01 == l_02){
    n_train_cut = round(l_01 * train_extent) #--> extent of train data
    n_train_cut = sample(1:l_01, n_train_cut, replace = FALSE)
    
    auto_data_01_train = auto_data_01[n_train_cut,]
    auto_data_01_test = auto_data_01[-n_train_cut,]
    
    auto_data_02_train = auto_data_02[n_train_cut,]
    auto_data_02_test = auto_data_02[-n_train_cut,]
    
    #--> combine dataframes
    auto_data_train = rbind(auto_data_01_train, auto_data_02_train)
    auto_data_test = rbind(auto_data_01_test, auto_data_02_test)
    
    #--> clean up environment
    rm(auto_data_01, auto_data_01_train, auto_data_01_test, auto_data_02, auto_data_02_train, auto_data_02_test)
    rm(l_01, l_02, l_cut, n_train_cut)
    
  }else{print("An error occurred: Data not balanced."); break}
  #--------------------------------------------------    
  
  
  
  #-------------------------------#
  #--- 5. train classification ---#
  #-------------------------------#
  
  #--> in progress
  
  itz = itz + 1
}
