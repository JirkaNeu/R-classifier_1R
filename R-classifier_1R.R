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
  
  auto_data = auto_data_train
  #--> preparations
  md_l = length(md_names) #--> md_names defined in 2.
  F_Quote = seq(0,0, length.out = md_l)
  i_Anzahl = seq(0,0, length.out = md_l)
  
  test_vec = NULL #--> check counts
  prognos_vec = NULL #--> vector for the respective prediction values
  
  
  #----------------- big while-loop ---#
  i = 1
  while (i <= md_l){
    
    md_ufac = unique(auto_data[,i]) #--> count unique factors in column i
    
    print(paste0("Attribut_", i, ": ", md_names[i], " hat ", length(md_ufac), " Faktoren: "))
    print(" ")
    
    prognos_vec = as.character(result_df[i, 1]) #--> prognos_vec goes to prognos_list
    
    #------------------- aditional for-loop --#
    for (iz in md_ufac){
      print(iz)
      print(" ")
      
      Sp_Fac = auto_data[auto_data[, i] == iz,]
      
      #--- Häufigkeiten ermitteln ---#    
      Anzahl = length(Sp_Fac$origin) #--> count observations in iz-charactaristic of depending Variable (origin)
      
      #--> je nach 1 und nach 3 filtern
      Sp_Fac_1 = Sp_Fac[Sp_Fac$origin == 1,]
      
      Sieg_x = length(Sp_Fac_1$origin)
      Lose_x = Anzahl - Sieg_x #
      
      print(paste0("Siege: ", Sieg_x, " von ", Anzahl))
      print(paste0("Lose: ", Lose_x, " von ", Anzahl))
      
      
      if (Sieg_x >= Lose_x) {
        prognos_vec = append(prognos_vec, c(as.character(iz), "Sieg")) #--> prognos_vec goes to prognos_list
        F_Quote_einzel = Lose_x #--> temporary
        print(paste0(iz, ": Sieg prognostiziert."))
      } else {
        prognos_vec = append(prognos_vec, c(as.character(iz), "Lose")) #--> prognos_vec goes to prognos_list
        F_Quote_einzel = Sieg_x #--> temporary
        print(paste0(iz, ": Lose prognostiziert."))
      }
      
      
      test_vec = append(test_vec, Anzahl)#--> 2 check 
      
      print(paste0("Fehlerquote: ", F_Quote_einzel, " von ", Anzahl))
      print(" ")
      
      
      F_Quote[i] = F_Quote[i] + F_Quote_einzel #--> error rate
      i_Anzahl[i] = i_Anzahl[i] + Anzahl #--> count
      
    }
    #----------------- for-loop end ---#
    
    
    result_df[i,2] = paste0(F_Quote[i], " von ", i_Anzahl[i])
    result_df[i,3] = F_Quote[i] / i_Anzahl[i] #--> error rate 
    
    prognos_list = append(prognos_list, list(prognos_vec)) #--> prediction vector being passed on to the list
    prognos_vec = NULL #--> prediction vector being reset
    
    print(paste0("Gesamtfehler: ", F_Quote[i]))
    print(paste0("Gesamtanzahl: ", i_Anzahl[i]))
    print(paste0("Gesamtfehlerquote: ", F_Quote[i], " von ", i_Anzahl[i]))
    print(paste0("Entspricht: ", F_Quote[i] / i_Anzahl[i]))
    print(" ")
    print("+-+-+_________________________________+-+-+")
    print(" ")
    
    if (i == 15){break} #--> safety break
    i = i + 1;
  }
  #----------------- big while-loop end ---#
  
  
  #--> in progress...
  
  
  
  
  itz = itz + 1
}
