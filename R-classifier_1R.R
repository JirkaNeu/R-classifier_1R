#install.packages("ISLR")
library(ISLR)

## 2dos
## upsampling
## result view


quantilize <- function(this_col){
  col_num = which(colnames(auto_data) == this_col)
  my_qu = quantile(auto_data[, col_num])
  
  auto_data[, col_num][auto_data[, col_num] < my_qu[2]] = 1 #25%
  auto_data[, col_num][auto_data[, col_num] >= my_qu[2] & auto_data[, col_num] < my_qu[3]] = 2 #50%
  auto_data[, col_num][auto_data[, col_num] >= my_qu[3] & auto_data[, col_num] < my_qu[4]] = 3 #75%
  auto_data[, col_num][auto_data[, col_num] >= my_qu[4]] = 4 #100%
  
  return(auto_data[, col_num])
}


do_loops = 10 #--> how many times the classification is to be repeated
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
    
    #------------------- additional for-loop --#
    for (iz in md_ufac){
      print(iz)
      print(" ")
      
      Sp_Fac = auto_data[auto_data[, i] == iz,]
      
      #--- get occurrences ---#    
      Anzahl = length(Sp_Fac$origin) #--> count observations in iz-characteristic of depending Variable (origin)
      
      #--> filter origin 1
      Sp_Fac_1 = Sp_Fac[Sp_Fac$origin == 1,]
      
      Sieg_x = length(Sp_Fac_1$origin)
      Lose_x = Anzahl - Sieg_x #
      
      print(paste0("Score: ", Sieg_x, " of ", Anzahl))
      print(paste0("Lose: ", Lose_x, " of ", Anzahl))
      
      
      if (Sieg_x >= Lose_x) {
        prognos_vec = append(prognos_vec, c(as.character(iz), "Sieg")) #--> prognos_vec goes to prognos_list
        F_Quote_einzel = Lose_x #--> temporary
        print(paste0(iz, ": Score predicted."))
      } else {
        prognos_vec = append(prognos_vec, c(as.character(iz), "Lose")) #--> prognos_vec goes to prognos_list
        F_Quote_einzel = Sieg_x #--> temporary
        print(paste0(iz, ": Lose predicted."))
      }
      
      
      test_vec = append(test_vec, Anzahl)#--> 2 check 
      
      print(paste0("Error rate: ", F_Quote_einzel, " of ", Anzahl))
      print(" ")
      
      
      F_Quote[i] = F_Quote[i] + F_Quote_einzel #--> error rate
      i_Anzahl[i] = i_Anzahl[i] + Anzahl #--> count
      
    }
    #----------------- for-loop end ---#
    
    
    result_df[i,2] = paste0(F_Quote[i], " von ", i_Anzahl[i])
    result_df[i,3] = F_Quote[i] / i_Anzahl[i] #--> error rate 
    
    prognos_list = append(prognos_list, list(prognos_vec)) #--> prediction vector being passed on to the list
    prognos_vec = NULL #--> prediction vector being reset
    
    print(paste0("Count errors: ", F_Quote[i])) #--> all errors
    print(paste0("Count: ", i_Anzahl[i]))
    print(paste0("error rate: ", F_Quote[i], " of ", i_Anzahl[i]))
    print(paste0("error rate: ", F_Quote[i] / i_Anzahl[i]))
    print(" ")
    print("_______________________________________________")
    print(" ")
    
    if (i == 15){break} #--> safety break
    i = i + 1;
  }
  #----------------- big while-loop end ---#
  
  
  #--> clean environment
  rm(Anzahl, F_Quote, F_Quote_einzel, i_Anzahl, Lose_x, md_l, md_names, md_ufac, Sieg_x, test_vec)
  
  
  min_FQ = min(result_df$Fehlerquote)
  min_FQ = subset(result_df, Fehlerquote == min_FQ)
  min_FQ = min_FQ[1, 1]
  print(" ")
  #print(paste0("Das Attribut mit der geringsten Fehlerquote lautet: ", min_FQ))
  print(paste0("attribute with the least error rate: ", min_FQ))
  print(" ")
  print(" ")
  
  #--> prediction values being put together
  i = 1
  while (i <= length(prognos_list)){
    
    if (prognos_list[[i]][1] == min_FQ){
      
      z = 2 #--> start with 2
      while (z <= length(prognos_list[[i]])){
        
        prognos_vec_i = i #--> save position of prediction values
        
        if (prognos_list[[i]][z + 1] == "Lose"){progausgabe = "non-US"}
        if (prognos_list[[i]][z + 1] == "Sieg"){progausgabe = "US"}
        #print(paste0("Wenn ", print(min_FQ), " den Wert ", prognos_list[[i]][z], " annimmt, wird ", progausgabe, " vorhergesagt."))
        print(paste0("When ", print(min_FQ), " is ", prognos_list[[i]][z], " then ", progausgabe, " is the prediction."))
        
        print(" ")
        z = z + 2
      }  
    }
    
    i = i + 1
    if (i >= 15){break} #--> safety break
  }
  
  
  
  #-----------------------------------------------------#
  #--- 6. apply classification training to test data ---#
  #-----------------------------------------------------#
  
  
  
  prognos_vec = prognos_list[[prognos_vec_i]] #--> update prognos_vec with respective list entry
  
  m_pos = match(min_FQ, names(auto_data_test)) #--> min_FQ has the win, in which column is it?
  
  
  
  auto_data_test$prediction = NA #--> new column for prediction values
  
  
  i = 2
  
  while(i <= length(prognos_vec)){
    
    Wert1 = as.numeric(prognos_vec[i]) #--> turn character to numeric
    Wert2 = (prognos_vec[i + 1])
    if (Wert2 == "Lose"){Wert2 = 3}
    if (Wert2 == "Sieg"){Wert2 = 1}
    
    auto_data_test$prediction[auto_data_test[, m_pos] == Wert1] <- Wert2 #--> filter column m_pos for Wert1 and pass on to $prediction Wert2
    
    i = i + 2
    if (i >= 25){break} #--> safety break
  }
  
  
  #--------------------------------------------#
  #--- 7. calculate success- and error-rate ---#
  #--------------------------------------------#
  
  
  Filter_T1 = subset(auto_data_test, origin == 1)
  Treffer1 = nrow(subset(Filter_T1, prediction == 1))
  Fehler1 = nrow(Filter_T1) - Treffer1
  
  Filter_T2 = subset(auto_data_test, origin == 3)
  Treffer2 = nrow(subset(Filter_T2, prediction == 3))
  Fehler2 = nrow(Filter_T2) - Treffer2
  
  
  TrefferQuote = (Treffer1 + Treffer2) / (Treffer1 + Treffer2 + Fehler1 + Fehler2)
  FehlerQuote = (Fehler1 + Fehler2) / (Treffer1 + Treffer2 + Fehler1 + Fehler2)
  
  print(paste0("Success Rate: ", TrefferQuote, " %"))
  print(" ")
  print(paste0("Error Rate: ", FehlerQuote, " %"))
  
  
  success_rate_mean[itz] = TrefferQuote
  
  itz = itz + 1
  if (itz > do_loops){break} #--> safety break
}
#--> big while-loop end <-----

#--> clean environment
rm(Filter_T1, Filter_T2, Sp_Fac, Fehler1, Fehler2, FehlerQuote, progausgabe, Treffer1, Treffer2, TrefferQuote, Wert1, Wert2)


#--------------------------------#
#--- 7.1 average success rate ---#
#--------------------------------#

print("")
print(paste0(itz - 1, " run(s) of the script have resulted in an average success rate of ", round(mean(success_rate_mean), 4), " %."))
print("")

summary(success_rate_mean)


