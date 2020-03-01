#load("full results.RData")

params = head(names(results[[1]]),-2)
columns =  c("config", "type", params, "(intercept)", "X1","X2","X3")

data = data.frame(matrix(ncol = length(columns), nrow = 0))


# rep.row<-function(x,n){
#   matrix(rep(x,each=n),nrow=n)
# }

n_configs = 1
n_simulations = 1

#load the configurations
for(c in 1:n_configs){
  config = results[[c]]
  
  if(length(config$simulation[[1]][[2]])>1){
    n_types = 4
  } else {
    n_types = 2
  }
  
  
  temp = data.frame(matrix(ncol = length(columns), nrow = n_simulations*n_types))
  
  temp[,1] = c
  temp[,3:(2+length(params))] = config[params]
    #unlist()
  
  for(s in 1:n_simulations){
    simulation = config$simulation[[s]]
    
      #mi
      mi_i = (s-1) * n_types + 1
      temp[mi_i, 2] = "MI"
      temp[mi_i, 13:16] = simulation$mi[,1]
        
      #boot
      boot_i = (s-1) * n_types + 2
      temp[boot_i, 2] = "Boot"
      temp[boot_i, 13:16] = simulation$boot[,1]
    
      if(n_types == 4){
      
        #mi ddc
        mi_i_ddc = (s-1) * n_types + 3
        temp[mi_i_ddc, 2] = "MI DDC"
        temp[mi_i_ddc, 13:16] = simulation$mi_ddc[,1]
        
        #boot ddc
        boot_i_ddc = (s-1) * n_types + 4 
        temp[boot_i_ddc, 2] = "Boot DDC"
        temp[boot_i_ddc, 13:16] = simulation$boot_ddc[,1]
      }
      
  }
  
  #save to df
  data = rbind(data, temp)
}

colnames(data) = columns