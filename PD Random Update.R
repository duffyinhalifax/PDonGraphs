###########################################################FUNCTIONS############################################################


## Generate a grid where colours tell how many times each of the vertices changed role after their initial assignment
changersMap <- function(M){
#  par(bty="n", xaxt="n", yaxt="n", oma=c(0,0,0,0), mar=c(0,0,0,0), plt=c(0,1,0,1))
  plot(0,0,xlim = c(1,n),ylim=c(1,m),type ="n",asp=1, xlab="", ylab="")
  for(i in 1:n){
    for(j in 1:m){
      if(M[i,j] == 0){points(i,j,col= "green",pch = 15)}#green is no change
      else if(M[i,j] == 1){points(i,j,col= "orange",pch = 15)}#1 change
      else if(M[i,j] == 2){points(i,j,col= "steelblue2",pch = 15)}#red is no change
      else if(M[i,j] == 3){points(i,j,col= "steelblue4",pch = 15)}#red is no change
      else {points(i,j,col= "red",pch = 15)}#many changes!
    }
  }
}


##THIS FUNCTION MAKES A PLOT OF THE GRID COLOURING COOPERATORS AS BLUE AND DEFECTORS AS RED
Plotea <- function(M){
  
  par(bty="n", xaxt="n", yaxt="n", oma=c(0,0,0,0), mar=c(0,0,0,0), plt=c(0,1,0,1))
  plot(0,0,xlim = c(1,n),ylim=c(1,m),type ="n",asp=1, xlab="", ylab="")
  for(i in 1:n){
    for(j in 1:m){
      if(M[i,j] == 0){points(i,j,col= "red",pch = 15)}#red is defect
      else points(i,j,col="blue",pch=15) #blue is collab
    }
  }
}

##Return the ratio of cooperators to total vertices
Ratio <- function(roles){
  count = 0;
  for(i in 1:n)
    for(j in 1:m)
      if(roles[i,j] == 1) {count = count+1}
  return(count/(n*m))
}

##Recalculate all of the current scores based on the current roles 
evalAllScore <- function(localroles){
  newScores = matrix(rep(0,n*m),ncol = m, nrow = n);
  for(i in 1:n)
    for(j in 1:m)
      for(k in -2:0)
        for(l in -2:0)
          if((k+l)%%2 == 1){newScores[i,j] = newScores[i,j] + payoff(localroles[i,j],localroles[((i+k)%%n)+1,((j+l)%%m)+1]);}
  return(newScores)
}

## Calculate the payoff for a in the game a vs. b #0 is defect, 1 is collab
payoff <- function(a,b){
  if(a == 0 && b == 1) return(t)
  if(a == 1 && b == 1) return(1)
  else return(0)
}


## Given a vertex and its neighbours, return the new role of the vertex.
change <- function(i,j,nbrsi,nbrsj){
  #Find my max neighbour
  maxi = i;
  maxj = j;
  M = nbrs(i,j,nbrsi,nbrsj)
  
  for (k in 1:4){
    if(score[M[k,1],M[k,2]] > score[maxi,maxj]){
      maxi = M[k,1]
      maxj = M[k,2]
    }
  }
  #Set my role to be that of the max
  return(roles[maxi,maxj])
}

##Update the scores surrounding a single vertex. 
singleupdate <- function(i,j,roles){
  M = nbrs(i,j,nbrsx,nbrsy)
  newscore=0
  for (k in 1:4){
    newscore = newscore + payoff(roles[i,j], roles[(M[k,1]),(M[k,2])])
  }
  return(newscore)
  
}

##Calculate the neighbourhood of a vertex
nbrs <- function(i,j,nbrsx,nbrsy){
  M = matrix(NA,4,2)
  M[1,] = c(nbrsx[[i]][1],j)
  M[2,] = c(nbrsx[[i]][2],j)
  M[3,] = c(i,nbrsy[[j]][1])
  M[4,] = c(i,nbrsy[[j]][2])
  return(M)
}



##This code can be used to autmatically save a plot to the local folder.
# M = roles
# pdf("roles.pdf")
# par(bty="n", xaxt="n", yaxt="n", oma=c(0,0,0,0), mar=c(0,0,0,0), plt=c(0,1,0,1))
# plot(0,0,xlim = c(1,n),ylim=c(1,m),type ="n",asp=1, xlab=NULL, ylab=NULL)
# for(i in 1:n){
#   for(j in 1:m){
#     if(M[i,j] == 0){points(i,j,col= "red",pch = 15)}#red is defect
#     else points(i,j,col="blue",pch=15) #blue is collab
#   }
# }
# dev.off()



######################################################Parameters############################################################
m = 50;n = 50;##GRID OF nxm
t =1.9 ##CHEATING ADVANTAGE
r = 0.9 ##PROB. OF INITIALY BEING A COOP.
N = 10 ##AMOUNT OF ITERATIONS WE WANT
Plot=NULL
roles = matrix(rep(1,n*m),ncol = m, nrow = n)
changes = matrix(0,n,m)
score = NULL
ratio = 0
Plot = NULL
changeLengthHistory = array(NA, 15000)
sizeHistory = array(NA,N)
intsHistory = array(NA,N)
nbrsx = list()
nbrsy = list()
initialChangersHistory = array()
weakcollabcountsim = array(0,1000)
prevRoles = matrix(0,m,n)
phiHistory = array(NA, N)
ratioHistory = array(NA,N)

##declare simulation history
simHistory = list()


######################################################PROCEDURE##########################################################

##Precompute Neighbourhoods
for (a in 1:n){
  nbrsx[[a]] = array(NA,2)
}

for (b in 1:m){
  nbrsy[[b]] = array(NA,2)
}

for (a in 2:n-1){
  for (b in 2:m-1){
    nbrsx[[a]][1] = a+1
    nbrsx[[a]][2] = a-1
    nbrsy[[b]][1] = b+1
    nbrsy[[b]][2] = b-1
  }
}

nbrsy[[1]][1] = 2
nbrsy[[1]][2] = m
nbrsy[[m]][1] = 1
nbrsy[[m]][2] = m-1

nbrsx[[1]][1] = 2
nbrsx[[1]][2] = n
nbrsx[[n]][1] = 1
nbrsx[[n]][2] = n-1


#### We will run N simulation, each time we will generate a new starting position based on the initial parameters.
for(beta in 1:N){

  #reset parameters for this simulation
  ratio = NULL
  ratio = Ratio(roles)
  changes = matrix(0,n,m)  
  rolesHistory = list()
  
  
  
#############Set the initial Configuration of the Grid###############
    
  ####IF YOU WANT THE INITIAL CONFIGURATION TO BE A SQUARE OF COOPERATORS
  ##AT THE CENTER OF THE GRID, THEN UNCOMMENT THE FOLLOWING 10 LINES
#   for(i in 1:n){   
#     for(j in 1:m){
#       if(i == floor(n/2) && j == floor(m/2)){ roles[i,j] = 1;}
#       else if(i == floor(n/2)  && j == (floor(m/2)+1)){ roles[i,j] = 1;}
#       else if(i == (floor(n/2)+1) && j == floor(m/2)){ roles[i,j] = 1;}
#       else if(i == (floor(n/2)+1) && j == (floor(m/2)+1)){ roles[i,j] = 1;}
#       else{roles[i,j] = 0;}
#     }
#   }
  
  for (i in 1:n){
    for (j in 1:m){
      roles[i,j] = 0
    }
  }
#   
  roles[15,15] = 1
  roles[15,16] = 1
  roles[16,15] = 1
  #roles[16,16] = 1
  #roles[17,15] = 1
  #roles[17,16] = 1
  #roles[16,14] = 1
  #roles[16,17] = 1  
  
  
#   #########Randomize the locations of the defectors
#   for (a in 1:n){
#     for (b in 1:m){
#       if (runif(1) > r){
#         roles[a,b]=0;
#       }
#       else {roles[a,b]=1;}
#     }
#   }
  
  
  ######A line of collaborators of length 4
#   for (a in 1:n){
#     for (b in 1:m){
#       roles[a,b]=0
#     }
#   }
#   
#   for(a in 20:23){
#     roles[a,20] = 1
#   }
 
  # ############Place a square of collaborators in a a field of defectors
  #   for(a in 1:n){
  #     for (b in 1:m){
  #       if ((a >  n/2-25 && a < n/2+25) && (b >  m/2-25 && b < m/2+25)){
  #         roles[a,b] = 1;
  #       }
  #       else{
  #         roles[a,b] = 0;
  #       }
  #     }
  #   }
  
  
#   #Choose the roles so that we get half fields
#   for (a in 1:n){
#     for (b in 1:m){
#       if (a > n/2){
#         roles[a,b] = 1
#       }
#       else{
#         roles[a,b] = 0;
#       }
#     }
#  }
   

  rolesHistory[[1]] = roles
  Plotea(roles)  
  

  
  ## Evaluate initial scores
  score = evalAllScore(roles)
  
  ### We assume that our process will end after at most 1500 iterations. This assumption is unfounded, but seems reasonable based on observation
  for (phi in 1:1500)
  {
   # Plotea(roles)
    
    prevRoles = roles ## Copy the current roles
    
    ##Locate the weak vertices and track how many there are
    changersx = array(NA)
    changersy = array(NA)
    changelength = 0;
    for (a in 1:n){
      for (b in 1:m)
        if (change(a,b,nbrsx,nbrsy) != roles[a,b]){
          changelength = changelength + 1;
          changersx[changelength] = a
          changersy[changelength] = b
          if (roles[a,b] == 1){weakcollabcountsim[phi]= weakcollabcountsim[phi]+1 }
        }
    }
    changeLengthHistory[phi]=changelength;
    if (changelength == 0){
      #print("round: "); print(beta);
      intsHistory[beta] = phi;
      break;
    }
    
    
    ##########UPDATE STRATEGY - Permute the list of things to be updated, and then update them in order  #################
    updateOrder = sample(changelength)

    for (i in 1:changelength)
    {
      ch = updateOrder[i] ######index of who I am changing this time
      
      ######change if we want and track change
      if (roles[changersx[ch],changersy[ch]] != change(changersx[ch],changersy[ch], nbrsx,nbrsy)){
        changes[changersx[ch],changersy[ch]] = changes[changersx[ch],changersy[ch]] + 1; ######track who I change so I know how many times each vertex changed
        roles[changersx[ch],changersy[ch]] = change(changersx[ch],changersy[ch], nbrsx,nbrsy)
      }
      M = nbrs(changersx[ch],changersy[ch],nbrsx,nbrsy); #####Locate my neighbours
      score[changersx[ch],changersy[ch]] = singleupdate(changersx[ch],changersy[ch],roles) #####Update my score and the scores of all my neighbours
      for (k in 1:4){
        score[M[k,1],M[k,2]] = singleupdate(M[k,1], M[k,2],roles) 
      }
    }
    

    rolesHistory[[phi+1]] = roles ##update the roleshistory

    ###### If none of the roles changed, then we have reached a stable set.   
    if(all(prevRoles == roles)){break}  
    
  } ##End of updating a round

#######Put rolesHistory in to simulation history.
simHistory[[beta]] = rolesHistory
} ## End of simulation

