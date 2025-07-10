#### Simulation 1: 
#### No p-hacking, null is true, 0% hacker
sim1 <- sim.multDVhack(nvar=1, r=0, d=0, prop_Hacker=0, prop_H1=0, het=0, iter=10000, alpha=0.05)

# Check if p-values for all three reporting strategies are the same
any(sim1[,1]!=sim1[,2])
any(sim1[,1]!=sim1[,3])

# Check if approximately 5% of p-values are below .05
sum(sim1[,1] < 0.05)/10000

#### Simulation 2: 
#### Null is true, 30% hacker, extreme p-hacking
sim2 <- sim.multDVhack(nvar=50, r=0, d=0, prop_Hacker=0.3, prop_H1=0, het=0, iter=10000, alpha=0.05)

# Check if more than 5% of p-values are below .05
sum(sim2[,1] < 0.05)/10000

# Check if among p-hackers % of p-values exceeds .05
sum(sim2[1:(0.3*10000),1] < 0.05)/length(1:(0.3*10000))

# Check if among non-p-hackers only 5% of p-values are below .05
sum(sim2[(0.3*10000+1):10000,1] < 0.05)/length((0.3*10000+1):10000)

#### Simulation 3: 
#### Null is true, 80% hacker, extreme p-hacking
sim3 <- sim.multDVhack(nvar=50, r=0, d=0, prop_Hacker=0.8, prop_H1=0, het=0, iter=10000, alpha=0.05)

# Check if proportion of p-values below .05 increases compared to Simulation 2
sum(sim2[,1] < 0.05)/10000 < sum(sim3[,1] < 0.05)/10000

#### Simulation 4: 
#### Null is true, p-hacking with 2 zero-correlated DVs, 100% hacker
sim4 <- sim.multDVhack(nvar=2, r=0, d=0, prop_Hacker=1, prop_H1=0, het=0, iter=10000, alpha=0.05)

# Check if approximately 1-(1-0.05)^2 = 9.75% of p-values are below .05
sum(sim4[,1] < 0.05)/10000

#### Simulation 5: 
#### Alternative is true, but ES is small (i.e., low power) with no heterogeneity, no p-hacking
sim5 <- sim.multDVhack(nvar=1, r=0, d=0.1, prop_Hacker=0, prop_H1=1, het=0, iter=10000, alpha=0.05)

# Check if more than 5% of p-values are below .05
sum(sim5[,1] < 0.05)/10000

#### Simulation 6: 
#### Alternative is true, but ES is small (i.e., low power) with no heterogeneity, 80% hacker, extreme p-hacking
sim6 <- sim.multDVhack(nvar=50, r=0, d=0.1, prop_Hacker=0.8, prop_H1=1, het=0, iter=10000, alpha=0.05)

# Check if more than 5% of p-values are below .05
sum(sim6[,1] < 0.05)/10000

# Check if among p-hackers % of p-values exceeds .05
sum(sim6[1:(0.8*10000),1] < 0.05)/length(1:(0.8*10000))

# Check if among non-p-hackers only 5% of p-values are below .05
sum(sim6[(0.8*10000+1):10000,1] < 0.05)/length((0.8*10000+1):10000)

# Check if more p-values were significant than in the previous simulation
sum(sim6[,1] < 0.05) > sum(sim5[,1] < 0.05)

#### Simulation 7: 
#### Alternative is true, ES is high (i.e., high power) with no heterogeneity, no p-hacking
sim7 <- sim.multDVhack(nvar=1, r=0, d=0.8, prop_Hacker=0, prop_H1=1, het=0, iter=10000, alpha=0.05)

# Check if more than 5% of p-values are below .05
sum(sim7[,1] < 0.05)/10000

#### Simulation 8: 
#### Alternative is true, ES is high (i.e., high power) with medium heterogeneity, no p-hacking
sim8 <- sim.multDVhack(nvar=1, r=0, d=0.8, prop_Hacker=0, prop_H1=1, het=0.3, iter=10000, alpha=0.05)

# Check if more than 5% of p-values are below .05
sum(sim8[,1] < 0.05)/10000

#### Simulation 9: 
#### Alternative is true, ES is high (i.e., high power) with medium heterogeneity, 10% medium p-hacking
sim9 <- sim.multDVhack(nvar=10, r=0, d=0.8, prop_Hacker=0.1, prop_H1=1, het=0.3, iter=10000, alpha=0.05)

# Check if more than 5% of p-values are below .05
sum(sim9[,1] < 0.05)/10000

# Check if more p-values were significant than in the previous simulation
sum(sim9[,1] < 0.05) > sum(sim8[,1] < 0.05)

#### Simulation 9: 
#### Alternative is true in 50% of cases, ES is high (i.e., high power) with medium heterogeneity, 10% medium p-hacking
sim10 <- sim.multDVhack(nvar=10, r=0, d=0.8, prop_Hacker=0.1, prop_H1=0.5, het=0.3, iter=10000, alpha=0.05)

# Check if more than 5% of p-values are below .05
sum(sim10[,1] < 0.05)/10000

# Check if fewer p-values were significant than in the previous simulation
sum(sim10[,1] < 0.05) < sum(sim9[,1] < 0.05)
