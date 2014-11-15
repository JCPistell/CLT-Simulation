require(ggplot2)
set.seed(7759)

# Create a non-uniform population of 100,000 numbers between 1 and 100
pop1 <- rnorm(20000, mean = 10, sd = 3)
pop2 <- rnorm(80000, mean = 70, sd = 10)
pop <- c(pop1, pop2)

mu <- mean(pop) #calculate the population mean
sigma <- sd(pop) #calculate the population standard deviation
rm(pop1, pop2) #clean up

# Histogram of population, stored in variable hg
popdf <- as.data.frame(pop)
hg <- ggplot(popdf, aes(x = pop)) + geom_histogram(colour = "black", fill = "steelblue") + 
        ggtitle("Histogram of Population") + xlab("value")
rm(popdf)

n <- c(1, 5, 10, 30, 50, 100) #set up number of samples
t <- c(10, 100, 1000, 10000) #set up number of trials in simulation

df <- data.frame() #initialize our empty data frame

# Run the simulation
for(i in n) { #for each value of n...
    col <- c()
    for(j in t) { #we loop through each value of t...
        trial <- 1:j
        counter <- j
        value <- c()
        while(counter > 0) {    # and extract n samples from the population...
            bucket <- sample(pop, i, replace = TRUE)
            xbar <- mean(bucket) #calculate the mean...
            value <- c(value, xbar) # and add it to a vector
            counter <- counter - 1
        }
        sbar <- sd(value) #calculate the standard deviation of our sample
        col <- cbind(trial, value, sbar, i, j) #stick all the info together...
        df <- rbind(df, col) #and attach it to our master data frame
    } #and we do it again for the next set of values until we're done!
    
}

rm(col, bucket, value, counter, i, j, n, sbar, t, xbar, trial) #clean up

# We tidy up our data frame to get it ready for graphing. Note that we built it in "tall"
# form so it's already structured for ggplot

names(df) <- c("trial#", "value", "sdev", "samples", "trials")

# Creating the plot, stored in variable g
g <- ggplot(df, aes(x = value)) + geom_density(fill = "steelblue") + 
        facet_grid(samples ~ trials, labeller = label_both) + 
        ggtitle("Demonstrating The Central Limit Theorem With Simulation") +
        geom_vline(xintercept = mu, linetype = "dashed")
g

# create data frame of simulated sample standard deviations
m <- matrix(unique(df$sdev), nrow = 4, ncol = 6)
sdf <- as.data.frame(m, row.names = c("t10", "t100", "t1000", "t10000"))
names(sdf) <- c("s1", "s5", "s10", "s30", "s50", "s100")
sdf <- t(sdf) #transposed to match our graph better
rm(m)

# Calculate our expected standard error from the population sigma
exvals <-  sigma/sqrt(c(1, 5, 10, 30, 50, 100))

# Express how close we are as percentages. Transposition is done to accomodate how R
# handles division within a data frame and so the result matches up better with our graph
pexval <- sdf/exvals

# Subtract 1 to show distance from ideal and round to 3 decimals to make it readable
round(pexval - 1, 3)
