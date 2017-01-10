############################################################
###                 RForcecom Analysis Tool              ###
############################################################


#### Overview: ######################################## ####

# Set-up  

# Step 1: Login and Get Object List 
# Step 2: Query & Prep Data
# Step 3: Count Tasks & Merge Back
# Step 4: Conduct Basic Logit Regression 
# Step 5: Update Salesforce Records 

# Set-up  ############################################# ####
############################################################

# If you need to, install RForcecom via by removing '##' and running the below code --v
## install.packages("RForcecom")

# It might ask you to select a CRAN mirror when installing packages.
# If so, remove the belove '##' and run code, and then rerun the install.packages("RForcecom") command
## chooseCRANmirror(0)  #<-- Choose '49' for USA (IA)

# Load the library
library(RForcecom)
# Ready to go!


# Step 1: Login and Get Object List  ################## ####
############################################################

# Establish your credentials within R, use them to login, and then save your active session to an R object
user <- 'yourname@yourusername.com'
pass_token <- 'yourpasswordandTOKEN#######################'
session <- rforcecom.login(user, pass_token)

# Take a look around the schema & print out a list of objects
schema <- rforcecom.getObjectList(session)
names(schema)
schema$name

# Save object meta-data to data.frames for easy exploration
opp_object <- rforcecom.getObjectDescription(session, "Opportunity")
task_object <- rforcecom.getObjectDescription(session, "Task")

# Get a list of fields for Opportunity and then Task 
names(opp_object)
opp_object$name
task_object$name

# Step 2: Query & Prep Data  ########################## ####
############################################################

# First, query your opportunities
soql_opp <- "SELECT Id, Name, IsClosed, Amount, IsWon FROM Opportunity"
opps <- rforcecom.query(session, soql_opp)
class(opps)

# Next, query your tasks
soql_task <- "SELECT Id, Subject, WhatId FROM Task"
tasks <- rforcecom.query(session, soql_task)
# WhatId will correspond to our Opportunities 

# Clean up booleans (the T/F outputs we get via RForcecom are factors, unfortunately, and not true Booleans)
opps$Closed[opps$IsClosed == "true"] <- TRUE
opps$Closed[opps$IsClosed == "false"] <- FALSE
opps$Won[opps$IsWon == "true"] <- TRUE
opps$Won[opps$IsWon == "false"] <- FALSE

# Also, convert Opportunity Amount from a factor to numeric
opps$Amount <- as.numeric(levels(opps$Amount))[opps$Amount]
class(opps$Amount)

# Double check to make sure it worked (if it didn't, it will print out factor levels in addition to the data)
opps[1,]$Amount


# Step 3: Count Tasks & Merge Back  ################### ####
############################################################

# Table (count) Tasks by WhatId, and reorder/reindex by descending frequency
task_counts <- as.data.frame(table(tasks$WhatId))
task_counts <- task_counts[order(task_counts$Freq, decreasing = TRUE),]
rownames(task_counts) <- NULL #<-- resets index
task_counts
names(task_counts) #<-- print out the names so you'll know what you need to merge back by

# Merge counts back to Opportunity table!!!
opp_tasks <- merge(opps, task_counts, by.x = "Id", by.y = "Var1", all.x = TRUE)
opp_tasks[is.na(opp_tasks)] <- 0	#<-- swaps out NAs with zeros
opp_tasks <- opp_tasks[order(opp_tasks$Freq, decreasing = TRUE),]
opp_tasks$Freq <- as.numeric(opp_tasks$Freq) #<-- just in case; class 'integer' should work otherwise
rownames(opp_tasks) <- NULL #<-- resets index

# Double-check everything:
opp_tasks
class(opp_tasks$Freq)
class(opp_tasks$Amount)


# Step 4: Conduct Basic Logit Regression  ############# ####
############################################################

# Great source reference article: https://ww2.coastal.edu/kingw/statistics/R-tutorials/logistic.html
# Run Regression and get summary
won_reg <- glm(Won ~ Freq, data = opp_tasks[opp_tasks$Closed == TRUE,], family = binomial )
summary(won_reg)

# If you need a measure of fit, compute a Pseudo R^2 (McFadden's R^2)
# http://thestatsgeek.com/2014/02/08/r-squared-in-logistic-regression/
won_null <- glm(Won ~ 1, data = opp_tasks[opp_tasks$Closed == TRUE,], family = binomial )
1-logLik(won_reg)/logLik(won_null)


# Plot the number of tasks vs the incident of winning (or not) for all Closed Opportunities
par(oma=c(1,1,1,1),mar=c(5,6,4,1)) #<-- extend the margins a bit
plot(opp_tasks[opp_tasks$Closed == TRUE,]$Freq,opp_tasks[opp_tasks$Closed == TRUE,]$Won, main = "# of Tasks for Closed Opportunities", xlab = "# of Tasks", ylab = "Fitted Win Probability \n(and actual outcome)\n LOST           WON")
lines(opp_tasks[opp_tasks$Closed == TRUE,]$Freq, won_reg$fitted, type = "l", col = "blue")

# Use the won_reg Regression output to predict success for open Opportunities
opp_tasks$Win_chance <- predict(won_reg, opp_tasks, type = "response")
opp_tasks[opp_tasks$Closed == FALSE,c("Name","Freq","Win_chance")]


# Step 5: Update Salesforce Records  ################## ####
############################################################

# Lastly, update Salesforce records with won_reg computed Win Probability
# To do so, use sapply with a quick custom function to move opp_tasks through "rforcecom.update"
sapply(1:nrow(opp_tasks), function(x) rforcecom.update(session,"Opportunity",opp_tasks[x,]$Id,fields = c(Win_Chance__c = (opp_tasks[x,]$Win_chance * 100) )) )
# In the 'fields' (where the actual data gets passed through), multiply by 100 to convert to Salesforce Percent data-type

############################################################
###                          End                         ###
############################################################