Mobile Price Prediction using R

Step 1: Install all the important header files and load the data.

Step 2: Display the basic infomation about the Dataset

Step 3 :Remove Data Duplicates while Retaining the First one - Similar to SQL DISTINCT :

Step 4: Fill or Imputes Missing values with Various Methods 

        i)Filling the missing values in City Code Patient

       ii)Getting uniques value of hospital code w.r.to null values of bed grade to list
       
        a) Displays Unique Values in Each Column of the Dataframe(Table) 
        
Step 5:Loading the test data after doing completeing step 4

Step 6: Concatinating of test and train data

Setp 7: Remove duplicated columns 

Step 8: Labelencoder to change the categorical feature into number 
      
Step 9:  Visualise the data
                
                i)This helps in seeing the correlation of data so we can drop the fields which are highly correlated
                
                ii)How clean is our data

Step 10:Drop irrelevant features

Step 11: Create/ modify features

Step 112:Create dummy variables(For categorical features)

Step 13:  Train and test Split()

Step 14: Fit and score the model
   
                i)Decision Tree classifer was used
                ii) Random Forrest classifier
                iii)SVR 
                
Step 13:Present the results

