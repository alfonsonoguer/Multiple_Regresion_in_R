## Product Profitability - Project Description


**Project Goal:** 
The fictitious client has concerns regarding the current sale of 4 specific products (PCs, Laptops, Netbooks and Smartphones). Firstly, using historical sales data we performing a sales forecast to understand how these four product types impact the company's overall profitability. Secondly, we aim to understand which product characteristic variables drive sales volume. 

**Data characteristics:**  
For the project, we have been provided with historical sales data and another set of potential new products which might be added to the portfolio of the client.  

**Language used:** R 

## Product Profitability  - Technical Approach

**1. Data Preprocessing:**

* Missing values check
* Duplicates check
* Data types & classes treatment
* Dummify variables
* Outliers detection & treatment

**2. Attribute Selection and Engineering**

* Correlation analysis (cor & corrplot)
* Decision Tree
* Multiple regression to understand the statistical significance of individual variables
*	Normalising Variables

**3. Data Partition & Cross Validation**

**4. Modelling**

* Linear Model
* Gradient Boosting Trees
* Random Forest

**5. Prediction and Error Analysis** 

**6. Validation**

## Product Profitability  - Results

**Successful Product Categories:** 
Looking at the four product types (PCs, Netbooks, Laptops, Smartphones), our model predicts that **PCs** are predicted to be the most successful category, followed by **netbooks, Laptops and Smartphones**. 

**Product characteristics driving sales volume:**
Regarding the second objective of the analysis (impact of service review on predicted volumes) we can note that **only PositiveServiceReview has a significant impact on the predicted Sales Volume**, yet its direct impact on the respective Product type categories cannot be assessed due to the limited information for the respective sales categories. Additionally, we can further highlight that **NegativeServiceReview does not have an impact on the predicted Sales Volume**, whereby, once  again, its impact on the respective product categories cannot be assessed due to the limited amount of data. 
