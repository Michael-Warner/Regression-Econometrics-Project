# Econometrics & Statistical Modeling 
## Purpose:
The purpose of this project was to be able to predict if a client would cancel a booking or not.

![image](https://user-images.githubusercontent.com/76630966/119968194-751cc800-bfad-11eb-92e3-7d50ca760822.png)

### Please see our detailed presentation [here](https://drive.google.com/file/d/1No2MAPx6lTfSguwJidgJet4MYOqzmOwH/view?usp=sharing).

## Data Set:
The data used for this project was open source from [Kaggle](https://www.kaggle.com/jessemostipak/hotel-booking-demand).
This data set from two different hotels, one is a Resort hotel (H1) and the other is a City hotel (H2).

There’re 32 variables describing 40,060 observations of H1 and 79,330 observations of H2.

It consists of a two year time period between July 1st, 2015 and August 31st, 2017. It includes the bookings of people who
arrived and the bookings that were canceled.

## Method:

The methodologies employed was to first create a data/stakeholder analysis chart to guide the work to be done. 
![image](https://user-images.githubusercontent.com/76630966/119970619-3fc5a980-bfb0-11eb-992f-1d38f25a54cb.png)

Then the data was cleaning and a deep exploratory data analysis (EDA), was taken place.

Lastly a logistic regression model was created based on the dichotomous dependant variable, canceled or not cancled, only using significant values.

## Results

The logistic regression model only correctly predicted the non cancelation clients 50% of the time, but had a 0% probability of correctly guessing when clients would actually cancel. 

![image](https://user-images.githubusercontent.com/76630966/119972863-06db0400-bfb3-11eb-8d1d-a698c6152b92.png)

## Conclusion

This model can be improved through real world use, instead of mock data sets. There were some variables in this data set that were not fully explained and by having access to the key stakeholders the concerns for specific variables could have been cleared up. 
