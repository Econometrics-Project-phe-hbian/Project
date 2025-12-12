# Project
# Repo for Part of Figure 1A from Peihua He
My work reproduced the plot 1a, which shows the relationships between the consumption of the energy and the response of the daily temperature changes under different global income deciles. Additionally, we differentiate between electricity and other fuels.

The data GMFD_TINV_clim_regsort.csv we used to analyze come from the data set. To increase the efficiency of data processing, we convert the data to the data.table format after we import the dataset. In addition, we construct the regional identification variables and the combinatorial variables of “sub-region × year” used for subsequent fixed-effect regression. After that, it is divided into two groups: electricity and other energy, and it eliminates the missing observations of per capita energy consumption.

The script estimates fixed-effect regression models for electricity and other energy sources, respectively. The model includes linear and quadratic temperature terms divided by the deciles of income, precipitation control variables, the “sub-region × year” fixed effect, and the standard error of clustering by regions. The setting allows the relationships between temperature and energy changes to be flexible around different income declines and controls the unobservable spatial and time heterogeneity.

After that, we construct a temperature grid from -5℃ to 35℃ after the regression estimation is completed. We used the estimated coefficient to rebuild the temperature response curve for each income decile and calculate the standard error and 95% confidence interval of the predicted value based on the variance-covariance matrix.

Then we selected three representative panels from temperature response curves covering all ten income deciles, corresponding to electricity and other energy: Income Decile 8 (electricity), Income Decile 10 (electricity), and Decile 5 (other fuels).

The final generated graph visually demonstrates the significant heterogeneity of the impact of temperature on energy consumption under different income levels and energy types. The results showed that the energy consumption of the high-income population is more sensitive to extreme temperatures. However, the temperature response of other energy sources in middle- and low-income groups is relatively gentle and shows a downward trend.