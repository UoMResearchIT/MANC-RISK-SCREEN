_A discrete event simulation model to evaluate the costs and outcomes of different approaches to breast cancer screening in the UK._
<br />
<br />

Welcome to the **MANC-RISK-SCREEN** Shiny App! This app provides an accessible interface for researchers and decision makers to explore the **MANC-RISK-SCREEN** model. This page provides a brief description of the underlying model. Click on the `help` tab for instructions on how use this app.

## About the model

The **MANC-RISK-SCREEN** model is a discrete event simulation model which aims to predict the costs, outcomes, and cost-effectiveness of six breast cancer screening strategies in the UK. The model includes three universal screening strategies (`No screening`, `2-yearly screening for all`, and `3-yearly screening for all`), and three strategies which use breast cancer risk prediction to alter the screening interval for women at different levels of risk:

1.	`PROCAS screening`: annual screening for women with 10-year risk higher than 8%, 2 yearly screenings for women with a risk between 5% and 8%, and 3 yearly screenings for all other women.
2.	`Fully stratified screening`: As above, but with 5 yearly screenings for women with a risk below 1.5%.
3.	`Risk Tertiles`: annual screening for women in the highest third of 10 year risk, 2 yearly screenings for women in the middle third, and 3 yearly screenings for women in the bottom third.

The risk prediction tool used in the last three cases is the Tyrer-Cuzick questionnaire and Volpara automated breast density measurement.

The model uses a wide range of input parameters to simulate women through each screening programme, producing average outcomes including costs and Quality-Adjusted Life Years (QALYs). In the full analysis, a probabilistic sensitivity analysis (PSA) was also conducted to address parameter uncertainty. Using the PSA draws, a regression model known as a generalised additive model (GAM) was estimated to predict the costs and QALYs for each strategy based on the values of the input parameters. This GAM model forms the basis for this shiny app, allowing the user to choose a selection of input values to explore their impact on the cost-effectiveness results.