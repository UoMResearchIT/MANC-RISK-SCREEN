## Using the app

To begin using the app, please select the `Main` tab at the top of the screen. This will take you to a new page featuring tabs for different input values on the left side and an output table and graph on the right side. 

Five tabs on the left hand side (`Mortality`, `Cancer Growth`, `Screening`, `Cost`, and `Utility`) allow you to switch between each input value. You can change the value of each input value dragging the corresponding slider. When an input value is changed, the predicted outputs reported in the model output table and graph will automatically update to reflect the new values.

The GAM model was estimated for wide ranges of parameter values. However, you can choose input values outside this range. In this case a warning message will appear, as it is possible that results produced from these values are less reliable.

For some parameters, such as 5-year cancer survival rates, there is also a logical ordering to the parameter values. A red warning message will be shown if you choose values which do not follow the logical ordering.

At any point you can click the `reset` button in the app to return all parameters to their base-case values. Use the `save` button to save the current input values into a text file, which can later be used to `restore` the session.