This Shiny application was created in response [to this question on StackOverflow](https://stackoverflow.com/questions/49452857/how-to-listen-for-multiple-events-which-contain-an-isolate-in-a-shiny-eventrea). In the app, after pressing "Start", the user is faced with two images (in this case, two randomly generated plots) for them to select which one they prefer. Based on the selected image, the second image changes and the user is presented with a new choice.

It is a simple app that uses `eventReactive` to handle the events.