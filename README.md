# ProjectDirectoryTemplate
A template for project directory structures (focussed on R based analyses).

## Background
Scientific analyses are much easier if you get a few basic principles of scientific programming right. It's much easier to do this right at the start. This template repository will help you do just that. Specifically it will help you write well organised reproducible code.

## Making your own repository from the template
First click the green button in the top right corner "Use this template". Follow the instructions to create your own repository. Once you are done with that we need to "clone" this repository onto your local machine so you can start working with it. There are lots of ways to do this. We're going to use GitHub Desktop. You can download it here: https://desktop.github.com/. Once it is installed, open it, click "file" in the top right corner and select "Clone repository". You should see your newly created repository in that list. Select it and click "Clone" (the blue button). Now you have your own project repository.

## Getting started with analysis
Before you start doing your own analysis it helps to get familiar with the basic workflow. The repository we just created contains a demo to do just that. First we need a good code editor, we're going to use Visual Studio Code, download it here: https://code.visualstudio.com/. Once it is installed, open it and open the folder we created in the step above (the local clone of your new repository). Next open a terminal (click "Terminal" > "New Terminal" along the top row of buttons). Once the terminal is open type "R" to open R. 

Now you can run the scripts in the code directory in the numbered order. If everything works you should end with a scatter plot in file called "Figure1.png" in the Figures directory.

## Starting your own analyses
If everything worked you can now use this template to start your own analyses. First delete the script 00_ExampleOnly_MakeDemoData.R along with the contents of Data, Outputs, and Figures. (Deleting the contents of Outputs and Figures is an important thing to do from time to time, it forces you to ensure that your code is well organised and you can easily recreate the Outputs and Figures!)

Now you should modify script 01_CleanData.R to load in your own dataset, then conduct any cleaning operations you need to perform to get it ready for analysis, and then save this cleaned version to the Outputs directory. Once this is done you can do the same with the subsequent scripts to do the modelling you need for your analysis, and make the figures you want. Remember you can add additional scripts as you need. Your final analysis will likely include many scripts, try and make sure that each is a clearly distinct task with inputs and outputs, and can be run in a relatively short period of time. 

## Other useful links
Try and write code in a consistent style, it will make it much easier to write, read, and to get help with. Follow this style guide: https://style.tidyverse.org/

If you want to better understand R then start with https://r4ds.hadley.nz/

If you're looking for deeper guidance on scientific programming this guide from Prof Samrat Pawar at Imperial is excellent: https://mhasoba.github.io/TheMulQuaBio/intro.html

If you are already confident with R and scientific programming and want to improve further, e.g. if you want to write more and better functions, then this is the guide for you: https://adv-r.hadley.nz/index.html

A personal favourite way of thinking about common statistical tests (hint: they are all linear models): https://lindeloev.github.io/tests-as-linear
