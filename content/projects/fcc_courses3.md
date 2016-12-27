+++
showonlyimage = false
draft = false
image = "projects/FCC_Courses_files/FCC_courses.jpg"
date = "2016-12-27"
title = "Mapping Student Course Activity"
weight = 0
type = "post"
author = "Amber Thomas"
tags = [
 "machine learning",
 "data viz"
]
keywords = [
"Free Code Camp",
"interactive visualization",
"data visualization",
"highcharter",
"network",
"MOOC"
]
+++

Cleaning, exploration, interactive visualizations, and machine learning with Free Code Camp's student course completion dataset. 
<!--more-->

-   [Introduction](#introduction)
    -   [Importing Necessary Packages](#importing-necessary-packages)
-   [Data Exploration](#data-exploration)
    -   [Importing Dataset](#importing-dataset)
    -   [Data Structure and Variables](#data-structure-and-variables)
-   [Feature Engineering](#feature-engineering)
    -   [Using Original Dataset](#using-original-dataset)
    -   [Web Scraping New Data](#web-scraping-new-data)
    -   [Engineered Variables](#engineered-variables)
-   [Data Visualizations](#data-visualizations)
    -   [What class do FCC students start with?](#what-class-do-fcc-students-start-with)
    -   [Which classes are the most popular?](#which-classes-are-the-most-popular)
    -   [How do students flow through the courses?](#how-do-students-flow-through-the-courses)
    -   [How many courses do students take?](#how-many-courses-do-students-take)
    -   [Can you group students based on the number of classes they take vs. the amount of time they spend per class?](#can-you-group-students-based-on-the-number-of-classes-they-take-vs.-the-amount-of-time-they-spend-per-class)
    -   [Does taking a 2 week break help or hurt course completion?](#does-taking-a-2-week-break-help-or-hurt-course-completion)
    -   [Which courses tend to be repeated by students?](#which-courses-tend-to-be-repeated-by-students)
    -   [Can we predict the number of courses a student will complete using machine learning?](#can-we-predict-the-number-of-courses-a-student-will-complete-using-machine-learning)
        -   [Fitting a baseline model](#fitting-a-baseline-model)
        -   [Creating trainControl](#creating-traincontrol)
        -   [Fitting a random forest model](#fitting-a-random-forest-model)
        -   [Fitting a glmnet model](#fitting-a-glmnet-model)
        -   [Comparing Model Fit](#comparing-model-fit)
-   [Conclusions](#conclusions)
-   [My Suggestions:](#my-suggestions)

Introduction
------------

[Free Code Camp](https://www.freecodecamp.com/) (FCC) is an online, self-paced, collection of massive open online courses (MOOCs) aimed at teaching users to code. Campers can log onto the service, complete coding challenges and projects, and earn certificates commemorating their completion. The topics covered include HTML5, CSS3, JavaScript, Databases, Git & Github, Node.js, React.js, and D3.js.

In December of 2015, FCC released [lots of data](https://medium.freecodecamp.com/free-code-camp-christmas-special-giving-the-gift-of-data-6ecbf0313d62#.ibujuvlhc) regarding the progress and solutions of their users throughout the courses. When I say lots, I mean there are data for over 100,000 students here!

Without having first looked at the dataset, the first questions I may consider are those posed by FCC itself:

-   Can our campers be broken down into cohort groups based on challenge completion behavior?
-   Is there a challenge completion tempo that typifies high-achieving campers? One that typifies slow-and-steady campers?
-   Many campers will leave Free Code Camp for several months due to burnout, or life getting in the way, then return a few months later. Are there any meaningful patterns here?
-   What proportion of campers complete challenges mostly in order, as opposed to skipping around?
-   What proportion of campers dive directly into harder challenges, then work backward until they are able to start successfully completing challenges?
-   Are there any challenges that seem excessively hard, and require significantly more time investment than adjacent challenges?
-   Are there any challenges that are disproportionately popular, considering their later position within our curriculum?
-   Which challenges support the most diverse solutions?
-   Are campers more likely to use Object Oriented Programming solutions or Functional Programming solutions?

That's certainly a good place to start. I don't think I'll get to answer all of those in this project, but I'll answer some.

Time to look at the data!

### Importing Necessary Packages

``` r
# For importing JSON data
library(tidyjson)
library(jsonlite)
library(data.table)

# For data manipulation and tidying
library(dplyr)
library(tidyr)
library(Hmisc)

# For data visualizations
library(ggplot2)
library(highcharter)
library(igraph)
library(networkD3)
library(htmlwidgets)

# For web scraping
library(rvest)
library(curl)

# For machine learning
library(caret)
library(ranger)
library(e1071)
```

Data Exploration
----------------

### Importing Dataset

The data are freely available from FCC and can be downloaded [here](http://academictorrents.com/details/030b10dad0846b5aecc3905692890fb02404adbf). I'm going to import the data using the`jsonlite`, `tidyjson` and `data.table` packages.

*Note: my poor little laptop couldn't handle importing/binding these files. I am lucky enough that [Parker](http://www.animoplex.com) let me use his animation rendering machine (i.e. work-horse in computer form) to complete this project.*

``` r
# Import JSON using the jsonlite package
fcc <- fromJSON("output.json", flatten = TRUE)
```

Now we have a list of data frames. Time to bind them into a giant data frame using the `data.table` package.

``` r
fcc_df <- rbindlist(fcc, fill = TRUE, idcol = TRUE)
```

Last thing is to convert the data.table to a data.frame (this one is in base R).

``` r
fcc_df2 <- as.data.frame(fcc_df)
```

### Data Structure and Variables

Great! Now that our data is imported, we can get a look at it. I'll start by looking at it's structure.

``` r
str(fcc_df2)
```

    ## 'data.frame':    7236580 obs. of  4 variables:
    ##  $ .id          : int  1 1 1 1 1 1 1 1 1 1 ...
    ##  $ name         : chr  "Waypoint: Learn how Free Code Camp Works" "Waypoint: Learn how Free Code Camp Works" "Waypoint: Preview our Challenge Map" "Waypoint: Browse our Field Guide" ...
    ##  $ completedDate: chr  "1434650566196" "1434650582665" "1434651100007" "1434651311734" ...
    ##  $ solution     : chr  NA NA NA NA ...

Ok, so as expected, we are working with a data frame with 4 variables and **7,236,580** observations! No wonder my laptop couldn't handle it.

Our 4 variables are:

-   **.id** : The ID number created when binding our list of data frames into a single data frame. In this case, it corresponds to FCC user number.
-   **name** : The FCC course completed
-   **completedDate** : The date the course was completed (as a Unix timestamp in milliseconds)
-   **solution** :The user's solution to the course challenge.

While a character string is fine for the "solution" column, I'm going to change the data type of the name, id, and completedDate columns. Name and id should both be factors.

``` r
fcc_df2$name <- as.factor(fcc_df2$name)
fcc_df2$.id <- as.factor(fcc_df2$.id)
```

How many users are we dealing with?

``` r
fcc_df2 %>% summarise(n_distinct(.id))
```

    ##   n_distinct(.id)
    ## 1          103091

103,091 different users!

How many challenges are there? And do we have any duplicates?

``` r
levels(fcc_df2$name)
```

    ##   [1] ""                                                                          
    ##   [2] ": Use the Javascript Console"                                              
    ##   [3] ": Using typeof"                                                            
    ##   [4] "Apply Angular.js Directives"                                               
    ##   [5] "Arguments Optional"                                                        
    ##   [6] "Basejump: Build a Nightlife Coordination App"                              
    ##   [7] "Basejump: Build a Pinterest Clone"                                         
    ##   [8] "Basejump: Build a Pintrest Clone"                                          
    ##   [9] "Basejump: Build a Voting App"                                              
    ##  [10] "Basejump: Chart the Stock Market"                                          
    ##  [11] "Basejump: Manage a Book Trading Club"                                      
    ##  [12] "Basejumps: Build a Nightlife Coordination App"                             
    ##  [13] "Basejumps: Build a Pinterest Clone"                                        
    ##  [14] "Basejumps: Build a Voting App"                                             
    ##  [15] "Basejumps: Chart the Stock Market"                                         
    ##  [16] "Basejumps: Manage a Book Trading Club"                                     
    ##  [17] "Binary Agents"                                                             
    ##  [18] "Bonfire: Arguments Optional"                                               
    ##  [19] "Bonfire: Binary Agents"                                                    
    ##  [20] "Bonfire: Boo who"                                                          
    ##  [21] "Bonfire: Check for Palindromes"                                            
    ##  [22] "Bonfire: Chunky Monkey"                                                    
    ##  [23] "Bonfire: Confirm the Ending"                                               
    ##  [24] "Bonfire: Convert HTML Entities"                                            
    ##  [25] "Bonfire: Diff Two Arrays"                                                  
    ##  [26] "Bonfire: DNA Pairing"                                                      
    ##  [27] "Bonfire: Drop it"                                                          
    ##  [28] "Bonfire: Everything Be True"                                               
    ##  [29] "Bonfire: Exact Change"                                                     
    ##  [30] "Bonfire: Factorialize a Number"                                            
    ##  [31] "Bonfire: Falsey Bouncer"                                                   
    ##  [32] "Bonfire: Falsy Bouncer"                                                    
    ##  [33] "Bonfire: Find the Longest Word in a String"                                
    ##  [34] "Bonfire: Finders Keepers"                                                  
    ##  [35] "Bonfire: Friendly Date Ranges"                                             
    ##  [36] "Bonfire: Inventory Update"                                                 
    ##  [37] "Bonfire: Make a Person"                                                    
    ##  [38] "Bonfire: Map the Debris"                                                   
    ##  [39] "Bonfire: Meet Bonfire"                                                     
    ##  [40] "Bonfire: Missing letters"                                                  
    ##  [41] "Bonfire: Mutations"                                                        
    ##  [42] "Bonfire: No repeats please"                                                
    ##  [43] "Bonfire: Pairwise"                                                         
    ##  [44] "Bonfire: Pig Latin"                                                        
    ##  [45] "Bonfire: Repeat a string repeat a string"                                  
    ##  [46] "Bonfire: Return Largest Numbers in Arrays"                                 
    ##  [47] "Bonfire: Reverse a String"                                                 
    ##  [48] "Bonfire: Roman Numeral Converter"                                          
    ##  [49] "Bonfire: Search and Replace"                                               
    ##  [50] "Bonfire: Seek and Destroy"                                                 
    ##  [51] "Bonfire: Slasher Flick"                                                    
    ##  [52] "Bonfire: Smallest Common Multiple"                                         
    ##  [53] "Bonfire: Sorted Union"                                                     
    ##  [54] "Bonfire: Spinal Tap Case"                                                  
    ##  [55] "Bonfire: Steamroller"                                                      
    ##  [56] "Bonfire: Sum All Numbers in a Range"                                       
    ##  [57] "Bonfire: Sum All Odd Fibonacci Numbers"                                    
    ##  [58] "Bonfire: Sum All Primes"                                                   
    ##  [59] "Bonfire: Symmetric Difference"                                             
    ##  [60] "Bonfire: Title Case a Sentence"                                            
    ##  [61] "Bonfire: Truncate a string"                                                
    ##  [62] "Bonfire: Validate US Telephone Numbers"                                    
    ##  [63] "Bonfire: Where art thou"                                                   
    ##  [64] "Bonfire: Where do I belong"                                                
    ##  [65] "Boo who"                                                                   
    ##  [66] "Browse our Field Guide"                                                    
    ##  [67] "Build a Cash Register"                                                     
    ##  [68] "Build a Contact List"                                                      
    ##  [69] "Build a Geolocation Angular App"                                           
    ##  [70] "Build a Landing Page with HTML"                                            
    ##  [71] "Build an Address Book"                                                     
    ##  [72] "Build an Adventure Game"                                                   
    ##  [73] "Build Rock Paper Scissors"                                                 
    ##  [74] "Build Web Apps with Express.js"                                            
    ##  [75] "Cash Register"                                                             
    ##  [76] "Check for Palindromes"                                                     
    ##  [77] "Chunky Monkey"                                                             
    ##  [78] "Confirm the Ending"                                                        
    ##  [79] "Convert HTML Entities"                                                     
    ##  [80] "Create Angular.js Services"                                                
    ##  [81] "Customize Angular.js Directives"                                           
    ##  [82] "Customize your Porfolio Page"                                              
    ##  [83] "Customize your Portfolio Page"                                             
    ##  [84] "Design a Layout with HTML"                                                 
    ##  [85] "Design Responsively with Bootstrap"                                        
    ##  [86] "Diff Two Arrays"                                                           
    ##  [87] "Discover Chrome's DevTools"                                                
    ##  [88] "DNA Pairing"                                                               
    ##  [89] "Drop it like it's hot"                                                     
    ##  [90] "Everything Be True"                                                        
    ##  [91] "Factorialize a Number"                                                     
    ##  [92] "Falsey Bouncer"                                                            
    ##  [93] "Find the Longest Word in a String"                                         
    ##  [94] "Finders Keepers"                                                           
    ##  [95] "Friendly Date Ranges"                                                      
    ##  [96] "Get Help the Hacker Way with RSAP"                                         
    ##  [97] "Get Set for Basejumps"                                                     
    ##  [98] "Get Set for Ziplines"                                                      
    ##  [99] "Get Started with Angular.js"                                               
    ## [100] "Get Started with jQuery"                                                   
    ## [101] "Harness Dynamic HTML"                                                      
    ## [102] "Inventory Update"                                                          
    ## [103] "Join Our Chat Room"                                                        
    ## [104] "Learn Basic Computer Science"                                              
    ## [105] "Learn Boolean Logic"                                                       
    ## [106] "Learn Computer Hardware"                                                   
    ## [107] "Learn Computer Networking"                                                 
    ## [108] "Learn Computer Security"                                                   
    ## [109] "Learn Control Flow"                                                        
    ## [110] "Learn how Free Code Camp Works"                                            
    ## [111] "Learn JavaScript For Loops"                                                
    ## [112] "Learn JavaScript While Loops"                                              
    ## [113] "Learn Loops"                                                               
    ## [114] "Learn Regular Expressions"                                                 
    ## [115] "Listen for jQuery Events"                                                  
    ## [116] "Make a Person"                                                             
    ## [117] "Manage Packages with NPM"                                                  
    ## [118] "Manage Source Code with Git"                                               
    ## [119] "Map the Debris"                                                            
    ## [120] "Meet Bonfire"                                                              
    ## [121] "Meet Other Campers in your City"                                           
    ## [122] "Missing letters"                                                           
    ## [123] "Mutations"                                                                 
    ## [124] "No repeats please"                                                         
    ## [125] "Pair Program on Bonfires"                                                  
    ## [126] "Pairwise"                                                                  
    ## [127] "Pig Latin"                                                                 
    ## [128] "Power Forms with Angular.js"                                               
    ## [129] "Preview our Challenge Map"                                                 
    ## [130] "Repeat a string repeat a string"                                           
    ## [131] "Return Largest Numbers in Arrays"                                          
    ## [132] "Reverse a String"                                                          
    ## [133] "Roman Numeral Converter"                                                   
    ## [134] "Search and Replace"                                                        
    ## [135] "Seek and Destroy"                                                          
    ## [136] "Slasher Flick"                                                             
    ## [137] "Smallest Common Multiple"                                                  
    ## [138] "Sorted Union"                                                              
    ## [139] "Space Out with CSS"                                                        
    ## [140] "Spinal Tap Case"                                                           
    ## [141] "Start a Node.js Server"                                                    
    ## [142] "Steamroller"                                                               
    ## [143] "Structure Angular.js Routes"                                               
    ## [144] "Style Text with CSS"                                                       
    ## [145] "Sum All Numbers in a Range"                                                
    ## [146] "Sum All Odd Fibonacci Numbers"                                             
    ## [147] "Sum All Primes"                                                            
    ## [148] "Symmetric Difference"                                                      
    ## [149] "Title Case a Sentence"                                                     
    ## [150] "Trigger jQuery Effects"                                                    
    ## [151] "Truncate a string"                                                         
    ## [152] "Try Camper News"                                                           
    ## [153] "Validate US Telephone Numbers"                                             
    ## [154] "Waypoint: Access Array Data with Indexes"                                  
    ## [155] "Waypoint: Add a Negative Margin to an Element"                             
    ## [156] "Waypoint: Add a Submit Button to a Form"                                   
    ## [157] "Waypoint: Add Alt Text to an Image for Accessibility"                      
    ## [158] "Waypoint: Add Borders Around your Elements"                                
    ## [159] "Waypoint: Add Different a Margin to Each Side of an Element"               
    ## [160] "Waypoint: Add Different Margins to Each Side of an Element"                
    ## [161] "Waypoint: Add Different Padding to Each Side of an Element"                
    ## [162] "Waypoint: Add Elements within your Bootstrap Wells"                        
    ## [163] "Waypoint: Add Font Awesome Icons all of our Buttons"                       
    ## [164] "Waypoint: Add Font Awesome Icons to all of our Buttons"                    
    ## [165] "Waypoint: Add Font Awesome Icons to our Buttons"                           
    ## [166] "Waypoint: Add Free Code Camp to your LinkedIn Profile"                     
    ## [167] "Waypoint: Add ID Attributes to Bootstrap Elements"                         
    ## [168] "Waypoint: Add Images to your Website"                                      
    ## [169] "Waypoint: Add New Properties to a JavaScript Object"                       
    ## [170] "Waypoint: Add Placeholder Text to a Text Field"                            
    ## [171] "Waypoint: Add Rounded Corners with a Border Radius"                        
    ## [172] "Waypoint: Add Two Numbers with JavaScript"                                 
    ## [173] "Waypoint: Add your JavaScript Slot Machine Slots"                          
    ## [174] "Waypoint: Adjust the Margin of an Element"                                 
    ## [175] "Waypoint: Adjusting the Padding of an Element"                             
    ## [176] "Waypoint: Apply Angular.js Directives"                                     
    ## [177] "Waypoint: Apply Angularjs Directives"                                      
    ## [178] "Waypoint: Apply the Default Bootstrap Button Style"                        
    ## [179] "Waypoint: Bring your JavaScript Slot Machine to Life"                      
    ## [180] "Waypoint: Browse Camper News"                                              
    ## [181] "Waypoint: Browse our Field Guide"                                          
    ## [182] "Waypoint: Build a Cash Register"                                           
    ## [183] "Waypoint: Build a Contact List"                                            
    ## [184] "Waypoint: Build an Address Book"                                           
    ## [185] "Waypoint: Build an Adventure Game"                                         
    ## [186] "Waypoint: Build JavaScript Objects"                                        
    ## [187] "Waypoint: Build Objects with Functional Classes"                           
    ## [188] "Waypoint: Build Objects with Prototypal Classes"                           
    ## [189] "Waypoint: Build Rock Paper Scissors"                                       
    ## [190] "Waypoint: Build Web Apps with Express.js"                                  
    ## [191] "Waypoint: Build Web Apps with Expressjs"                                   
    ## [192] "Waypoint: Call out Optional Actions with Button Info"                      
    ## [193] "Waypoint: Center Text with Bootstrap"                                      
    ## [194] "Waypoint: Change Text Inside an Element Using jQuery"                      
    ## [195] "Waypoint: Change Text with Click Events"                                   
    ## [196] "Waypoint: Change the Color of Text"                                        
    ## [197] "Waypoint: Change the CSS of an Element Using jQuery"                       
    ## [198] "Waypoint: Change the Font Size of an Element"                              
    ## [199] "Waypoint: Check Radio Buttons and Checkboxes by Default"                   
    ## [200] "Waypoint: Check the Length Property of a String Variable"                  
    ## [201] "Waypoint: Claim Your Back End Development Certificate"                     
    ## [202] "Waypoint: Claim Your Front End Development Certificate"                    
    ## [203] "Waypoint: Claim Your Full Stack Development Certificate"                   
    ## [204] "Waypoint: Clean up your form using Linebreaks"                             
    ## [205] "Waypoint: Clone an Element Using jQuery"                                   
    ## [206] "Waypoint: Comment out HTML"                                                
    ## [207] "Waypoint: Comment your JavaScript Code"                                    
    ## [208] "Waypoint: Commit to a Goal and a Nonprofit"                                
    ## [209] "Waypoint: Concatenate Arrays with concat"                                  
    ## [210] "Waypoint: Concatenate Strings with concat"                                 
    ## [211] "Waypoint: Condense arrays with reduce"                                     
    ## [212] "Waypoint: Configure your Code Portfolio"                                   
    ## [213] "Waypoint: Configure your Public Profile"                                   
    ## [214] "Waypoint: Construct JavaScript Objects with Functions"                     
    ## [215] "Waypoint: Constructing JavaScript Objects with Functions"                  
    ## [216] "Waypoint: Continue working with Node.js Servers"                           
    ## [217] "Waypoint: Continue working with Nodejs Servers"                            
    ## [218] "Waypoint: Convert JSON Data to HTML"                                       
    ## [219] "Waypoint: Count Backwards With a For Loop"                                 
    ## [220] "Waypoint: Create a Block Element Bootstrap Button"                         
    ## [221] "Waypoint: Create a Bootstrap Button"                                       
    ## [222] "Waypoint: Create a Bootstrap Headline"                                     
    ## [223] "Waypoint: Create a Bootstrap Row"                                          
    ## [224] "Waypoint: Create a Bulleted Unordered List"                                
    ## [225] "Waypoint: Create a Class to Target with jQuery Selectors"                  
    ## [226] "Waypoint: Create a Custom Heading"                                         
    ## [227] "Waypoint: Create a Form Element"                                           
    ## [228] "Waypoint: Create a GitHub Account and Join our Chat Rooms"                 
    ## [229] "Waypoint: Create a JavaScript Slot Machine"                                
    ## [230] "Waypoint: Create a Set of Checkboxes"                                      
    ## [231] "Waypoint: Create a Set of Radio Buttons"                                   
    ## [232] "Waypoint: Create a Text Field"                                             
    ## [233] "Waypoint: Create an Ordered List"                                          
    ## [234] "Waypoint: Create Angular.js Services"                                      
    ## [235] "Waypoint: Create Angularjs Services"                                       
    ## [236] "Waypoint: Create Bootstrap Wells"                                          
    ## [237] "Waypoint: Create Decimal Numbers with JavaScript"                          
    ## [238] "Waypoint: Customize Angular.js Directives"                                 
    ## [239] "Waypoint: Customize Angularjs Directives"                                  
    ## [240] "Waypoint: Customize your Portfolio Page"                                   
    ## [241] "Waypoint: Declare JavaScript Objects as Variables"                         
    ## [242] "Waypoint: Declare JavaScript Variables"                                    
    ## [243] "Waypoint: Declare String Variables"                                        
    ## [244] "Waypoint: Declaring JavaScript Objects as Variables"                       
    ## [245] "Waypoint: Delete HTML Elements"                                            
    ## [246] "Waypoint: Delete Properties from a JavaScript Object"                      
    ## [247] "Waypoint: Delete your jQuery Functions"                                    
    ## [248] "Waypoint: Disable an Element Using jQuery"                                 
    ## [249] "Waypoint: Discover Chrome DevTools"                                        
    ## [250] "Waypoint: Ditch Custom CSS for Bootstrap"                                  
    ## [251] "Waypoint: Divide one Decimal by Another with JavaScript"                   
    ## [252] "Waypoint: Divide One Number by Another with JavaScript"                    
    ## [253] "Waypoint: Duplicate Instances of Objects from a Constructor Function"      
    ## [254] "Waypoint: Fill in the Blank with Placeholder Text"                         
    ## [255] "Waypoint: Filter Arrays with filter"                                       
    ## [256] "Waypoint: Find Numbers with Regular Expressions"                           
    ## [257] "Waypoint: Find White Space with Regular Expressions"                       
    ## [258] "Waypoint: Find Whitespace with Regular Expressions"                        
    ## [259] "Waypoint: Finish working with Node.js Servers"                             
    ## [260] "Waypoint: Finish working with Nodejs Servers"                              
    ## [261] "Waypoint: Generate Random Fractions with JavaScript"                       
    ## [262] "Waypoint: Generate Random Whole Numbers with JavaScript"                   
    ## [263] "Waypoint: Generate Random Whole Numbers within a Range"                    
    ## [264] "Waypoint: Get Geolocation Data"                                            
    ## [265] "Waypoint: Get Help the Hacker Way with RSAP"                               
    ## [266] "Waypoint: Get JSON with the jQuery getJSON Method"                         
    ## [267] "Waypoint: Get Set for Basejumps"                                           
    ## [268] "Waypoint: Get Set for Bonfires"                                            
    ## [269] "Waypoint: Get Set for Ziplines"                                            
    ## [270] "Waypoint: Get Started with Angular.js"                                     
    ## [271] "Waypoint: Get Started with Angularjs"                                      
    ## [272] "Waypoint: Get Started with jQuery"                                         
    ## [273] "Waypoint: Give a Background Color to a Div Element"                        
    ## [274] "Waypoint: Give Each Element a Unique ID"                                   
    ## [275] "Waypoint: Give your JavaScript Slot Machine some stylish images"           
    ## [276] "Waypoint: Give your JavaScript Slot Machine some Stylish Images"           
    ## [277] "Waypoint: Harness Dynamic HTML"                                            
    ## [278] "Waypoint: Headline with the h2 Element"                                    
    ## [279] "Waypoint: House our page within a Bootstrap Container Fluid Div"           
    ## [280] "Waypoint: Import a Google Font"                                            
    ## [281] "Waypoint: Inform with the Paragraph Element"                               
    ## [282] "Waypoint: Inherit Styles from the Body Element"                            
    ## [283] "Waypoint: Invert Regular Expression Matches with JavaScript"               
    ## [284] "Waypoint: Iterate Odd Numbers With a For Loop"                             
    ## [285] "Waypoint: Iterate over Arrays with map"                                    
    ## [286] "Waypoint: Iterate with JavaScript For Loops"                               
    ## [287] "Waypoint: Iterate with JavaScript While Loops"                             
    ## [288] "Waypoint: Join a Campsite in Your City"                                    
    ## [289] "Waypoint: Join our Alumni Network and Commit to Your Goal"                 
    ## [290] "Waypoint: Join Our Chat Room"                                              
    ## [291] "Waypoint: Join our LinkedIn Alumni Network"                                
    ## [292] "Waypoint: Join Strings with join"                                          
    ## [293] "Waypoint: Label Bootstrap Buttons"                                         
    ## [294] "Waypoint: Label Bootstrap Wells"                                           
    ## [295] "Waypoint: Learn Basic Computer Science"                                    
    ## [296] "Waypoint: Learn Boolean Logic"                                             
    ## [297] "Waypoint: Learn Computer Hardware"                                         
    ## [298] "Waypoint: Learn Computer Networking"                                       
    ## [299] "Waypoint: Learn Computer Security"                                         
    ## [300] "Waypoint: Learn Control Flow"                                              
    ## [301] "Waypoint: Learn how Free Code Camp Works"                                  
    ## [302] "Waypoint: Learn how Script Tags and Document Ready Work"                   
    ## [303] "Waypoint: Learn JavaScript For Loops"                                      
    ## [304] "Waypoint: Learn JavaScript While Loops"                                    
    ## [305] "Waypoint: Learn Loops"                                                     
    ## [306] "Waypoint: Learn Regular Expressions"                                       
    ## [307] "Waypoint: Learn What to Do If You Get Stuck"                               
    ## [308] "Waypoint: Line up Form Elements Responsively with Bootstrap"               
    ## [309] "Waypoint: Link to External Pages with Anchor Elements"                     
    ## [310] "Waypoint: Listen for jQuery Events"                                        
    ## [311] "Waypoint: Make Circular Images with a Border Radius"                       
    ## [312] "Waypoint: Make Dead Links using the Hash Symbol"                           
    ## [313] "Waypoint: Make Images Mobile Responsive"                                   
    ## [314] "Waypoint: Make Instances of Objects with a Constructor Function"           
    ## [315] "Waypoint: Make Object Properties Private"                                  
    ## [316] "Waypoint: Make Unique Objects by Passing Parameters to our Constructor"    
    ## [317] "Waypoint: Manage Packages with NPM"                                        
    ## [318] "Waypoint: Manage Source Code with Git"                                     
    ## [319] "Waypoint: Manipulate Arrays With pop"                                      
    ## [320] "Waypoint: Manipulate Arrays With push"                                     
    ## [321] "Waypoint: Manipulate Arrays With shift"                                    
    ## [322] "Waypoint: Manipulate Arrays With unshift"                                  
    ## [323] "Waypoint: Manipulate JavaScript Objects"                                   
    ## [324] "Waypoint: Meet Other Campers in your City"                                 
    ## [325] "Waypoint: Mobile Responsive Images"                                        
    ## [326] "Waypoint: Modify Array Data With Indexes"                                  
    ## [327] "Waypoint: Multiply Two Decimals with JavaScript"                           
    ## [328] "Waypoint: Multiply Two Numbers with JavaScript"                            
    ## [329] "Waypoint: Nest an Anchor Element within a Paragraph"                       
    ## [330] "Waypoint: Nest Many Elements within a Single Div Element"                  
    ## [331] "Waypoint: Nest one Array within Another Array"                             
    ## [332] "Waypoint: Override All Other Styles by using Important"                    
    ## [333] "Waypoint: Override Class Declarations by Styling ID Attributes"            
    ## [334] "Waypoint: Override Class Declarations with Inline Styles"                  
    ## [335] "Waypoint: Override Styles in Subsequent CSS"                               
    ## [336] "Waypoint: Override Styles with Important"                                  
    ## [337] "Waypoint: Pair Program on Bonfires"                                        
    ## [338] "Waypoint: Perform Arithmetic Operations on Decimals with JavaScript"       
    ## [339] "Waypoint: Power Forms with Angular.js"                                     
    ## [340] "Waypoint: Power Forms with Angularjs"                                      
    ## [341] "Waypoint: Practice Functional Programming"                                 
    ## [342] "Waypoint: Prefilter JSON"                                                  
    ## [343] "Waypoint: Preview our Challenge Map"                                       
    ## [344] "Waypoint: Prioritize One Style Over Another"                               
    ## [345] "Waypoint: Reference our Wiki"                                              
    ## [346] "Waypoint: Reference your Current Object with This"                         
    ## [347] "Waypoint: Remove an Element Using jQuery"                                  
    ## [348] "Waypoint: Remove Classes from an element with jQuery"                      
    ## [349] "Waypoint: Render Images from Data Sources"                                 
    ## [350] "Waypoint: Responsively Style a Radio Buttons"                              
    ## [351] "Waypoint: Responsively Style Checkboxes"                                   
    ## [352] "Waypoint: Responsively Style Radio Buttons"                                
    ## [353] "Waypoint: Reuse Code with Decorators"                                      
    ## [354] "Waypoint: Reverse Arrays with reverse"                                     
    ## [355] "Waypoint: Save your Code Revisions Forever with Git"                       
    ## [356] "Waypoint: Say Hello to HTML Elements"                                      
    ## [357] "Waypoint: Scope Your Variables"                                            
    ## [358] "Waypoint: Set the Font Family of an Element"                               
    ## [359] "Waypoint: Set the ID of an Element"                                        
    ## [360] "Waypoint: Sift through Text with Regular Expressions"                      
    ## [361] "Waypoint: Size your Images"                                                
    ## [362] "Waypoint: Sort Arrays with sort"                                           
    ## [363] "Waypoint: Specify How Fonts Should Degrade"                                
    ## [364] "Waypoint: Split Strings with split"                                        
    ## [365] "Waypoint: Split your Bootstrap Row"                                        
    ## [366] "Waypoint: Start a Node.js Server"                                          
    ## [367] "Waypoint: Start a Nodejs Server"                                           
    ## [368] "Waypoint: Store Data in MongoDB"                                           
    ## [369] "Waypoint: Store Multiple Values in one Variable using JavaScript Arrays"   
    ## [370] "Waypoint: Style Multiple Elements with a CSS Class"                        
    ## [371] "Waypoint: Style Multiple Elements with a CSS Classes"                      
    ## [372] "Waypoint: Style Text Inputs as Form Controls"                              
    ## [373] "Waypoint: Style the HTML Body Element"                                     
    ## [374] "Waypoint: Subclass one Object to Another"                                  
    ## [375] "Waypoint: Subtract One Number from Another with JavaScript"                
    ## [376] "Waypoint: Target a Specific Child of an Element Using jQuery"              
    ## [377] "Waypoint: Target Elements by Class Using jQuery"                           
    ## [378] "Waypoint: Target Elements by ID Using jQuery"                              
    ## [379] "Waypoint: Target Even Numbered Elements Using jQuery"                      
    ## [380] "Waypoint: Target HTML Elements with Selectors Using jQuery"                
    ## [381] "Waypoint: Target the Children of an Element Using jQuery"                  
    ## [382] "Waypoint: Target the Parent of an Element Using jQuery"                    
    ## [383] "Waypoint: Target the same element with multiple jQuery Selectors"          
    ## [384] "Waypoint: Taste the Bootstrap Button Color Rainbow"                        
    ## [385] "Waypoint: Traverse the Prototype Chain"                                    
    ## [386] "Waypoint: Trigger Click Events with jQuery"                                
    ## [387] "Waypoint: Trigger jQuery Effects"                                          
    ## [388] "Waypoint: Trigger on click Events with jQuery"                             
    ## [389] "Waypoint: Try Camper News"                                                 
    ## [390] "Waypoint: Turn an Image into a Link"                                       
    ## [391] "Waypoint: Uncomment HTML"                                                  
    ## [392] "Waypoint: Understand Boolean Values"                                       
    ## [393] "Waypoint: Understand Pseudoclassical Patterns"                             
    ## [394] "Waypoint: Understanding Public and Private Properties"                     
    ## [395] "Waypoint: Update the Properties of a JavaScript Object"                    
    ## [396] "Waypoint: Use a CSS Class to Style an Element"                             
    ## [397] "Waypoint: Use Abbreviated Hex Code"                                        
    ## [398] "Waypoint: Use an ID Attribute to Style an Element"                         
    ## [399] "Waypoint: Use appendTo to Move Elements with jQuery"                       
    ## [400] "Waypoint: Use Bracket Notation to Find the First Character in a String"    
    ## [401] "Waypoint: Use Bracket Notation to Find the Last Character in a String"     
    ## [402] "Waypoint: Use Bracket Notation to Find the Nth Character in a String"      
    ## [403] "Waypoint: Use Bracket Notation to Find the NthtoLast Character in a String"
    ## [404] "Waypoint: Use Clockwise Notation to Specify the Margin of an Element"      
    ## [405] "Waypoint: Use Clockwise Notation to Specify the Padding of an Element"     
    ## [406] "Waypoint: Use Comments to Clarify Code"                                    
    ## [407] "Waypoint: Use Conditional Logic with If and Else Statements"               
    ## [408] "Waypoint: Use Conditional Logic with IfElse Statements"                    
    ## [409] "Waypoint: Use CSS Selectors to Style Elements"                             
    ## [410] "Waypoint: Use Hex Code for Specific Colors"                                
    ## [411] "Waypoint: Use Hex Code for Specific Shades of Gray"                        
    ## [412] "Waypoint: Use Hex Code to Color Elements Blue"                             
    ## [413] "Waypoint: Use Hex Code to Color Elements Gray"                             
    ## [414] "Waypoint: Use Hex Code to Color Elements Green"                            
    ## [415] "Waypoint: Use Hex Code to Color Elements Red"                              
    ## [416] "Waypoint: Use Hex Code to Color Elements White"                            
    ## [417] "Waypoint: Use Hex Code to Mix Colors"                                      
    ## [418] "Waypoint: Use HTML5 to Require a Field"                                    
    ## [419] "Waypoint: Use jQuery to Modify the Entire Page"                            
    ## [420] "Waypoint: Use Pseudoclassical Subclasses"                                  
    ## [421] "Waypoint: Use Responsive Design with Bootstrap Fluid Containers"           
    ## [422] "Waypoint: Use RGB to Color Elements Blue"                                  
    ## [423] "Waypoint: Use RGB to Color Elements Gray"                                  
    ## [424] "Waypoint: Use RGB to Color Elements Green"                                 
    ## [425] "Waypoint: Use RGB to Color Elements Red"                                   
    ## [426] "Waypoint: Use RGB to Color Elements White"                                 
    ## [427] "Waypoint: Use RGB to Mix Colors"                                           
    ## [428] "Waypoint: Use RGB values to Color Elements"                                
    ## [429] "Waypoint: Use Spans for Inline Elements"                                   
    ## [430] "Waypoint: Use the Bootstrap Grid to Put Elements Side By Side"             
    ## [431] "Waypoint: Use the Javascript Console"                                      
    ## [432] "Waypoint: Using concat"                                                    
    ## [433] "Waypoint: Using filter"                                                    
    ## [434] "Waypoint: Using join"                                                      
    ## [435] "Waypoint: Using map"                                                       
    ## [436] "Waypoint: Using reduce"                                                    
    ## [437] "Waypoint: Using reverse"                                                   
    ## [438] "Waypoint: Using sort"                                                      
    ## [439] "Waypoint: Using split"                                                     
    ## [440] "Waypoint: Using typeof"                                                    
    ## [441] "Waypoint: Visually Separate Elements with Line Breaks"                     
    ## [442] "Waypoint: Warn your Users of a Dangerous Action"                           
    ## [443] "Waypoint: Wrap an Anchor Element within a Paragraph"                       
    ## [444] "Waypoint: Wrap Many Elements within a Div Element"                         
    ## [445] "Waypoint: Wrap Many Elements within a Single Div Element"                  
    ## [446] "Waypoint: Write Functions with jQuery"                                     
    ## [447] "Waypoint: Write Reusable JavaScript with Functions"                        
    ## [448] "Where art thou"                                                            
    ## [449] "Where do I belong"                                                         
    ## [450] "Write Functions with jQuery"                                               
    ## [451] "Zipline: Build a JavaScript Calculator"                                    
    ## [452] "Zipline: Build a Personal Portfolio Webpage"                               
    ## [453] "Zipline: Build a Pomodoro Clock"                                           
    ## [454] "Zipline: Build a Random Quote Machine"                                     
    ## [455] "Zipline: Build a Simon Game"                                               
    ## [456] "Zipline: Build a Tic Tac Toe Game"                                         
    ## [457] "Zipline: Build a Wikipedia Viewer"                                         
    ## [458] "Zipline: Show the Local Weather"                                           
    ## [459] "Zipline: Stylize Stories on Camper News"                                   
    ## [460] "Zipline: Use the Twitch.tv JSON API"                                       
    ## [461] "Zipline: Use the Twitchtv JSON API"                                        
    ## [462] "Zipline: Wikipedia Viewer"

Wow! So 462 challenges. A quick skim of the levels indicate that these data are pretty clean but it's hard to tell if there are duplicates. There are definitely some NA values, and some labelled as "". I'm going to remove those from this dataset.

``` r
fcc_df3 <- fcc_df2 %>% filter(!is.na(name)) %>% filter(!name == 
    "") %>% droplevels()

# Validate that there are no more NA course names
sum(is.na(fcc_df3$name))
```

    ## [1] 0

I'll deal more with the course names a bit later. One more thing before moving on; I'd like to separate the "name" column into "type" and course, splitting the course name at the ":"

``` r
# Subset data frame to only include student entries where the
# course name has a ':' and separate course name into 2
# columns: type and course
fcc_sub1 <- fcc_df3 %>% filter(grepl("\\:\\s", name)) %>% separate(col = name, 
    into = c("type", "course"), sep = ": ")

# Subset data frame to only include student entries where
# course name does not have a ':'. Add a new 'type' column
# filled with NA
fcc_sub2 <- fcc_df3 %>% filter(!grepl("\\:\\s", name)) %>% mutate(type = NA)

# Rename the 'name' variable in the second subset to 'course'
# (to match first subset)
fcc_sub2 <- rename(fcc_sub2, course = name)

# Bind subsets together
fcc_all <- rbind(fcc_sub1, fcc_sub2)

# Make both 'course' and 'type' factors
fcc_all$course <- as.factor(fcc_all$course)
fcc_all$type <- as.factor(fcc_all$type)

# Check structure of 'course'
str(fcc_all$course)
```

    ##  Factor w/ 370 levels "Access Array Data with Indexes",..: 187 187 238 31 110 306 214 150 255 163 ...

Perfect! That will make dealing with course names easier in the future, and it looks like it trimmed us down to 370 courses (some of them must have had duplicate names).

While we're at it, let's clean up our completedDate variable; it should be a POSIXct object.

``` r
# Convert characters to numeric
fcc_all$completedDate <- as.numeric(fcc_all$completedDate)

# Divide by 1000 to account for the numbers being in
# milliseconds
fcc_all$completedDate <- fcc_all[, "completedDate"]/1000

# Convert to POSIXct object
fcc_all$completedDate <- as.POSIXct((fcc_all$completedDate), 
    origin = "1970-01-01", tz = "GMT")
```

Perfect. Were all the dates coded correctly?

Let's see if any entries are recorded as being completed before FCC launched in October 2014.

``` r
wrong_dates <- fcc_all %>% 
			filter(completedDate < "2014-10-01")
```

Oops! 19 of our entries have incorrectly coded dates. We'll remove those from the dataset.

``` r
fcc_all <- fcc_all %>% 
	       filter(completedDate > "2014-10-01")
```

Do we have any NA values in our completedDate column? If so, we'll remove those as well.

``` r
# Count NA
sum(is.na(fcc_all$completedDate))
```

    ## [1] 0

``` r
# Remove NA
fcc_all <- fcc_all %>% 
	       filter(!is.na(completedDate))
```

Ok, so far so good. Let's take another quick look at our structure.

``` r
str(fcc_all)
```

    ## 'data.frame':    7216723 obs. of  5 variables:
    ##  $ .id          : Factor w/ 102882 levels "1","2","3","4",..: 1 1 1 1 1 1 1 1 1 1 ...
    ##  $ type         : Factor w/ 6 levels "","Basejump",..: 5 5 5 5 5 5 5 5 5 5 ...
    ##  $ course       : Factor w/ 370 levels "Access Array Data with Indexes",..: 187 187 238 31 110 306 214 150 255 163 ...
    ##  $ completedDate: POSIXct, format: "2015-06-18 18:02:46" "2015-06-18 18:03:02" ...
    ##  $ solution     : chr  NA NA NA NA ...

That looks much better!

Feature Engineering
-------------------

### Using Original Dataset

There are a few things that I'm interested in that aren't readily available from this dataset.

-   What order did each student complete the courses in?
-   How long did it take for each student to finish one course and then complete another?
-   Which class did each student take *before* the current course and which did they take *after*?
-   Did students generally take FCC classes in the order that they are offered?
-   Are the most frequently completed courses the ones offered at the beginning?

We'll look at one student first:

*I chose the second student because that student only completed 25 courses, a much more reasonable starting-point than the 226 completed by student \#1*

``` r
second_student <- fcc_all %>% 
			     filter(.id == "2")

second_student
```

    ##    .id type                             course       completedDate
    ## 1    2 <NA>     Learn how Free Code Camp Works 2015-05-21 17:44:57
    ## 2    2 <NA>                 Join Our Chat Room 2015-05-21 17:58:10
    ## 3    2 <NA>          Preview our Challenge Map 2015-05-21 18:45:44
    ## 4    2 <NA>             Browse our Field Guide 2015-05-21 18:49:15
    ## 5    2 <NA>      Customize your Portfolio Page 2015-05-21 19:06:19
    ## 6    2 <NA>                    Try Camper News 2015-05-21 19:22:35
    ## 7    2 <NA>    Meet Other Campers in your City 2015-05-21 19:45:10
    ## 8    2 <NA>  Get Help the Hacker Way with RSAP 2015-05-21 19:46:43
    ## 9    2 <NA>     Build a Landing Page with HTML 2015-05-22 14:40:57
    ## 10   2 <NA>                Style Text with CSS 2015-05-22 14:41:30
    ## 11   2 <NA>                 Space Out with CSS 2015-05-22 14:47:10
    ## 12   2 <NA>          Design a Layout with HTML 2015-05-22 14:49:57
    ## 13   2 <NA> Design Responsively with Bootstrap 2015-05-22 14:56:51
    ## 14   2 <NA>            Get Started with jQuery 2015-05-22 14:57:21
    ## 15   2 <NA>        Write Functions with jQuery 2015-05-22 15:10:05
    ## 16   2 <NA>               Harness Dynamic HTML 2015-05-22 15:31:01
    ## 17   2 <NA>           Listen for jQuery Events 2015-05-22 15:31:32
    ## 18   2 <NA>             Trigger jQuery Effects 2015-05-22 16:02:40
    ## 19   2 <NA>       Learn Basic Computer Science 2015-05-22 17:37:14
    ## 20   2 <NA>                        Learn Loops 2015-05-22 17:37:28
    ## 21   2 <NA>            Learn Computer Hardware 2015-05-24 22:20:35
    ## 22   2 <NA>          Learn Computer Networking 2015-05-25 22:03:14
    ## 23   2 <NA>                Learn Boolean Logic 2015-05-26 01:48:40
    ## 24   2 <NA>            Learn Computer Security 2015-05-26 02:07:25
    ## 25   2 <NA>                       Meet Bonfire 2015-05-22 00:15:04
    ##                                                                                                                                                                                                    solution
    ## 1                                                                                                                                                                                                      <NA>
    ## 2                                                                                                                                                                                                      <NA>
    ## 3                                                                                                                                                                                                      <NA>
    ## 4                                                                                                                                                                                                      <NA>
    ## 5                                                                                                                                                                                                      <NA>
    ## 6                                                                                                                                                                                                      <NA>
    ## 7                                                                                                                                                                                                      <NA>
    ## 8                                                                                                                                                                                                      <NA>
    ## 9                                                                                                                                                                                                      <NA>
    ## 10                                                                                                                                                                                                     <NA>
    ## 11                                                                                                                                                                                                     <NA>
    ## 12                                                                                                                                                                                                     <NA>
    ## 13                                                                                                                                                                                                     <NA>
    ## 14                                                                                                                                                                                                     <NA>
    ## 15                                                                                                                                                                                                     <NA>
    ## 16                                                                                                                                                                                                     <NA>
    ## 17                                                                                                                                                                                                     <NA>
    ## 18                                                                                                                                                                                                     <NA>
    ## 19                                                                                                                                                                                                     <NA>
    ## 20                                                                                                                                                                                                     <NA>
    ## 21                                                                                                                                                                                                     <NA>
    ## 22                                                                                                                                                                                                     <NA>
    ## 23                                                                                                                                                                                                     <NA>
    ## 24                                                                                                                                                                                                     <NA>
    ## 25 function meetBonfire(argument) {\n  // Good luck!\n  console.log("you can read this function's argument in the developer tools", argument);\n\n  return true;\n}\n\n\n\nmeetBonfire("You can do this!");

So the second student in our list completed 25 courses. I'm going to try extracting the information I want on this sub-sample first to ensure that it's behaving the way I'd expect before using it to generate information for 102,000 students.

``` r
second_student_2 <- second_student %>% 
		group_by(.id) %>% 
		arrange(completedDate) %>% 
    		mutate(st_cour_num = rank(completedDate), 
			    time_since_last = completedDate - lag(completedDate), 
			    from_type = lag(type), 
			    from_course = lag(course), 
        			    to_type = lead(type), 
			    to_course = lead(course))

head(second_student_2)
```

    ## Source: local data frame [6 x 11]
    ## Groups: .id [1]
    ## 
    ##      .id   type                         course       completedDate
    ##   <fctr> <fctr>                         <fctr>              <dttm>
    ## 1      2     NA Learn how Free Code Camp Works 2015-05-21 17:44:57
    ## 2      2     NA             Join Our Chat Room 2015-05-21 17:58:10
    ## 3      2     NA      Preview our Challenge Map 2015-05-21 18:45:44
    ## 4      2     NA         Browse our Field Guide 2015-05-21 18:49:15
    ## 5      2     NA  Customize your Portfolio Page 2015-05-21 19:06:19
    ## 6      2     NA                Try Camper News 2015-05-21 19:22:35
    ## # ... with 7 more variables: solution <chr>, st_cour_num <dbl>,
    ## #   time_since_last <time>, from_type <fctr>, from_course <fctr>,
    ## #   to_type <fctr>, to_course <fctr>

Great! That works just as we'd expect. Let's make sure it doesn't do anything strange when there are more than one student.

``` r
second_third <- fcc_all %>% 
			 filter(.id %in% c("2", "3")) %>% 
    			 group_by(.id) %>% 
			 arrange(completedDate) %>% 
			 mutate(st_cour_num = rank(completedDate, ties.method = "first"), 
			 	     time_since_last = completedDate - lag(completedDate), 
				     from_type = lag(type), 
				     from_course = lag(course), 
				     to_type = lead(type), 
				     to_course = lead(course))

second_third <- second_third %>% 
			 arrange(.id, st_cour_num)
```

So far so good. Let's check rows 25 - 27 to make sure our counts started over again with a new student.

``` r
second_third[25:27, c(".id", "st_cour_num")]
```

    ## Source: local data frame [3 x 2]
    ## Groups: .id [2]
    ## 
    ##      .id st_cour_num
    ##   <fctr>       <int>
    ## 1      2          25
    ## 2      3           1
    ## 3      3           2

Perfect! We will apply this to the entire data structure, but I want to modify a few more things first.

### Web Scraping New Data

I wonder if the most commonly taken courses are the ones offered at the beginning of the Free Code Camp program. To find out, I'm going to scrape the information from an archived version of the FCC course map from January 2016 (approximately 2 weeks after this dataset was released). I'll use the `rvest` package for this.

``` r
url <- "https://web.archive.org/web/20160101182240/http://www.freecodecamp.com/map#getting-started"
webpage <- read_html(curl(url, handle = curl::new_handle(useragent = "Chrome")))

course_table <- html_nodes(webpage, ".col-md-offset-2 > .row .col-md-9")
course_table2 <- html_text(course_table)
course_table2 <- as.data.frame(course_table2)
```

Now we have a list of the courses in the order that FCC has them on their map. I'm going to add a few columns to this:

-   What section of the FCC course map did this course come from?
-   What number course is this inside each section and inside the entire map?

``` r
course_table3 <- course_table2 %>% 
	mutate(section = ifelse(row_number() %in% 1:5, "Getting Started", 
ifelse(row_number() %in% 6:73, "HTML5 and CSS", 
    ifelse(row_number() %in% 74:104, "Responsive Design with Bootstrap", 
        ifelse(row_number() %in% 105:108, "Gear up for Success", 
            ifelse(row_number() %in% 109:126, "jQuery", ifelse(row_number() %in% 127:230, "Basic JavaScript", 
            ifelse(row_number() %in% 231:243, "Object Oriented and Functional Programming", 
                ifelse(row_number() %in% 244:260, "Basic Algorithm Scripting", 
                  ifelse(row_number() %in% 261:265, "Basic Front End Development Projects", 
                    ifelse(row_number() %in% 266:286, "Intermediate Algorithm Scripting", 
                      ifelse(row_number() %in% 287:293, "JSON APIs and Ajax", 
                        ifelse(row_number() %in% 294:299, "Intermediate Front End Development Projects", 
                          ifelse(row_number() == 300, "Claim Your Front End Development Certificate", 
                            ifelse(row_number() == 301, "Sass", 
                              ifelse(row_number() == 302, "React", 
                                ifelse(row_number() %in% 303:307, "React Projects", 
                                ifelse(row_number() == 308, "D3", 
                                 ifelse(row_number() %in% 309:313, "Data Visualization Projects", 
                                    ifelse(row_number() == 314, "Claim Your Data Visualization Certificate", 
                                      ifelse(row_number() %in% 315:317, "Upper Intermediate Algorithm Scripting", 
                                        ifelse(row_number() %in% 318:319, "Automated Testing and Debugging", 
                                          ifelse(row_number() %in%  320:325, "Advanced Algorithm Scripting", 
                                            ifelse(row_number() %in% 326:330, "Node.js and Express.js", 
                                              ifelse(row_number() ==  331, "Git", 
                                              ifelse(row_number() == 332, "MongoDB",  
                                              ifelse(row_number() %in% 333:338, "API Projects", 
                                                  ifelse(row_number() %in%  339:343,  "Dynamic Web Applications", 
                                                    ifelse(row_number() ==  344, "Claim Your Back End Development Certificate", 
                                                      NA))))))))))))))))))))))))))))) %>% 
    group_by(section) %>% mutate(coursenum = row_number()) %>% 
    ungroup %>% mutate(overallnum = row_number())
```

I do plan to join this data table with the student data that we already have, but currently, the names won't match. Let's clean it up a bit.

``` r
# Rename column
course_table3 <- rename(course_table3, course = course_table2)

# Remove everything after (and including) the phrase "incomplete" from course column
course_table3$course <- gsub("(.*?)Incomplete.*", "\\1", course_table3$course)

# Separate the course column into Type and Course columns. 
course_table3 <- separate(course_table3, col = course, into = c("type", "course"), sep = ": ")

# Capitalize the first letter of type column
course_table3$type <- capitalize(course_table3$type) 

# Remove trailing space from course column
course_table3$course <- gsub("\\s$", "", course_table3$course)

# Recode words that are entered differently in the original dataset
course_table4 <- course_table3 %>% 
                  mutate(course = gsub("(.*?)\\.(.*?)", "\\1\\2", course),       # Removing the "." at beginning of words
                         course = gsub("(.*?)\\(\\).*", "\\1", course),          # Removing () at the end of pop(), shift() etc.
                         course = gsub("Nth\\-to\\-Last", "NthtoLast", course),  # Removing hyphens     
                         course = gsub("Geo-location", "Geolocation", course),   # Remove hyphen from Geo-location
                         course = gsub("Node(\\s)js|Nodejs", "Node\\.js", course),
                         course = gsub("Expressjs|Express js", "Express\\.js", course))       

# Remove trailing space from course column
course_table4$course <- gsub("\\s$", "", course_table4$course)
```

Ok, any other non-matches come from errors in our original dataset or courses whose name was different (or that no longer existed) when this dataset was published.

Here are a few quick fixes to the original dataset.

``` r
fcc_all2 <- fcc_all %>% 
		mutate(course = recode(course, `Add Font Awesome Icons all of our Buttons` = "Add Font Awesome Icons to all of our Buttons"), 
    course = gsub("Pintrest", "Pinterest", course), course = gsub("Node(\\s)js|Nodejs", 
        "Node\\.js", course), course = gsub("Build Web Apps with Expressjs|Build Web Apps with Express\\.js|Build Web Apps with Express js", 
        "Build Web Apps with Express\\.js", course), course = recode(course, 
        `Add Free Code Camp to your LinkedIn Profile` = "Join our LinkedIn Alumni Network"), 
    course = recode(course, `Add Different a Margin to Each Side of an Element` = "Add Different Margins to Each Side of an Element"), 
    course = recode(course, `Check the Length Property of a String Variable` = "Find the Length of a String"), 
    course = gsub("\\.concat", "concat", course), course = gsub("White Space", 
        "Whitespace", course), course = recode(course, `Learn JavaScript For Loops` = "Iterate with JavaScript For Loops"), 
    course = recode(course, `Learn JavaScript While Loops` = "Iterate with JavaScript While Loops"), 
    course = recode(course, `Mobile Responsive Images` = "Make Images Mobile Responsive"), 
    course = recode(course, `Override Styles with Important` = "Override All Other Styles by using Important"), 
    course = recode(course, `Style Multiple Elements with a CSS Classes` = "Style Multiple Elements with a CSS Class"), 
    course = recode(course, `Trigger jQuery Effects` = "Trigger Click Events with jQuery"), 
    course = recode(course, `Trigger on click Events with jQuery` = "Trigger Click Events with jQuery"), 
    course = recode(course, `Try Camper News` = "Browse Camper News"), 
    course = recode(course, `Update the Properties of a JavaScript Object` = "Updating Object Properties"), 
    course = recode(course, `Use Conditional Logic with If and Else Statements` = "Use Conditional Logic with If Statements"), 
    course = recode(course, `Use Conditional Logic with IfElse Statements` = "Use Conditional Logic with If Statements"), 
    course = gsub("twitch\\.tv", "twitchtv", course), course = recode(course, 
        `Wikipedia Viewer` = "Build a Wikipedia Viewer"), course = recode(course, 
        `Wrap an Anchor Element within a Paragraph` = "Nest an Anchor Element within a Paragraph"), 
    course = recode(course, `Wrap Many Elements within a Div Element` = "Nest Many Elements within a Single Div Element"), 
    course = recode(course, `Wrap Many Elements within a Single Div Element` = "Nest Many Elements within a Single Div Element"))

# Remove trailing space from course column
fcc_all2$course <- gsub("\\s$", "", fcc_all2$course)
```

Now to join the two dataframes based on the "course" column.

``` r
sc_full <- left_join(fcc_all2, course_table4, by = "course")
```

How many of our entries are still missing a "section" label?

``` r
sum(is.na(sc_full$section))
```

    ## [1] 539128

That's 539,128 entries (or about 7.5% of our entire dataset). I checked the following subset of data (only focused on entries with an "NA" in `section`), and then returned to code above to adjust the names of courses as necessary. Any remaining unmatched courses at this point were not on the FCC course map at the end of 2015, and thus will be removed from this dataset.

``` r
# Filter only entries with NA sections
sub_na <- sc_full %>% filter(is.na(section)) %>% group_by(course) %>% 
    summarise(count = n())

# Remove anything that still has missing section values
sc_full2 <- sc_full %>% filter(!is.na(section))

# Remove the original dataset's 'Type' column in exchange for
# the amended 'Type column'
sc_full3 <- sc_full2 %>% select(-2)

sc_full3 <- rename(sc_full3, type = type.y)
```

### Engineered Variables

Perfect! Now our data frame is all set to add other variables.

``` r
sc_full4 <- sc_full3 %>% group_by(.id) %>% arrange(completedDate) %>% 
    mutate(st_cour_num = rank(completedDate, ties.method = "first"), 
        time_since_last = completedDate - lag(completedDate), 
        from_type = lag(type), from_course = lag(course), from_section = lag(section), 
        to_type = lead(type), to_course = lead(course), to_section = lead(section)) %>% 
    ungroup()
```

Let's look at the top few data entries.

``` r
head(sc_full4)
```

    ## # A tibble: 6 × 17
    ##      .id       course       completedDate
    ##   <fctr>        <chr>              <dttm>
    ## 1    954 Meet Bonfire 2015-01-28 06:41:03
    ## 2    954 Meet Bonfire 2015-01-28 06:41:03
    ## 3    954 Meet Bonfire 2015-01-28 06:41:03
    ## 4    954 Meet Bonfire 2015-01-28 06:41:03
    ## 5    954 Meet Bonfire 2015-01-28 06:41:03
    ## 6    954 Meet Bonfire 2015-01-28 06:41:03
    ## # ... with 14 more variables: solution <chr>, X <int>, type <chr>,
    ## #   section <chr>, coursenum <int>, overallnum <int>, st_cour_num <int>,
    ## #   time_since_last <time>, from_type <chr>, from_course <chr>,
    ## #   from_section <chr>, to_type <chr>, to_course <chr>, to_section <chr>

Great! Now that I have the following variables to work with:

-   **.id** : Individual student ID number (generated from JSON)
-   **course** : Course name (cleaned to match course map from Jan 2016)
-   **completedDate** : The POSIXct date and time that a course was completed by a student
-   **solution** : The student's solution to a given problem
-   **type** : The type of course (Basejump, Bonfire, Waypoint, Zipline)
-   **section** :The specific section that each course falls into on the course map ("Getting Started", "HTML5 and CSS", etc.)
-   **coursenum** : The number of the course within each section
-   **overallnum** :The number of the course within the entire curriculum
-   **st\_cour\_num** : The number of the course within each student's specific journey (i.e., a student's first completed class would be 1, 2nd completed class would be 2, etc.)
-   **time\_since\_last** : The time between the completion of the current course and the completion of the last course (in seconds)
-   **from\_type** : The type of class that was taken immediately before the present class (Basejump, Bonfire, Waypoint, Zipline)
-   **from\_course** : The name of the class that was taken immediately before the present class
-   **from\_section** : The section of the course curriculum that the last class taken was in
-   **to\_type** : The type of class that was taken immediately after the present class (Basejump, Bonfire, Waypoint, Zipline)
-   **to\_course** : The name of the class that was taken immediately after the present class
-   **to\_section** : The section of the course curriculum that the next class is in

Data Visualizations
-------------------

Now that I have my variables sorted and my data cleaned, let's try to answer a few simple questions.

### What class do FCC students start with?

-   Does every student start with the same class?

``` r
# Separate out first courses
first <- sc_full4 %>% 
	   filter(st_cour_num == 1) %>% 
	   droplevels()

# How many different courses were the first ones taken?
length(unique(sc_full4))
```

    ## [1] 17

It looks like most users start in the same place. Let's see how the popularity of first courses breaks down.

``` r
# Count number of students for each course first
first_2 <- first %>% group_by(course) %>% summarise(n = n()) %>% 
    ungroup() %>% mutate(rank = dense_rank(desc(n))) %>% filter(rank <= 
    10) %>% arrange(rank)

fig1 <- hchart(first_2, "column", x = course, y = n, zoomType = "x") %>% 
    hc_xAxis(title = list(text = "First Completed Course on FCC")) %>% 
    hc_yAxis(title = list(text = "Number of Students"), max = 50000) %>% 
    hc_chart(zoomType = "x")

fig1
```
<iframe seamless src="../FCC_Courses_files/figure-markdown_github/fig1/index.html" width = "100%" height="500" id="iframe_container"></iframe>

So about 48% of users start with the very first course in the curriculum "Learn How Free Code Camp Works", another 10% skip straight to the first course in the "HTML5 and CSS" section to start their FCC journey. The remaining 42% of users started their first class on random courses throughout the curriculum.

### Which classes are the most popular?

-   How many students completed each course at some point?
    -    Note: This figure is zoom-able!

``` r
pop_fig <- sc_full4 %>% 
		group_by(overallnum, section, course) %>% 
    		summarise(count = n()) %>% 
		arrange(overallnum) %>% ungroup()


# Plot
fig2 <- hchart(pop_fig, "column", x = course, y = count, color = section) %>% 
    hc_title(text = "Course Completion in Curriculum Order") %>% 
    hc_xAxis(title = list(text = "Completed Course on FCC (in order of FCC course curriculum)")) %>% 
    hc_yAxis(title = list(text = "Number of Students"), max = 1e+05) %>% 
    hc_chart(zoomType = "xy") %>% 
    hc_tooltip(headerFormat = "", 
    	pointFormat = "<b>Class: {point.course}</b> <br>
    	Section: {point.section}<br>
    	Number of Students: {point.y}")

fig2
```
<iframe seamless src="../FCC_Courses_files/figure-markdown_github/fig2/index.html" width = "100%" height="500" id="iframe_container"></iframe>

It definitely seems like the courses offered earlier in the curriculum are completed more frequently than courses offered later in the curriculum. There is also a spike in course completion at the beginning of the "HTML5 and CSS" and "Responsive Design with Boostrap" sections that declines throughout the course section. In the latter, that drop-off seems to happen sharply after the course "Line up Form Elements Responsively with Bootstrap" where almost 11,000 did not complete the next course.

I wonder how the amount of time spent on courses changes throughout the curriculum, as courses (theoretically) get more difficult.

*For this figure, I eliminated all course completion times that were over 1,210,000 seconds (14 days).*

``` r
time_spent <- sc_full4 %>% 
			group_by(overallnum, section, course) %>% 
    			filter(!is.na(time_since_last)) %>% 
			filter(time_since_last < 1210000) %>% 
			mutate(time_since_last_m = time_since_last/60) %>% 
    			summarise(median = median(time_since_last_m), 
					  mean = mean(time_since_last_m), 
        					  sd = sd(time_since_last_m))

fig3 <- hchart(time_spent, "column", x = course, y = mean, color = section) %>% 
    hc_title(text = "Average Time for Course Completion") %>% 
    hc_subtitle(text = "<i>Does not contain course completion for students' first class</i>") %>% 
    hc_xAxis(title = list(text = "Completed Course on FCC (in order of FCC course curriculum)")) %>% 
    hc_yAxis(title = list(text = "Mean Time to Course Completion (minutes)"), 
        max = 6800) %>% hc_chart(zoomType = "xy") %>% hc_tooltip(headerFormat = "", 
    pointFormat = "<b>Class: {point.course}</b> <br>Section: {point.section}<br>Mean time to completion: {point.y} minutes", 
    valueDecimals = 2)

fig3
```
<iframe seamless src="../FCC_Courses_files/figure-markdown_github/fig3/index.html" width = "100%" height="500" id="iframe_container"></iframe>

The class that took the longest to complete ("Claim Your Backend Development Certificate" with average 6845 minutes or 4.75 days) is the last and one of the least popular courses in the curriculum. It is possible that students completed all of the other courses and returned to FCC days later to claim their certificate. 

The next courses that took the longest to complete were:

-   Build a Voting App (6675 minutes or 4.6 days)
-   Build a Nightlife Coordination App (4861 minutes or 3.4 days)
-   Use the Twitchtv JSON API (4289 minutes or 3 days)
-   Build a Simon Game (4280 minutes or 3 days)

These courses either came immediately after a break (that was less than two weeks in duration) for many students or took students longer on these courses than others in the curriculum.  It may be worth investigating how long each of these projects is projected to take. 

### How do students flow through the courses?

Originally, I planned to depict student flow through the various FCC courses using a [Sankey plot](https://en.wikipedia.org/wiki/Sankey_diagram), but with so many courses in the curriculum this did not seem like a good option. I decided to visualize student course map as a network using the `igraph` and `highcharter` packages. Below, color indicates the section of each course, connections indicate flow of students from one course to another and the thickness of each connection indicates how many students followed that path. The size of each circle indicates how many students have taken each course.

*Hover over any point for more information and zoom in on sections for clearer detail*

``` r
nodes <- sc_full4 %>% 
		group_by(course, section, overallnum) %>% 
    		summarise(count = n()) %>% 
		ungroup()

edges <- sc_full4 %>% 
		group_by(from_course, to_course) %>% 
		summarise(weight = n()) %>% 
    		filter(!is.na(to_course), !is.na(from_course)) %>% 
		ungroup() %>% 
    		filter(weight > 200)
        

# Make graph object
net <- graph.data.frame(edges, nodes, directed = FALSE)

V(net)$color <- colorize(V(net)$section)
V(net)$size <- V(net)$count
E(net)$width <- E(net)$weight/4000
V(net)$Course_Number <- nodes$overallnum

# Plot graph using highcharter
set.seed(10)
fig4 <- hchart(net, minSize = 3, maxSize = 20, layout = layout_nicely)

fig4
```
<iframe seamless src="../FCC_Courses_files/figure-markdown_github/fig4/index.html" width = "100%" height="500" id="iframe_container"></iframe>

There are several notable things about this figure:

-   The "Getting Started" section is the "jumping off point" for many different tracks
-   One of the most frequently used tracks runs through the HTML5 and CSS section
-   Certain parts of the HTML5 and CSS section (i.e., the parts all about changing colors) are taken in order, while others skip entire chunks of the section (See the heavily followed path from class \#8 (Inform with the Paragraph Element) and class \#40 (Check radio buttons and checkboxes by default)).
-   Users tend to flow straight from the jQuery section into Basic JavaScript following classes mostly in the recommended order
-   Fewer users flow through the Intermediate and Advanced algorithm scripting sections than the Basic algorithm scripting section
-   Several courses don't follow any typical flow with the rest of the course curriculum. These courses seem to be treated as "stand-alone" courses instead of integrated parts of the map, but are also infrequently taken in general. These courses are:
    -   Courses in the Node.js and Express.js section
    -   "Get Set for Basejumps"
    -   "Save your code revisions forever with Git"
    -   Courses in the Dynamic Web application section

### How many courses do students take?

Given that there are 344 courses in the curriculum map that match the courses that these students have taken, how many does each student generally take?

``` r
student_courses <- sc_full4 %>% 
		group_by(.id) %>% 
		mutate(time = as.numeric(time_since_last, units = "mins")) %>% 
		summarise(count = n(), 
			total_time = sum(time, na.rm = TRUE), 
			median_time = median(time, na.rm = TRUE), 
    			average_time = mean(time, na.rm = TRUE)) %>% 
			mutate(time_per_class = (total_time/count)) %>% 
    			filter(count > 0)

fig5 <- hchart(student_courses$count) %>% 
	hc_title(text = "Number of Courses Completed") %>% 
	hc_chart(zoomType = "xy") %>% 
	hc_xAxis(title = list(text = "Number of Completed Courses")) %>% 
   	hc_yAxis(title = list(text = "Number of Students"), max = 35000) %>% 
   	hc_legend(enabled = FALSE)

fig5
```
<iframe seamless src="../FCC_Courses_files/figure-markdown_github/fig5/index.html" width = "100%" height="500" id="iframe_container"></iframe>

Wow! There's a few things to point out here:

-   The highest number of students (33,563 or 33% of students) completed only between 1 and 5 courses.
-   The highest number of courses completed is 775! Since we're only looking at 344 courses here, that's a bit surprising. That student would have needed to complete almost every course twice! Is that the only student who has taken courses more than once?

``` r
multi_class <- student_courses %>% 
			filter(count > 344)

nrow(multi_class)
```

    ## [1] 219

219 students have taken more than the offered 344 courses and thus must have repeated some! It's possible that the other students have repeated some courses too. I'll come back to [that](#which-courses-tend-to-be-repeated-by-students).

### Can you group students based on the number of classes they take vs. the amount of time they spend per class?

The FCC folks who released this dataset seem interested in ways to group their students. I'm curious if this can be done by looking at the number of courses students have completed in comparison to the amount of time spent per course. Do students who complete classes more quickly tend to burn out quickly and ultimately take fewer courses?

``` r
student_courses2 <- student_courses %>% 
		filter(count > 1)


ggplot(student_courses2, aes(x = count, y = median_time)) + geom_point(alpha = 1/4) + 
    labs(x = "Number of Completed Courses", y = "Average time per Course (minutes)")
```

<img src="../FCC_Courses_files/figure-markdown_github/unnamed-chunk-44-1.png" class = "img-responsive" style = "display: block; margin: auto;" />


This figure is a little overwhelmed with outliers who spent a very long time completing few courses.

I'd like to see what this looks like if students don't take breaks between classes, so I'm going to remove any entries where the time to complete the course was greater than 14 days.

``` r
student_no_break <- sc_full4 %>% 
		filter(overallnum > 5) %>% group_by(.id) %>% 
    		mutate(time = as.numeric(time_since_last, units = "mins")) %>% 
    		summarise(count = n(), total_time = sum(time, na.rm = TRUE), 
        			median_time = median(time, na.rm = TRUE), 
			average_time = mean(time, na.rm = TRUE), 
			max_time = max(time, na.rm = TRUE)) %>% 
    		filter(count > 1, max_time < 20160) %>% 
		mutate(time_per_class = (total_time/count))

ggplot(student_no_break, aes(x = count, y = average_time)) + 
    geom_point(alpha = 1/4) + labs(x = "Number of Completed Courses", 
    y = "Average time per Course (minutes)")
```

<img src="../FCC_Courses_files/figure-markdown_github/unnamed-chunk-45-1.png" class = "img-responsive" style = "display: block; margin: auto;" />

Generally speaking, it looks like students who complete courses quickly (not including any breaks) complete the most courses overall. But what if we take breaks into account?

### Does taking a 2 week break help or hurt course completion?

I wanted to look at a box plot of course completion depending on the number of breaks students completed.

``` r
total_breaks <- sc_full4 %>% 
		group_by(.id) %>% 
		mutate(time = as.numeric(time_since_last, units = "mins")) %>% 
		summarise(breaks = length(.id[time > 20160])) %>% 
		mutate(breaks = breaks - 1) %>% 
		ungroup()

breaks <- left_join(total_breaks, student_courses, by = ".id")

multi_class_breaks <- breaks %>% 
		filter(count > 1)


fig6 <- highchart() %>% 
		hc_add_series_boxplot(multi_class_breaks$count, multi_class_breaks$breaks) %>% 	
		hc_xAxis(title = list(text = "Number of > 14 Day Breaks")) %>% 
    		hc_yAxis(title = list(text = "Number of Completed Courses"), 
        			max = 500)

fig6
```
<iframe seamless src="../FCC_Courses_files/figure-markdown_github/fig6/index.html" width = "100%" height="500" id="iframe_container"></iframe>

It seems possible that taking one or more 2 week breaks from the FCC program may actually have a positive impact on the number of courses completed. However, it is unclear at this time if any of those courses are repeats. Let's try to take out repeat courses.

``` r
no_repeats <- sc_full4 %>% 
	group_by(.id) %>% 
	distinct(course) %>% 
    	summarise(nr_count = n()) %>% 
	ungroup()



no_repeat_join <- left_join(no_repeats, breaks, by = ".id")

nr_multi <- no_repeat_join %>% filter(nr_count > 1)


fig7 <- highchart() %>% 
	hc_add_series_boxplot(nr_multi$nr_count, nr_multi$breaks) %>% 
	hc_xAxis(title = list(text = "Number of > 14 Day Breaks")) %>% 
    	hc_yAxis(title = list(text = "Number of Completed Courses (Without Repeats)"), 
        			max = 300, 
			min = 0)

fig7
```
<iframe seamless src="../FCC_Courses_files/figure-markdown_github/fig7/index.html" width = "100%" height="500" id="iframe_container"></iframe>

Even taking repeated courses out of the dataset seems to indicate that of students who completed more than one course, the presence of one or more 2 week breaks actually increases the median number of courses that will be completed. It may be worth encouraging students to take short breaks between classes if they feel they need to.

### Which courses tend to be repeated by students?

``` r
repeats <- sc_full4 %>% 
	group_by(.id, section, overallnum, course) %>% 
    	summarise(course_count = n()) %>% 
	ungroup() %>% 
	group_by(overallnum, section, course) %>% 
	summarise(student_repeats = n(), 
		avg_repeats = mean(course_count)) %>% 
    	arrange(overallnum)


fig8 <- hchart(repeats, "column", x = course, y = avg_repeats, color = section) %>% 
	hc_title(text = "Average Number of Times a Single Student Completed Each Course") %>% 
    	hc_subtitle(text = "<i>In course curriculum order</i>") %>% 
    	hc_xAxis(title = list(text = "FCC Course (in order of FCC course curriculum)")) %>% 
    	hc_yAxis(title = list(text = "Mean # Times Single Student Completed Each Course"), 
       		max = 1.5, min = 1) %>% 
	hc_chart(zoomType = "xy") %>% 
    	hc_tooltip(headerFormat = "", pointFormat = "<b>Class: {point.course}</b> <br>
		Section: {point.section}<br>
		Mean times course taken by single student: {point.y} times", 
        		valueDecimals = 2)

fig8
```
<iframe seamless src="../FCC_Courses_files/figure-markdown_github/fig8/index.html" width = "100%" height="500" id="iframe_container"></iframe>

The courses closest to 1 in this figure were repeated the fewest number of times by students. The most commonly repeated course in the entire curriculum was "Where art thou", a course near the beginning of the "Intermediate Algorithm Scripting" Section.

Generally speaking, sections with the most repeated courses were:

-   Basic JavaScript
-   Basic Algorithm Scripting
-   Intermediate Algorithm Scripting
-   Automated Testing and Debugging
-   Advanced Algorithm Scripting

This could indicate that the courses in these sections are simply more difficult than other sections or that the material in these courses needs to be viewed more than once to completely comprehend the material. Either way, the content in these course sections should be assessed.

### Can we predict the number of courses a student will complete using machine learning?

I'm going to use some simple machine learning to see if I can predict the number of courses a student will complete (not counting repeated courses). I'm going to include 4 variables in this model:

-   First completed course
-   Number of repeated courses
-   Number of 14 day breaks
-   Median course completion time (not including &gt; 14 day breaks)

First, I need to transform my dataset to fit into a frame with the needed variables. To remain consistent, I'll once again use `dplyr` functions for this.

``` r
repeat_students <- sc_full4 %>% 
	group_by(.id) %>% 
	summarise(total_count = n()) %>% 
    	ungroup() %>% 
	filter(total_count > 1)

first_course <- sc_full4 %>% 
	group_by(.id) %>% 
	filter(st_cour_num == 1) %>% 
	select(1:2) %>% 
	ungroup()

repeated_courses <- sc_full4 %>% 
	group_by(.id, course) %>% 
	summarise(course_count = n()) %>% 
    	filter(course_count > 1) %>% 
	summarise(repeat_count = n()) %>% 
   	ungroup()

breaks_courses <- sc_full4 %>% 
	group_by(.id) %>% 
	mutate(time = as.numeric(time_since_last, units = "mins")) %>% 
	summarise(breaks = length(.id[time > 20160])) %>% 
	mutate(breaks = breaks - 1) %>% 
	ungroup()

median_courses <- sc_full4 %>% 
	group_by(.id) %>% 
	mutate(time = as.numeric(time_since_last, units = "mins")) %>% 
	filter(time < 20160) %>% 
	summarise(median_time = median(time, na.rm = TRUE)) %>% 
	ungroup()

completed_courses <- sc_full4 %>% 
	group_by(.id) %>% 
	distinct(course) %>% 
   	summarise(nr_count = n()) %>% 
	ungroup()

mlset <- left_join(repeat_students, first_course, by = ".id")
mlset <- left_join(mlset, repeated_courses, by = ".id")
mlset <- left_join(mlset, breaks_courses, by = ".id")
mlset <- left_join(mlset, median_courses, by = ".id")
mlset <- left_join(mlset, completed_courses, by = ".id")

# Replacing NA in repeat_count column with 0
mlset$repeat_count[is.na(mlset$repeat_count)] <- 0

# Making course into a factor
mlset$course <- as.factor(mlset$course)
```

Now that the dataframe is set up the way I want (a single row per student and only the variables I'm interested in), I'm going to randomize the dataset and extract a test set.

``` r
# Set random seed
set.seed(1)

# Shuffle rows of data set
n <- nrow(mlset)
shuffled <- mlset[sample(n), ]

# Perform 70/30 split (70% to training set, 30% to test set)
train_indices <- 1:round(0.7 * n)
train <- shuffled[train_indices, ]

test_indices <- (round(0.7 * n) + 1):n
test <- shuffled[test_indices, ]
```

#### Fitting a baseline model

Without adding any extra variables, how well can we predict the number of courses that a student will complete? Without any additional variables, my best guess is that the mean number of courses taken (without replication) would help us predict the number of courses completed. This will give us a baseline to compare our more complex models with. Wow a RMSE of 73.17!

``` r
# Creating baseline model
baseline <- mean(train$nr_count)

# Evaluate RMSE on the test set
base_test <- sqrt(mean((baseline - test$nr_count)^2))
base_test
```

    ## [1] 73.17323

#### Creating trainControl

Since I plan to use the `caret` package to test two different models for these data, I will create a system that will perform a 10-fold cross-validation of the training dataset.

``` r
myControl <- trainControl(method = "cv", number = 10, verboseIter = TRUE)
```

#### Fitting a random forest model

The first type of model I'd like to use is a random forest model (using the `ranger` and `caret` packages and the trainControl object I just created).

``` r
rf_model <- train(nr_count ~ course + repeat_count + breaks + 
    median_time, data = train, tuneLength = 3, method = "ranger", 
    trControl = myControl, importance = "impurity")
```

``` r
rf_model
```

    ## Random Forest 
    ## 
    ## 55502 samples
    ##     4 predictor
    ## 
    ## No pre-processing
    ## Resampling: Cross-Validated (10 fold) 
    ## Summary of sample sizes: 49951, 49952, 49952, 49952, 49953, 49951, ... 
    ## Resampling results across tuning parameters:
    ## 
    ##   mtry  RMSE      Rsquared 
    ##     2   69.16746  0.2839369
    ##   122   58.47552  0.3601910
    ##   243   66.00565  0.2475999
    ## 
    ## RMSE was used to select the optimal model using  the smallest value.
    ## The final value used for the model was mtry = 122.

Our model is an OK fit, explaining about 55% of the variation in our model with a RMSE of 49.

#### Fitting a glmnet model

I'm also going to try to fit a glmnet model to these data.

``` r
glm_model <- train(nr_count ~ course + repeat_count + breaks + 
    median_time, data = train, tuneLength = 3, method = "glmnet", 
    trControl = myControl)
```

    ## Loading required package: glmnet

    ## Loading required package: Matrix

    ## 
    ## Attaching package: 'Matrix'

    ## The following object is masked from 'package:tidyr':
    ## 
    ##     expand

    ## Loading required package: foreach

    ## Loaded glmnet 2.0-5

``` r
glm_model
```

    ## glmnet 
    ## 
    ## 55502 samples
    ##     4 predictor
    ## 
    ## No pre-processing
    ## Resampling: Cross-Validated (10 fold) 
    ## Summary of sample sizes: 49952, 49950, 49952, 49952, 49951, 49952, ... 
    ## Resampling results across tuning parameters:
    ## 
    ##   alpha  lambda      RMSE      Rsquared 
    ##   0.10   0.05861448  64.27342  0.2272597
    ##   0.10   0.58614484  64.23881  0.2280560
    ##   0.10   5.86144844  64.26716  0.2287188
    ##   0.55   0.05861448  64.25864  0.2276078
    ##   0.55   0.58614484  64.17347  0.2297203
    ##   0.55   5.86144844  64.84995  0.2170755
    ##   1.00   0.05861448  64.23882  0.2280713
    ##   1.00   0.58614484  64.21272  0.2288975
    ##   1.00   5.86144844  65.07230  0.2166004
    ## 
    ## RMSE was used to select the optimal model using  the smallest value.
    ## The final values used for the model were alpha = 0.55 and lambda
    ##  = 0.5861448.

The best glmnet model explained about 48% of the variance in our dataset with a RMSE of about 53.

#### Comparing Model Fit

``` r
# Create a list of models
models <- list(rf = rf_model, glmnet = glm_model)

# Resample the models
resampled <- resamples(models)

# Generate a summary
summary(resampled)
```

    ## 
    ## Call:
    ## summary.resamples(object = resampled)
    ## 
    ## Models: rf, glmnet 
    ## Number of resamples: 10 
    ## 
    ## RMSE 
    ##         Min. 1st Qu. Median  Mean 3rd Qu.  Max. NA's
    ## rf     57.79   57.91  58.52 58.48   58.85 59.50    0
    ## glmnet 63.36   64.06  64.27 64.17   64.34 64.75    0
    ## 
    ## Rsquared 
    ##          Min. 1st Qu. Median   Mean 3rd Qu.   Max. NA's
    ## rf     0.3312  0.3510 0.3629 0.3602  0.3724 0.3781    0
    ## glmnet 0.2204  0.2258 0.2268 0.2297  0.2317 0.2487    0

``` r
# Plot the differences between model fits
dotplot(resampled, metric = "RMSE")
```

<img src ="../FCC_Courses_files/figure-markdown_github/unnamed-chunk-60-1.png" class = "img-responsive" style = "display: block; margin: auto;" />


``` r
dotplot(resampled, metric = "Rsquared")
```

<img src ="../FCC_Courses_files/figure-markdown_github/unnamed-chunk-60-2.png" class = "img-responsive" style = "display: block; margin: auto;" />


So our Random Forest model has a better fit and a lower error than the glmnet model. Let's see which predictors were the most important to that model.

``` r
# Creating a Variable Importance variable
vimp <- varImp(rf_model)

# Plotting 'vimp'
ggplot(vimp, top = 20[1])
```

<img src="../FCC_Courses_files/figure-markdown_github/unnamed-chunk-61-1.png" class = "img-responsive" style = "display: block; margin: auto;" />


The median time spent on each class is the most important variable in our current model, followed by the first course in the series, and the number of courses that were repeated.

Conclusions
-----------

While I didn't answer every one of the questions that FCC was interested in, I was able to answer a few, and here's what I found:

-   **Starting Location** : While users start their FCC journey at a few different places, most begin at either "Learn how Free Code Camp works" and "Say Hello to HTML Elements".
-   **Most Popular Courses** : The courses offered early in the curriculum were completed mre frequently, and course completion drops off throughout the "HTML5 and CSS" and "Responsive Design with Bootstrap" sections.
-   **Longest Courses to Complete** : The courses that took the longest (on average) to complete were:
    -   Claim Your Backend Development Certificate (6845 minutes or 4.75 days)
    -   Build a Voting App (6675 minutes or 4.6 days)
    -   Build a Nightlife Coordination App (4861 minutes or 3.4 days)
    -   Use the Twitchtv JSON API (4289 minutes or 3 days)
    -   Build a Simon Game (4280 minutes or 3 days)
-   **Course Completion** : About 1/3 of students only complete between 1 and 5 courses.
-   **Repeated Course Completion** : Many students complete the same course more than once. The most commonly repeated courses are:
    -   Basic JavaScript
    -   Basic Algorithm Scripting
    -   Intermediate Algorithm Scripting
    -   Automated Testing and Debugging
    -   Advanced Algorithm Scripting
-   **Taking Two Week Breaks** : Taking one or more two week breaks actually appears to lead to a higher median number of completed courses.
-   **Student Flow Through Course Map** : Users tend to take many of the completed courses in the recommended order, but some suggested pathways are ignored in favor of more user-preferred pathways. Several courses don't appear to fit into the typical flow of the course curriculum. These courses seem to be treated as stand-alone courses:
    -   Courses in the Node.js and Express.js section
    -   "Get Set for Basejumps"
    -   "Save your code revisions forever with Git"
    -   Courses in the Dynamic Web application section
-   **Predicting Course Completion** : While a model could be fit, more information is needed to properly predict course completion for FCC students.

My Suggestions
---------------

While I can only offer suggestions based on the data I am given, I suggest that FCC:

-   Investigate "Line up Form Elements Responsively with Bootstrap" course. Almost 11,000 students complete this course but don't continue to the next courses in the section.
-   Determine why the longest courses to complete take so long. Are these courses just unlikely to be completed in one sitting? Are they significantly more difficult than other courses in the curriculum? 
-   Suggest to students that it is OK (and even sometimes helpful) to take a break and then come back. Though, it may be helpful to remind them to come back if they've been gone for more than 2 weeks.
-   Determine why some courses were taken repeatedly by the same students. Is the course material in these courses harder and thus needed to be taken multiple times to comprehend?
-   Integrate the "stand-alone" courses into the curriculum map in a more cohesive way.
-   Offer different maps based on students' end-goal. All courses may not be necessary for each type of student.

As always, I appreciate any and all feedback on my work, so feel free to let me know what you think!
