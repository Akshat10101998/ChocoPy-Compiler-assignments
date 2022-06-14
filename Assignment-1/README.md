# Assignment 1

[PA1 Specification]: 
[ChocoPy Specification]: 

Note: Users running Windows should replace the colon (`:`) with a semicolon (`;`) in the classpath argument for all command listed below.

## Getting started

Run the following command to generate and compile your parser, and then run all the provided tests:

    mvn clean package

    java -cp "chocopy-ref.jar:target/assignment.jar" chocopy.ChocoPy --pass=s --test --dir src/test/data/pa1/sample/

In the starter code, only one test should pass. Your objective is to build a parser that passes all the provided tests and meets the assignment specifications.

To manually observe the output of your parser when run on a given input ChocoPy program, run the following command (replace the last argument to change the input file):

    java -cp "chocopy-ref.jar:target/assignment.jar" chocopy.ChocoPy --pass=s src/test/data/pa1/sample/expr_plus.py

You can check the output produced by the staff-provided reference implementation on the same input file, as follows:

    java -cp "chocopy-ref.jar:target/assignment.jar" chocopy.ChocoPy --pass=r src/test/data/pa1/sample/expr_plus.py

Try this with another input file as well, such as `src/test/data/pa1/sample/coverage.py`, to see what happens when the results disagree.

## Assignment specifications

See the [PA1 specification][] on the GitHub page for a detailed specification of the assignment.

Refer to the [ChocoPy Specification][] on the Canvas page for the specification of the ChocoPy language. 

## Receiving updates to this repository

Add the `upstream` repository remotes (you only need to do this once in your local clone):

    git remote add upstream https://github.com/IITGN-CS327-2022/Assignment1.git

To sync with updates upstream:

    git pull upstream master


## Submission writeup

(Students should edit this section with their write-up)

Team member 1: Akshat Mangal (17110010)

Team member 2: Gaurav Sonkusle (17110055)

Team member 3: Mohamed Shamir (17110084)

Team member 4: Mohmmad Aslam (1711086)

<hr>

Answers to questions:

1. What strategy did you use to emit INDENT and DEDENT tokens correctly? Mention the filename and the line number(s) for the core part of your solution.

    We are keeping track of Indent levels in the code using a stack, we keep track of previous indent levels and current indent be stored in a variable name curr_indentation_level. We keep the stack updated whenever we encountered any whitespaces. And, also whenever we read a non-{WhiteSpace} character, we either start emitting INDENT/DEDENT tokens, or enter into normal processing mode which will ensures that we can emit as many DEDENT tokens as necessary. If column > current_indentation_level, then, INDENT token is returned else if current_indentation_level > column, then the last indent level is popped from the stack of previous indentation states and that popped value gets assigned to the current indentation level, and if after popping, column > current_indentation_level, invalid indentation else DEDENT token is returned.
    Suppose NEWLINE is followed by some non-{WhiteSpace} character, then, the buffer gets pushed back by 1, and, any previous indentation block levels, still left in the indent_level_stack, DEDENT token is returned & that indent_level is popped out of the stack.

    Filename: src/main/jflex/chocopy/pa1/ChocoPy.jflex

    Line Numbers: 
    
    1. Lines from 86-115, in which we initialized the variables for whitespaces, curr_indentation_level and stack for keeping track of Indent levels. whereas, 

    2. Lines from 250-320 for handling different cases for whitespaces and emitting INDENT/DEDENT tokens accordingly.

<hr>
2. What was the hardest language feature (not including indentation) to implement in this assignment? Why was it challenging? Mention the filename and line number(s) for the core part of your solution.

<br>
The grammar rules were given in the chocopy reference manual on page number 15. However, implementing them took much time. The abstract syntax tree was made up of different classes combined (Using cup parser). There were many parent child relations and it was difficult to understand from the figure 1 that was shared along with the assignment and couldn't find much documentation online about the same. We looked into the various class declarations in common/astnodes. 

<br>
File name: src/main/cup/chocopy/pa1/ChocoPy.cup

<br>
Line Numbers: Lines from 302-536 are lines regarding the grammar rules in cup parser.
<hr>

We didn't use any grace hours to complete this assignment.


**References**
1. https://andreil26.github.io/me/uniprojects/2019/06/21/lexer_parser.html
2. https://www.youtube.com/watch?v=hXQlfJUVyXo&ab_channel=RaheelSiddiqi
3. http://www2.cs.tum.edu/projects/cup/examples.php