I compile the code with:

ocamlc str.cma -o rpn rpn.ml

and I run the program with

./rpn

It will read each line from stdin, and I assume the user will press
Ctrl+D or the windows equivalent to trigger an EOF when they are
finished running calculations. After reading each line, it will either
put the result of the expression or else the error message that occured
while parsing it.

I run my tests written in test.txt by performing this command:

./rpn < test.txt

This will pipe the file into stdin, where I can compare the result to
the expected output found in test_result.txt, when I want to
automatically compare the result I use this command:

./rpn < test.txt | diff - test_result.txt
