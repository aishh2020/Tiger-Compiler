# CS3140 Compiler Design Laboratory

Name : Aiswarya H \
Roll No : 111901006 

## Directories
`reverse-polish/` -> contains reverse-polish compiler lab files \
`target/` -> contains the Makefile and all the files of tiger compiler: the MIPS, temp , translate and basic block structures, parser, lexer, AST, Graph and IR structures \

## Build and Testing

Execute the following make command inside target directory to build the programs 


make

Save the test input of the form of the tiger subset language into a file say `test.inp`. Run it using the below command
and store the output in a file say `test.out`

./ec test.inp > test.out

Note that the basic blocks are themselves demarcated by a blank line in the resulting `test.out` mips code.
`test.out` now contains the MIPS code and the basic blocks. To test working of the MIPS code using SPIM, read and run the file containing the MIPS code.

spim
read "filename"
run

