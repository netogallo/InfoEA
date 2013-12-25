== Genetic ==

=== Requirements ===

The program requires Haskell to be compiled. Cabal is the recomended
tool to resolve dependencies and build the program. 

GNUPlot is required to render the graphs. Without GNUPlot, there is no
way to visualize the results of the program

=== Installation ===

The provided cabal file allows the program to be easily installed
with all it's dependencies. The program can be easily installed
by running:
 
> cabal install

=== Running ===

An executable called Genetic will be placed in your cabal binary
directory. Calling the executable without arguments or the --help
flag will provide information about how to run the program. 
As an example, consider:

src/Genetic -i 15 -f 30 -p 6 +RTS -N4 -K512M

This will run experiments for all functions 15 times and the generation
size of repetition i will be i*30.

Using the runtime system flags -N is recommended to parallelize the
multiple repetitions across many cpus and a bigger heap size is also
recommended when using big generations, the -K flag allows this to
be changed.