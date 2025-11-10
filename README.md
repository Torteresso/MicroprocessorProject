REQUIREMENTS
=============

* Dune (version >= 3.10)  

PRESENTATION
=============

This repository contains a program capable of simulating any diginital synchroneous circuit. Given the circuit description in the form of a netlist and the value of every input at each of the cycles, it returns every output at each of the cycles.

USES
=============

To simulate a circuit, simply run the following command : 


```
opam exec -- dune exec -- simulator -n 10 your_netlist.net 

```

You should provide the circuit description with a netlist file (here ./test/fulladder.net). 
You can change the number of steps of the simulation with the option -n <n_steps>.

Alternatively, you can only print the netlist of a circuit with the command : 


```
opam exec -- dune exec -- simulator -print your_netlist.net 

```

CODE ARCHITECTURE
==============

The netlist folder contains code that extract information from a netlist to ocaml.

The scheduler folder contains code that simulator use graph representation to make sure that a netlist is sorted i.e. can be simulated sequentially.

The simulator folder contains code that compile the simulator.


DIFFICULTIES ENCOUNTERED
==============

- Learn Ocaml as it was a new language for me.
- Understand how ROM/RAM should behave if multiple equations in the netlist use it 

