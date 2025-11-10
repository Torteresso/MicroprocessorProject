REQUIREMENTS
=============

* Dune (version >= 3.10)  

You can then run 

```
opam install . --deps-only
```

to install dependencies if needed.

PRESENTATION
=============

This repository contains a program capable of simulating any digital synchroneous circuit. Given the circuit description in the form of a netlist and the value of every input at each of the cycles, it returns every output at each of the cycles.

USES
=============

To simulate a circuit, simply run the following command : 


```
opam exec -- dune exec -- simulator -n 10 your_netlist.net 
```

You should provide the circuit description with a netlist file.
You can change the number of steps of the simulation with the option -n <n_steps>.

Alternatively, you can only print the sorted netlist of a circuit with the command : 


```
opam exec -- dune exec -- simulator -print your_netlist.net 
```

CODE ARCHITECTURE
==============

The netlist folder contains code that extract information from a netlist to ocaml.

The scheduler folder contains code that use graph representation to make sure that a netlist is sorted i.e. can be simulated sequentially.

The simulator folder contains the code that compiles the simulator.

TESTS
==============

I added some extra tests to the default one :

Some that should compile (test/extra/positive folder) : 

* Some that test every operations and memory on 1 bit and buses.
* Some that test the possibility to have multiple ROMs and RAMs. 
* One that takes an unsorted netlist as input.

Some that should not compile (test/extra/negative folder) : 

* Some that uses 1 bit instead of buses and vice versa.
* Some that uses wrong arguments for operators.
* Some that uses wrong arguments for address in ROM/RAM.
* One that defines multiple times the same variable.
* One that has cycle.


DIFFICULTIES ENCOUNTERED
==============

- Learning Ocaml as I didn't know the language before.
- Understanding how ROM/RAM should behave if multiple equations in the netlist use them.



