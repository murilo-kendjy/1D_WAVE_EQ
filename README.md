# 1D_WAVE_EQ

This is a project made to a subject in my college course. The project consists in modeling a 1D wave system and solve it using computational methods.

After modeling the system, Fortran language was used to solve it numeracally and afterwards Python to plot the results.

All theory and modeling can be found in the `theory` folder, and the code itself in the main directory.

## Testing the code

Donwnload all code parts and run `make` command.

The main program `wave` receives the input name file and outputs something like: `Data_[2-0-0-0-0-0]_|1000|1000|400|400|200|.dat` and `Data_Vecs_[1000-1000].dat` that must be passed to the python script in order.

All numbers in output file is about the system itself so you can store multiple systems.

### Example

```
make

./wave

python plot.py Data_\[2-0-0-0-0-0\]_\|1000\|1000\|400\|400\|200\|.dat Data_Vecs_\[1000-1000\].dat
```


---

Plans to traduct all the theory to english later on...
