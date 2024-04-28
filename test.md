# qs8 - Python Statevector Simulator

qs8 is a quantum statevector simulator built in Python for small-scale projects. It was mostly an exercise in my own abilities in how the domains of linear algebra and quantum computing overlap, and only uses the `numpy` module as its core dependency.

In the future, I might adapt this project to JS for browser-based quantum simulation projects.

## How does it work?
Quantum circuits are organized as a list of gates, where each entry is a timestep column. Index 0 is `t0`, index 1 is `t1`, etc. You can interpret the circuit by using matrix multiplication to evolve the statevector column by column, or build the entire circuit into a single unitary to then execute in one step. The single-stepping feature of this makes it easier to debug and figure out problems with state. 

Pre-made gates include `H`, `X`, `Y`, `Z`, `CX`, `CXl`, and `SWP`. `CX` and`CXl` are the standard and `l`ower version of the `CNOT` gate. Custom gates can be created as numpy arrays as seen in the current implementation of `qs8.py`:
```python
# this has the first qb listed as control
CX = np.asarray([[1, 0, 0, 0],
                 [0, 0, 0, 1],
                 [0, 0, 1, 0],
                 [0, 1, 0, 0]])
```
The core stipulation of this project is that there is currently **no** sugar on the multi-qubit operations. This means that qubits must be directly next to each other for a multi-qubit operation to take place.

For an example, let's create a circuit that initializes a GHZ state between 4 qubits.
```python
from qs8 import *
qb = 4
# creating a circuit w/ 4 qubits
qc = QCirc(qb)
```
After creating the quantum circuit with the associated amount of wires, or qubits, let's set the first couple of gates to test with.
```python
# setting the gate on q0 in column 0 to X
qc.set_gate(X, [0], 0)
# setting the gate on q0 in column 1 to H
qc.set_gate(H, [0], 1)
```
The X gate runs a NOT on qubit 0, or `q0`, in column 0, and the H runs a Hadamard on `q0` in column 1. Notice that the affected qubits are input as a list.
```python
# create CX gates linking from q0 to qb-1 (q3)
for i in range(qb - 1):
    qc.set_gate(CX, [i, i + 1], i + 2)
```
Now we use a for loop to add CX gates between `q(i)` and `q(i+1)`
```python
# builds and runs circuit
qc.run_circuit(build=True)

# getting a dictionary of counts
counts = qc.get_counts(10000)

# print the circuit and counts
print(str(qc))
print(counts)
```
After the circuit is created, you can build the unitary while running by setting `build=True`. Additionally, you can interpret the circuit column by column by running `qc.interpret_circuit()` instead. You can get a dictionary of n shots by using the `get_counts` function on this circuit. Finally, the circuit has a `__str__` method that prints the circuit in a readable format.
The output after running this script can be seen below:

```
q0 X-H-CX----
       |
q1 ----o-CX--
         |
q2 ------o-CX
           |
q3 --------o-

{'15': 4986, '0': 5014}
```
### Sources
- [qs8](https://github.com/Birduo/qs8)
- [p5.min.js](https://cdnjs.cloudflare.com/ajax/libs/p5.js/1.9.2/p5.min.js)
// - [math.js](https://cdnjs.cloudflare.com/ajax/libs/mathjs/12.4.2/math.min.js)

### Navigation
- [Home](/README.md)
- [Quantum](/quantum)
- [Simulation](/simulation)

```js
document.title = "qs8"
```
