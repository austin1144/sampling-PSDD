# Sampling Structures from Circuits
Exact Interence on PSDD(Machine Learning)
# Abstract
The probabilistic sentential decision diagram (PSDD) has been proposed recently as a representation of tractable structured probability spaces which are subject to logical
constraints.[6] PSDDs are known for expressing probability distributions in a more efficient, compact structure than Bayesian Networks (BNs). Knowing this provides
the foundation to believe that this desirable property will improve sampling efficiency in certain cases. Also, perceiving that PSDDs could encode arbitrary constraints,
it may be possible to use them to perform more complex forms of sampling. For instance, sampling with evidence, sampling in diverse configuration.
Probabilistic reasoning with, and structure learning of, PSDDs has been extensively studied.

However, the sampling algorithm for a configuration in this class of circuits has not yet been proposed. This thesis is based on probabilistic graphical models to develop an algorithm to perform efficient random sampling to generate a configuration, programming done in Scala and Python.
First, this thesis explores how to devise a sampling algorithm on tractable circuit - PSDD. The author researches on developing a sampling algorithm from tractable
circuits. The case of sampling with evidence and without evidence between PSDD and Bayesian Networks will be compared. Second, the conversion algorithm for BNs
with a choice of particular variable tree (vtree) to PSDDs will be researched. In the end, the thesis is able to prove efficient sampling on the above tractable circuit.
