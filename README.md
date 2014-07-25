# Introduction

[TIFMO](http://kmcs.nii.ac.jp/tianran/tifmo/) (Textual Inference Forward-chaining MOdule) is an unsupervised Recognizing Textual Entailment (RTE) system based on Dependency-based Compositional Semantics (DCS) and logical inference.

This repository is a fork of `https://github.com/tianran/tifmo`, containing the work that I have done during my 2014 internship at [National Institute of Informatics, Japan](http://www.nii.ac.jp/). The major contribution is the support for generalized quantifiers.

# Quick start

 * Download the code: `git clone https://github.com/tomtung/tifmo.git`
 * Build with './sbt compile'
 * Run FraCaS demo: `./sbt "run-main tifmo.demo.FraCaS input/fracas.xml" > fracas-out.txt`
 * Check accuracy: `./AccuracyFraCaS.sh fracas-out.txt single 1 80`
 * Run RTE demo: `./sbt "run-main tifmo.demo.RTE input/RTE2_dev.xml" > rte2-dev-out.txt`
 * Check accuracy: `./AccuracyRTE.sh rte2-dev-out.txt 0.4`
