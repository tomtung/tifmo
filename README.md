# Introduction

[TIFMO](http://kmcs.nii.ac.jp/tianran/tifmo/) (Textual Inference Forward-chaining MOdule) is an unsupervised Recognizing Textual Entailment (RTE) system based on Dependency-based Compositional Semantics (DCS) and logical inference.

This repository is a fork of `https://github.com/tianran/tifmo` by [@tianran](https://github.com/tianran).
During my internship at [National Institute of Informatics, Japan](http://www.nii.ac.jp/) in 2014, I extended his work to introduce support for generalized quantifiers. The result will be published in [the 28th Pacific Asia Conference on Language, Information and Computing](http://www.arts.chula.ac.th/~ling/paclic28/):

> *Yubing Dong, Ran Tian and Yusuke Miyao.* "Encoding Generalized Quantifiers in Dependency-based Compositional Semantics." Proceedings of the 28th Pacific Asia Conference on Language, Information and Computing. Phuket, Thailand, 2014. \[[paper](http://www.arts.chula.ac.th/~ling/paclic28/program/pdf/585.pdf)\]\[[slides](http://www.slideshare.net/yubingdong/encoding-generalized-quantifiers-in-dependencybased-compositional-semantics)\]

# Quick start

 * Download the code: `git clone https://github.com/tomtung/tifmo.git`
 * Build with `./sbt compile`
 * Run FraCaS demo: `./sbt "run-main tifmo.demo.FraCaS input/fracas.xml" > fracas-out.txt`
 * Check accuracy: `./AccuracyFraCaS.sh fracas-out.txt single 1 80`
 * Run RTE demo: `./sbt "run-main tifmo.demo.RTE input/RTE2_dev.xml" > rte2-dev-out.txt`
 * Check accuracy: `./AccuracyRTE.sh rte2-dev-out.txt 0.4`
