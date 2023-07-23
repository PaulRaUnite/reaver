# ReaVer
(from https://pschrammel.bitbucket.io/schrammel-it/research/reaver/)
> ReaVer is a tool framework for safety verification of discrete and hybrid systems specified by logico-numerical data-flow languages, like Lustre, Lucid Synchrone or Zelus. It provides time-unbounded analysis based on abstract interpretation techniques.

> It features partitioning techniques and logico-numerical analysis methods based on Kleene iteration with widening and descending iterations, abstract acceleration, max-strategy iteration, and relational abstractions; logico-numerical product and power domains (based on the APRON and BddApron domain libraries) with convex polyhedra, octagons, intervals, and template polyhedra; and frontends for the hybrid NBac format, Lustre via lus2nbac, and Zelus/Lucid Synchrone. 

## Installation
Use docker, otherwise GMP installation will probably break your system.
```bash
./build_image.sh
```

If you still want to do it, use `install.sh` script (not tested).

Original ReaVer description is [here](./trunk/src/README)

## Usage

Guide: https://pschrammel.bitbucket.io/schrammel-it/research/reaver/reaver_userguide.pdf

Using script that calls built docker image:
```bash
./run.sh <filepath> [-s <strategy instructions from the guide above>]
```