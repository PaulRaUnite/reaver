# ReaVer
(Based on https://github.com/PaulRaUnite/ocaml-esy-vscode-hello and https://bitbucket.org/pschrammel/reaver/)

## Description
(from https://pschrammel.bitbucket.io/schrammel-it/research/reaver/)
> ReaVer is a tool framework for safety verification of discrete and hybrid systems specified by logico-numerical data-flow languages, like Lustre, Lucid Synchrone or Zelus. It provides time-unbounded analysis based on abstract interpretation techniques.

> It features partitioning techniques and logico-numerical analysis methods based on Kleene iteration with widening and descending iterations, abstract acceleration, max-strategy iteration, and relational abstractions; logico-numerical product and power domains (based on the APRON and BddApron domain libraries) with convex polyhedra, octagons, intervals, and template polyhedra; and frontends for the hybrid NBac format, Lustre via lus2nbac, and Zelus/Lucid Synchrone. 

## Development setup

1. Install [rootless](https://docs.docker.com/engine/security/rootless/) or [non-root](https://docs.docker.com/engine/install/linux-postinstall/#manage-docker-as-a-non-root-user) [Docker](https://www.docker.com) and VSCode;
2. Install [Dev Containers extension](https://marketplace.visualstudio.com/items?itemName=ms-vscode-remote.remote-containers);
3. Download or clone the repository and open it in the editor;
4. Reopen in dev container and wait for it to finish the build;
5. Run `esy` command inside the container to install OCaml language server and build the project.
Packages will be cached into local `.esy` folder to persist between the container runs.
(if no typing inference, `Ctrl+Shift+P -> OCaml: restart language server`)

## Compilation
```bash
./build_image.sh
```

## Usage

Guide: https://pschrammel.bitbucket.io/schrammel-it/research/reaver/reaver_userguide.pdf

Using script that calls built docker image:
```bash
./run.sh <filepath> [-s <strategy instructions from the PDF guide above>]
```