Compilation:

- installer les dependances par le script install.sh de ReaVer

ou

- installer (avec opam) apron (gere les dependances)

- installer camllib et fixpoint (solveur de point-fixe qui a besoin de camllib).

  Nicolas Berthier a(vait) prepare une version opam, qu'on peut
  retrouver sous le nom de fix (!) qui s'installe sans camllib (ca
  doit l'incorporer).

  Si tu veux garder le Makefile actuel,

  svn co https://scm.gforge.inria.fr/anonscm/svn/bjeannet/pkg/camllib/trunk camllib
  svn co https://scm.gforge.inria.fr/anonscm/svn/bjeannet/pkg/fixpoint/trunk camllib

  make all install

- il faut avoir ocamlbuild

- taper make (qui délègue à ocamlbuild)
  * construit tout dans sous-repertoire _build, executable
    _build/main.byte ou _build/main.native (defaut)

Execution:

- exemples d'appel: script.sh
  * format d'entree natif (très bas niveau): .lts
  * option "-debug 1" importante (sinon sortie dot vide)
  * reste des printf meme en -debug 0 (affichage des formes de Jordan)
    ils sont prefixes par "if true then"

- connexion a Lustre: script lus2lts.sh

  utilise reaver (outil de Peter), script jordan.pl et SAGE pour
  passer d'un .lus à un .lts avec calcul des formes de Jordan et des
  matrices de passage.
