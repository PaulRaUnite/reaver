FROM ubuntu:22.04 AS dev
ARG DEBIAN_FRONTEND=noninteractive

RUN apt update && apt install -y build-essential m4 autoconf curl git libgmp-dev libmpfr-dev opam wget && apt-get clean

WORKDIR /home/root/
COPY vendor/yices/* ./
RUN wget -nc https://yices.csl.sri.com/old/binaries/yices-1.0.40-x86_64-unknown-linux-gnu.tar.gz
RUN bash install-yices.sh ./yices-1.0.40-x86_64-unknown-linux-gnu.tar.gz
RUN opam init --disable-sandboxing
RUN opam update
RUN opam install -y dune ocaml-lsp-server ocamlformat user-setup
RUN eval $(opam env)

FROM dev
WORKDIR /workspaces/reaver/
COPY . /workspaces/reaver/
RUN dune build
ENTRYPOINT [ "dune", "exec", "reaver" ]