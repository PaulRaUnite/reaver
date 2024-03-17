FROM ubuntu:22.04 AS dev
ARG DEBIAN_FRONTEND=noninteractive

RUN apt update && apt install -y build-essential m4 autoconf curl git libgmp-dev libmpfr-dev opam wget && apt-get clean

WORKDIR /home/root/
COPY vendor/yices/* ./
RUN wget -nc https://yices.csl.sri.com/old/binaries/yices-1.0.40-x86_64-unknown-linux-gnu.tar.gz
RUN bash install-yices.sh ./yices-1.0.40-x86_64-unknown-linux-gnu.tar.gz
RUN opam init --disable-sandboxing
RUN opam repo add index https://github.com/voodoos/opam-repository-index.git
RUN opam update
RUN opam switch create --repositories=default,index 4.14.2+index
RUN opam switch set 4.14.2+index
RUN opam install -y dune ocaml-lsp-server ocamlformat user-setup indexing-tools
RUN eval $(opam env)

FROM dev
WORKDIR /workspaces/reaver/
COPY . /workspaces/reaver/
RUN dune build
ENTRYPOINT [ "dune", "exec", "reaver" ]