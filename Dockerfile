FROM ubuntu:20.04
ARG DEBIAN_FRONTEND=noninteractive
LABEL authors="ptokarie"

# Make sudo dummy replacement, so we don't weaken docker security
RUN echo "#!/bin/bash\n\$@" > /usr/bin/sudo
RUN chmod +x /usr/bin/sudo

RUN apt update && apt install -y build-essential m4 autoconf git && apt-get clean
RUN mkdir ~/vendor
WORKDIR /root/vendor
COPY ./vendor/ocaml.sh /root/vendor
COPY ./vendor/ocaml-4.02.0.tar.gz /root/vendor
RUN ./ocaml.sh
COPY ./vendor/camlidl.sh /root/vendor
COPY ./vendor/camlidl-1.05.tar.gz /root/vendor
RUN ./camlidl.sh
COPY ./vendor/findlib.sh /root/vendor
COPY ./vendor/findlib-1.9.6.tar.gz /root/vendor
RUN ./findlib.sh
COPY ./vendor/gmp.sh /root/vendor
COPY ./vendor/gmp-5.1.3.tar.bz2 /root/vendor
RUN ./gmp.sh
COPY ./vendor/mpfr.sh /root/vendor
COPY ./vendor/mpfr-3.1.6.tar.bz2 /root/vendor
RUN ./mpfr.sh
COPY ./vendor/eglib.sh /root/vendor
COPY ./vendor/EGlib.tar.bz2 /root/vendor
RUN ./eglib.sh
COPY ./vendor/qsopt_ex.sh /root/vendor
COPY ./vendor/QSoptExact.tar.bz2 /root/vendor
RUN ./qsopt_ex.sh
COPY ./vendor/ocamlyices.sh /root/vendor
COPY ./vendor/ocamlyices /root/vendor/ocamlyices
COPY ./vendor/yices-1.0.40-x86_64-unknown-linux-gnu.tar.gz /root/vendor/
RUN ./ocamlyices.sh
COPY ./vendor/mlgmpidl.sh /root/vendor
COPY ./vendor/mlgmpidl /root/vendor/mlgmpidl
RUN ./mlgmpidl.sh
COPY ./vendor/mlcuddidl.sh /root/vendor
COPY ./vendor/mlcuddidl /root/vendor/mlcuddidl
RUN ./mlcuddidl.sh
COPY ./vendor/camllib.sh /root/vendor
COPY ./vendor/camllib /root/vendor/camllib
RUN ./camllib.sh
COPY ./vendor/fixpoint.sh /root/vendor
COPY ./vendor/fixpoint-2.3.2.tar.gz /root/vendor
RUN ./fixpoint.sh
COPY ./vendor/apron.sh /root/vendor
COPY ./vendor/apron-0.9.11.tar.gz /root/vendor
RUN ./apron.sh
COPY ./vendor/bddapron.sh /root/vendor
COPY ./vendor/bddapron-2.2.1.tar.gz /root/vendor
RUN ./bddapron.sh
COPY ./trunk /reaver/
WORKDIR /reaver/src/
RUN ./install.sh
ENV OCAMLRUNPARAM=b
ENTRYPOINT ["./reaver.opt"]