FROM ubuntu:20.04
ARG DEBIAN_FRONTEND=noninteractive
LABEL authors="ptokarie"

# Make sudo dummy replacement, so we don't weaken docker security
RUN echo "#!/bin/bash\n\$@" > /usr/bin/sudo
RUN chmod +x /usr/bin/sudo

RUN apt update && apt install -y build-essential m4 autoconf libwww-perl git && apt-get clean
COPY ./trunk/src/download.sh .
RUN mkdir -p /root/Downloads/
RUN ./download.sh
WORKDIR /root/Downloads
COPY ./trunk/src/ocaml.sh /root/Downloads
RUN ./ocaml.sh
COPY ./trunk/src/camlidl.sh /root/Downloads
RUN ./camlidl.sh
COPY ./trunk/src/findlib.sh /root/Downloads
RUN ./findlib.sh
COPY ./trunk/src/gmp.sh /root/Downloads
RUN ./gmp.sh
COPY ./trunk/src/mpfr.sh /root/Downloads
RUN ./mpfr.sh
COPY ./trunk/src/eglib.sh /root/Downloads
RUN ./eglib.sh
COPY ./trunk/src/qsopt_ex.sh /root/Downloads
RUN ./qsopt_ex.sh
COPY ./trunk/src/ocamlyices.sh /root/Downloads
RUN ./ocamlyices.sh
COPY ./trunk/src/mlgmpidl.sh /root/Downloads
RUN ./mlgmpidl.sh
COPY ./trunk/src/mlcuddidl.sh /root/Downloads
RUN ./mlcuddidl.sh
COPY ./trunk/src/camllib.sh /root/Downloads
RUN ./camllib.sh
COPY ./trunk/src/fixpoint.sh /root/Downloads
RUN ./fixpoint.sh
COPY ./trunk/src/apron.sh /root/Downloads
RUN ./apron.sh
COPY ./trunk/src/bddapron.sh /root/Downloads
RUN ./bddapron.sh
COPY . /reaver/
WORKDIR /reaver/trunk/src/
RUN ./finish.sh
RUN make
RUN make install
ENV OCAMLRUNPARAM=b
ENTRYPOINT ["./reaver.opt"]