FROM ubuntu:20.04 AS dev
ARG DEBIAN_FRONTEND=noninteractive

# Make sudo dummy replacement, so we don't weaken docker security
RUN echo "#!/bin/bash\n\$@" > /usr/bin/sudo
RUN chmod +x /usr/bin/sudo

RUN apt update && apt install -y build-essential m4 autoconf git npm wget curl libgmp-dev libmpfr-dev && apt-get clean
RUN npm install -g --unsafe-perm esy

WORKDIR /root/vendor/
RUN wget https://yices.csl.sri.com/old/binaries/yices-1.0.40-x86_64-unknown-linux-gnu.tar.gz
RUN wget https://raw.githubusercontent.com/polazarus/ocamlyices/448a098165c996aaa2d0428808749e88d0a2d0a0/install-yices.sh
RUN bash install-yices.sh ./yices-1.0.40-x86_64-unknown-linux-gnu.tar.gz


FROM dev
WORKDIR /workspaces/reaver/
COPY . /workspaces/reaver/
RUN esy install
RUN esy
ENTRYPOINT [ "esy", "x", "reaver" ]