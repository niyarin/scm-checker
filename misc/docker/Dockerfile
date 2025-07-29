FROM practicalscheme/gauche:0.9.15

RUN apt-get update && \
    apt-get install -y --no-install-recommends git  && \
    apt-get clean && \
    rm -rf /var/lib/apt/lists/*

WORKDIR /
RUN git clone https://github.com/niyarin/scm-checker
WORKDIR /scm-checker
RUN git submodule update --init
CMD ["/usr/bin/gosh", "-I", "./src", "-I", "./src/scheme-reader/", "src/main.scm", "-"]
