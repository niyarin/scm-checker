FROM alpine:3.21 AS gauche-builder
RUN apk add alpine-sdk
RUN apk add make
RUN apk add tar


WORKDIR /
RUN wget https://github.com/shirok/Gauche/releases/download/release0_9_15/Gauche-0.9.15.tgz
RUN tar xzvf Gauche-0.9.15.tgz
WORKDIR Gauche-0.9.15

RUN mkdir /gauche-bin/
RUN ./configure --prefix /gauche-bin/
RUN make
RUN make install


FROM alpine:3.21 AS getter

RUN apk update
RUN apk add git


WORKDIR /
RUN git clone https://github.com/niyarin/scm-checker
WORKDIR /scm-checker
RUN git submodule update --init

FROM alpine:3.21 AS runtime

COPY --from=gauche-builder /gauche-bin /gauche-bin

COPY --from=getter /scm-checker /scm-checker

WORKDIR /scm-checker

CMD ["/gauche-bin/bin/gosh", "-I", "./src", "-I", "./src/scheme-reader/", "src/main.scm", "-"]
