FROM alpine:3.21 AS gauche-builder
RUN apk add alpine-sdk
RUN apk add make
RUN apk add tar


WORKDIR /
RUN wget https://github.com/shirok/Gauche/releases/download/release0_9_15/Gauche-0.9.15.tgz
RUN tar xzvf Gauche-0.9.15.tgz
WORKDIR Gauche-0.9.15

RUN mkdir /gauche-usr/
RUN ./configure --prefix=/usr
RUN make
RUN make install
RUN make DESTDIR=/gauche-dir/ install


FROM alpine:3.21 AS getter

RUN apk update
RUN apk add git


WORKDIR /
RUN git clone https://github.com/niyarin/scm-checker -b 0.1.6
WORKDIR /scm-checker
RUN git submodule update --init

FROM alpine:3.21 AS runtime

COPY --from=gauche-builder /gauche-dir/usr /usr/
COPY --from=getter /scm-checker /scm-checker
WORKDIR /scm-checker

CMD ["gosh", "-I", "./src", "-I", "./src/scheme-reader/", "src/main.scm", "--long-output", "-"]
