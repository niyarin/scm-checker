FROM ubuntu:18.04

RUN apt-get update
RUN apt-get install -y git wget g++-i686-linux-gnu g++ g++-aarch64-linux-gnu make
RUN apt-get clean \
    && rm -rf /var/lib/apt/lists/*

RUN wget https://code.call-cc.org/releases/5.4.0/chicken-5.4.0.tar.gz
RUN tar xzvf chicken-5.4.0.tar.gz

WORKDIR /chicken-5.4.0
RUN make -j"$(nproc)"
RUN make install
RUN chicken-install r7rs
RUN chicken-install srfi-113

RUN rm -rf chicken-5.4.0
WORKDIR /
RUN tar xzvf chicken-5.4.0.tar.gz
RUN cp -r chicken-5.4.0 chicken-5.4.0-i686

RUN mkdir /cross-chicken
RUN mkdir /i686-target
RUN mkdir /aarch64-target

WORKDIR /chicken-5.4.0-i686

RUN make  ARCH= \
        PREFIX=/usr \
        PLATFORM=linux \
        HOSTSYSTEM=i686-linux-gnu \
        DESTDIR=/i686-target \
        install

RUN make clean

RUN make  PLATFORM=linux \
          PREFIX=/cross-chicken \
          TARGETSYSTEM=i686-linux-gnu \
          PROGRAM_PREFIX=i686- \
          TARGET_PREFIX=/i686-target/usr \
          TARGET_RUN_PREFIX=/usr \
          install

RUN /cross-chicken/bin/i686-chicken-install r7rs
RUN /cross-chicken/bin/i686-chicken-install srfi-113

WORKDIR /
RUN cp -r chicken-5.4.0 chicken-5.4.0-aarch64
WORKDIR /chicken-5.4.0-aarch64

RUN make  ARCH= \
        PREFIX=/usr \
        PLATFORM=linux \
        HOSTSYSTEM=aarch64-linux-gnu \
        DESTDIR=/aarch64-target \
        install
RUN make clean
RUN make  PLATFORM=linux \
          PREFIX=/cross-chicken \
          TARGETSYSTEM=aarch64-linux-gnu \
          PROGRAM_PREFIX=aarch64- \
          TARGET_PREFIX=/aarch64-target/usr \
          TARGET_RUN_PREFIX=/usr \
          install

WORKDIR /
RUN /cross-chicken/bin/aarch64-chicken-install r7rs
RUN /cross-chicken/bin/aarch64-chicken-install srfi-113

COPY ./entrypoint.sh /entrypoint.sh
RUN chmod +x /entrypoint.sh

ENTRYPOINT ["bash", "/entrypoint.sh"]
CMD ["latest"]
