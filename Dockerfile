FROM fpco/stack-build:lts-14.27
USER root
RUN mkdir -p /opt/lightson/
ARG BINARY_PATH
WORKDIR /opt/lightson
COPY "./.stack-work/dist/x86_64-linux/Cabal-2.4.0.1/build/lightson-exe" /opt/lightson
CMD ["/opt/lightson/lightson-exe"]
