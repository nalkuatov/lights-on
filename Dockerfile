FROM fpco/stack-build:lts-14.27
USER root
RUN mkdir -p /opt/covid-project/
ARG BINARY_PATH
WORKDIR /opt/covid-project
COPY "./.stack-work/dist/x86_64-linux/Cabal-2.4.0.1/build/covid-project-exe" /opt/covid-project
CMD ["/opt/covid-project/covid-project-exe"]
