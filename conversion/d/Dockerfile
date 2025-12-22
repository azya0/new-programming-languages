FROM dlang2/ldc-ubuntu:latest

WORKDIR /app

COPY dub.json .
COPY ./source ./source

RUN dub build

RUN mkdir -p input output

ENTRYPOINT ["./yaml-to-toml"]
