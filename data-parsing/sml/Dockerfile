FROM ubuntu:22.04

RUN apt-get update && apt-get install -y smlnj

WORKDIR /app

COPY source/main.sml .

CMD ["sml", "-m", "main.sml"]
