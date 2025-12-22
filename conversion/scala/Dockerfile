FROM eclipse-temurin:11-jre

RUN apt-get update && apt-get install -y wget

RUN wget -O- https://github.com/coursier/launchers/raw/master/cs-x86_64-pc-linux.gz | gzip -d > /usr/local/bin/cs && \
    chmod +x /usr/local/bin/cs

RUN cs install sbt --dir /usr/local/bin

WORKDIR /app

COPY build.sbt .

RUN sbt update

COPY ./source ./src/main/scala/

RUN sbt compile

CMD ["sbt", "run"]
