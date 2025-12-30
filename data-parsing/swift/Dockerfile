FROM swift:5.9-jammy

WORKDIR /app

COPY source/main.swift .

RUN swiftc -o app main.swift

ENTRYPOINT ["./app"]