FROM haskell

WORKDIR /app

COPY ./source/main.hs .

RUN ghc -o btree main.hs

CMD ["./btree"]
