FROM ocaml/opam:alpine

WORKDIR /app

RUN opam install ocaml -y

COPY ./source/main.ml .

RUN eval $(opam env) && ocamlopt -o expr main.ml

CMD ["./expr"]
