FROM daewok/lisp-devel:latest

WORKDIR /app

COPY ./source/main.lisp .

CMD ["sbcl", "--script", "main.lisp"]
