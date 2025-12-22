FROM julia:1.9-alpine

WORKDIR /app

COPY ./source/main.jl .

ENV JULIA_NUM_THREADS=8

CMD ["julia", "main.jl"]