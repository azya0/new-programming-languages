FROM elixir:1.15-alpine

RUN apk add --no-cache build-base

WORKDIR /app

RUN mix local.hex --force && \
    mix local.rebar --force

COPY ./source/main.exs .

CMD ["elixir", "main.exs"]
