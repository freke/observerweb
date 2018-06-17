FROM codesimple/elm:0.18 AS elm_build
RUN npm install -g bower
COPY apps/observerweb/elm /code
COPY bower.json /code
COPY elm-package.json /code
WORKDIR /code
RUN bower --allow-root install
RUN elm make --yes Main.elm --output=/build/observerweb.js

FROM alpine:edge AS erlang_base
RUN apk update
RUN apk upgrade --available
RUN apk add --update-cache musl zlib ncurses-libs libgcc libstdc++
RUN rm -rf /var/cache/apk/*

FROM erlang_base AS erlang_build_base
RUN apk update
RUN apk upgrade --available
RUN apk add --update-cache musl zlib ncurses-libs libgcc libstdc++
RUN rm -rf /var/cache/apk/*
RUN apk add --update-cache build-base git bash file
RUN apk add --update-cache erlang erlang-asn1 erlang-common-test erlang-compiler erlang-cosevent erlang-coseventdomain erlang-cosfiletransfer erlang-cosnotification erlang-cosproperty erlang-costime erlang-costransaction erlang-crypto erlang-debugger erlang-dev erlang-dialyzer erlang-diameter erlang-edoc erlang-eldap erlang-erl-docgen erlang-erl-interface erlang-erts erlang-et erlang-eunit erlang-hipe erlang-ic erlang-inets erlang-jinterface erlang-kernel erlang-megaco erlang-mnesia erlang-observer erlang-odbc erlang-orber erlang-os-mon erlang-otp-mibs erlang-parsetools erlang-public-key erlang-reltool erlang-runtime-tools erlang-sasl erlang-snmp erlang-ssh erlang-ssl erlang-stdlib erlang-syntax-tools erlang-tools erlang-xmerl erlang-wx
RUN rm -rf /var/cache/apk/*

RUN mkdir /opt
WORKDIR /opt
RUN rm -fr ~/.cache/rebar3 _build
RUN git clone https://github.com/erlang/rebar3.git
WORKDIR /opt/rebar3
RUN ./bootstrap
RUN ln -s /opt/rebar3/rebar3 /bin/rebar3
COPY apps/observerweb /src/observerweb
COPY rebar.config /src/observerweb/rebar.config
WORKDIR /src/observerweb
RUN rebar3 compile

FROM erlang_build_base
RUN rebar3 dialyzer

FROM erlang_build_base
RUN rebar3 eunit

FROM erlang_build_base AS build_release
COPY config /src/observerweb/config
RUN rebar3 as prod release

FROM erlang_base AS observerweb
RUN apk add --update-cache ncurses-dev
RUN rm -rf /var/cache/apk/*
RUN echo -en '\n' > ~/.hosts.erlang
COPY --from=build_release /src/observerweb/_build/prod/ /
ENTRYPOINT /rel/observerweb/bin/observerweb foreground
EXPOSE 8080
