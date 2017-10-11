FROM alpine:latest

RUN sed -i -e 's/v3\.2/edge/g' /etc/apk/repositories && echo "http://dl-4.alpinelinux.org/alpine/edge/community" >> /etc/apk/repositories
RUN apk update
RUN apk upgrade --available
RUN apk add --update-cache musl zlib ncurses-libs libgcc libstdc++
RUN rm -rf /var/cache/apk/*
RUN apk add --update-cache build-base git bash file
RUN apk add --update-cache erlang erlang-asn1 erlang-common-test erlang-compiler erlang-cosevent erlang-coseventdomain erlang-cosfiletransfer erlang-cosnotification erlang-cosproperty erlang-costime erlang-costransaction erlang-crypto erlang-debugger erlang-dev erlang-dialyzer erlang-diameter erlang-edoc erlang-eldap erlang-erl-docgen erlang-erl-interface erlang-erts erlang-et erlang-eunit erlang-hipe erlang-ic erlang-inets erlang-jinterface erlang-kernel erlang-megaco erlang-mnesia erlang-observer erlang-odbc erlang-orber erlang-os-mon erlang-otp-mibs erlang-parsetools erlang-public-key erlang-reltool erlang-runtime-tools erlang-sasl erlang-snmp erlang-ssh erlang-ssl erlang-stdlib erlang-syntax-tools erlang-tools erlang-xmerl
RUN rm -rf /var/cache/apk/*

RUN mkdir /opt
WORKDIR /opt
RUN rm -fr ~/.cache/rebar3 _build
RUN git clone https://github.com/erlang/rebar3.git
WORKDIR /opt/rebar3
RUN ./bootstrap
RUN ln -s /opt/rebar3/rebar3 /bin/rebar3
WORKDIR /src/observerweb

EXPOSE 8080

ENTRYPOINT []
