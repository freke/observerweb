.PHONY: _build elm erlang

REBAR = ./rebar
ELM		= ./elm
ELM_FORMAT	= ./elm-format

all: elm erlang

elm:
	@$(ELM_FORMAT) apps/observerweb/elm --yes
	@$(ELM) make --warn apps/observerweb/elm/Main.elm --output=apps/observerweb/priv/js/observerweb.js

erlang:
	@$(REBAR) compile

clean:
	rm -Rf apps/observerweb/priv/js/*
	rm -Rf elm-stuff/build-artifacts
	@$(REBAR) clean

shell: elm erlang
	@$(REBAR) shell

rel:
	mkdir -p release
	docker build -t observerweb -f docker/prod.dockerfile .
