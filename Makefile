.PHONY: _build elm erlang

REBAR = ./rebar
ELM		= ./elm
ELM_FORMAT	= ./elm-format
ELM_INSTALL = ./elm-install

all: elm erlang

elm:
	@$(ELM_INSTALL)
	@$(ELM_FORMAT) apps/observerweb/elm --yes
	@$(ELM) make --warn apps/observerweb/elm/Main.elm --output=apps/observerweb/priv/js/observerweb.js --yes

erlang:
	@$(REBAR) compile

test: dialyzer eunit

dialyzer:
	@$(REBAR) dialyzer

eunit:
	@$(REBAR) eunit

ct:
	@$(REBAR) ct

clean:
	rm -Rf apps/observerweb/priv/js/*
	rm -Rf elm-stuff/build-artifacts
	@$(REBAR) clean

shell: elm erlang
	@$(REBAR) shell

rel:
	mkdir -p release
	#docker build --no-cache -t observerweb -f docker/prod.dockerfile .
	docker build -t observerweb -f docker/prod.dockerfile .
