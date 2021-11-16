.PHONY: compile rel test typecheck ci

REBAR=./rebar3

compile:
	$(REBAR) compile

clean:
	$(REBAR) clean

test: compile
	$(REBAR) eunit

typecheck:
	$(REBAR) dialyzer

ci:compile
	$(REBAR) dialyzer && $(REBAR) as test do eunit
