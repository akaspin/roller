GIT=`which git`
ERL=`which erl`
ERLC=`which erlc`
ESCRIPT=`which escript`
REBAR=$(ESCRIPT) rebar

BLD=.build

DEPS=ebin deps/*/ebin

.PHONY: deps doc test

all: deps compile

deps: rebar
	@exec $(REBAR) get-deps
	@exec $(REBAR) update-deps

compile: rebar
	@exec $(REBAR) compile
	
docs: rebar
	@exec $(REBAR) doc

test: compile
	@-rm -rf .eunit
	@-rm -f TEST*
	@mkdir -p .eunit
	@$(REBAR) skip_deps=true eunit

clean: rebar
	@exec $(REBAR) clean
	@-rm -f erl_crash.dump
	@-rm -f TEST*
	
distclean: clean
	@-exec $(REBAR) delete-deps 
	@-rm -rf deps
	
rebar:
	@-rm -rf $(BLD)
	@mkdir -p $(BLD)
	@cd $(BLD) && $(GIT) clone -q git://github.com/basho/rebar.git 
	@cd $(BLD)/rebar && exec make
	@-mv $(BLD)/rebar/rebar .
	@-rm -rf $(BLD)
	