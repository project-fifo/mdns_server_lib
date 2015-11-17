REBAR=rebar3
all: compile

compile:
	@$(REBAR) compile

clean: clean-docs
	@$(REBAR) clean

clean-docs:
	rm doc/*.html doc/*.png doc/*.css doc/edoc-info

test: all
	$(REBAR) xref
	$(REBAR) eunit

docs: clean-docs
	@$(REBAR) doc

xref:
	@$(REBAR) xref 

