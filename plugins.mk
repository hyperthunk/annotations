#
# Author: Jean Parpaillon <jean.parpaillon@free.fr>
#
# erlang.mk plugin for erlang annotations
#
# Usage: declare annotation modules in ANNOTATIONS variable
#
THIS := $(dir $(realpath $(lastword $(MAKEFILE_LIST))))

ifneq ($(ANNOTATIONS),)

COMPILE_FIRST += $(ANNOTATIONS)

$(PROJECT).d:: annotations.config

distclean:: clean-annotations-config

define annotation_config
[
  {registered, [$(call comma_list,$(1))]}
].

endef

annotations.config: Makefile
	$(verbose) printf "$(subst $(newline),\n,$(subst ",\",$(call annotation_config,$(ANNOTATIONS))))" > $@

clean-annotations-config:
	-rm -f annotations.config

.PHONY: clean-annotations-config

endif
