PROJECT = annotations
PROJECT_DESCRIPTION = MetaData and Code Instrumentation

DEPS = parse_trans

dep_parse_trans = git https://github.com/esl/parse_trans.git master

include erlang.mk
