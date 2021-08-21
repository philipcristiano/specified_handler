PROJECT = specified_handler
PROJECT_DESCRIPTION = New project
PROJECT_VERSION = 0.1.0

BUILD_DEPS = erlfmt
DEPS = jsx
TEST_DEPS = meck trails

TEST_DIR = tests
DIALYZER_DIRS = --src src tests

dep_erlfmt = git https://github.com/WhatsApp/erlfmt.git v0.8.0
dep_trails = hex 2.0.0

include erlang.mk

erlfmt:
	$(gen_verbose) $(SHELL_ERL) -pa $(SHELL_PATHS) -eval 'erlfmt_cli:do("erlfmt", [write, {files, ["src/*.erl", "tests/*.erl"]} ]), halt(0)'

erlfmt_check:
	$(gen_verbose) $(SHELL_ERL) -pa $(SHELL_PATHS) -eval 'erlfmt_cli:do("erlfmt", [check, {files, ["src/*.erl", "tests/*.erl"]} ]), halt(0)'

