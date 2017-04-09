SWIPL=swipl
PROLOG_SRC_FILES=$(shell find . -type f -iname '*.pl')
PROLOG_TEST_FILES=$(shell find . -type f -iname '*.plt')

test: $(PROLOG_SRC_FILES) $(PROLOG_TEST_FILES)
	$(SWIPL) run_tests.pl
