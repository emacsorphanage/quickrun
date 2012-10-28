.PHONY : test

test:
	emacs -Q -batch -L . -l test-quickrun.el -f ert-run-tests-batch-and-exit
