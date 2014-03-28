test: ert ecukes

ert:
	emacs -batch -l ert -l emacsagist-*.el -l emacsagist.el -l test/emacsagist-tests.el -f ert-run-tests-batch-and-exit

ecukes:
	cask exec ecukes
