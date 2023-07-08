.PHONY: test

test:
	stack test --test-arguments='--hide-successes'
