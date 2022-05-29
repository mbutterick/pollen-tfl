export POLLEN := BUILD
.PHONY: build go build-helper quick test

build: test build-helper go

go:
	raco pollen render -p
	raco pollen publish

build-helper:
	raco pollen reset
	raco pollen setup -p

quick: go

test:
	echo $${POLLEN}