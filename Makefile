MERGEBIN := $(shell cabal list-bin merge-md)
QUESTIONS := examples/exampleQuestions.md
ANSWERS := examples/exampleAnswers.md

all: build merge

merge:
	$(MERGEBIN) -a $(QUESTIONS) -q $(ANSWERS)

build:
	cabal build

clean:
	rm -r dist-newstyle
