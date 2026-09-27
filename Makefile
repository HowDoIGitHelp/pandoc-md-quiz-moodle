MERGEBIN := $(shell cabal list-bin merge-md)
FILTERBIN := $(shell cabal list-bin md-quiz-moodle)
QUESTIONS := examples/exampleQuestions.md
ANSWERS := examples/exampleAnswers.md
OUTPUT := examples/output.xml

all: build merge

merge:
	> $(OUTPUT)
	$(MERGEBIN) -q $(QUESTIONS) -a $(ANSWERS) | \
		pandoc -t json | \
		$(FILTERBIN) \ | \
		pandoc -f json \
		-t plain | \
		XMLLINT_INDENT="    " xmllint --format - --output $(OUTPUT)

build:
	cabal build

clean:
	rm -r dist-newstyle
