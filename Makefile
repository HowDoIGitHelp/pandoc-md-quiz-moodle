MERGEBIN := $(shell cabal list-bin merge-md)
FILTERBIN := $(shell cabal list-bin md-quiz-moodle)
QUESTIONS1 := examples/exampleQuestions.md
ANSWERS1 := examples/exampleAnswers.md
OUTPUT1 := examples/output1.xml
QUESTIONS2 := examples/exampleQuestionsSA.md
ANSWERS2 := examples/exampleAnswersSA.md
OUTPUT2 := examples/output2.xml
QUESTIONS3 := examples/exampleQuestionsCloze.md
ANSWERS3 := examples/exampleAnswersCloze.md
OUTPUT3 := examples/output3.xml

all: build convert-multichoice convert-shortanswer convert-cloze

convert-multichoice:
	> $(OUTPUT1)
	$(MERGEBIN) -q $(QUESTIONS1) -a $(ANSWERS1) | \
		pandoc -t json | \
		$(FILTERBIN) \ | \
		pandoc -f json \
		-t plain | \
		XMLLINT_INDENT="    " xmllint --format - --output $(OUTPUT1)

convert-shortanswer:
	> $(OUTPUT2)
	$(MERGEBIN) -q $(QUESTIONS2) -a $(ANSWERS2) | \
		pandoc -t json | \
		$(FILTERBIN) \ | \
		pandoc -f json \
		-t plain | \
		XMLLINT_INDENT="    " xmllint --format - --output $(OUTPUT2)

convert-cloze:
	> $(OUTPUT3)
	$(MERGEBIN) -q $(QUESTIONS3) -a $(ANSWERS3) | \
		pandoc -t json | \
		$(FILTERBIN) \ | \
		pandoc -f json \
		-t plain | \
		XMLLINT_INDENT="    " xmllint --format - --output $(OUTPUT3)

merge:
	$(MERGEBIN) -q $(QUESTIONS) -a $(ANSWERS)

build:
	cabal build

clean:
	rm -r dist-newstyle
	rm $(OUTPUT1)
	rm $(OUTPUT2)
