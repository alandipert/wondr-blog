POSTS := $(wildcard posts/*.md)
XMLS := $(POSTS:%.md=%.xml)
HTMLS := $(XMLS:%.xml=%.html)

all: index.html

index.html: index.xml $(HTMLS)
	sblg -o index.html -t index.xml $(HTMLS)

$(HTMLS): article.xml $(XMLS)
	sblg -t article.xml -L $(XMLS)

%.xml: %.md
	@printf "✓ %s\n" $(shell basename $<)
	@( echo "<?xml version=\"1.0\" encoding=\"UTF-8\" ?>" ; \
	 	 echo "<article data-sblg-article=\"1\">" ; \
		 lowdown $< ; \
		 echo "</article>" ; ) > $@

clean:
	rm -f index.html $(XMLS) $(HTMLS)

dev:
	ag -l | entr -s 'make && xvkbd -window Firefox -text "\Cr"'

print-%:
	@echo '$*=$($*)'
