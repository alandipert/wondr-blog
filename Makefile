# -*- mode: makefile-gmake; -*-
POSTS_MD := $(wildcard posts/*.md)
POSTS_HTML := $(POSTS_MD:%.md=%.html)
SCSS := $(shell find stylesheets -type f -name '*.scss')
FEED_ID := urn:uuid:3e5be466-173c-4159-996f-17326c665756
TITLE := "Wondr Blog"
AUTHOR := "Alan Dipert"
BLOG_URL := http://example.com
POST_BASE_URL := $(BLOG_URL)/posts
GEN := emacs --quick --script scripts/gen.el

all: atom.xml index.html style.css $(POSTS_HTML)

style.css: $(SCSS)
	sass stylesheets/style.scss > style.css

atom.xml: $(POSTS_MD) scripts/gen.el
	$(GEN) atom "posts" $(TITLE) $(AUTHOR) $(BLOG_URL) $(FEED_ID) $(POST_BASE_URL) > $@

index.html: templates/index.html $(POSTS_HTML) scripts/gen.el
	$(GEN) index "posts" $< > $@

%.html: %.md templates/article.html scripts/gen.el
	$(GEN) post $< > $@

clean:
	rm -f atom.xml index.html style.css $(POSTS_HTML)

print-%:
	@echo '$*=$($*)'

.PHONY: clean print-%
