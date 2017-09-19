# -*- mode: makefile-gmake; -*-
POSTS_MD := $(wildcard posts/*.md)
POSTS_HTML := $(POSTS_MD:%.md=%.html)
SCSS := $(shell find stylesheets -type f -name '*.scss')

all: index.html style.css $(POSTS_HTML)

style.css: $(SCSS)
	sass stylesheets/style.scss > style.css

index.html: templates/index.html $(POSTS_HTML) scripts/gen.el
	./scripts/gen.el index "posts" $< > $@

%.html: %.md templates/article.html scripts/gen.el
	./scripts/gen.el post $< > $@

clean:
	rm -f index.html style.css $(POSTS_HTML)

print-%:
	@echo '$*=$($*)'

.PHONY: clean print-%
