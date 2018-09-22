# -*- mode: makefile-gmake; -*-
POSTS_MD := $(wildcard posts/*.md)
POSTS_HTML := $(POSTS_MD:%.md=%.html)
SCSS := $(shell find stylesheets -type f -name '*.scss')

AUTHOR := "Alan Dipert"
BLOG_URL := https://tailrecursion.com/wondr
FEED_ID := urn:uuid:3e5be466-173c-4159-996f-17326c665756
POST_BASE_URL := $(BLOG_URL)/posts
TITLE := "Wondr Blog"

CF_DIST := E6GOTXLS9MCZF
S3_PATH := s3://tailrecursion.com/wondr
CACHE_CONTROL := max-age=604800

GEN := emacs --quick --script scripts/gen.el

all: public

style.css: $(SCSS)
	sassc stylesheets/style.scss > style.css

code_highlight.css: style.css
	$(GEN) syntax-highlight-css > $@

atom.xml: $(POSTS_MD) scripts/gen.el
	$(GEN) atom "posts" $(TITLE) $(AUTHOR) $(BLOG_URL) $(FEED_ID) $(POST_BASE_URL) > $@

index.html: templates/index.html $(POSTS_HTML) scripts/gen.el
	$(GEN) index "posts" $< > $@

%.html: %.md templates/article.html scripts/gen.el
	$(GEN) post $< > $@

public: atom.xml index.html code_highlight.css style.css $(POSTS_HTML)
	$(shell mkdir -p public/posts)
	cp atom.xml public
	cp index.html public
	cp style.css public
	cp code_highlight.css public
	$(foreach html,$(POSTS_HTML),$(shell cp $(html) public/$(html)))

deploy: public
	aws s3 sync public $(S3_PATH) --cache-control $(CACHE_CONTROL)
	aws cloudfront create-invalidation --distribution-id $(CF_DIST) --paths '/*'

clean:
	rm -f atom.xml index.html style.css code_highlight.css $(POSTS_HTML)
	rm -rf public

print-%:
	@echo '$*=$($*)'

.PHONY: clean print-% deploy
