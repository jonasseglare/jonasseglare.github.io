PASDROWKA_FILES = public/pasdrowka/index.html public/pasdrowka/style.css public/pasdrowka/impl.js
PASDROWKA_BASE = https://raw.githubusercontent.com/jonasseglare/pasdrowka/master

.PHONY: build

build: $(PASDROWKA_FILES)
	bb build

$(PASDROWKA_FILES): public/pasdrowka/%:
	mkdir -p public/pasdrowka
	curl -fsSL $(PASDROWKA_BASE)/$* -o $@

