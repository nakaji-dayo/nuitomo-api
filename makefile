DBURL="postgres://api:passwd@localhost:5432/nuitomo-api?sslmode=disable"
DEFAULT_BUILD_OPTS="-j -Wall -Werror"
FAST_BUILD_OPTS="-j -Wall"

TARGET=api

.PHONY: default
default: build

# build app
.PHONY: build
build:
	stack build --ghc-options=$(DEFAULT_BUILD_OPTS) :nuitomo-api-$(TARGET)

.PHONY: build-all
build-all:
	stack build --ghc-options=$(DEFAULT_BUILD_OPTS)

.PHONY: build-watch
build-watch:
	stack build --ghc-options=$(DEFAULT_BUILD_OPTS) --file-watch

.PHONY: fast
fast:
	stack build --fast --ghc-options=$(FAST_BUILD_OPTS)

.PHONY: clean
clean:
	stack clean

# repl
.PHONY: ghci
ghci:
	stack ghci --ghc-options="-j" --ghc-options="-Wall" nuitomo-api:lib

.PHONY: ghci-test
ghci-test:
	stack ghci --ghc-options="-j" --ghc-options="-Wall" nuitomo-api:lib nuitomo-api:test:nuitomo-api-test

# migrate postgresql database
.PHONY: migrate-up
migrate-up:
	migrate -path migrations/ -database $(DBURL) up
	./scripts/genTables.sh

# migrate down postgresql database
.PHONY: migrate-down
migrate-down:
	migrate -path migrations/ -database $(DBURL) down
	./scripts/genTables.sh

# list dependencies
.PHONY: deps
deps:
	stack list-dependencies

# run linter
.PHONY: lint
lint:
	hlint ./src --report
	hlint ./test --report

# run tests
.PHONY: test
test:
	stack test --ghc-options=$(DEFAULT_BUILD_OPTS)

# generate library document
.PHONY: doc
doc:
	stack haddock --ghc-options=$(DEFAULT_BUILD_OPTS) --open