DBURL="postgres://api:passwd@localhost:5432/nuitomo-api?sslmode=disable"
DEFAULT_BUILD_OPTS="-j -Wall -Werror"
FAST_BUILD_OPTS="-j -Wall"

OPTIMIZED_BUILD_OPTS="-j -Wall -O1"

TARGET=api

.PHONY: default
default: build

# build app
.PHONY: build
build:
	stack build --ghc-options=$(DEFAULT_BUILD_OPTS) :nuitomo-$(TARGET)

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

migrate-down-hard:
	psql $(DBURL) -Atc "drop schema public cascade; create schema public;"

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

###
# Docker image
####
STACK_FLAG :=

.PHONY: build-release
build-release:
	stack build $(STACK_FLAG) --ghc-options=$(OPTIMIZED_BUILD_OPTS)

.PHONY: prepare-image
prepare-image: build-release
	rm -rf docker/api/tmp/*
	mkdir -p docker/tmp
	cp config.yaml $(shell stack $(STACK_FLAG) exec which nuitomo-api) docker/tmp/
	cp -r migrations docker/tmp/

.PHONY: image-api
image-api:
	docker build -t nuitomo-api -f ./docker/Dockerfile-api ./docker

.PHONY: image-migrate
image-migrate:
	docker build -t nuitomo-api-migrate -f ./docker/Dockerfile-migrate ./docker
