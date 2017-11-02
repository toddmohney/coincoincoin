.PHONY: help create-account
.DEFAULT_GOAL := help

GETH_USER ?= "gustcoin"
GETH_NETWORK_ID ?= 1978

# you're going to want to override this with your own address
COINBASE ?= "31c552f7045abf6287e38c6eb719550d6b0ec1fc"

help:
	@grep -E '^[a-zA-Z_-]+:.*?## .*$$' $(MAKEFILE_LIST) | awk 'BEGIN {FS = ":.*?## "}; {printf "\033[36m%-30s\033[0m %s\n", $$1, $$2}'

create-account:
	GETH_USER=$(GETH_USER) \
		docker-compose run -u $(GETH_USER) --rm geth \
		geth account new

init-blockchain:
	GETH_USER=$(GETH_USER) \
		docker-compose run -u $(GETH_USER) --rm geth \
		geth init /home/$(GETH_USER)/genesis.json

start-mining:
	GETH_USER=$(GETH_USER) \
		GETH_NETWORK_ID=$(GETH_NETWORK_ID) \
		COINBASE=$(COINBASE) \
		docker-compose up -d && \
		docker-compose ps

console:
	GETH_USER=$(GETH_USER) \
		GETH_NETWORK_ID=$(GETH_NETWORK_ID) \
		docker-compose exec geth \
		geth attach ipc:/home/gustcoin/.ethereum/geth.ipc

build: build-geth build-block-explorer

build-geth:
	GETH_USER=$(GETH_USER) \
		GETH_NETWORK_ID=$(GETH_NETWORK_ID) \
		COINBASE=$(COINBASE) \
		docker-compose build geth

build-block-explorer:
	docker-compose build block-explorer

sh:
	GETH_USER=$(GETH_USER) \
		docker-compose run -u $(GETH_USER) --rm geth bash
