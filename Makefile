.PHONY: help create-account
.DEFAULT_GOAL := help

GETH_USER ?= "coincoincoin"
GETH_NETWORK_ID ?= 15

TRUFFLE_NETWORK_TARGET ?= "ganache"

# you're going to want to override this with your own address
COINBASE ?= "b3ed286c1d088016589b5d2b0729a73a1e24f8a7"

help:
	@grep -E '^[a-zA-Z_-]+:.*?## .*$$' $(MAKEFILE_LIST) | awk 'BEGIN {FS = ":.*?## "}; {printf "\033[36m%-30s\033[0m %s\n", $$1, $$2}'

create-account: ## Creates a new account with keys managed by Geth
	GETH_USER=$(GETH_USER) \
		docker-compose run -u $(GETH_USER) --rm geth \
		geth account new

init-blockchain: ## Creates the genesis block from genesis.json
	GETH_USER=$(GETH_USER) \
		docker-compose run -u $(GETH_USER) --rm geth \
		geth init /home/$(GETH_USER)/genesis.json

start-mining: build ## Starts a mining node
	GETH_USER=$(GETH_USER) \
		GETH_NETWORK_ID=$(GETH_NETWORK_ID) \
		COINBASE=$(COINBASE) \
		docker-compose up -d && \
		docker-compose ps

console: ## Begins a Geth console session
	GETH_USER=$(GETH_USER) \
		GETH_NETWORK_ID=$(GETH_NETWORK_ID) \
		docker-compose exec geth \
		geth attach ipc:/home/coincoincoin/.ethereum/geth.ipc

build: build-geth build-dapp build-node-event-producer build-node-event-consumer build-block-explorer ## Builds all docker images

build-geth:
	GETH_USER=$(GETH_USER) \
		GETH_NETWORK_ID=$(GETH_NETWORK_ID) \
		COINBASE=$(COINBASE) \
		docker-compose build geth

build-dapp:
	cd hello && \
	  make build && \
	  docker-compose build dapp

build-block-explorer:
	docker-compose build block-explorer

build-node-event-producer:
	docker-compose build node-event-producer

build-node-event-consumer:
	cd coincoincoin-hs && $(MAKE) image
	docker-compose build congress-event-consumer

sh: ## Starts a bash session inside of the geth container
	GETH_USER=$(GETH_USER) \
		docker-compose run -u $(GETH_USER) --rm geth bash

start-test-rpc-server:
	testrpc --port 8555 --gasPrice 1 --networkId 16

test: deploy
	cd contracts && \
		truffle test --network $(TRUFFLE_NETWORK_TARGET)

reset-network:
	cd contracts && \
		truffle migrate --reset --network $(TRUFFLE_NETWORK_TARGET)

deploy: deploy-contracts import-contracts

deploy-contracts:
	cd contracts && \
		truffle migrate --network $(TRUFFLE_NETWORK_TARGET)

import-contracts:
	contract-importer -c postgres://coincoincoin:coincoincoin@localhost:5432/coincoincoin ./contracts/build/contracts

unlock:
	GETH_USER=$(GETH_USER) \
		GETH_NETWORK_ID=$(GETH_NETWORK_ID) \
		COINBASE=$(COINBASE) \
		docker-compose run -u $(GETH_USER) --rm geth \
		geth --unlock $(COINBASE)

psql:
	docker-compose exec postgres \
		psql -U coincoincoin coincoincoin

init-test-db:
	docker-compose exec postgres \
		createdb -U coincoincoin coincoincoin_test || true
