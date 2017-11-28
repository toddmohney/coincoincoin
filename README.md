# CoinCoinCoin

![](https://defenders.org/sites/default/files/styles/homepage-feature-2015/public/black-bear_duane-cross.png?itok=zDF-t85w)

## Setup

### Build docker images

```sh
make build
```

### Create an account to use as your coinbase

Record the address you receive after running the command. You'll need it in
the following steps

```sh
make create-account
```

If you're creating a new network...

### Create the genesis block

1. Replace the address found in the `alloc` object in `genesis.json` with the address created in the previous step

2. Replace the address found in the `COINBASE` variable in `Makefile` with your address

Now seed your new blockchain with your genesis block.

```sh
make init-blockchain
```

### Start mining

```sh
make start-mining
```

At this point, your Ethereum node will begin to generate its DAG (Directed-Acyclic Graph) which will take some time. You can watch the progress by running `docker-compose logs -f geth` and note the _percentage_ output by the logs. Expect this process to take ~15 mins. Once this one-time setup process completes, mining will begin.

Once the node starts mining, check out your blockchain in a web browser at

```sh
http://localhost:8000
```

Try looking up the address you created in the `make create-account` step.

## Contract deployment

In a terminal run

```sh
make console

personal.unlockAccount(<address>, <password>)
```

In another terminal run

```sh
# Make sure to record the addresses returned for each of
# your deployed contracts!
make deploy
```

The output of `make deploy` will display the contracts deployed to the blockchain via our migration runner. We'll need to record the addresses of our contracts in order to use them via our DApp.

Watch the output for lines that look like

```sh
Running migration: 2_add_congress.js
  Replacing Congress... ...(some hash)
  Congress: 0x321d5513c291de3a2fb63bf4b6e711c34f57ba28
```

Open up `./hello/js/src/congressContract.js` and replace the value set for `congressContractAddr` with `0x321d5513c291de3a2fb63bf4b6e711c34f57ba28` in this case.

## Testing

We're using the [Truffle Framework](http://truffleframework.com/) and a test RPC client for testing.

Note: We'll soon be swapping test RPC for [Ganache](https://github.com/trufflesuite/ganache)

```sh
# test framework
npm install -g truffle
# test ethereum server
npm install -g ethereumjs-testrpc
# solidity compiler (optional)
npm install -g solc
```

## Geth Console

To operate on the blockchain directly via the Geth console, try

```
make console
```

Refer to the Geth documentation for the CLI API

Try the following command to view all accounts

```
# view all accounts
eth.accounts
eth.getBalance("<address>")
```
