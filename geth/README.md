# Gustcoin

## Setup

Build docker images

```sh
make build
```

Create an account to use as your coinbase

Record the address you receive after running the command. You'll need it in
the following steps

```sh
make create-account
```

If you're createing a new network...

Create the genesis block
- Replace the address found in the `alloc` object in `genesis.json` with the
address created in the previous step
- Replace the address found in the `COINBASE` variable in `Makefile` with your
address

Initialize your blockchain

```sh
make init-blockchain
```

Start mining!

```sh
make start-mining
```

Check out your blockchain in a web browser at

```sh
http://localhost:8000
```

## Genesis block

## Console

```
make console
```

```
# view all accounts
eth.accounts
eth.getBalance("<address>")
```

## Contract deployment

In a terminal run

```sh
make console

# personal.unlockAccount(<address>, <password>)
```

In another terminal run

```sh
# Make sure to record the addresses returned for each of
# your deployed contracts!
make deploy
```

## Testing

```sh
# test framework
npm install -g truffle
# test ethereum server
npm install -g ethereumjs-testrpc
# solidity compiler (optional)
npm install -g solc
```
