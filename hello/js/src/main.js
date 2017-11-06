const Web3 = require("web3");

var web3 = new Web3(Web3.givenProvider || 'http://localhost:8545');
var helloContractAddr = '0x32c9e197951a3674aab69a5df245a27069fb6e5d';
var contractAbi = [
  {
    "constant": true,
    "inputs": [],
    "name": "getHellos",
    "outputs": [
      {
        "name": "",
        "type": "uint256"
      }
    ],
    "payable": false,
    "stateMutability": "view",
    "type": "function"
  },
  {
    "constant": false,
    "inputs": [],
    "name": "sayHello",
    "outputs": [],
    "payable": false,
    "stateMutability": "nonpayable",
    "type": "function"
  },
  {
    "constant": true,
    "inputs": [
      {
        "name": "",
        "type": "address"
      }
    ],
    "name": "hellos",
    "outputs": [
      {
        "name": "",
        "type": "uint256"
      }
    ],
    "payable": false,
    "stateMutability": "view",
    "type": "function"
  }
];

const helloContract = new web3.eth.Contract(contractAbi, helloContractAddr);

const app = Elm.Main.fullscreen(localStorage.session || null);

app.ports.getHelloCount.subscribe(function(address) {
  helloContract.methods.getHellos().call({
    from: address
  })
  .then((result) => {
    app.ports.helloCountReceived.send(Number(result));
  });
});

app.ports.sayHello.subscribe(function(address) {
  helloContract.methods.sayHello().send(
    {
      from: address,
      gasPrice: (20 * 1000000000).toString()
    }
  )
  .once('transactionHash', (hash) => {
    console.log("tx received", hash);
    console.log("waiting for tx to be mined...");
    app.ports.helloTxReceived.send(hash);
  })
  .once('receipt', (receipt) => {
    console.log("tx receipt received", receipt);
    app.ports.helloTxReceiptReceived.send(receipt);
  })
  .on('confirmation', (confNumber, receipt) => {
    console.log("tx confirmed", confNumber, receipt);
    app.ports.helloTxConfirmed.send(confNumber);
  })
  .on('error', (err) => {
    console.log("error!", err);
    app.ports.helloTxError.send(err);
  })
  .then((result) => {
    console.log("our block has been mined!", result);
    app.ports.helloTxMined.send(result);
  })
  .catch((err) => {
    console.log("error caught!", err);
    app.ports.helloTxError.send(err);
  });
});
