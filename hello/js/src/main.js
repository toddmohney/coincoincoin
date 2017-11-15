const Web3 = require("web3");

const { helloContract } = require("./helloContract.js");
const { congressContract } = require("./congressContract.js");

const web3 = new Web3(Web3.givenProvider || 'ws://localhost:8546');


const app = Elm.Main.fullscreen(localStorage.session || null);

app.ports.getTx.subscribe((txhash) => {
  web3.eth.getTransaction(txhash)
    .then((tx) => {
      console.log("tx received", tx);
      app.ports.txReceived.send(tx);
    });
});

app.ports.getHelloCount.subscribe((address) => {
  helloContract.methods.getHellos().call({
    from: address
  })
  .then((result) => {
    app.ports.helloCountReceived.send(Number(result));
  });
});

app.ports.sayHello.subscribe((req) => {
  console.log("sayHello", req);
  helloContract.methods.sayHello().send(
    {
      from: req.address,
      gasPrice: req.gasPrice.toString()
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

app.ports.submitProposal.subscribe((req) => {
  console.log("submitProposal", req);

  const beneficiary = req.beneficiary;
  const amount = req.etherAmount;
  const details = req.details;
  const bytecode = web3.utils.fromAscii("");

  congressContract.methods.newProposalInEther(beneficiary, amount, details, bytecode).send(
    {
      from: req.senderAddress,
      gasPrice: req.gasPrice.toString()
    }
  )
  .once('transactionHash', (hash) => {
    console.log("tx received", hash);
    console.log("waiting for tx to be mined...");
  })
  .once('receipt', (receipt) => {
    console.log("tx receipt received", receipt);
  })
  .on('confirmation', (confNumber, receipt) => {
    console.log("tx confirmed", confNumber, receipt);
  })
  .on('error', (err) => {
    console.log("error!", err);
  })
  .then((result) => {
    console.log("our block has been mined!", result);
  })
  .catch((err) => {
    console.log("error caught!", err);
  });
});
