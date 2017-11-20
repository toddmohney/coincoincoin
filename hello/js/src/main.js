const Web3 = require("web3");

const { congressContract } = require("./congressContract.js");

const web3 = new Web3(Web3.givenProvider || 'ws://localhost:8546');


const app = Elm.Main.fullscreen(localStorage.session || null);

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

    app.ports.proposalAddedTxHashCreated.send(hash);
  })
  .once('receipt', (receipt) => {
    console.log("tx receipt received", receipt);
  })
  .on('allEvents', (err, evt) => {
    console.log("Event err", err);
    console.log("Event evt", evt);
  })
  .on('confirmation', (confNumber, receipt) => {
    console.log("tx confirmed", confNumber, receipt);
  })
  .on('error', (err) => {
    console.log("error!", err);
  })
  .then((result) => {
    console.log("our block has been mined!", result);

    app.ports.proposalAdded.send(result);
  })
  .catch((err) => {
    console.log("error caught!", err);
  });
});

app.ports.addMember.subscribe((req) => {
  console.log("addMember", req);

  const memberAddress = req.memberAddress;
  const memberName = req.memberName;

  congressContract.methods.addMember(memberAddress, memberName).send(
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
  .on('allEvents', (err, evt) => {
    console.log("Event err", err);
    console.log("Event evt", evt);
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

app.ports.removeMember.subscribe((req) => {
  console.log("removeMember", req);

  const memberAddress = req.memberAddress;

  congressContract.methods.removeMember(memberAddress).send(
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
  .on('allEvents', (err, evt) => {
    console.log("Event err", err);
    console.log("Event evt", evt);
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

app.ports.submitVote.subscribe((req) => {
  console.log("submitVote", req);

  const senderAddress = req.senderAddress;
  const gasPrice = req.gasPrice;

  const proposalNumber = req.proposalNumber;
  const proposalSupport = req.proposalSupport;
  const supportJustification = req.supportJustification;

  congressContract.methods.vote(
    proposalNumber,
    proposalSupport,
    supportJustification
  ).send(
    {
      from: req.senderAddress,
      gasPrice: req.gasPrice.toString()
    }
  )
  .once('transactionHash', (hash) => {
    console.log("tx received", hash);
    console.log("waiting for tx to be mined...");

    app.ports.votedTxHashCreated.send(hash);
  })
  .once('receipt', (receipt) => {
    console.log("tx receipt received", receipt);
  })
  .on('allEvents', (err, evt) => {
    console.log("Event err", err);
    console.log("Event evt", evt);
  })
  .on('confirmation', (confNumber, receipt) => {
    console.log("tx confirmed", confNumber, receipt);
  })
  .on('error', (err) => {
    console.log("error!", err);
  })
  .then((result) => {
    console.log("our block has been mined!", result);

    app.ports.voted.send(result);
  })
  .catch((err) => {
    console.log("error caught!", err);
  });
});
