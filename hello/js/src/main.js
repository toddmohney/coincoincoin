const Web3 = require("web3");
const Q = require('q');

const { congressContract, congressContractAddr } = require("./congressContract.js");

const web3 = new Web3(Web3.givenProvider || 'ws://localhost:8546');


const app = Elm.Main.fullscreen(localStorage.session || null);


app.ports.getVotingRules.subscribe((_) => {
  Q.all([
    congressContract.methods.minimumQuorum().call({
      from: congressContractAddr
    }),
    congressContract.methods.debatingPeriodInMinutes().call({
      from: congressContractAddr
    }),
    congressContract.methods.majorityMargin().call({
      from: congressContractAddr
    })
  ])
  .then((result) => {
    console.log("getVotingRules", result);
    app.ports.votingRulesReceived.send({
      minimumQuorum: Number(result[0]),
      debatingPeriodInMinutes: Number(result[1]),
      majorityMargin: Number(result[2])
    });
  }).catch(() => {
    console.error("getVotingRules", arguments);
  });
});


app.ports.getProposal.subscribe((proposalNum) => {
  congressContract.methods.proposals(proposalNum).call({
    from: congressContractAddr
  })
  .then((result) => {
    console.log("getProposal", result);
    app.ports.proposalReceived.send({
      amount: result.amount,
      currentResult: Number(result.currentResult),
      description: result.description,
      executed: result.executed,
      numberOfVotes: Number(result.numberOfVotes),
      proposalPassed: result.proposalPassed,
      proposalHash: result.proposalHash,
      recipient: result.recipient,
      votingDeadline: Number(result.votingDeadline)
    });
  }).catch(() => {
    console.error("getProposal", arguments);
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
  const proposalSupport = req.proposalSupport || false;
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

    app.ports.votedTxAddressCreated.send(hash);
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
