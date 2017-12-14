const Web3 = require("web3");
const Q = require('q');

const { congressContractAbi, congressContractAddr } = require("./congressContract.js");

// web3 is injected into the page by MetaMask
let w3;
let blockNum = 0;

const app = Elm.Main.fullscreen(localStorage.session || null);

const loadSession = (accts) => {
  console.log("loadSession", accts);

  if(accts.length !== 0) {
    app.ports.sessionLoaded.send(accts[0]);
  }
}

$(document).ready(() => {
  // Checking if Web3 has been injected by the browser (Mist/MetaMask)
  if (typeof web3 !== 'undefined') {
    // Use the browser's ethereum provider
    w3 = new Web3(web3.currentProvider);
    w3.eth.getAccounts().then(loadSession);
    window.setInterval(sendNodeDiagnostics, 3000);
  } else {
    console.log('No web3? You should consider trying MetaMask!')
  }
});

const sendNodeDiagnostics = () => {
  Q.all([
    w3.eth.getCoinbase(),
    w3.eth.isMining(),
    w3.eth.getHashrate(),
    w3.eth.getGasPrice(),
    w3.eth.isSyncing(),
    w3.eth.getBlockNumber(),
    w3.eth.getAccounts()
  ]).then((results) => {
    app.ports.nodeDiagnosticsLoaded.send({
      coinbase: results[0],
      isMining: results[1],
      hashrate: results[2],
      gasPrice: results[3],
      isSyncing: results[4],
      blockNumber: results[5],
      accounts: results[6]
    });
  });
}

app.ports.getVotingRules.subscribe((_) => {
  const congressContract = new w3.eth.Contract(congressContractAbi, congressContractAddr);

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


app.ports.updateVotingRules.subscribe((req) => {
  console.log("updateVotingRules", req);
  const congressContract = new w3.eth.Contract(congressContractAbi, congressContractAddr);

  congressContract.methods.changeVotingRules(
    req.minimumQuorum,
    req.debatingPeriodInMinutes,
    req.majorityMargin
  ).send(
    {
      from: req.senderAddress,
      gasPrice: req.gasPrice.toString()
    }
  )
  .once('transactionHash', (hash) => {
    console.log("tx received", hash);
    console.log("waiting for tx to be mined...");

    app.ports.votingRulesUpdatedTxAddressCreated.send(hash);
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

    app.ports.votingRulesUpdated.send(result);
  })
  .catch((err) => {
    console.log("error caught!", err);
  });
});


app.ports.getProposal.subscribe((proposalNum) => {
  const congressContract = new w3.eth.Contract(congressContractAbi, congressContractAddr);

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

  const congressContract = new w3.eth.Contract(congressContractAbi, congressContractAddr);
  const beneficiary = req.beneficiary;
  const amount = req.etherAmount;
  const details = req.details;
  const bytecode = w3.utils.fromAscii("");

  congressContract.methods.newProposalInEther(
    beneficiary,
    amount,
    details,
    bytecode
  ).send(
    {
      from: req.senderAddress,
      gasPrice: req.gasPrice.toString()
    }
  )
  .once('transactionHash', (hash) => {
    console.log("tx received", hash);
    console.log("waiting for tx to be mined...");

    app.ports.proposalAddedTxAddressCreated.send(hash);
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


app.ports.executeProposal.subscribe((req) => {
  console.log("executeProposal", req);

  const congressContract = new w3.eth.Contract(congressContractAbi, congressContractAddr);
  const proposalID = req.proposalID;
  const bytecode = w3.utils.fromAscii("");

  congressContract.methods.executeProposal(
    proposalID,
    bytecode
  ).send(
    {
      from: req.senderAddress,
      gasPrice: req.gasPrice.toString()
    }
  )
  .once('transactionHash', (hash) => {
    console.log("tx received", hash);
    console.log("waiting for tx to be mined...");

    app.ports.proposalExecutedTxAddressCreated.send(hash);
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

    app.ports.proposalExecuted.send(result);
  })
  .catch((err) => {
    console.log("error caught!", err);
  });
});

app.ports.addMember.subscribe((req) => {
  console.log("addMember", req);

  const congressContract = new w3.eth.Contract(congressContractAbi, congressContractAddr);
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

  const congressContract = new w3.eth.Contract(congressContractAbi, congressContractAddr);
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

  const congressContract = new w3.eth.Contract(congressContractAbi, congressContractAddr);
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
