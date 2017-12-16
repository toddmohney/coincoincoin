const Web3 = require("web3");
const Kafka = require('node-rdkafka');
const sleep = require('sleep');
var _ = require('lodash');

console.log("hi!");

const congressContractAddr = '0x321d5513c291de3a2fb63bf4b6e711c34f57ba28';
const congressContractAbi = [
  {
    "constant": true,
    "inputs": [
      {
        "name": "",
        "type": "uint256"
      }
    ],
    "name": "proposals",
    "outputs": [
      {
        "name": "recipient",
        "type": "address"
      },
      {
        "name": "amount",
        "type": "uint256"
      },
      {
        "name": "description",
        "type": "string"
      },
      {
        "name": "votingDeadline",
        "type": "uint256"
      },
      {
        "name": "executed",
        "type": "bool"
      },
      {
        "name": "proposalPassed",
        "type": "bool"
      },
      {
        "name": "numberOfVotes",
        "type": "uint256"
      },
      {
        "name": "currentResult",
        "type": "int256"
      },
      {
        "name": "proposalHash",
        "type": "bytes32"
      }
    ],
    "payable": false,
    "stateMutability": "view",
    "type": "function"
  },
  {
    "constant": false,
    "inputs": [
      {
        "name": "targetMember",
        "type": "address"
      }
    ],
    "name": "removeMember",
    "outputs": [],
    "payable": false,
    "stateMutability": "nonpayable",
    "type": "function"
  },
  {
    "constant": false,
    "inputs": [
      {
        "name": "proposalNumber",
        "type": "uint256"
      },
      {
        "name": "transactionBytecode",
        "type": "bytes"
      }
    ],
    "name": "executeProposal",
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
    "name": "memberId",
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
    "constant": true,
    "inputs": [],
    "name": "numProposals",
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
    "constant": true,
    "inputs": [
      {
        "name": "",
        "type": "uint256"
      }
    ],
    "name": "members",
    "outputs": [
      {
        "name": "member",
        "type": "address"
      },
      {
        "name": "name",
        "type": "string"
      },
      {
        "name": "memberSince",
        "type": "uint256"
      }
    ],
    "payable": false,
    "stateMutability": "view",
    "type": "function"
  },
  {
    "constant": true,
    "inputs": [],
    "name": "debatingPeriodInMinutes",
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
    "constant": true,
    "inputs": [],
    "name": "minimumQuorum",
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
    "constant": true,
    "inputs": [],
    "name": "owner",
    "outputs": [
      {
        "name": "",
        "type": "address"
      }
    ],
    "payable": false,
    "stateMutability": "view",
    "type": "function"
  },
  {
    "constant": false,
    "inputs": [
      {
        "name": "_from",
        "type": "address"
      },
      {
        "name": "_value",
        "type": "uint256"
      },
      {
        "name": "_token",
        "type": "address"
      },
      {
        "name": "_extraData",
        "type": "bytes"
      }
    ],
    "name": "receiveApproval",
    "outputs": [],
    "payable": false,
    "stateMutability": "nonpayable",
    "type": "function"
  },
  {
    "constant": true,
    "inputs": [],
    "name": "majorityMargin",
    "outputs": [
      {
        "name": "",
        "type": "int256"
      }
    ],
    "payable": false,
    "stateMutability": "view",
    "type": "function"
  },
  {
    "constant": false,
    "inputs": [
      {
        "name": "beneficiary",
        "type": "address"
      },
      {
        "name": "weiAmount",
        "type": "uint256"
      },
      {
        "name": "jobDescription",
        "type": "string"
      },
      {
        "name": "transactionBytecode",
        "type": "bytes"
      }
    ],
    "name": "newProposal",
    "outputs": [
      {
        "name": "proposalID",
        "type": "uint256"
      }
    ],
    "payable": false,
    "stateMutability": "nonpayable",
    "type": "function"
  },
  {
    "constant": false,
    "inputs": [
      {
        "name": "beneficiary",
        "type": "address"
      },
      {
        "name": "etherAmount",
        "type": "uint256"
      },
      {
        "name": "jobDescription",
        "type": "string"
      },
      {
        "name": "transactionBytecode",
        "type": "bytes"
      }
    ],
    "name": "newProposalInEther",
    "outputs": [
      {
        "name": "proposalID",
        "type": "uint256"
      }
    ],
    "payable": false,
    "stateMutability": "nonpayable",
    "type": "function"
  },
  {
    "constant": false,
    "inputs": [
      {
        "name": "minimumQuorumForProposals",
        "type": "uint256"
      },
      {
        "name": "minutesForDebate",
        "type": "uint256"
      },
      {
        "name": "marginOfVotesForMajority",
        "type": "int256"
      }
    ],
    "name": "changeVotingRules",
    "outputs": [],
    "payable": false,
    "stateMutability": "nonpayable",
    "type": "function"
  },
  {
    "constant": false,
    "inputs": [
      {
        "name": "targetMember",
        "type": "address"
      },
      {
        "name": "memberName",
        "type": "string"
      }
    ],
    "name": "addMember",
    "outputs": [],
    "payable": false,
    "stateMutability": "nonpayable",
    "type": "function"
  },
  {
    "constant": false,
    "inputs": [
      {
        "name": "proposalNumber",
        "type": "uint256"
      },
      {
        "name": "supportsProposal",
        "type": "bool"
      },
      {
        "name": "justificationText",
        "type": "string"
      }
    ],
    "name": "vote",
    "outputs": [
      {
        "name": "voteID",
        "type": "uint256"
      }
    ],
    "payable": false,
    "stateMutability": "nonpayable",
    "type": "function"
  },
  {
    "constant": true,
    "inputs": [
      {
        "name": "proposalNumber",
        "type": "uint256"
      },
      {
        "name": "beneficiary",
        "type": "address"
      },
      {
        "name": "weiAmount",
        "type": "uint256"
      },
      {
        "name": "transactionBytecode",
        "type": "bytes"
      }
    ],
    "name": "checkProposalCode",
    "outputs": [
      {
        "name": "codeChecksOut",
        "type": "bool"
      }
    ],
    "payable": false,
    "stateMutability": "view",
    "type": "function"
  },
  {
    "constant": false,
    "inputs": [
      {
        "name": "newOwner",
        "type": "address"
      }
    ],
    "name": "transferOwnership",
    "outputs": [],
    "payable": false,
    "stateMutability": "nonpayable",
    "type": "function"
  },
  {
    "inputs": [
      {
        "name": "minimumQuorumForProposals",
        "type": "uint256"
      },
      {
        "name": "minutesForDebate",
        "type": "uint256"
      },
      {
        "name": "marginOfVotesForMajority",
        "type": "int256"
      }
    ],
    "payable": true,
    "stateMutability": "payable",
    "type": "constructor"
  },
  {
    "payable": true,
    "stateMutability": "payable",
    "type": "fallback"
  },
  {
    "anonymous": false,
    "inputs": [
      {
        "indexed": false,
        "name": "proposalID",
        "type": "uint256"
      },
      {
        "indexed": false,
        "name": "recipient",
        "type": "address"
      },
      {
        "indexed": false,
        "name": "amount",
        "type": "uint256"
      },
      {
        "indexed": false,
        "name": "description",
        "type": "string"
      }
    ],
    "name": "ProposalAdded",
    "type": "event"
  },
  {
    "anonymous": false,
    "inputs": [
      {
        "indexed": false,
        "name": "proposalID",
        "type": "uint256"
      },
      {
        "indexed": false,
        "name": "position",
        "type": "bool"
      },
      {
        "indexed": false,
        "name": "voter",
        "type": "address"
      },
      {
        "indexed": false,
        "name": "justification",
        "type": "string"
      }
    ],
    "name": "Voted",
    "type": "event"
  },
  {
    "anonymous": false,
    "inputs": [
      {
        "indexed": false,
        "name": "proposalID",
        "type": "uint256"
      },
      {
        "indexed": false,
        "name": "result",
        "type": "int256"
      },
      {
        "indexed": false,
        "name": "quorum",
        "type": "uint256"
      },
      {
        "indexed": false,
        "name": "active",
        "type": "bool"
      }
    ],
    "name": "ProposalTallied",
    "type": "event"
  },
  {
    "anonymous": false,
    "inputs": [
      {
        "indexed": false,
        "name": "member",
        "type": "address"
      },
      {
        "indexed": false,
        "name": "isMember",
        "type": "bool"
      }
    ],
    "name": "MembershipChanged",
    "type": "event"
  },
  {
    "anonymous": false,
    "inputs": [
      {
        "indexed": false,
        "name": "newMinimumQuorum",
        "type": "uint256"
      },
      {
        "indexed": false,
        "name": "newDebatingPeriodInMinutes",
        "type": "uint256"
      },
      {
        "indexed": false,
        "name": "newMajorityMargin",
        "type": "int256"
      }
    ],
    "name": "ChangeOfRules",
    "type": "event"
  },
  {
    "anonymous": false,
    "inputs": [
      {
        "indexed": false,
        "name": "sender",
        "type": "address"
      },
      {
        "indexed": false,
        "name": "amount",
        "type": "uint256"
      }
    ],
    "name": "receivedEther",
    "type": "event"
  },
  {
    "anonymous": false,
    "inputs": [
      {
        "indexed": false,
        "name": "_from",
        "type": "address"
      },
      {
        "indexed": false,
        "name": "_value",
        "type": "uint256"
      },
      {
        "indexed": false,
        "name": "_token",
        "type": "address"
      },
      {
        "indexed": false,
        "name": "_extraData",
        "type": "bytes"
      }
    ],
    "name": "receivedTokens",
    "type": "event"
  }
];

const web3 = new Web3(new Web3.providers.HttpProvider('http://geth:8545'));
const congressContract = new web3.eth.Contract(congressContractAbi, congressContractAddr);

let blockNum = 0;
let producerIsReady = false;

const getEvents = () => {
  if (!producerIsReady) {
    console("Waiting for producer to be ready.");
    return;
  }

  const filter = { fromBlock: blockNum, toBlock: 'latest' };

  congressContract.getPastEvents(
    'allEvents',
    filter,
    (error, _) => {
      if (error) { console.log("error!", error); }
    }
  )
  .then((events) => {
    _.forEach(events, (evt) => {
      console.log(evt);
      produceCongressContractMessage(producer, evt);
      blockNum = events[events.length - 1].blockNumber + 1;
    });
  });
}

const produceCongressContractMessage = (producer, event) => {
  const topic = 'CongressContractEvent;
  const partition = 1;
  const message = new Buffer(JSON.stringify(event))
  const partitionKey = event.address;

  try {
    producer.produce(
      // Topic to send the message to
      topic,
      // optionally we can manually specify a partition for the message
      // this defaults to -1 - which will use librdkafka's default partitioner (consistent random for keyed messages, random for unkeyed messages)
      partition,
      // Message to send. Must be a buffer
      message,
      // for keyed messages, we also specify the key - note that this field is optional
      partitionKey,
      // you can send a timestamp here. If your broker version supports it,
      // it will get added. Otherwise, we default to 0
      Date.now(),
      // you can send an opaque token here, which gets passed along
      // to your delivery reports
    );
  } catch (err) {
    console.error('A problem occurred when sending our message');
    console.error(err);
  }
}

const producer = new Kafka.Producer({
  'metadata.broker.list': 'kafka:9092',
});

// Connect to the broker manually
producer.connect();

producer.on('ready', function() {
  console.log("Producer ready!");
  producerIsReady = true;
});

// Any errors we encounter, including connection errors
producer.on('event.error', function(err) {
  console.error('Error from producer');
  console.error(err);
})

setInterval(getEvents, 5000);
