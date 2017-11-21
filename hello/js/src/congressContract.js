const Web3 = require("web3");
const web3 = new Web3(Web3.givenProvider || 'ws://localhost:8546');

const congressContractAddr = '0x91cdc7c41c03312efca063acdeb016870e4e8411';
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
exports.congressContract = new web3.eth.Contract(congressContractAbi, congressContractAddr);
exports.congressContractAddr = congressContractAddr;
