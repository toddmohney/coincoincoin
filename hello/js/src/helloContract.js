const Web3 = require("web3");
const web3 = new Web3(Web3.givenProvider || 'ws://localhost:8546');

const helloContractAddr = '0x0ed41af5ecc84a9a4a46127e255c56c6f27f89fd';
const helloContractAbi = [
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

exports.helloContract = new web3.eth.Contract(helloContractAbi, helloContractAddr);
