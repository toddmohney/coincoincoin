import PropTypes from 'prop-types';
import React from 'react';
import ReactDOM from 'react-dom';
import Paper from 'material-ui/Paper';
import FlatButton from 'material-ui/FlatButton';
import Web3 from 'web3';

class UserList extends React.Component {
  getHellos() {
    const web3 = new Web3(Web3.givenProvider || 'http://localhost:8545');
    const helloContractAddr = '0x32c9e197951a3674aab69a5df245a27069fb6e5d';
    const contractAbi = [
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

    helloContract.methods.getHellos().call({
      from: '0x6f466bb3540e96436298c8cb8fb2f07c515f8068'
    })
      .then((result) => {
        console.log("getHellos result");
        console.log(result)
      });
  }

  sayHello() {
    const web3 = new Web3(Web3.givenProvider || 'http://localhost:8545');
    const helloContractAddr = '0x32c9e197951a3674aab69a5df245a27069fb6e5d';
    const contractAbi = [
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

    helloContract.methods.sayHello().send(
      {
        from: '0x6f466bb3540e96436298c8cb8fb2f07c515f8068',
        gasPrice: '20000000000'
      }
    ).then((result) => {
      console.log("sayHello result");
      console.log(result)
    });
  }

  render() {
    return (
      <Paper>
        <FlatButton label="Get Hellos" onClick={this.getHellos} />
        <FlatButton label="Say Hello" onClick={this.sayHello} />
      </Paper>
    );
  }
};

UserList.propTypes = {
  users: PropTypes.object.isRequired,
}

export default UserList;
