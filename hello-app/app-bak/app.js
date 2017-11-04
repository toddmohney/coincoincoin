const Web3 = require('web3');
const fs = require('fs');

const web3 = new Web3(Web3.givenProvider || 'http://localhost:8545');

console.log('Accounts');
web3.eth.getAccounts(console.log)

fs.readFile('/home/toddmohney/workspace/personal/gustcoin/hello-app/contracts/build/contracts/Hellos.json', 'utf8', (err, file) => {
  const contract = JSON.parse(file);
  const contractAbi = contract.abi;
  const helloContractAddr = '0x9b24481161d616539e6f80827ea4315801027bb2';
  const helloContract = new web3.eth.Contract(contractAbi, helloContractAddr);

  helloContract.methods.sayHello().send(
    {
      from: '0x31c552f7045aBf6287e38C6Eb719550D6b0Ec1FC',
      gasPrice: '20000000000'
    }
  ).then((result) => {
    console.log("sayHello result");
    console.log(result)
  });

  helloContract.methods.getHellos().call(
    {
      from: '0x31c552f7045aBf6287e38C6Eb719550D6b0Ec1FC',
      gasPrice: '20000000000'
    }
  ).then((result) => {
    console.log("getHellos result");
    console.log(result)
  });
});
