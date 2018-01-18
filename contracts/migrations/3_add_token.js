var CoinCoinCoinToken = artifacts.require("./CoinCoinCoinToken.sol");

module.exports = function(deployer) {
  deployer.deploy(CoinCoinCoinToken);
};


