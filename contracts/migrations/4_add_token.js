var MintableToken = artifacts.require("./token/MintableToken.sol");

module.exports = function(deployer) {
  deployer.deploy(MintableToken);
};

