var Hellos = artifacts.require("./Hellos.sol");

module.exports = function(deployer) {
  deployer.deploy(Hellos);
};
