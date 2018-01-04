var Crowdsale = artifacts.require("./crowdsale/Crowdsale.sol");

function daysInMillisecs(numDays) {
  return numDays * 24 * 60 * 60 * 1000
}

module.exports = function(deployer) {
  const fundraisingPeriod = daysInMillisecs(90);
  const startTime = Date.now();
  const endTime = startTime + fundraisingPeriod;

  const weiToTokenConversionRate = 1;
  const beneficiary = "0xb3ED286C1D088016589B5d2B0729A73A1e24f8A7";

  deployer.deploy(Crowdsale, startTime, endTime, weiToTokenConversionRate, beneficiary);
};

