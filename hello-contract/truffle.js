module.exports = {
  // See <http://truffleframework.com/docs/advanced/configuration>
  // to customize your Truffle configuration!
  networks: {
    test: {
      host: 'localhost',
      port: 8555,
      network_id: '*',
      gas: 4600000
    },
    development: {
      host: 'localhost',
      port: 8545,
      network_id: '*',
      gas: 4600000
    },
  },
  mocha: {
    useColors: true
  }
};
