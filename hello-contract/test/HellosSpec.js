var Hellos = artifacts.require('Hellos');

contract('Hellos', (accts) => {
  it('returns 0 when the sender has not said hello', () => {
    return Hellos.deployed().then((instance) => {
      return instance.getHellos.call();
    }).then((helloCount) => {
      assert.equal(helloCount.valueOf(), 0);
    });
  });

  it('returns the count of hellos', () => {
    let contract;

    return Hellos.deployed().then((instance) => {
      contract = instance;
      contract.sayHello();
    }).then(() => {
      return contract.sayHello();
    }).then(() => {
      return contract.getHellos();
    }).then((helloCount) => {
      assert.equal(helloCount.valueOf(), 2);
    });
  });
});
