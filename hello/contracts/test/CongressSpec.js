const Q = require('q');
const Congress = artifacts.require('Congress');

contract('Congress', (accts) => {
  describe('addMember', () => {
    const minQuorum = 2;
    const minMinutes = 0;
    const victoryMargin = 0;

    let contract;

    it('adds a new member', () => {
      return Congress.new(minQuorum, minMinutes, victoryMargin).then((instance) => {
        contract = instance;
        return contract.addMember(accts[0], "Brenda Walsh");
      }).then(() => {
        return contract.members.call(1);
      }).then((member) => {
        const memberAddr = member[0];
        const memberName = member[1];
        assert.equal(memberAddr, accts[0]);
        assert.equal(memberName, "Brenda Walsh");
      });
    });

    it('updates an existing member', () => {
      return Congress.new(minQuorum, minMinutes, victoryMargin).then((instance) => {
        contract = instance;
        return Q.all([
          contract.addMember(accts[0], "Brenda Walsh"),
          contract.addMember(accts[0], "Brennnnnda Walssssssh")
        ]);
      }).then(() => {
        return contract.members.call(1);
      }).then((member) => {
        const memberAddr = member[0];
        const memberName = member[1];
        assert.equal(memberAddr, accts[0]);
        assert.equal(memberName, "Brennnnnda Walssssssh");
      });
    });
  });

  describe('removeMember', () => {
    const minQuorum = 2;
    const minMinutes = 0;
    const victoryMargin = 0;

    let contract;

    it('removes an existing member', () => {
      return Congress.new(minQuorum, minMinutes, victoryMargin).then((instance) => {
        contract = instance;
        return contract.addMember(accts[0], "Brenda Walsh");
      }).then(() => {
        return contract.removeMember(accts[0]);
      }).then((result) => {
        for (let i = 0; i < result.logs.length; i++) {
          const log = result.logs[i];
          if (log.event == "MembershipChanged") {
            assert.equal(log.args.isMember, false);
            break;
          }
        }

        assert.equal(true, true);
      });
    });
  });
});
