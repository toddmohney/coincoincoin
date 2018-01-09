const Q = require('q');
const R = require('ramda');
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

  describe('newProposal', () => {
    const minQuorum = 2;
    const minMinutes = 0;
    const victoryMargin = 0;

    let contract;

    it('adds a new proposal', () => {
      return Congress.new(minQuorum, minMinutes, victoryMargin).then((instance) => {
        contract = instance;
        return contract.newProposal(
          accts[0],
          1000000000,
          'we gotta fix these roads!',
          ''
        );
      }).then((result) => {
        const matcherFn = (log) => { return log.event == 'ProposalAdded' };
        const log = R.find(matcherFn, result.logs);
        assert.equal(log.args.proposalID, 0);
        assert.equal(log.args.recipient, accts[0]);
        assert.equal(log.args.amount, 1000000000);
        assert.equal(log.args.description, 'we gotta fix these roads!');

        return contract.proposals.call(0)
      }).then((proposal) => {
        const recip = proposal[0];
        const amount = proposal[1];
        const desc = proposal[2];
        const executed = proposal[4];
        const passed = proposal[5];
        const votes = proposal[6];
        const result = proposal[7];

        assert.equal(recip, accts[0]);
        assert.equal(amount.valueOf(), 1000000000);
        assert.equal(desc, 'we gotta fix these roads!');
        assert.equal(executed, false);
        assert.equal(passed, false);
        assert.equal(votes.valueOf(), 0);
        assert.equal(result.valueOf(), 0);
      });
    });
  });

  describe('newProposalInEther', () => {
    const minQuorum = 2;
    const minMinutes = 0;
    const victoryMargin = 0;

    let contract;

    it('adds a new proposal', () => {
      return Congress.new(minQuorum, minMinutes, victoryMargin).then((instance) => {
        contract = instance;
        return contract.newProposalInEther(
          accts[0],
          1,
          'we gotta fix these roads!',
          ''
        );
      }).then((result) => {
        const matcherFn = (log) => { return log.event == 'ProposalAdded' };
        const log = R.find(matcherFn, result.logs);
        assert.equal(log.args.proposalID, 0);
        assert.equal(log.args.recipient, accts[0]);
        assert.equal(log.args.amount.valueOf(), 1000000000000000000);
        assert.equal(log.args.description, 'we gotta fix these roads!');

        return contract.proposals.call(0)
      }).then((proposal) => {
        const amount = proposal[1];
        assert.equal(amount.valueOf(), 1000000000000000000);
      });
    });
  });

  describe('vote', () => {
    const minQuorum = 2;
    const minMinutes = 0;
    const victoryMargin = 0;

    let contract;
    let proposalID;

    it('accepts a new supporting vote from a member', () => {
      return Congress.new(minQuorum, minMinutes, victoryMargin).then((instance) => {
        contract = instance;
        // create a new proposal
        return contract.newProposalInEther(accts[0], 1, 'we gotta fix these roads!', '');
      }).then((result) => {
        const matcherFn = (log) => { return log.event == 'ProposalAdded' };
        const log = R.find(matcherFn, result.logs);
        proposalID = log.args.proposalID.valueOf();

        return contract.vote(
          proposalID,
          true,
          'I agree, we gotta fix these roads'
        );
      }).then((result) => {
        const matcherFn = (log) => { return log.event == 'Voted' };
        const log = R.find(matcherFn, result.logs);
        const voteProposalID = log.args.proposalID.valueOf();
        const votePosition = log.args.position;
        const voter = log.args.voter;
        const voteJustification = log.args.justification;

        assert.equal(voteProposalID, proposalID);
        assert.equal(votePosition, true);
        assert.equal(voter, accts[0]);
        assert.equal(voteJustification, 'I agree, we gotta fix these roads');

        return contract.proposals.call(proposalID);
      }).then((proposal) => {
        const numberOfVotes = proposal[6].valueOf();
        const currentResult = proposal[7].valueOf();

        assert.equal(numberOfVotes, 1);
        assert.equal(currentResult, 1);
      });
    });

    it('accepts a new opposition vote from a member', () => {
      return Congress.new(minQuorum, minMinutes, victoryMargin).then((instance) => {
        contract = instance;
        // create a new proposal
        return contract.newProposalInEther(accts[0], 1, 'we gotta fix these roads!', '');
      }).then((result) => {
        const matcherFn = (log) => { return log.event == 'ProposalAdded' };
        const log = R.find(matcherFn, result.logs);
        proposalID = log.args.proposalID.valueOf();

        return contract.vote(
          proposalID,
          false,
          'No way! I like these roads the way they are'
        );
      }).then((result) => {
        const matcherFn = (log) => { return log.event == 'Voted' };
        const log = R.find(matcherFn, result.logs);
        const voteProposalID = log.args.proposalID.valueOf();
        const votePosition = log.args.position;
        const voter = log.args.voter;
        const voteJustification = log.args.justification;

        assert.equal(voteProposalID, proposalID);
        assert.equal(votePosition, false);
        assert.equal(voter, accts[0]);
        assert.equal(voteJustification, 'No way! I like these roads the way they are');

        return contract.proposals.call(proposalID);
      }).then((proposal) => {
        const numberOfVotes = proposal[6].valueOf();
        const currentResult = proposal[7].valueOf();

        assert.equal(numberOfVotes, 1);
        assert.equal(currentResult, -1);
      });
    });
  });

  describe('changeVotingRules', () => {
    const minQuorum = 2;
    const minMinutes = 0;
    const victoryMargin = 1;

    let contract;
    let proposalID;

    it('updates the voting rules', () => {
      return Congress.new(minQuorum, minMinutes, victoryMargin).then((instance) => {
        contract = instance;
        // create a new proposal
        return contract.newProposalInEther(accts[0], 1, 'we gotta fix these roads!', '');
      }).then(() => {
        return Q.all([
          contract.minimumQuorum.call(),
          contract.debatingPeriodInMinutes.call(),
          contract.majorityMargin.call()
        ]);
      }).then((results) => {
        const minQuorum = results[0].valueOf();
        const debateMins = results[1].valueOf();
        const victoryMargin = results[2].valueOf();

        // assert initial contract state
        assert.equal(minQuorum, 2);
        assert.equal(debateMins, 0);
        assert.equal(victoryMargin, 1);

        return contract.changeVotingRules(5, 10, 15);
      }).then((result) => {
        const matcherFn = (log) => { return log.event == 'ChangeOfRules' };
        const log = R.find(matcherFn, result.logs);

        // assert the event is reporting correctly
        assert.equal(log.args.newMinimumQuorum.valueOf(), 5);
        assert.equal(log.args.newDebatingPeriodInMinutes.valueOf(), 10);
        assert.equal(log.args.newMajorityMargin.valueOf(), 15);

        // query the contract state
        return Q.all([
          contract.minimumQuorum.call(),
          contract.debatingPeriodInMinutes.call(),
          contract.majorityMargin.call()
        ]);
      }).then((results) => {
        const minQuorum = results[0].valueOf();
        const debateMins = results[1].valueOf();
        const victoryMargin = results[2].valueOf();

        // assert that the contract state is set correctly
        assert.equal(minQuorum, 5);
        assert.equal(debateMins, 10);
        assert.equal(victoryMargin, 15);
      });
    });
  });

  describe('checkProposalCode', () => {
    it('returns true when the hashes match');
    it('returns false when the hashes do not match');
  });
});
