const { Pool } = require('pg')
const Web3 = require("web3");
const Kafka = require('node-rdkafka');
const sleep = require('sleep');
var _ = require('lodash');

const contractName = 'Congress';
const networkId = process.env.GETH_NETWORK_ID;

const pool = new Pool({
  connectionString: process.env.DATABASE_URL
});
const query = 'SELECT * from contracts where name = $1 AND network_id = $2';
const values = [contractName, networkId];
pool.query(query, values, (err, res) => {
  pool.end();

  const contract = res.rows[0];
  const congressContractAddr = contract.address;
  const congressContractAbi = JSON.parse(contract.abi.toString());

  console.log("Address:", congressContractAddr);
  console.log("ABI:", congressContractAbi);

  const nodeUrl = process.env.GETH_NODE_URL;
  const web3 = new Web3(new Web3.providers.HttpProvider(nodeUrl));
  const congressContract = new web3.eth.Contract(congressContractAbi, congressContractAddr);

  let blockNum = 0;
  let producerIsReady = false;

  const getEvents = () => {
    if (!producerIsReady) {
      console.log("Waiting for producer to be ready.");
      producer.disconnect();
      producer.connect();
      return;
    }

    const filter = { fromBlock: blockNum, toBlock: 'latest' };

    congressContract.getPastEvents(
      'allEvents',
      filter,
      (error, _) => {
        if (error) { console.log("error!", error); }
      }
    )
    .then((events) => {
      _.forEach(events, (evt) => {
        console.log(evt);
        console.log(JSON.stringify(evt));
        produceCongressContractMessage(producer, evt);
        blockNum = events[events.length - 1].blockNumber + 1;
      });
    });
  }

  const produceCongressContractMessage = (producer, event) => {
    const topic = 'CongressContractEventReceived';
    const partition = 0;
    const message = new Buffer(JSON.stringify(event))
    const partitionKey = event.address;

    try {
      producer.produce(
        // Topic to send the message to
        topic,
        // optionally we can manually specify a partition for the message
        // this defaults to -1 - which will use librdkafka's default partitioner (consistent random for keyed messages, random for unkeyed messages)
        partition,
        // Message to send. Must be a buffer
        message,
        // for keyed messages, we also specify the key - note that this field is optional
        partitionKey,
        // you can send a timestamp here. If your broker version supports it,
        // it will get added. Otherwise, we default to 0
        Date.now(),
        // you can send an opaque token here, which gets passed along
        // to your delivery reports
      );
    } catch (err) {
      console.error('A problem occurred when sending our message');
      console.error(err);
    }
  }

  const producer = new Kafka.Producer({
    'metadata.broker.list': 'kafka:9092',
  });

  // Connect to the broker manually
  producer.connect();

  producer.on('ready', function() {
    console.log("Producer ready!");
    producerIsReady = true;
  });

  // Any errors we encounter, including connection errors
  producer.on('event.error', function(err) {
    console.error('Error from producer');
    console.error(err);
  })

  setInterval(getEvents, 5000);
});
