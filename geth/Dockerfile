FROM ubuntu:16.04

RUN apt-get update && apt-get install -y \
  software-properties-common

RUN add-apt-repository -y ppa:ethereum/ethereum

RUN apt-get update && apt-get install -y \
  ethereum

ARG username
ARG coinbase
ARG networkid

RUN useradd -m $username
USER $username

WORKDIR /home/$username

RUN mkdir -p /home/$username/.ethereum
RUN mkdir -p /home/$username/.ethash

COPY genesis.json /home/$username/genesis.json

EXPOSE 8545
EXPOSE 8646
EXPOSE 6060
EXPOSE 30303

CMD ["geth"]
