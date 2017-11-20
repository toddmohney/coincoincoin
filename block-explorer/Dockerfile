FROM node:8.8.1

RUN apt-get update && apt-get install -y git

RUN useradd -m blockexplorer
USER blockexplorer

WORKDIR /home/blockexplorer

RUN git clone https://github.com/etherparty/explorer

WORKDIR /home/blockexplorer/explorer

RUN npm install

EXPOSE 8000

CMD ["npm", "start"]
