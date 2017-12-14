FROM node:9.2.1

RUN mkdir /app

WORKDIR /app

COPY app/package.json .
COPY app/package-lock.json .

RUN npm install

COPY app .

