FROM node:9.2.1-alpine

RUN apk update && apk upgrade && apk add --no-cache \
  bash \
  git \
  openssh \
  python \
  python-dev \
  py-pip \
  build-base

RUN mkdir /app

WORKDIR /app

COPY app/package.json .
COPY app/package-lock.json .

RUN npm install

COPY app .

