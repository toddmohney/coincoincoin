FROM node:7.7.4
MAINTAINER Todd Mohney <toddmohney@gmail.com>

RUN mkdir -p /var/lib/webapp/webapp-frontend
WORKDIR /var/lib/webapp/webapp-frontend

COPY ./package.json package.json

RUN npm install gulp -g
RUN npm install

COPY . /var/lib/webapp/webapp-frontend

VOLUME /var/lib/webapp/webapp-frontend

RUN gulp production

EXPOSE 3000

CMD "npm start"
