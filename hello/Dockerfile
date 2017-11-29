FROM ruby:2.3.5

RUN gem install sinatra

RUN mkdir -p /usr/local/dao

WORKDIR /usr/local/dao

COPY app.rb app.rb
COPY dist public

EXPOSE 4567

CMD ["ruby", "app.rb"]
