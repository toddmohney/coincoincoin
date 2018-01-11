FROM ruby:2.3.5

RUN gem install sinatra
RUN gem install bundler

RUN mkdir -p /usr/local/dao

WORKDIR /usr/local/dao

COPY Gemfile Gemfile
RUN bundle install

COPY app.rb app.rb
COPY dist public

EXPOSE 4567

CMD ["ruby", "app.rb"]
