FROM ruby:latest

# installing headers will implicitly install libraries
RUN apt update
RUN apt install -y liblmdb-dev libvips-dev libxml2-dev libxslt1-dev libsass-dev

RUN adduser --ingroup users --home /var/lib/intertwingler intertwingler

RUN gem install bundler
RUN bundle config set path.system true
# COPY ./intertwingler /tmp/src
RUN --mount=type=bind,src=.,dst=/tmp/src bundle install --gemfile /tmp/src/Gemfile

CMD bash
