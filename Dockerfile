#FROM --platform=linux/amd64 ruby:latest
#FROM --platform=linux/arm64 arm64v8/ruby:latest
FROM ruby:latest

# installing headers will implicitly install libraries
RUN apt-get -y update
RUN apt-get install -y liblmdb-dev libvips-dev libxml2-dev libxslt1-dev libsass-dev less vim

RUN adduser --ingroup users --home /var/lib/intertwingler intertwingler

RUN gem install bundler
RUN bundle config set path.system true

RUN mkdir /tmp/src
WORKDIR /tmp/src

# tried to do these with bundler; it absolutely did not cooperate

# RUN git clone https://gitlab.com/doriantaylor/uri-urn.git && cd uri-urn && gem build && gem install *.gem && cd -

# XXX TEMPORARY UNTIL 3.3.3 IS RELEASED
RUN git clone -b speed-up-rdf-graph https://github.com/doriantaylor/rdf.git && cd rdf && gem build && gem install *.gem && cd -

RUN git clone -b aliases-etc https://github.com/doriantaylor/rb-mimemagic.git && cd rb-mimemagic && gem build && gem install *.gem && cd -

RUN git clone -b add-prompt-to-collector https://github.com/doriantaylor/rb-tty-prompt.git && cd rb-tty-prompt && gem build && gem install *.gem && cd -

RUN git clone -b control-w-please https://github.com/doriantaylor/rb-tty-reader.git && cd rb-tty-reader && gem build && gem install *.gem && cd -

RUN git clone https://github.com/doriantaylor/rb-store-digest.git && cd rb-store-digest && gem build && gem install *.gem && cd -

RUN git clone https://github.com/doriantaylor/rb-store-digest-http.git && cd rb-store-digest-http && gem build && gem install *.gem && cd -

RUN git clone https://github.com/doriantaylor/rb-md-noko.git && cd rb-md-noko && gem build && gem install *.gem && cd -

RUN git clone https://github.com/doriantaylor/rb-rdf-lmdb.git && cd rb-rdf-lmdb && gem build && gem install *.gem && cd -

# RUN --mount=type=bind,src=.,dst=/tmp/src bundle install --gemfile /tmp/src/Gemfile
COPY . /tmp/src/intertwingler

# RUN git clone -b transform-queues https://github.com/doriantaylor/rb-intertwingler.git /tmp/src

WORKDIR /tmp/src/intertwingler

RUN gem install pry pry-byebug puma engtagger
# this one, i mean wtf
RUN find /usr/local/bundle/gems/engtagger* -type f -print0 | xargs -0 chmod 644
RUN bundle install
RUN gem build ; gem install *.gem

RUN mkdir -p /var/lib/type

# RUN mkdir -p /var/lib/app-ibis
# RUN git clone https://github.com/doriantaylor/app-ibis-front-end.git ; cd app-ibis-front-end ; make ; rsync -av target/ /var/lib/app-ibis ; cd -

RUN rm -rf /tmp/src

EXPOSE 10101

WORKDIR /var/lib/intertwingler

USER intertwingler
ENV INTERTWINGLER_HOME=/var/lib/intertwingler

RUN mkdir root
COPY --chown=intertwingler:users example/transforms2.ttl config.ttl

CMD intertwingler
