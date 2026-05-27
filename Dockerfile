FROM perl:5.42

# Install system dependencies
RUN apt-get update && apt-get install -y \
	redis-server \
	postgresql-client \
	libpq-dev \
	cpanminus \
	build-essential \
	gettext \
	git \
	vim \
	&& rm -rf /var/lib/apt/lists/*

# Set working directory
WORKDIR /game

# Install extra dependencies
RUN cpanm --notest Carmel App::Yath App::Sqitch UUID DBD::Pg

# Set Perl library path
ENV PERL5LIB=/game/lib:/game/lib-base:/game/local/lib/perl5:${PERL5LIB}
ENV PATH=/game/local/bin:${PATH}

