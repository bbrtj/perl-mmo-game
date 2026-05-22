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

# Copy dependency files if they exist
COPY cpanfile* ./

# Install extra dependencies
RUN cpanm --notest Carmel App::Yath App::Sqitch UUID DBD::Pg

# Create logs directory
RUN mkdir -p logs

# Set Perl library path
ENV PERL5LIB=/game/lib:/game/lib-base:/game/local/lib/perl5:${PERL5LIB}

