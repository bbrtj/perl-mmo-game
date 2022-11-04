# MMORPG Game (name tbd)

## Prerequisites

- Perl 5.36 (preferably from perlbrew)
- Postgresql client library with headers
- Postgresql server
- GNU make

## Setup

Copy `.env.example` to `.env` and adjust the variables.

### Setup environment

Run this command to get required CPAN modules:

```
cpanm Carmel Sqitch Code::TidyAll Perl::Critic::Community Perl::Tidy App::Yath
```

### Setup dependencies

Run this:

```
carmel install && carmel rollout
```

### Setup tools

Not required, but can help:

```
mkdir -p .git/hooks
cp tools/git-hook-precommit .git/hooks/pre-commit
```

## Running servers

`./run` will run both TCP server and workers, but not web server.

## Running website

`./run web-dev` to run development web server (auto restart on changes).

`./run web` to run production server (same server).

## Development tools

`./test` to run tests

`./run lint` to lint files

`./run critic` and `./run critic_tests` to see possible problems with code

## Code layout

### lib-base

Directory for base utilities that do not depend on the rest of the system.

### lib

#### CLI

Command line utilities' code.

#### Game

Actual game data structures and mechanics.

#### Server

Game backend related infrastructure. TCP servers, workers, game processes and message / event types.

#### Web

Website-related stuff. May be moved away to other repository in the future.

## Tips and Tricks

- better stack traces during debugging: `./run -MCarp::Always`

