#!/usr/bin/env perl

use Mojolicious::Lite;
use Game::Common;

my $config = Game::Common->get_config(app, '../vars.conf');

plugin Minion => {Pg => $config->{db}{connection}};
plugin 'Minion::Admin';

app->start;
