package Server::Worker::Command::Test;

use My::Moose;

use header;

extends 'Server::Worker::Command';

use constant name => 'test';

sub handle ($self, $job)
{
	$job->app->log->debug("test test");
	return;
}

