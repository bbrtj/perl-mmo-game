package Server::Worker::Command::Test;

use My::Moose;

use header;

with 'Server::Worker::Command';

use constant name => 'test';

sub handler ($job)
{
	$job->app->log->debug("test test");
	return;
}

