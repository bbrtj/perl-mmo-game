package Server::Worker::Command::Test;

use Moo;

use header;

with 'Server::Worker::Command';

use constant name => 'test';

sub handler ($job)
{
	$job->app->log->debug("test test");
	return;
}

