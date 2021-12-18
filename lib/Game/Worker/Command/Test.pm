package Game::Worker::Command::Test;

use Moo;

use header;

with 'Game::Worker::Command';

use constant name => 'test';

sub handler ($job)
{
	$job->app->log->debug("test test");
	return;
}

