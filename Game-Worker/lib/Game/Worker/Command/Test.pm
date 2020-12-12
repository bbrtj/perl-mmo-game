package Game::Worker::Command::Test;

use Mojo::Base -signatures;
use Moo;

with 'Game::Worker::Command';

use constant name => 'test';

sub handler($job)
{
	$job->app->log->debug("test test");
}

1;
