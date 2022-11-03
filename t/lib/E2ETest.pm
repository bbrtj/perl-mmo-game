package E2ETest;

use Exporter qw(import);
use Test2::Tools::DatabaseTest;

use Server;
use Server::Worker;
use Server::Config;

use header;

our @EXPORT = qw(e2e_test);

sub e2e_test ($tester)
{
	my @child_ids;

	my sub do_fork ()
	{
		my $pid = fork;
		die 'error forking'
			unless defined $pid;

		push @child_ids, $pid
			if $pid;

		return $pid == 0;
	}

	DI->set('channel_service', DI->get('channel_service', key => '_test_server_feedback'), 1);
	DI->set('data_bus', DI->get('data_bus', key => '_test_data_bus'), 1);

	my $server_port = Server::Config->GAME_SERVER_PORT + 1;

	my $cleanup = manual_database_test;

	if (do_fork) {
		my $worker = Server::Worker->new();
		$worker->start(2);
		exit;
	}

	if (do_fork) {
		my $server = Server->new(port => $server_port);
		$server->start_listening(1);
		exit;
	}

	defer { $cleanup->() }

	# give server / worker some time to boot
	sleep 1;

	$tester->($server_port);

	kill 'INT', @child_ids;
	1 while wait != -1;

	return;
}

