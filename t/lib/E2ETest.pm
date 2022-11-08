package E2ETest;

use Exporter qw(import);
use Test2::Tools::DatabaseTest;
use Utils;

use Server;
use Server::Worker;
use Server::Config;

use header;

our @EXPORT = qw(e2e_test e2e_client);
our $SERVER_PORT;

sub e2e_test : prototype(&) ($tester)
{
	my @child_ids;

	my sub finished ()
	{
		kill 'INT', @child_ids;
		1 while wait != -1;
	}

	my sub do_fork ()
	{
		my $pid = Utils->safe_fork;
		die 'error forking'
			unless defined $pid;

		push @child_ids, $pid
			if $pid;

		return $pid == 0;
	}

	DI->set('channel_service', DI->get('channel_service', key => '_test_server_feedback'), 1);
	DI->set('data_bus', DI->get('data_bus', key => '_test_data_bus'), 1);

	local $SERVER_PORT = Server::Config->GAME_SERVER_PORT + 1;

	my $cleanup = manual_database_test;

	if (do_fork) {
		my $worker = Server::Worker->new();
		$worker->start(2);
		exit;
	}

	if (do_fork) {
		my $server = Server->new(port => $SERVER_PORT);
		$server->start_listening(1);
		exit;
	}

	defer {
		finished();
		$cleanup->();
	}

	# give server / worker some time to boot
	sleep 1;

	$tester->();

	return;
}

sub e2e_client ($first_message, $on_receive)
{
	my $receive_no = 0;
	return Mojo::IOLoop->client(
		{address => '127.0.0.1', port => $SERVER_PORT},
		sub ($loop, $err, $stream) {
			die "error connecting: $err" if $err;

			$stream->on(
				read => sub ($stream, $bytes) {
					chomp $bytes;

					$on_receive->($stream, $bytes, ++$receive_no);
				}
			);

			$stream->write($first_message);
		}
	);
}

