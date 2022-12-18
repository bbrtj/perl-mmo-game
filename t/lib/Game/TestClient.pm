package Game::TestClient;

use My::Moose;
use Test2::Tools::E2ETest;
use Mojo::IOLoop;
use Server::Config;
use Data::Compare;
use all 'Unit', 'Game::TestClient';

use header;

has param 'actor' => (
	isa => Types::InstanceOf ['Unit::Actor'],
);

has field 'actions' => (
	isa => Types::ArrayRef [Types::InstanceOf ['Game::TestClient::Action']],
	default => sub { [] },
	'handles[]' => {
		'_add_action' => 'push',
	}
);

has field 'finished' => (
	isa => Types::Bool,
	default => !!0,
	'handles!!' => {
		'_set_finished' => 'set',
		'_reset_finished' => 'reset',
	}
);

has field 'success' => (
	isa => Types::Bool,
	default => !!1,
	'handles!!' => {
		'_set_failed' => 'unset',
	}
);

sub add_action ($self, $name, @args)
{
	$name = "Game::TestClient::Action::$name";
	$self->_add_action($name->new(client => $self, @args));

	return $self;
}

sub raise ($self, $error, $warn = !!0)
{
	my $str = "TestClient error: $error";
	if (!$warn) {
		$self->_set_finished;
		$self->_set_failed;
		croak $str;
	}

	warn $str;
	return;
}

sub run ($self, $loop = Mojo::IOLoop->singleton)
{
	$self->_reset_finished;

	my $action_index = 0;
	my $action;

	my sub grab_action ()
	{
		if (!$action || $action->finished) {
			if ($action_index > $self->actions->$#*) {
				$self->_set_finished;
				die '[ finished ]';
			}

			$action = $self->actions->[$action_index];

			$action->setup_state;
			++$action_index;
		}

		return $action;
	}

	my $last_sent_id = 0;

	my sub get_send_data ()
	{
		my @data = grab_action->get_data->@*;
		unshift @data, ++$last_sent_id;
		push @data, join Server::Config::PROTOCOL_SEPARATOR, splice @data, 2;

		return join(Server::Config::PROTOCOL_CONTROL_CHARACTER, @data) . "\r\n";
	}

	my sub compare_received_data ($data)
	{
		my @parts = split quotemeta(Server::Config::PROTOCOL_CONTROL_CHARACTER), $data, 3;
		$self->raise("$action: unexpected id from server communication: \nGot: $parts[0] \nExpected: $last_sent_id")
			unless !length $parts[0] || $parts[0] == $last_sent_id;

		unless (grab_action->find_and_compare(@parts[1, 2])) {
			my $type = $action->get_expected_type;
			my $expected = $action->get_expected_data;

			$self->raise(
				"$action: unexpected type/data from server communication: \nGot: $parts[1]/$parts[2] \nExpected: $type/$expected"
			);
		}

		if ($parts[0]) {
			say "$action: Server response #$last_sent_id ok";
		}
		else {
			say "$action: Server feed ok";
		}

		return;
	}

	e2e_client(
		$loop,
		get_send_data,
		sub ($stream, $bytes, $receive_no) {
			if ($self->finished) {
				$self->raise("trailing data: $bytes", 'warn');
				return;
			}

			try {
				compare_received_data($bytes);
				if (grab_action->should_send) {
					$stream->write(get_send_data);

					# try to get next action. Will stop the loop if there are no more actions
					grab_action;
				}
			}
			catch ($e) {
				$self->raise($e) unless $self->finished && $self->success;
			}
		}
	);

	return;
}

