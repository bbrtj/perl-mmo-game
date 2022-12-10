package Game::TestClient::Action::Stop;

use My::Moose;
use Game::Config;
use Game::Object::Movement;
use all 'Resource';

use header;

extends 'Game::TestClient::Action';

use constant requires => ['EnterGame'];

has param 'was_moving' => (
	isa => Types::Bool,
	default => !!1,
);

sub send_queue ($self)
{
	return (
		['stop'],
	);
}

sub receive_queue ($self)
{
	if ($self->was_moving) {
		return (
			Resource::ActorState->new(subject => $self->client->actor, stopped => 1),
		);
	}
	else {
		return ();
	}
}

sub find_and_compare ($self, $type, $data)
{
	my $result = $self->SUPER::find_and_compare($type, $data);

	if (!$result) {
		my $actual_data = __deserialize($data);
		my $actor_variables = $self->client->actor->variables;
		$actor_variables->set_pos_x($actual_data->{x});
		$actor_variables->set_pos_y($actual_data->{y});

		$result = $self->SUPER::find_and_compare($type, $data);
	}

	return $result;
}

